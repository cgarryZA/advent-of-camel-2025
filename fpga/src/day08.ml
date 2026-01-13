(* src/day08.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth  = 64
let extra_synth_args = []

(* ====================== RAM ====================== *)

module Ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 32
  let depth           = 16384
  let num_ports       = 2
  let zero_on_startup = false
end)

(* ====================== LOADER ====================== *)

module Loader = struct
  module I = struct
    type 'a t =
      { clock    : 'a
      ; clear    : 'a
      ; uart_rx  : 'a Uart.Byte_with_valid.t
      ; uart_rts : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { load_finished : 'a
      ; ram_write     : 'a Ram.Port.t
      ; word_count    : 'a [@bits 14]
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create _ ({ clock; clear; uart_rx; uart_rts } : _ I.t) =
    let spec = Reg_spec.create ~clock ~clear () in
    let word_in = Util.shift_in ~clock ~clear ~n:4 uart_rx in

    let loaded  = Variable.reg spec ~width:1 in
    let wr_addr = Variable.reg spec ~width:14 in
    let wr_en   = word_in.valid &: ~:(loaded.value) in

    compile
      [ when_ wr_en    [ wr_addr <-- wr_addr.value +:. 1 ]
      ; when_ uart_rts [ loaded  <-- vdd ]
      ];

    { O.
      load_finished = loaded.value
    ; ram_write =
        { address      = wr_addr.value
        ; write_data   = word_in.value
        ; write_enable = wr_en
        }
    ; word_count    = wr_addr.value
    ; uart_rx_ready = vdd
    }
  ;;

  let hierarchical scope =
    let module S = Hierarchy.In_scope (I) (O) in
    S.hierarchical ~name:"loader" ~scope create
  ;;
end

(* ====================== STAGE 12 ALGO ====================== *)
(* All-pairs squared Euclidean distance
   Part 1 = min d2
   Part 2 = max d2
*)

module States = struct
  type t =
    | Loading | Init
    | Addr_ax | Wait_ax | Consume_ax
    | Addr_ay | Wait_ay | Consume_ay
    | Addr_az | Wait_az | Consume_az
    | Addr_bx | Wait_bx | Consume_bx
    | Addr_by | Wait_by | Consume_by
    | Addr_bz | Wait_bz | Consume_bz
    | Compute
    | Advance_j
    | Advance_i
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo ~clock ~clear ~load_finished ~(word_count : Signal.t) ~(read_word : Signal.t) =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  let addr   = Variable.reg spec ~width:14 in
  let base_i = Variable.reg spec ~width:14 in
  let base_j = Variable.reg spec ~width:14 in

  let ax = Variable.reg spec ~width:32 in
  let ay = Variable.reg spec ~width:32 in
  let az = Variable.reg spec ~width:32 in
  let bx = Variable.reg spec ~width:32 in
  let by = Variable.reg spec ~width:32 in
  let bz = Variable.reg spec ~width:32 in

  let min_d2 = Variable.reg spec ~width:64 in
  let max_d2 = Variable.reg spec ~width:64 in

  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  let abs32 a b =
    let d = a -: b in
    mux2 (msb d) (zero 32 -: d) d
  in

  let sq64 x =
    let x64 = uresize ~width:64 x in
    uresize ~width:64 (x64 *: x64)
  in

  let d2 =
    sq64 (abs32 ax.value bx.value)
    +: sq64 (abs32 ay.value by.value)
    +: sq64 (abs32 az.value bz.value)
  in

  compile
    [ sm.switch
        [ (Loading, [ when_ load_finished [ sm.set_next Init ] ])

        ; (Init,
            [ base_i <--. 0
            ; base_j <--. 3
            ; min_d2 <-- ones 64
            ; max_d2 <-- zero 64
            ; done_fired <-- gnd
            ; sm.set_next Addr_ax ])

        ; (Addr_ax, [ addr <-- base_i.value; sm.set_next Wait_ax ])
        ; (Wait_ax, [ sm.set_next Consume_ax ])
        ; (Consume_ax, [ ax <-- read_word; sm.set_next Addr_ay ])

        ; (Addr_ay, [ addr <-- base_i.value +:. 1; sm.set_next Wait_ay ])
        ; (Wait_ay, [ sm.set_next Consume_ay ])
        ; (Consume_ay, [ ay <-- read_word; sm.set_next Addr_az ])

        ; (Addr_az, [ addr <-- base_i.value +:. 2; sm.set_next Wait_az ])
        ; (Wait_az, [ sm.set_next Consume_az ])
        ; (Consume_az, [ az <-- read_word; sm.set_next Addr_bx ])

        ; (Addr_bx, [ addr <-- base_j.value; sm.set_next Wait_bx ])
        ; (Wait_bx, [ sm.set_next Consume_bx ])
        ; (Consume_bx, [ bx <-- read_word; sm.set_next Addr_by ])

        ; (Addr_by, [ addr <-- base_j.value +:. 1; sm.set_next Wait_by ])
        ; (Wait_by, [ sm.set_next Consume_by ])
        ; (Consume_by, [ by <-- read_word; sm.set_next Addr_bz ])

        ; (Addr_bz, [ addr <-- base_j.value +:. 2; sm.set_next Wait_bz ])
        ; (Wait_bz, [ sm.set_next Consume_bz ])
        ; (Consume_bz, [ bz <-- read_word; sm.set_next Compute ])

        ; (Compute,
            [ when_ (d2 <: min_d2.value) [ min_d2 <-- d2 ]
            ; when_ (d2 >: max_d2.value) [ max_d2 <-- d2 ]
            ; sm.set_next Advance_j ])

        ; (Advance_j,
            [ base_j <-- base_j.value +:. 3
            ; if_ ((base_j.value +:. 3) >=: word_count)
                [ sm.set_next Advance_i ]
                [ sm.set_next Addr_bx ] ])

        ; (Advance_i,
            [ base_i <-- base_i.value +:. 3
            ; base_j <-- base_i.value +:. 6
            ; if_ ((base_i.value +:. 6) >=: word_count)
                [ sm.set_next Done ]
                [ sm.set_next Addr_ax ] ])

        ; (Done,
            [ when_ (done_fired.value ==:. 0) [ done_fired <-- vdd ] ])
        ]
    ];

  addr.value, min_d2.value, max_d2.value, done_pulse
;;

(* ====================== TOP ====================== *)

let create scope ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t) =
  let loader = Loader.hierarchical scope { clock; clear; uart_rx; uart_rts } in
  let ram_ports = Array.init 2 ~f:(fun _ -> Ram.Port.Of_signal.wires ()) in

  let%tydi ram =
    Ram.hierarchical ~name:"ram" scope
      { clock; clear
      ; load_ports    = [| loader.ram_write; Ram.Port.unused |]
      ; load_finished = loader.load_finished
      ; ram_ports
      }
  in

  let addr, p1, p2, done_pulse =
    algo ~clock ~clear
      ~load_finished:loader.load_finished
      ~word_count:loader.word_count
      ~read_word:ram.read_data.(0)
  in

  Ram.Port.Of_signal.assign ram_ports.(0)
    { address = addr; write_data = zero 32; write_enable = gnd };

  Ram.Port.Of_signal.assign ram_ports.(1)
    { address = zero 14; write_data = zero 32; write_enable = gnd };

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock; clear
      ; part1 = { value = uresize ~width:60 p1; valid = done_pulse }
      ; part2 = { value = uresize ~width:60 p2; valid = done_pulse }
      }
  in

  { Ulx3s.O.
    leds          = concat_lsb [ ~:clear; uart_rx_overflow; loader.load_finished; zero 5 ]
  ; uart_tx       = byte_out
  ; uart_rx_ready = loader.uart_rx_ready
  }
;;

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day08" ~scope create
;;
