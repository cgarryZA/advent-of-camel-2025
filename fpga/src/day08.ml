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
(* Input format:
   flat stream of 32-bit words:
     x0, y0, z0, x1, y1, z1, ...
   RTS ends load
*)

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

  let create _scope ({ clock; clear; uart_rx; uart_rts } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~clear () in

    (* Assemble 4 UART bytes into 1x 32-bit word *)
    let word_in = Util.shift_in ~clock ~clear ~n:4 uart_rx in

    let loaded  = Variable.reg spec ~width:1 in
    let wr_addr = Variable.reg spec ~width:14 in

    let wr_en = word_in.valid &: ~:(loaded.value) in

    compile
      [ when_ wr_en
          [ wr_addr <-- wr_addr.value +:. 1 ]
      ; when_ uart_rts
          [ loaded <-- vdd ]
      ]
    ;

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

(* ====================== STAGE 1 ALGO ====================== *)

module States = struct
  type t =
    | Loading
    | Addr_x | Wait_x | Consume_x
    | Addr_y | Wait_y | Consume_y
    | Addr_z | Wait_z | Consume_z
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo
    ~clock
    ~clear
    ~load_finished
    ~(read_word : Signal.t)
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  let addr = Variable.reg spec ~width:14 in

  let x0 = Variable.reg spec ~width:32 in
  let y0 = Variable.reg spec ~width:32 in
  let z0 = Variable.reg spec ~width:32 in

  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  compile
    [ sm.switch
        [ ( Loading
          , [ when_ load_finished [ sm.set_next Addr_x ] ] )

        ; ( Addr_x
          , [ addr <--. 0
            ; sm.set_next Wait_x
            ] )

        ; ( Wait_x
          , [ sm.set_next Consume_x ] )

        ; ( Consume_x
          , [ x0 <-- read_word
            ; sm.set_next Addr_y
            ] )

        ; ( Addr_y
          , [ addr <--. 1
            ; sm.set_next Wait_y
            ] )

        ; ( Wait_y
          , [ sm.set_next Consume_y ] )

        ; ( Consume_y
          , [ y0 <-- read_word
            ; sm.set_next Addr_z
            ] )

        ; ( Addr_z
          , [ addr <--. 2
            ; sm.set_next Wait_z
            ] )

        ; ( Wait_z
          , [ sm.set_next Consume_z ] )

        ; ( Consume_z
          , [ z0 <-- read_word
            ; sm.set_next Done
            ] )

        ; ( Done
          , [ when_ (done_fired.value ==:. 0)
                [ done_fired <-- vdd ]
            ] )
        ]
    ];

  addr.value, x0.value, y0.value, done_pulse
;;

(* ====================== TOP ====================== *)

let create
    scope
    ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t)
  =
  let loader =
    Loader.hierarchical scope { clock; clear; uart_rx; uart_rts }
  in

  let ram_ports =
    Array.init 2 ~f:(fun _ -> Ram.Port.Of_signal.wires ())
  in

  let%tydi ram =
    Ram.hierarchical ~name:"ram" scope
      { clock
      ; clear
      ; load_ports    = [| loader.ram_write; Ram.Port.unused |]
      ; load_finished = loader.load_finished
      ; ram_ports
      }
  in

  let addr, p1, p2, done_pulse =
    algo
      ~clock
      ~clear
      ~load_finished:loader.load_finished
      ~read_word:ram.read_data.(0)
  in

  (* Drive RAM read address *)
  Ram.Port.Of_signal.assign ram_ports.(0)
    { address      = addr
    ; write_data   = zero 32
    ; write_enable = gnd
    };

  Ram.Port.Of_signal.assign ram_ports.(1)
    { address      = zero 14
    ; write_data   = zero 32
    ; write_enable = gnd
    };

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock
      ; clear
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
