(* src/day09.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth  = 64
let extra_synth_args = []

(* ====================== RAM ====================== *)

module Ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 64
  let depth           = 16384
  let num_ports       = 2
  let zero_on_startup = false
end)

(* ====================== LOADER ====================== *)
(* Input format (words):
   w0 = point_count (u64)
   payload:
     addr 2*i     = x[i]
     addr 2*i + 1 = y[i]
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
      ; point_count   : 'a [@bits 13]
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create _ ({ clock; clear; uart_rx; uart_rts } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~clear () in

    (* pack 8 bytes -> 1x 64-bit word *)
    let word_in = Util.shift_in ~clock ~clear ~n:8 uart_rx in

    (* counts received 64-bit words including header *)
    let word_count =
      reg_fb spec ~width:14 ~enable:word_in.valid ~f:(fun x -> x +:. 1)
    in

    let loaded      = Variable.reg spec ~width:1 in
    let point_count = Variable.reg spec ~width:13 in

    compile
      [ when_ (word_in.valid &: (word_count ==:. 0))
          [ point_count <-- select word_in.value ~high:12 ~low:0 ]
      ; when_ uart_rts
          [ loaded <-- vdd ]
      ]
    ;

    (* payload starts at word 1 *)
    let wr_en   = word_in.valid &: (word_count >=:. 1) in
    let wr_addr = uresize ~width:14 (word_count -:. 1) in

    { O.
      load_finished = loaded.value
    ; ram_write =
        { address      = wr_addr
        ; write_data   = word_in.value
        ; write_enable = wr_en
        }
    ; point_count   = point_count.value
    ; uart_rx_ready = vdd
    }
  ;;

  let hierarchical scope =
    let module S = Hierarchy.In_scope (I) (O) in
    S.hierarchical ~name:"loader" ~scope create
  ;;
end

(* ====================== ALGORITHM ====================== *)

module States = struct
  type t =
    | Loading
    | Read_a
    | Consume_a
    | Read_b
    | Consume_b
    | Advance_b
    | Advance_a
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo
    ~clock
    ~clear
    ~(read_data : Signal.t array)
    ~load_finished
    ~(point_count : Signal.t)
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  let a_idx = Variable.reg spec ~width:13 in
  let b_idx = Variable.reg spec ~width:13 in

  let ax = Variable.reg spec ~width:64 in
  let ay = Variable.reg spec ~width:64 in

  (* IMPORTANT: (|dx|+1) and (|dy|+1) are 64-bit, mul is 128-bit. *)
  let max_area = Variable.reg spec ~width:128 in

  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  (* loop bounds (assumes point_count >= 2) *)
  let a_last = a_idx.value ==: (point_count -:. 2) in
  let b_last = b_idx.value ==: (point_count -:. 1) in

  (* drive RAM addresses: while reading A use a_idx, else use b_idx *)
  let use_a = sm.is Read_a |: sm.is Consume_a in
  let idx   = mux2 use_a a_idx.value b_idx.value in

  let addr_x = uresize ~width:14 (idx @: gnd) in
  let addr_y = uresize ~width:14 (idx @: vdd) in

  (* In Consume_b, read_data.(0/1) are bx/by for the address presented in Read_b. *)
  let bx = read_data.(0) in
  let by = read_data.(1) in

  let dx =
    mux2 (ax.value >=: bx)
      (ax.value -: bx)
      (bx -: ax.value)
  in

  let dy =
    mux2 (ay.value >=: by)
      (ay.value -: by)
      (by -: ay.value)
  in

  (* inclusive tile rectangle area *)
  let w    = dx +:. 1 in
  let h    = dy +:. 1 in
  let area = w *: h in (* 128-bit *)

  let bigger = area >: max_area.value in

  compile
    [ sm.switch
        [ ( Loading
          , [ when_ load_finished
                [ a_idx      <--. 0
                ; b_idx      <--. 1
                ; ax         <--. 0
                ; ay         <--. 0
                ; max_area   <--. 0
                ; done_fired <-- gnd
                ; sm.set_next Read_a
                ]
            ]
          )

        ; ( Read_a
          , [ sm.set_next Consume_a ]
          )

        ; ( Consume_a
          , [ ax <-- read_data.(0)
            ; ay <-- read_data.(1)
            ; b_idx <-- (a_idx.value +:. 1)
            ; sm.set_next Read_b
            ]
          )

        ; ( Read_b
          , [ sm.set_next Consume_b ]
          )

        ; ( Consume_b
          , [ when_ bigger [ max_area <-- area ]
            ; sm.set_next Advance_b
            ]
          )

        ; ( Advance_b
          , [ if_ b_last
                [ sm.set_next Advance_a ]
                [ b_idx <-- (b_idx.value +:. 1)
                ; sm.set_next Read_b
                ]
            ]
          )

        ; ( Advance_a
          , [ if_ a_last
                [ sm.set_next Done ]
                [ a_idx <-- (a_idx.value +:. 1)
                ; sm.set_next Read_a
                ]
            ]
          )

        ; ( Done
          , [ when_ (done_fired.value ==:. 0) [ done_fired <-- vdd ] ]
          )
        ]
    ]
  ;

  addr_x, addr_y, max_area.value, done_pulse
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

  let addr_x, addr_y, max_area_128, done_pulse =
    algo
      ~clock ~clear
      ~read_data:ram.read_data
      ~load_finished:loader.load_finished
      ~point_count:loader.point_count
  in

  Ram.Port.Of_signal.assign ram_ports.(0)
    { address      = addr_x
    ; write_data   = zero 64
    ; write_enable = gnd
    }
  ;

  Ram.Port.Of_signal.assign ram_ports.(1)
    { address      = addr_y
    ; write_data   = zero 64
    ; write_enable = gnd
    }
  ;

  (* Print module in this repo expects both valids to fire (Day05 style). *)
  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 max_area_128; valid = done_pulse }
      ; part2 = { value = zero 60;                    valid = done_pulse }
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
  S.hierarchical ~name:"day09" ~scope create
;;
