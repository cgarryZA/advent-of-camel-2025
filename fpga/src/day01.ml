(* src/day01.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth  = 32
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
      ; data_length   : 'a [@bits 13]
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create _scope ({ clock; clear; uart_rx; uart_rts } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~clear () in

    (* pack 4 bytes -> 1x 32-bit word *)
    let word_in = Util.shift_in ~clock ~clear ~n:4 uart_rx in

    (* count 32-bit words written *)
    let word_count =
      reg_fb spec ~width:14 ~enable:word_in.valid ~f:(fun x -> x +:. 1)
    in

    (* sticky end-of-load *)
    let loaded = Variable.reg spec ~width:1 in

    (* Freeze instruction count on RTS.
       data_length is number of *instructions* (word_count / 2). *)
    let instr_len = Variable.reg spec ~width:13 in

    (* If RTS happens in the same cycle as the last word write, include it. *)
    let word_count_written = mux2 word_in.valid (word_count +:. 1) word_count in

    compile
      [ when_ uart_rts
          [ loaded    <-- vdd
          ; instr_len <-- select word_count_written ~high:13 ~low:1
          ]
      ];

    O.
      { load_finished = loaded.value
      ; ram_write =
          { address      = word_count
          ; write_data   = word_in.value
          ; write_enable = word_in.valid
          }
      ; data_length   = instr_len.value
      ; uart_rx_ready = vdd
      }
  ;;

  let hierarchical scope =
    let module S = Hierarchy.In_scope (I) (O) in
    S.hierarchical ~name:"loader" ~scope create
  ;;
end

(* ====================== ALGORITHM ====================== *)

let dial_bits = 7
let count_bits = 32

let div100_u32 x =
  let x     = uresize ~width:32 x in
  let magic = of_int_trunc ~width:32 0x51EB851F in
  let prod  = x *: magic in
  select prod ~high:63 ~low:37 |> uresize ~width:32
;;

let mod100_u32_to7 x =
  let q = div100_u32 x in
  let r = uresize ~width:39 x -: (q *: of_int_trunc ~width:7 100) in
  uresize ~width:7 r
;;

module States = struct
  type t = Loading | Running | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo ~clock ~clear ~(read_data : Signal.t array) ~load_finished ~data_length =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm = State_machine.create (module States) spec in

  (*   - fetch_idx drives addresses (instruction to be available next cycle)
       - consume_idx counts which instruction we're consuming this cycle
       - primed=0 for the first cycle of Running (no valid instruction yet) *)
  let fetch_idx   = Variable.reg spec ~width:13 in
  let consume_idx = Variable.reg spec ~width:13 in
  let primed      = Variable.reg spec ~width:1 in

  let value = Variable.reg spec ~width:dial_bits in
  let p1    = Variable.reg spec ~width:count_bits in
  let p2    = Variable.reg spec ~width:count_bits in

  (* Delay done by 1 cycle to let the last update settle. *)
  let done_pending = Variable.reg spec ~width:1 in
  let done_out     = reg spec done_pending.value in

  let addr_dir   = (fetch_idx.value @: gnd) |> uresize ~width:14 in
  let addr_steps = (fetch_idx.value @: vdd) |> uresize ~width:14 in

  let dir_word   = read_data.(0) in
  let steps_word = read_data.(1) in

  (* 0=Left(dec), 1=Right(inc) *)
  let direction = lsb dir_word in
  let steps     = steps_word in

  (* final value only depends on steps mod 100 *)
  let delta  = mod100_u32_to7 steps in
  let value9 = uresize ~width:9 value.value in
  let delta9 = uresize ~width:9 delta in

  let next_value =
    mux2 direction
      (* Right: (val + delta) % 100 *)
      (uresize ~width:7
         (mux2
            (value9 +: delta9 >=:. 100)
            (value9 +: delta9 -:. 100)
            (value9 +: delta9)))
      (* Left: (val - delta + 100) % 100 *)
      (uresize ~width:7
         (mux2
            (value9 >=: delta9)
            (value9 -: delta9)
            (value9 +:. 100 -: delta9)))
  in

  let ends_at_zero = next_value ==:. 0 in

  (* ---------------- Part 2: count "arrivals at 0" on clicks ----------------

     Let val in [0..99]. We count clicks k = 1..steps such that position after k clicks is 0.

     Distance (in clicks) to the *first arrival at 0*:
       Right: if val=0 -> 100 else 100 - val
       Left : if val=0 -> 100 else val

     After the first arrival, you arrive at 0 every additional 100 clicks.

     Number of arrivals:
       if steps < first -> 0
       else 1 + floor((steps - first) / 100)

     This includes the endpoint when it lands on 0.
  *)
  let val32 = uresize ~width:32 value.value in

  let first_right =
    mux2 (value.value ==:. 0)
      (of_int_trunc ~width:32 100)
      (of_int_trunc ~width:32 100 -: val32)
  in
  let first_left =
    mux2 (value.value ==:. 0)
      (of_int_trunc ~width:32 100)
      val32
  in

  let first = mux2 direction first_right first_left in

  let hits =
    mux2 (steps >=: first)
      (div100_u32 (steps -: first) +:. 1)
      (zero 32)
  in

  let last_fetch   = fetch_idx.value   ==: (data_length -:. 1) in
  let last_consume = consume_idx.value ==: (data_length -:. 1) in

  compile
    [ sm.switch
        [ ( Loading
          , [ when_ load_finished
                [ fetch_idx    <--. 0
                ; consume_idx  <--. 0
                ; primed       <-- gnd
                ; done_pending <-- gnd
                ; p1           <-- zero count_bits
                ; p2           <-- zero count_bits
                ; value        <-- of_int_trunc ~width:dial_bits 50
                ; sm.set_next Running
                ]
            ] )
        ; ( Running
          , [ when_ 
                (~:last_fetch) 
                  [ fetch_idx <-- fetch_idx.value +:. 1 ]
            ; if_
                (primed.value ==:. 0)
                [ primed <-- vdd ]
                [ value  <-- next_value
                ; p2     <-- p2.value +: hits
                ; when_ 
                  (ends_at_zero)
                    [ p1 <-- p1.value +:. 1 ]
                ; if_ 
                    (last_consume)
                      [ done_pending <-- vdd
                      ; sm.set_next Done
                      ]
                      [ consume_idx <-- consume_idx.value +:. 1 ]
                ]
            ] )
        ; Done, [ done_pending <-- vdd ]
        ]
    ];

  addr_dir, addr_steps, p1.value, p2.value, done_out
;;

(* ====================== TOP ====================== *)

let create
    scope
    ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t)
  =
  let loader_out =
    Loader.hierarchical scope { clock; clear; uart_rx; uart_rts }
  in
  let load_finished = loader_out.load_finished in
  let ram_write     = loader_out.ram_write in
  let data_length   = loader_out.data_length in
  let uart_rx_ready = loader_out.uart_rx_ready in

  let ram_ports = Array.init 2 ~f:(fun _ -> Ram.Port.Of_signal.wires ()) in

  let%tydi ram_out =
    Ram.hierarchical ~name:"ram" scope
      { clock
      ; clear
      ; load_ports = [| ram_write; Ram.Port.unused |]
      ; load_finished
      ; ram_ports
      }
  in
  let read_data = ram_out.read_data in

  let addr0, addr1, p1, p2, done_ =
    algo ~clock ~clear ~read_data ~load_finished ~data_length
  in

  Ram.Port.Of_signal.assign ram_ports.(0)
    { address      = mux2 load_finished addr0 (zero 14)
    ; write_data   = zero 32
    ; write_enable = gnd
    };

  Ram.Port.Of_signal.assign ram_ports.(1)
    { address      = mux2 load_finished addr1 (zero 14)
    ; write_data   = zero 32
    ; write_enable = gnd
    };

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 p1; valid = done_ }
      ; part2 = { value = uresize ~width:60 p2; valid = done_ }
      }
  in

  { Ulx3s.O.
    leds = concat_lsb [ ~:clear; uart_rx_overflow; load_finished; zero 5 ]
  ; uart_tx = byte_out
  ; uart_rx_ready
  }
;;

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day01" ~scope create
;;
