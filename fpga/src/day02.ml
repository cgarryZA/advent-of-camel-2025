(* src/day02.ml *)

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
      ; data_words    : 'a [@bits 14]
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

    let loaded     = Variable.reg spec ~width:1 in
    let data_words = Variable.reg spec ~width:14 in

    (* If RTS happens in the same cycle as the last word write, include it. *)
    let word_count_written =
      mux2 word_in.valid (word_count +:. 1) word_count
    in

    compile
      [ when_ uart_rts
          [ loaded     <-- vdd
          ; data_words <-- word_count_written
          ]
      ]
    ;

    { O.
      load_finished = loaded.value
    ; ram_write =
        { address      = word_count
        ; write_data   = word_in.value
        ; write_enable = word_in.valid
        }
    ; data_words    = data_words.value
    ; uart_rx_ready = vdd
    }
  ;;

  let hierarchical scope =
    let module S = Hierarchy.In_scope (I) (O) in
    S.hierarchical ~name:"loader" ~scope create
  ;;
end

(* ====================== FSM ====================== *)

module States = struct
  type t =
    | Loading
    | Lo_lo_read
    | Lo_lo_consume
    | Lo_hi_read
    | Lo_hi_consume
    | Hi_lo_read
    | Hi_lo_consume
    | Hi_hi_read
    | Hi_hi_consume
    | Count_check
    | Dec_init
    | Dec_div
    | Check
    | Count_step
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

(* ====================== DECIMAL HELPERS ====================== *)

let max_digits  = 20
let digits_bits = max_digits * 4

let div10_u64 (x : Signal.t) =
  let x = uresize ~width:64 x in
  let magic = of_int64_trunc ~width:64 0xCCCCCCCCCCCCCCCDL in
  let prod  = x *: magic in
  let q = select prod ~high:127 ~low:67 |> uresize ~width:64 in
  let q10 = uresize ~width:64 (q *: of_int_trunc ~width:64 10) in
  let r = x -: q10 in
  q, uresize ~width:4 r
;;

let place_digit_ls ~(idx : Signal.t) ~(digit4 : Signal.t) =
  let mk k =
    if k < max_digits then
      let upper_w = digits_bits - 4 - (4 * k) in
      let lower_w = 4 * k in
      let parts =
        (if upper_w > 0 then [ zero upper_w ] else [])
        @ [ digit4 ]
        @ (if lower_w > 0 then [ zero lower_w ] else [])
      in
      concat_msb parts
    else
      zero digits_bits
  in
  mux idx (List.init 32 ~f:mk)
;;

let repeats_check ~(digits : Signal.t) ~(len : Signal.t) ~(s : int) =
  let ok_len = len >=:. (2*s) in
  let all =
    List.init max_digits ~f:(fun i ->
      let in_range = len >:. i in
      let d0 =
        select digits ~high:(4*((i mod s)+1)-1) ~low:(4*(i mod s))
      in
      let di =
        select digits ~high:(4*(i+1)-1) ~low:(4*i)
      in
      (~:in_range) |: (d0 ==: di))
    |> List.fold ~init:vdd ~f:( &: )
  in
  ok_len &: all
;;

(* ====================== ALGORITHM ====================== *)

let algo ~clock ~clear ~read_word ~data_words ~load_finished =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  (* RAM address *)
  let rd_word = Variable.reg spec ~width:14 in

  (* assemble 64-bit endpoints from 2x 32-bit words *)
  let lo_lo = Variable.reg spec ~width:32 in
  let hi_lo = Variable.reg spec ~width:32 in
  let lo    = Variable.reg spec ~width:64 in
  let hi    = Variable.reg spec ~width:64 in

  (* iteration *)
  let cur = Variable.reg spec ~width:64 in

  (* decimal extraction *)
  let tmp       = Variable.reg spec ~width:64 in
  let dec_len   = Variable.reg spec ~width:5 in
  let digits_ls = Variable.reg spec ~width:digits_bits in

  (* outputs *)
  let part1 = Variable.reg spec ~width:64 in
  let part2 = Variable.reg spec ~width:64 in

  (* one-shot done pulse *)
  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  (* div/mod10 for tmp *)
  let q, r = div10_u64 tmp.value in
  let placed = place_digit_ls ~idx:dec_len.value ~digit4:r in

  (* part1: exactly two repeats *)
  let part1_hit =
    List.init (max_digits / 2) ~f:(fun i ->
      let s = i + 1 in
      (dec_len.value ==:. (2*s))
      &: repeats_check ~digits:digits_ls.value ~len:dec_len.value ~s)
    |> List.fold ~init:gnd ~f:( |: )
  in

  (* part2: repeats at least twice => len is a multiple of s, and >= 2*s *)
  let part2_hit =
    List.init (max_digits / 2) ~f:(fun i ->
      let s = i + 1 in

      (* dec_len is a multiple of s, and >= 2*s *)
      let mult_ok =
        List.init 32 ~f:(fun k ->
          if k mod s = 0 && k >= 2*s then
            dec_len.value ==:. k
          else
            gnd)
        |> List.fold ~init:gnd ~f:( |: )
      in

      repeats_check ~digits:digits_ls.value ~len:dec_len.value ~s
      &: mult_ok)
    |> List.fold ~init:gnd ~f:( |: )
  in

  compile
    [ sm.switch
        [ Loading,
          [ when_ load_finished
              [ rd_word    <--. 0
              ; part1      <--. 0
              ; part2      <--. 0
              ; done_fired <-- gnd
              ; sm.set_next Lo_lo_read
              ]
          ]

        (* ---- read 64-bit LO (two 32-bit words) ---- *)
        ; Lo_lo_read,     [ sm.set_next Lo_lo_consume ]
        ; Lo_lo_consume,
          [ lo_lo   <-- read_word
          ; rd_word <-- rd_word.value +:. 1
          ; sm.set_next Lo_hi_read
          ]
        ; Lo_hi_read,     [ sm.set_next Lo_hi_consume ]
        ; Lo_hi_consume,
          [ lo      <-- concat_msb [ read_word; lo_lo.value ]
          ; rd_word <-- rd_word.value +:. 1
          ; sm.set_next Hi_lo_read
          ]

        (* ---- read 64-bit HI (two 32-bit words) ---- *)
        ; Hi_lo_read,     [ sm.set_next Hi_lo_consume ]
        ; Hi_lo_consume,
          [ hi_lo   <-- read_word
          ; rd_word <-- rd_word.value +:. 1
          ; sm.set_next Hi_hi_read
          ]
        ; Hi_hi_read,     [ sm.set_next Hi_hi_consume ]
        ; Hi_hi_consume,
          [ hi      <-- concat_msb [ read_word; hi_lo.value ]
          ; rd_word <-- rd_word.value +:. 1
          ; cur     <-- lo.value
          ; sm.set_next Count_check
          ]

        (* ---- iterate range ---- *)
        ; Count_check,
          [ if_ (cur.value <=: hi.value)
              [ sm.set_next Dec_init ]
              [ if_ (rd_word.value +:. 3 <: data_words)
                  [ sm.set_next Lo_lo_read ]
                  [ sm.set_next Done ]
              ]
          ]

        (* ---- decimal extraction ---- *)
        ; Dec_init,
          [ tmp       <-- cur.value
          ; dec_len   <--. 0
          ; digits_ls <-- zero digits_bits
          ; sm.set_next Dec_div
          ]

        ; Dec_div,
          [ digits_ls <-- (digits_ls.value |: placed)
          ; dec_len   <-- dec_len.value +:. 1
          ; tmp       <-- q
          ; if_ (q ==:. 0) [ sm.set_next Check ] []
          ]

        ; Check,
          [ when_ part1_hit [ part1 <-- part1.value +: cur.value ]
          ; when_ part2_hit [ part2 <-- part2.value +: cur.value ]
          ; sm.set_next Count_step
          ]

        ; Count_step,
          [ cur <-- cur.value +:. 1
          ; sm.set_next Count_check
          ]

        ; Done,
          [ when_ (done_fired.value ==:. 0)
              [ done_fired <-- vdd ]
          ]
        ]
    ]
  ;

  rd_word.value, part1.value, part2.value, done_pulse
;;

(* ====================== TOP ====================== *)

let create
    scope
    ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t)
  =
  let loader = Loader.hierarchical scope { clock; clear; uart_rx; uart_rts } in

  let ram_ports = Array.init 2 ~f:(fun _ -> Ram.Port.Of_signal.wires ()) in

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
    algo ~clock ~clear
      ~read_word:ram.read_data.(0)
      ~data_words:loader.data_words
      ~load_finished:loader.load_finished
  in

  Ram.Port.Of_signal.assign ram_ports.(0)
    { address      = mux2 loader.load_finished addr (zero 14)
    ; write_data   = zero 32
    ; write_enable = gnd
    }
  ;

  Ram.Port.Of_signal.assign ram_ports.(1)
    { address      = zero 14
    ; write_data   = zero 32
    ; write_enable = gnd
    }
  ;

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
  S.hierarchical ~name:"day02" ~scope create
;;
