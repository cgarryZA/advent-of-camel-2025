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

    (* one-time per-range init: lo(binary) -> cur_bcd *)
    | Bcd_div

    (* 1 cycle per ID *)
    | Iterate

    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

(* ====================== BCD / DIGIT HELPERS ====================== *)

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

(* Place digit at nibble idx (0 = least significant digit nibble) *)
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

(* Increment a BCD value stored little-endian in nibbles (LS digit at nibble 0). *)
let bcd_incr (x : Signal.t) : Signal.t =
  assert (width x % 4 = 0);
  let rec go ~carry = function
    | [] -> []
    | d :: ds ->
      let is9 = d ==:. 9 in
      let next_d =
        mux2 carry (mux2 is9 (zero 4) (d +:. 1)) d
      in
      let carry = carry &: is9 in
      next_d :: go ds ~carry
  in
  x |> split_lsb ~exact:true ~part_width:4 |> go ~carry:vdd |> concat_lsb
;;

(* "x has exactly n digits (no leading zeros)" for little-endian BCD. *)
let bcd_is_length ~(n : int) (x : Signal.t) : Signal.t =
  if n <= 0 || n > max_digits then gnd
  else
    let msd =
      select x ~high:(4*n - 1) ~low:(4*(n-1))
    in
    let upper_ok =
      if digits_bits = 4*n then vdd
      else
        let hi = select x ~high:(digits_bits - 1) ~low:(4*n) in
        hi ==:. 0
    in
    upper_ok &: (msd <>:. 0)
;;

let repeated_substring_check ~(digits : Signal.t) ~(len : int) ~(div : int) : Signal.t =
  (* Preconditions handled by caller: div divides len, and len/div >= 2 *)
  let slice = sel_bottom digits ~width:(4 * len) in
  let chunks = split_lsb slice ~exact:true ~part_width:(4 * div) in
  match chunks with
  | [] | [_] -> gnd
  | first :: rest ->
    List.map rest ~f:(fun c -> c ==: first)
    |> List.fold ~init:vdd ~f:( &: )
;;

let invalid_for_part1 ~(id_bcd : Signal.t) ~(id_valid : Signal.t) : Signal.t =
  (* Part 1: exactly two repeats => length even, div = length/2, halves equal *)
  List.init (max_digits - 1) ~f:(fun i ->
      let len = i + 2 in
      if len % 2 <> 0 then gnd
      else
        let div = len / 2 in
        let matches_len = bcd_is_length ~n:len id_bcd in
        id_valid &: matches_len &: repeated_substring_check ~digits:id_bcd ~len ~div
    )
  |> List.fold ~init:gnd ~f:( |: )
;;

let invalid_for_part2 ~(id_bcd : Signal.t) ~(id_valid : Signal.t) : Signal.t =
  (* Part 2: repeats at least twice => any div where div|len and len/div>=2 *)
  List.init (max_digits - 1) ~f:(fun i ->
      let len = i + 2 in
      let matches_len = bcd_is_length ~n:len id_bcd in
      let any_div =
        List.init (len - 1) ~f:(fun j ->
            let div = j + 1 in
            if (len % div = 0) && (len / div >= 2)
            then repeated_substring_check ~digits:id_bcd ~len ~div
            else gnd
          )
        |> List.fold ~init:gnd ~f:( |: )
      in
      id_valid &: matches_len &: any_div
    )
  |> List.fold ~init:gnd ~f:( |: )
;;

(* ====================== STREAMING-BCD ALGORITHM ====================== *)

let algo ~clock ~clear ~read_word ~data_words ~load_finished =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  (* global cycle counter *)
  let cycles = Variable.reg spec ~width:64 in

  (* RAM address *)
  let rd_word = Variable.reg spec ~width:14 in

  (* assemble 64-bit endpoints from 2x 32-bit words *)
  let lo_lo = Variable.reg spec ~width:32 in
  let hi_lo = Variable.reg spec ~width:32 in
  let lo    = Variable.reg spec ~width:64 in
  let hi    = Variable.reg spec ~width:64 in

  (* iteration: binary for compare+sum, BCD for pattern checks *)
  let cur_bin = Variable.reg spec ~width:64 in
  let cur_bcd = Variable.reg spec ~width:digits_bits in

  (* one-time conversion workspace (per range): lo (binary) -> cur_bcd *)
  let tmp_bin = Variable.reg spec ~width:64 in
  let bcd_idx = Variable.reg spec ~width:5 in

  (* outputs *)
  let part1 = Variable.reg spec ~width:64 in
  let part2 = Variable.reg spec ~width:64 in

  (* one-shot done pulse *)
  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  (* combinational invalid checks (active only in Iterate) *)
  let id_valid = sm.is Iterate in
  let inv1 = invalid_for_part1 ~id_bcd:cur_bcd.value ~id_valid in
  let inv2 = invalid_for_part2 ~id_bcd:cur_bcd.value ~id_valid in

  (* conversion step wires *)
  let q10, r10 = div10_u64 tmp_bin.value in
  let placed   = place_digit_ls ~idx:bcd_idx.value ~digit4:r10 in

  compile
    [ cycles <-- cycles.value +:. 1
    ; sm.switch
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

          (* seed iteration state *)
          ; cur_bin <-- lo.value

          (* begin one-time lo -> bcd conversion *)
          ; tmp_bin <-- lo.value
          ; bcd_idx <--. 0
          ; cur_bcd <-- zero digits_bits

          ; sm.set_next Bcd_div
          ]

        (* ---- one-time convert lo (binary) -> cur_bcd (BCD digits) ---- *)
        ; Bcd_div,
          [ cur_bcd <-- (cur_bcd.value |: placed)
          ; bcd_idx <-- bcd_idx.value +:. 1
          ; tmp_bin <-- q10
          ; if_ (q10 ==:. 0)
              [ sm.set_next Iterate ]
              []
          ]

        (* ---- iterate range: 1 cycle per ID ---- *)
        ; Iterate,
          [ when_ inv1 [ part1 <-- part1.value +: cur_bin.value ]
          ; when_ inv2 [ part2 <-- part2.value +: cur_bin.value ]

          ; if_ (cur_bin.value ==: hi.value)
              [ (* finished this range *)
                if_ (rd_word.value +:. 3 <: data_words)
                    [ sm.set_next Lo_lo_read ]
                    [ sm.set_next Done ]
              ]
              [ (* advance to next ID *)
                cur_bin <-- cur_bin.value +:. 1
              ; cur_bcd <-- bcd_incr cur_bcd.value
              ]
          ]

        ; Done,
          [ when_ (done_fired.value ==:. 0)
              [ done_fired <-- vdd 
              ; cycles <-- cycles.value
              ]
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

  (* Read port: only active after load_finished, to avoid weird early addresses *)
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
      ; extra = { value = uresize ~width:60 cycles.value; valid = done_pulse }
      }
  in

  { Ulx3s.O.
    leds =
      concat_lsb
        [ ~:clear
        ; uart_rx_overflow
        ; loader.load_finished
        ; done_pulse
        ; zero 4
        ]
  ; uart_tx       = byte_out
  ; uart_rx_ready = loader.uart_rx_ready
  }
;;

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day02" ~scope create
;;
