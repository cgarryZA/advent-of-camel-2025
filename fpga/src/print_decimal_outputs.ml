(* src/print_decimal_outputs.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

(* This has to be 60 because of a lingering bug in hardcaml-circuits *)
let width_bits = 60
let width_chars = Float.to_int (Float.log10 (Float.of_int64 Int64.max_value)) + 1

module Solution = With_valid.Vector (struct
    let width = width_bits
  end)

module Divide_by_constant = Hardcaml_circuits.Divide_by_constant.Make (Signal)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; part1 : 'a Solution.t
    ; part2 : 'a Solution.t
    }
  [@@deriving hardcaml ~rtlmangle:"$"]
end

module O = struct
  type 'a t = { byte_out : 'a Uart.Byte_with_valid.t }
  [@@deriving hardcaml ~rtlmangle:"$"]
end

let bcd_to_ascii x = Signal.of_char '0' +: uresize ~width:8 x

module States = struct
  type t =
    | Idle
    | Active
    | Done
  [@@deriving sexp_of, enumerate]

  let rank = function
    | Idle -> 0
    | Active -> 1
    | Done -> 2

  let compare a b = Int.compare (rank a) (rank b)
  let compare__local = compare
end

let bin_to_decimal ~clock ~clear (x : _ With_valid.t) =
  let open Always in
  let spec = Reg_spec.create ~clock ~clear () in

  let counter = Variable.reg ~width:(num_bits_to_represent width_chars) spec in
  let temp = Variable.reg ~width:width_bits spec in
  let out_valid = Variable.reg ~width:1 spec in

  let div10 = Divide_by_constant.divide ~divisor:(Bigint.of_int 10) temp.value in

  let byte =
    let temp_reg = reg spec temp.value in
    let div10_reg = reg spec div10 in

    (* IMPORTANT: keep arithmetic widths aligned to [width_bits]. *)
    let ten = of_int_trunc ~width:width_bits 10 in
    let prod10 = uresize ~width:width_bits (div10_reg *: ten) in
    let mod10 =
      (temp_reg -: prod10) |> sel_bottom ~width:4
    in

    mux2 (temp_reg ==:. 0) (zero 8) (bcd_to_ascii mod10)
  in

  let mod10_valid = Variable.reg ~width:1 spec in
  let out =
    reg_fb spec ~width:(width_chars * 8) ~enable:mod10_valid.value ~f:(fun acc ->
      byte @: drop_bottom ~width:8 acc)
  in

  let sm = State_machine.create (module States) spec in
  compile
    [ mod10_valid <-- gnd
    ; out_valid <-- gnd
    ; sm.switch
        [ States.Idle
        , [ when_ x.valid
              [ temp <-- x.value
              ; counter <--. 0
              ; sm.set_next States.Active
              ]
          ]
        ; ( States.Active
          , [ counter <-- counter.value +:. 1
            ; mod10_valid <-- vdd
            ; temp <-- uresize ~width:width_bits div10
            ; when_ (counter.value ==:. width_chars - 1) [ sm.set_next States.Done ]
            ] )
        ; States.Done, [ out_valid <-- vdd ]
        ]
    ];
  out_valid.value, out
;;

let create scope ({ clock; clear; part1; part2 } : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~clear () in

  let part1_valid, part1_reg = bin_to_decimal ~clock ~clear part1 in
  let part2_valid, part2_reg = bin_to_decimal ~clock ~clear part2 in

  let signal_of_string s =
    s |> String.to_list |> List.map ~f:Signal.of_char |> concat_msb
  in

  let output_string =
    concat_msb
      [ signal_of_string "Part 1: "
      ; part1_reg
      ; signal_of_string "\nPart 2: "
      ; part2_reg
      ; signal_of_string "\n"
      ]
  in

  let total_chars = width output_string / 8 in
  let enable = part1_valid &: part2_valid in

  let counter =
    reg_fb spec ~width:(num_bits_to_represent total_chars) ~f:(fun c ->
      mux2 (enable &: (c <>:. total_chars)) (c +:. 1) c)
  in

  let byte = output_string |> split_msb ~exact:true ~part_width:8 |> mux counter in

  { byte_out =
      { valid = enable &: (counter <>:. total_chars) &: (byte <>:. 0)
      ; value = byte
      }
  }
  |> O.Of_signal.reg spec
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~name:"print_decimal_outputs" ~scope create
;;
