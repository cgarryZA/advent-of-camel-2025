(* test/test_day03.ml *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm

open Advent_of_caml.Day03
module Ulx3s = Advent_of_caml.Ulx3s
open Test_wrapper

let%expect_test "day03 end-to-end (sample)" =
  let sim = create ~hierarchical ~vcd_file:"/tmp/day03.vcd" () in

  let inputs =
    Advent_of_caml_input_parser.Day03.parse "sample3.txt"
  in

  (* Count bytes excluding RTS *)
  let byte_count =
    List.count inputs ~f:(function
      | Advent_of_caml_input_parser.Util.Uart_symbol.Byte _ -> true
      | Advent_of_caml_input_parser.Util.Uart_symbol.Rts _ -> false
      | Advent_of_caml_input_parser.Util.Uart_symbol.Stream_word _ -> false)
  in

  feed_inputs sim inputs;

  print_s [%message "Loaded byte_count" (byte_count : int)];

  cycle ~n:200_000 sim;
  dump_uart_output sim;

  [%expect {|
    ("Loaded byte_count" (byte_count 64))
    Part 1: 357
    Part 2: 3121910778619
    |}]

;;
