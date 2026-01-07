(* test/test_day05.ml *)

open! Core
open! Hardcaml

module Day05 = Advent_of_caml.Day05
open Test_wrapper

let%expect_test "day05 end-to-end (sample)" =
  let sim =
    create
      ~hierarchical:Day05.hierarchical
      ~vcd_file:"/tmp/day05.vcd"
      ()
  in

  let inputs =
    Advent_of_caml_input_parser.Day05.parse "sample5.txt"
    @ [ Advent_of_caml_input_parser.Util.Uart_symbol.Rts true ]
  in

  feed_inputs sim inputs;
  cycle ~n:500_000 sim;

  dump_uart_output sim;

  [%expect {|
    Part 1: 3
    Part 2: 14
    |}]
;;