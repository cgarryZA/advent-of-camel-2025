(* test/test_day04.ml *)

open! Core
open! Hardcaml

module Day04 = Advent_of_caml.Day04
open Test_wrapper

let%expect_test "day04 end-to-end (sample)" =
  let sim =
  create
    ~hierarchical:Day04.hierarchical
    ~vcd_file:"/tmp/day04.vcd"
    ()
  in

  let inputs =
    Advent_of_caml_input_parser.Day04.parse "input4.txt"
    @ [ Advent_of_caml_input_parser.Util.Uart_symbol.Rts true ]
  in

  feed_inputs sim inputs;
  cycle ~n:500_000 sim;

  dump_uart_output sim;

  [%expect {|
    Part 1: 13
    Part 2: 43
    |}]
;;
