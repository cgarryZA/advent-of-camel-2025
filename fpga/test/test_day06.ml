(* test/test_day05.ml *)

open! Core
open! Hardcaml

module Day06 = Advent_of_caml.Day06
open Test_wrapper

let%expect_test "day06 end-to-end (sample)" =
  let sim =
    create
      ~hierarchical:Day06.hierarchical
      ~vcd_file:"/tmp/day06.vcd"
      ()
  in

  let inputs =
    Advent_of_caml_input_parser.Day06.parse "sample6.txt"
    @ [ Advent_of_caml_input_parser.Util.Uart_symbol.Rts true ]
  in

  feed_inputs sim inputs;
  cycle ~n:500_000 sim;

  dump_uart_output sim;

  [%expect {|
    Part 1: 4277556
    Part 2: 1
    |}]
;;