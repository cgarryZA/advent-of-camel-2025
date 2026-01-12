(* test/test_day11.ml *)

open! Core
open! Hardcaml

module Day11 = Advent_of_caml.Day11
open Test_wrapper

let%expect_test "day11 end-to-end (sample)" =
  let sim =
    create
      ~hierarchical:Day11.hierarchical
      ~vcd_file:"/tmp/day11.vcd"
      ()
  in

  let inputs =
    Advent_of_caml_input_parser.Day11.parse "sample11.txt"
  in

  feed_inputs sim inputs;
  cycle ~n:500_000 sim;

  dump_uart_output sim;

  [%expect {|
    Part 1: 5
    Part 2: 2
    |}]
;;
