(* test/test_day10.ml *)

open! Core
open! Hardcaml

module Day10 = Advent_of_caml.Day10
open Test_wrapper

let%expect_test "day10 end-to-end (sample)" =
  let sim =
    create
      ~hierarchical:Day10.hierarchical
      ~vcd_file:"/tmp/day10.vcd"
      ()
  in

  let inputs =
    Advent_of_caml_input_parser.Day10.parse "sample10.txt"
  in

  feed_inputs sim inputs;
  cycle ~n:500_000 sim;

  dump_uart_output sim;

  [%expect {|
    Part 1: 7
    Part 2:
    |}]
;;