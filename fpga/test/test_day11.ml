(* test/test_day11.ml *)

open! Core
open! Hardcaml

module Day11 = Advent_of_caml.Day11
open Test_wrapper

let run_and_dump ~input =
  let sim =
    create
      ~hierarchical:Day11.hierarchical
      ~vcd_file:"/tmp/day11.vcd"
      ()
  in
  let inputs = Advent_of_caml_input_parser.Day11.parse input in
  feed_inputs sim inputs;
  cycle ~n:500_000 sim;
  dump_uart_output sim
;;

let%expect_test "day11 sample without part2 (sample11_1)" =
  run_and_dump ~input:"sample11_1.txt";
  [%expect {| 
    Part 1: 5
    Part 2: 
  |}]
;;

let%expect_test "day11 sample with part2 (sample11_2)" =
  run_and_dump ~input:"sample11_2.txt";
  [%expect {|
    Part 1: 8
    Part 2: 2
    |}]
;;
