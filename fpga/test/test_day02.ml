(* test/test_day02.ml *)

open! Core
open! Hardcaml

open Advent_of_caml.Day02
module Ulx3s = Advent_of_caml.Ulx3s
open Test_wrapper

let%expect_test "day02 end-to-end (sample)" =
  let sim = create ~hierarchical ~vcd_file:"/tmp/day02.vcd" () in

  let inputs =
    Advent_of_caml_input_parser.Day02.parse "sample2.txt"
  in

  feed_inputs sim inputs;

  cycle ~n:500_000 sim;
  dump_uart_output sim;

  [%expect {|
    Part 1: 1227775554
    Part 2: 4174379265
  |}]
;;