(* test/test_day05.ml *)

open! Core
open! Hardcaml

module Day09 = Advent_of_caml.Day09
open Test_wrapper

let%expect_test "day09 end-to-end (sample)" =
  let sim =
    create
      ~hierarchical:Day09.hierarchical
      ~vcd_file:"/tmp/day09.vcd"
      ()
  in

  let inputs =
    Advent_of_caml_input_parser.Day09.parse "sample9.txt"
    @ [ Advent_of_caml_input_parser.Util.Uart_symbol.Rts true ]
  in

  feed_inputs sim inputs;
  cycle ~n:500_000 sim;

  dump_uart_output sim;

  [%expect {|
    Part 1: 50
    Part 2: 24
    |}]
;;