(* test/test_day07.ml *)

open! Core
open! Hardcaml

module Day07 = Advent_of_caml.Day07
open Test_wrapper

let%expect_test "day07 end-to-end (sample)" =
  let sim =
    create
      ~hierarchical:Day07.hierarchical
      ~vcd_file:"/tmp/day07.vcd"
      ()
  in

  let inputs =
    Advent_of_caml_input_parser.Day07.parse "sample7.txt"
    @ [ Advent_of_caml_input_parser.Util.Uart_symbol.Rts true ]
  in

  feed_inputs sim inputs;
  cycle ~n:500_000 sim;

  dump_uart_output sim;

  [%expect {|
    Part 1: 21
    Part 2: 3263827
    |}]
;;
