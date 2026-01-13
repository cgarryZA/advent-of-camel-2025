open! Core
open! Hardcaml

open Day_test

let%expect_test "day10 end-to-end (sample)" =
  run_sample
    ~day:10
    ~hierarchical:Advent_of_caml.Day10.hierarchical
    ~parser:Advent_of_caml_input_parser.Day10.parse
    ~cycles:100_000
    ()
  |> print_endline;

  [%expect {|
    Part 1: 7
    Part 2:
    |}]
;;