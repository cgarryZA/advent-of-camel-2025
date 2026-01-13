open! Core
open! Hardcaml

open Day_test

let%expect_test "day08 end-to-end (sample)" =
  run_sample
    ~day:8
    ~hierarchical:Advent_of_caml.Day08.hierarchical
    ~parser:Advent_of_caml_input_parser.Day08.parse
    ~cycles:100_000
    ()
  |> print_endline;

  [%expect {|
    Part 1: 40
    Part 2:
    |}]
;;