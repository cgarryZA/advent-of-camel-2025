open! Core
open! Hardcaml

open Day_test

let%expect_test "day03 end-to-end (sample)" =
  run_sample
    ~day:3
    ~hierarchical:Advent_of_caml.Day03.hierarchical
    ~parser:Advent_of_caml_input_parser.Day03.parse
    ~cycles:100_000
    ()
  |> print_endline;

  [%expect {|
    Part 1: 357
    Part 2: 3121910778619
    |}]
;;
