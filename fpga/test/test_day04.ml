open! Core
open! Hardcaml

open Day_test

let%expect_test "day04 end-to-end (sample)" =
  run_sample
    ~day:4
    ~hierarchical:Advent_of_caml.Day04.hierarchical
    ~parser:Advent_of_caml_input_parser.Day04.parse
    ~cycles:100_000
    ()
  |> print_endline;

  [%expect {|
    Part 1: 13
    Part 2: 43
    |}]
;;
