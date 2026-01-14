open! Core
open! Hardcaml

open Day_test

let%expect_test "day05 end-to-end (sample)" =
  run_sample
    ~day:5
    ~hierarchical:Advent_of_caml.Day05.hierarchical
    ~parser:Advent_of_caml_input_parser.Day05.parse
    ~cycles:100_000
    ()
  |> print_endline;

  [%expect {|
    Part 1: 3
    Part 2: 14
    |}]
;;
