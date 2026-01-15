open! Core
open! Hardcaml

open Day_test

let%expect_test "day09 end-to-end (sample)" =
  run_sample
    ~day:9
    ~hierarchical:Advent_of_caml.Day09.hierarchical
    ~parser:Advent_of_caml_input_parser.Day09.parse
    ~cycles:500_000
    ()
  |> print_endline;

  [%expect {|
    Part 1: 50
    Part 2: 24
    |}]
;;
