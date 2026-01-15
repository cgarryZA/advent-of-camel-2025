open! Core
open! Hardcaml

open Day_test

let%expect_test "day07 end-to-end (sample)" =
  run_sample
    ~day:7
    ~hierarchical:Advent_of_caml.Day07.hierarchical
    ~parser:(Advent_of_caml_input_parser.Day07.parse ~verbose:false)
    ~cycles:100_000
    ()
  |> print_endline;

  [%expect {|
    Part 1: 2100
    Part 2: 25272
    |}]
;;