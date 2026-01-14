open! Core
open! Hardcaml

open Day_test

let%expect_test "day02 end-to-end (sample)" =
  run_sample
    ~day:2
    ~hierarchical:Advent_of_caml.Day02.hierarchical
    ~parser:Advent_of_caml_input_parser.Day02.parse
    ~cycles:100_000
    ()
  |> print_endline;

  [%expect {|
    Part 1: 1227775554
    Part 2: 4174379265
    |}]
;;
