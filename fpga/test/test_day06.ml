open! Core
open! Hardcaml

open Day_test

let%expect_test "day06 end-to-end (sample)" =
  run_sample
    ~day:6
    ~hierarchical:Advent_of_caml.Day06.hierarchical
    ~parser:Advent_of_caml_input_parser.Day06.parse
    ~cycles:100_000
    ()
  |> print_endline;

  [%expect {|
    Part 1: 4277556
    Part 2: 3263827
    |}]
;;
