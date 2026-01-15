open! Core
open! Hardcaml

open Day_test

let%expect_test "day12 sample without part2 (input12)" =
  run_sample
    ~day:12
    ~hierarchical:Advent_of_caml.Day12.hierarchical
    ~parser:Advent_of_caml_input_parser.Day12.parse
    ~add_rts:false
    ~cycles:500_000
    ()
  |> print_endline;

  [%expect {|
    Part 1: 
    Part 2:
    |}]
;;