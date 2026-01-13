open! Core
open! Hardcaml

open Day_test

let%expect_test "day11 sample without part2 (sample11_1)" =
  run_sample
    ~day:11
    ~hierarchical:Advent_of_caml.Day11.hierarchical
    ~parser:Advent_of_caml_input_parser.Day11.parse
    ~input:"sample11_1.txt"
    ~add_rts:false
    ~cycles:500_000
    ()
  |> print_endline;

  [%expect {|
    Part 1: 5
    Part 2:
    |}]
;;

let%expect_test "day11 sample with part2 (sample11_2)" =
  run_sample
    ~day:11
    ~hierarchical:Advent_of_caml.Day11.hierarchical
    ~parser:Advent_of_caml_input_parser.Day11.parse
    ~input:"sample11_2.txt"
    ~add_rts:false
    ~cycles:500_000
    ()
  |> print_endline;

  [%expect {|
    Part 1: 8
    Part 2: 2
    |}]
;;
