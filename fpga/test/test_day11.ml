open! Core
open! Hardcaml

open Day_test

let%expect_test "day11 sample without part2 (sample11_1)" =
  make_day_test ~day:11 ~input:"sample11_1.txt" ~add_rts:false ~cycles:500_000 ();
  [%expect {|
    Part 1: 5
    Part 2:
    |}]
;;

let%expect_test "day11 sample with part2 (sample11_2)" =
  make_day_test ~day:11 ~input:"sample11_2.txt" ~add_rts:false ~cycles:500_000 ();
  [%expect {|
    Part 1: 8
    Part 2: 2
    |}]
;;
