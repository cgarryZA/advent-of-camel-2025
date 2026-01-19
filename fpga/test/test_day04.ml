open! Core
open! Hardcaml

open Day_test

let%expect_test "day04 end-to-end (sample)" =
  make_day_test ~day:4 ();
  [%expect {|
    Part 1: 13
    Part 2: 43
    |}]
;;
