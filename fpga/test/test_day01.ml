open! Core
open! Hardcaml

open Day_test

let%expect_test "day01 end-to-end (sample)" =
  make_day_test ~day:1 ();
  [%expect {|
    Part 1: 3
    Part 2: 6
    |}]
;;
