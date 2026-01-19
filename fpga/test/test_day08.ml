open! Core
open! Hardcaml

open Day_test

let%expect_test "day08 end-to-end (sample)" =
  make_day_test ~day:8 ();
  [%expect {|
    Part 1: 40
    Part 2: 25272
    |}]
;;