open! Core
open! Hardcaml

open Day_test

let%expect_test "day03 end-to-end (sample)" =
  make_day_test ~day:3 ();
  [%expect {|
    Part 1: 357
    Part 2: 3121910778619
    |}]
;;
