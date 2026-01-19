open! Core
open! Hardcaml

open Day_test

let%expect_test "day05 end-to-end (sample)" =
  make_day_test ~day:5 ();
  [%expect {|
    Part 1: 3
    Part 2: 14
    |}]
;;
