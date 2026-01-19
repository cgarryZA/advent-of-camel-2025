open! Core
open! Hardcaml

open Day_test

let%expect_test "day06 end-to-end (sample)" =
  make_day_test ~day:6 ();
  [%expect {|
    Part 1: 4277556
    Part 2: 3263827
    |}]
;;
