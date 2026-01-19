open! Core
open! Hardcaml

open Day_test

let%expect_test "day09 end-to-end (sample)" =
  make_day_test ~day:9 ~cycles:500_000 ();
  [%expect {|
    Part 1: 50
    Part 2: 24
    |}]
;;
