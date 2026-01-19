open! Core
open! Hardcaml

open Day_test

let%expect_test "day02 end-to-end (sample)" =
  make_day_test ~day:2 ();
  [%expect {|
    Part 1: 1227775554
    Part 2: 4174379265
    |}]
;;
