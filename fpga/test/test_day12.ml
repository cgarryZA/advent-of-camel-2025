open! Core
open! Hardcaml

open Day_test

let%expect_test "day12 sample without part2 (input12)" =
  make_day_test ~day:12 ~add_rts:false ~cycles:500_000 ();
  [%expect {|
    Part 1:
    Part 2:
    |}]
;;