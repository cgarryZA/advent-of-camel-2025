open! Core
open! Hardcaml
open! Hardcaml_test_harness

module Day1 = Advent_of_caml.Day1_dial
module Harness = Cyclesim_harness.Make (Day1.I) (Day1.O)

let ( <--. ) = Bits.( <--. )

let example =
  [ "L68"
  ; "L30"
  ; "R48"
  ; "L5"
  ; "R60"
  ; "L55"
  ; "L1"
  ; "L99"
  ; "R14"
  ; "L82"
  ]

let parse (s : string) =
  let dir = if Char.(s.[0] = 'L') then 0 else 1 in
  let steps = Int.of_string (String.sub s ~pos:1 ~len:(String.length s - 1)) in
  dir, steps
;;

let simple_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in

  let wait_ready () =
    while not (Bits.to_bool !(outputs.ready)) do
      cycle ()
    done
  in

  let send_instruction (dir, steps) =
    wait_ready ();
    inputs.direction <--. dir;
    inputs.steps <--. steps;
    inputs.valid := Bits.vdd;
    cycle ();
    inputs.valid := Bits.gnd;
    cycle ()
  in

  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();

  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  cycle ();

  List.iter example ~f:(fun s -> send_instruction (parse s));

  wait_ready ();
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();

  while not (Bits.to_bool !(outputs.finished)) do
    cycle ()
  done;

  let p1 = Bits.to_unsigned_int !(outputs.p1) in
  let p2 = Bits.to_unsigned_int !(outputs.p2) in
  print_s [%message (p1 : int) (p2 : int)]
;;

let%expect_test "AoC 2025 Day 1 example" =
  Harness.run_advanced ~waves_config:Waves_config.no_waves ~create:Day1.hierarchical simple_testbench;
  [%expect {| ((p1 3) (p2 6)) |}]
;;
