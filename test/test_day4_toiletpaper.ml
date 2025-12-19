open! Core
open! Hardcaml
open! Hardcaml_test_harness

module Day4 = Advent_of_caml.Day4_toiletpaper
module Harness = Cyclesim_harness.Make (Day4.I) (Day4.O)

let ( <--. ) = Bits.( <--. )

(* Load the 135x135 grid from a file into a flat list of 0/1 (row-major). *)
let load_grid filename =
  let lines =
    In_channel.read_lines filename
    |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
  in
  let r = List.length lines in
  let c = String.length (List.hd_exn lines) in
  if r <> Day4.rows || c <> Day4.cols then
    failwithf "Expected %dx%d grid, got %dx%d" Day4.rows Day4.cols r c ();

  let bits =
    lines
    |> List.concat_map ~f:(fun line ->
      String.to_list line
      |> List.map ~f:(function
        | '@' -> 1
        | '.' -> 0
        | ch -> failwithf "Unexpected char '%c' in input" ch ()))
  in
  if List.length bits <> Day4.cells then
    failwithf "Bad input length: expected %d, got %d" Day4.cells (List.length bits) ();
  bits
;;

let simple_testbench ~(grid_bits:int list) (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in

  (* Reset *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();

  (* Keep controls low *)
  inputs.start := Bits.gnd;
  inputs.finish := Bits.gnd;
  inputs.load_we := Bits.gnd;
  cycle ();

  (* Preload grid while in Idle *)
  inputs.load_we := Bits.vdd;
  List.iteri grid_bits ~f:(fun addr bit ->
    inputs.load_addr <--. addr;
    inputs.load_data <--. bit;
    cycle ()
  );
  inputs.load_we := Bits.gnd;
  cycle ();

  (* Start *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;

  (* Run until finished *)
  while not (Bits.to_bool !(outputs.finished)) do
    cycle ()
  done;

  let p1 = Bits.to_unsigned_int !(outputs.p1) in
  let p2 = Bits.to_unsigned_int !(outputs.p2) in
  print_s [%message (p1 : int) (p2 : int)]
;;

(*(circuit is hardcoded to 135x135, so it needs padding). *)
let%expect_test "AoC 2025 Day 4 (day4test.txt)" =
  let grid_bits = load_grid "day4test.txt" in
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day4.hierarchical
    (simple_testbench ~grid_bits);
  [%expect {| ((p1 24) (p2 24)) |}]
;;
