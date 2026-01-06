open Core
open Hardcaml

module E = Advent_of_caml.Day3.Engine.Make

type result =
  { part1 : int64
  ; part2 : int64
  ; cycles : int
  ; seconds : float
  }

let run (input : string) : result =
  let module Sim = Cyclesim.With_interface (E.I) (E.O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (E.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let cycles = ref 0 in
  let cycle () =
    Cyclesim.cycle sim;
    Int.incr cycles
  in

  inputs.in_valid := Bits.gnd;
  inputs.in_byte := Bits.zero 8;
  inputs.in_last := Bits.gnd;

  let t0 = Time_ns.now () in

  (* synchronous clear *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;

  let bytes = Bytes.of_string input in
  let len = Bytes.length bytes in
  for idx = 0 to len - 1 do
    inputs.in_valid := Bits.vdd;
    inputs.in_byte :=
      Bits.of_int_trunc ~width:8 (Char.to_int (Bytes.get bytes idx));
    inputs.in_last := if idx = len - 1 then Bits.vdd else Bits.gnd;
    cycle ()
  done;

  let t1 = Time_ns.now () in
  let seconds = Time_ns.Span.to_sec (Time_ns.diff t1 t0) in

  { part1 = Bits.to_int64_trunc !(outputs.part1)
  ; part2 = Bits.to_int64_trunc !(outputs.part2)
  ; cycles = !cycles
  ; seconds
  }

let read_input () : string =
  match Sys.get_argv () with
  | [| _ |] ->
      (* no args: read stdin *)
      In_channel.input_all In_channel.stdin
  | [| _; filename |] ->
      In_channel.read_all filename
  | _ ->
      eprintf "Usage: solve_day3 [input_file]\n";
      exit 2

let () =
  let input = read_input () in
  let r = run input in
  printf "Part 1: %Ld\n" r.part1;
  printf "Part 2: %Ld\n" r.part2;
  Advent_of_caml.Metrics.print ~cycles:r.cycles ~dt_s:r.seconds
