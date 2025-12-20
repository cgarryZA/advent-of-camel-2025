open! Core
open! Hardcaml
open! Hardcaml_test_harness

module Day1 = Advent_of_caml.Day1
module Harness = Cyclesim_harness.Make (Day1.I) (Day1.O)

let ( <--. ) = Bits.( <--. )

let parse (s : string) =
  let s = String.strip s in
  let dir = if Char.(s.[0] = 'L') then 0 else 1 in
  let steps = Int.of_string (String.sub s ~pos:1 ~len:(String.length s - 1)) in
  dir, steps
;;

let run_file filename =
  let instrs =
    In_channel.read_lines filename
    |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
    |> List.map ~f:parse
  in
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day1.hierarchical
    (fun sim ->
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

      (* reset *)
      inputs.clear := Bits.vdd;
      cycle ();
      inputs.clear := Bits.gnd;
      cycle ();

      (* start *)
      inputs.start := Bits.vdd;
      cycle ();
      inputs.start := Bits.gnd;
      cycle ();

      List.iter instrs ~f:send_instruction;

      (* finish (only when not mid-instruction) *)
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
      printf "p1=%d\np2=%d\n" p1 p2)
;;

let command =
  Command.basic
    ~summary:"Run Day1 cyclesim on an input file and print p1/p2"
    [%map_open.Command
      let file = anon ("file" %: string) in
      fun () -> run_file file]
;;

let () = Command_unix.run command
