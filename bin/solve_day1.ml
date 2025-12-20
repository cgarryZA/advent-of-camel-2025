open! Core
open! Hardcaml
open! Hardcaml_test_harness

module Day1 = Advent_of_caml.Day1.Engine
module Harness = Cyclesim_harness.Make (Day1.I) (Day1.O)

let ( <--. ) = Bits.( <--. )

let parse (s : string) =
  let s = String.strip s in
  let dir = if Char.(s.[0] = 'L') then 0 else 1 in
  let steps = Int.of_string (String.sub s ~pos:1 ~len:(String.length s - 1)) in
  dir, steps
;;

let run_file ~(vcd : string option) filename =
  let t0 = Time_ns.now () in
  let cycles = ref 0 in

  let instrs =
    In_channel.read_lines filename
    |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
    |> List.map ~f:parse
  in

  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day1.hierarchical
    (fun sim ->
      let vcd_oc = Option.map vcd ~f:Out_channel.create in
      let sim =
        match vcd_oc with
        | None -> sim
        | Some oc -> Hardcaml.Vcd.wrap oc sim
      in

      let inputs = Cyclesim.inputs sim in
      let outputs = Cyclesim.outputs sim in

      let cycle () =
        incr cycles;
        Cyclesim.cycle sim
      in

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

      inputs.clear := Bits.vdd; cycle ();
      inputs.clear := Bits.gnd; cycle ();

      inputs.start := Bits.vdd; cycle ();
      inputs.start := Bits.gnd; cycle ();

      List.iter instrs ~f:send_instruction;

      wait_ready ();
      inputs.finish := Bits.vdd; cycle ();
      inputs.finish := Bits.gnd; cycle ();

      while not (Bits.to_bool !(outputs.finished)) do
        cycle ()
      done;

      let p1 = Bits.to_unsigned_int !(outputs.p1) in
      let p2 = Bits.to_unsigned_int !(outputs.p2) in

      let t1 = Time_ns.now () in
      let dt = Time_ns.diff t1 t0 |> Time_ns.Span.to_sec in

      printf "Part 1: %d\nPart 2: %d\n" p1 p2;
      printf "Cycles: %d\n" !cycles;
      printf "Time  : %.3fs\n" dt;

      Option.iter vcd_oc ~f:Out_channel.close)
;;

let command =
  Command.basic
    ~summary:"Run Day1 cyclesim on an input file and print p1/p2"
    [%map_open.Command
      let file = anon ("file" %: string)
      and vcd =
        flag "vcd" (optional string)
          ~doc:"FILE Write VCD waveform to FILE"
      in
      fun () -> run_file ~vcd file]
;;

let () = Command_unix.run command
