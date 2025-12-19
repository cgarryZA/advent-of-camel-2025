open! Core
open! Hardcaml
open! Hardcaml_test_harness

module Day4 = Advent_of_caml.Day4_toiletpaper
module Harness = Cyclesim_harness.Make (Day4.I) (Day4.O)

let ( <--. ) = Bits.( <--. )

let load_grid filename =
  let lines =
    In_channel.read_lines filename
    |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
  in
  let r = List.length lines in
  let c = String.length (List.hd_exn lines) in
  if r <> 135 || c <> 135 then
    failwithf "Expected 135x135 grid, got %dx%d" r c ();

  let bits =
    lines
    |> List.concat_map ~f:(fun line ->
         String.to_list line
         |> List.map ~f:(function
              | '@' -> 1
              | '.' -> 0
              | ch -> failwithf "Unexpected char '%c' in input" ch ()))
  in
  if List.length bits <> (135 * 135) then failwith "Bad input length after flattening";
  bits
;;

let run_file filename =
  let grid_bits = load_grid filename in
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day4.hierarchical
    (fun sim ->
      let inputs = Cyclesim.inputs sim in
      let outputs = Cyclesim.outputs sim in

      (* timing + cycle counter (count only cycles after start pulse) *)
      let t0 = Time_ns.now () in
      let cycles = ref 0 in
      let counting = ref false in

      let cycle ?n () =
        match n with
        | None ->
            Cyclesim.cycle sim;
            if !counting then Int.incr cycles
        | Some k ->
            for _ = 1 to k do
              Cyclesim.cycle sim;
              if !counting then Int.incr cycles
            done
      in

      (* reset *)
      inputs.clear := Bits.vdd;
      cycle ();
      inputs.clear := Bits.gnd;
      cycle ();

      (* keep control lines low *)
      inputs.start := Bits.gnd;
      inputs.finish := Bits.gnd;
      inputs.load_we := Bits.gnd;
      cycle ();

      (* preload grid while in Idle *)
      inputs.load_we := Bits.vdd;
      List.iteri grid_bits ~f:(fun addr bit ->
        inputs.load_addr <--. addr;
        inputs.load_data <--. bit;
        cycle ());
      inputs.load_we := Bits.gnd;
      cycle ();

      (* start *)
      inputs.start := Bits.vdd;
      cycle ();
      inputs.start := Bits.gnd;

      (* start counting cycles after the start pulse *)
      counting := true;

      (* run *)
      while not (Bits.to_bool !(outputs.finished)) do
        cycle ()
      done;

      let p1 = Bits.to_unsigned_int !(outputs.p1) in
      let p2 = Bits.to_unsigned_int !(outputs.p2) in
      let dt = Time_ns.diff (Time_ns.now ()) t0 in

      printf "Part 1: %d\nPart 2: %d\n" p1 p2;
      printf "Cycles: %d\n" !cycles;
      printf "Time  : %s\n" (Time_ns.Span.to_string_hum dt))
;;

let command =
  Command.basic
    ~summary:"Run Day4_toiletpaper cyclesim on an input file and print p1/p2 + cycles + time"
    [%map_open.Command
      let file = anon ("file" %: string) in
      fun () -> run_file file]
;;

let () = Command_unix.run command