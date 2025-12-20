open! Core
open! Hardcaml
open! Hardcaml_test_harness

module Day4 = Advent_of_caml.Day4
module Harness = Cyclesim_harness.Make (Day4.I) (Day4.O)

let ( <--. ) = Bits.( <--. )

let read_input_135 filename : string list =
  In_channel.read_lines filename
  |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
;;

let build_extended_rows (rows_135 : string list) : string array =
  if List.length rows_135 <> 135 then failwith "Expected 135 lines";

  List.iter rows_135 ~f:(fun line ->
    if String.length line <> 135 then failwith "Expected 135 columns per line");

  let ext = Array.create ~len:Day4.ext_rows (String.make Day4.ext_cols '.') in
  for r = 0 to 134 do
    let line = List.nth_exn rows_135 r in
    ext.(r + 1) <- "." ^ line ^ "."
  done;
  ext
;;

let pack_word ~(line:string) ~(word_idx:int) : Bits.t =
  (* lane0 is LSB, lane (lanes-1) is MSB *)
  let bits =
    List.init Day4.lanes ~f:(fun lane ->
      let col = (word_idx * Day4.lanes) + lane in
      let bit =
        if col < String.length line && Char.(line.[col] = '@') then 1 else 0
      in
      Bits.of_int_trunc ~width:1 bit)
  in
  Bits.concat_msb (List.rev bits)
;;

let load_words filename : Bits.t array =
  let rows_135 = read_input_135 filename in
  let ext_rows = build_extended_rows rows_135 in
  Array.init Day4.words_total ~f:(fun addr ->
    let r = addr / Day4.words_per_row in
    let w = addr mod Day4.words_per_row in
    pack_word ~line:ext_rows.(r) ~word_idx:w)
;;

let simple_testbench ~(words:Bits.t array) (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in

  (* reset *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();

  (* preload packed words while in Idle *)
  inputs.start := Bits.gnd;
  inputs.finish := Bits.gnd;
  inputs.load_we := Bits.vdd;

  Array.iteri words ~f:(fun addr word ->
    inputs.load_addr <--. addr;
    inputs.load_word := word;
    cycle ()
  );

  inputs.load_we := Bits.gnd;
  cycle ();

  (* start *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;

  (* run until finished *)
  while not (Bits.to_bool !(outputs.finished)) do
    cycle ()
  done;

  let p1 = Bits.to_unsigned_int !(outputs.p1) in
  let p2 = Bits.to_unsigned_int !(outputs.p2) in
  print_s [%message (p1 : int) (p2 : int)]
;;

let%expect_test "AoC 2025 Day 4 (day4input.txt) packed" =
  (* IMPORTANT:
     ppx_expect runs tests in a sandbox; the file must be in test/ and referenced relative to test/.
     You currently have test/day4input.txt, so use that. *)
  let words = load_words "day4input.txt" in
  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day4.hierarchical
    (simple_testbench ~words);
  [%expect {| ((p1 1389) (p2 9000)) |}]
;;
