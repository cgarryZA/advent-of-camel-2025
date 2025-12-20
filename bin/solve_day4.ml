open! Core
open! Hardcaml
open! Hardcaml_test_harness

module Day4_gen = Advent_of_caml.Day4_toiletpaper

let ( <--. ) = Bits.( <--. )

let read_grid filename : string list =
  In_channel.read_lines filename
  |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
;;

let infer_dims (lines : string list) : int * int =
  match lines with
  | [] -> failwith "Empty grid (after removing blank lines)."
  | first :: _ ->
    let cols = String.length first in
    if cols = 0 then failwith "First line has 0 columns.";
    List.iteri lines ~f:(fun r line ->
      if String.length line <> cols then
        failwithf "Non-rectangular grid: line %d has %d cols, expected %d"
          r (String.length line) cols ());
    (List.length lines, cols)
;;

let build_extended_rows ~rows ~cols (grid_lines : string list) : string array =
  (* ext has '.' border: (rows+2) x (cols+2) *)
  let ext_rows = rows + 2 in
  let ext_cols = cols + 2 in
  let ext = Array.create ~len:ext_rows (String.make ext_cols '.') in
  for r = 0 to rows - 1 do
    let line = List.nth_exn grid_lines r in
    ext.(r + 1) <- "." ^ line ^ "."
  done;
  ext
;;

let pack_word ~lanes ~(line : string) ~(word_idx : int) : Bits.t =
  (* lane0 is LSB, lane(lanes-1) is MSB *)
  let bits =
    List.init lanes ~f:(fun lane ->
      let col = (word_idx * lanes) + lane in
      let bit =
        if col < String.length line && Char.(line.[col] = '@') then 1 else 0
      in
      Bits.of_int_trunc ~width:1 bit)
  in
  Bits.concat_msb (List.rev bits)
;;

let load_words
  ~(lanes : int)
  ~(rows : int)
  ~(cols : int)
  ~(words_total : int)
  ~(words_per_row : int)
  ~(filename : string)
  : Bits.t array
  =
  let lines = read_grid filename in
  let ext_rows = build_extended_rows ~rows ~cols lines in
  Array.init words_total ~f:(fun addr ->
    let r = addr / words_per_row in
    let w = addr mod words_per_row in
    pack_word ~lanes ~line:ext_rows.(r) ~word_idx:w)
;;

let run_file ~(lanes : int) (filename : string) =
  let t0 = Time_ns.now () in
  let cycles = ref 0 in

  let lines = read_grid filename in
  let (rows, cols) = infer_dims lines in

  (* Compute the derived packing params (must match the RTL generator). *)
  let ext_rows = rows + 2 in
  let ext_cols = cols + 2 in
  let words_per_row = (ext_cols + lanes - 1) / lanes in
  let words_total = ext_rows * words_per_row in

  (* Instantiate a specialised circuit for this input size. *)
  let module D =
    Day4_gen.Make (struct
      let lanes = lanes
      let rows = rows
      let cols = cols
    end)
  in

  (* Optional sanity checks (nice to catch accidental divergence). *)
  if D.words_per_row <> words_per_row then
    failwithf "Internal error: words_per_row mismatch (solver=%d rtl=%d)"
      words_per_row D.words_per_row ();
  if D.words_total <> words_total then
    failwithf "Internal error: words_total mismatch (solver=%d rtl=%d)"
      words_total D.words_total ();

  let module Harness = Cyclesim_harness.Make (D.I) (D.O) in

  let words =
    load_words
      ~lanes
      ~rows
      ~cols
      ~words_total
      ~words_per_row
      ~filename
  in

  printf "Grid: rows=%d cols=%d lanes=%d\n%!" rows cols lanes;

  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:D.hierarchical
    (fun sim ->
      let inputs = Cyclesim.inputs sim in
      let outputs = Cyclesim.outputs sim in

      let cycle () =
        incr cycles;
        Cyclesim.cycle sim
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

      (* preload packed words while in Idle *)
      inputs.load_we := Bits.vdd;
      Array.iteri words ~f:(fun addr word ->
        inputs.load_addr <--. addr;
        inputs.load_word := word;
        cycle ());
      inputs.load_we := Bits.gnd;
      cycle ();

      (* start *)
      inputs.start := Bits.vdd;
      cycle ();
      inputs.start := Bits.gnd;

      (* run *)
      while not (Bits.to_bool !(outputs.finished)) do
        cycle ()
      done;

      let p1 = Bits.to_unsigned_int !(outputs.p1) in
      let p2 = Bits.to_unsigned_int !(outputs.p2) in

      let t1 = Time_ns.now () in
      let dt = Time_ns.diff t1 t0 |> Time_ns.Span.to_sec in

      printf "Part 1: %d\nPart 2: %d\n" p1 p2;
      printf "Cycles: %d\n" !cycles;
      printf "Time  : %.3fs\n" dt
    )
;;

let command =
  Command.basic
    ~summary:"Run Day4_toiletpaper packed-word cyclesim (auto grid size)"
    [%map_open.Command
      let file = anon ("file" %: string)
      and lanes =
        flag "lanes" (optional_with_default 64 int)
          ~doc:"INT Packed bits per word (default 64)"
      in
      fun () -> run_file ~lanes file]
;;

let () = Command_unix.run command
