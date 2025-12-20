open! Core
open! Hardcaml
open! Hardcaml_test_harness

module Axis_gen = Advent_of_caml.Day4_stream

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

let run_file ~(lanes : int) ~(vcd : string option) (filename : string) =
  let t0 = Time_ns.now () in
  let cycles = ref 0 in

  let lines = read_grid filename in
  let (rows, cols) = infer_dims lines in

  let ext_rows = rows + 2 in
  let ext_cols = cols + 2 in
  let words_per_row = (ext_cols + lanes - 1) / lanes in
  let words_total = ext_rows * words_per_row in

  let module D =
    Axis_gen.Make (struct
      let lanes = lanes
      let rows = rows
      let cols = cols
    end)
  in
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

  printf "Grid: rows=%d cols=%d lanes=%d words_total=%d\n%!"
    rows cols lanes words_total;

  Harness.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:D.hierarchical
    (fun sim ->
      (* Optional VCD output (your hardcaml uses Vcd.wrap out_channel). *)
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

      (* reset *)
      inputs.clear := Bits.vdd; cycle ();
      inputs.clear := Bits.gnd; cycle ();

      (* defaults *)
      inputs.s_axis_tvalid := Bits.gnd;
      inputs.s_axis_tdata := Bits.zero lanes;
      inputs.s_axis_tlast := Bits.gnd;
      inputs.s_axis_tuser := Bits.gnd;
      inputs.m_axis_tready := Bits.gnd;
      cycle ();

      (* Drive input AXI-stream *)
      let rec send idx =
        if idx = Array.length words then ()
        else begin
          (* wait until ready *)
          while not (Bits.to_bool !(outputs.s_axis_tready)) do
            inputs.s_axis_tvalid := Bits.gnd;
            inputs.s_axis_tlast := Bits.gnd;
            inputs.s_axis_tuser := Bits.gnd;
            cycle ()
          done;

          let is_first = (idx = 0) in
          let is_last  = (idx = Array.length words - 1) in

          inputs.s_axis_tvalid := Bits.vdd;
          inputs.s_axis_tdata := words.(idx);
          inputs.s_axis_tuser := Bits.of_bool is_first;  (* SOF *)
          inputs.s_axis_tlast := Bits.of_bool is_last;   (* TLAST on final *)

          cycle ();

          (* deassert by default next cycle *)
          inputs.s_axis_tvalid := Bits.gnd;
          inputs.s_axis_tlast := Bits.gnd;
          inputs.s_axis_tuser := Bits.gnd;

          send (idx + 1)
        end
      in
      send 0;

      (* Wait for result beat *)
      while not (Bits.to_bool !(outputs.m_axis_tvalid)) do
        cycle ()
      done;

      (* Extract {p2,p1} from m_axis_tdata *)
      let d = !(outputs.m_axis_tdata) in
      let p1_bits = Bits.select d ~high:(D.count_bits - 1) ~low:0 in
      let p2_bits = Bits.select d ~high:(2 * D.count_bits - 1) ~low:D.count_bits in
      let p1 = Bits.to_unsigned_int p1_bits in
      let p2 = Bits.to_unsigned_int p2_bits in

      (* Accept result *)
      inputs.m_axis_tready := Bits.vdd;
      cycle ();
      inputs.m_axis_tready := Bits.gnd;

      let t1 = Time_ns.now () in
      let dt = Time_ns.diff t1 t0 |> Time_ns.Span.to_sec in

      printf "Part 1: %d\nPart 2: %d\n" p1 p2;
      printf "Cycles: %d\n" !cycles;
      printf "Time  : %.3fs\n" dt;

      if Bits.to_bool !(outputs.frame_error) then
        printf "WARNING: frame_error was asserted (protocol violation)\n%!";

      Option.iter vcd_oc ~f:Out_channel.close
    )
;;

let command =
  Command.basic
    ~summary:"Run Day4 AXI-Stream wrapper (auto grid size)"
    [%map_open.Command
      let file = anon ("file" %: string)
      and lanes =
        flag "lanes" (optional_with_default 64 int)
          ~doc:"INT Packed bits per word (default 64)"
      and vcd =
        flag "vcd" (optional string)
          ~doc:"FILE Write VCD waveform to FILE"
      in
      fun () -> run_file ~lanes ~vcd file]
;;

let () = Command_unix.run command
