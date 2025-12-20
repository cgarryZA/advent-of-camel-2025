open! Core
open! Hardcaml
open! Hardcaml_test_harness

let ( <--. ) = Bits.( <--. )

let resolve_from_repo_root (rel : string) : string =
  match Sys.getenv "DUNE_SOURCEROOT" with
  | Some root -> Filename.concat root rel
  | None -> rel
;;

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

let load_words ~(lanes : int) ~(rows : int) ~(cols : int) ~(filename : string) : Bits.t array =
  let lines = read_grid filename in
  let ext = build_extended_rows ~rows ~cols lines in
  let ext_rows = rows + 2 in
  let ext_cols = cols + 2 in
  let words_per_row = (ext_cols + lanes - 1) / lanes in
  let words_total = ext_rows * words_per_row in
  Array.init words_total ~f:(fun addr ->
    let r = addr / words_per_row in
    let w = addr mod words_per_row in
    pack_word ~lanes ~line:ext.(r) ~word_idx:w)
;;

let lcg_next (s : int ref) : int =
  s := (!s * 1103515245 + 12345) land 0x7fffffff;
  !s
;;

let rand_pct ~seed ~(pct_true : int) : bool =
  (lcg_next seed mod 100) < pct_true
;;

let%expect_test "Day4 Engine (135x135 synthetic) matches known output" =
  let lanes = 64 in
  let rows = 135 in
  let cols = 135 in
  let module E =
    Advent_of_caml.Day4.Engine.Make (struct
      let lanes = lanes
      let rows = rows
      let cols = cols
    end)
  in
  let module H = Cyclesim_harness.Make (E.I) (E.O) in
  let words =
    load_words
      ~lanes
      ~rows
      ~cols
      ~filename:(resolve_from_repo_root "inputs/day4/135x135.txt")
  in
  let p1 = ref 0 in
  let p2 = ref 0 in
  H.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:E.hierarchical
    (fun sim ->
      let inputs = Cyclesim.inputs sim in
      let outputs = Cyclesim.outputs sim in
      let cycle () = Cyclesim.cycle sim in

      inputs.clear := Bits.vdd; cycle ();
      inputs.clear := Bits.gnd; cycle ();

      inputs.start := Bits.gnd;
      inputs.finish := Bits.gnd;
      inputs.load_we := Bits.vdd;

      Array.iteri words ~f:(fun addr word ->
        inputs.load_addr <--. addr;
        inputs.load_word := word;
        cycle ());

      inputs.load_we := Bits.gnd;
      cycle ();

      inputs.start := Bits.vdd; cycle ();
      inputs.start := Bits.gnd;

      let guard = ref 0 in
      while (not (Bits.to_bool !(outputs.finished))) && !guard < 2_000_000 do
        incr guard;
        cycle ()
      done;

      if not (Bits.to_bool !(outputs.finished)) then failwith "Engine: timeout";

      p1 := Bits.to_unsigned_int !(outputs.p1);
      p2 := Bits.to_unsigned_int !(outputs.p2));

  let p1 = !p1 in
  let p2 = !p2 in
  print_s [%message (p1 : int) (p2 : int)];
  [%expect {| ((p1 4150) (p2 4621)) |}]
;;

let%expect_test "Day4 Engine (512x512 synthetic) matches known output" =
  let lanes = 64 in
  let rows = 512 in
  let cols = 512 in
  let module E =
    Advent_of_caml.Day4.Engine.Make (struct
      let lanes = lanes
      let rows = rows
      let cols = cols
    end)
  in
  let module H = Cyclesim_harness.Make (E.I) (E.O) in
  let words =
    load_words
      ~lanes
      ~rows
      ~cols
      ~filename:(resolve_from_repo_root "inputs/day4/512x512.txt")
  in
  let p1 = ref 0 in
  let p2 = ref 0 in
  H.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:E.hierarchical
    (fun sim ->
      let inputs = Cyclesim.inputs sim in
      let outputs = Cyclesim.outputs sim in
      let cycle () = Cyclesim.cycle sim in

      inputs.clear := Bits.vdd; cycle ();
      inputs.clear := Bits.gnd; cycle ();

      inputs.start := Bits.gnd;
      inputs.finish := Bits.gnd;
      inputs.load_we := Bits.vdd;

      Array.iteri words ~f:(fun addr word ->
        inputs.load_addr <--. addr;
        inputs.load_word := word;
        cycle ());

      inputs.load_we := Bits.gnd;
      cycle ();

      inputs.start := Bits.vdd; cycle ();
      inputs.start := Bits.gnd;

      let guard = ref 0 in
      while (not (Bits.to_bool !(outputs.finished))) && !guard < 10_000_000 do
        incr guard;
        cycle ()
      done;

      if not (Bits.to_bool !(outputs.finished)) then failwith "Engine: timeout";

      p1 := Bits.to_unsigned_int !(outputs.p1);
      p2 := Bits.to_unsigned_int !(outputs.p2));

  let p1 = !p1 in
  let p2 = !p2 in
  print_s [%message (p1 : int) (p2 : int)];
  [%expect {| ((p1 58050) (p2 65520)) |}]
;;

let run_stream_once
  ~(lanes : int)
  ~(rows : int)
  ~(cols : int)
  ~(filename : string)
  ~(inject_missing_tlast_on_last : bool)
  : int * int * bool
  =
  let module S =
    Advent_of_caml.Day4.Stream.Make (struct
      let lanes = lanes
      let rows = rows
      let cols = cols
    end)
  in
  let module H = Cyclesim_harness.Make (S.I) (S.O) in
  let words = load_words ~lanes ~rows ~cols ~filename in

  let seed = ref 1 in
  let p1 = ref 0 in
  let p2 = ref 0 in
  let frame_error_seen = ref false in

  H.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:S.hierarchical
    (fun sim ->
      let inputs = Cyclesim.inputs sim in
      let outputs = Cyclesim.outputs sim in

      let cycle () =
        Cyclesim.cycle sim;
        if Bits.to_bool !(outputs.frame_error) then frame_error_seen := true
      in

      inputs.clear := Bits.vdd; cycle ();
      inputs.clear := Bits.gnd; cycle ();

      inputs.s_axis_tvalid := Bits.gnd;
      inputs.s_axis_tdata := Bits.zero lanes;
      inputs.s_axis_tlast := Bits.gnd;
      inputs.s_axis_tuser := Bits.gnd;
      inputs.m_axis_tready := Bits.gnd;
      cycle ();

      let idx = ref 0 in
      let guard = ref 0 in
      while (!idx < Array.length words) && !guard < 10_000_000 do
        incr guard;

        let stall_sender = rand_pct ~seed ~pct_true:35 in
        if stall_sender then begin
          inputs.s_axis_tvalid := Bits.gnd;
          inputs.s_axis_tlast := Bits.gnd;
          inputs.s_axis_tuser := Bits.gnd;
          cycle ()
        end else begin
          while (not (Bits.to_bool !(outputs.s_axis_tready))) && !guard < 10_000_000 do
            incr guard;
            inputs.s_axis_tvalid := Bits.gnd;
            inputs.s_axis_tlast := Bits.gnd;
            inputs.s_axis_tuser := Bits.gnd;
            cycle ()
          done;

          let is_first = (!idx = 0) in
          let is_last  = (!idx = Array.length words - 1) in
          let tlast =
            if is_last && inject_missing_tlast_on_last then false else is_last
          in

          inputs.s_axis_tvalid := Bits.vdd;
          inputs.s_axis_tdata := words.(!idx);
          inputs.s_axis_tuser := Bits.of_bool is_first;
          inputs.s_axis_tlast := Bits.of_bool tlast;

          cycle ();

          inputs.s_axis_tvalid := Bits.gnd;
          inputs.s_axis_tlast := Bits.gnd;
          inputs.s_axis_tuser := Bits.gnd;

          incr idx
        end
      done;

      if !idx <> Array.length words then failwith "Stream: input send timeout";

      let got = ref false in
      let guard2 = ref 0 in
      while (not !got) && !guard2 < 10_000_000 do
        incr guard2;

        let backpressure = rand_pct ~seed ~pct_true:50 in
        inputs.m_axis_tready := Bits.of_bool (not backpressure);

        if Bits.to_bool !(outputs.m_axis_tvalid) && Bits.to_bool !(inputs.m_axis_tready) then begin
          let d = !(outputs.m_axis_tdata) in
          let p1_bits = Bits.select d ~high:(S.count_bits - 1) ~low:0 in
          let p2_bits = Bits.select d ~high:(2 * S.count_bits - 1) ~low:S.count_bits in
          p1 := Bits.to_unsigned_int p1_bits;
          p2 := Bits.to_unsigned_int p2_bits;
          got := true;
          cycle ();
          inputs.m_axis_tready := Bits.gnd
        end else begin
          cycle ()
        end
      done;

      if not !got then failwith "Stream: result timeout");

  (!p1, !p2, !frame_error_seen)
;;

let%expect_test "Day4 Stream (135x135 synthetic) stalls + backpressure, no protocol error" =
  let p1, p2, frame_error =
    run_stream_once
      ~lanes:64 ~rows:135 ~cols:135
      ~filename:(resolve_from_repo_root "inputs/day4/135x135.txt")
      ~inject_missing_tlast_on_last:false
  in
  print_s [%message (p1 : int) (p2 : int) (frame_error : bool)];
  [%expect {| ((p1 4150) (p2 4621) (frame_error false)) |}]
;;

let%expect_test "Day4 Stream flags protocol error when TLAST missing" =
  let p1, p2, frame_error =
    run_stream_once
      ~lanes:64 ~rows:135 ~cols:135
      ~filename:(resolve_from_repo_root "inputs/day4/135x135.txt")
      ~inject_missing_tlast_on_last:true
  in
  print_s [%message (p1 : int) (p2 : int) (frame_error : bool)];
  [%expect {| ((p1 4150) (p2 4621) (frame_error true)) |}]
;;
