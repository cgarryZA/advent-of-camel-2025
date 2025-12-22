open Core
open Hardcaml

module E = Advent_of_caml.Day3.Engine.Make

type sim_result =
  { part1 : int64
  ; part2 : int64
  ; cycles : int
  ; seconds : float
  }

let run_string (s : string) : sim_result =
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

  (* sync clear *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;

  let bytes = Bytes.of_string s in
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

(* ----------------------------
   Reference (software) solution
   ---------------------------- *)

let max_k (line : string) (k : int) : int64 =
  (* DP: best[n] = best number formed using exactly n digits *)
  let neg1 = Int64.minus_one in
  let best = Array.create ~len:(k + 1) neg1 in
  best.(0) <- 0L;

  String.iter line ~f:(fun ch ->
    if Char.(ch >= '0' && ch <= '9') then (
      let d = Int64.of_int (Char.to_int ch - Char.to_int '0') in
      for n = k downto 1 do
        let prev = best.(n - 1) in
        if Int64.(prev >= 0L) then (
          let cand = Int64.((prev * 10L) + d) in
          if Int64.(cand > best.(n)) then
            best.(n) <- cand
        )
      done
    )
  );

  if Int64.(best.(k) < 0L) then 0L else best.(k)

let ref_solve (input : string) : int64 * int64 =
  let lines =
    input
    |> String.split_lines
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun l -> not (String.is_empty l))
  in
  let p1 = ref 0L in
  let p2 = ref 0L in
  List.iter lines ~f:(fun line ->
    p1 := Int64.(!p1 + max_k line 2);
    p2 := Int64.(!p2 + max_k line 12)
  );
  (!p1, !p2)

let assert_eq ~ctx (got1, got2) (exp1, exp2) =
  if not Int64.(got1 = exp1) then
    failwithf "%s Part 1 mismatch: got %Ld expected %Ld" ctx got1 exp1 ();
  if not Int64.(got2 = exp2) then
    failwithf "%s Part 2 mismatch: got %Ld expected %Ld" ctx got2 exp2 ()

let () =
  (* Official sample *)
  let sample =
    String.concat_lines
      [ "987654321111111"
      ; "811111111111119"
      ; "234234234234278"
      ; "818181911112111"
      ]
    ^ "\n"
  in
  let sim = run_string sample in
  let exp = (357L, 3121910778619L) in

  printf "sample cycles=%s time=%.3fms\n"
    (Int.to_string_hum sim.cycles)
    (sim.seconds *. 1000.0);

  assert_eq ~ctx:"sample(sim)" (sim.part1, sim.part2) exp;

  (* Sample with NO trailing newline *)
  let sample_no_nl =
    String.concat_lines
      [ "987654321111111"
      ; "811111111111119"
      ; "234234234234278"
      ; "818181911112111"
      ]
  in
  let sim2 = run_string sample_no_nl in
  assert_eq ~ctx:"sample(no_nl)" (sim2.part1, sim2.part2) exp;

  (* Windows newlines *)
  let sample_crlf =
    "987654321111111\r\n\
     811111111111119\r\n\
     234234234234278\r\n\
     818181911112111\r\n"
  in
  let sim3 = run_string sample_crlf in
  assert_eq ~ctx:"sample(crlf)" (sim3.part1, sim3.part2) exp;

  (* Property tests *)
  let rng = Random.State.make [| 0xC0FFEE |] in

  let gen_line () =
    let len = 1 + Random.State.int rng 80 in
    String.init len ~f:(fun _ ->
      Char.of_int_exn (Char.to_int '0' + (1 + Random.State.int rng 9)))
  in

  let gen_input ~lines ~trailing_nl ~crlf =
    let sep = if crlf then "\r\n" else "\n" in
    let body =
      List.init lines ~f:(fun _ -> gen_line ())
      |> String.concat ~sep
    in
    if trailing_nl then body ^ sep else body
  in

  for t = 1 to 50 do
    let lines = 1 + Random.State.int rng 200 in
    let trailing_nl = Random.State.bool rng in
    let crlf = Random.State.bool rng in
    let input = gen_input ~lines ~trailing_nl ~crlf in

    let simr = run_string input in
    let r1, r2 = ref_solve input in

    assert_eq
      ~ctx:(sprintf "random[%d] lines=%d nl=%b crlf=%b"
               t lines trailing_nl crlf)
      (simr.part1, simr.part2)
      (r1, r2)
  done;

  printf "ok\n"
