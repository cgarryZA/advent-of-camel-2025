open! Core
open! Hardcaml
open! Hardcaml_test_harness

module Day1 = Advent_of_caml.Day1.Engine

let ( <--. ) = Bits.( <--. )

let example =
  [ "L68"
  ; "L30"
  ; "R48"
  ; "L5"
  ; "R60"
  ; "L55"
  ; "L1"
  ; "L99"
  ; "R14"
  ; "L82"
  ]
;;

let parse (s : string) =
  let s = String.strip s in
  if String.length s < 2 then failwith "Bad instruction";
  let dir = if Char.(s.[0] = 'L') then 0 else 1 in
  let steps = Int.of_string (String.sub s ~pos:1 ~len:(String.length s - 1)) in
  dir, steps
;;

let lcg_next (s : int ref) : int =
  s := (!s * 1103515245 + 12345) land 0x7fffffff;
  !s
;;

let rand_int (s : int ref) ~bound =
  if bound <= 0 then 0 else lcg_next s mod bound
;;

let gen_instrs ~seed ~n ~max_steps : (int * int) list =
  let s = ref seed in
  List.init n ~f:(fun _ ->
    let dir = rand_int s ~bound:2 in
    let steps = rand_int s ~bound:(max_steps + 1) in
    dir, steps)
;;

let mod100 x =
  let r = x mod 100 in
  if r < 0 then r + 100 else r
;;

let count_zero_hits ~value ~dir ~steps =
  if steps <= 0 then 0
  else
    let first_hit =
      if value = 0 then 100
      else if dir = 1 then (100 - value) else value
    in
    if steps < first_hit then 0 else 1 + ((steps - first_hit) / 100)
;;

let model (instrs : (int * int) list) : int * int =
  let value = ref 50 in
  let p1 = ref 0 in
  let p2 = ref 0 in
  List.iter instrs ~f:(fun (dir, steps) ->
    if steps = 0 then begin
      if !value = 0 then incr p1
    end else begin
      p2 := !p2 + count_zero_hits ~value:!value ~dir ~steps;
      if dir = 1 then value := mod100 (!value + steps) else value := mod100 (!value - steps);
      if !value = 0 then incr p1
    end);
  !p1, !p2
;;

let run_hw ~(instrs : (int * int) list) ~(idle_bubbles_pct : int) : int * int * int =
  let cycles = ref 0 in
  let p1 = ref 0 in
  let p2 = ref 0 in
  let module H = Cyclesim_harness.Make (Day1.I) (Day1.O) in
  let seed = ref 1 in

  H.run_advanced
    ~waves_config:Waves_config.no_waves
    ~create:Day1.hierarchical
    (fun sim ->
      let inputs = Cyclesim.inputs sim in
      let outputs = Cyclesim.outputs sim in

      let cycle () =
        incr cycles;
        Cyclesim.cycle sim
      in

      let wait_ready ~guard_limit =
        let guard = ref 0 in
        while (not (Bits.to_bool !(outputs.ready))) && !guard < guard_limit do
          incr guard;
          cycle ()
        done;
        if not (Bits.to_bool !(outputs.ready)) then failwith "Timeout waiting for ready"
      in

      let maybe_idle () =
        let r = lcg_next seed mod 100 in
        if r < idle_bubbles_pct then cycle ()
      in

      let send_instruction (dir, steps) =
        wait_ready ~guard_limit:1_000_000;
        maybe_idle ();
        inputs.direction <--. dir;
        inputs.steps <--. steps;
        inputs.valid := Bits.vdd;
        cycle ();
        inputs.valid := Bits.gnd
      in

      inputs.clear := Bits.vdd; cycle ();
      inputs.clear := Bits.gnd; cycle ();

      inputs.start := Bits.vdd; cycle ();
      inputs.start := Bits.gnd; cycle ();

      List.iter instrs ~f:send_instruction;

      wait_ready ~guard_limit:1_000_000;
      inputs.finish := Bits.vdd; cycle ();
      inputs.finish := Bits.gnd;

      let guard = ref 0 in
      while (not (Bits.to_bool !(outputs.finished))) && !guard < 2_000_000 do
        incr guard;
        cycle ()
      done;
      if not (Bits.to_bool !(outputs.finished)) then failwith "Timeout waiting for finished";

      p1 := Bits.to_unsigned_int !(outputs.p1);
      p2 := Bits.to_unsigned_int !(outputs.p2)
    );

  !p1, !p2, !cycles
;;

let%expect_test "Day1: example matches (and reports cycles)" =
  let instrs = List.map example ~f:parse in
  let exp_p1, exp_p2 = model instrs in
  let got_p1, got_p2, cycles = run_hw ~instrs ~idle_bubbles_pct:0 in
  ignore cycles;
  print_s [%message (exp_p1 : int) (exp_p2 : int) (got_p1 : int) (got_p2 : int)];
  [%expect {| ((exp_p1 3) (exp_p2 6) (got_p1 3) (got_p2 6)) |}]
;;

let%expect_test "Day1: deterministic random smoke (1k instrs, bubbles)" =
  let instrs = gen_instrs ~seed:123 ~n:1_000 ~max_steps:9999 in
  let exp_p1, exp_p2 = model instrs in
  let got_p1, got_p2, cycles = run_hw ~instrs ~idle_bubbles_pct:25 in
  ignore cycles;
  print_s [%message (exp_p1 : int) (exp_p2 : int) (got_p1 : int) (got_p2 : int)];
  [%expect {| ((exp_p1 0) (exp_p2 50384) (got_p1 0) (got_p2 50384)) |}]
;;

let%expect_test "Day1: deterministic random large (20k instrs, bubbles)" =
  let instrs = gen_instrs ~seed:999 ~n:20_000 ~max_steps:9999 in
  let exp_p1, exp_p2 = model instrs in
  let got_p1, got_p2, cycles = run_hw ~instrs ~idle_bubbles_pct:10 in
  ignore cycles;
  print_s [%message (exp_p1 : int) (exp_p2 : int) (got_p1 : int) (got_p2 : int)];
  [%expect {| ((exp_p1 0) (exp_p2 1006012) (got_p1 0) (got_p2 1006012)) |}]
;;

