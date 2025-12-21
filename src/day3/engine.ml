open Core
open Hardcaml

module Make = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_valid : 'a
      ; in_byte : 'a [@bits 8]
      ; in_last : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { in_ready : 'a
      ; part1 : 'a [@bits 64]
      ; part2 : 'a [@bits 64]
      ; out_done : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create (_scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let open Signal in
    let open Hardcaml.Always in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

    (* DP registers: best[n] is best value selecting exactly n digits so far, n=0..12.
       12 digits max => < 1e12 fits in 40 bits. *)
    let best : Variable.t array = Array.init 13 ~f:(fun _ -> Variable.reg ~width:40 spec) in
    let have_digit = Variable.reg ~width:1 spec in

    let sum1 = Variable.reg ~width:64 spec in
    let sum2 = Variable.reg ~width:64 spec in

    (* 1-cycle pulse when we consume the final byte (in_last=1). *)
    let out_done = Variable.reg ~width:1 spec in

    let in_ready = vdd in
    let fire = i.in_valid &: in_ready in

    let byte = i.in_byte in
    let is_lf = fire &: (byte ==:. Char.to_int '\n') in
    let is_cr = fire &: (byte ==:. Char.to_int '\r') in

    let is_digit =
      fire
      &: (byte >=:. Char.to_int '0')
      &: (byte <=:. Char.to_int '9')
    in

    let digit40 = uresize ~width:40 (byte -:. Char.to_int '0') in

    (* x*10 = (x<<3) + (x<<1) using concat+resize (portable across hardcaml versions). *)
    let mul10_40 (x : Signal.t) : Signal.t =
      let x2 = uresize ~width:40 (concat_msb [ x; zero 1 ]) in
      let x8 = uresize ~width:40 (concat_msb [ x; zero 3 ]) in
      uresize ~width:40 (x2 +: x8)
    in

    (* Combinational "after processing this digit" values, derived from *current* regs.
       This matters for the (digit + in_last) case where we must commit including the digit. *)
    let best_after_digit : Signal.t array =
      Array.init 13 ~f:(fun n ->
        if n = 0 then best.(0).value
        else
          let cand =
            uresize ~width:40 (mul10_40 best.(n - 1).value +: digit40)
          in
          mux2 (cand >: best.(n).value) cand best.(n).value)
    in

    let boundary = is_lf |: (fire &: i.in_last) in
    let will_have_digit = have_digit.value |: is_digit in

    (* Values to commit on a boundary: if this cycle is a digit too, use best_after_digit;
       otherwise use the stored best. *)
    let b2_commit = mux2 is_digit best_after_digit.(2) best.(2).value in
    let b12_commit = mux2 is_digit best_after_digit.(12) best.(12).value in

    let clear_best_stmts =
      List.init 13 ~f:(fun idx -> best.(idx) <-- zero 40)
    in

    (* 64-bit add with truncation (wrap) to avoid width growth warnings/errors *)
    let add64_trunc (a : Signal.t) (b : Signal.t) : Signal.t =
      uresize ~width:64 (a +: b)
    in

    compile
      ([
         (* defaults each cycle *)
         out_done <-- gnd;

         (* ignore CR (windows newlines) *)
         when_ is_cr [];

         (* digit step: update descending is already baked into best_after_digit,
            so we can just assign best[n] <- best_after_digit[n]. *)
         when_ is_digit
           ([
              have_digit <-- vdd;
            ]
            @ List.init 12 ~f:(fun k ->
              let n = 12 - k in
              best.(n) <-- best_after_digit.(n)));

         (* pulse done on final byte *)
         when_ (fire &: i.in_last) [ out_done <-- vdd ];

         (* end-of-line or end-of-stream commit *)
         when_ (boundary &: will_have_digit)
           ([
              sum1 <-- add64_trunc sum1.value (uresize ~width:64 b2_commit);
              sum2 <-- add64_trunc sum2.value (uresize ~width:64 b12_commit);
              have_digit <-- gnd;
            ]
            @ clear_best_stmts);
       ]);

    { O.in_ready
    ; part1 = sum1.value
    ; part2 = sum2.value
    ; out_done = out_done.value
    }
end

include Make
