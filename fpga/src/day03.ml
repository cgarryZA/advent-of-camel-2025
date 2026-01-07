(* src/day03.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth  = 32
let extra_synth_args = []

(* ====================== STREAMING CORE ====================== *)

let core (_scope : Scope.t) ~(clock : Signal.t) ~(clear : Signal.t)
    ~(uart_rx : Signal.t Uart.Byte_with_valid.t) ~(uart_rts : Signal.t) :
    Signal.t * Signal.t * Signal.t =
  let spec = Reg_spec.create ~clock ~clear () in

  (* DP registers: best[n] is best value selecting exactly n digits so far, n=0..12.
     12 digits max => < 1e12 fits in 40 bits. *)
  let best : Variable.t array = Array.init 13 ~f:(fun _ -> Variable.reg ~width:40 spec) in
  let have_digit = Variable.reg ~width:1 spec in

  let sum1 = Variable.reg ~width:64 spec in
  let sum2 = Variable.reg ~width:64 spec in

  (* We will assert done one cycle AFTER we apply the final commit, so the printer
     sees the updated sums. *)
  let done_out     = reg spec done_pending.value in
  let done_pending = Variable.reg ~width:1 spec in

  let in_ready = vdd in
  let fire     = uart_rx.valid &: in_ready in

  let byte  = uart_rx.value in
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

  (* Combinational "after processing this digit" values, derived from *current* regs. *)
  let best_after_digit : Signal.t array =
    Array.init 13 ~f:(fun n ->
      if n = 0 then best.(0).value
      else
        let cand =
          uresize ~width:40 (mul10_40 best.(n - 1).value +: digit40)
        in
        mux2 (cand >: best.(n).value) cand best.(n).value)
  in

  (* Boundary is newline OR RTS (end-of-stream). In your harness RTS is a separate
     symbol/cycle, so don't AND it with fire. *)
  let boundary        = is_lf |: uart_rts in
  let will_have_digit = have_digit.value |: is_digit in

  (* Values to commit on a boundary: if this cycle is a digit too, use best_after_digit;
     otherwise use the stored best. *)
  let b2_commit  = mux2 is_digit best_after_digit.(2) best.(2).value in
  let b12_commit = mux2 is_digit best_after_digit.(12) best.(12).value in

  let clear_best_stmts =
    List.init 13 ~f:(fun idx -> best.(idx) <-- zero 40)
  in

  (* 64-bit add with truncation (wrap) to avoid width growth warnings/errors *)
  let add64_trunc (a : Signal.t) (b : Signal.t) : Signal.t =
    uresize ~width:64 (a +: b)
  in

  compile
    [
      (* defaults each cycle *)
      done_pending <-- gnd;

      (* ignore CR (windows newlines) *)
      when_ is_cr [];

      (* digit step *)
      when_ is_digit
        ([
           have_digit <-- vdd;
         ]
         @ List.init 12 ~f:(fun k ->
             let n = 12 - k in
             best.(n) <-- best_after_digit.(n)));

      (* end-of-line or end-of-stream commit *)
      when_ (boundary &: will_have_digit)
        ([
           sum1 <-- add64_trunc sum1.value (uresize ~width:64 b2_commit);
           sum2 <-- add64_trunc sum2.value (uresize ~width:64 b12_commit);
           have_digit <-- gnd;
         ]
         @ clear_best_stmts);

      (* schedule done pulse after the final commit *)
      when_ uart_rts [ done_pending <-- vdd ];
    ];

  sum1.value, sum2.value, done_out
;;

(* ====================== TOP (ULX3S WRAPPER) ====================== *)

let create
    scope
    ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t)
  =
  let p1, p2, done_ = core scope ~clock ~clear ~uart_rx ~uart_rts in

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 p1; valid = done_ }
      ; part2 = { value = uresize ~width:60 p2; valid = done_ }
      }
  in

  { Ulx3s.O.
    leds          = concat_lsb [ ~:clear; uart_rx_overflow; done_; zero 5 ]
  ; uart_tx       = byte_out
  ; uart_rx_ready = vdd
  }
;;

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day03" ~scope create
;;
