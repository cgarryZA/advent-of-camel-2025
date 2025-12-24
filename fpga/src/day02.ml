(* =========================
   day02.ml
   ========================= *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 32
let extra_synth_args = []

(* ========================================================================= *)
(* Parameters                                                                *)
(* ========================================================================= *)

let max_digits = 12
let digit_bits = 4
let bcd_width = max_digits * digit_bits
let len_bits = Int.ceil_log2 (max_digits + 1)

(* ========================================================================= *)
(* Core algorithm                                                            *)
(* ========================================================================= *)

module Core_algo = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; rx_valid : 'a
      ; rx_data : 'a [@bits 8]
      ; rts : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { ready : 'a
      ; part1 : 'a [@bits 64]
      ; part2 : 'a [@bits 64]
      ; finished : 'a
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Idle
      | ReadLow
      | ReadHigh
      | Count
      | Finalize
      | Done
    [@@deriving enumerate, sexp_of, compare ~localize]
  end

  (* ASCII helpers *)
  let c0 = of_int_trunc ~width:8 (Char.to_int '0')
  let c9 = of_int_trunc ~width:8 (Char.to_int '9')
  let c_dash = of_int_trunc ~width:8 (Char.to_int '-')
  let c_comma = of_int_trunc ~width:8 (Char.to_int ',')

  let ascii_is_digit c = (c >=: c0) &: (c <=: c9)
  let ascii_to_digit c = uresize ~width:4 (c -: c0)

  let zero_bcd = zero bcd_width

  let digit_at (bcd : Signal.t) (i : int) =
    select bcd ~high:((i + 1) * 4 - 1) ~low:(i * 4)
  ;;

  (* shift left by 1 digit (4 bits) and append new digit at LSB *)
  let bcd_shift_in bcd digit =
    concat_msb
      [ select bcd ~high:(bcd_width - digit_bits - 1) ~low:0
      ; digit
      ]
  ;;

  (* Small mod by constant k for small signals (len <= 12). *)
  let mod_small ~(x : Signal.t) ~(k : int) =
    let r = ref x in
    for _ = 1 to max_digits do
      r := mux2 (!r >=:. k) (!r -:. k) !r
    done;
    !r
  ;;

  (* BCD increment (fixed width). *)
  let bcd_increment ~enable bcd =
    let digits = Array.init max_digits ~f:(fun i -> digit_at bcd i) in
    let carry = ref enable in
    let out = Array.create ~len:max_digits (zero 4) in
    for i = 0 to max_digits - 1 do
      let sum = digits.(i) +: uresize ~width:4 !carry in
      let ov = sum ==:. 10 in
      out.(i) <- mux2 ov (zero 4) sum;
      carry := ov
    done;
    concat_msb (Array.to_list out)
  ;;

  (* BCD → u64 using len (1..max_digits).
     NOTE: *: widens (64x64 -> 128), so clamp. Also +: may widen, so clamp. *)
  let bcd_to_u64 (bcd : Signal.t) (len : Signal.t) : Signal.t =
    let acc = ref (zero 64) in
    let ten = of_int_trunc ~width:64 10 in
    for i = max_digits - 1 downto 0 do
      let i_s = of_int_trunc ~width:len_bits (i + 1) in
      let use = i_s <=: len in
      let d = uresize ~width:64 (digit_at bcd i) in
      let prod64 = uresize ~width:64 (!acc *: ten) in
      let next64 = uresize ~width:64 (prod64 +: d) in
      acc := mux2 use next64 !acc
    done;
    !acc
  ;;

  (* Part 1 condition: string is exactly two repeats (t||t). *)
  let repeated_twice (bcd : Signal.t) (len : Signal.t) : Signal.t =
    let even = ~:(lsb len) in
    let min_len = len >=:. 2 in
    let half = uresize ~width:len_bits (srl ~by:1 len) in
    let checks =
      List.init (max_digits / 2) ~f:(fun i ->
        let i_s = of_int_trunc ~width:len_bits i in
        let active = even &: min_len &: (i_s <: half) in
        let a = digit_at bcd i in
        let b_sel = ref (digit_at bcd i) in
        for k = 1 to (max_digits / 2) do
          b_sel := mux2 (half ==:. k) (digit_at bcd (i + k)) !b_sel
        done;
        mux2 active (a ==: !b_sel) vdd)
    in
    let all_ok = reduce ~f:( &: ) checks in
    mux2 (even &: min_len) all_ok gnd
  ;;

  (* Part 2 condition: periodic with some k dividing len and len/k >= 2. *)
  let repeated_any (bcd : Signal.t) (len : Signal.t) : Signal.t =
    let check_k (k : int) =
      let divides = (mod_small ~x:len ~k) ==:. 0 in
      (* len_bits=4, so clamp constant to avoid >=:. 16 explosion *)
      let two_k =
        of_int_trunc ~width:len_bits (min (2 * k) ((1 lsl len_bits) - 1))
      in
      let at_least_two = len >=: two_k in
      let active = divides &: at_least_two in
      let pos_checks =
        List.init max_digits ~f:(fun pos ->
          let pos_s = of_int_trunc ~width:len_bits pos in
          let in_len = pos_s <: len in
          let active_pos = active &: in_len in
          let base = digit_at bcd (pos % k) in
          mux2 active_pos (digit_at bcd pos ==: base) vdd)
      in
      let all_ok = reduce ~f:( &: ) pos_checks in
      mux2 active all_ok gnd
    in
    let ks = List.init (max_digits - 1) ~f:(fun i -> i + 1) in
    reduce ~f:( |: ) (List.map ks ~f:check_k)
  ;;

  let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module States) spec ~enable:vdd in

    let low = Variable.reg spec ~width:bcd_width
    and high = Variable.reg spec ~width:bcd_width
    and cur = Variable.reg spec ~width:bcd_width in

    let low_len = Variable.reg spec ~width:len_bits
    and cur_len = Variable.reg spec ~width:len_bits in

    (* Sticky RTS flag: set whenever rts is seen, regardless of state. *)
    let rts_seen = Variable.reg spec ~width:1 in

    let part1 = Variable.reg spec ~width:64
    and part2 = Variable.reg spec ~width:64 in

    let cur_u64 = bcd_to_u64 cur.value cur_len.value in
    let cond_p1 = repeated_twice cur.value cur_len.value in
    let cond_p2 = repeated_any cur.value cur_len.value in
    let at_high = cur.value ==: high.value in

    compile
      [ (* Sticky RTS (do NOT try to use it combinationally in the same cycle) *)
        when_ i.rts [ rts_seen <-- vdd ]

      ; sm.switch
          [ ( Idle
            , [ low <-- zero_bcd
              ; high <-- zero_bcd
              ; cur <-- zero_bcd
              ; low_len <-- zero len_bits
              ; cur_len <-- zero len_bits
              ; rts_seen <-- gnd
              ; part1 <-- zero 64
              ; part2 <-- zero 64
              ; when_ i.rx_valid
                  [ low <-- zero_bcd
                  ; high <-- zero_bcd
                  ; low_len <-- zero len_bits
                  ; sm.set_next ReadLow
                  ]
              ] )

          ; ( ReadLow
            , [ when_ (i.rx_valid &: ascii_is_digit i.rx_data)
                  [ low <-- bcd_shift_in low.value (ascii_to_digit i.rx_data)
                  ; low_len <-- low_len.value +:. 1
                  ]
              ; when_ (i.rx_valid &: (i.rx_data ==: c_dash))
                  [ high <-- zero_bcd
                  ; sm.set_next ReadHigh
                  ]
              ] )

          ; ( ReadHigh
            , [ when_ (i.rx_valid &: ascii_is_digit i.rx_data)
                  [ high <-- bcd_shift_in high.value (ascii_to_digit i.rx_data) ]
              ; when_ (i.rx_valid &: (i.rx_data ==: c_comma))
                  [ cur <-- low.value
                  ; cur_len <-- low_len.value
                  ; sm.set_next Count
                  ]
              ; when_ i.rts
                  [ cur <-- low.value
                  ; cur_len <-- low_len.value
                  ; sm.set_next Count
                  ]
              ] )

          ; ( Count
            , [ (* accumulate for this cur *)
                when_ cond_p1 [ part1 <-- part1.value +: cur_u64 ]
              ; when_ cond_p2 [ part2 <-- part2.value +: cur_u64 ]

              ; (* decide next step *)
                when_ at_high
                  [ if_
                      rts_seen.value
                      [ (* IMPORTANT: go through Finalize for 1-cycle settling *)
                        sm.set_next Finalize
                      ]
                      [ low <-- zero_bcd
                      ; high <-- zero_bcd
                      ; low_len <-- zero len_bits
                      ; sm.set_next ReadLow
                      ]
                  ]

              ; when_ (~:at_high)
                  [ cur <-- bcd_increment ~enable:vdd cur.value
                  ; cur_len <-- cur_len.value
                  ]
              ] )

          ; ( Finalize
            , [ (* one dead cycle so part1/part2 are guaranteed stable before Done *)
                sm.set_next Done
              ] )

          ; ( Done
            , [ (* stay done forever *) ] )
          ]
      ];

    { O.ready = vdd
    ; part1 = part1.value
    ; part2 = part2.value
    ; finished = sm.is Done
    }
  ;;

  let hierarchical = create
end

(* ========================================================================= *)
(* ULX3S wrapper                                                             *)
(* ========================================================================= *)

let create
    scope
    ({ Ulx3s.I.clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t)
  : _ Ulx3s.O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in

  let core =
    Core_algo.hierarchical scope
      { Core_algo.I.clock
      ; clear
      ; rx_valid = uart_rx.valid
      ; rx_data = uart_rx.value
      ; rts = uart_rts
      }
  in

  (* Edge-detect finished *)
  let fin_d = Variable.reg spec ~width:1 in
  let fin_pulse = core.finished &: ~:(fin_d.value) in

  (* Latch results, then raise print_req one cycle later (so printer sees latched values). *)
  let p1_latched = Variable.reg spec ~width:64 in
  let p2_latched = Variable.reg spec ~width:64 in
  let print_req_r = Variable.reg spec ~width:1 in
  let print_req = print_req_r.value in

  compile
    [ fin_d <-- core.finished

    ; (* default: no print request *)
      print_req_r <-- gnd

    ; when_ fin_pulse
        [ (* latch results (become visible next cycle) *)
          p1_latched <-- core.part1
        ; p2_latched <-- core.part2
        ; (* request printing (also becomes visible next cycle) *)
          print_req_r <-- vdd
        ]
    ];

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 p1_latched.value; valid = print_req }
      ; part2 = { value = uresize ~width:60 p2_latched.value; valid = print_req }
      }
  in

  { Ulx3s.O.
    leds = concat_lsb [ ~:clear; uart_rx_overflow; core.finished; zero 5 ]
  ; uart_tx = byte_out
  ; uart_rx_ready = vdd
  }
;;

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day02" ~scope create
;;
