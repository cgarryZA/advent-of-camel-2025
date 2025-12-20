open! Core
open! Hardcaml
open! Signal
open! Always

let dial_bits = 7
let steps_bits = 32
let count_bits = 32

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; direction : 'a
    ; steps : 'a [@bits steps_bits]
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a
    ; p1 : 'a [@bits count_bits]
    ; p2 : 'a [@bits count_bits]
    ; finished : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Run
    | Finished
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let div100_u32 (x : Signal.t) : Signal.t =
  let x = uresize ~width:32 x in
  let magic = of_int_trunc ~width:32 0x51EB851F in
  let prod = x *: magic in
  let q = select prod ~high:63 ~low:37 in
  uresize ~width:32 q
;;

let mod100_u32_to7 (x : Signal.t) : Signal.t =
  let q = div100_u32 x in
  let c100 = of_int_trunc ~width:7 100 in
  let xw = uresize ~width:39 x in
  let prod = q *: c100 in
  let r = xw -: prod in
  uresize ~width:7 r
;;

let create scope ({ clock; clear; start; finish; direction; steps; valid } : _ I.t)
  : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm = State_machine.create (module States) spec in

  let%hw_var value = Variable.reg spec ~width:dial_bits in
  let%hw_var p1 = Variable.reg spec ~width:count_bits in
  let%hw_var p2 = Variable.reg spec ~width:count_bits in

  let ready_s = sm.is Run in
  let accept = ready_s &: valid in

  let value_s = value.value in
  let delta = mod100_u32_to7 steps in

  let value9 = uresize ~width:9 value_s in
  let delta9 = uresize ~width:9 delta in

  let sum9 =
    (uresize ~width:9 value_s) +: (uresize ~width:9 delta)
  in
  let sum_ge_100 = sum9 >=:. 100 in
  let right9 = mux2 sum_ge_100 (sum9 -:. 100) sum9 in
  let right7 = uresize ~width:dial_bits right9 in

  let ge = value9 >=: delta9 in
  let left9 =
    mux2 ge
      (value9 -: delta9)
      ((value9 +:. 100) -: delta9)
  in
  let left7 = uresize ~width:dial_bits left9 in

  let next_value = mux2 direction right7 left7 in
  let next_is_zero = next_value ==:. 0 in

  let c100_7 = of_int_trunc ~width:dial_bits 100 in
  let right_first = c100_7 -: value_s in
  let left_first = mux2 (value_s ==:. 0) c100_7 value_s in
  let first7 = mux2 direction right_first left_first in

  let first32 = uresize ~width:32 first7 in
  let steps_ge_first = steps >=: first32 in
  let diff = steps -: first32 in
  let q_diff = div100_u32 diff in

  let hits32 =
    mux2 steps_ge_first
      (uresize ~width:32 (q_diff +:. 1))
      (zero 32)
  in

  let p1_next = uresize ~width:count_bits (p1.value +:. 1) in
  let p2_next = uresize ~width:count_bits (p2.value +: hits32) in

  compile
    [ sm.switch
        [ ( Idle
          , [ when_ start
                [ value <-- of_int_trunc ~width:dial_bits 50
                ; p1 <-- zero count_bits
                ; p2 <-- zero count_bits
                ; sm.set_next Run
                ]
            ] )
        ; ( Run
          , [ when_ accept
                [ value <-- next_value
                ; when_ next_is_zero [ p1 <-- p1_next ]
                ; p2 <-- p2_next
                ]
            ; when_ (finish &: ~:accept) [ sm.set_next Finished ]
            ] )
        ; ( Finished
          , [ when_ start
                [ value <-- of_int_trunc ~width:dial_bits 50
                ; p1 <-- zero count_bits
                ; p2 <-- zero count_bits
                ; sm.set_next Run
                ]
            ] )
        ]
    ];

  ignore scope;

  { ready = ready_s
  ; p1 = p1.value
  ; p2 = p2.value
  ; finished = sm.is Finished
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day1" create
;;
