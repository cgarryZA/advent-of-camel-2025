open! Core
open! Hardcaml
open! Signal

let dial_bits = 7
let steps_bits = 32
let count_bits = 32

module I = struct
  type 'a t =
    { clock       : 'a
    ; clear       : 'a
    ; start       : 'a
    ; finish      : 'a
    ; direction   : 'a
    ; steps : 'a [@bits steps_bits]
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a
    ; p1          : 'a [@bits count_bits]
    ; p2          : 'a [@bits count_bits]
    ; finished    : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Wait_instr
    | Stepping
    | Finished
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope
    ({ clock; clear; start; finish; direction; steps; valid } : _ I.t)
  : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in

  let%hw_var value = Variable.reg spec ~width:dial_bits in
  let%hw_var p1 = Variable.reg spec ~width:count_bits in
  let%hw_var p2 = Variable.reg spec ~width:count_bits in
  let%hw_var remaining = Variable.reg spec ~width:steps_bits in
  let%hw_var dir = Variable.reg spec ~width:1 in

  let ready = Variable.wire ~default:gnd () in
  let finished = Variable.wire ~default:gnd () in

  let value_s = value.value in
  let right_next = mux2 (value_s ==:. 99) (zero dial_bits) (value_s +:. 1) in
  let left_next = mux2 (value_s ==:. 0) (of_int_trunc ~width:dial_bits 99) (value_s -:. 1) in
  let next_value = mux2 dir.value right_next left_next in
  let hit_zero = next_value ==:. 0 in

  compile
    [ sm.switch
        [ ( Idle
          , [ ready <-- gnd
            ; finished <-- gnd
            ; when_
                start
                [ value <-- of_int_trunc ~width:dial_bits 50
                ; p1 <-- zero count_bits
                ; p2 <-- zero count_bits
                ; sm.set_next Wait_instr
                ]
            ] )
        ; ( Wait_instr
          , [ ready <-- vdd
            ; finished <-- gnd
            ; when_
                valid
                [ dir <-- direction
                ; remaining <-- steps
                ;
                  when_
                    (steps ==:. 0)
                    [ when_ (value.value ==:. 0) [ p1 <-- p1.value +:. 1 ] ]
                ; when_
                    (~:(steps ==:. 0))
                    [ sm.set_next Stepping ]
                ]
            ; when_ finish [ sm.set_next Finished ]
            ] )
        ; ( Stepping
          , [ ready <-- gnd
            ; finished <-- gnd
            ;
              when_ (remaining.value ==:. 0) [ sm.set_next Wait_instr ]
            ; when_
                (remaining.value >:. 0)
                [
                  value <-- next_value
                ; when_ hit_zero [ p2 <-- p2.value +:. 1 ]
                ;
                  when_
                    (remaining.value ==:. 1)
                    [ when_ hit_zero [ p1 <-- p1.value +:. 1 ]
                    ; remaining <-- zero steps_bits
                    ; sm.set_next Wait_instr
                    ]
                ; when_
                    (remaining.value >:. 1)
                    [ remaining <-- remaining.value -:. 1 ]
                ]
            ] )
        ; ( Finished
          , [ ready <-- gnd
            ; finished <-- vdd
            ; when_
                start
                [ value <-- of_int_trunc ~width:dial_bits 50
                ; p1 <-- zero count_bits
                ; p2 <-- zero count_bits
                ; sm.set_next Wait_instr
                ]
            ] )
        ]
    ];

  { ready = ready.value
  ; p1 = p1.value
  ; p2 = p2.value
  ; finished = finished.value
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day1_dial" create
;;
