(* src/day08.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth  = 64
let extra_synth_args = []

let k_part1 = 10   (* use 1000 for real input *)

module Loader = struct
  module I = struct
    type 'a t =
      { clock    : 'a
      ; clear    : 'a
      ; uart_rx  : 'a Uart.Byte_with_valid.t
      ; uart_rts : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { load_finished : 'a
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create _ _ =
    { O.load_finished = vdd
    ; O.uart_rx_ready = vdd
    }

  let hierarchical scope =
    let module S = Hierarchy.In_scope (I) (O) in
    S.hierarchical ~name:"loader" ~scope create
end

module States = struct
  type t =
    | Loading
    | Init
    | Xs_wait
    | Xs_consume
    | Wait_u
    | Read_u
    | Wait_v
    | Read_v
    | FindU
    | FindV
    | Do_union
    | Sweep_init
    | Sweep_step
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo
    ~clock
    ~clear
    ~load_finished
    ~(uart_rts : Signal.t)
    ~(rx_byte  : Signal.t)
    ~(rx_valid : Signal.t)
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  (* ---------------- xs preload ---------------- *)
  let xs      = Array.init 20 ~f:(fun _ -> Variable.reg spec ~width:32) in
  let xs_vals = Array.map xs ~f:(fun v -> v.value) in

  let xs_word     = Variable.reg spec ~width:32 in
  let xs_byte_idx = Variable.reg spec ~width:2 in
  let xs_word_idx = Variable.reg spec ~width:5 in

  (* ---------------- edge stream ---------------- *)
  let edge_u     = Variable.reg spec ~width:5 in
  let edge_v     = Variable.reg spec ~width:5 in
  let edge_count = Variable.reg spec ~width:16 in

  (* ---------------- union-find ---------------- *)
  let parent     = Array.init 20 ~f:(fun _ -> Variable.reg spec ~width:5) in
  let size       = Array.init 20 ~f:(fun _ -> Variable.reg spec ~width:6) in
  let components = Variable.reg spec ~width:6 in

  let parent_vals = Array.map parent ~f:(fun v -> v.value) in
  let size_vals   = Array.map size   ~f:(fun v -> v.value) in

  let cur = Variable.reg spec ~width:5 in
  let ru  = Variable.reg spec ~width:5 in
  let rv  = Variable.reg spec ~width:5 in

  let last_u = Variable.reg spec ~width:5 in
  let last_v = Variable.reg spec ~width:5 in

  (* ---------------- Part 1 sweep ---------------- *)
  let sweep_i        = Variable.reg spec ~width:5 in
  let top1           = Variable.reg spec ~width:6 in
  let top2           = Variable.reg spec ~width:6 in
  let top3           = Variable.reg spec ~width:6 in
  let part1_captured = Variable.reg spec ~width:1 in

  (* ---------------- outputs ---------------- *)
  let part1      = Variable.reg spec ~width:60 in
  let part2      = Variable.reg spec ~width:60 in
  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  (* CRITICAL FIX:
     Only claim we're ready for a byte in states that actually "accept" a byte.
     If uart_rx_ready is high during Find/Union/etc, the runner will keep sending
     and we will drop bytes. *)
  let uart_rx_ready =
    sm.is Xs_wait |: sm.is Wait_u |: sm.is Wait_v
  in

  let read_rf rf idx = mux idx (Array.to_list rf) in

  let p_cur = read_rf parent_vals cur.value in
  let s_ru  = read_rf size_vals   ru.value in
  let s_rv  = read_rf size_vals   rv.value in

  let product_top3 =
    uresize ~width:60
      ((uresize ~width:60 top1.value *:
        uresize ~width:60 top2.value)
       *: uresize ~width:60 top3.value)
  in

  (* ---------------- byte assembly ---------------- *)
  let byte_u32 =
    uresize ~width:32 (select rx_byte ~high:7 ~low:0)
  in

  let byte_shifted =
    mux xs_byte_idx.value
      [ byte_u32
      ; sll byte_u32 ~by:8
      ; sll byte_u32 ~by:16
      ; sll byte_u32 ~by:24
      ]
  in

  let byte_mask =
    mux xs_byte_idx.value
      [ of_int_trunc ~width:32 0x000000FF
      ; of_int_trunc ~width:32 0x0000FF00
      ; of_int_trunc ~width:32 0x00FF0000
      ; of_int_trunc ~width:32 0xFF000000
      ]
  in

  let byte_masked =
    (xs_word.value &: ~:byte_mask) |: byte_shifted
  in

  let want_part1_sweep =
    (edge_count.value ==: of_int_trunc ~width:16 k_part1)
    &: ~:(part1_captured.value)
  in

  let merged_size = uresize ~width:6 (s_ru +: s_rv) in

  let attach_rv_to_ru =
    List.concat
      [ List.init 20 ~f:(fun i ->
          when_ (rv.value ==:. i)
            [ parent.(i) <-- ru.value
            ; size.(i)   <--. 0 ])
      ; List.init 20 ~f:(fun i ->
          when_ (ru.value ==:. i)
            [ size.(i) <-- merged_size ])
      ]
  in

  let attach_ru_to_rv =
    List.concat
      [ List.init 20 ~f:(fun i ->
          when_ (ru.value ==:. i)
            [ parent.(i) <-- rv.value
            ; size.(i)   <--. 0 ])
      ; List.init 20 ~f:(fun i ->
          when_ (rv.value ==:. i)
            [ size.(i) <-- merged_size ])
      ]
  in

  (* 32 -> 60 (zero-extend), mul -> 120, then take low 60 bits *)
  let part2_of_edge =
    let xu   = uresize ~width:60 (read_rf xs_vals edge_u.value) in
    let xv   = uresize ~width:60 (read_rf xs_vals edge_v.value) in
    let prod = xu *: xv in
    select prod ~high:59 ~low:0
  in

  compile
    [ sm.switch
        [ (Loading,
            [ when_ load_finished [ sm.set_next Init ] ])

        ; (Init,
            (List.concat (List.init 20 ~f:(fun i ->
               [ parent.(i) <--. i
               ; size.(i)   <--. 1
               ; xs.(i)     <--. 0 ])))
            @
            [ components      <--. 20
            ; edge_count      <--. 0
            ; xs_word         <--. 0
            ; xs_byte_idx     <--. 0
            ; xs_word_idx     <--. 0
            ; part1_captured  <-- gnd
            ; part1           <--. 0
            ; part2           <--. 0
            ; last_u          <--. 0
            ; last_v          <--. 0
            ; done_fired      <-- gnd
            ; sm.set_next Xs_wait ])

        ; (Xs_wait,
            [ if_ rx_valid [ sm.set_next Xs_consume ]
                [ sm.set_next Xs_wait ] ])

        ; (Xs_consume,
            let is_last_byte = xs_byte_idx.value ==:. 3 in
            let is_last_word = xs_word_idx.value ==:. 19 in
            [ xs_word <-- byte_masked
            ; if_ is_last_byte
                ((List.concat (List.init 20 ~f:(fun i ->
                    [ when_ (xs_word_idx.value ==:. i)
                        [ xs.(i) <-- byte_masked ] ])))
                 @
                 [ xs_word     <--. 0
                 ; xs_byte_idx <--. 0
                 ; if_ is_last_word
                     [ sm.set_next Wait_u ]
                     [ xs_word_idx <-- xs_word_idx.value +:. 1
                     ; sm.set_next Xs_wait ] ])
                [ xs_byte_idx <-- xs_byte_idx.value +:. 1
                ; sm.set_next Xs_wait ] ])

        ; (Wait_u,
            (* Priority: if a byte is present, consume it; RTS is only acted on when no byte is valid. *)
            [ if_ rx_valid
                [ sm.set_next Read_u ]
                [ if_ uart_rts
                    [ if_ (~:(part1_captured.value))
                        [ part1_captured <-- vdd
                        ; sm.set_next Sweep_init ]
                        [ if_ (components.value ==:. 1)
                            [ sm.set_next Done ]
                            [ sm.set_next Wait_u ] ] ]
                    [ sm.set_next Wait_u ] ] ])

        ; (Read_u,
            [ edge_u <-- select rx_byte ~high:4 ~low:0
            ; sm.set_next Wait_v ])

        ; (Wait_v,
            [ if_ rx_valid
                [ sm.set_next Read_v ]
                [ if_ uart_rts
                    [ if_ (~:(part1_captured.value))
                        [ part1_captured <-- vdd
                        ; sm.set_next Sweep_init ]
                        [ if_ (components.value ==:. 1)
                            [ sm.set_next Done ]
                            [ sm.set_next Wait_v ] ] ]
                    [ sm.set_next Wait_v ] ] ])

        ; (Read_v,
            [ edge_v     <-- select rx_byte ~high:4 ~low:0
            ; edge_count <-- edge_count.value +:. 1
            ; cur        <-- edge_u.value
            ; sm.set_next FindU ])

        ; (FindU,
            [ if_ (p_cur ==: cur.value)
                [ ru <-- cur.value
                ; cur <-- edge_v.value
                ; sm.set_next FindV ]
                [ cur <-- p_cur ] ])

        ; (FindV,
            [ if_ (p_cur ==: cur.value)
                [ rv <-- cur.value
                ; sm.set_next Do_union ]
                [ cur <-- p_cur ] ])

        ; (Do_union,
            let did_union   = ru.value <>: rv.value in
            let final_union = did_union &: (components.value ==:. 2) in
            [ when_ did_union
                ([ components <-- components.value -:. 1 ]
                 @ [ if_ (s_ru >=: s_rv) attach_rv_to_ru attach_ru_to_rv ])
            ; when_ final_union
                [ last_u <-- edge_u.value
                ; last_v <-- edge_v.value
                ; part2  <-- part2_of_edge ]
            ; when_ want_part1_sweep
                [ part1_captured <-- vdd
                ; sm.set_next Sweep_init ]
            ; when_ (~:(want_part1_sweep))
                [ sm.set_next Wait_u ]
            ])

        ; (Sweep_init,
            [ sweep_i <--. 0
            ; top1    <--. 0
            ; top2    <--. 0
            ; top3    <--. 0
            ; sm.set_next Sweep_step ])

        ; (Sweep_step,
            let i       = sweep_i.value in
            let pi      = read_rf parent_vals i in
            let is_root = pi ==: i in
            let s       = mux2 is_root (read_rf size_vals i) (zero 6) in
            [ when_ (s >: top1.value)
                [ top3 <-- top2.value
                ; top2 <-- top1.value
                ; top1 <-- s ]
            ; when_ ((s <=: top1.value) &: (s >: top2.value))
                [ top3 <-- top2.value
                ; top2 <-- s ]
            ; when_ ((s <=: top2.value) &: (s >: top3.value))
                [ top3 <-- s ]
            ; if_ (i ==:. 19)
                [ part1 <-- product_top3
                ; sm.set_next Wait_u ]
                [ sweep_i <-- i +:. 1
                ; sm.set_next Sweep_step ]
            ])

        ; (Done,
            [ when_ (~:(done_fired.value))
                [ done_fired <-- vdd ] ])
        ]
    ];

  part1.value, part2.value, done_pulse, uart_rx_ready

let create scope ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t) =
  let loader = Loader.hierarchical scope { clock; clear; uart_rx; uart_rts } in

  let p1, p2, done_pulse, algo_rx_ready =
    algo ~clock ~clear
      ~load_finished:loader.load_finished
      ~uart_rts
      ~rx_byte:uart_rx.value
      ~rx_valid:uart_rx.valid
  in

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock; clear
      ; part1 = { value = uresize ~width:60 p1; valid = done_pulse }
      ; part2 = { value = uresize ~width:60 p2; valid = done_pulse } }
  in

  { Ulx3s.O.
    leds          = concat_lsb [ ~:clear; uart_rx_overflow; loader.load_finished; zero 5 ]
  ; uart_tx       = byte_out
  ; uart_rx_ready = algo_rx_ready
  }

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day08" ~scope create
;;
