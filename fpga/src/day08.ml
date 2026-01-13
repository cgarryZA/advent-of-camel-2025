(* src/day08.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth  = 64
let extra_synth_args = []

(* ====================== RAM ====================== *)

module Ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 32
  let depth           = 16384
  let num_ports       = 2
  let zero_on_startup = false
end)

(* ====================== LOADER ====================== *)

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
      ; ram_write     : 'a Ram.Port.t
      ; word_count    : 'a [@bits 14]
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create _ ({ clock; clear; uart_rx; uart_rts } : _ I.t) =
    let spec = Reg_spec.create ~clock ~clear () in
    let word_in = Util.shift_in ~clock ~clear ~n:4 uart_rx in

    let loaded  = Variable.reg spec ~width:1 in
    let wr_addr = Variable.reg spec ~width:14 in
    let wr_en   = word_in.valid &: ~:(loaded.value) in

    compile
      [ when_ wr_en    [ wr_addr <-- wr_addr.value +:. 1 ]
      ; when_ uart_rts [ loaded  <-- vdd ]
      ];

    { O.
      load_finished = loaded.value
    ; ram_write =
        { address      = wr_addr.value
        ; write_data   = word_in.value
        ; write_enable = wr_en
        }
    ; word_count    = wr_addr.value
    ; uart_rx_ready = vdd
    }
  ;;

  let hierarchical scope =
    let module S = Hierarchy.In_scope (I) (O) in
    S.hierarchical ~name:"loader" ~scope create
  ;;
end

(* ====================== STAGE 15 ALGO ====================== *)
(* Top-10 edges by squared Euclidean distance, then apply them (Kruskal prefix).
   Part 1 = product of the sizes of the three largest circuits after 10 edges
   Part 2 = number of successful unions among those 10 edges
*)

module States = struct
  type t =
    | Loading
    | Init_scan
    (* scan pairs -> build top-10 edge list *)
    | Addr_ax | Wait_ax | Consume_ax
    | Addr_ay | Wait_ay | Consume_ay
    | Addr_az | Wait_az | Consume_az
    | Addr_bx | Wait_bx | Consume_bx
    | Addr_by | Wait_by | Consume_by
    | Addr_bz | Wait_bz | Consume_bz
    | Compute_edge
    | Advance_j
    | Advance_i
    (* init union-find *)
    | Init_uf
    (* apply edges 0..9 *)
    | Apply_load
    | FindU_step
    | FindV_step
    | Do_union
    | Next_edge
    (* sweep roots to find top3 sizes *)
    | Sweep_init
    | Sweep_step
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo ~clock ~clear ~load_finished ~(word_count : Signal.t) ~(read_word : Signal.t) =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  let addr   = Variable.reg spec ~width:14 in
  let base_i = Variable.reg spec ~width:14 in
  let base_j = Variable.reg spec ~width:14 in
  let i_idx  = Variable.reg spec ~width:5 in
  let j_idx  = Variable.reg spec ~width:5 in

  let ax = Variable.reg spec ~width:32 in
  let ay = Variable.reg spec ~width:32 in
  let az = Variable.reg spec ~width:32 in
  let bx = Variable.reg spec ~width:32 in
  let by = Variable.reg spec ~width:32 in
  let bz = Variable.reg spec ~width:32 in

  (* ===== Top-16 edges ===== *)
  let best_d2 = Array.init 16 ~f:(fun _ -> Variable.reg spec ~width:64) in
  let best_u  = Array.init 16 ~f:(fun _ -> Variable.reg spec ~width:5) in
  let best_v  = Array.init 16 ~f:(fun _ -> Variable.reg spec ~width:5) in

  (* ===== Union-find ===== *)
  let parent = Array.init 20 ~f:(fun _ -> Variable.reg spec ~width:5) in
  let size   = Array.init 20 ~f:(fun _ -> Variable.reg spec ~width:6) in
  let unions_ok = Variable.reg spec ~width:6 in
  let e_idx = Variable.reg spec ~width:4 in

  let cur = Variable.reg spec ~width:5 in
  let ru  = Variable.reg spec ~width:5 in
  let rv  = Variable.reg spec ~width:5 in

  let top1 = Variable.reg spec ~width:6 in
  let top2 = Variable.reg spec ~width:6 in
  let top3 = Variable.reg spec ~width:6 in
  let sweep_i = Variable.reg spec ~width:5 in

  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  let abs32 a b =
    let d = a -: b in
    mux2 (msb d) (zero 32 -: d) d
  in

  let sq64 x =
    let x64 = uresize ~width:64 x in
    uresize ~width:64 (x64 *: x64)
  in

  let d2 =
    sq64 (abs32 ax.value bx.value)
    +: sq64 (abs32 ay.value by.value)
    +: sq64 (abs32 az.value bz.value)
  in

  let bd = Array.map best_d2 ~f:(fun v -> v.value) in
  let bu = Array.map best_u  ~f:(fun v -> v.value) in
  let bv = Array.map best_v  ~f:(fun v -> v.value) in

  let insert_at k =
    let shifts =
      List.concat
        (List.init (15 - k) ~f:(fun t ->
           [ best_d2.(k+t+1) <-- bd.(k+t)
           ; best_u.(k+t+1)  <-- bu.(k+t)
           ; best_v.(k+t+1)  <-- bv.(k+t)
           ]))
    in
    shifts @
    [ best_d2.(k) <-- d2
    ; best_u.(k)  <-- i_idx.value
    ; best_v.(k)  <-- j_idx.value
    ]
  in

  let guards =
    Array.init 16 ~f:(fun k ->
      if k = 0 then d2 <: bd.(0)
      else (d2 >=: bd.(k-1)) &: (d2 <: bd.(k)))
  in

  let read_rf rf idx = mux idx (Array.to_list rf) in
  let parent_vals = Array.map parent ~f:(fun v -> v.value) in
  let size_vals   = Array.map size   ~f:(fun v -> v.value) in

  let p_cur = read_rf parent_vals cur.value in
  let s_ru  = read_rf size_vals   ru.value in
  let s_rv  = read_rf size_vals   rv.value in

  let edge_u = read_rf (Array.map best_u ~f:(fun v -> v.value)) e_idx.value in
  let edge_v = read_rf (Array.map best_v ~f:(fun v -> v.value)) e_idx.value in

  let product_top3 =
    uresize ~width:60
      ((uresize ~width:60 top1.value *:
        uresize ~width:60 top2.value) *:
        uresize ~width:60 top3.value)
  in

  compile
    [ sm.switch
        [ (Loading, [ when_ load_finished [ sm.set_next Init_scan ] ])

        ; (Init_scan,
            [ base_i <--. 0
            ; base_j <--. 3
            ; i_idx  <--. 0
            ; j_idx  <--. 1
            ; done_fired <-- gnd
            ]
            @ List.concat (List.init 16 ~f:(fun k ->
                [ best_d2.(k) <-- ones 64
                ; best_u.(k)  <--. 0
                ; best_v.(k)  <--. 0 ]))
            @ [ sm.set_next Addr_ax ])

        ; (Addr_ax, [ addr <-- base_i.value; sm.set_next Wait_ax ])
        ; (Wait_ax, [ sm.set_next Consume_ax ])
        ; (Consume_ax, [ ax <-- read_word; sm.set_next Addr_ay ])

        ; (Addr_ay, [ addr <-- base_i.value +:. 1; sm.set_next Wait_ay ])
        ; (Wait_ay, [ sm.set_next Consume_ay ])
        ; (Consume_ay, [ ay <-- read_word; sm.set_next Addr_az ])

        ; (Addr_az, [ addr <-- base_i.value +:. 2; sm.set_next Wait_az ])
        ; (Wait_az, [ sm.set_next Consume_az ])
        ; (Consume_az, [ az <-- read_word; sm.set_next Addr_bx ])

        ; (Addr_bx, [ addr <-- base_j.value; sm.set_next Wait_bx ])
        ; (Wait_bx, [ sm.set_next Consume_bx ])
        ; (Consume_bx, [ bx <-- read_word; sm.set_next Addr_by ])

        ; (Addr_by, [ addr <-- base_j.value +:. 1; sm.set_next Wait_by ])
        ; (Wait_by, [ sm.set_next Consume_by ])
        ; (Consume_by, [ by <-- read_word; sm.set_next Addr_bz ])

        ; (Addr_bz, [ addr <-- base_j.value +:. 2; sm.set_next Wait_bz ])
        ; (Wait_bz, [ sm.set_next Consume_bz ])
        ; (Consume_bz, [ bz <-- read_word; sm.set_next Compute_edge ])

        ; (Compute_edge,
            List.concat (List.init 16 ~f:(fun k ->
              [ when_ guards.(k) (insert_at k) ]))
            @ [ sm.set_next Advance_j ])

        ; (Advance_j,
            [ base_j <-- base_j.value +:. 3
            ; j_idx  <-- j_idx.value  +:. 1
            ; if_ ((base_j.value +:. 3) >=: word_count)
                [ sm.set_next Advance_i ]
                [ sm.set_next Addr_bx ] ])

        ; (Advance_i,
            [ base_i <-- base_i.value +:. 3
            ; i_idx  <-- i_idx.value  +:. 1
            ; base_j <-- base_i.value +:. 6
            ; j_idx  <-- i_idx.value  +:. 2
            ; if_ ((base_i.value +:. 6) >=: word_count)
                [ sm.set_next Init_uf ]
                [ sm.set_next Addr_ax ] ])

        ; (Init_uf,
            [ unions_ok <--. 0
            ; e_idx     <--. 0
            ; top1 <--. 0; top2 <--. 0; top3 <--. 0
            ; sweep_i <--. 0 ]
            @ List.concat (List.init 20 ~f:(fun i ->
                [ parent.(i) <--. i; size.(i) <--. 1 ]))
            @ [ sm.set_next Apply_load ])

        ; (Apply_load, [ cur <-- edge_u; sm.set_next FindU_step ])

        ; (FindU_step,
            [ if_ (p_cur ==: cur.value)
                [ ru <-- cur.value; cur <-- edge_v; sm.set_next FindV_step ]
                [ cur <-- p_cur; sm.set_next FindU_step ] ])

        ; (FindV_step,
            [ if_ (p_cur ==: cur.value)
                [ rv <-- cur.value; sm.set_next Do_union ]
                [ cur <-- p_cur; sm.set_next FindV_step ] ])

        ; (Do_union,
            let attach_rv_to_ru =
              List.concat
                [ List.init 20 ~f:(fun n ->
                    when_ (rv.value ==:. n) [ parent.(n) <-- ru.value ])
                ; List.init 20 ~f:(fun n ->
                    when_ (ru.value ==:. n) [ size.(n) <-- (s_ru +: s_rv) ])
                ]
            in
            let attach_ru_to_rv =
              List.concat
                [ List.init 20 ~f:(fun n ->
                    when_ (ru.value ==:. n) [ parent.(n) <-- rv.value ])
                ; List.init 20 ~f:(fun n ->
                    when_ (rv.value ==:. n) [ size.(n) <-- (s_ru +: s_rv) ])
                ]
            in
            [
              when_ (ru.value <>: rv.value)
                [
                  unions_ok <-- unions_ok.value +:. 1;
                  if_ (s_ru >=: s_rv)
                    attach_rv_to_ru
                    attach_ru_to_rv
                ];
              sm.set_next Next_edge
            ])

        ; (Next_edge,
            [ if_ (e_idx.value ==:. 15)
                [ sm.set_next Sweep_init ]
                [ e_idx <-- e_idx.value +:. 1
                ; sm.set_next Apply_load ] ])

        ; (Sweep_init,
            [ sweep_i <--. 0; top1 <--. 0; top2 <--. 0; top3 <--. 0
            ; sm.set_next Sweep_step ])

        ; (Sweep_step,
            let i = sweep_i.value in
            let is_root = (read_rf parent_vals i ==: i) in
            let s = mux2 is_root (read_rf size_vals i) (zero 6) in
            [ when_ (s >: top1.value)
                [ top3 <-- top2.value; top2 <-- top1.value; top1 <-- s ]
            ; when_ ((s <=: top1.value) &: (s >: top2.value))
                [ top3 <-- top2.value; top2 <-- s ]
            ; when_ ((s <=: top2.value) &: (s >: top3.value))
                [ top3 <-- s ]
            ; if_ (i ==:. 19)
                [ sm.set_next Done ]
                [ sweep_i <-- i +:. 1; sm.set_next Sweep_step ] ])

        ; (Done, [ when_ (~:(done_fired.value)) [ done_fired <-- vdd ] ])
        ] ];

  addr.value,
  product_top3,
  uresize ~width:60 unions_ok.value,
  done_pulse
;;

(* ====================== TOP ====================== *)

let create scope ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t) =
  let loader = Loader.hierarchical scope { clock; clear; uart_rx; uart_rts } in
  let ram_ports = Array.init 2 ~f:(fun _ -> Ram.Port.Of_signal.wires ()) in

  let%tydi ram =
    Ram.hierarchical ~name:"ram" scope
      { clock; clear
      ; load_ports    = [| loader.ram_write; Ram.Port.unused |]
      ; load_finished = loader.load_finished
      ; ram_ports
      }
  in

  let addr, p1, p2, done_pulse =
    algo ~clock ~clear
      ~load_finished:loader.load_finished
      ~word_count:loader.word_count
      ~read_word:ram.read_data.(0)
  in

  Ram.Port.Of_signal.assign ram_ports.(0)
    { address = addr; write_data = zero 32; write_enable = gnd };

  Ram.Port.Of_signal.assign ram_ports.(1)
    { address = zero 14; write_data = zero 32; write_enable = gnd };

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock; clear
      ; part1 = { value = uresize ~width:60 p1; valid = done_pulse }
      ; part2 = { value = uresize ~width:60 p2; valid = done_pulse }
      }
  in

  { Ulx3s.O.
    leds          = concat_lsb [ ~:clear; uart_rx_overflow; loader.load_finished; zero 5 ]
  ; uart_tx       = byte_out
  ; uart_rx_ready = loader.uart_rx_ready
  }
;;

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day08" ~scope create
;;
