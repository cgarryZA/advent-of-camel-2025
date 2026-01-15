(* src/day09.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth  = 32
let extra_synth_args = []

(* let ram_depth = 131072
let addr_bits = 17  *)

(* SIMULATION-ONLY IMPLEMENTATION
   Full weighted prefix-sum grid requires ~2 MiB RAM.
   This will NOT synthesize on ULX3S. *)
let ram_depth = 524288
let addr_bits = 19

(* ====================== RAM ====================== *)

module Ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 32
  let depth           = ram_depth
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
      ; data_words    : 'a [@bits addr_bits]
      ; uart_rx_ready : 'a
      ; overflow      : 'a
      }
    [@@deriving hardcaml]
  end

  let create _scope ({ clock; clear; uart_rx; uart_rts } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~clear () in

    (* Pack 4 bytes -> 1x 32-bit word *)
    let word_in = Util.shift_in ~clock ~clear ~n:4 uart_rx in

    let loaded   = Variable.reg spec ~width:1 in
    let overflow = Variable.reg spec ~width:1 in

    let word_count = Variable.reg spec ~width:addr_bits in
    let max_addr   = of_int_trunc ~width:addr_bits (ram_depth - 1) in

    (* Write-enable stops once overflow is set, but RX_READY does NOT. *)
    let we = word_in.valid &: ~:(loaded.value) &: ~:(overflow.value) in

    compile
      [ (* Advance address, saturating at max_addr, and latch overflow
           *after* successfully writing the last word. *)
        when_ we
          [ if_
              (word_count.value ==: max_addr)
              [ overflow <-- vdd ]
              [ word_count <-- word_count.value +:. 1 ]
          ]

      ; (* End-of-load: sticky *)
        when_ uart_rts
          [ loaded <-- vdd ]
      ];

    (* data_words is informational; it’s not used by the algo. Keep it stable. *)
    let data_words = Variable.reg spec ~width:addr_bits in
    compile
      [ when_ uart_rts
          [ data_words <-- word_count.value ]
      ];

    { O.
      load_finished = loaded.value
    ; ram_write =
        { address      = word_count.value
        ; write_data   = word_in.value
        ; write_enable = we
        }
    ; data_words    = data_words.value
      (* CRITICAL: ignore overflow for uart_rx_ready so the sender can finish and assert RTS. *)
    ; uart_rx_ready = ~:(loaded.value)
    ; overflow      = overflow.value
    }
  ;;

  let hierarchical scope =
    let module S = Hierarchy.In_scope (I) (O) in
    S.hierarchical ~name:"loader" ~scope create
  ;;
end

(* ====================== FSM ====================== *)

module States = struct
  type t =
    | Loading
    | Magic_read | Magic_consume
    | N_read | N_consume
    | Meta_w_read | Meta_w_consume
    | Meta_h_read | Meta_h_consume
    | I_read_x | I_consume_x
    | I_read_y | I_consume_y
    | I_read_idx | I_consume_idx
    | J_init
    | J_read_x | J_consume_x
    | J_read_y | J_consume_y
    | J_read_idx | J_consume_idx
    | Pair_compute
    | Ps_lo_read | Ps_lo_consume
    | Ps_hi_read | Ps_hi_consume
    | Pair_evaluate
    | Next_j
    | Next_i
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo
    ~clock
    ~clear
    ~(ram0_rd : Signal.t)
    ~(ram1_rd : Signal.t)
    ~(load_finished : Signal.t)
    ~(load_overflow : Signal.t)
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  let p0_addr_r = Variable.reg spec ~width:addr_bits in
  let p1_addr_r = Variable.reg spec ~width:addr_bits in
  let p0_addr = Variable.wire ~default:p0_addr_r.value () in
  let p1_addr = Variable.wire ~default:p1_addr_r.value () in

  let p0_req (a : Signal.t) = [ p0_addr <-- a; p0_addr_r <-- a ] in
  let p1_req (a : Signal.t) = [ p1_addr <-- a; p1_addr_r <-- a ] in

  let n_points   = Variable.reg spec ~width:32 in
  let meta_base  = Variable.reg spec ~width:addr_bits in
  let base_ps    = Variable.reg spec ~width:addr_bits in
  let ps_w       = Variable.reg spec ~width:32 in
  let ps_h       = Variable.reg spec ~width:32 in

  let i_idx = Variable.reg spec ~width:32 in
  let j_idx = Variable.reg spec ~width:32 in

  let i_x   = Variable.reg spec ~width:32 in
  let i_y   = Variable.reg spec ~width:32 in
  let i_ix  = Variable.reg spec ~width:16 in
  let i_iy  = Variable.reg spec ~width:16 in

  let j_x   = Variable.reg spec ~width:32 in
  let j_y   = Variable.reg spec ~width:32 in
  let j_ix  = Variable.reg spec ~width:16 in
  let j_iy  = Variable.reg spec ~width:16 in

  let max_p1 = Variable.reg spec ~width:64 in
  let max_p2 = Variable.reg spec ~width:64 in
  let pair_area = Variable.reg spec ~width:64 in

  let x_lo = Variable.reg spec ~width:32 in
  let x_hi = Variable.reg spec ~width:32 in
  let y_lo = Variable.reg spec ~width:32 in
  let y_hi = Variable.reg spec ~width:32 in

  let ps_step   = Variable.reg spec ~width:2 in
  let ps_lo_tmp = Variable.reg spec ~width:32 in
  let ps_a      = Variable.reg spec ~width:64 in
  let ps_b      = Variable.reg spec ~width:64 in
  let ps_c      = Variable.reg spec ~width:64 in
  let ps_d      = Variable.reg spec ~width:64 in

  let done_pending = Variable.reg spec ~width:1 in
  let done_out     = reg spec done_pending.value in

  let word2_of_u32 (x : Signal.t) = concat_lsb [ gnd; x ] in
  let mul3_u32 (x : Signal.t) =
    let x2 = word2_of_u32 x in
    uresize ~width:32 (x +: uresize ~width:32 x2)
  in
  let point_base_addr (idx : Signal.t) =
    let idx3 = mul3_u32 idx in
    uresize ~width:addr_bits ((of_int_trunc ~width:addr_bits 2) +: uresize ~width:addr_bits idx3)
  in
  let ps_entry_addr_lo ~(yy : Signal.t) ~(xx : Signal.t) =
    let prod = yy *: ps_w.value in
    let idx  = uresize ~width:32 (uresize ~width:32 prod +: xx) in
    let idx2 = word2_of_u32 idx in
    uresize ~width:addr_bits (base_ps.value +: uresize ~width:addr_bits idx2)
  in
  let ps_entry_addr_hi ~(yy : Signal.t) ~(xx : Signal.t) =
    ps_entry_addr_lo ~yy ~xx +:. 1
  in

  let abs_diff_u32 (a : Signal.t) (b : Signal.t) =
    let a_ge_b = a >=: b in
    mux2 a_ge_b (a -: b) (b -: a)
  in
  let compute_area_u32 (x1 : Signal.t) (y1 : Signal.t) (x2 : Signal.t) (y2 : Signal.t) =
    let dx  = abs_diff_u32 x1 x2 in
    let dy  = abs_diff_u32 y1 y2 in
    let dx1 = uresize ~width:64 (dx +:. 1) in
    let dy1 = uresize ~width:64 (dy +:. 1) in
    uresize ~width:64 (dx1 *: dy1)
  in

  let min_u16 (a : Signal.t) (b : Signal.t) = mux2 (a <=: b) a b in
  let max_u16 (a : Signal.t) (b : Signal.t) = mux2 (a <=: b) b a in

  let ps_x_sel =
    mux ps_step.value [ x_hi.value; x_hi.value; x_lo.value; x_lo.value ]
  in
  let ps_y_sel =
    mux ps_step.value [ y_hi.value; y_lo.value; y_hi.value; y_lo.value ]
  in

  let ix_lo = min_u16 i_ix.value j_ix.value in
  let ix_hi = max_u16 i_ix.value j_ix.value in
  let iy_lo = min_u16 i_iy.value j_iy.value in
  let iy_hi = max_u16 i_iy.value j_iy.value in

  let ps64 = concat_msb [ ram0_rd; ps_lo_tmp.value ] in

  let t1 = ps_a.value -: ps_b.value in
  let t2 = t1 -: ps_c.value in
  let allowed_sum = t2 +: ps_d.value in

  let ok   = allowed_sum ==: pair_area.value in
  let area = pair_area.value in

  compile
    [ sm.switch
        [ ( Loading
          , [ when_ load_finished
                [ max_p1       <--. 0
                ; max_p2       <--. 0
                ; done_pending <-- gnd
                ; i_idx        <--. 0
                ; j_idx        <--. 0
                ; (* If the load overflowed, bail out cleanly instead of running on garbage. *)
                  if_ load_overflow
                    [ done_pending <-- vdd
                    ; sm.set_next Done
                    ]
                    [ sm.set_next Magic_read ]
                ]
            ] )

        ; ( Magic_read
          , p0_req (of_int_trunc ~width:addr_bits 0) @ [ sm.set_next Magic_consume ] )
        ; ( Magic_consume
          , [ sm.set_next N_read ] )

        ; ( N_read
          , p0_req (of_int_trunc ~width:addr_bits 1) @ [ sm.set_next N_consume ] )
        ; ( N_consume
          , [ n_points <-- ram0_rd
            ; meta_base <--
                uresize ~width:addr_bits
                  ((of_int_trunc ~width:addr_bits 2)
                   +: uresize ~width:addr_bits (mul3_u32 ram0_rd))
            ; base_ps <--
                uresize ~width:addr_bits
                  ( uresize ~width:addr_bits
                      ((of_int_trunc ~width:addr_bits 2)
                       +: uresize ~width:addr_bits (mul3_u32 ram0_rd))
                    +:. 2 )
            ; i_idx <--. 0
            ; sm.set_next Meta_w_read
            ] )

        ; ( Meta_w_read
          , p0_req meta_base.value @ [ sm.set_next Meta_w_consume ] )
        ; ( Meta_w_consume
          , [ ps_w <-- ram0_rd; sm.set_next Meta_h_read ] )

        ; ( Meta_h_read
          , p0_req (meta_base.value +:. 1) @ [ sm.set_next Meta_h_consume ] )
        ; ( Meta_h_consume
          , [ ps_h <-- ram0_rd; sm.set_next I_read_x ] )

        ; ( I_read_x
          , p0_req (point_base_addr i_idx.value) @ [ sm.set_next I_consume_x ] )
        ; ( I_consume_x
          , [ i_x <-- ram0_rd; sm.set_next I_read_y ] )

        ; ( I_read_y
          , p0_req (point_base_addr i_idx.value +:. 1) @ [ sm.set_next I_consume_y ] )
        ; ( I_consume_y
          , [ i_y <-- ram0_rd; sm.set_next I_read_idx ] )

        ; ( I_read_idx
          , p0_req (point_base_addr i_idx.value +:. 2) @ [ sm.set_next I_consume_idx ] )
        ; ( I_consume_idx
          , [ i_ix <-- select ram0_rd ~high:15 ~low:0
            ; i_iy <-- select ram0_rd ~high:31 ~low:16
            ; sm.set_next J_init
            ] )

        ; ( J_init
          , [ j_idx <-- i_idx.value +:. 1; sm.set_next J_read_x ] )

        ; ( J_read_x
          , [ if_ (j_idx.value ==: n_points.value)
                [ sm.set_next Next_i ]
                (p1_req (point_base_addr j_idx.value) @ [ sm.set_next J_consume_x ])
            ] )
        ; ( J_consume_x
          , [ j_x <-- ram1_rd; sm.set_next J_read_y ] )

        ; ( J_read_y
          , p1_req (point_base_addr j_idx.value +:. 1) @ [ sm.set_next J_consume_y ] )
        ; ( J_consume_y
          , [ j_y <-- ram1_rd; sm.set_next J_read_idx ] )

        ; ( J_read_idx
          , p1_req (point_base_addr j_idx.value +:. 2) @ [ sm.set_next J_consume_idx ] )
        ; ( J_consume_idx
          , [ j_ix <-- select ram1_rd ~high:15 ~low:0
            ; j_iy <-- select ram1_rd ~high:31 ~low:16
            ; sm.set_next Pair_compute
            ] )

        ; ( Pair_compute
          , [ pair_area <-- compute_area_u32 i_x.value i_y.value j_x.value j_y.value
            ; max_p1 <--
                mux2
                  (compute_area_u32 i_x.value i_y.value j_x.value j_y.value >: max_p1.value)
                  (compute_area_u32 i_x.value i_y.value j_x.value j_y.value)
                  max_p1.value
            ; x_lo <-- uresize ~width:32 ix_lo
            ; x_hi <-- uresize ~width:32 (ix_hi +:. 1)
            ; y_lo <-- uresize ~width:32 iy_lo
            ; y_hi <-- uresize ~width:32 (iy_hi +:. 1)
            ; ps_step <--. 0
            ; sm.set_next Ps_lo_read
            ] )

        ; ( Ps_lo_read
          , p0_req (ps_entry_addr_lo ~yy:ps_y_sel ~xx:ps_x_sel) @ [ sm.set_next Ps_lo_consume ] )
        ; ( Ps_lo_consume
          , [ ps_lo_tmp <-- ram0_rd; sm.set_next Ps_hi_read ] )

        ; ( Ps_hi_read
          , p0_req (ps_entry_addr_hi ~yy:ps_y_sel ~xx:ps_x_sel) @ [ sm.set_next Ps_hi_consume ] )
        ; ( Ps_hi_consume
          , [ if_ (ps_step.value ==:. 0) [ ps_a <-- ps64 ] []
            ; if_ (ps_step.value ==:. 1) [ ps_b <-- ps64 ] []
            ; if_ (ps_step.value ==:. 2) [ ps_c <-- ps64 ] []
            ; if_ (ps_step.value ==:. 3) [ ps_d <-- ps64 ] []
            ; if_
                (ps_step.value ==:. 3)
                [ sm.set_next Pair_evaluate ]
                [ ps_step <-- ps_step.value +:. 1
                ; sm.set_next Ps_lo_read
                ]
            ] )

        ; ( Pair_evaluate
          , [ max_p2 <-- mux2 (ok &: (area >: max_p2.value)) area max_p2.value
            ; sm.set_next Next_j
            ] )

        ; ( Next_j
          , [ j_idx <-- j_idx.value +:. 1; sm.set_next J_read_x ] )

        ; ( Next_i
          , [ i_idx <-- i_idx.value +:. 1
            ; if_ ((i_idx.value +:. 1) >=: n_points.value)
                [ done_pending <-- vdd; sm.set_next Done ]
                [ sm.set_next I_read_x ]
            ] )

        ; ( Done
          , [ done_pending <-- vdd; sm.set_next Done ] )
        ]
    ];

  ( p0_addr.value
  , p1_addr.value
  , max_p1.value
  , max_p2.value
  , done_out
  , done_out
  )
;;

(* ====================== TOP ====================== *)

let create
    scope
    ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t)
  =
  let loader = Loader.hierarchical scope { clock; clear; uart_rx; uart_rts } in

  let ram_ports = Array.init 2 ~f:(fun _ -> Ram.Port.Of_signal.wires ()) in

  let%tydi ram =
    Ram.hierarchical ~name:"ram" scope
      { clock
      ; clear
      ; load_ports = [| loader.ram_write; Ram.Port.unused |]
      ; load_finished = loader.load_finished
      ; ram_ports
      }
  in

  let addr0, addr1, p1, p2, p1_valid, p2_valid =
    algo
      ~clock
      ~clear
      ~ram0_rd:ram.read_data.(0)
      ~ram1_rd:ram.read_data.(1)
      ~load_finished:loader.load_finished
      ~load_overflow:loader.overflow
  in

  Ram.Port.Of_signal.assign ram_ports.(0)
    { address      = mux2 loader.load_finished addr0 (zero addr_bits)
    ; write_data   = zero 32
    ; write_enable = gnd
    };

  Ram.Port.Of_signal.assign ram_ports.(1)
    { address      = mux2 loader.load_finished addr1 (zero addr_bits)
    ; write_data   = zero 32
    ; write_enable = gnd
    };

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 p1; valid = p1_valid }
      ; part2 = { value = uresize ~width:60 p2; valid = p2_valid }
      }
  in

  { Ulx3s.O.
    (* LED[0]=~clear, LED[1]=uart_rx_overflow (external), LED[2]=loader overflow, LED[3]=load_finished *)
    leds = concat_lsb [ ~:clear; uart_rx_overflow; loader.overflow; loader.load_finished; zero 4 ]
  ; uart_tx = byte_out
  ; uart_rx_ready = loader.uart_rx_ready
  }
;;

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day09" ~scope create
;;
