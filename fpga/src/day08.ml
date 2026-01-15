(* src/day08.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth  = 64
let extra_synth_args = []

(* ====================== PARAMS ====================== *)

let max_points = 1024
let addr_bits  = Int.ceil_log2 max_points
let idx_w      = addr_bits
let size_w     = addr_bits + 1
let count_w    = 32

let addr_lsb (x : Signal.t) = select x ~high:(addr_bits - 1) ~low:0
let idx_lsb  (x : Signal.t) = select x ~high:(idx_w - 1) ~low:0

(* ====================== RAMS ====================== *)

module Xs_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 32
  let depth           = max_points
  let num_ports       = 2
  let zero_on_startup = false
end)

module Parent_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = idx_w
  let depth           = max_points
  let num_ports       = 2
  let zero_on_startup = false
end)

module Size_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = size_w
  let depth           = max_points
  let num_ports       = 2
  let zero_on_startup = false
end)

(* ====================== LOADER (stub) ====================== *)

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

(* ====================== FSM ====================== *)

module States = struct
  type t =
    | Loading

    (* xs preload (UART -> xs RAM) *)
    | Xs_wait
    | Xs_consume
    | Xs_write

    (* init parent/size RAM for n points *)
    | Init_ram_setup
    | Init_ram_write

    (* edge stream (UART -> u16,v16) *)
    | Wait_u
    | Read_u
    | Wait_v
    | Read_v

    (* find roots via parent RAM *)
    | FindU_req
    | FindU_consume
    | FindV_req
    | FindV_consume

    (* union-by-size via size RAM *)
    | Union_read_sizes_req
    | Union_read_sizes_consume
    | Union_write_a
    | Union_write_b

    (* part2 xs read for final edge *)
    | Part2_xs_req
    | Part2_xs_consume

    (* part1 sweep over RAM *)
    | Sweep_init
    | Sweep_req
    | Sweep_consume

    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo
    ~clock
    ~clear
    ~(uart_rts : Signal.t)
    ~(rx_byte  : Signal.t)
    ~(rx_valid : Signal.t)
    ~(xs_rd    : Signal.t array)
    ~(parent_rd: Signal.t array)
    ~(size_rd  : Signal.t array)
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  (* ---------- counts / flags ---------- *)
  let n_points       = Variable.reg spec ~width:size_w in
  let init_i         = Variable.reg spec ~width:size_w in
  let components     = Variable.reg spec ~width:size_w in

  let k_part1_r      = Variable.reg spec ~width:count_w in

  let edge_count     = Variable.reg spec ~width:count_w in

  let part1_captured = Variable.reg spec ~width:1 in
  let want_sweep     = Variable.reg spec ~width:1 in
  let need_part2     = Variable.reg spec ~width:1 in

  (* ---------- xs UART assembly ---------- *)
  let xs_word     = Variable.reg spec ~width:32 in
  let xs_byte_idx = Variable.reg spec ~width:2 in
  let xs_word_idx = Variable.reg spec ~width:size_w in

  (* ---------- edge u16/v16 UART assembly ---------- *)
  let u_lo     = Variable.reg spec ~width:8 in
  let v_lo     = Variable.reg spec ~width:8 in
  let u_half   = Variable.reg spec ~width:1 in
  let v_half   = Variable.reg spec ~width:1 in
  let edge_u16 = Variable.reg spec ~width:16 in
  let edge_v16 = Variable.reg spec ~width:16 in

  (* ---------- union-find working regs ---------- *)
  let cur   = Variable.reg spec ~width:idx_w in
  let ru    = Variable.reg spec ~width:idx_w in
  let rv    = Variable.reg spec ~width:idx_w in

  let root  = Variable.reg spec ~width:idx_w in
  let child = Variable.reg spec ~width:idx_w in

  let merged_size = Variable.reg spec ~width:size_w in
  let did_union_r = Variable.reg spec ~width:1 in

  (* ---------- sweep regs ---------- *)
  let sweep_i = Variable.reg spec ~width:size_w in
  let top1    = Variable.reg spec ~width:size_w in
  let top2    = Variable.reg spec ~width:size_w in
  let top3    = Variable.reg spec ~width:size_w in

  (* ---------- outputs ---------- *)
  let part1      = Variable.reg spec ~width:60 in
  let part2      = Variable.reg spec ~width:60 in
  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  let uart_rx_ready =
    sm.is Xs_wait |: sm.is Wait_u |: sm.is Wait_v
  in

  (* ---------- helpers ---------- *)
  let u8       = select rx_byte ~high:7 ~low:0 in
  let byte_u32 = uresize ~width:32 u8 in

  let product_top3 =
    uresize ~width:60
      ((uresize ~width:60 top1.value *: uresize ~width:60 top2.value)
       *: (uresize ~width:60 top3.value))
  in

  let want_part1_sweep_now =
    (edge_count.value ==: k_part1_r.value)
    &: ~:(part1_captured.value)
  in

  (* ---------- xs byte assembly (little-endian) ---------- *)
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

  (* ---------- part2 compute (xs[u] * xs[v], low 60 bits) ---------- *)
  let part2_of_edge =
    let u_idx = idx_lsb edge_u16.value in
    let v_idx = idx_lsb edge_v16.value in
    let xu   = uresize ~width:60 (xs_rd.(0)) in
    let xv   = uresize ~width:60 (xs_rd.(1)) in
    let prod = xu *: xv in
    ignore (u_idx, v_idx);
    select prod ~high:59 ~low:0
  in

  (* ====================== FSM BODY ====================== *)

  compile
    [ sm.switch
        [ (Loading,
            [ (* reset all stateful bookkeeping *)
              n_points       <--. 0
            ; init_i         <--. 0
            ; components     <--. 0
            ; edge_count     <--. 0
            ; part1_captured <-- gnd
            ; want_sweep     <-- gnd
            ; need_part2     <-- gnd
            ; did_union_r    <-- gnd
            ; xs_word        <--. 0
            ; xs_byte_idx    <--. 0
            ; xs_word_idx    <--. 0
            ; u_half         <-- gnd
            ; v_half         <-- gnd
            ; u_lo           <--. 0
            ; v_lo           <--. 0
            ; edge_u16       <--. 0
            ; edge_v16       <--. 0
            ; cur            <--. 0
            ; ru             <--. 0
            ; rv             <--. 0
            ; root           <--. 0
            ; child          <--. 0
            ; merged_size    <--. 0
            ; sweep_i        <--. 0
            ; top1           <--. 0
            ; top2           <--. 0
            ; top3           <--. 0
            ; part1          <--. 0
            ; part2          <--. 0
            ; done_fired     <-- gnd
            ; sm.set_next Xs_wait
            ])

        ; (Xs_wait,
            [ (* End-of-xs delimiter: RTS true (pulse), with no rx_valid. *)
              if_ rx_valid
                [ sm.set_next Xs_consume ]
                [ if_ uart_rts
                    [ (* xs_word_idx is number of 32-bit words written *)
                      n_points   <-- xs_word_idx.value
                    ; sm.set_next Init_ram_setup
                    ]
                    [ sm.set_next Xs_wait ]
                ]
            ])

        ; (Xs_consume,
            let is_last_byte = xs_byte_idx.value ==:. 3 in
            [ xs_word <-- byte_masked
            ; if_ is_last_byte
                [ xs_byte_idx <--. 0
                ; sm.set_next Xs_write
                ]
                [ xs_byte_idx <-- xs_byte_idx.value +:. 1
                ; sm.set_next Xs_wait
                ]
            ])

        ; (Xs_write,
            [ xs_word     <--. 0
            ; xs_word_idx <-- xs_word_idx.value +:. 1
            ; sm.set_next Xs_wait
            ])

        ; (Init_ram_setup,
            [ init_i     <--. 0
            ; components <-- n_points.value
            ; k_part1_r <--
                mux2 (n_points.value <=:. 20)
                  (of_int_trunc ~width:count_w 10)
                  (mux2 (n_points.value <:. 1000)
                    (uresize ~width:count_w (srl n_points.value ~by:1))
                    (uresize ~width:count_w n_points.value))

            ; sm.set_next Init_ram_write
            ])

        ; (Init_ram_write,
            let last_i = n_points.value -:. 1 in
            [ if_ (init_i.value ==: last_i)
                [ sm.set_next Wait_u ]
                [ init_i <-- init_i.value +:. 1
                ; sm.set_next Init_ram_write
                ]
            ])

        ; (Wait_u,
            [ if_ rx_valid
                [ sm.set_next Read_u ]
                [ if_ uart_rts
                    [ if_ (~:(part1_captured.value))
                        [ part1_captured <-- vdd
                        ; sm.set_next Sweep_init
                        ]
                        [ if_ (components.value ==:. 1)
                            [ sm.set_next Done ]
                            [ sm.set_next Wait_u ]
                        ]
                    ]
                    [ sm.set_next Wait_u ]
                ]
            ])

        ; (Read_u,
            [ if_ (u_half.value ==:. 0)
                [ u_lo   <-- u8
                ; u_half <-- vdd
                ; sm.set_next Wait_u
                ]
                [ (* assemble u16 = lo | (hi<<8); stream is LE: lo then hi *)
                  edge_u16 <-- concat_msb [ u8; u_lo.value ]
                ; u_half   <-- gnd
                ; sm.set_next Wait_v
                ]
            ])

        ; (Wait_v,
            [ if_ rx_valid
                [ sm.set_next Read_v ]
                [ if_ uart_rts
                    [ if_ (~:(part1_captured.value))
                        [ part1_captured <-- vdd
                        ; sm.set_next Sweep_init
                        ]
                        [ if_ (components.value ==:. 1)
                            [ sm.set_next Done ]
                            [ sm.set_next Wait_v ]
                        ]
                    ]
                    [ sm.set_next Wait_v ]
                ]
            ])

        ; (Read_v,
            [ if_ (v_half.value ==:. 0)
                [ v_lo   <-- u8
                ; v_half <-- vdd
                ; sm.set_next Wait_v
                ]
                [ edge_v16  <-- concat_msb [ u8; v_lo.value ]
                ; v_half    <-- gnd
                ; edge_count <-- edge_count.value +:. 1
                ; cur       <-- idx_lsb edge_u16.value
                ; sm.set_next FindU_req
                ]
            ])

        ; (FindU_req,
            [ (* wait 1 cycle for RAM read_data to reflect cur address *)
              sm.set_next FindU_consume
            ])

        ; (FindU_consume,
            let p_cur = parent_rd.(0) in
            [ if_ (p_cur ==: cur.value)
                [ ru  <-- cur.value
                ; cur <-- idx_lsb edge_v16.value
                ; sm.set_next FindV_req
                ]
                [ cur <-- p_cur
                ; sm.set_next FindU_req
                ]
            ])

        ; (FindV_req,
            [ sm.set_next FindV_consume ])

        ; (FindV_consume,
            let p_cur = parent_rd.(0) in
            [ if_ (p_cur ==: cur.value)
                [ rv  <-- cur.value
                ; sm.set_next Union_read_sizes_req
                ]
                [ cur <-- p_cur
                ; sm.set_next FindV_req
                ]
            ])

        ; (Union_read_sizes_req,
            [ sm.set_next Union_read_sizes_consume ])

        ; (Union_read_sizes_consume,
            let s_ru = size_rd.(0) in
            let s_rv = size_rd.(1) in
            let did_union = ru.value <>: rv.value in
            let final_union = did_union &: (components.value ==:. 2) in
            let merged = uresize ~width:size_w (s_ru +: s_rv) in
            [ did_union_r <-- did_union
            ; merged_size <-- merged
            ; when_ did_union
                [ components <-- components.value -:. 1 ]
            ; when_ final_union
                [ need_part2 <-- vdd ]
            ; when_ (want_part1_sweep_now)
                [ want_sweep <-- vdd ]

            ; (* choose root/child by size *)
              if_ (s_ru >=: s_rv)
                [ root  <-- ru.value
                ; child <-- rv.value
                ]
                [ root  <-- rv.value
                ; child <-- ru.value
                ]

            ; sm.set_next Union_write_a
            ])

        ; (Union_write_a,
            [ sm.set_next Union_write_b ])

        ; (Union_write_b,
            [ if_ (need_part2.value ==:. 1)
                [ sm.set_next Part2_xs_req ]
                [ if_ (want_sweep.value ==:. 1)
                    [ part1_captured <-- vdd
                    ; want_sweep     <-- gnd
                    ; sm.set_next Sweep_init
                    ]
                    [ sm.set_next Wait_u ]
                ]
            ])

        ; (Part2_xs_req,
            [ sm.set_next Part2_xs_consume ])

        ; (Part2_xs_consume,
            [ part2      <-- part2_of_edge
            ; need_part2 <-- gnd
            ; if_ (want_sweep.value ==:. 1)
                [ part1_captured <-- vdd
                ; want_sweep     <-- gnd
                ; sm.set_next Sweep_init
                ]
                [ sm.set_next Wait_u ]
            ])

        ; (Sweep_init,
            [ sweep_i <--. 0
            ; top1    <--. 0
            ; top2    <--. 0
            ; top3    <--. 0
            ; sm.set_next Sweep_req
            ])

        ; (Sweep_req,
            [ sm.set_next Sweep_consume ])
        ; (Sweep_consume,
            let i  = sweep_i.value in
            let pi = parent_rd.(0) in
            let is_root = pi ==: idx_lsb i in
            let s =
              mux2 is_root
                size_rd.(0)
                (zero size_w)
            in
            let last_i = n_points.value -:. 1 in
            [ when_ (s >: top1.value)
                [ top3 <-- top2.value
                ; top2 <-- top1.value
                ; top1 <-- s
                ]
            ; when_ ((s <=: top1.value) &: (s >: top2.value))
                [ top3 <-- top2.value
                ; top2 <-- s
                ]
            ; when_ ((s <=: top2.value) &: (s >: top3.value))
                [ top3 <-- s
                ]
            ; if_ (i ==: last_i)
                [ part1 <-- product_top3
                ; sm.set_next Wait_u
                ]
                [ sweep_i <-- i +:. 1
                ; sm.set_next Sweep_req
                ]
            ])

        ; (Done,
            [ when_ (~:(done_fired.value))
                [ done_fired <-- vdd ] ])
        ]
    ];

  (* ====================== RAM PORT DRIVING (combinational) ====================== *)

  let xs_port0 : Signal.t Xs_ram.Port.t =
    { address =
        mux2 (sm.is Xs_write)
          (addr_lsb xs_word_idx.value)
          (mux2 (sm.is Part2_xs_req)
             (addr_lsb (uresize ~width:size_w (idx_lsb edge_u16.value)))
             (zero addr_bits))
    ; write_data = xs_word.value
    ; write_enable = sm.is Xs_write
    }
  in

  let xs_port1 : Signal.t Xs_ram.Port.t =
    { address =
        mux2 (sm.is Part2_xs_req)
          (addr_lsb (uresize ~width:size_w (idx_lsb edge_v16.value)))
          (zero addr_bits)
    ; write_data = zero 32
    ; write_enable = gnd
    }
  in

  let parent_port0 : Signal.t Parent_ram.Port.t =
    { address =
        mux2 (sm.is Init_ram_write)
          (addr_lsb init_i.value)
          (mux2 (sm.is Union_write_a)
             (addr_lsb (uresize ~width:size_w child.value))
             (mux2 (sm.is FindU_req |: sm.is FindV_req)
                (addr_lsb (uresize ~width:size_w cur.value))
                  (mux2 (sm.is Sweep_req |: sm.is Sweep_consume)
                    (addr_lsb sweep_i.value)
                    (zero addr_bits))))
    ; write_data =
        mux2 (sm.is Init_ram_write)
          (idx_lsb init_i.value)
          (idx_lsb root.value)
    ; write_enable =
        (sm.is Init_ram_write)
        |: (sm.is Union_write_a &: did_union_r.value)
    }
  in

  let parent_port1 : Signal.t Parent_ram.Port.t =
    { address = zero addr_bits
    ; write_data = zero idx_w
    ; write_enable = gnd
    }
  in

  let size_port0 : Signal.t Size_ram.Port.t =
    { address =
        mux2 (sm.is Init_ram_write)
          (addr_lsb init_i.value)
          (mux2 (sm.is Union_read_sizes_req)
             (addr_lsb (uresize ~width:size_w ru.value))
             (mux2 (sm.is Union_write_a)
                (addr_lsb (uresize ~width:size_w child.value))
                (mux2 (sm.is Union_write_b)
                   (addr_lsb (uresize ~width:size_w root.value))
                   (mux2 (sm.is Sweep_req |: sm.is Sweep_consume)
                    (addr_lsb sweep_i.value)
                    (zero addr_bits)))))
    ; write_data =
        mux2 (sm.is Init_ram_write)
          (of_int_trunc ~width:size_w 1)
          (mux2 (sm.is Union_write_a)
             (zero size_w)
             merged_size.value)
    ; write_enable =
        (sm.is Init_ram_write)
        |: ((sm.is Union_write_a |: sm.is Union_write_b) &: did_union_r.value)
    }
  in

  let size_port1 : Signal.t Size_ram.Port.t =
    { address =
        mux2 (sm.is Union_read_sizes_req)
          (addr_lsb (uresize ~width:size_w rv.value))
          (zero addr_bits)
    ; write_data = zero size_w
    ; write_enable = gnd
    }
  in

  let xs_ports     = [| xs_port0; xs_port1 |] in
  let parent_ports = [| parent_port0; parent_port1 |] in
  let size_ports   = [| size_port0; size_port1 |] in

  xs_ports, parent_ports, size_ports, part1.value, part2.value, done_pulse, uart_rx_ready
;;

(* ====================== TOP ====================== *)

let create scope ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t) =
  let loader = Loader.hierarchical scope { clock; clear; uart_rx; uart_rts } in

  let xs_ports_w     = Array.init 2 ~f:(fun _ -> Xs_ram.Port.Of_signal.wires ()) in
  let parent_ports_w = Array.init 2 ~f:(fun _ -> Parent_ram.Port.Of_signal.wires ()) in
  let size_ports_w   = Array.init 2 ~f:(fun _ -> Size_ram.Port.Of_signal.wires ()) in

  let%tydi xs_mem =
    Xs_ram.hierarchical ~name:"xs_ram" scope
      { clock
      ; clear
      ; load_ports = [| Xs_ram.Port.unused; Xs_ram.Port.unused |]
      ; load_finished = vdd
      ; ram_ports = xs_ports_w
      }
  in

  let%tydi parent_mem =
    Parent_ram.hierarchical ~name:"parent_ram" scope
      { clock
      ; clear
      ; load_ports = [| Parent_ram.Port.unused; Parent_ram.Port.unused |]
      ; load_finished = vdd
      ; ram_ports = parent_ports_w
      }
  in

  let%tydi size_mem =
    Size_ram.hierarchical ~name:"size_ram" scope
      { clock
      ; clear
      ; load_ports = [| Size_ram.Port.unused; Size_ram.Port.unused |]
      ; load_finished = vdd
      ; ram_ports = size_ports_w
      }
  in

  let xs_ports, parent_ports, size_ports, p1, p2, done_pulse, algo_rx_ready =
    algo
      ~clock
      ~clear
      ~uart_rts
      ~rx_byte:uart_rx.value
      ~rx_valid:uart_rx.valid
      ~xs_rd:xs_mem.read_data
      ~parent_rd:parent_mem.read_data
      ~size_rd:size_mem.read_data
  in

  Array.iteri xs_ports_w ~f:(fun i w -> Xs_ram.Port.Of_signal.assign w xs_ports.(i));
  Array.iteri parent_ports_w ~f:(fun i w -> Parent_ram.Port.Of_signal.assign w parent_ports.(i));
  Array.iteri size_ports_w ~f:(fun i w -> Size_ram.Port.Of_signal.assign w size_ports.(i));

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
  ; uart_rx_ready = algo_rx_ready
  }
;;

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day08" ~scope create
;;
