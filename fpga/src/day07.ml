(* src/day07.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth  = 32
let extra_synth_args = []

(* ====================== GRID RAM ====================== *)
(* Stores 1 bit per input byte: '^' -> 1, everything else -> 0.
   Layout matches the Python model: width chars + '\n' per row, so stride = width + 1. *)

module Grid_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 1
  let depth           = 16384
  let num_ports       = 2
  let zero_on_startup = false
end)

let grid_port_tieoff ~(addr : Signal.t) : Signal.t Grid_ram.Port.t =
  { address = addr; write_data = zero 1; write_enable = gnd }
;;

(* ====================== BFS / GRAPH RAMs ====================== *)

module Queue_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 28  (* row[13:0] || col[13:0] *)
  let depth           = 16384
  let num_ports       = 1
  let zero_on_startup = true
end)

let queue_port_tieoff ~(addr : Signal.t) : Signal.t Queue_ram.Port.t =
  { address = addr; write_data = zero 28; write_enable = gnd }
;;

module Visited_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 1
  let depth           = 16384
  let num_ports       = 1
  let zero_on_startup = true
end)

let visited_port_tieoff ~(addr : Signal.t) : Signal.t Visited_ram.Port.t =
  { address = addr; write_data = zero 1; write_enable = gnd }
;;

(* Edges per node (up to 2 successors). Packed 32-bit:
   [13:0]   succ0_id
   [27:14]  succ1_id
   [28]     succ0_valid
   [29]     succ1_valid
   [31:30]  0
*)
module Edge_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 32
  let depth           = 16384
  let num_ports       = 1
  let zero_on_startup = true
end)

let edge_port_tieoff ~(addr : Signal.t) : Signal.t Edge_ram.Port.t =
  { address = addr; write_data = zero 32; write_enable = gnd }
;;

(* Indegree counter per node *)
module Indeg_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 16
  let depth           = 16384
  let num_ports       = 1
  let zero_on_startup = true
end)

let indeg_port_tieoff ~(addr : Signal.t) : Signal.t Indeg_ram.Port.t =
  { address = addr; write_data = zero 16; write_enable = gnd }
;;

(* Topological order list (stores node_id). Dual port:
   port0 read (topo_head / dp_idx)
   port1 write (append)
*)
module Topo_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 14
  let depth           = 16384
  let num_ports       = 2
  let zero_on_startup = true
end)

let topo_port_tieoff ~(addr : Signal.t) : Signal.t Topo_ram.Port.t =
  { address = addr; write_data = zero 14; write_enable = gnd }
;;

(* Ways DP: ways[node] = number of paths to exits. *)
module Ways_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 60
  let depth           = 16384
  let num_ports       = 1
  let zero_on_startup = true
end)

let ways_port_tieoff ~(addr : Signal.t) : Signal.t Ways_ram.Port.t =
  { address = addr; write_data = zero 60; write_enable = gnd }
;;

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
      ; ram_write     : 'a Grid_ram.Port.t
      ; start_col     : 'a [@bits 14]
      ; width         : 'a [@bits 14]
      ; height        : 'a [@bits 14]
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml]
  end

  module Phase = struct
    type t =
      | Start_lo | Start_hi
      | W_lo     | W_hi
      | H_lo     | H_hi
      | Load_grid
    [@@deriving enumerate, sexp_of, compare ~localize]
  end

  let create _ ({ clock; clear; uart_rx; uart_rts } : _ I.t) =
    let spec  = Reg_spec.create ~clock ~clear () in
    let phase = State_machine.create (module Phase) spec in

    let loaded    = Variable.reg spec ~width:1 in
    let tmp_lo    = Variable.reg spec ~width:8 in
    let start_col = Variable.reg spec ~width:14 in
    let width     = Variable.reg spec ~width:14 in
    let height    = Variable.reg spec ~width:14 in

    let assembled_u16 = concat_msb [ uart_rx.value; tmp_lo.value ] in
    let assembled_u14 = uresize ~width:14 assembled_u16 in

    let write_addr =
      reg_fb spec ~width:14
        ~enable:(uart_rx.valid &: phase.is Load_grid &: ~:(loaded.value))
        ~f:(fun x -> x +:. 1)
    in

    compile
      [ when_ uart_rts [ loaded <-- vdd ]
      ; phase.switch
          [ Start_lo, [ when_ uart_rx.valid [ tmp_lo <-- uart_rx.value; phase.set_next Start_hi ] ]
          ; Start_hi, [ when_ uart_rx.valid [ start_col <-- assembled_u14; phase.set_next W_lo ] ]
          ; W_lo,     [ when_ uart_rx.valid [ tmp_lo <-- uart_rx.value; phase.set_next W_hi ] ]
          ; W_hi,     [ when_ uart_rx.valid [ width <-- assembled_u14; phase.set_next H_lo ] ]
          ; H_lo,     [ when_ uart_rx.valid [ tmp_lo <-- uart_rx.value; phase.set_next H_hi ] ]
          ; H_hi,     [ when_ uart_rx.valid [ height <-- assembled_u14; phase.set_next Load_grid ] ]
          ; Load_grid, []
          ]
      ]
    ;

    { O.load_finished = loaded.value
    ; ram_write =
        { address      = write_addr
        ; write_data   = uart_rx.value ==:. Char.to_int '^'
        ; write_enable = uart_rx.valid &: phase.is Load_grid &: ~:(loaded.value)
        }
    ; start_col     = start_col.value
    ; width         = width.value
    ; height        = height.value
    ; uart_rx_ready = vdd
    }

  let hierarchical scope =
    let module S = Hierarchy.In_scope (I) (O) in
    S.hierarchical ~name:"loader" ~scope create
end

(* ====================== FSM ====================== *)

module States = struct
  type t =
    | Loading
    | Init_start
    | QRead
    | QGot
    | CellAddr
    | CellEval
    | Succ0_Read
    | Succ0_Commit
    | Succ1_Read
    | Succ1_Commit
    | TopoInit
    | TopoRead
    | TopoGot
    | TopoEdgeGot
    | TopoS0_Read
    | TopoS0_Commit
    | TopoS0_Append
    | TopoS1_Read
    | TopoS1_Commit
    | TopoS1_Append
    | DpInit
    | DpReadU
    | DpGotU
    | DpEdgeGot
    | DpReadW0
    | DpGotW0
    | DpReadW1
    | DpGotW1
    | DpWriteU
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let create scope ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t) =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  let loader = Loader.hierarchical scope { clock; clear; uart_rx; uart_rts } in

  (* ---- Instantiate RAMs ---- *)

  let grid_ports = Array.init 2 ~f:(fun _ -> Grid_ram.Port.Of_signal.wires ()) in
  let%tydi grid =
    Grid_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| loader.ram_write; grid_port_tieoff ~addr:(zero 14) |]
      ; load_finished  = loader.load_finished
      ; ram_ports      = grid_ports
      }
  in

  let queue_ports = [| Queue_ram.Port.Of_signal.wires () |] in
  let%tydi queue =
    Queue_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| queue_port_tieoff ~addr:(zero 14) |]
      ; load_finished  = vdd
      ; ram_ports      = queue_ports
      }
  in

  let visited_ports = [| Visited_ram.Port.Of_signal.wires () |] in
  let%tydi visited =
    Visited_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| visited_port_tieoff ~addr:(zero 14) |]
      ; load_finished  = vdd
      ; ram_ports      = visited_ports
      }
  in

  let edge_ports = [| Edge_ram.Port.Of_signal.wires () |] in
  let%tydi edges =
    Edge_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| edge_port_tieoff ~addr:(zero 14) |]
      ; load_finished  = vdd
      ; ram_ports      = edge_ports
      }
  in

  let indeg_ports = [| Indeg_ram.Port.Of_signal.wires () |] in
  let%tydi indeg =
    Indeg_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| indeg_port_tieoff ~addr:(zero 14) |]
      ; load_finished  = vdd
      ; ram_ports      = indeg_ports
      }
  in

  let topo_ports = Array.init 2 ~f:(fun _ -> Topo_ram.Port.Of_signal.wires ()) in
  let%tydi topo =
    Topo_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| topo_port_tieoff ~addr:(zero 14); topo_port_tieoff ~addr:(zero 14) |]
      ; load_finished  = vdd
      ; ram_ports      = topo_ports
      }
  in

  let ways_ports = [| Ways_ram.Port.Of_signal.wires () |] in
  let%tydi ways =
    Ways_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| ways_port_tieoff ~addr:(zero 14) |]
      ; load_finished  = vdd
      ; ram_ports      = ways_ports
      }
  in

  (* ====================== Helpers ====================== *)

  (* grid linear address: r*(width+1) + c *)
  let stride =
    let w28 = uresize ~width:28 loader.width in
    w28 +: of_int_trunc ~width:28 1
  in
  let grid_addr (r : Signal.t) (c : Signal.t) =
    let r28 = uresize ~width:28 r in
    let c28 = uresize ~width:28 c in
    let prod = r28 *: stride in
    let pw = Signal.width prod in
    let sum = prod +: uresize ~width:pw c28 in
    uresize ~width:14 sum
  in

  (* exit_base = height*width *)
  let exit_base14 =
    let h28 = uresize ~width:28 loader.height in
    let w28 = uresize ~width:28 loader.width in
    let prod = h28 *: w28 in
    uresize ~width:14 prod
  in

  let node_id_of_rc (r : Signal.t) (c : Signal.t) =
    let r28 = uresize ~width:28 r in
    let w28 = uresize ~width:28 loader.width in
    let c28 = uresize ~width:28 c in
    let prod = r28 *: w28 in
    let pw = Signal.width prod in
    let sum = prod +: uresize ~width:pw c28 in
    uresize ~width:14 sum
  in

  (* ====================== Control regs ====================== *)

  let q_head = Variable.reg spec ~width:14 in
  let q_tail = Variable.reg spec ~width:14 in
  let q_cnt  = Variable.reg spec ~width:15 in

  let cur_r  = Variable.reg spec ~width:14 in
  let cur_c  = Variable.reg spec ~width:14 in
  let cur_id = Variable.reg spec ~width:14 in

  let s0_r     = Variable.reg spec ~width:14 in
  let s0_c     = Variable.reg spec ~width:14 in
  let s0_id    = Variable.reg spec ~width:14 in
  let s0_valid = Variable.reg spec ~width:1 in

  let s1_r     = Variable.reg spec ~width:14 in
  let s1_c     = Variable.reg spec ~width:14 in
  let s1_id    = Variable.reg spec ~width:14 in
  let s1_valid = Variable.reg spec ~width:1 in

  let part1      = Variable.reg spec ~width:60 in
  let part2      = Variable.reg spec ~width:60 in
  let done_level = Variable.reg spec ~width:1 in
  let done_pulse = Variable.reg spec ~width:1 in

  let topo_head = Variable.reg spec ~width:14 in
  let topo_tail = Variable.reg spec ~width:14 in
  let topo_cnt  = Variable.reg spec ~width:14 in

  let topo_u = Variable.reg spec ~width:14 in

  let dp_idx = Variable.reg spec ~width:14 in
  let dp_u   = Variable.reg spec ~width:14 in
  let dp_w0  = Variable.reg spec ~width:60 in
  let dp_w1  = Variable.reg spec ~width:60 in

  let start_id = Variable.reg spec ~width:14 in

  (* ====================== Port wires (combinational) ====================== *)

  let grid_read_addr = Variable.wire ~default:(zero 14) () in

  let q_addr  = Variable.wire ~default:(zero 14) () in
  let q_wdata = Variable.wire ~default:(zero 28) () in
  let q_we    = Variable.wire ~default:gnd () in

  let vis_addr  = Variable.wire ~default:(zero 14) () in
  let vis_wdata = Variable.wire ~default:(zero 1) () in
  let vis_we    = Variable.wire ~default:gnd () in

  let edge_addr  = Variable.wire ~default:(zero 14) () in
  let edge_wdata = Variable.wire ~default:(zero 32) () in
  let edge_we    = Variable.wire ~default:gnd () in

  let indeg_addr  = Variable.wire ~default:(zero 14) () in
  let indeg_wdata = Variable.wire ~default:(zero 16) () in
  let indeg_we    = Variable.wire ~default:gnd () in

  let topo_addr0  = Variable.wire ~default:(zero 14) () in
  let topo_addr1  = Variable.wire ~default:(zero 14) () in
  let topo_wdata1 = Variable.wire ~default:(zero 14) () in
  let topo_we1    = Variable.wire ~default:gnd () in

  let ways_addr  = Variable.wire ~default:(zero 14) () in
  let ways_wdata = Variable.wire ~default:(zero 60) () in
  let ways_we    = Variable.wire ~default:gnd () in

  (* ====================== Connect RAM ports ====================== *)

  Grid_ram.Port.Of_signal.assign grid_ports.(0) (grid_port_tieoff ~addr:grid_read_addr.value);
  Grid_ram.Port.Of_signal.assign grid_ports.(1) (grid_port_tieoff ~addr:(zero 14));

  Queue_ram.Port.Of_signal.assign queue_ports.(0)
    { address = q_addr.value; write_data = q_wdata.value; write_enable = q_we.value };

  Visited_ram.Port.Of_signal.assign visited_ports.(0)
    { address = vis_addr.value; write_data = vis_wdata.value; write_enable = vis_we.value };

  Edge_ram.Port.Of_signal.assign edge_ports.(0)
    { address = edge_addr.value; write_data = edge_wdata.value; write_enable = edge_we.value };

  Indeg_ram.Port.Of_signal.assign indeg_ports.(0)
    { address = indeg_addr.value; write_data = indeg_wdata.value; write_enable = indeg_we.value };

  Topo_ram.Port.Of_signal.assign topo_ports.(0)
      { address = topo_addr0.value; write_data = zero 14; write_enable = gnd };
  Topo_ram.Port.Of_signal.assign topo_ports.(1)
      { address = topo_addr1.value; write_data = topo_wdata1.value; write_enable = topo_we1.value };

  Ways_ram.Port.Of_signal.assign ways_ports.(0)
    { address = ways_addr.value; write_data = ways_wdata.value; write_enable = ways_we.value };

  (* ====================== Read data aliases ====================== *)

  let grid_bit_rd = grid.read_data.(0) in
  let q_rd        = queue.read_data.(0) in
  let vis_rd      = visited.read_data.(0) in
  let indeg_rd    = indeg.read_data.(0) in
  let edge_rd     = edges.read_data.(0) in
  let topo_rd     = topo.read_data.(0) in
  let ways_rd     = ways.read_data.(0) in

  (* Edge decode *)
  let edge_s0_id    = select edge_rd ~high:13 ~low:0 in
  let edge_s1_id    = select edge_rd ~high:27 ~low:14 in
  let edge_s0_valid = select edge_rd ~high:28 ~low:28 in
  let edge_s1_valid = select edge_rd ~high:29 ~low:29 in

  (* QGot convenience decode (from q_rd) *)
  let q_r = select q_rd ~high:27 ~low:14 in
  let q_c = select q_rd ~high:13 ~low:0 in

  let q_is_exit = q_r ==: loader.height in
  let q_id_exit = uresize ~width:14 (exit_base14 +: q_c) in
  let q_id_norm = node_id_of_rc q_r q_c in
  let q_cur_id  = mux2 q_is_exit q_id_norm q_id_exit in

  let queue_empty = q_cnt.value ==:. 0 in
  let topo_done   = topo_head.value ==: topo_cnt.value in

  (* DP convenience *)
  let dp_u_is_exit = dp_u.value >=: exit_base14 in
  let dp_val_u =
    mux2 dp_u_is_exit
      (of_int_trunc ~width:60 1)
      (uresize ~width:60 (dp_w0.value +: dp_w1.value))
  in

  (* ----------------------
     Successor combinational signals (critical fix):
     These are used to WRITE Edge_ram in the SAME cycle as cell evaluation,
     so we must NOT use s0_id.value/s1_id.value (register values).
     ---------------------- *)
  let is_split  = grid_bit_rd in
  let r0        = cur_r.value in
  let c0        = cur_c.value in

  let left_ok   = c0 <>: of_int_trunc ~width:14 0 in
  let right_ok  = c0 <: (loader.width -:. 1) in

  let left_c    = c0 -:. 1 in
  let right_c   = c0 +:. 1 in
  let down_r    = r0 +:. 1 in

  let down_id =
    (* node_id_of_rc works for r==height too (it equals exit_base+c),
       but keep the explicit form to match the Python mental model. *)
    mux2 (down_r ==: loader.height)
      (uresize ~width:14 (exit_base14 +: c0))
      (node_id_of_rc down_r c0)
  in

  (* s0 = left (if splitter) else down *)
  let s0_r_n     = mux2 is_split r0 down_r in
  let s0_c_n     = mux2 is_split left_c c0 in
  let s0_id_n    = mux2 is_split (node_id_of_rc r0 left_c) down_id in
  let s0_valid_n = mux2 is_split left_ok vdd in

  (* s1 = right (only if splitter) *)
  let s1_r_n     = r0 in
  let s1_c_n     = right_c in
  let s1_id_n    = mux2 is_split (node_id_of_rc r0 right_c) (zero 14) in
  let s1_valid_n = is_split &: right_ok in

  let newv = indeg_rd -:. 1 in

  (* ====================== FSM ====================== *)

  compile
    [ (* defaults *)
      grid_read_addr <-- zero 14

    ; q_addr  <-- q_head.value
    ; q_wdata <-- zero 28
    ; q_we    <-- gnd

    ; vis_addr  <-- zero 14
    ; vis_wdata <-- zero 1
    ; vis_we    <-- gnd

    ; edge_addr  <-- zero 14
    ; edge_wdata <-- zero 32
    ; edge_we    <-- gnd

    ; indeg_addr  <-- zero 14
    ; indeg_wdata <-- zero 16
    ; indeg_we    <-- gnd

    ; topo_addr0  <-- topo_head.value
    ; topo_addr1  <-- topo_tail.value
    ; topo_wdata1 <-- zero 14
    ; topo_we1    <-- gnd

    ; ways_addr  <-- zero 14
    ; ways_wdata <-- zero 60
    ; ways_we    <-- gnd

    ; done_pulse <-- gnd

    ; sm.switch
        [ Loading,
          [ when_ loader.load_finished
              [ start_id <-- node_id_of_rc (of_int_trunc ~width:14 1) loader.start_col

              ; q_head <--. 0
              ; q_tail <--. 0
              ; q_cnt  <--. 0

              ; part1 <--. 0
              ; part2 <--. 0
              ; done_level <-- gnd

              ; topo_head <--. 0
              ; topo_tail <--. 0
              ; topo_cnt  <--. 0

              ; sm.set_next Init_start
              ]
          ]

        ; Init_start,
          [ (* enqueue start rc *)
            q_addr  <--. 0
          ; q_wdata <-- concat_msb [ of_int_trunc ~width:14 1; loader.start_col ]
          ; q_we    <-- vdd
          ; q_head  <--. 0
          ; q_tail  <--. 1
          ; q_cnt   <--. 1

          ; (* visited[start_id] = 1 *)
            vis_addr  <-- start_id.value
          ; vis_wdata <-- vdd
          ; vis_we    <-- vdd

          ; sm.set_next QRead
          ]

        ; QRead,
          [ when_ queue_empty [ sm.set_next TopoInit ]
          ; when_ (~:queue_empty)
              [ q_addr <-- q_head.value
              ; sm.set_next QGot
              ]
          ]

        ; QGot,
          [ (* capture rc *)
            cur_r <-- q_r
          ; cur_c <-- q_c
          ; cur_id <-- q_cur_id

          ; (* pop queue *)
            q_head <-- q_head.value +:. 1
          ; q_cnt  <-- q_cnt.value  -:. 1

          ; s0_valid <-- gnd
          ; s1_valid <-- gnd

          ; when_ q_is_exit
              [ (* exit node => no outgoing edges *)
                edge_addr  <-- q_cur_id
              ; edge_wdata <-- zero 32
              ; edge_we    <-- vdd
              ; sm.set_next Succ0_Read
              ]

          ; when_ (~:q_is_exit)
              [ sm.set_next CellAddr ]
          ]

        ; CellAddr,
          [ grid_read_addr <-- grid_addr cur_r.value cur_c.value
          ; sm.set_next CellEval
          ]

        ; CellEval,
          [ (* part1 counts distinct reachable splitters: BFS guarantees each node once *)
            when_ is_split [ part1 <-- part1.value +:. 1 ]

          ; (* latch successors into regs for the RAM-latency states that follow *)
            s0_r     <-- s0_r_n
          ; s0_c     <-- s0_c_n
          ; s0_id    <-- s0_id_n
          ; s0_valid <-- uresize ~width:1 s0_valid_n

          ; s1_r     <-- s1_r_n
          ; s1_c     <-- s1_c_n
          ; s1_id    <-- s1_id_n
          ; s1_valid <-- uresize ~width:1 s1_valid_n

          ; (* write edge table for cur_id (CRITICAL: use *_n, not stale regs) *)
            edge_addr <-- cur_id.value
          ; edge_wdata <--
              concat_lsb
                [ s0_id_n
                ; s1_id_n
                ; uresize ~width:1 s0_valid_n
                ; uresize ~width:1 s1_valid_n
                ; zero 2
                ]
          ; edge_we <-- vdd

          ; sm.set_next Succ0_Read
          ]

        ; Succ0_Read,
          [ when_ s0_valid.value
              [ vis_addr   <-- s0_id.value
              ; indeg_addr <-- s0_id.value
              ; sm.set_next Succ0_Commit
              ]
          ; when_ (~:(s0_valid.value)) [ sm.set_next Succ1_Read ]
          ]

        ; Succ0_Commit,
          [ indeg_addr  <-- s0_id.value
          ; indeg_wdata <-- uresize ~width:16 (indeg_rd +:. 1)
          ; indeg_we    <-- vdd

          ; when_ (~:vis_rd)
              [ vis_addr  <-- s0_id.value
              ; vis_wdata <-- vdd
              ; vis_we    <-- vdd

              ; q_addr  <-- q_tail.value
              ; q_wdata <-- concat_msb [ s0_r.value; s0_c.value ]
              ; q_we    <-- vdd
              ; q_tail  <-- q_tail.value +:. 1
              ; q_cnt   <-- q_cnt.value +:. 1
              ]

          ; sm.set_next Succ1_Read
          ]

        ; Succ1_Read,
          [ when_ s1_valid.value
              [ vis_addr   <-- s1_id.value
              ; indeg_addr <-- s1_id.value
              ; sm.set_next Succ1_Commit
              ]
          ; when_ (~:(s1_valid.value)) [ sm.set_next QRead ]
          ]

        ; Succ1_Commit,
          [ indeg_addr  <-- s1_id.value
          ; indeg_wdata <-- uresize ~width:16 (indeg_rd +:. 1)
          ; indeg_we    <-- vdd

          ; when_ (~:vis_rd)
              [ vis_addr  <-- s1_id.value
              ; vis_wdata <-- vdd
              ; vis_we    <-- vdd

              ; q_addr  <-- q_tail.value
              ; q_wdata <-- concat_msb [ s1_r.value; s1_c.value ]
              ; q_we    <-- vdd
              ; q_tail  <-- q_tail.value +:. 1
              ; q_cnt   <-- q_cnt.value +:. 1
              ]

          ; sm.set_next QRead
          ]

        (* ---------------- Topological order (match Python exactly) ----------------
           topo = [start_id]
           for u in topo:
             for v in succ(u):
               indeg[v] -= 1
               if indeg[v] < 1: topo.append(v)
        *)
        ; TopoInit,
          [ topo_head <--. 0
          ; topo_tail <--. 1
          ; topo_cnt  <--. 1

          ; topo_addr1  <--. 0
          ; topo_wdata1 <-- start_id.value
          ; topo_we1    <-- vdd

          ; sm.set_next TopoRead
          ]

        ; TopoRead,
          [ when_ topo_done [ sm.set_next DpInit ]
          ; when_ (~:topo_done)
              [ topo_addr0 <-- topo_head.value
              ; sm.set_next TopoGot
              ]
          ]

        ; TopoGot,
          [ topo_u    <-- topo_rd
          ; topo_head <-- topo_head.value +:. 1
          ; edge_addr <-- topo_rd
          ; sm.set_next TopoEdgeGot
          ]

        ; TopoEdgeGot,
          [ s0_id    <-- edge_s0_id
          ; s1_id    <-- edge_s1_id
          ; s0_valid <-- edge_s0_valid
          ; s1_valid <-- edge_s1_valid
          ; sm.set_next TopoS0_Read
          ]

        ; TopoS0_Read,
          [ when_ s0_valid.value
              [ indeg_addr <-- s0_id.value
              ; sm.set_next TopoS0_Commit
              ]
          ; when_ (~:(s0_valid.value)) [ sm.set_next TopoS1_Read ]
          ]

        ; TopoS0_Commit,
          [ indeg_addr  <-- s0_id.value
          ; indeg_wdata <-- uresize ~width:16 newv
          ; indeg_we    <-- vdd
          ; when_ (newv ==:. 0) [ sm.set_next TopoS0_Append ]
          ; when_ (newv <>: of_int_trunc ~width:16 0) [ sm.set_next TopoS1_Read ]
          ]

        ; TopoS0_Append,
          [ topo_addr1  <-- topo_tail.value
          ; topo_wdata1 <-- s0_id.value
          ; topo_we1    <-- vdd
          ; topo_tail   <-- topo_tail.value +:. 1
          ; topo_cnt    <-- topo_cnt.value +:. 1
          ; sm.set_next TopoS1_Read
          ]

        ; TopoS1_Read,
          [ when_ s1_valid.value
              [ indeg_addr <-- s1_id.value
              ; sm.set_next TopoS1_Commit
              ]
          ; when_ (~:(s1_valid.value)) [ sm.set_next TopoRead ]
          ]

        ; TopoS1_Commit,
          [ indeg_addr  <-- s1_id.value
          ; indeg_wdata <-- uresize ~width:16 newv
          ; indeg_we    <-- vdd
          ; when_ (newv ==:. 0) [ sm.set_next TopoS1_Append ]
          ; when_ (newv <>: of_int_trunc ~width:16 0) [ sm.set_next TopoRead ]
          ]

        ; TopoS1_Append,
          [ topo_addr1  <-- topo_tail.value
          ; topo_wdata1 <-- s1_id.value
          ; topo_we1    <-- vdd
          ; topo_tail   <-- topo_tail.value +:. 1
          ; topo_cnt    <-- topo_cnt.value +:. 1
          ; sm.set_next TopoRead
          ]

        (* ---------------- DP in reverse topo ---------------- *)
        ; DpInit,
          [ dp_idx <-- topo_cnt.value -:. 1
          ; sm.set_next DpReadU
          ]

        ; DpReadU,
          [ topo_addr0 <-- dp_idx.value
          ; sm.set_next DpGotU
          ]

        ; DpGotU,
          [ dp_u     <-- topo_rd
          ; edge_addr <-- topo_rd
          ; sm.set_next DpEdgeGot
          ]

        ; DpEdgeGot,
          [ s0_id    <-- edge_s0_id
          ; s1_id    <-- edge_s1_id
          ; s0_valid <-- edge_s0_valid
          ; s1_valid <-- edge_s1_valid

          ; dp_w0 <--. 0
          ; dp_w1 <--. 0

          ; when_ dp_u_is_exit [ sm.set_next DpWriteU ]
          ; when_ (~:dp_u_is_exit) [ sm.set_next DpReadW0 ]
          ]

        ; DpReadW0,
          [ when_ s0_valid.value
              [ ways_addr <-- s0_id.value
              ; sm.set_next DpGotW0
              ]
          ; when_ (~:(s0_valid.value)) [ sm.set_next DpReadW1 ]
          ]

        ; DpGotW0,
          [ dp_w0 <-- ways_rd
          ; sm.set_next DpReadW1
          ]

        ; DpReadW1,
          [ when_ s1_valid.value
              [ ways_addr <-- s1_id.value
              ; sm.set_next DpGotW1
              ]
          ; when_ (~:(s1_valid.value)) [ sm.set_next DpWriteU ]
          ]

        ; DpGotW1,
          [ dp_w1 <-- ways_rd
          ; sm.set_next DpWriteU
          ]

        ; DpWriteU,
          [ ways_addr  <-- dp_u.value
          ; ways_wdata <-- dp_val_u
          ; ways_we    <-- vdd

          ; when_ (dp_u.value ==: start_id.value) [ part2 <-- dp_val_u ]

          ; when_ (dp_idx.value ==:. 0)
              [ done_level <-- vdd
              ; done_pulse <-- vdd
              ; sm.set_next Done
              ]
          ; when_ (dp_idx.value <>: of_int_trunc ~width:14 0)
              [ dp_idx <-- dp_idx.value -:. 1
              ; sm.set_next DpReadU
              ]
          ]

        ; Done, []
        ]
    ]
  ;

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock; clear
      ; part1 = { value = part1.value; valid = done_pulse.value }
      ; part2 = { value = part2.value; valid = done_pulse.value }
      }
  in

  { Ulx3s.O.
    leds          = concat_lsb [ ~:clear; uart_rx_overflow; loader.load_finished; done_level.value; zero 4 ]
  ; uart_tx       = byte_out
  ; uart_rx_ready = loader.uart_rx_ready
  }

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day07" ~scope create
;;
