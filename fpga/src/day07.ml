(* src/day07.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth  = 32
let extra_synth_args = []

(* ====================== GRID RAM ====================== *)

module Grid_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 1
  let depth           = 16384
  let num_ports       = 2
  let zero_on_startup = false
end)

let grid_port_tieoff ~(addr : Signal.t) : Signal.t Grid_ram.Port.t =
  { address = addr; write_data = zero 1; write_enable = gnd }
;;

(* ====================== COUNTS RAM (DP) ====================== *)
(* Ping-pong banks: current row counts (read) -> next row counts (write) *)

module Count_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 60
  let depth           = 16384
  let num_ports       = 2
  let zero_on_startup = true
end)

let count_port_tieoff ~(addr : Signal.t) : Signal.t Count_ram.Port.t =
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

(* ====================== DP SWEEP FSM ====================== *)

module States = struct
  type t =
    | Loading
    | Init_counts
    | Prime0
    | Prime1
    | Prime2
    | Sweep
    | Next_row
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let create scope ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t) =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  let loader = Loader.hierarchical scope { clock; clear; uart_rx; uart_rts } in

  (* ---- Grid RAM ---- *)
  let grid_ports = Array.init 2 ~f:(fun _ -> Grid_ram.Port.Of_signal.wires ()) in
  let%tydi grid =
    Grid_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| loader.ram_write; grid_port_tieoff ~addr:(zero 14) |]
      ; load_finished  = loader.load_finished
      ; ram_ports      = grid_ports
      }
  in

  (* ---- Count banks ---- *)
  let count_a_ports = Array.init 2 ~f:(fun _ -> Count_ram.Port.Of_signal.wires ()) in
  let count_b_ports = Array.init 2 ~f:(fun _ -> Count_ram.Port.Of_signal.wires ()) in

  let%tydi count_a =
    Count_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| count_port_tieoff ~addr:(zero 14); count_port_tieoff ~addr:(zero 14) |]
      ; load_finished  = vdd
      ; ram_ports      = count_a_ports
      }
  in
  let%tydi count_b =
    Count_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| count_port_tieoff ~addr:(zero 14); count_port_tieoff ~addr:(zero 14) |]
      ; load_finished  = vdd
      ; ram_ports      = count_b_ports
      }
  in

  (* bank_sel=0: A=current(read), B=next(write)
     bank_sel=1: B=current(read), A=next(write) *)
  let bank_sel = Variable.reg spec ~width:1 in

  let count_rd =
    mux2 bank_sel.value count_a.read_data.(0) count_b.read_data.(0)
  in

  (* ---- Indices ---- *)
  let row_idx = Variable.reg spec ~width:14 in
  let col_idx = Variable.reg spec ~width:14 in

  (* ---- Sliding window: counts (c-1,c,c+1) and split bits (c-1,c,c+1) ---- *)
  let left_count  = Variable.reg spec ~width:60 in
  let mid_count   = Variable.reg spec ~width:60 in
  let right_count = Variable.reg spec ~width:60 in

  let left_split  = Variable.reg spec ~width:1 in
  let mid_split   = Variable.reg spec ~width:1 in
  let right_split = Variable.reg spec ~width:1 in

  (* ---- Outputs ---- *)
  let split_count = Variable.reg spec ~width:60 in
  let part2_sum   = Variable.reg spec ~width:60 in
  let done_level  = Variable.reg spec ~width:1 in
  let done_pulse  = Variable.reg spec ~width:1 in

  (* ---- Address math for grid (linear byte address, stride=width+1) ---- *)
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

  (* ---- RAM control wires (driven by FSM) ---- *)
  let count_read_addr  = Variable.wire ~default:(zero 14) in
  let dp_write_addr    = Variable.wire ~default:(zero 14) in
  let dp_write_data    = Variable.wire ~default:(zero 60) in
  let dp_write_en      = Variable.wire ~default:gnd in

  let grid_read_addr   = Variable.wire ~default:(zero 14) in

  (* ---- Hook up grid read port 0 ---- *)
  Grid_ram.Port.Of_signal.assign grid_ports.(0) (grid_port_tieoff ~addr:grid_read_addr.value);
  Grid_ram.Port.Of_signal.assign grid_ports.(1) (grid_port_tieoff ~addr:(zero 14));

  let grid_bit_rd = grid.read_data.(0) in

  (* ---- Hook up count banks ports ---- *)
  (* Read port0: current bank only *)
  let a_p0_addr = mux2 bank_sel.value count_read_addr.value (zero 14) in
  let b_p0_addr = mux2 bank_sel.value (zero 14) count_read_addr.value in

  Count_ram.Port.Of_signal.assign count_a_ports.(0)
    { address = a_p0_addr; write_data = zero 60; write_enable = gnd };
  Count_ram.Port.Of_signal.assign count_b_ports.(0)
    { address = b_p0_addr; write_data = zero 60; write_enable = gnd };

  (* Write port1: next bank only *)
  let a_p1_addr = mux2 bank_sel.value dp_write_addr.value (zero 14) in
  let b_p1_addr = mux2 bank_sel.value (zero 14) dp_write_addr.value in
  let a_p1_we   = bank_sel.value &: dp_write_en.value in
  let b_p1_we   = (~:bank_sel.value) &: dp_write_en.value in

  Count_ram.Port.Of_signal.assign count_a_ports.(1)
    { address = a_p1_addr; write_data = dp_write_data.value; write_enable = a_p1_we };
  Count_ram.Port.Of_signal.assign count_b_ports.(1)
    { address = b_p1_addr; write_data = dp_write_data.value; write_enable = b_p1_we };

  (* ---- Common flags ---- *)
  let width_minus_1  = loader.width -:. 1 in
  let height_minus_1 = loader.height -:. 1 in

  let col_last  = col_idx.value ==: width_minus_1 in
  let last_row  = row_idx.value ==: height_minus_1 in

  (* in Sweep, the arriving count_rd/grid_bit_rd correspond to (col_idx + 2) *)
  let incoming_col   = col_idx.value +:. 2 in
  let incoming_valid = incoming_col <: loader.width in

  let prefetch_col   = col_idx.value +:. 3 in
  let prefetch_valid = prefetch_col <: loader.width in

  (* ---- Stencil compute for next row count at column c ---- *)
  (* out[c] = (~s[c])*x[c] + s[c-1]*x[c-1] + s[c+1]*x[c+1] *)
  let from_down  = mux2 mid_split.value mid_count.value (zero 60) in
  let from_left  = mux2 left_split.value (zero 60) left_count.value in
  let from_right = mux2 right_split.value (zero 60) right_count.value in
  let out_sum    = from_down +: from_left +: from_right in
  let out_count  = uresize ~width:60 out_sum in

  let mid_nonzero = ~:(mid_count.value ==:. 0) in
  let split_hit   = mid_split.value &: mid_nonzero in

  (* ====================== FSM ====================== *)
  compile
    [ (* defaults (every cycle) *)
      dp_write_en   <-- gnd
    ; dp_write_addr <-- zero 14
    ; dp_write_data <-- zero 60
    ; count_read_addr <-- zero 14
    ; grid_read_addr  <-- zero 14
    ; done_pulse    <-- gnd

    ; sm.switch
        [ Loading,
          [ when_ loader.load_finished
              [ (* init writes go to A by setting next=A (bank_sel=1) *)
                bank_sel    <-- vdd
              ; row_idx     <--. 1
              ; col_idx     <--. 0

              ; left_count  <--. 0
              ; mid_count   <--. 0
              ; right_count <--. 0
              ; left_split  <-- gnd
              ; mid_split   <-- gnd
              ; right_split <-- gnd

              ; split_count <--. 0
              ; part2_sum   <--. 0
              ; done_level  <-- gnd

              ; sm.set_next Init_counts
              ]
          ]

        ; Init_counts,
          [ (* write A[col] = (col==start_col ? 1 : 0) *)
            dp_write_en   <-- vdd
          ; dp_write_addr <-- col_idx.value
          ; dp_write_data <-- mux2 (col_idx.value ==: loader.start_col)
                               (of_int_trunc ~width:60 0)
                               (of_int_trunc ~width:60 1)

          ; when_ col_last
              [ (* switch to normal: current=A, next=B *)
                bank_sel <-- gnd
              ; col_idx  <--. 0
              ; sm.set_next Prime0
              ]
          ; when_ (~:col_last)
              [ col_idx <-- col_idx.value +:. 1 ]
          ]

        ; Prime0,
          [ (* request col0 *)
            count_read_addr <--. 0
          ; grid_read_addr  <-- grid_addr row_idx.value (zero 14)

          ; left_count <--. 0
          ; left_split <-- gnd
          ; col_idx    <--. 0

          ; sm.set_next Prime1
          ]

        ; Prime1,
          [ (* capture col0 into mid; request col1 *)
            mid_count <-- count_rd
          ; mid_split <-- grid_bit_rd

          ; count_read_addr <--. 1
          ; grid_read_addr  <-- grid_addr row_idx.value (of_int_trunc ~width:14 1)

          ; sm.set_next Prime2
          ]

        ; Prime2,
          [ (* capture col1 into right (or 0 if width<2); request col2 *)
            let col1_valid = (of_int_trunc ~width:14 1) <: loader.width in
            right_count <-- mux2 col1_valid (zero 60) count_rd
          ; right_split <-- mux2 col1_valid gnd grid_bit_rd

          ; count_read_addr <--. 2
          ; grid_read_addr  <-- grid_addr row_idx.value (of_int_trunc ~width:14 2)

          ; sm.set_next Sweep
          ]

        ; Sweep,
          [ (* write next[row+1, col_idx] *)
            dp_write_en   <-- vdd
          ; dp_write_addr <-- col_idx.value
          ; dp_write_data <-- out_count

          ; when_ split_hit [ split_count <-- split_count.value +:. 1 ]

          ; when_ last_row
              [ part2_sum <-- uresize ~width:60 (part2_sum.value +: out_count) ]

          ; (* shift window; incoming is (col_idx+2) *)
            let incoming_count = mux2 incoming_valid (zero 60) count_rd in
            let incoming_split = mux2 incoming_valid gnd grid_bit_rd in

            left_count  <-- mid_count.value
          ; mid_count   <-- right_count.value
          ; right_count <-- incoming_count

          ; left_split  <-- mid_split.value
          ; mid_split   <-- right_split.value
          ; right_split <-- incoming_split

          ; (* issue prefetch for (col_idx+3) for next cycle *)
            count_read_addr <-- mux2 prefetch_valid (zero 14) prefetch_col
          ; grid_read_addr  <-- mux2 prefetch_valid
                               (zero 14)
                               (grid_addr row_idx.value prefetch_col)

          ; when_ col_last
              [ (* finished this row *)
                when_ last_row
                  [ done_level <-- vdd
                  ; done_pulse <-- vdd
                  ; sm.set_next Done
                  ]
              ; when_ (~:last_row)
                  [ sm.set_next Next_row ]
              ]

          ; when_ (~:col_last)
              [ col_idx <-- col_idx.value +:. 1 ]
          ]

        ; Next_row,
          [ (* advance row: next bank becomes current *)
            bank_sel <-- ~:(bank_sel.value)
          ; row_idx  <-- row_idx.value +:. 1
          ; col_idx  <--. 0
          ; sm.set_next Prime0
          ]

        ; Done, []
        ]
    ]
  ;

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock; clear
      ; part1 = { value = split_count.value; valid = done_pulse.value }
      ; part2 = { value = part2_sum.value; valid = done_pulse.value }
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
