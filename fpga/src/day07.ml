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

module Queue_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 28
  let depth           = 256
  let num_ports       = 1
  let zero_on_startup = true
end)

module Visited_ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 1
  let depth           = 16384
  let num_ports       = 1
  let zero_on_startup = true
end)

let grid_port_tieoff ~(addr : Signal.t) : Signal.t Grid_ram.Port.t =
  { address = addr; write_data = zero 1; write_enable = gnd }
;;

let queue_port_tieoff ~(addr : Signal.t) : Signal.t Queue_ram.Port.t =
  { address = addr; write_data = zero 28; write_enable = gnd }
;;

let visited_port_tieoff ~(addr : Signal.t) : Signal.t Visited_ram.Port.t =
  { address = addr; write_data = zero 1; write_enable = gnd }
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
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml]
  end

  module Phase = struct
    type t = Start_lo | Start_hi | W_lo | W_hi | Load_grid
    [@@deriving enumerate, sexp_of, compare ~localize]
  end

  let create _ ({ clock; clear; uart_rx; uart_rts } : _ I.t) =
    let spec  = Reg_spec.create ~clock ~clear () in
    let phase = State_machine.create (module Phase) spec in

    let loaded    = Variable.reg spec ~width:1 in
    let tmp_lo    = Variable.reg spec ~width:8 in
    let start_col = Variable.reg spec ~width:14 in
    let width     = Variable.reg spec ~width:14 in

    let assembled =
      uresize ~width:14 (concat_msb [ uart_rx.value; tmp_lo.value ])
    in

    let write_addr =
      reg_fb spec ~width:14
        ~enable:(uart_rx.valid &: phase.is Load_grid &: ~:(loaded.value))
        ~f:(fun x -> x +:. 1)
    in

    compile
      [ when_ uart_rts [ loaded <-- vdd ]
      ; phase.switch
          [ Start_lo, [ when_ uart_rx.valid [ tmp_lo <-- uart_rx.value; phase.set_next Start_hi ] ]
          ; Start_hi, [ when_ uart_rx.valid [ start_col <-- assembled; phase.set_next W_lo ] ]
          ; W_lo,     [ when_ uart_rx.valid [ tmp_lo <-- uart_rx.value; phase.set_next W_hi ] ]
          ; W_hi,     [ when_ uart_rx.valid [ width <-- assembled; phase.set_next Load_grid ] ]
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
    | Deq
    | Emit
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo
    ~clock
    ~clear
    ~grid_bit
    ~queue_rd
    ~visited_rd
    ~load_finished
    ~start_col
    ~grid_width
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  (* Queue state *)
  let q_head = Variable.reg spec ~width:8 in
  let q_tail = Variable.reg spec ~width:8 in
  let q_cnt  = Variable.reg spec ~width:9 in

  (* Current cell *)
  let q_row  = Variable.reg spec ~width:14 in
  let q_col  = Variable.reg spec ~width:14 in

  (* Outputs *)
  let split_count = Variable.reg spec ~width:16 in
  let step_count  = Variable.reg spec ~width:16 in

  (* Address math (WIDTH-SAFE) *)
  let stride =
    (* stride = grid_width + 1, widen to 28 so multiply width is stable *)
    uresize ~width:28 (grid_width +: of_int_trunc ~width:14 1)
  in

  let addr r c =
    let r28  = uresize ~width:28 r in
    let c28  = uresize ~width:28 c in
    let prod = r28 *: stride in (* 56-bit *)
    let c56  = uresize ~width:(Signal.width prod) c28 in
    let sum  = prod +: c56 in
    uresize ~width:14 sum
  in

  let done_reg = Variable.reg spec ~width:1 in

  (* RAM ports *)
  let queue_addr   = Variable.reg spec ~width:8 in
  let queue_data   = Variable.reg spec ~width:28 in
  let queue_we     = Variable.reg spec ~width:1 in

  let visited_addr = Variable.reg spec ~width:14 in
  let visited_data = Variable.reg spec ~width:1 in
  let visited_we   = Variable.reg spec ~width:1 in

  let queue_empty = q_cnt.value ==:. 0 in

  compile
    [ (* defaults: drive EVERYTHING every cycle *)
      queue_we   <-- gnd
    ; visited_we <-- gnd

    ; queue_addr   <-- q_head.value
    ; queue_data   <-- queue_data.value

    ; visited_addr <-- visited_addr.value
    ; visited_data <-- visited_data.value

    ; sm.switch
        [ (* -------------------- LOADING -------------------- *)
          Loading,
          [ when_ load_finished
              [ q_head <--. 0
              ; q_tail <--. 1
              ; q_cnt  <--. 1

              ; q_row  <--. 1
              ; q_col  <-- start_col

              ; queue_addr <--. 0
              ; queue_data <-- concat_msb [ of_int_trunc ~width:14 1; start_col ]
              ; queue_we   <-- vdd

              ; split_count <--. 0
              ; step_count  <--. 0

              ; sm.set_next Deq
              ]
          ]

        (* -------------------- DEQUEUE -------------------- *)
        ; Deq,
          [ when_ queue_empty 
              [ done_reg <-- vdd
              ; sm.set_next Done 
              ]
          ; when_ (~:queue_empty)
              [ (* read head *)
                queue_addr <-- q_head.value

              ; q_row <-- select queue_rd ~high:27 ~low:14
              ; q_col <-- select queue_rd ~high:13 ~low:0

              ; q_head <-- q_head.value +:. 1
              ; q_cnt  <-- q_cnt.value -:. 1
              ; step_count <-- step_count.value +:. 1

              ; visited_addr <-- addr
                  (select queue_rd ~high:27 ~low:14)
                  (select queue_rd ~high:13 ~low:0)

              ; sm.set_next Emit
              ]
          ]

        (* -------------------- EMIT -------------------- *)
        ; Emit,
          [ (* ensure visited read address is stable in this state too *)
            visited_addr <-- addr q_row.value q_col.value

          ; if_ grid_bit
              [ (* splitter '^' *)
                when_ (~:visited_rd)
                  [ visited_we   <-- vdd
                  ; visited_data <-- vdd
                  ; split_count  <-- split_count.value +:. 1
                  ]

              ; queue_addr <-- q_tail.value
              ; queue_data <-- concat_msb [ q_row.value; q_col.value -:. 1 ]
              ; queue_we   <-- vdd
              ; q_tail     <-- q_tail.value +:. 1
              ; q_cnt      <-- q_cnt.value +:. 1
              ]
              [ (* empty -> down *)
                queue_addr <-- q_tail.value
              ; queue_data <-- concat_msb [ q_row.value +:. 1; q_col.value ]
              ; queue_we   <-- vdd
              ; q_tail     <-- q_tail.value +:. 1
              ; q_cnt      <-- q_cnt.value +:. 1
              ]

          ; sm.set_next Deq
          ]

        (* -------------------- DONE -------------------- *)
        ; Done, []
        ]
    ]
  ;

  ( { Queue_ram.Port.address      = queue_addr.value
    ; write_data                  = queue_data.value
    ; write_enable                = queue_we.value
    }
  , { Visited_ram.Port.address    = visited_addr.value
    ; write_data                  = visited_data.value
    ; write_enable                = visited_we.value
    }
  , addr q_row.value q_col.value
  , uresize ~width:60 split_count.value
  , uresize ~width:60 step_count.value
  , done_reg.value
  )

(* ====================== TOP ====================== *)

let create scope ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t) =
  let loader = Loader.hierarchical scope { clock; clear; uart_rx; uart_rts } in

  let grid_ports    = Array.init 2 ~f:(fun _ -> Grid_ram.Port.Of_signal.wires ()) in
  let queue_ports   = [| Queue_ram.Port.Of_signal.wires () |] in
  let visited_ports = [| Visited_ram.Port.Of_signal.wires () |] in

  let%tydi grid =
    Grid_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| loader.ram_write; grid_port_tieoff ~addr:(zero 14) |]
      ; load_finished  = loader.load_finished
      ; ram_ports      = grid_ports
      }
  in

  let%tydi queue =
    Queue_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| queue_port_tieoff ~addr:(zero 8) |]
      ; load_finished  = vdd
      ; ram_ports      = queue_ports
      }
  in

  let%tydi visited =
    Visited_ram.hierarchical scope
      { clock; clear
      ; load_ports     = [| visited_port_tieoff ~addr:(zero 14) |]
      ; load_finished  = vdd
      ; ram_ports      = visited_ports
      }
  in

  let queue_p, visited_p, grid_rd_addr, p1, p2, done_pulse =
    algo
      ~clock ~clear
      ~grid_bit:grid.read_data.(0)
      ~queue_rd:queue.read_data.(0)
      ~visited_rd:visited.read_data.(0)
      ~load_finished:loader.load_finished
      ~start_col:loader.start_col
      ~grid_width:loader.width
  in

  Queue_ram.Port.Of_signal.assign queue_ports.(0) queue_p;
  Visited_ram.Port.Of_signal.assign visited_ports.(0) visited_p;

  (* grid read port 0 driven by algorithm; port 1 tied off *)
  Grid_ram.Port.Of_signal.assign grid_ports.(0) (grid_port_tieoff ~addr:grid_rd_addr);
  Grid_ram.Port.Of_signal.assign grid_ports.(1) (grid_port_tieoff ~addr:(zero 14));

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock; clear
      ; part1 = { value = p1; valid = done_pulse }
      ; part2 = { value = p2; valid = done_pulse }
      }
  in

  { Ulx3s.O.
    leds          = concat_lsb [ ~:clear; uart_rx_overflow; loader.load_finished; done_pulse; zero 4 ]
  ; uart_tx       = byte_out
  ; uart_rx_ready = loader.uart_rx_ready
  }

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day07" ~scope create
;;
