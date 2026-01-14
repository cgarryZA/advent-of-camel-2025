(* src/day07.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth  = 32
let extra_synth_args = []

(* ====================== RAM ====================== *)

module Ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 1
  let depth           = 16384
  let num_ports       = 2
  let zero_on_startup = false
end)

(* ====================== LOADER ====================== *)
(* UART format:
   - 2 bytes header: start_col u16 little-endian
   - 2 bytes header: width     u16 little-endian
   - 2 bytes header: height    u16 little-endian
   - then grid bytes (normalized; 'S' already replaced with '.')
   Loader writes 1 bit per received grid byte:
     '^' -> 1
     everything else ('.', '\n', etc.) -> 0
   RTS ends load.
*)

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
      ; write_count   : 'a [@bits 14]  (* grid bytes only *)
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

  let create _ ({ clock; clear; uart_rx; uart_rts } : _ I.t) : _ O.t =
    let spec  = Reg_spec.create ~clock ~clear () in
    let phase = State_machine.create (module Phase) spec in

    let loaded = Variable.reg spec ~width:1 in

    let tmp_lo    = Variable.reg spec ~width:8 in
    let start_col = Variable.reg spec ~width:14 in
    let width     = Variable.reg spec ~width:14 in
    let height    = Variable.reg spec ~width:14 in

    let hi = select uart_rx.value ~high:7 ~low:0 in
    let assembled_u16 = uresize ~width:14 (concat_msb [ hi; tmp_lo.value ]) in

    (* count grid bytes actually written into RAM (excludes 6-byte header) *)
    let write_count =
      reg_fb spec ~width:14
        ~enable:(uart_rx.valid &: phase.is Load_grid &: ~:(loaded.value))
        ~f:(fun x -> x +:. 1)
    in

    compile
      [ when_ uart_rts [ loaded <-- vdd ]

      ; phase.switch
          [ (Start_lo,
              [ when_ (uart_rx.valid &: ~:(loaded.value))
                  [ tmp_lo <-- uresize ~width:8 uart_rx.value
                  ; phase.set_next Start_hi
                  ]
              ])

          ; (Start_hi,
              [ when_ (uart_rx.valid &: ~:(loaded.value))
                  [ start_col <-- assembled_u16
                  ; phase.set_next W_lo
                  ]
              ])

          ; (W_lo,
              [ when_ (uart_rx.valid &: ~:(loaded.value))
                  [ tmp_lo <-- uresize ~width:8 uart_rx.value
                  ; phase.set_next W_hi
                  ]
              ])

          ; (W_hi,
              [ when_ (uart_rx.valid &: ~:(loaded.value))
                  [ width <-- assembled_u16
                  ; phase.set_next H_lo
                  ]
              ])

          ; (H_lo,
              [ when_ (uart_rx.valid &: ~:(loaded.value))
                  [ tmp_lo <-- uresize ~width:8 uart_rx.value
                  ; phase.set_next H_hi
                  ]
              ])

          ; (H_hi,
              [ when_ (uart_rx.valid &: ~:(loaded.value))
                  [ height <-- assembled_u16
                  ; phase.set_next Load_grid
                  ]
              ])

          ; (Load_grid, [ (* RAM write is combinational below *) ])
          ]
      ]
    ;

    let wr_en = uart_rx.valid &: phase.is Load_grid &: ~:(loaded.value) in
    let wr_data = uart_rx.value ==:. Char.to_int '^' in

    { O.load_finished = loaded.value
    ; ram_write =
        { address      = uresize ~width:14 write_count
        ; write_data   = wr_data
        ; write_enable = wr_en
        }
    ; write_count
    ; start_col = start_col.value
    ; width     = width.value
    ; height    = height.value
    ; uart_rx_ready = vdd
    }
  ;;

  let hierarchical scope =
    let module S = Hierarchy.In_scope (I) (O) in
    S.hierarchical ~name:"loader" ~scope create
  ;;
end

(* ====================== STAGE 7 DEBUG ALGO ====================== *)
(* Two-beam scheduler, no loops, no visited set *)

module States = struct
  type t =
    | Loading
    | Beam0_req | Beam0_prime | Beam0_consume
    | Beam1_req | Beam1_prime | Beam1_consume
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo
    ~clock
    ~clear
    ~(ram_bit : Signal.t)
    ~(load_finished : Signal.t)
    ~(start_col : Signal.t)
    ~(width : Signal.t)
    ~(height : Signal.t)
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  let row = Variable.reg spec ~width:14 in
  let col = Variable.reg spec ~width:14 in
  let rd_addr = Variable.reg spec ~width:14 in

  let beam0_steps = Variable.reg spec ~width:14 in
  let beam1_steps = Variable.reg spec ~width:14 in

  let hit_row = Variable.reg spec ~width:14 in

  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  let stride =
    uresize ~width:14 (width +: of_int_trunc ~width:14 1)
  in

  let addr_of_row_col r c =
    let r14 = uresize ~width:14 r in
    let c14 = uresize ~width:14 c in
    let prod = r14 *: stride in
    let c_ext = uresize ~width:28 c14 in
    let sum = prod +: c_ext in
    uresize ~width:14 sum
  in

  compile
    [ sm.switch
        [ ( Loading
          , [ when_ load_finished
                [ row <--. 1
                ; col <-- start_col
                ; beam0_steps <--. 0
                ; beam1_steps <--. 0
                ; done_fired <-- gnd
                ; sm.set_next Beam0_req
                ]
            ]
          )

        (* -------- Beam 0: vertical -------- *)
        ; ( Beam0_req
          , [ rd_addr <-- addr_of_row_col row.value col.value
            ; sm.set_next Beam0_prime
            ]
          )
        ; ( Beam0_prime
          , [ sm.set_next Beam0_consume ]
          )
        ; ( Beam0_consume
          , [ beam0_steps <-- beam0_steps.value +:. 1
            ; if_ (ram_bit ==:. 1)
                [ hit_row <-- row.value
                ; row <-- hit_row.value
                ; col <-- start_col +:. 1
                ; sm.set_next Beam1_req
                ]
                [ row <-- row.value +:. 1
                ; if_ (row.value ==: height)
                    [ sm.set_next Beam1_req ]
                    [ sm.set_next Beam0_req ]
                ]
            ]
          )

        (* -------- Beam 1: right -------- *)
        ; ( Beam1_req
          , [ rd_addr <-- addr_of_row_col hit_row.value col.value
            ; sm.set_next Beam1_prime
            ]
          )
        ; ( Beam1_prime
          , [ sm.set_next Beam1_consume ]
          )
        ; ( Beam1_consume
          , [ beam1_steps <-- beam1_steps.value +:. 1
            ; if_ (col.value ==: width)
                [ sm.set_next Done ]
                [ if_ (ram_bit ==:. 1)
                    [ sm.set_next Done ]
                    [ col <-- col.value +:. 1
                    ; sm.set_next Beam1_req
                    ]
                ]
            ]
          )

        ; ( Done
          , [ when_ (~:(done_fired.value)) [ done_fired <-- vdd ] ]
          )
        ]
    ]
  ;

  rd_addr.value,
  uresize ~width:60 beam0_steps.value,
  uresize ~width:60 beam1_steps.value,
  done_pulse
;;




(* ====================== TOP ====================== *)

let create
    scope
    ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t)
  =
  let loader =
    Loader.hierarchical scope { clock; clear; uart_rx; uart_rts }
  in

  let ram_ports =
    Array.init 2 ~f:(fun _ -> Ram.Port.Of_signal.wires ())
  in

  let%tydi ram =
    Ram.hierarchical ~name:"ram" scope
      { clock
      ; clear
      ; load_ports    = [| loader.ram_write; Ram.Port.unused |]
      ; load_finished = loader.load_finished
      ; ram_ports
      }
  in

  let rd_addr, part1_dbg, part2_dbg, done_pulse =
    algo ~clock ~clear
      ~ram_bit:ram.read_data.(0)
      ~load_finished:loader.load_finished
      ~start_col:loader.start_col
      ~width:loader.width
      ~height:loader.height
  in

  Ram.Port.Of_signal.assign ram_ports.(0)
    { address      = rd_addr
    ; write_data   = zero 1
    ; write_enable = gnd
    }
  ;

  Ram.Port.Of_signal.assign ram_ports.(1)
    { address      = zero 14
    ; write_data   = zero 1
    ; write_enable = gnd
    }
  ;

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock
      ; clear
      ; part1 = { value = part1_dbg; valid = done_pulse }
      ; part2 = { value = part2_dbg; valid = done_pulse }
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
  S.hierarchical ~name:"day07" ~scope create
;;
