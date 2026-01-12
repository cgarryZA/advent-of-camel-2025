(* src/day11.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz

let uart_fifo_depth  = 64
let extra_synth_args = []

(* ====================== RAM ====================== *)

module Ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 64
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
      ; data_words    : 'a [@bits 14]
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create _scope ({ clock; clear; uart_rx; uart_rts } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~clear () in

    (* Pack 8 bytes -> 1x 64-bit word *)
    let word_in = Util.shift_in ~clock ~clear ~n:8 uart_rx in

    (* word_count = number of 64b words already written *)
    let word_count =
      reg_fb spec ~width:14 ~enable:word_in.valid ~f:(fun x -> x +:. 1)
    in

    let loaded     = Variable.reg spec ~width:1 in
    let data_words = Variable.reg spec ~width:14 in

    (* If RTS comes on the same cycle as the final completed word, include it. *)
    let word_count_written = mux2 word_in.valid (word_count +:. 1) word_count in

    compile
      [ when_ uart_rts
          [ loaded     <-- vdd
          ; data_words <-- word_count_written
          ]
      ]
    ;

    { O.
      load_finished = loaded.value
    ; ram_write =
        { address      = word_count
        ; write_data   = word_in.value
        ; write_enable = word_in.valid
        }
    ; data_words    = data_words.value
    ; uart_rx_ready = vdd
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
    | Header0_read
    | Header0_consume
    | Header1_read
    | Header1_consume
    | Run_setup
    | Reset_read
    | Reset_write
    | Node_read
    | Node_consume
    | Edge_read
    | Edge_consume
    | Child_read
    | Child_write
    | Capture_addr
    | Capture_consume
    | Final_compute
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

type header_vars =
  { num_nodes :  Variable.t
  ; you       :  Variable.t
  ; out_      :  Variable.t
  ; svr       :  Variable.t
  ; fft       :  Variable.t
  ; dac       :  Variable.t
  ; has_p2    :  Variable.t
  }

let algo ~clock ~clear ~(read_data : Signal.t array) ~load_finished =
  let spec = Reg_spec.create ~clock ~clear () in

  let sm = State_machine.create (module States) spec in

  let header_words = 2 in
  let node_base    = header_words in

  let write_en   = Variable.wire ~default:gnd () in
  let ram_addr   = Variable.wire ~default:(zero 14) () in
  let write_addr = Variable.wire ~default:(zero 14) () in
  let write_data = Variable.wire ~default:(zero 64) () in

  (* Read from port 0 (addressed by ram_addr in top) *)
  let read_word = read_data.(0) in

  (* ---------- Helpers ---------- *)

  let to64 x = uresize ~width:64 x in

  let mul_lo64 a b =
    let p = a *: b in
    select p ~high:63 ~low:0
  in

  let or_reduce (x : Signal.t) =
    match bits_lsb x with
    | []    -> gnd
    | [ b ] -> b
    | bs    -> List.reduce_exn bs ~f:(|:)
  in

  let mux_arr (sel : Signal.t) (arr : Signal.t array) =
    mux sel (Array.to_list arr)
  in

  (* ---------- Header regs bundled ---------- *)

  let hdr : header_vars =
    { num_nodes = Variable.reg spec ~width:14
    ; you       = Variable.reg spec ~width:14
    ; out_      = Variable.reg spec ~width:14
    ; svr       = Variable.reg spec ~width:14
    ; fft       = Variable.reg spec ~width:14
    ; dac       = Variable.reg spec ~width:14
    ; has_p2    = Variable.reg spec ~width:1
    }
  in

  (* Runs:
     0: you->out
     1: svr->fft
     2: fft->dac
     3: dac->out
     4: svr->dac
     5: dac->fft
     6: fft->out *)
  let run_id     = Variable.reg spec ~width:3  in
  let start_idx  = Variable.reg spec ~width:14 in
  let target_idx = Variable.reg spec ~width:14 in

  let run_start =
    [| hdr.you.value
     ; hdr.svr.value
     ; hdr.fft.value
     ; hdr.dac.value
     ; hdr.svr.value
     ; hdr.dac.value
     ; hdr.fft.value
     ; hdr.you.value
    |]
  in

  let run_target =
    [| hdr.out_.value
     ; hdr.fft.value
     ; hdr.dac.value
     ; hdr.out_.value
     ; hdr.dac.value
     ; hdr.fft.value
     ; hdr.out_.value
     ; hdr.out_.value
    |]
  in

  (* last_run = 0 when no part2, else 6 *)
  let last_run = mux2 hdr.has_p2.value (of_int_trunc ~width:3 6) (zero 3) in

  (* Iterators *)
  let reset_idx = Variable.reg spec ~width:14 in
  let node_idx  = Variable.reg spec ~width:14 in
  let edge_idx  = Variable.reg spec ~width:10 in

  (* Latches *)
  let parent_count    = Variable.reg spec ~width:32 in
  let curr_edge_base  = Variable.reg spec ~width:14 in
  let curr_edge_count = Variable.reg spec ~width:10 in
  let child_idx       = Variable.reg spec ~width:14 in

  (* Captures (paths for each run_id 0..6) *)
  let captures = Array.init 7 ~f:(fun _ -> Variable.reg spec ~width:32) in

  let p_you_out = captures.(0) in
  let p_svr_fft = captures.(1) in
  let p_fft_dac = captures.(2) in
  let p_dac_out = captures.(3) in
  let p_svr_dac = captures.(4) in
  let p_dac_fft = captures.(5) in
  let p_fft_out = captures.(6) in

  let part1 = Variable.reg spec ~width:64 in
  let part2 = Variable.reg spec ~width:64 in

  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  (* Node word layout:
     [63:32] count (u32)
     [31:18] edge_base (u14) absolute RAM addr
     [17:8 ] edge_count (u10)
     [7:0  ] flags (u8) *)
  let node_count w      = select w ~high:63 ~low:32 in
  let node_edge_base w  = select w ~high:31 ~low:18 in
  let node_edge_count w = select w ~high:17 ~low:8 in

  let term_a =
    mul_lo64
      (mul_lo64 (to64 p_svr_fft.value) (to64 p_fft_dac.value))
      (to64 p_dac_out.value)
  in
  let term_b =
    mul_lo64
      (mul_lo64 (to64 p_svr_dac.value) (to64 p_dac_fft.value))
      (to64 p_fft_out.value)
  in
  let part2_calc = term_a +: term_b in

  (* Small local proc to keep Node_consume readable *)
  let do_node_dp () =
    proc
      [ parent_count    <-- node_count read_word
      ; curr_edge_base  <-- node_edge_base read_word
      ; curr_edge_count <-- node_edge_count read_word
      ; if_ ((node_count read_word ==:. 0) |: (node_edge_count read_word ==:. 0))
          [ node_idx <-- node_idx.value +:. 1
          ; sm.set_next Node_read
          ]
          [ edge_idx <--. 0
          ; sm.set_next Edge_read
          ]
      ]
  in

  compile
    [ sm.switch
        [ ( Loading
          , [ when_ load_finished
                [ done_fired <-- gnd
                ; sm.set_next Header0_read
                ]
            ]
          )

        ; ( Header0_read
          , [ ram_addr <--. 0
            ; sm.set_next Header0_consume
            ]
          )

        ; ( Header0_consume
          , [ hdr.you       <-- select read_word ~high:27 ~low:14
            ; hdr.svr       <-- select read_word ~high:55 ~low:42
            ; hdr.out_      <-- select read_word ~high:41 ~low:28
            ; hdr.num_nodes <-- select read_word ~high:13 ~low:0

            ; hdr.has_p2    <-- or_reduce (select read_word ~high:63 ~low:56)
            ; sm.set_next Header1_read
            ]
          )

        ; ( Header1_read
          , [ ram_addr <--. 1
            ; sm.set_next Header1_consume
            ]
          )

        ; ( Header1_consume
          , [ run_id     <--. 0
            ; hdr.fft    <-- select read_word ~high:13 ~low:0
            ; hdr.dac    <-- select read_word ~high:27 ~low:14
            ; hdr.has_p2 <-- (hdr.has_p2.value |: or_reduce (select read_word ~high:63 ~low:28))
            ; sm.set_next Run_setup
            ]
          )

        ; ( Run_setup
          , [ start_idx  <-- mux_arr run_id.value run_start
            ; reset_idx  <--. 0
            ; target_idx <-- mux_arr run_id.value run_target
            ; sm.set_next Reset_read
            ]
          )

        ; ( Reset_read
          , [ if_ (reset_idx.value ==: hdr.num_nodes.value)
                [ (* traverse all nodes; start node is the only one seeded to 1 *)
                  node_idx <--. 0
                ; sm.set_next Node_read
                ]
                [ ram_addr <-- uresize ~width:14 (reset_idx.value +:. node_base)
                ; sm.set_next Reset_write
                ]
            ]
          )

        ; ( Reset_write
          , [ write_addr <-- uresize ~width:14 (reset_idx.value +:. node_base)
            ; write_data <--
                concat_msb
                  [ mux2 (reset_idx.value ==: start_idx.value) (one 32) (zero 32)
                  ; select read_word ~high:31 ~low:0
                  ]
            ; write_en  <-- vdd
            ; reset_idx <-- reset_idx.value +:. 1
            ; sm.set_next Reset_read
            ]
          )

        ; ( Node_read
          , [ if_ (node_idx.value ==: hdr.num_nodes.value)
                [ sm.set_next Capture_addr ]
                [ ram_addr <-- uresize ~width:14 (node_idx.value +:. node_base)
                ; sm.set_next Node_consume
                ]
            ]
          )

        ; ( Node_consume
          , [ do_node_dp () ]
          )

        ; ( Edge_read
          , [ ram_addr <--
                uresize ~width:14
                  (curr_edge_base.value +:
                   uresize ~width:14 edge_idx.value)
            ; sm.set_next Edge_consume
            ]
          )

        ; ( Edge_consume
          , [ child_idx <-- select read_word ~high:13 ~low:0
            ; sm.set_next Child_read
            ]
          )

        ; ( Child_read
          , [ ram_addr <-- uresize ~width:14 (child_idx.value +:. node_base)
            ; sm.set_next Child_write
            ]
          )

        ; ( Child_write
          , [ write_addr <-- uresize ~width:14 (child_idx.value +:. node_base)
            ; write_data <--
                (let sum32 =
                   uresize ~width:32 (node_count read_word +: parent_count.value)
                 in
                 concat_msb
                   [ sum32
                   ; select read_word ~high:31 ~low:0
                   ])
            ; write_en <-- vdd
            ; edge_idx <-- edge_idx.value +:. 1
            ; if_ (edge_idx.value +:. 1 ==: curr_edge_count.value)
                [ node_idx <-- node_idx.value +:. 1
                ; sm.set_next Node_read
                ]
                [ sm.set_next Edge_read ]
            ]
          )

        ; ( Capture_addr
          , [ ram_addr <-- uresize ~width:14 (target_idx.value +:. node_base)
            ; sm.set_next Capture_consume
            ]
          )

        ; ( Capture_consume
          , [ (* Write capture for the current run_id *)
              proc
                (List.init 7 ~f:(fun i ->
                     when_ (run_id.value ==:. i)
                       [ captures.(i) <-- node_count read_word ]))
            ; if_ (run_id.value ==: last_run)
                [ sm.set_next Final_compute ]
                [ run_id <-- run_id.value +:. 1
                ; sm.set_next Run_setup
                ]
            ]
          )

        ; ( Final_compute
          , [ part1 <-- to64 p_you_out.value
            ; part2 <-- mux2 hdr.has_p2.value part2_calc (zero 64)
            ; sm.set_next Done
            ]
          )

        ; ( Done
          , [ when_ (~:(done_fired.value)) [ done_fired <-- vdd ] ]
          )
        ]
    ]
  ;

  ( ram_addr.value
  , write_addr.value
  , write_data.value
  , write_en.value
  , part1.value
  , part2.value
  , done_pulse
  )
;;

(* ====================== TOP ====================== *)

let create scope ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t) =
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

  let a, b, c, d, p1, p2, done_pulse =
    algo ~clock ~clear ~read_data:ram.read_data ~load_finished:loader.load_finished
  in

  Ram.Port.Of_signal.assign ram_ports.(0)
    { address = a; write_data = zero 64; write_enable = gnd }
  ;

  Ram.Port.Of_signal.assign ram_ports.(1)
    { address = b; write_data = c; write_enable = d }
  ;

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock; clear
      ; part1 = { value = uresize ~width:60 p1; valid = done_pulse }
      ; part2 = { value = uresize ~width:60 p2; valid = done_pulse }
      }
  in

  { Ulx3s.O.
    leds = concat_lsb [ ~:clear; uart_rx_overflow; loader.load_finished; done_pulse; zero 4 ]
  ; uart_tx = byte_out
  ; uart_rx_ready = loader.uart_rx_ready
  }
;;

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day11" ~scope create
;;
