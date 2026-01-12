(* src/day10.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
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
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create _ ({ clock; clear; uart_rx; uart_rts } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~clear () in

    (* Pack 8 bytes → 1x 64-bit word *)
    let word_in = Util.shift_in ~clock ~clear ~n:8 uart_rx in

    (* Counts how many full 64b words have been received *)
    let word_count = Variable.reg spec ~width:14 in
    let loaded     = Variable.reg spec ~width:1 in

    compile
      [ when_ word_in.valid
          [ (* increment AFTER writing this word *)
            word_count <-- (word_count.value +:. 1)
          ]
      ; when_ uart_rts
          [ loaded <-- vdd ]
      ]
    ;

    { O.
      load_finished = loaded.value
    ; ram_write =
        { (* CRITICAL FIX: write to word_count - 1 *)
          address      = word_count.value -:. 1
        ; write_data   = word_in.value
        ; write_enable = word_in.valid
        }
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
    | Read_machine_count
    | Consume_machine_count
    | Read_header
    | Consume_header
    | Read_masks
    | Consume_mask
    | Subset_init
    | Subset_step
    | Subset_done
    | Next_machine
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

(* ====================== Helpers ====================== *)

let popcount_upto_16 (x : Signal.t) : Signal.t =
  List.init 16 ~f:(fun i -> uresize ~width:6 (bit x ~pos:i))
  |> List.reduce_exn ~f:( +: )
;;

let mask_low_m_bits ~(m8 : Signal.t) : Signal.t =
  (* bit i = 1 if i < m *)
  List.init 64 ~f:(fun i ->
    let ii = of_int_trunc ~width:8 i in
    ii <: m8)
  |> concat_msb
;;

let pow2_upto_16 (k8 : Signal.t) : Signal.t =
  (* 2^k for k in [0..16], width 17 *)
  List.init 17 ~f:(fun i ->
    mux2 (k8 ==: of_int_trunc ~width:8 i)
      (of_int_trunc ~width:17 (1 lsl i))
      (zero 17))
  |> List.reduce_exn ~f:( +: )
;;

(* ====================== ALGORITHM ====================== *)

let algo ~clock ~clear ~(read_data : Signal.t array) ~load_finished =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  (* Sync RAM read: set addr in Read_* state, consume next state *)
  let addr0 = Variable.reg spec ~width:14 in
  let addr1 = zero 14 in

  let total_p1 = Variable.reg spec ~width:64 in

  let machine_count = Variable.reg spec ~width:16 in
  let machine_idx   = Variable.reg spec ~width:16 in
  let ptr           = Variable.reg spec ~width:14 in

  let m_lights = Variable.reg spec ~width:8 in
  let k_btns   = Variable.reg spec ~width:8 in
  let target   = Variable.reg spec ~width:64 in
  let tgt_mask = Variable.reg spec ~width:64 in

  let masks = Array.init 16 ~f:(fun _ -> Variable.reg spec ~width:64) in
  let mask_load_idx = Variable.reg spec ~width:5 in

  let subset     = Variable.reg spec ~width:17 in
  let best       = Variable.reg spec ~width:6 in
  let limit      = Variable.reg spec ~width:17 in
  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  let r0 = read_data.(0) in
  ignore read_data.(1);

  let last_machine =
    machine_idx.value ==: (machine_count.value -:. 1)
  in

  (* Clip to 16 (hardware searches up to 16 buttons) *)
  let k_clip =
    mux2 (k_btns.value >:. 16)
      (of_int_trunc ~width:8 16)
      k_btns.value
  in

  let limit_val = pow2_upto_16 k_clip in

  (* XOR of selected masks *)
  let subset_lo16 = uresize ~width:16 subset.value in
  let state_xor =
    Array.foldi masks ~init:(zero 64) ~f:(fun i acc v ->
      let use_i = bit subset_lo16 ~pos:i in
      acc ^: mux2 use_i v.value (zero 64))
  in

  (* Check equality only on the m_lights low bits *)
  let diff = (state_xor ^: target.value) &: tgt_mask.value in
  let state_ok = diff ==: zero 64 in

  let w      = popcount_upto_16 subset_lo16 in
  let better = state_ok &: (w <: best.value) in

  compile
    [ sm.switch
        [ Loading,
          [ when_ load_finished
              [ total_p1    <--. 0
              ; machine_idx <--. 0
              ; ptr         <--. 1 (* word0 = machine_count, word1 = first header *)
              ; done_fired  <-- gnd
              ; sm.set_next Read_machine_count
              ]
          ]

        ; Read_machine_count,
          [ addr0 <--. 0
          ; sm.set_next Consume_machine_count
          ]

        ; Consume_machine_count,
          [ machine_count <-- select r0 ~high:15 ~low:0
          ; sm.set_next Read_header
          ]

        ; Read_header,
          [ addr0 <-- ptr.value
          ; sm.set_next Consume_header
          ]

        ; Consume_header,
          [ m_lights <-- select r0 ~high:7 ~low:0
          ; k_btns   <-- select r0 ~high:15 ~low:8
          ; target   <-- uresize ~width:64 (srl r0 ~by:16)
          ; tgt_mask <-- mask_low_m_bits ~m8:(select r0 ~high:7 ~low:0)
          ; mask_load_idx <--. 0
          ; sm.set_next Read_masks
          ]

        ; Read_masks,
          [ if_ (k_btns.value ==:. 0)
              [ sm.set_next Subset_init ]
              [ addr0 <-- (ptr.value +:. 1 +: uresize ~width:14 mask_load_idx.value)
              ; sm.set_next Consume_mask
              ]
          ]

        ; Consume_mask,
          [ (* store only first 16 masks; still read through k_btns *)
            when_ (mask_load_idx.value <: of_int_trunc ~width:5 16)
              (Array.to_list
                 (Array.mapi masks ~f:(fun i r ->
                    r <-- mux2 (mask_load_idx.value ==: of_int_trunc ~width:5 i)
                      (r0 &: tgt_mask.value)
                      r.value)))
          ; if_ (mask_load_idx.value +:. 1 ==: uresize ~width:5 k_btns.value)
              [ sm.set_next Subset_init ]
              [ mask_load_idx <-- (mask_load_idx.value +:. 1)
              ; sm.set_next Read_masks
              ]
          ]

        ; Subset_init,
          [ subset <--. 0
          ; limit  <-- limit_val
          ; best   <-- uresize ~width:6 k_clip
          ; sm.set_next Subset_step
          ]

        ; Subset_step,
          [ when_ better [ best <-- w ]
          ; if_ (subset.value ==: (limit.value -:. 1))
              [ sm.set_next Subset_done ]
              [ subset <-- (subset.value +:. 1)
              ; sm.set_next Subset_step
              ]
          ]

        ; Subset_done,
          [ total_p1 <-- (total_p1.value +: uresize ~width:64 best.value)
          ; ptr      <-- (ptr.value +:. 1 +: uresize ~width:14 k_btns.value)
          ; sm.set_next Next_machine
          ]

        ; Next_machine,
          [ if_ last_machine
              [ sm.set_next Done ]
              [ machine_idx <-- (machine_idx.value +:. 1)
              ; sm.set_next Read_header
              ]
          ]

        ; Done,
          [ when_ (done_fired.value ==:. 0) [ done_fired <-- vdd ] ]
        ]
    ]
  ;

  addr0.value, addr1, total_p1.value, done_pulse
;;

(* ====================== TOP ====================== *)

let create scope ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t) =
  let loader = Loader.hierarchical scope { clock; clear; uart_rx; uart_rts } in
  let ram_ports = Array.init 2 ~f:(fun _ -> Ram.Port.Of_signal.wires ()) in

  let%tydi ram =
    Ram.hierarchical ~name:"ram" scope
      { clock
      ; clear
      ; load_ports    = [| loader.ram_write; Ram.Port.unused |]
      ; load_finished = loader.load_finished
      ; ram_ports
      }
  in

  let addr0, addr1, p1, done_pulse =
    algo ~clock ~clear ~read_data:ram.read_data ~load_finished:loader.load_finished
  in

  Ram.Port.Of_signal.assign ram_ports.(0)
    { address = addr0; write_data = zero 64; write_enable = gnd }
  ;

  Ram.Port.Of_signal.assign ram_ports.(1)
    { address = addr1; write_data = zero 64; write_enable = gnd }
  ;

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock; clear
      ; part1 = { value = uresize ~width:60 p1; valid = done_pulse }
      ; part2 = { value = zero 60; valid = done_pulse }
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
  S.hierarchical ~name:"day10" ~scope create
;;
