(* src/day06.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq       = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth  = 32
let extra_synth_args = []

(* ====================== RAM ====================== *)

module Ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width           = 32
  let depth           = 8192
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
      ; data_words    : 'a [@bits 13]
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create _scope ({ clock; clear; uart_rx; uart_rts } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~clear () in

    (* Pack 4 bytes -> 1x 32-bit word *)
    let word_in = Util.shift_in ~clock ~clear ~n:4 uart_rx in

    (* Count 32-bit words written *)
    let word_count =
      reg_fb spec ~width:13 ~enable:word_in.valid ~f:(fun x -> x +:. 1)
    in

    (* Sticky end-of-load *)
    let loaded     = Variable.reg spec ~width:1 in
    let data_words = Variable.reg spec ~width:13 in

    (* If RTS happens in the same cycle as the last word write, include it. *)
    let word_count_written =
      mux2 word_in.valid (word_count +:. 1) word_count
    in

    compile
      [ when_ uart_rts
          [ loaded     <-- vdd
          ; data_words <-- word_count_written
          ]
      ];

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
    | Mode_read
    | Mode_consume
    | Op_read
    | Op_consume
    | Count_read
    | Count_consume
    | Value_lo_read
    | Value_lo_consume
    | Value_hi_read
    | Value_hi_consume
    | Fold_value
    | Finish_problem
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo ~clock ~clear ~read_word ~data_words:_ ~load_finished =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm   = State_machine.create (module States) spec in

  let rd_word = Variable.reg spec ~width:13 in
  let rd_byte = Variable.reg spec ~width:2 in

  let op_u8      = Variable.reg spec ~width:8 in
  let remaining  = Variable.reg spec ~width:8 in
  let value_lo   = Variable.reg spec ~width:8 in
  let value_u16  = Variable.reg spec ~width:16 in

  let problem_acc = Variable.reg spec ~width:64 in
  let global_sum  = Variable.reg spec ~width:64 in

  let part1_res = Variable.reg spec ~width:64 in
  let part2_res = Variable.reg spec ~width:64 in
  let phase     = Variable.reg spec ~width:1 in  (* 0 = Part1 stream, 1 = Part2 stream *)

  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  let byte0 = select read_word ~high:7  ~low:0 in
  let byte1 = select read_word ~high:15 ~low:8 in
  let byte2 = select read_word ~high:23 ~low:16 in
  let byte3 = select read_word ~high:31 ~low:24 in

  let sel_byte = mux rd_byte.value [ byte0; byte1; byte2; byte3 ] in

  let advance =
    let nb = rd_byte.value +:. 1 in
    let wrap = nb ==:. 0 in
    [ rd_byte <-- nb
    ; rd_word <-- mux2 wrap (rd_word.value +:. 1) rd_word.value
    ]
  in

  let init_problem =
    let is_mul = op_u8.value ==:. 1 in
    [ problem_acc <--
        mux2 is_mul
          (of_int_trunc ~width:64 1)
          (of_int_trunc ~width:64 0)
    ]
  in

  let fold_value =
    let v64 = uresize ~width:64 value_u16.value in
    let is_mul = op_u8.value ==:. 1 in
    [ problem_acc <--
        mux2 is_mul
          (uresize ~width:64 (problem_acc.value *: v64))
          (problem_acc.value +: v64)
    ]
  in

  compile
    [ sm.switch
        [ ( Loading
          , [ when_ load_finished
                [ rd_word    <--. 0
                ; rd_byte    <--. 0
                ; global_sum <--. 0
                ; part1_res  <--. 0
                ; part2_res  <--. 0
                ; phase      <-- gnd
                ; done_fired <-- gnd
                ; sm.set_next Mode_read
                ]
            ]
          )

        ; ( Mode_read
          , [ (* wait 1 cycle for RAM read to settle *)
              sm.set_next Mode_consume
            ]
          )

        ; ( Mode_consume
          , [ (* discard "mode" byte in stream *)
              sm.set_next Op_read
            ] @ advance
          )

        ; ( Op_read
          , [ sm.set_next Op_consume ]
          )

        ; ( Op_consume
          , [ op_u8 <-- sel_byte
            ; sm.set_next Count_read
            ] @ advance
          )

        ; ( Count_read
          , [ sm.set_next Count_consume ]
          )

        ; ( Count_consume
          , [ remaining <-- sel_byte ]
            @
            [ if_ (sel_byte ==:. 0)
                [ (* delimiter between Part1/Part2 streams, or end delimiter *)
                  if_ (phase.value ==:. 0)
                    [ part1_res  <-- global_sum.value
                    ; global_sum <--. 0
                    ; phase      <-- vdd
                    ; sm.set_next Mode_read
                    ]
                    [ part2_res  <-- global_sum.value
                    ; sm.set_next Done
                    ]
                ]
                ( init_problem @ [ sm.set_next Value_lo_read ] )
            ]
            @ advance
          )

        ; ( Value_lo_read
          , [ sm.set_next Value_lo_consume ]
          )

        ; ( Value_lo_consume
          , [ value_lo <-- sel_byte
            ; sm.set_next Value_hi_read
            ] @ advance
          )

        ; ( Value_hi_read
          , [ sm.set_next Value_hi_consume ]
          )

        ; ( Value_hi_consume
          , [ (* LE: value = lo | (hi<<8) *)
              value_u16 <-- concat_msb [ sel_byte; value_lo.value ]
            ; sm.set_next Fold_value
            ] @ advance
          )

        ; ( Fold_value
          , [ remaining <-- remaining.value -:. 1
            ; if_ (remaining.value ==:. 1)
                [ sm.set_next Finish_problem ]
                [ sm.set_next Value_lo_read ]
            ] @ fold_value
          )

        ; ( Finish_problem
          , [ global_sum <-- global_sum.value +: problem_acc.value
            ; sm.set_next Op_read
            ]
          )

        ; ( Done
          , [ when_ (done_fired.value ==:. 0) [ done_fired <-- vdd ] ]
          )
        ]
    ];

  rd_word.value, part1_res.value, part2_res.value, done_pulse
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

  let addr, p1, p2, done_pulse =
    algo
      ~clock
      ~clear
      ~read_word:ram.read_data.(0)
      ~data_words:loader.data_words
      ~load_finished:loader.load_finished
  in

  Ram.Port.Of_signal.assign ram_ports.(0)
    { address      = mux2 loader.load_finished addr (zero 13)
    ; write_data   = zero 32
    ; write_enable = gnd
    };

  Ram.Port.Of_signal.assign ram_ports.(1)
    { address      = zero 13
    ; write_data   = zero 32
    ; write_enable = gnd
    };

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 p1; valid = done_pulse }
      ; part2 = { value = uresize ~width:60 p2; valid = done_pulse }
      }
  in

  { Ulx3s.O.
    leds = concat_lsb [ ~:clear; uart_rx_overflow; loader.load_finished; zero 5 ]
  ; uart_tx = byte_out
  ; uart_rx_ready = loader.uart_rx_ready
  }
;;

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day06" ~scope create
;;
