(* src/uart.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

module Byte_with_valid = With_valid.Vector (struct
    let width = 8
  end)

module Make (Config : sig
    val baud : int
    val clock_freq_hz : int
    val rx_fifo_depth : int
    val tx_fifo_depth : int
  end) =
struct
  open Config

  let clocks_per_baud = clock_freq_hz / baud
  let clocks_per_half_baud = clocks_per_baud / 2

  (* ================= RX ================= *)

  module Rx = struct
    module States = struct
      type t =
        | Idle | Start
        | Data0 | Data1 | Data2 | Data3
        | Data4 | Data5 | Data6 | Data7
        | Stop
      [@@deriving sexp_of, enumerate]

      let rank = function
        | Idle -> 0 | Start -> 1
        | Data0 -> 2 | Data1 -> 3 | Data2 -> 4 | Data3 -> 5
        | Data4 -> 6 | Data5 -> 7 | Data6 -> 8 | Data7 -> 9
        | Stop -> 10

      let compare a b = Int.compare (rank a) (rank b)
      let compare__local = compare

      let next = function
        | Idle -> Start
        | Start -> Data0
        | Data0 -> Data1 | Data1 -> Data2 | Data2 -> Data3
        | Data3 -> Data4 | Data4 -> Data5 | Data5 -> Data6
        | Data6 -> Data7 | Data7 -> Stop
        | Stop -> Idle
    end

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; ready : 'a
        ; rx : 'a
        }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    module O = struct
      type 'a t =
        { byte_out : 'a Byte_with_valid.t
        ; fifo_overflow : 'a
        }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    let create scope ({ clock; clear; ready; rx } : _ I.t) : _ O.t =
      let spec = Reg_spec.create ~clock ~clear () in
      let sm = State_machine.create (module States) spec in

      let after_n ~n body =
        let ctr = Variable.reg ~width:(num_bits_to_represent n) spec in
        proc
          [ ctr <-- ctr.value +:. 1
          ; when_ (ctr.value ==:. n - 1)
              [ ctr <--. 0; proc body ]
          ]
      in

      let byte_rx = Variable.reg ~width:8 spec in
      let fifo_wr = Variable.wire ~default:gnd () in

      compile
        [ sm.switch
            [ States.Idle,
              [ when_ ~:rx [ sm.set_next States.Start ] ]

            ; States.Start,
              [ after_n ~n:clocks_per_half_baud
                  [ if_ ~:rx
                      [ sm.set_next States.Data0 ]
                      @@ else_
                      [ sm.set_next States.Idle ] ] ]

            ; States.Data0,
              [ after_n ~n:clocks_per_baud
                  [ byte_rx <-- rx @: msbs byte_rx.value
                  ; sm.set_next States.Data1 ] ]

            ; States.Data1,
              [ after_n ~n:clocks_per_baud
                  [ byte_rx <-- rx @: msbs byte_rx.value
                  ; sm.set_next States.Data2 ] ]

            ; States.Data2,
              [ after_n ~n:clocks_per_baud
                  [ byte_rx <-- rx @: msbs byte_rx.value
                  ; sm.set_next States.Data3 ] ]

            ; States.Data3,
              [ after_n ~n:clocks_per_baud
                  [ byte_rx <-- rx @: msbs byte_rx.value
                  ; sm.set_next States.Data4 ] ]

            ; States.Data4,
              [ after_n ~n:clocks_per_baud
                  [ byte_rx <-- rx @: msbs byte_rx.value
                  ; sm.set_next States.Data5 ] ]

            ; States.Data5,
              [ after_n ~n:clocks_per_baud
                  [ byte_rx <-- rx @: msbs byte_rx.value
                  ; sm.set_next States.Data6 ] ]

            ; States.Data6,
              [ after_n ~n:clocks_per_baud
                  [ byte_rx <-- rx @: msbs byte_rx.value
                  ; sm.set_next States.Data7 ] ]

            ; States.Data7,
              [ after_n ~n:clocks_per_baud
                  [ byte_rx <-- rx @: msbs byte_rx.value
                  ; sm.set_next States.Stop ] ]

            ; States.Stop,
              [ after_n ~n:clocks_per_baud
                  [ when_ rx [ fifo_wr <-- vdd ]
                  ; sm.set_next States.Idle ] ]
            ]
        ];

      let%tydi { q; full; empty; _ } =
        Fifo.create
          ~showahead:true
          ~scope:(Scope.sub_scope scope "fifo")
          ~capacity:rx_fifo_depth
          ~clock
          ~clear
          ~wr:fifo_wr.value
          ~d:byte_rx.value
          ~rd:(~:empty &: ready)
          ()
      in

      { byte_out = { value = q; valid = ~:empty }
      ; fifo_overflow = reg_fb spec ~width:1 ~f:(fun x -> x |: full)
      }
    ;;
  end

  (* ================= TX ================= *)

  module Tx = struct
    module States = Rx.States

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; byte_in : 'a Byte_with_valid.t
        }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    module O = struct
      type 'a t =
        { ready : 'a
        ; tx : 'a
        }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    let create scope ({ clock; clear; byte_in } : _ I.t) : _ O.t =
      let spec = Reg_spec.create ~clock ~clear () in
      let sm = State_machine.create (module States) spec in

      let fifo_rd = Variable.wire ~default:gnd () in
      let byte_tx = Variable.reg ~width:8 spec in
      let tx = Variable.wire ~default:vdd () in

      let%tydi { q; full; empty; _ } =
        Fifo.create
          ~showahead:true
          ~scope:(Scope.sub_scope scope "fifo")
          ~capacity:tx_fifo_depth
          ~clock
          ~clear
          ~wr:byte_in.valid
          ~d:byte_in.value
          ~rd:fifo_rd.value
          ()
      in

      let after_n ~n body =
        let ctr = Variable.reg ~width:(num_bits_to_represent n) spec in
        proc
          [ ctr <-- ctr.value +:. 1
          ; when_ (ctr.value ==:. n - 1)
              [ ctr <--. 0; proc body ]
          ]
      in

      let data_state_actions (next_state : States.t) =
        [ tx <-- lsb byte_tx.value
        ; after_n ~n:clocks_per_baud
            [ byte_tx <-- (gnd @: msbs byte_tx.value)
            ; sm.set_next next_state
            ]
        ]
      in

      compile
        [ (* default assignments every cycle *)
          fifo_rd <-- gnd

        ; sm.switch ~default:[ sm.set_next States.Idle ]
            [ States.Idle,
              [ tx <-- vdd
              ; when_ ~:empty
                  [ fifo_rd <-- vdd
                  ; byte_tx <-- q
                  ; sm.set_next States.Start
                  ]
              ]

            ; States.Start,
              [ tx <-- gnd
              ; after_n ~n:clocks_per_baud [ sm.set_next States.Data0 ]
              ]

            ; States.Data0, data_state_actions States.Data1
            ; States.Data1, data_state_actions States.Data2
            ; States.Data2, data_state_actions States.Data3
            ; States.Data3, data_state_actions States.Data4
            ; States.Data4, data_state_actions States.Data5
            ; States.Data5, data_state_actions States.Data6
            ; States.Data6, data_state_actions States.Data7
            ; States.Data7, data_state_actions States.Stop

            ; States.Stop,
              [ tx <-- vdd
              ; after_n ~n:clocks_per_baud [ sm.set_next States.Idle ]
              ]
            ]
        ];

      { ready = ~:full; tx = tx.value }
    ;;
  end
end
