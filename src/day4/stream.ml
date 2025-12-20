open! Core
open! Hardcaml
open! Signal
open! Always

(* ============================================================================
   Day 4 — AXI-Stream style wrapper (ready/valid)

   Wraps the existing packed-word core with an AXI-Stream style interface.

   S_AXIS (input):
     - s_axis_tvalid, s_axis_tready, s_axis_tdata[lanes-1:0]
     - s_axis_tuser[0] = SOF (start-of-frame) on first beat
     - s_axis_tlast = 1 on final beat of the frame

   M_AXIS (output):
     - m_axis_tvalid, m_axis_tready, m_axis_tdata[(2*count_bits)-1:0]
     - m_axis_tdata = {p2, p1} (MSB..LSB)
     - m_axis_tuser[0] = 1 (SOF for result packet)
     - m_axis_tlast = 1 (single-beat packet)

   Frame length is fixed by the generated instance: words_total.
   We check SOF and TLAST for realism; on error we set frame_error sticky until
   the next SOF arrives.

   This stays synthesizable and keeps the core untouched.
   ============================================================================ *)

module Make (P : sig
  val lanes : int
  val rows : int
  val cols : int
end) =
struct
  module Core = Engine.Make (P)

  let lanes = Core.lanes
  let rows = Core.rows
  let cols = Core.cols
  let words_total = Core.words_total
  let load_addr_bits = Core.load_addr_bits
  let count_bits = Int.ceil_log2 (rows * cols + 1)
  let out_data_bits = 2 * count_bits

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a

      (* S_AXIS input *)
      ; s_axis_tvalid : 'a
      ; s_axis_tdata : 'a [@bits lanes]
      ; s_axis_tlast : 'a
      ; s_axis_tuser : 'a [@bits 1] (* [0]=SOF *)

      (* M_AXIS output *)
      ; m_axis_tready : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { (* S_AXIS *)
        s_axis_tready : 'a

      ; (* M_AXIS *)
        m_axis_tvalid : 'a
      ; m_axis_tdata : 'a [@bits out_data_bits]
      ; m_axis_tlast : 'a
      ; m_axis_tuser : 'a [@bits 1]

      ; (* status *)
        busy : 'a
      ; frame_error : 'a
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Wait_sof
      | Ingest
      | Start
      | Run
      | Result
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create (scope : Scope.t) (i : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module States) spec ~enable:vdd in

    (* Counters/regs *)
    let load_addr = Variable.reg spec ~width:load_addr_bits in
    let p1_latched = Variable.reg spec ~width:count_bits in
    let p2_latched = Variable.reg spec ~width:count_bits in
    let frame_error = Variable.reg spec ~width:1 in

    let load_addr_s = Variable.value load_addr in
    let frame_error_s = Variable.value frame_error in

    let s_axis_tready_w = Variable.wire ~default:gnd () in
    let m_axis_tvalid_w = Variable.wire ~default:gnd () in
    let busy_w = Variable.wire ~default:gnd () in

    (* Handshake *)
    let s_hs = i.s_axis_tvalid &: Variable.value s_axis_tready_w in
    let sof = select i.s_axis_tuser ~high:0 ~low:0 in

    let last_addr = of_int_trunc ~width:load_addr_bits (words_total - 1) in
    let is_last_word = load_addr_s ==: last_addr in
    let ingest_done_hs = s_hs &: is_last_word in

    (* Validate TLAST on final beat *)
    let last_ok = i.s_axis_tlast in

    (* Core interface driven internally *)
    let core_load_we = (sm.is Ingest) &: s_hs in
    let core_load_addr = load_addr_s in
    let core_load_word = i.s_axis_tdata in
    let core_start = sm.is Start in
    let core_finish = vdd in

    let core_i : _ Core.I.t =
      { clock = i.clock
      ; clear = i.clear
      ; start = core_start
      ; finish = core_finish
      ; load_we = core_load_we
      ; load_addr = core_load_addr
      ; load_word = core_load_word
      }
    in
    let core_o = Core.hierarchical (Scope.sub_scope scope "core") core_i in

    (* Ready/valid outputs *)
    let s_axis_ready =
      (sm.is Wait_sof |: sm.is Ingest) &: core_o.ready
    in
    let () = Signal.assign (Variable.value s_axis_tready_w) s_axis_ready in

    let () = Signal.assign (Variable.value m_axis_tvalid_w) (sm.is Result) in
    let () = Signal.assign (Variable.value busy_w) (~:(sm.is Wait_sof)) in

    (* Output payload: {p2, p1} *)
    let out_data = concat_msb [ Variable.value p2_latched; Variable.value p1_latched ] in

    Always.compile
      [ sm.switch
          [ ( Wait_sof
            , [ (* Clear addr; hold ready high (combinational) *)
                load_addr <-- zero load_addr_bits

              ; (* If a beat arrives without SOF, flag error but ignore it. *)
                when_ s_hs
                  [ when_ (~:sof) [ frame_error <-- vdd ]
                  ; when_ sof
                      [ (* new frame starts: clear error and start ingest *)
                        frame_error <-- gnd
                      ; (* treat this first beat as word0 *)
                        load_addr <-- (load_addr_s +:. 1)
                      ; sm.set_next Ingest
                      ]
                  ]
              ] )
          ; ( Ingest
            , [ (* accept words and increment address *)
                when_ s_hs
                  [ load_addr <-- (load_addr_s +:. 1) ]

              ; (* If SOF is asserted mid-frame, that's a protocol error. *)
                when_ (s_hs &: sof) [ frame_error <-- vdd ]

              ; (* When final word arrives, check TLAST and move on *)
                when_ ingest_done_hs
                  [ when_ (~:last_ok) [ frame_error <-- vdd ]
                  ; load_addr <-- zero load_addr_bits
                  ; sm.set_next Start
                  ]
              ] )
          ; ( Start
            , [ (* one-cycle start pulse *)
                sm.set_next Run
              ] )
          ; ( Run
            , [ when_ core_o.finished
                  [ p1_latched <-- core_o.p1
                  ; p2_latched <-- core_o.p2
                  ; sm.set_next Result
                  ]
              ] )
          ; ( Result
            , [ (* wait for M_AXIS accept *)
                when_ i.m_axis_tready
                  [ sm.set_next Wait_sof ]
              ] )
          ]
      ]
    ;

    { O.s_axis_tready = Variable.value s_axis_tready_w
    ; m_axis_tvalid = Variable.value m_axis_tvalid_w
    ; m_axis_tdata = out_data
    ; m_axis_tlast = vdd (* single-beat result *)
    ; m_axis_tuser = vdd (* result SOF *)
    ; busy = Variable.value busy_w
    ; frame_error = frame_error_s
    }
  ;;

  let hierarchical = create
end
