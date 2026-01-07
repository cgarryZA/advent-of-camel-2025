(* src/day05.ml *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 64
let extra_synth_args = []

(* ====================== RAM ====================== *)

module Ram = Loadable_pseudo_dual_port_ram.Make (struct
  let width = 64
  let depth = 16384
  let zero_on_startup = false
  let num_ports = 2
end)

(* ====================== LOADER ====================== *)
(* Input format (words):
   w0 = range_count (u64)
   w1 = item_count  (u64)
   then payload written to RAM at address 0:
     ranges: 2*range_count words (lo, hi) pairs
     items : item_count words    (one u64 each)
   RTS ends load.
*)

module Loader = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a Uart.Byte_with_valid.t
      ; uart_rts : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { load_finished : 'a
      ; ram_write : 'a Ram.Port.t
      ; data_length : 'a [@bits 13]  (* range_count *)
      ; item_count : 'a [@bits 13]
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create _ ({ clock; clear; uart_rx; uart_rts } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~clear () in

    (* pack 4 bytes -> 1x 64-bit word *)
    let word_in = Util.shift_in ~clock ~clear ~n:8 uart_rx in

    (* counts received 64-bit words including header *)
    let word_count =
      reg_fb spec ~width:14 ~enable:word_in.valid ~f:(fun x -> x +:. 1)
    in

    let loaded = Variable.reg spec ~width:1 in
    let range_count = Variable.reg spec ~width:13 in
    let item_count = Variable.reg spec ~width:13 in

    compile
      [ (* capture header words *)
        when_ (word_in.valid &: (word_count ==:. 0))
          [ range_count <-- select word_in.value ~high:12 ~low:0 ]
      ; when_ (word_in.valid &: (word_count ==:. 1))
          [ item_count <-- select word_in.value ~high:12 ~low:0 ]
      ; when_ uart_rts [ loaded <-- vdd ]
      ]
    ;

    (* write payload only (skip header words 0 and 1) *)
    let ge2 = word_count >=: of_int_trunc ~width:14 2 in
    let wr_en = word_in.valid &: ge2 in
    let wr_addr = uresize ~width:14 (word_count -:. 2) in

    O.
      { load_finished = loaded.value
      ; ram_write =
          { address = wr_addr
          ; write_data = word_in.value
          ; write_enable = wr_en
          }
      ; data_length = range_count.value
      ; item_count = item_count.value
      ; uart_rx_ready = vdd
      }
  ;;

  let hierarchical scope =
    let module S = Hierarchy.In_scope (I) (O) in
    S.hierarchical ~name:"loader" ~scope create
  ;;
end

(* ====================== ALGORITHM ====================== *)

module States = struct
  type t =
    | Loading
    | Merge_read
    | Merge_consume
    | Flush_lo
    | Flush_hi
    | Item_read
    | Item_consume
    | Range_read
    | Range_consume
    | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo
    ~clock
    ~clear
    ~(read_data : Signal.t array)
    ~load_finished
    ~(range_count : Signal.t)
    ~(item_count : Signal.t)
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm = State_machine.create (module States) spec in

  (* ---- merge control ---- *)
  let rd_idx = Variable.reg spec ~width:13 in
  let curr_lo = Variable.reg spec ~width:64 in
  let curr_hi = Variable.reg spec ~width:64 in
  let merge_active = Variable.reg spec ~width:1 in

  let pending_lo = Variable.reg spec ~width:64 in
  let pending_hi = Variable.reg spec ~width:64 in
  let pending_valid = Variable.reg spec ~width:1 in

  (* capture-to-print regs *)
  let part1_val = Variable.reg spec ~width:64 in
  let part2_val = Variable.reg spec ~width:64 in

  (* merged-range count (how many merged ranges have been flushed so far) *)
  let merge_counter = Variable.reg spec ~width:13 in

  (* one-shot done pulse *)
  let done_fired = Variable.reg spec ~width:1 in
  let done_pulse = sm.is Done &: ~:(done_fired.value) in

  (* address helpers: payload ranges are stored at base 0: (i<<1)+{0,1} *)
  let addr_range_lo = uresize ~width:14 (rd_idx.value @: gnd) in
  let addr_range_hi = uresize ~width:14 (rd_idx.value @: vdd) in

  (* merged_base = (range_count<<1) + item_count  (ranges then items in RAM) *)
  let range_words = uresize ~width:14 (range_count @: gnd) in
  let item_words = uresize ~width:14 item_count in
  let merged_base = uresize ~width:14 (range_words +: item_words) in

  let merged_addr_lo =
    uresize ~width:14 (merged_base +: uresize ~width:14 (merge_counter.value @: gnd))
  in
  let merged_addr_hi =
    uresize ~width:14 (merged_base +: uresize ~width:14 (merge_counter.value @: vdd))
  in

  (* RAM outputs (valid in the cycle after the address is presented) *)
  let new_lo = read_data.(0) in
  let new_hi = read_data.(1) in

  (* merge predicates *)
  let overlap = new_lo <=: (curr_hi.value +:. 1) in
  let merged_hi = mux2 (curr_hi.value >=: new_hi) curr_hi.value new_hi in
  
  let range_idx = Variable.reg spec ~width:13 in

  let merged_rd_addr_lo =
    merged_base +: uresize ~width:14 (range_idx.value @: gnd)
  in
  let merged_rd_addr_hi =
    merged_base +: uresize ~width:14 (range_idx.value @: gnd) +:. 1
  in
  (* ---- item control ---- *)
  let item_idx = Variable.reg spec ~width:13 in
  let curr_item = Variable.reg spec ~width:64 in

  let item_base = uresize ~width:14 (range_words) in

  let item_addr =
    item_base +: uresize ~width:14 item_idx.value
  in

  let new_item = read_data.(0) in

  (* ---- query predicates (combinational) ---- *)
  let item_before_range =
    curr_item.value <: new_lo
  in

  let item_in_range =
    (curr_item.value >=: new_lo) &: 
    (curr_item.value <=: new_hi)
  in

  let last_merged_range =
    range_idx.value ==: (merge_counter.value -:. 1)
  in

  let last_item =
    item_idx.value ==: (item_count -:. 1)
  in

  let advance_item_or_done =
    if_ last_item
      [ sm.set_next Done ]
      [ item_idx <-- (item_idx.value +:. 1)

      ; sm.set_next Item_read
      ]
  in

  compile
    [ sm.switch
        [ ( Loading
          , [ when_ 
                (load_finished ==:. 1)
                  [ rd_idx        <--. 0
                  ; item_idx      <--. 0
                  ; range_idx     <--. 0
                  ; done_fired    <-- gnd
                  ; merge_active  <-- gnd
                  ; pending_valid <-- gnd
                  ; merge_counter <--. 0

                  ; sm.set_next Merge_read
                  ]
            ]
          )
        ; ( Merge_read
          , [ (* present addresses for rd_idx; data valid next cycle *)
              sm.set_next Merge_consume
            ]
          )

        ; ( Merge_consume
          , [ sm.set_next Merge_read
            ; if_ 
                (merge_active.value ==:. 0)
                  [ curr_lo <-- new_lo
                  ; curr_hi <-- new_hi
                  ; merge_active <-- vdd
                  ]
                  [ if_ 
                    (overlap)
                      [ curr_hi <-- merged_hi ]
                      [ (* non-overlap: stash next range and flush curr *)
                        pending_lo <-- new_lo
                      ; pending_hi <-- new_hi
                      ; pending_valid <-- vdd
                      ; sm.set_next Flush_lo
                      ]
                  ]
            ; (* advance rd_idx past the range we just consumed *)
              rd_idx <-- (rd_idx.value +:. 1)

            ; (* if we just consumed the last input range, flush final curr *)
              when_ 
                (rd_idx.value ==: (range_count -:. 1))
                  [ sm.set_next Flush_lo ]
            ]
          )

        ; ( Flush_lo
          , [ (* write curr_lo this cycle via combinational write port *)
              sm.set_next Flush_hi
            ]
          )
        ; ( Flush_hi
          , [ (* write curr_hi this cycle via combinational write port *)
            merge_counter <-- (merge_counter.value +:. 1)
          ; part2_val     <-- (part2_val.value +: (curr_hi.value -: curr_lo.value +:. 1))

          ; if_ 
              (pending_valid.value ==:. 1)
                [ curr_lo       <-- pending_lo.value
                ; curr_hi       <-- pending_hi.value
                ; pending_valid <-- gnd

                ; if_ (rd_idx.value <: range_count)
                    [ sm.set_next Merge_read ]
                    [ sm.set_next Flush_lo   ]
                ]
                [ (* no pending *)
                  if_ (rd_idx.value <: range_count)
                    [ sm.set_next Merge_read ]
                    [ sm.set_next Item_read  ]
                ]
            ]
          )
(*----------------------------------Test Items in merged Ranges------------------------------------------------*)
        ; ( Item_read
          , [ sm.set_next Item_consume
            ; range_idx <--. 0
            ]
          )

        ; ( Item_consume
          , [ curr_item <-- new_item
            ; range_idx <--. 0
            ; sm.set_next Range_read
            ]
          )

        ; ( Range_read
          , [ sm.set_next Range_consume ]
          )

        ; ( Range_consume
          , [
              (* Default: continue scanning ranges *)
              sm.set_next Range_read

            ; if_ item_in_range
                [ part1_val <-- (part1_val.value +:. 1)
                ; advance_item_or_done
                ]
                [ if_ item_before_range
                    [ (* Item < lo ⇒ cannot appear in any later merged range *)
                      advance_item_or_done
                    ]
                    [ (* Item > hi *)
                      if_ last_merged_range
                        [ advance_item_or_done ]
                        [ range_idx <-- (range_idx.value +:. 1) ]
                    ]
                ]
            ]
          )

        ; ( Done
          , [ when_ (done_fired.value ==:. 0) [ done_fired <-- vdd ] ]
          )
        ]
    ]
  ;

  (* combinational write port (no registered delay!) *)
  let wr_en = sm.is Flush_lo |: sm.is Flush_hi in
  let wr_addr = mux2 (sm.is Flush_lo) merged_addr_lo merged_addr_hi in
  let wr_data = mux2 (sm.is Flush_lo) curr_lo.value curr_hi.value in

  (* RAM read ports:
     - port0 reads low word of current input range during merge
     - port1 reads high word of current input range during merge *)

  (* Which phase is driving the RAM *)
  let querying = sm.is Range_read |: sm.is Range_consume in
  let iteming  = sm.is Item_read |: sm.is Item_consume in

  let port0_addr =
    mux2 querying
      merged_rd_addr_lo
      (mux2 iteming item_addr addr_range_lo)
  in

  let port1_addr =
    mux2 querying
      merged_rd_addr_hi
      addr_range_hi
  in


  let ram_write_back : Signal.t Ram.Port.t =
    { address = wr_addr
    ; write_data = wr_data
    ; write_enable = wr_en
    }
  in

  port0_addr, port1_addr, ram_write_back, part1_val.value, part2_val.value, done_pulse
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

  let addr0, addr1, ram_wb, part1_value, part2_value, done_pulse =
    algo
      ~clock ~clear
      ~read_data:ram.read_data
      ~load_finished:loader.load_finished
      ~range_count:loader.data_length
      ~item_count:loader.item_count
  in

  (* When writing merged ranges, port0 must use ram_wb.address, not addr0 *)
  let port0_addr = mux2 ram_wb.write_enable ram_wb.address addr0 in

  Ram.Port.Of_signal.assign ram_ports.(0)
    ({ address = port0_addr
     ; write_data = ram_wb.write_data
     ; write_enable = ram_wb.write_enable
     } : Signal.t Ram.Port.t)
  ;

  Ram.Port.Of_signal.assign ram_ports.(1)
    ({ address = addr1
     ; write_data = zero 64
     ; write_enable = gnd
     } : Signal.t Ram.Port.t)
  ;

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 part1_value; valid = done_pulse }
      ; part2 = { value = uresize ~width:60 part2_value; valid = done_pulse }
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
  S.hierarchical ~name:"day05" ~scope create
;;
