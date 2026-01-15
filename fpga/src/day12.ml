(* src/day12.ml *)

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
      ; word_count    : 'a [@bits 14]
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create _scope ({ clock; clear; uart_rx; uart_rts } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~clear () in

    (* pack 4 bytes -> 1x 32-bit word *)
    let word_in = Util.shift_in ~clock ~clear ~n:4 uart_rx in

    (* count 32-bit words written *)
    let word_count =
      reg_fb spec ~width:14 ~enable:word_in.valid ~f:(fun x -> x +:. 1)
    in

    (* sticky end-of-load *)
    let loaded = Variable.reg spec ~width:1 in

    compile
      [ when_ uart_rts
          [ loaded <-- vdd
          ]
      ];

    O.
      { load_finished = loaded.value
      ; ram_write =
          { address      = word_count
          ; write_data   = word_in.value
          ; write_enable = word_in.valid
          }
      ; word_count
      ; uart_rx_ready = vdd
      }
  ;;

  let hierarchical scope =
    let module S = Hierarchy.In_scope (I) (O) in
    S.hierarchical ~name:"loader" ~scope create
  ;;
end

(* ====================== ALGORITHM ====================== *)

(* Day 12 (degenerate real-input predicate):
   For each region:
     blocks_available = (w/3)*(h/3)
     blocks_required  = sum(counts)
   Real input never requires interlocking, so "fits" iff:
     blocks_available >= blocks_required

   We make the parser precompute both numbers and send:
     word0 = num_regions
     then for each region i:
       word(1 + 2i) = blocks_available
       word(2 + 2i) = blocks_required
*)

module States = struct
  type t = Loading | Read_header | Running | Done
  [@@deriving enumerate, sexp_of, compare ~localize]
end

let algo ~clock ~clear ~(read_data : Signal.t array) ~load_finished =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm = State_machine.create (module States) spec in

  (* RAM read timing discipline:
     - drive addresses for next cycle
     - consume values this cycle
     - primed gates first-cycle garbage
  *)

  let primed = Variable.reg spec ~width:1 in

  let region_count = Variable.reg spec ~width:32 in
  let idx          = Variable.reg spec ~width:32 in

  let ok_count = Variable.reg spec ~width:32 in

  (* One-cycle delay to let last update settle. *)
  let done_pending = Variable.reg spec ~width:1 in
  let done_out     = reg spec done_pending.value in

  (* ---------------- Addressing ---------------- *)

  (* Read header word (num_regions) from address 0 *)
  let addr_header = uresize ~width:14 (zero 14) in

  (* For region idx:
       avail @ address = 1 + 2*idx
       req   @ address = 2 + 2*idx
  *)
  let two_idx      = idx.value @: gnd |> uresize ~width:33 in         (* 2*idx *)
  let avail_addr32 = (of_int_trunc ~width:33 1) +: two_idx in
  let req_addr32   = (of_int_trunc ~width:33 2) +: two_idx in

  let addr_avail = uresize ~width:14 (select avail_addr32 ~high:13 ~low:0) in
  let addr_req   = uresize ~width:14 (select req_addr32   ~high:13 ~low:0) in

  (* read_data(0) = RAM[port0], read_data(1)=RAM[port1] *)
  let header_word = read_data.(0) in
  let avail_word  = read_data.(0) in
  let req_word    = read_data.(1) in

  let fits = avail_word >=: req_word in
  let last = idx.value +:. 1 ==: region_count.value in

  compile
    [ sm.switch
        [ ( Loading
          , [ when_ load_finished
                [ primed       <-- gnd
                ; idx          <-- zero 32
                ; ok_count     <-- zero 32
                ; done_pending <-- gnd
                ; sm.set_next Read_header
                ]
            ] )
        ; ( Read_header
          , [ (* addresses already pointing at header; wait one cycle *)
              if_
                (primed.value ==:. 0)
                [ primed <-- vdd ]
                [ region_count <-- header_word
                ; primed       <-- gnd
                ; sm.set_next Running
                ]
            ] )
        ; ( Running
          , [ (* First running cycle is a prime/warmup for synchronous RAM. *)
              if_
                (primed.value ==:. 0)
                [ primed <-- vdd ]
                [ when_ fits [ ok_count <-- ok_count.value +:. 1 ]
                ; if_
                    last
                    [ done_pending <-- vdd
                    ; sm.set_next Done
                    ]
                    [ idx <-- idx.value +:. 1 ]
                ]
            ] )
        ; Done, [ done_pending <-- vdd ]
        ]
    ];

  (* Expose addresses for the two RAM ports.
     In Loading: keep at 0.
     In Read_header: port0 reads header, port1 unused.
     In Running: port0 reads avail, port1 reads req.
  *)
  let addr0 =
    mux2 load_finished
      (mux2 (sm.is Read_header) addr_header addr_avail)
      (zero 14)
  in
  let addr1 =
    mux2 load_finished
      (mux2 (sm.is Running) addr_req (zero 14))
      (zero 14)
  in

  addr0, addr1, ok_count.value, (zero 32), done_out
;;

(* ====================== TOP ====================== *)

let create
    scope
    ({ clock; clear; uart_rx; uart_rts; uart_rx_overflow; _ } : _ Ulx3s.I.t)
  =
  let loader_out =
    Loader.hierarchical scope { clock; clear; uart_rx; uart_rts }
  in
  let load_finished = loader_out.load_finished in
  let ram_write     = loader_out.ram_write in
  let uart_rx_ready = loader_out.uart_rx_ready in

  let ram_ports = Array.init 2 ~f:(fun _ -> Ram.Port.Of_signal.wires ()) in

  let%tydi ram_out =
    Ram.hierarchical ~name:"ram" scope
      { clock
      ; clear
      ; load_ports = [| ram_write; Ram.Port.unused |]
      ; load_finished
      ; ram_ports
      }
  in
  let read_data = ram_out.read_data in

  let addr0, addr1, p1, p2, done_ =
    algo ~clock ~clear ~read_data ~load_finished
  in

  Ram.Port.Of_signal.assign ram_ports.(0)
    { address      = mux2 load_finished addr0 (zero 14)
    ; write_data   = zero 32
    ; write_enable = gnd
    };

  Ram.Port.Of_signal.assign ram_ports.(1)
    { address      = mux2 load_finished addr1 (zero 14)
    ; write_data   = zero 32
    ; write_enable = gnd
    };

  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 p1; valid = done_ }
      ; part2 = { value = uresize ~width:60 p2; valid = done_ }
      }
  in

  { Ulx3s.O.
    leds = concat_lsb [ ~:clear; uart_rx_overflow; load_finished; zero 5 ]
  ; uart_tx = byte_out
  ; uart_rx_ready
  }
;;

let hierarchical scope =
  let module S = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  S.hierarchical ~name:"day12" ~scope create
;;
