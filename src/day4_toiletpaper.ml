open! Core
open! Hardcaml
open! Signal

let count_bits = 32

let rows = 135
let cols = 135
let cells = rows * cols
let addr_bits = 15 (* ceil_log2(135*135)=15 *)

(* extended scan width (pad 1 cell left/right) *)
let ext_cols = cols + 2
let stride = ext_cols

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; load_we : 'a
    ; load_addr : 'a [@bits addr_bits]
    ; load_data : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a
    ; p1 : 'a [@bits count_bits]
    ; p2 : 'a [@bits count_bits]
    ; finished : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Prime
    | Scan
    | Check
    | Finished
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
  let open Always in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create (module States) spec ~enable:vdd in

  (* -------------------------------------------------------------------------- *)
  (* helpers                                                                    *)
  (* -------------------------------------------------------------------------- *)

  let cols_c = of_int_trunc ~width:addr_bits cols in

  let addr_of_row_col (r : Signal.t) (c : Signal.t) : Signal.t =
    let r15 = uresize ~width:addr_bits r in
    let c15 = uresize ~width:addr_bits c in
    let prod15 = uresize ~width:addr_bits (r15 *: cols_c) in
    prod15 +: c15
  in

  let delay_n (n : int) (x : Signal.t) : Signal.t =
    let y = ref x in
    for _ = 1 to n do
      y := reg spec !y
    done;
    !y
  in

  (* -------------------------------------------------------------------------- *)
  (* regs                                                                        *)
  (* -------------------------------------------------------------------------- *)

  (* which=0: src=mem  dst=next
     which=1: src=next dst=mem *)
  let which = Variable.reg spec ~width:1 in
  let which_s = Variable.value which in

  let erow_bits = 8 in
  let ecol_bits = 8 in
  let erow = Variable.reg spec ~width:erow_bits in
  let ecol = Variable.reg spec ~width:ecol_bits in
  let erow_s = Variable.value erow in
  let ecol_s = Variable.value ecol in

  let done_issued = Variable.reg spec ~width:1 in
  let done_issued_s = Variable.value done_issued in

  let prime_cnt = Variable.reg spec ~width:16 in
  let prime_cnt_s = Variable.value prime_cnt in

  let removed = Variable.reg spec ~width:count_bits in
  let p1 = Variable.reg spec ~width:count_bits in
  let p2 = Variable.reg spec ~width:count_bits in
  let first_done = Variable.reg spec ~width:1 in

  let removed_s = Variable.value removed in
  let p1_s = Variable.value p1 in
  let p2_s = Variable.value p2 in
  let first_done_s = Variable.value first_done in

  (* -------------------------------------------------------------------------- *)
  (* issued coord -> source RAM address (extended padding)                       *)
  (* -------------------------------------------------------------------------- *)

  (* in-range for real grid is erow=1..rows, ecol=1..cols *)
  let in_range_cur =
    (erow_s >:. 0) &: (erow_s <:. (rows + 1)) &: (ecol_s >:. 0) &: (ecol_s <:. (cols + 1))
  in

  let src_row_cur = erow_s -:. 1 in
  let src_col_cur = ecol_s -:. 1 in
  let src_addr_cur = addr_of_row_col src_row_cur src_col_cur in
  let safe_src_addr = mux2 in_range_cur src_addr_cur (zero addr_bits) in

  (* -------------------------------------------------------------------------- *)
  (* RAMs: 1 read port, write_data via wires                                      *)
  (* -------------------------------------------------------------------------- *)

  let load_active = (sm.is Idle) &: i.load_we in

  (* write_data wires *)
  let mem_wdata = wire 1 in
  let next_wdata = wire 1 in

  (* Destination address comes from delayed center coordinate (defined later),
     but we need a placeholder here for write_address wires. *)
  let dst_addr_w = wire addr_bits in

  let we_next_w = wire 1 in
  let we_mem_core_w = wire 1 in
  let we_mem_w = wire 1 in
  let mem_wr_addr_w = wire addr_bits in

  let read_port =
    Read_port.{ read_clock = i.clock; read_enable = vdd; read_address = safe_src_addr }
  in

  let grid_mem_q =
    Ram.create
      ~name:"grid_mem"
      ~collision_mode:Read_before_write
      ~size:cells
      ~write_ports:
        [| Write_port.
             { write_clock = i.clock
             ; write_enable = we_mem_w
             ; write_address = mem_wr_addr_w
             ; write_data = mem_wdata
             }
        |]
      ~read_ports:[| read_port |]
      ()
  in

  let grid_next_q =
    Ram.create
      ~name:"grid_next"
      ~collision_mode:Read_before_write
      ~size:cells
      ~write_ports:
        [| Write_port.
             { write_clock = i.clock
             ; write_enable = we_next_w
             ; write_address = dst_addr_w
             ; write_data = next_wdata
             }
        |]
      ~read_ports:[| read_port |]
      ()
  in

  (* pick source bit *)
  let src_bit = mux2 which_s grid_next_q.(0) grid_mem_q.(0) in

  (* RAM output is for address issued previous cycle; mask with delayed in_range *)
  let in_range_rd = reg spec in_range_cur in
  let south_stream_raw = mux2 in_range_rd src_bit gnd in

  (* during Prime we want to flush with zeros, but still clock the delays *)
  let stream_enable = (sm.is Scan) |: (sm.is Prime) in
  let south_stream = mux2 stream_enable south_stream_raw gnd in

  (* -------------------------------------------------------------------------- *)
  (* line delays: produce north/center/south samples for same column             *)
  (* -------------------------------------------------------------------------- *)

  let center_in = delay_n stride south_stream in
  let north_in = delay_n stride center_in in
  let south_in = south_stream in

  (* -------------------------------------------------------------------------- *)
  (* column windows: 2 regs (west/center) + incoming is east                     *)
  (* -------------------------------------------------------------------------- *)

  let nw_r = Variable.reg spec ~width:1 in
  let nc_r = Variable.reg spec ~width:1 in

  let cw_r = Variable.reg spec ~width:1 in
  let cc_r = Variable.reg spec ~width:1 in

  let sw_r = Variable.reg spec ~width:1 in
  let sc_r = Variable.reg spec ~width:1 in

  let nw = Variable.value nw_r in
  let nc = Variable.value nc_r in
  let cw = Variable.value cw_r in
  let cc = Variable.value cc_r in
  let sw = Variable.value sw_r in
  let sc = Variable.value sc_r in

  (* 3x3 taps:
     [ nw  nc  north_in
       cw  cc  center_in
       sw  sc  south_in ] *)

  let sum_bits = 4 in
  let e4 x = uresize ~width:sum_bits x in

  let neighbor_sum =
    e4 nw +: e4 nc +: e4 north_in +:
    e4 cw +:            e4 center_in +:
    e4 sw +: e4 sc +: e4 south_in
  in

  let remove = cc &: (neighbor_sum <:. 4) in
  let write_bit = cc &: ~:remove in

  (* -------------------------------------------------------------------------- *)
  (* destination address alignment (THIS IS THE FIX)                             *)
  (* -------------------------------------------------------------------------- *)

  (* center pixel (cc) corresponds to issued coordinate delayed by (stride + 2) *)
  let erow_center = delay_n (stride + 2) erow_s in
  let ecol_center = delay_n (stride + 2) ecol_s in
  let in_range_center = delay_n (stride + 2) in_range_cur in

  let write_valid = (sm.is Scan) &: in_range_center in

  let dst_row = erow_center -:. 1 in
  let dst_col = ecol_center -:. 1 in
  let dst_addr = mux2 write_valid (addr_of_row_col dst_row dst_col) (zero addr_bits) in

  let we_next = write_valid &: ~:which_s in
  let we_mem_core = write_valid &: which_s in
  let we_mem = load_active |: we_mem_core in

  let mem_wr_addr = mux2 load_active i.load_addr dst_addr in

  (* drive the RAM control wires *)
  let () = Signal.assign dst_addr_w dst_addr in
  let () = Signal.assign we_next_w we_next in
  let () = Signal.assign we_mem_core_w we_mem_core in
  let () = Signal.assign we_mem_w we_mem in
  let () = Signal.assign mem_wr_addr_w mem_wr_addr in

  (* drive the RAM write_data wires *)
  let () = Signal.assign next_wdata write_bit in
  let () = Signal.assign mem_wdata (mux2 load_active i.load_data write_bit) in

  (* -------------------------------------------------------------------------- *)
  (* control                                                                    *)
  (* -------------------------------------------------------------------------- *)

  let prime_cycles = (2 * stride) + 4 in
  let prime_last = prime_cycles - 1 in

  let at_last_issue =
    (erow_s ==:. (rows + 1)) &: (ecol_s ==:. (cols + 1))
  in

  let ready = Variable.wire ~default:gnd () in
  let finished = Variable.wire ~default:gnd () in

  compile
    [ sm.switch
        [ ( Idle
          , [ ready <-- vdd
            ; finished <-- gnd
            ; done_issued <-- gnd
            ; erow <-- zero erow_bits
            ; ecol <-- zero ecol_bits
            ; prime_cnt <-- zero 16
            ; when_ i.start
                [ which <-- gnd
                ; removed <-- zero count_bits
                ; p1 <-- zero count_bits
                ; p2 <-- zero count_bits
                ; first_done <-- gnd
                ; nw_r <-- gnd; nc_r <-- gnd
                ; cw_r <-- gnd; cc_r <-- gnd
                ; sw_r <-- gnd; sc_r <-- gnd
                ; prime_cnt <-- zero 16
                ; sm.set_next Prime
                ]
            ] )

        ; ( Prime
          , [ ready <-- gnd
            ; finished <-- gnd
            ; done_issued <-- gnd
            ; (* shift windows with zeros (incoming samples are forced 0 via stream_enable) *)
              nw_r <-- nc
            ; nc_r <-- gnd
            ; cw_r <-- cc
            ; cc_r <-- gnd
            ; sw_r <-- sc
            ; sc_r <-- gnd
            ; prime_cnt <-- prime_cnt_s +:. 1
            ; when_ (prime_cnt_s ==:. prime_last)
                [ erow <-- zero erow_bits
                ; ecol <-- zero ecol_bits
                ; prime_cnt <-- zero 16
                ; sm.set_next Scan
                ]
            ] )

        ; ( Scan
          , [ ready <-- gnd
            ; finished <-- gnd

            ; (* shift windows (incoming samples are north_in/center_in/south_in) *)
              nw_r <-- nc
            ; nc_r <-- north_in
            ; cw_r <-- cc
            ; cc_r <-- center_in
            ; sw_r <-- sc
            ; sc_r <-- south_in

            ; when_ (write_valid &: remove)
                [ removed <-- removed_s +:. 1 ]

            ; when_ done_issued_s [ sm.set_next Check ]

            ; when_ (~:done_issued_s)
                [ when_ at_last_issue [ done_issued <-- vdd ]
                ; when_ (~:at_last_issue)
                    [ when_ (ecol_s ==:. (cols + 1))
                        [ ecol <-- zero ecol_bits
                        ; erow <-- erow_s +:. 1
                        ]
                    ; when_ (~:(ecol_s ==:. (cols + 1)))
                        [ ecol <-- ecol_s +:. 1 ]
                    ]
                ]
            ] )

        ; ( Check
          , [ ready <-- gnd
            ; finished <-- gnd

            ; when_ (~:first_done_s)
                [ p1 <-- removed_s
                ; first_done <-- vdd
                ]

            ; when_ (removed_s ==:. 0) [ sm.set_next Finished ]

            ; when_ (~:(removed_s ==:. 0))
                [ p2 <-- p2_s +: removed_s
                ; removed <-- zero count_bits
                ; which <-- ~:which_s
                ; done_issued <-- gnd
                ; erow <-- zero erow_bits
                ; ecol <-- zero ecol_bits
                ; prime_cnt <-- zero 16
                ; nw_r <-- gnd; nc_r <-- gnd
                ; cw_r <-- gnd; cc_r <-- gnd
                ; sw_r <-- gnd; sc_r <-- gnd
                ; sm.set_next Prime
                ]
            ] )

        ; ( Finished
          , [ ready <-- gnd
            ; finished <-- vdd
            ; when_ i.finish [ sm.set_next Idle ]
            ] )
        ]
    ];

  { O.ready = Variable.value ready
  ; p1 = p1_s
  ; p2 = p2_s
  ; finished = Variable.value finished
  }
;;

let hierarchical = create
