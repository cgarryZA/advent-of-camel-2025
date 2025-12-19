open! Core
open! Hardcaml
open! Signal

let count_bits = 32

(* Fixed grid dims for Day 4 *)
let rows = 135
let cols = 135
let cells = rows * cols
let addr_bits = 15 (* ceil_log2(135*135)=15 *)

(* 32 cells/cycle lanes *)
let lanes = 18

(* Extended padded grid: (rows+2) x (cols+2) with zeros around border. *)
let ext_rows = rows + 2
let ext_cols = cols + 2

(* We scan in 32-wide words, padding columns out to a multiple of 32. *)
let words_per_row = (ext_cols + lanes - 1) / lanes

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

(* bitwise majority (full-adder carry) for same-width vectors *)
let maj a b c = (a &: b) |: (a &: c) |: (b &: c)

(* Add two 2-bit numbers lane-wise:
   a = (a0 + 2*a1), b = (b0 + 2*b1)
   returns 3-bit: (s0,s1,s2) representing 0..4
*)
let add2_2bit (a0,a1) (b0,b1) =
  let s0 = a0 ^: b0 in
  let c0 = a0 &: b0 in
  let s1 = a1 ^: b1 ^: c0 in
  let c1 = maj a1 b1 c0 in
  (s0, s1, c1)

(* Add two 3-bit numbers lane-wise:
   a = a0 + 2*a1 + 4*a2, b = b0 + 2*b1 + 4*b2
   returns 4-bit: (s0,s1,s2,s3) representing 0..8
*)
let add2_3bit (a0,a1,a2) (b0,b1,b2) =
  let s0 = a0 ^: b0 in
  let c0 = a0 &: b0 in
  let s1 = a1 ^: b1 ^: c0 in
  let c1 = maj a1 b1 c0 in
  let s2 = a2 ^: b2 ^: c1 in
  let c2 = maj a2 b2 c1 in
  let s3 = c2 in
  (s0,s1,s2,s3)

let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
  let open Always in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create (module States) spec ~enable:vdd in

  (* -------------------------------------------------------------------------- *)
  (* Address helpers                                                            *)
  (* -------------------------------------------------------------------------- *)

  let cols_c = of_int_trunc ~width:addr_bits cols in

  let addr_of_row_col (r : Signal.t) (c : Signal.t) : Signal.t =
    let r15 = uresize ~width:addr_bits r in
    let c15 = uresize ~width:addr_bits c in
    let prod15 = uresize ~width:addr_bits (r15 *: cols_c) in
    prod15 +: c15
  in

  (* Delay by n cycles *)
  let delay_n (n:int) (x:Signal.t) =
    let y = ref x in
    for _ = 1 to n do y := reg spec !y done;
    !y
  in

  (* -------------------------------------------------------------------------- *)
  (* Control regs                                                               *)
  (* -------------------------------------------------------------------------- *)

  (* which=0: src=grid_mem  dst=grid_next
     which=1: src=grid_next dst=grid_mem *)
  let which = Variable.reg spec ~width:1 in
  let which_s = Variable.value which in

  (* Scan counters over extended padded grid, in 32-wide words *)
  let erow_bits = 8 in
  let widx_bits = 4 in (* words_per_row=5 fits in 3 bits; use 4 *)
  let erow = Variable.reg spec ~width:erow_bits in
  let widx = Variable.reg spec ~width:widx_bits in
  let erow_s = Variable.value erow in
  let widx_s = Variable.value widx in

  (* Tag pipelines *)
  let tag_erow_ram = delay_n 1 erow_s in
  let tag_widx_ram = delay_n 1 widx_s in

  let tag_erow_center_in = delay_n words_per_row tag_erow_ram in
  let tag_widx_center_in = delay_n words_per_row tag_widx_ram in

  (* 3-word window tags for center row *)
  let tag_erow_prev = Variable.reg spec ~width:erow_bits in
  let tag_erow_cur  = Variable.reg spec ~width:erow_bits in
  let tag_erow_next = Variable.reg spec ~width:erow_bits in
  let tag_widx_prev = Variable.reg spec ~width:widx_bits in
  let tag_widx_cur  = Variable.reg spec ~width:widx_bits in
  let tag_widx_next = Variable.reg spec ~width:widx_bits in

  let tag_erow_cur_s = Variable.value tag_erow_cur in
  let tag_widx_cur_s = Variable.value tag_widx_cur in

  (* iteration/result regs *)
  let removed = Variable.reg spec ~width:count_bits in
  let p1 = Variable.reg spec ~width:count_bits in
  let p2 = Variable.reg spec ~width:count_bits in
  let first_done = Variable.reg spec ~width:1 in

  let removed_s = Variable.value removed in
  let p1_s = Variable.value p1 in
  let p2_s = Variable.value p2 in
  let first_done_s = Variable.value first_done in

  (* prime/flush *)
  let prime_cnt = Variable.reg spec ~width:16 in
  let prime_cnt_s = Variable.value prime_cnt in
  let done_issued = Variable.reg spec ~width:1 in
  let done_issued_s = Variable.value done_issued in
  let flush_cnt = Variable.reg spec ~width:16 in
  let flush_cnt_s = Variable.value flush_cnt in

  (* Outputs *)
  let ready = Variable.wire ~default:gnd () in
  let finished = Variable.wire ~default:gnd () in

(* -------------------------------------------------------------------------- *)
  (* Build 32 lane addresses for reading the SOURCE grid                        *)
  (* -------------------------------------------------------------------------- *)

  let lanes_c16 = of_int_trunc ~width:16 lanes in
  let widx16 = uresize ~width:16 widx_s in
  let wbase16 = uresize ~width:16 (widx16 *: lanes_c16) in

  let erow_is_in_src = (erow_s >:. 0) &: (erow_s <:. (rows + 1)) in

  (* Calculate Address AND Valid bit for each lane *)
  let rd_logic =
    Array.init lanes ~f:(fun lane ->
      let lane_c16 = of_int_trunc ~width:16 lane in
      let ecol16 = wbase16 +: lane_c16 in
      let ecol_in_ext = ecol16 <:. ext_cols in
      let ecol_is_in_src = (ecol16 >:. 0) &: (ecol16 <:. (cols + 1)) in
      let in_src = erow_is_in_src &: ecol_in_ext &: ecol_is_in_src in
      
      let src_row = erow_s -:. 1 in
      let src_col = (uresize ~width:8 ecol16) -:. 1 in
      let a = addr_of_row_col src_row src_col in
      
      (* We still mux address to 0 to prevent out-of-bounds reads, 
         but the data masking is the critical part. *)
      (in_src, mux2 in_src a (zero addr_bits))
    )
  in

  let rd_addrs = Array.map rd_logic ~f:snd in
  let rd_valids = Array.map rd_logic ~f:fst in

  let rd_ports =
    Array.map rd_addrs ~f:(fun a ->
      Read_port.{ read_clock = i.clock; read_enable = vdd; read_address = a })
  in

  (* -------------------------------------------------------------------------- *)
  (* Double-buffer bit RAMs                                                     *)
  (* -------------------------------------------------------------------------- *)

  (* ... (Existing write logic setup matches exactly, omitting for brevity) ... *)
  let load_active = (sm.is Idle) &: i.load_we in
  
  (* Copy your existing write port logic here (dst_lane_enable_and_addr, etc.) *)
  let dst_lane_enable_and_addr (lane:int) =
    let lane_c16 = of_int_trunc ~width:16 lane in
    let tag_widx16 = uresize ~width:16 tag_widx_cur_s in
    let wbase16 = uresize ~width:16 (tag_widx16 *: lanes_c16) in
    let ecol16 = wbase16 +: lane_c16 in
    let ecol_in_ext = ecol16 <:. ext_cols in
    let er_ok = (tag_erow_cur_s >:. 0) &: (tag_erow_cur_s <:. (rows + 1)) in
    let ec_ok = (ecol16 >:. 0) &: (ecol16 <:. (cols + 1)) in
    let valid = (sm.is Scan) &: er_ok &: ec_ok &: ecol_in_ext in
    let r = tag_erow_cur_s -:. 1 in
    let c = (uresize ~width:8 ecol16) -:. 1 in
    let addr = addr_of_row_col r c in
    (valid, addr)
  in

  let dst_wdata = Array.init lanes ~f:(fun _ -> wire 1) in

  let grid_mem_write_ports =
    Array.append
      [| Write_port.
           { write_clock = i.clock
           ; write_enable = load_active
           ; write_address = i.load_addr
           ; write_data = i.load_data
           }
      |]
      (Array.init lanes ~f:(fun lane ->
         let (v, a) = dst_lane_enable_and_addr lane in
         Write_port.
           { write_clock = i.clock
           ; write_enable = v &: which_s
           ; write_address = a
           ; write_data = dst_wdata.(lane)
           }))
  in

  let grid_next_write_ports =
    Array.init lanes ~f:(fun lane ->
      let (v, a) = dst_lane_enable_and_addr lane in
      Write_port.
        { write_clock = i.clock
        ; write_enable = v &: ~:which_s
        ; write_address = a
        ; write_data = dst_wdata.(lane)
        })
  in

  let grid_mem_q =
    Ram.create
      ~name:"grid_mem"
      ~collision_mode:Read_before_write
      ~size:cells
      ~write_ports:grid_mem_write_ports
      ~read_ports:rd_ports
      ()
  in

  let grid_next_q =
    Ram.create
      ~name:"grid_next"
      ~collision_mode:Read_before_write
      ~size:cells
      ~write_ports:grid_next_write_ports
      ~read_ports:rd_ports
      ()
  in

  (* Pipeline the valid bits to match RAM latency (1 cycle) *)
  let rd_valids_pipelined = Array.map rd_valids ~f:(fun v -> Signal.reg spec v) in

  let src_bits =
    Array.init lanes ~f:(fun lane ->
      let raw_data = mux2 which_s grid_next_q.(lane) grid_mem_q.(lane) in
      (* CRITICAL FIX: Mask the data with the pipelined valid bit *)
      raw_data &: rd_valids_pipelined.(lane)
    )
  in

  let south_word_in = concat_msb (Array.to_list (Array.rev src_bits)) in
  let center_word_in = delay_n words_per_row south_word_in in
  let north_word_in  = delay_n words_per_row center_word_in in

  (* 3-word windows for each row stream *)
  let n_prev = Variable.reg spec ~width:lanes in
  let n_cur  = Variable.reg spec ~width:lanes in
  let n_next = Variable.reg spec ~width:lanes in
  let c_prev = Variable.reg spec ~width:lanes in
  let c_cur  = Variable.reg spec ~width:lanes in
  let c_next = Variable.reg spec ~width:lanes in
  let s_prev = Variable.reg spec ~width:lanes in
  let s_cur  = Variable.reg spec ~width:lanes in
  let s_next = Variable.reg spec ~width:lanes in

  let n_prev_s = Variable.value n_prev in
  let n_cur_s  = Variable.value n_cur in
  let n_next_s = Variable.value n_next in
  let c_prev_s = Variable.value c_prev in
  let c_cur_s  = Variable.value c_cur in
  let c_next_s = Variable.value c_next in
  let s_prev_s = Variable.value s_prev in
  let s_cur_s  = Variable.value s_cur in
  let s_next_s = Variable.value s_next in

  let msb x = select x ~high:(lanes-1) ~low:(lanes-1) in
  let lsb x = select x ~high:0 ~low:0 in

  let west cur prev =
    concat_msb [ select cur ~high:(lanes-2) ~low:0; msb prev ]
  in
  let east cur next =
    concat_msb [ lsb next; select cur ~high:(lanes-1) ~low:1 ]
  in

  let nw = west n_cur_s n_prev_s in
  let n  = n_cur_s in
  let ne = east n_cur_s n_next_s in

  let w  = west c_cur_s c_prev_s in
  let c0 = c_cur_s in
  let e  = east c_cur_s c_next_s in

  let sw = west s_cur_s s_prev_s in
  let s  = s_cur_s in
  let se = east s_cur_s s_next_s in

  (* Popcount8 per lane (bit-sliced) *)
  let (p0_l, p0_h) = (nw ^: n,  nw &: n) in
  let (p1_l, p1_h) = (ne ^: w,  ne &: w) in
  let (p2_l, p2_h) = (e  ^: sw, e  &: sw) in
  let (p3_l, p3_h) = (s  ^: se, s  &: se) in

  let s01 = add2_2bit (p0_l,p0_h) (p1_l,p1_h) in
  let s23 = add2_2bit (p2_l,p2_h) (p3_l,p3_h) in
  let (_b0,_b1,b2,b3) = add2_3bit s01 s23 in

  let lt4 = ~:(b2 |: b3) in
  let remove = c0 &: lt4 in
  let write_word = c0 &: ~:remove in

  let () =
    for lane = 0 to lanes - 1 do
      let bit = select write_word ~high:lane ~low:lane in
      Signal.assign dst_wdata.(lane) bit
    done
  in

  let removal_count_word =
    let bits = List.init lanes ~f:(fun lane -> select remove ~high:lane ~low:lane) in
    List.fold bits ~init:(zero count_bits) ~f:(fun acc b -> acc +: uresize ~width:count_bits b)
  in

  (* -------------------------------------------------------------------------- *)
  (* State machine sequencing                                                   *)
  (* -------------------------------------------------------------------------- *)

  let at_last_issue =
    (erow_s ==:. (ext_rows - 1)) &: (widx_s ==:. (words_per_row - 1))
  in

  let prime_cycles = (2 * words_per_row) + 4 in
  let prime_last = prime_cycles - 1 in

  let flush_cycles = (2 * words_per_row) + 6 in
  let flush_last = flush_cycles - 1 in

  compile
    [ sm.switch
        [ ( Idle
          , [ ready <-- vdd
            ; finished <-- gnd
            ; done_issued <-- gnd
            ; flush_cnt <-- zero 16
            ; prime_cnt <-- zero 16
            ; erow <-- zero erow_bits
            ; widx <-- zero widx_bits
            ; n_prev <-- zero lanes; n_cur <-- zero lanes; n_next <-- zero lanes
            ; c_prev <-- zero lanes; c_cur <-- zero lanes; c_next <-- zero lanes
            ; s_prev <-- zero lanes; s_cur <-- zero lanes; s_next <-- zero lanes
            ; tag_erow_prev <-- zero erow_bits
            ; tag_erow_cur  <-- zero erow_bits
            ; tag_erow_next <-- zero erow_bits
            ; tag_widx_prev <-- zero widx_bits
            ; tag_widx_cur  <-- zero widx_bits
            ; tag_widx_next <-- zero widx_bits
            ; when_ i.start
                [ which <-- gnd
                ; removed <-- zero count_bits
                ; p1 <-- zero count_bits
                ; p2 <-- zero count_bits
                ; first_done <-- gnd
                ; prime_cnt <-- zero 16
                ; sm.set_next Prime
                ]
            ] )

        ; ( Prime
          , [ ready <-- gnd
            ; finished <-- gnd
            ; n_prev <-- n_cur_s; n_cur <-- n_next_s; n_next <-- zero lanes
            ; c_prev <-- c_cur_s; c_cur <-- c_next_s; c_next <-- zero lanes
            ; s_prev <-- s_cur_s; s_cur <-- s_next_s; s_next <-- zero lanes
            ; tag_erow_prev <-- Variable.value tag_erow_cur
            ; tag_erow_cur  <-- Variable.value tag_erow_next
            ; tag_erow_next <-- zero erow_bits
            ; tag_widx_prev <-- Variable.value tag_widx_cur
            ; tag_widx_cur  <-- Variable.value tag_widx_next
            ; tag_widx_next <-- zero widx_bits
            ; prime_cnt <-- prime_cnt_s +:. 1
            ; when_ (prime_cnt_s ==:. prime_last)
                [ prime_cnt <-- zero 16
                ; erow <-- zero erow_bits
                ; widx <-- zero widx_bits
                ; done_issued <-- gnd
                ; sm.set_next Scan
                ]
            ] )

        ; ( Scan
          , [ ready <-- gnd
            ; finished <-- gnd
            ; n_prev <-- n_cur_s
            ; n_cur  <-- n_next_s
            ; n_next <-- north_word_in
            ; c_prev <-- c_cur_s
            ; c_cur  <-- c_next_s
            ; c_next <-- center_word_in
            ; s_prev <-- s_cur_s
            ; s_cur  <-- s_next_s
            ; s_next <-- south_word_in
            ; tag_erow_prev <-- Variable.value tag_erow_cur
            ; tag_erow_cur  <-- Variable.value tag_erow_next
            ; tag_erow_next <-- tag_erow_center_in
            ; tag_widx_prev <-- Variable.value tag_widx_cur
            ; tag_widx_cur  <-- Variable.value tag_widx_next
            ; tag_widx_next <-- tag_widx_center_in

            ; removed <-- removed_s +: removal_count_word

            ; when_ done_issued_s
                [ flush_cnt <-- flush_cnt_s +:. 1
                ; when_ (flush_cnt_s ==:. flush_last)
                    [ flush_cnt <-- zero 16
                    ; sm.set_next Check
                    ]
                ]

            ; when_ (~:done_issued_s)
                [ when_ at_last_issue [ done_issued <-- vdd ]
                ; when_ (~:at_last_issue)
                    [ when_ (widx_s ==:. (words_per_row - 1))
                        [ widx <-- zero widx_bits
                        ; erow <-- erow_s +:. 1
                        ]
                    ; when_ (~:(widx_s ==:. (words_per_row - 1)))
                        [ widx <-- widx_s +:. 1 ]
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
                ; flush_cnt <-- zero 16
                ; prime_cnt <-- zero 16
                ; erow <-- zero erow_bits
                ; widx <-- zero widx_bits
                ; n_prev <-- zero lanes; n_cur <-- zero lanes; n_next <-- zero lanes
                ; c_prev <-- zero lanes; c_cur <-- zero lanes; c_next <-- zero lanes
                ; s_prev <-- zero lanes; s_cur <-- zero lanes; s_next <-- zero lanes
                ; tag_erow_prev <-- zero erow_bits
                ; tag_erow_cur  <-- zero erow_bits
                ; tag_erow_next <-- zero erow_bits
                ; tag_widx_prev <-- zero widx_bits
                ; tag_widx_cur  <-- zero widx_bits
                ; tag_widx_next <-- zero widx_bits
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
