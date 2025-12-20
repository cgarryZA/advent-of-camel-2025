open! Core
open! Hardcaml
open! Signal
open! Always

(* ============================================================================
   Day 4 “toilet paper” — packed / streaming stencil RTL

   Grid update uses a 3x3 neighbourhood rule over a padded (rows+2)x(cols+2)
   bit-grid. The padded border is zero, so boundary conditions become masking.

   The grid is streamed left-to-right, row-by-row, in packed words of [lanes]
   bits. A 3x3 window is formed with:
   - two vertical delays (line buffers)
   - three-word horizontal shift registers per stream (prev/cur/next)

   Double-buffer RAM alternates source/destination each pass until no removals.
   ============================================================================ *)

let lanes = 64

let rows = 135
let cols = 135

let ext_rows = rows + 2
let ext_cols = cols + 2

let words_per_row = (ext_cols + lanes - 1) / lanes
let stride = words_per_row + 2

let words_total = ext_rows * words_per_row
let load_addr_bits = Int.ceil_log2 words_total

let count_bits = Int.ceil_log2 (rows * cols + 1)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; load_we : 'a
    ; load_addr : 'a [@bits load_addr_bits]
    ; load_word : 'a [@bits lanes]
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
    | Scan
    | Check
    | Finished
  [@@deriving sexp_of, compare ~localize, enumerate]
end

type tag_pipe =
  { aligned : Variable.t
  ; incoming : Variable.t
  }

type window =
  { nw : Signal.t
  ; n : Signal.t
  ; ne : Signal.t
  ; w : Signal.t
  ; c : Signal.t
  ; e : Signal.t
  ; sw : Signal.t
  ; s : Signal.t
  ; se : Signal.t
  }

let maj a b c = (a &: b) |: (a &: c) |: (b &: c)

let add2_2bit (a0, a1) (b0, b1) =
  let s0 = a0 ^: b0 in
  let c0 = a0 &: b0 in
  let s1 = a1 ^: b1 ^: c0 in
  let c1 = maj a1 b1 c0 in
  (s0, s1, c1)

let add2_3bit (a0, a1, a2) (b0, b1, b2) =
  let s0 = a0 ^: b0 in
  let c0 = a0 &: b0 in
  let s1 = a1 ^: b1 ^: c0 in
  let c1 = maj a1 b1 c0 in
  let s2 = a2 ^: b2 ^: c1 in
  let c2 = maj a2 b2 c1 in
  let s3 = c2 in
  (s0, s1, s2, s3)

let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
  let open Always in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create (module States) spec ~enable:vdd in

  let word_idx_bits = Int.ceil_log2 stride in
  let ext_row_bits = Int.ceil_log2 ext_rows in

  (* Tag packs (ext_row, word_idx, valid) to align the center stream. *)
  let tag_bits = ext_row_bits + word_idx_bits + 1 in

  let pack_tag ~(ext_row : Signal.t) ~(word_idx : Signal.t) ~(valid : Signal.t) : Signal.t =
    concat_msb
      [ uresize ~width:ext_row_bits ext_row
      ; uresize ~width:word_idx_bits word_idx
      ; uresize ~width:1 valid
      ]
  in

  let tag_ext_row (t : Signal.t) = select t ~high:(tag_bits - 1) ~low:(word_idx_bits + 1) in
  let tag_word_idx (t : Signal.t) = select t ~high:word_idx_bits ~low:1 in
  let tag_valid (t : Signal.t) = select t ~high:0 ~low:0 in

  let tag_aligned = Variable.reg spec ~width:tag_bits in
  let tag_incoming = Variable.reg spec ~width:tag_bits in

  let tag : tag_pipe = { aligned = tag_aligned; incoming = tag_incoming } in

  let tag_cur_s = Variable.value tag.aligned in
  let tag_incoming_s = Variable.value tag.incoming in

  let tag_ext_row_cur_s = tag_ext_row tag_cur_s in
  let tag_word_idx_cur_s = tag_word_idx tag_cur_s in
  let v_cur_s = tag_valid tag_cur_s in

  let delay_n ~enable n x =
    let y = ref x in
    for _ = 1 to n do
      y := reg spec ~enable !y
    done;
    !y
  in

  let is_scan = sm.is Scan in
  let shift_en = is_scan in

  let zero_lanes = zero lanes in

  (* Double-buffer select: 0 reads grid_mem, 1 reads grid_next. *)
  let src_sel = Variable.reg spec ~width:1 in
  let src_sel_s = Variable.value src_sel in

  (* Scan pointer over the streamed (ext_row, word_idx) schedule. *)
  let ext_row = Variable.reg spec ~width:ext_row_bits in
  let word_idx = Variable.reg spec ~width:word_idx_bits in
  let ext_row_s = Variable.value ext_row in
  let word_idx_s = Variable.value word_idx in

  (* Per-pass removal count and accumulated outputs. *)
  let removed_this_pass = Variable.reg spec ~width:count_bits in
  let p1 = Variable.reg spec ~width:count_bits in
  let p2 = Variable.reg spec ~width:count_bits in
  let first_done = Variable.reg spec ~width:1 in

  let removed_this_pass_s = Variable.value removed_this_pass in
  let p1_s = Variable.value p1 in
  let p2_s = Variable.value p2 in
  let first_done_s = Variable.value first_done in

  (* Drain flag holds shifting long enough for the center tags/window to flush. *)
  let draining = Variable.reg spec ~width:1 in
  let draining_s = Variable.value draining in

  let ready = Variable.wire ~default:gnd () in
  let finished = Variable.wire ~default:gnd () in

  (* Load is accepted only in Idle. *)
  let load_active = (sm.is Idle) &: i.load_we in

  (* Packed word address: ext_row * words_per_row + word_idx *)
  let words_per_row_c = of_int_trunc ~width:load_addr_bits words_per_row in
  let addr_of_ext_row_word_idx (er : Signal.t) (wi : Signal.t) : Signal.t =
    let er_w = uresize ~width:load_addr_bits er in
    let wi_w = uresize ~width:load_addr_bits wi in
    let prod = uresize ~width:load_addr_bits (er_w *: words_per_row_c) in
    prod +: wi_w
  in

  (* Stream issues dummy words for flushing; dummy reads are clamped/masked to zero. *)
  let issued_is_real_word = word_idx_s <:. words_per_row in
  let issued_word_idx_clamped = mux2 issued_is_real_word word_idx_s (zero word_idx_bits) in
  let rd_addr = addr_of_ext_row_word_idx ext_row_s issued_word_idx_clamped in

  (* Tags are aligned to the synchronous RAM output. *)
  let tag_ext_row_ram = delay_n ~enable:shift_en 1 ext_row_s in
  let tag_word_idx_ram = delay_n ~enable:shift_en 1 word_idx_s in
  let valid_ram = delay_n ~enable:shift_en 1 (is_scan &: issued_is_real_word) in

  (* Center tag follows the center stream latency (two line buffers). *)
  let tag_ext_row_center_in = delay_n ~enable:shift_en stride tag_ext_row_ram in
  let tag_word_idx_center_in = delay_n ~enable:shift_en stride tag_word_idx_ram in
  let valid_center_in = delay_n ~enable:shift_en stride valid_ram in

  let tag_center_in =
    pack_tag ~ext_row:tag_ext_row_center_in ~word_idx:tag_word_idx_center_in ~valid:valid_center_in
  in

  (* Column mask enables only interior columns (1 .. ext_cols-2). *)
  let mask_words : Signal.t array =
    Array.init words_per_row ~f:(fun wi ->
      let bits =
        List.init lanes ~f:(fun b ->
          let col = (wi * lanes) + b in
          let ok = col < ext_cols && col > 0 && col < (ext_cols - 1) in
          if ok then vdd else gnd)
      in
      concat_msb (List.rev bits))
  in

  let mask_for_word_idx (wi : Signal.t) : Signal.t =
    let acc = ref mask_words.(0) in
    for j = 1 to words_per_row - 1 do
      acc := mux2 (wi ==:. j) mask_words.(j) !acc
    done;
    !acc
  in

  let rd_port =
    Read_port.{ read_clock = i.clock; read_enable = vdd; read_address = rd_addr }
  in

  (* Center updates exclude top/bottom padded rows and dummy words. *)
  let center_row_is_interior =
    (tag_ext_row_cur_s >:. 0) &: (tag_ext_row_cur_s <:. (ext_rows - 1))
  in
  let center_word_is_real = tag_word_idx_cur_s <:. words_per_row in
  let center_can_update = is_scan &: v_cur_s &: center_row_is_interior &: center_word_is_real in

  let dst_wi = mux2 center_word_is_real tag_word_idx_cur_s (zero word_idx_bits) in
  let dst_addr = addr_of_ext_row_word_idx tag_ext_row_cur_s dst_wi in

  let write_word_w = wire lanes in

  let mk_ram ~name ~write_enable =
    Ram.create
      ~name
      ~collision_mode:Read_before_write
      ~size:words_total
      ~write_ports:
        [| Write_port.
             { write_clock = i.clock
             ; write_enable
             ; write_address = mux2 load_active i.load_addr dst_addr
             ; write_data = mux2 load_active i.load_word write_word_w
             }
        |]
      ~read_ports:[| rd_port |]
      ()
  in

  let grid_mem_q =
    mk_ram
      ~name:"grid_mem_words"
      ~write_enable:(load_active |: (center_can_update &: src_sel_s))
  in

  let grid_next_q =
    mk_ram
      ~name:"grid_next_words"
      ~write_enable:(load_active |: (center_can_update &: ~:src_sel_s))
  in

  let src_word_raw = mux2 src_sel_s grid_next_q.(0) grid_mem_q.(0) in
  let src_word_issued = mux2 (tag_word_idx_ram <:. words_per_row) src_word_raw zero_lanes in

  let south_word_in = mux2 is_scan src_word_issued zero_lanes in
  let center_word_in = delay_n ~enable:shift_en stride south_word_in in
  let north_word_in = delay_n ~enable:shift_en stride center_word_in in

  let word_idx_last = word_idx_s ==:. (stride - 1) in
  let word_idx_next = mux2 word_idx_last (zero word_idx_bits) (word_idx_s +:. 1) in
  let ext_row_next = mux2 word_idx_last (ext_row_s +:. 1) ext_row_s in

  (* Horizontal 3-word window per stream. *)
  let n_prev = Variable.reg spec ~width:lanes in
  let n_cur = Variable.reg spec ~width:lanes in
  let n_next = Variable.reg spec ~width:lanes in
  let c_prev = Variable.reg spec ~width:lanes in
  let c_cur = Variable.reg spec ~width:lanes in
  let c_next = Variable.reg spec ~width:lanes in
  let s_prev = Variable.reg spec ~width:lanes in
  let s_cur = Variable.reg spec ~width:lanes in
  let s_next = Variable.reg spec ~width:lanes in

  let n_prev_s = Variable.value n_prev in
  let n_cur_s = Variable.value n_cur in
  let n_next_s = Variable.value n_next in
  let c_prev_s = Variable.value c_prev in
  let c_cur_s = Variable.value c_cur in
  let c_next_s = Variable.value c_next in
  let s_prev_s = Variable.value s_prev in
  let s_cur_s = Variable.value s_cur in
  let s_next_s = Variable.value s_next in

  let clear_windows =
    [ n_prev <-- zero_lanes; n_cur <-- zero_lanes; n_next <-- zero_lanes
    ; c_prev <-- zero_lanes; c_cur <-- zero_lanes; c_next <-- zero_lanes
    ; s_prev <-- zero_lanes; s_cur <-- zero_lanes; s_next <-- zero_lanes
    ]
  in

  let clear_scan_ptrs =
    [ ext_row <-- zero ext_row_bits
    ; word_idx <-- zero word_idx_bits
    ]
  in

  let clear_tag =
    [ tag.aligned <-- zero tag_bits
    ; tag.incoming <-- zero tag_bits
    ]
  in

  let msb x = select x ~high:(lanes - 1) ~low:(lanes - 1) in
  let lsb x = select x ~high:0 ~low:0 in

  let west cur prev = concat_msb [ select cur ~high:(lanes - 2) ~low:0; msb prev ] in
  let east cur next = concat_msb [ lsb next; select cur ~high:(lanes - 1) ~low:1 ] in

  let win : window =
    { nw = west n_cur_s n_prev_s
    ; n = n_cur_s
    ; ne = east n_cur_s n_next_s
    ; w = west c_cur_s c_prev_s
    ; c = c_cur_s
    ; e = east c_cur_s c_next_s
    ; sw = west s_cur_s s_prev_s
    ; s = s_cur_s
    ; se = east s_cur_s s_next_s
    }
  in

  (* Bit-sliced popcount of the 8 neighbours, then remove if count < 4. *)
  let (p0_l, p0_h) = (win.nw ^: win.n, win.nw &: win.n) in
  let (p1_l, p1_h) = (win.ne ^: win.w, win.ne &: win.w) in
  let (p2_l, p2_h) = (win.e ^: win.sw, win.e &: win.sw) in
  let (p3_l, p3_h) = (win.s ^: win.se, win.s &: win.se) in

  let s01 = add2_2bit (p0_l, p0_h) (p1_l, p1_h) in
  let s23 = add2_2bit (p2_l, p2_h) (p3_l, p3_h) in
  let (_b0, _b1, b2, b3) = add2_3bit s01 s23 in

  let lt4 = ~:(b2 |: b3) in
  let remove = win.c &: lt4 in
  let keep = win.c &: ~:remove in

  let col_mask = mask_for_word_idx tag_word_idx_cur_s in
  let remove_masked = remove &: col_mask in
  let keep_masked = keep &: col_mask in

  let () = Signal.assign write_word_w keep_masked in

  (* Wide popcount using a reduction tree; avoids relying on library popcount. *)
  let removal_count_word =
    let bits = List.init lanes ~f:(fun lane -> select remove_masked ~high:lane ~low:lane) in
    let add_widen a b =
      let w = Int.max (width a) (width b) + 1 in
      (uresize ~width:w a) +: (uresize ~width:w b)
    in
    let rec pairwise acc = function
      | a :: b :: tl -> pairwise (add_widen a b :: acc) tl
      | [a] -> List.rev (a :: acc)
      | [] -> List.rev acc
    in
    let rec reduce = function
      | [] -> zero 1
      | [x] -> x
      | many -> reduce (pairwise [] many)
    in
    uresize ~width:count_bits (reduce bits)
  in

  (* End-of-scan and drain conditions. *)
  let issued_last_word = (ext_row_s ==:. (ext_rows - 1)) &: (word_idx_s ==:. (stride - 1)) in
  let pipeline_drained =
    draining_s
    &: v_cur_s
    &: (tag_ext_row_cur_s ==:. (ext_rows - 2))
    &: (tag_word_idx_cur_s ==:. (words_per_row - 1))
  in

  let row_start_in = (tag_valid tag_center_in) &: (tag_word_idx tag_center_in ==:. 0) in

  compile
    [ sm.switch
        [ ( Idle
          , [ ready <-- vdd
            ; finished <-- gnd
            ; draining <-- gnd
            ; removed_this_pass <-- zero count_bits
            ] @ clear_scan_ptrs @ clear_windows @ clear_tag
              @ [ when_ i.start
                    [ src_sel <-- gnd
                    ; removed_this_pass <-- zero count_bits
                    ; p1 <-- zero count_bits
                    ; p2 <-- zero count_bits
                    ; first_done <-- gnd
                    ; draining <-- gnd
                    ; sm.set_next Scan
                    ]
                ]
          )
        ; ( Scan
          , [ ready <-- gnd
            ; finished <-- gnd

            ; when_ row_start_in
                [ n_prev <-- zero_lanes; n_cur <-- zero_lanes; n_next <-- north_word_in
                ; c_prev <-- zero_lanes; c_cur <-- zero_lanes; c_next <-- center_word_in
                ; s_prev <-- zero_lanes; s_cur <-- zero_lanes; s_next <-- south_word_in
                ; tag.aligned <-- zero tag_bits
                ; tag.incoming <-- tag_center_in
                ]

            ; when_ (~:row_start_in)
                [ n_prev <-- n_cur_s; n_cur <-- n_next_s; n_next <-- north_word_in
                ; c_prev <-- c_cur_s; c_cur <-- c_next_s; c_next <-- center_word_in
                ; s_prev <-- s_cur_s; s_cur <-- s_next_s; s_next <-- south_word_in
                ; tag.aligned <-- tag_incoming_s
                ; tag.incoming <-- tag_center_in
                ]

            ; when_ center_can_update
                [ removed_this_pass <-- removed_this_pass_s +: removal_count_word ]

            ; when_ ((~:draining_s) &: issued_last_word)
                [ draining <-- vdd ]

            ; when_ pipeline_drained
                [ draining <-- gnd
                ; sm.set_next Check
                ]

            ; when_ ((~:draining_s) &: (~:issued_last_word))
                [ word_idx <-- word_idx_next
                ; ext_row <-- ext_row_next
                ]
            ] )
        ; ( Check
          , [ ready <-- gnd
            ; finished <-- gnd
            ; draining <-- gnd

            ; when_ (~:first_done_s)
                [ p1 <-- removed_this_pass_s
                ; first_done <-- vdd
                ]

            ; when_ (removed_this_pass_s ==:. 0)
                [ sm.set_next Finished ]

            ; when_ (~:(removed_this_pass_s ==:. 0))
                ( [ p2 <-- p2_s +: removed_this_pass_s
                  ; removed_this_pass <-- zero count_bits
                  ; src_sel <-- ~:src_sel_s
                  ; draining <-- gnd
                  ] @ clear_scan_ptrs @ clear_windows @ clear_tag @ [ sm.set_next Scan ] )
            ] )
        ; ( Finished
          , [ ready <-- gnd
            ; finished <-- vdd
            ; when_ i.finish [ sm.set_next Idle ]
            ] )
        ]
    ]
  ;

  { O.ready = Variable.value ready
  ; p1 = p1_s
  ; p2 = p2_s
  ; finished = Variable.value finished
  }
;;

let hierarchical = create
