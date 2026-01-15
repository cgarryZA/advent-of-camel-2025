(* input_parser/day09.ml *)

open! Core
open! Util

(* SIMULATION-ONLY: allow full prefix-sum grid *)
let ram_depth_words = 524288

(* let ram_depth_words = 131072*)

let u32_to_uart_bytes_le (x : int) : Uart_symbol.t list =
  int_to_uart_bytes_le ~n:4 x
;;

let u64_to_uart_bytes_le (x : int64) : Uart_symbol.t list =
  List.init 8 ~f:(fun i ->
    let shifted = Stdlib.Int64.shift_right_logical x (i * 8) in
    let byte = Stdlib.Int64.logand shifted 0xffL |> Stdlib.Int64.to_int in
    Uart_symbol.Byte (Char.of_int_exn byte))
;;

let pack_u16x2_le ~(lo : int) ~(hi : int) : Uart_symbol.t list =
  let lo = lo land 0xFFFF in
  let hi = hi land 0xFFFF in
  u32_to_uart_bytes_le (lo lor (hi lsl 16))
;;

let require_u16 (v : int) ~(what : string) =
  if v < 0 || v > 0xFFFF then
    failwithf "Day09: %s does not fit u16 (%d)" what v ()
;;

let require_u32_nonneg (v : int) ~(what : string) =
  if v < 0 then failwithf "Day09: %s is negative (%d)" what v ();
  if v > 0xFFFF_FFFF then failwithf "Day09: %s does not fit u32 (%d)" what v ();
  v
;;

let compress_coords (pts : (int * int) list) =
  let xs =
    pts
    |> List.map ~f:fst
    |> List.concat_map ~f:(fun x -> [ x; x + 1 ])
    |> List.dedup_and_sort ~compare:Int.compare
  in
  let ys =
    pts
    |> List.map ~f:snd
    |> List.concat_map ~f:(fun y -> [ y; y + 1 ])
    |> List.dedup_and_sort ~compare:Int.compare
  in
  let xid =
    Map.of_alist_exn
      (module Int)
      (List.mapi xs ~f:(fun i v -> v, i))
  in
  let yid =
    Map.of_alist_exn
      (module Int)
      (List.mapi ys ~f:(fun i v -> v, i))
  in
  Array.of_list xs, Array.of_list ys, xid, yid
;;

let mark_boundary_tiles
    ~(boundary : bool array array)
    ~(xid : int Map.M(Int).t)
    ~(yid : int Map.M(Int).t)
    (pts : (int * int) list)
  =
  let n = List.length pts in
  let pts = Array.of_list pts in

  let h = Array.length boundary in
  let w = Array.length boundary.(0) in

  let mark y x =
    if y >= 0 && y < h && x >= 0 && x < w then boundary.(y).(x) <- true
  in

  for i = 0 to n - 1 do
    let x0, y0 = pts.(i) in
    let x1, y1 = pts.((i + 1) mod n) in

    if x0 = x1 then begin
      let x_idx = Map.find_exn xid x0 in
      let y_lo = Int.min y0 y1 in
      let y_hi = Int.max y0 y1 in
      let y0i = Map.find_exn yid y_lo in
      let y1p1 = Map.find_exn yid (y_hi + 1) in
      for y = y0i to y1p1 - 1 do
        mark y x_idx
      done
    end else if y0 = y1 then begin
      let y_idx = Map.find_exn yid y0 in
      let x_lo = Int.min x0 x1 in
      let x_hi = Int.max x0 x1 in
      let x0i = Map.find_exn xid x_lo in
      let x1p1 = Map.find_exn xid (x_hi + 1) in
      for x = x0i to x1p1 - 1 do
        mark y_idx x
      done
    end else
      failwith "Day09: non-axis-aligned edge"
  done
;;

let flood_fill_outside ~(boundary : bool array array) =
  let h = Array.length boundary in
  let w = Array.length boundary.(0) in
  let outside = Array.make_matrix ~dimx:h ~dimy:w false in
  let q = Queue.create () in

  let push y x =
    if y >= 0 && y < h && x >= 0 && x < w then Queue.enqueue q (y, x)
  in

  for x = 0 to w - 1 do
    push 0 x;
    push (h - 1) x
  done;
  for y = 0 to h - 1 do
    push y 0;
    push y (w - 1)
  done;

  while not (Queue.is_empty q) do
    let y, x = Queue.dequeue_exn q in
    if (not outside.(y).(x)) && (not boundary.(y).(x)) then begin
      outside.(y).(x) <- true;
      push (y + 1) x;
      push (y - 1) x;
      push y (x + 1);
      push y (x - 1);
    end
  done;
  outside
;;

let build_weighted_prefix_sum ~(cx:int array) ~(cy:int array) ~(outside:bool array array) =
  let h = Array.length outside in
  let w = Array.length outside.(0) in

  let dx = Array.init w ~f:(fun i -> cx.(i + 1) - cx.(i)) in
  let dy = Array.init h ~f:(fun j -> cy.(j + 1) - cy.(j)) in

  let ps = Array.make_matrix ~dimx:(h + 1) ~dimy:(w + 1) 0L in
  for y = 1 to h do
    let row_sum = ref 0L in
    for x = 1 to w do
      let allowed = not outside.(y - 1).(x - 1) in
      let cell =
        if allowed
        then
          Stdlib.Int64.mul
            (Stdlib.Int64.of_int dx.(x - 1))
            (Stdlib.Int64.of_int dy.(y - 1))
        else
          0L
      in
      row_sum := Stdlib.Int64.add !row_sum cell;
      ps.(y).(x) <- Stdlib.Int64.add ps.(y - 1).(x) !row_sum
    done
  done;
  ps
;;

let parse ?(verbose=false) (filename : string) : Uart_symbol.t list =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  let points =
    raw
    |> String.split_lines
    |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
    |> List.map ~f:(fun line ->
      match String.split line ~on:',' with
      | [ x; y ] -> Int.of_string (String.strip x), Int.of_string (String.strip y)
      | _ -> failwithf "Day09: bad line (expected x,y): %s" line ())
  in

  let n = List.length points in
  if n < 2 then failwith "Day09: need at least 2 points";

  let cx, cy, xid, yid = compress_coords points in
  let ps_w = Array.length cx in
  let ps_h = Array.length cy in

  (* Stream words = 4 + 3n + 2*(ps_w*ps_h). If it won’t fit RAM, fail immediately. *)
let required_words =
  let words =
    4
    + (3 * n)
    + (2 * ps_w * ps_h)
  in
  words
in
  if required_words > ram_depth_words then
    failwithf
      "Day09: stream too large for FPGA RAM.\n\
       required_words=%d (%.2f KiB) but ram_depth=%d (%.2f KiB)\n\
       Fix: change the protocol (don’t stream full u64 prefix-sum), or increase RAM depth for simulation-only."
      required_words
      (Float.of_int (required_words * 4) /. 1024.)
      ram_depth_words
      (Float.of_int (ram_depth_words * 4) /. 1024.)
      ();

  let w = ps_w - 1 in
  let h = ps_h - 1 in
  if w <= 0 || h <= 0 then failwith "Day09: degenerate compression";

  let boundary = Array.make_matrix ~dimx:h ~dimy:w false in
  mark_boundary_tiles ~boundary ~xid ~yid points;

  let outside = flood_fill_outside ~boundary in
  let ps = build_weighted_prefix_sum ~cx ~cy ~outside in

  (* Reverse-accumulator output (fast). *)
  let acc_rev : Uart_symbol.t list ref = ref [] in
  let emit (xs : Uart_symbol.t list) = acc_rev := List.rev_append xs !acc_rev in

  let magic = 0xD0090001 in
  emit (u32_to_uart_bytes_le magic);
  emit (u32_to_uart_bytes_le n);

  List.iteri points ~f:(fun i (x,y) ->
    let x_u32 = require_u32_nonneg x ~what:(sprintf "x[%d]" i) in
    let y_u32 = require_u32_nonneg y ~what:(sprintf "y[%d]" i) in
    let ix = Map.find_exn xid x in
    let iy = Map.find_exn yid y in
    require_u16 ix ~what:(sprintf "ix[%d]" i);
    require_u16 iy ~what:(sprintf "iy[%d]" i);
    emit (u32_to_uart_bytes_le x_u32);
    emit (u32_to_uart_bytes_le y_u32);
    emit (pack_u16x2_le ~lo:ix ~hi:iy);
  );

  emit (u32_to_uart_bytes_le ps_w);
  emit (u32_to_uart_bytes_le ps_h);

  for yy = 0 to ps_h - 1 do
    for xx = 0 to ps_w - 1 do
      emit (u64_to_uart_bytes_le ps.(yy).(xx))
    done
  done;

  List.rev !acc_rev
;;
