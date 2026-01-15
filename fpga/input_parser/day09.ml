(* input_parser/day09.ml *)

open! Core
open! Util

(* ------------------------------------------------------------ *)
(* Helpers                                                      *)
(* ------------------------------------------------------------ *)

let int64_to_uart_bytes_le ~(n : int) (x : int64) =
  List.init n ~f:(fun i ->
    let shift = i * 8 in
    let byte =
      Int64.(to_int_exn ((shift_right_logical x shift) land 0xffL))
    in
    Uart_symbol.Byte (Char.of_int_exn byte))
;;

let u32_to_uart_bytes_le (x : int) =
  (* x must fit in unsigned 32-bit, but OCaml int is fine for that range *)
  int_to_uart_bytes_le ~n:4 x
;;

(* Pack two u16 into one u32: low16 = lo, high16 = hi *)
let pack_u16x2_le ~(lo : int) ~(hi : int) : Uart_symbol.t list =
  let lo = lo land 0xFFFF in
  let hi = hi land 0xFFFF in
  u32_to_uart_bytes_le (lo lor (hi lsl 16))
;;

let require_u32_nonneg (v : int64) ~(what : string) : int =
  if Int64.(v < 0L) then
    failwithf "Day09: %s is negative (%Ld)" what v ()
  else if Int64.(v > 0xFFFF_FFFFL) then
    failwithf "Day09: %s does not fit u32 (%Ld)" what v ()
  else
    Int64.to_int_exn v
;;

let require_u16 (v : int) ~(what : string) =
  if v < 0 || v > 0xFFFF then
    failwithf "Day09: %s does not fit u16 (%d)" what v ()
;;

(* ------------------------------------------------------------ *)
(* Coordinate compression for tile-grid + weighted prefix sum    *)
(* ------------------------------------------------------------ *)

let compress_coords (pts : (int64 * int64) list) =
  let xs =
    pts
    |> List.map ~f:fst
    |> List.concat_map ~f:(fun x -> Int64.[ x; x + 1L ])
  in
  let ys =
    pts
    |> List.map ~f:snd
    |> List.concat_map ~f:(fun y -> Int64.[ y; y + 1L ])
  in
  let cx = xs |> List.dedup_and_sort ~compare:Int64.compare in
  let cy = ys |> List.dedup_and_sort ~compare:Int64.compare in

  let xid =
    Map.of_alist_exn
      (module Int64)
      (List.mapi cx ~f:(fun i v -> v, i))
  in
  let yid =
    Map.of_alist_exn
      (module Int64)
      (List.mapi cy ~f:(fun i v -> v, i))
  in
  Array.of_list cx, Array.of_list cy, xid, yid
;;

let mark_boundary_tiles
    ~(boundary : bool array array)
    ~(xid : int Map.M(Int64).t)
    ~(yid : int Map.M(Int64).t)
    (pts : (int64 * int64) list)
  =
  let n = List.length pts in
  let pts = Array.of_list pts in

  let mark y x =
    if y >= 0
       && y < Array.length boundary
       && x >= 0
       && x < Array.length boundary.(0)
    then boundary.(y).(x) <- true
  in

  for i = 0 to n - 1 do
    let x0, y0 = pts.(i) in
    let x1, y1 = pts.((i + 1) mod n) in

    if Int64.(x0 = x1) then begin
      (* Vertical segment: tiles at x0, y in [min..max] inclusive *)
      let x_idx = Map.find_exn xid x0 in
      let y_lo = Int64.min y0 y1 in
      let y_hi = Int64.max y0 y1 in
      let y0i = Map.find_exn yid y_lo in
      let y1p1 = Map.find_exn yid Int64.(y_hi + 1L) in
      for y = y0i to y1p1 - 1 do
        mark y x_idx
      done
    end else if Int64.(y0 = y1) then begin
      (* Horizontal segment: tiles at y0, x in [min..max] inclusive *)
      let y_idx = Map.find_exn yid y0 in
      let x_lo = Int64.min x0 x1 in
      let x_hi = Int64.max x0 x1 in
      let x0i = Map.find_exn xid x_lo in
      let x1p1 = Map.find_exn xid Int64.(x_hi + 1L) in
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
    if y >= 0 && y < h && x >= 0 && x < w then
      Queue.enqueue q (y, x)
  in

  (* Seed with border cells *)
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

let build_weighted_prefix_sum
    ~(cx : int64 array)
    ~(cy : int64 array)
    ~(outside : bool array array)
  =
  let h = Array.length outside in
  let w = Array.length outside.(0) in

  (* cell widths/heights in tiles (since coords are integer tile positions) *)
  let dx =
    Array.init w ~f:(fun i ->
      Int64.(cx.(i + 1) - cx.(i)))
  in

  let dy =
    Array.init h ~f:(fun j ->
      Int64.(cy.(j + 1) - cy.(j)))
  in

  let ps = Array.make_matrix ~dimx:(h + 1) ~dimy:(w + 1) 0L in

  for y = 1 to h do
    let row_sum = ref 0L in
    for x = 1 to w do
      let allowed =
        (* Not outside => inside or boundary => allowed *)
        not outside.(y - 1).(x - 1)
      in
      let cell =
        if allowed
        then Int64.(dx.(x - 1) * dy.(y - 1))
        else 0L
      in
      row_sum := Int64.(!row_sum + cell);
      ps.(y).(x) <- Int64.(ps.(y - 1).(x) + !row_sum)
    done
  done;
  ps
;;

(* ------------------------------------------------------------ *)
(* Main parser                                                   *)
(* Stream format (all little-endian u32/u64, final RTS only):
   word0: 0xD0090001
   word1: n
   for i=0..n-1:
     word: x_i (u32)
     word: y_i (u32)
     word: (ix_i | (iy_i<<16))  where ix/iy are indices into cx/cy (line indices)
   word: ps_w  (= length cx)    (u32)
   word: ps_h  (= length cy)    (u32)
   then ps_h * ps_w entries of u64, row-major: ps[y][x]
   final: RTS true
*)
(* ------------------------------------------------------------ *)

let parse ?(verbose = false) (filename : string) : Uart_symbol.t list =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  let points =
    raw
    |> String.split_lines
    |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
    |> List.map ~f:(fun line ->
      match String.split line ~on:',' with
      | [ x; y ] ->
        Int64.of_string (String.strip x),
        Int64.of_string (String.strip y)
      | _ ->
        failwithf "Day09: bad line (expected x,y): %s" line ())
  in

  let n = List.length points in
  if n < 2 then failwith "Day09: need at least 2 points";

  let cx, cy, xid, yid = compress_coords points in
  let w = Array.length cx - 1 in
  let h = Array.length cy - 1 in
  if w <= 0 || h <= 0 then failwith "Day09: degenerate compression";

  let boundary = Array.make_matrix ~dimx:h ~dimy:w false in
  mark_boundary_tiles ~boundary ~xid ~yid points;

  let outside = flood_fill_outside ~boundary in
  let ps = build_weighted_prefix_sum ~cx ~cy ~outside in

  (* Emit header *)
  let magic = 0xD0090001 in
  let out =
    ref
      (u32_to_uart_bytes_le magic
       @ u32_to_uart_bytes_le n)
  in

  (* Emit points + packed indices *)
  List.iteri points ~f:(fun i (x, y) ->
    let x_u32 = require_u32_nonneg x ~what:(sprintf "x[%d]" i) in
    let y_u32 = require_u32_nonneg y ~what:(sprintf "y[%d]" i) in
    let ix = Map.find_exn xid x in
    let iy = Map.find_exn yid y in
    require_u16 ix ~what:(sprintf "ix[%d]" i);
    require_u16 iy ~what:(sprintf "iy[%d]" i);
    out :=
      !out
      @ u32_to_uart_bytes_le x_u32
      @ u32_to_uart_bytes_le y_u32
      @ pack_u16x2_le ~lo:ix ~hi:iy);

  (* Emit ps dimensions (ps_w = len(cx), ps_h = len(cy)) *)
  let ps_w = Array.length cx in
  let ps_h = Array.length cy in
  out := !out @ u32_to_uart_bytes_le ps_w @ u32_to_uart_bytes_le ps_h;

  (* Emit ps as u64 entries, row-major *)
  for yy = 0 to ps_h - 1 do
    for xx = 0 to ps_w - 1 do
      out := !out @ int64_to_uart_bytes_le ~n:8 ps.(yy).(xx)
    done
  done;

  !out @ [ Uart_symbol.Rts true ]
;;
