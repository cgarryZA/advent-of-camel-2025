(* input_parser/day09.ml *)

open! Core
open! Util

(* ------------------------------------------------------------ *)
(* Helpers                                                      *)
(* ------------------------------------------------------------ *)

(* Avoid Int64 operator-scope entirely; use Stdlib.Int64.* to prevent operator
   resolution/shadowing issues under Core/Util opens. *)
let i64_add (a : int64) (b : int64) : int64 = Stdlib.Int64.add a b
let i64_sub (a : int64) (b : int64) : int64 = Stdlib.Int64.sub a b
let i64_mul (a : int64) (b : int64) : int64 = Stdlib.Int64.mul a b
let i64_min (a : int64) (b : int64) : int64 = if Stdlib.Int64.compare a b <= 0 then a else b
let i64_max (a : int64) (b : int64) : int64 = if Stdlib.Int64.compare a b <= 0 then b else a
let i64_lt  (a : int64) (b : int64) : bool  = Stdlib.Int64.compare a b < 0
let i64_gt  (a : int64) (b : int64) : bool  = Stdlib.Int64.compare a b > 0

let int64_to_uart_bytes_le ~(n : int) (x : int64) : Uart_symbol.t list =
  List.init n ~f:(fun i ->
    let shift = i * 8 in
    let shifted = Stdlib.Int64.shift_right_logical x shift in
    let byte = Stdlib.Int64.logand shifted 0xffL |> Stdlib.Int64.to_int in
    Uart_symbol.Byte (Char.of_int_exn byte))
;;

let u32_to_uart_bytes_le (x : int) : Uart_symbol.t list =
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
  if i64_lt v 0L then
    failwithf "Day09: %s is negative (%Ld)" what v ()
  else if i64_gt v 0xFFFF_FFFFL then
    failwithf "Day09: %s does not fit u32 (%Ld)" what v ()
  else
    Stdlib.Int64.to_int v
;;

let require_u16 (v : int) ~(what : string) =
  if v < 0 || v > 0xFFFF then
    failwithf "Day09: %s does not fit u16 (%d)" what v ()
;;

let int64_to_nonneg_int_exn ~(what : string) (v : int64) : int =
  if i64_lt v 0L then failwithf "Day09: %s is negative (%Ld)" what v () ;
  (* also keep it sane for array sizes; you can widen later if needed *)
  if i64_gt v (Stdlib.Int64.of_int Int.max_value) then
    failwithf "Day09: %s too large for int (%Ld)" what v () ;
  Stdlib.Int64.to_int v
;;

(* ------------------------------------------------------------ *)
(* Coordinate compression for tile-grid + weighted prefix sum    *)
(* ------------------------------------------------------------ *)

let compress_coords (pts : (int64 * int64) list) =
  let xs =
    pts
    |> List.map ~f:fst
    |> List.concat_map ~f:(fun x -> [ x; i64_add x 1L ])
  in
  let ys =
    pts
    |> List.map ~f:snd
    |> List.concat_map ~f:(fun y -> [ y; i64_add y 1L ])
  in
  let cx = xs |> List.dedup_and_sort ~compare:Stdlib.Int64.compare in
  let cy = ys |> List.dedup_and_sort ~compare:Stdlib.Int64.compare in

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

    if Stdlib.Int64.compare x0 x1 = 0 then begin
      (* Vertical segment *)
      let x_idx = Map.find_exn xid x0 in
      let y_lo = i64_min y0 y1 in
      let y_hi = i64_max y0 y1 in
      let y0i = Map.find_exn yid y_lo in
      let y1p1 = Map.find_exn yid (i64_add y_hi 1L) in
      for y = y0i to y1p1 - 1 do
        mark y x_idx
      done
    end else if Stdlib.Int64.compare y0 y1 = 0 then begin
      (* Horizontal segment *)
      let y_idx = Map.find_exn yid y0 in
      let x_lo = i64_min x0 x1 in
      let x_hi = i64_max x0 x1 in
      let x0i = Map.find_exn xid x_lo in
      let x1p1 = Map.find_exn xid (i64_add x_hi 1L) in
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

  (* Keep dx/dy as int (to reduce int64 usage) but compute via Stdlib.Int64.sub safely. *)
  let dx : int array =
    Array.init w ~f:(fun i ->
      let d = i64_sub cx.(i + 1) cx.(i) in
      int64_to_nonneg_int_exn ~what:(sprintf "dx[%d]" i) d)
  in

  let dy : int array =
    Array.init h ~f:(fun j ->
      let d = i64_sub cy.(j + 1) cy.(j) in
      int64_to_nonneg_int_exn ~what:(sprintf "dy[%d]" j) d)
  in

  let ps = Array.make_matrix ~dimx:(h + 1) ~dimy:(w + 1) 0L in

  for y = 1 to h do
    let row_sum = ref 0L in
    for x = 1 to w do
      let allowed = not outside.(y - 1).(x - 1) in
      let cell =
        if allowed
        then
          (* cell area = dx * dy, promoted to int64 *)
          i64_mul (Stdlib.Int64.of_int dx.(x - 1)) (Stdlib.Int64.of_int dy.(y - 1))
        else
          0L
      in
      row_sum := i64_add !row_sum cell;
      ps.(y).(x) <- i64_add ps.(y - 1).(x) !row_sum
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
        Stdlib.Int64.of_string (String.strip x),
        Stdlib.Int64.of_string (String.strip y)
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
