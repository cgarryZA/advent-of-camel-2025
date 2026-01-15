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

let int32_to_uart_bytes_le (x : int) =
  int_to_uart_bytes_le ~n:4 x
;;

(* Pack two u16 into one u32: low16 = lo, high16 = hi *)
let pack_u16x2_le ~(lo : int) ~(hi : int) : Uart_symbol.t list =
  let lo = lo land 0xFFFF in
  let hi = hi land 0xFFFF in
  int32_to_uart_bytes_le (lo lor (hi lsl 16))
;;

(* ------------------------------------------------------------ *)
(* Part 2 preprocessing                                         *)
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
  cx, cy, xid, yid
;;

let rasterize_boundary (pts : (int64 * int64) list) xid yid (w : int) (h : int) =
  let grid = Array.make_matrix ~dimx:h ~dimy:w 0 in
  let n = List.length pts in
  let pts = Array.of_list pts in

  for i = 0 to n - 1 do
    let x0, y0 = pts.(i) in
    let x1, y1 = pts.((i + 1) mod n) in
    let ix0 = Map.find_exn xid x0 in
    let iy0 = Map.find_exn yid y0 in
    let ix1 = Map.find_exn xid x1 in
    let iy1 = Map.find_exn yid y1 in

    if ix0 = ix1 then
      for y = Int.min iy0 iy1 to Int.max iy0 iy1 - 1 do
        grid.(y).(ix0) <- 1
      done
    else if iy0 = iy1 then
      for x = Int.min ix0 ix1 to Int.max ix0 ix1 - 1 do
        grid.(iy0).(x) <- 1
      done
    else
      failwith "Day09: non-axis-aligned edge"
  done;
  grid
;;

let flood_fill_outside (boundary : int array array) =
  let h = Array.length boundary in
  let w = Array.length boundary.(0) in
  let outside = Array.make_matrix ~dimx:h ~dimy:w false in
  let q = Queue.create () in

  let push y x =
    if y >= 0 && y < h && x >= 0 && x < w then
      Queue.enqueue q (y, x)
  in

  for x = 0 to w - 1 do
    push 0 x; push (h - 1) x
  done;
  for y = 0 to h - 1 do
    push y 0; push y (w - 1)
  done;

  while not (Queue.is_empty q) do
    let y, x = Queue.dequeue_exn q in
    if not outside.(y).(x) && boundary.(y).(x) = 0 then begin
      outside.(y).(x) <- true;
      push (y + 1) x;
      push (y - 1) x;
      push y (x + 1);
      push y (x - 1);
    end
  done;
  outside
;;

let build_prefix_sum_allowed (allowed : int array array) =
  let h = Array.length allowed in
  let w = Array.length allowed.(0) in
  let ps = Array.make_matrix ~dimx:(h + 1) ~dimy:(w + 1) 0 in

  for y = 1 to h do
    let row_sum = ref 0 in
    for x = 1 to w do
      row_sum := !row_sum + allowed.(y - 1).(x - 1);
      ps.(y).(x) <- ps.(y - 1).(x) + !row_sum
    done
  done;
  ps
;;

(* ------------------------------------------------------------ *)
(* Main parser                                                   *)
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

  (* ---------------- Part 1 stream ---------------- *)

  let part1_bytes =
    List.concat
      [ int64_to_uart_bytes_le ~n:8 (Int64.of_int n)
      ; List.concat_map points ~f:(fun (x, y) ->
          int64_to_uart_bytes_le ~n:8 x
          @ int64_to_uart_bytes_le ~n:8 y)
      ]
  in

  (* ---------------- Part 2 preprocessing ---------------- *)

  let _cx, _cy, xid, yid = compress_coords points in
  let w = Map.length xid in
  let h = Map.length yid in

  let boundary = rasterize_boundary points xid yid w h in
  let outside = flood_fill_outside boundary in

  let allowed =
    Array.init h ~f:(fun y ->
      Array.init w ~f:(fun x ->
        if outside.(y).(x) then 0 else 1))
  in

  let ps = build_prefix_sum_allowed allowed in

  let idx_bytes =
    points
    |> List.concat_map ~f:(fun (x, y) ->
      let xi = Map.find_exn xid x in
      let yi = Map.find_exn yid y in
      pack_u16x2_le ~lo:xi ~hi:yi)
  in

  (* ---------------- Part 2 stream ---------------- *)

  let part2_bytes =
    List.concat
      [ int32_to_uart_bytes_le w
      ; int32_to_uart_bytes_le h
      ; List.concat_map (List.range 0 (h + 1)) ~f:(fun yy ->
          List.concat_map (List.range 0 (w + 1)) ~f:(fun xx ->
            int32_to_uart_bytes_le ps.(yy).(xx)))
      ; int32_to_uart_bytes_le n
      ; idx_bytes
      ]
  in

  (* ---------------- Final UART stream ---------------- *)

  part1_bytes
  @ [ Uart_symbol.Rts true; Uart_symbol.Rts false ]
  @ part2_bytes
  @ [ Uart_symbol.Rts true ]
;;
