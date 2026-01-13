(* input_parser/day08.ml *)

open! Core
open! Util

let parse ?(verbose = false) (filename : string) : Uart_symbol.t list =
  let raw = get_input_file filename in
  if verbose then begin
    print_endline "==== RAW INPUT ====";
    print_endline raw;
  end;

  let points =
    raw
    |> String.split_lines
    |> List.filter_map ~f:(fun line ->
      let line = String.strip line in
      if String.is_empty line then None
      else
        let line = String.rstrip line ~drop:(Char.equal '\r') in
        match String.split line ~on:',' with
        | [ x; y; z ] ->
            let x = String.strip x in
            let y = String.strip y in
            let z = String.strip z in
            Some (Int.of_string x, Int.of_string y, Int.of_string z)
        | _ ->
            failwithf "Invalid line (expected x,y,z): %s" line ())
  in

  let n = List.length points in
  if n <> 20 then
    failwithf "Day08 FPGA design currently expects exactly 20 points, but input has %d lines" n ();

  if verbose then begin
    printf "Parsed %d points\n" n;
    List.iteri points ~f:(fun i (x, y, z) ->
      printf "  %02d: x=%d y=%d z=%d\n" i x y z);
  end;

  let xs = Array.of_list (List.map points ~f:(fun (x, _, _) -> x)) in

  (* Build all edges *)
  let edges =
    List.concat_mapi points ~f:(fun i (xi, yi, zi) ->
      List.mapi points ~f:(fun j (xj, yj, zj) ->
        if j <= i then None
        else
          let dx = xi - xj in
          let dy = yi - yj in
          let dz = zi - zj in
          let d2 = (dx * dx) + (dy * dy) + (dz * dz) in
          Some (d2, i, j))
      |> List.filter_opt)
  in

  (* Canonical global sort: (d2, u, v) *)
  let edges =
    List.sort edges ~compare:(fun (d2a, ia, ja) (d2b, ib, jb) ->
      match Int.compare d2a d2b with
      | 0 ->
          (match Int.compare ia ib with
           | 0 -> Int.compare ja jb
           | c -> c)
      | c -> c)
  in

  if verbose then begin
    printf "Built %d candidate edges\n" (List.length edges);
    print_endline "==== SORTED EDGES (d2, u, v) ====";
    List.iter edges ~f:(fun (d2, u, v) ->
      printf "  %12d : (%d,%d)\n" d2 u v);
  end;

  (* 1) xs preload (32-bit little-endian words) *)
  let xs_uart =
    Array.to_list xs
    |> List.concat_map ~f:(fun x ->
      int_to_uart_bytes_le ~n:4 x)
  in

  (* 2) edge stream (u,v bytes) *)
  let edges_uart =
    edges
    |> List.concat_map ~f:(fun (_d2, u, v) ->
      [ Uart_symbol.Byte (Char.of_int_exn u)
      ; Uart_symbol.Byte (Char.of_int_exn v)
      ])
  in

  xs_uart @ edges_uart
;;
