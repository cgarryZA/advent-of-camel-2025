(* input_parser/day12.ml *)

open! Core
open! Util

let is_region_line (line : string) =
  (* Region lines look like "12x5: 1 0 1 0 2 2" *)
  String.is_substring line ~substring:"x"
  && String.is_substring line ~substring:":"
;;

let parse_region_line (line : string) : int * int =
  (* returns (blocks_available, blocks_required) *)
  let line = String.strip line in
  let dims, counts =
    match String.lsplit2 line ~on:':' with
    | Some (a, b) -> String.strip a, String.strip b
    | None -> failwith ("Bad region line (missing ':'): " ^ line)
  in
  let w, h =
    match String.lsplit2 dims ~on:'x' with
    | Some (a, b) -> Int.of_string (String.strip a), Int.of_string (String.strip b)
    | None -> failwith ("Bad dims (missing 'x'): " ^ dims)
  in
  let counts =
    counts
    |> String.split ~on:' '
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:Int.of_string
  in
  let blocks_available = (w / 3) * (h / 3) in
  let blocks_required = List.fold counts ~init:0 ~f:( + ) in
  blocks_available, blocks_required
;;

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  let lines = String.split_lines raw in
  let regions =
    lines
    |> List.filter ~f:(fun l -> not (String.is_empty (String.strip l)))
    |> List.filter ~f:is_region_line
    |> List.map ~f:parse_region_line
  in

  let num_regions = List.length regions in

  (* word0 = num_regions *)
  let out =
    int_to_uart_bytes_le ~n:4 num_regions
    @ (regions
       |> List.concat_map ~f:(fun (avail, req) ->
            int_to_uart_bytes_le ~n:4 avail
            @ int_to_uart_bytes_le ~n:4 req))
    @ [ Uart_symbol.Rts true ]
  in
  out
;;
