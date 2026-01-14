(* input_parser/day07.ml *)

open! Core
open! Util

let is_valid_char = function
  | '.' | '^' | 'S' | '\n' -> true
  | _ -> false
;;

let normalize_char = function
  | 'S' -> '.'
  | c -> c
;;

let find_start_col_exn (raw : string) : int =
  let lines = String.split_lines raw in
  let all_s =
    lines
    |> List.concat_mapi ~f:(fun r line ->
         String.to_list line
         |> List.filter_mapi ~f:(fun c ch -> if Char.(ch = 'S') then Some (r, c) else None))
  in
  match all_s with
  | [ (_r, c) ] -> c
  | [] -> failwith "day07: expected exactly one 'S' but found none"
  | _ -> failwith "day07: expected exactly one 'S' but found multiple"
;;

let u16_le_bytes (x : int) : Uart_symbol.t list =
  let lo = x land 0xFF in
  let hi = (x lsr 8) land 0xFF in
  [ Uart_symbol.Byte (Char.of_int_exn lo)
  ; Uart_symbol.Byte (Char.of_int_exn hi)
  ]
;;

let parse ?(verbose = false) (filename : string) : Uart_symbol.t list =
  let raw = get_input_file filename in

  raw
  |> String.iter ~f:(fun c ->
         if not (is_valid_char c) then
           failwithf "day07: invalid character '%c' in input" c ());

  let start_col = find_start_col_exn raw in
  let normalized = String.map raw ~f:normalize_char in

  let lines = String.split_lines normalized in
  let height = List.length lines in
  let width =
    match lines with
    | [] -> 0
    | hd :: _ -> String.length hd
  in

  let () =
    if verbose then begin
      printf "=== Day 7 input dump ===\n";
      printf "Grid size: height=%d width=%d\n" height width;
      printf "start_col (0-indexed): %d\n\n" start_col;
      List.iteri lines ~f:(fun r line ->
        printf "%4d | %s\n" r line);
      printf "\nLegend: '.' empty, '^' splitter\n";
      printf "========================\n\n";
    end
  in

  (* Stream format:
     - 2 bytes: start_col as u16 little-endian
     - 2 bytes: width as u16 little-endian
     - 2 bytes: height as u16 little-endian
     - then normalized grid bytes (includes '\n' characters)
     - RTS true
  *)
  (u16_le_bytes start_col)
  @ (u16_le_bytes width)
  @ (u16_le_bytes height)
  @ (normalized |> String.to_list |> List.map ~f:(fun c -> Uart_symbol.Byte c))
  @ [ Uart_symbol.Rts true ]
;;
