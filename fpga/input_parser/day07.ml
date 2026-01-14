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

let parse ?(verbose = false) (filename : string) : Uart_symbol.t list =
  let raw = get_input_file filename in

  raw
  |> String.iter ~f:(fun c ->
         if not (is_valid_char c) then
           failwithf "day07: invalid character '%c' in input" c ());

  let normalized = String.map raw ~f:normalize_char in

  let () =
    if verbose then begin
      let lines = String.split_lines normalized in
      let height = List.length lines in
      let width =
        match lines with
        | [] -> 0
        | hd :: _ -> String.length hd
      in
      printf "=== Day 7 input dump ===\n";
      printf "Grid size: height=%d width=%d\n\n" height width;
      List.iteri lines ~f:(fun r line ->
        printf "%4d | %s\n" r line);
      printf "\nLegend: '.' empty, '^' splitter\n";
      printf "========================\n\n";
    end
  in

  (normalized
   |> String.to_list
   |> List.map ~f:(fun c -> Uart_symbol.Byte c))
  @ [ Uart_symbol.Rts true ]
;;
