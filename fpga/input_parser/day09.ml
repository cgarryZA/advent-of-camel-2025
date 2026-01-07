(* input_parser/day09.ml *)

open! Core
open! Util

let int64_to_uart_bytes_le ~(n : int) (x : int64) =
  List.init n ~f:(fun i ->
    let shift = i * 8 in
    let byte =
      Int64.(to_int_exn ((shift_right_logical x shift) land 0xffL))
    in
    Uart_symbol.Byte (Char.of_int_exn byte))
;;

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  let points =
    raw
    |> String.split_lines
    |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
    |> List.map ~f:(fun line ->
         match String.split line ~on:',' with
         | [ x; y ] ->
           let x = Int64.of_string (String.strip x) in
           let y = Int64.of_string (String.strip y) in
           x, y
         | _ ->
           failwithf "Bad line (expected x,y): %s" line ())
  in

  let n = List.length points in

  let bytes =
    List.concat
      [ int64_to_uart_bytes_le ~n:8 (Int64.of_int n)
      ; List.concat_map points ~f:(fun (x, y) ->
            int64_to_uart_bytes_le ~n:8 x
          @ int64_to_uart_bytes_le ~n:8 y)
      ]
  in

  bytes @ [ Uart_symbol.Rts true ]
;;
