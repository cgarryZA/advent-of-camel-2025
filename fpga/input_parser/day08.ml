(* input_parser/day08.ml *)

open! Core
open! Util

let parse ?(verbose = false) (filename : string) : Uart_symbol.t list =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  (* Each line is: x,y,z *)
  let coords =
    raw
    |> String.split_lines
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:(fun line ->
      match String.split line ~on:',' with
      | [ x; y; z ] ->
        ( Int.of_string x
        , Int.of_string y
        , Int.of_string z
        )
      | _ ->
        failwithf "Invalid line (expected x,y,z): %s" line ())
  in

  (* Encode as a flat stream of 32-bit unsigned ints, little-endian *)
  let uart_bytes =
    coords
    |> List.concat_map ~f:(fun (x, y, z) ->
      List.concat
        [ int_to_uart_bytes_le ~n:32 x
        ; int_to_uart_bytes_le ~n:32 y
        ; int_to_uart_bytes_le ~n:32 z
        ])
  in

  uart_bytes @ [ Uart_symbol.Rts true ]
;;
