(* input_parser/day07.ml *)

open! Core
open! Util

let lanes = 8

let dimensions (_ : string) =
  0, 0   (* inferred dynamically by hardware *)

let parse ?(verbose=false) filename =
  let raw = Util.get_input_file filename in
  if verbose then print_endline raw;

  (* ensure final newline so Row_flush triggers *)
  let raw =
    if String.is_suffix raw ~suffix:"\n" then raw else raw ^ "\n"
  in

  raw
  |> String.to_list
  |> List.map ~f:(fun ch -> Util.Uart_symbol.Byte ch)
;;
