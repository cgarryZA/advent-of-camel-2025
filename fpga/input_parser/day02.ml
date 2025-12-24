open! Core
open! Util

let parse ?(verbose=false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;
  (String.to_list (String.strip raw) |> List.map ~f:(fun c -> Uart_symbol.Byte c))
  @ [ Uart_symbol.Rts true ]
;;
