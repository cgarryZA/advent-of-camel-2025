open! Core
open! Util

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  (raw
   |> String.split_lines
   |> List.concat_map ~f:(fun line ->
        let digits =
          String.to_list line
          |> List.filter ~f:Char.is_digit
          |> List.map ~f:(fun c -> Uart_symbol.Byte c)
        in
        digits @ [ Uart_symbol.Byte '\n' ]))
  @ [ Uart_symbol.Rts true ]
;;
