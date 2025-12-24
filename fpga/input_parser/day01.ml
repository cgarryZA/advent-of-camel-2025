open! Core
open! Util

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  (raw
   |> String.split_lines
   |> List.filter ~f:(Fn.non String.is_empty)
   |> List.concat_map ~f:(fun line ->
        let dir = if Char.(line.[0] = 'L') then 0 else 1 in
        let steps =
          Int.of_string (String.sub line ~pos:1 ~len:(String.length line - 1))
        in
        (* CRITICAL: send 4 bytes for dir, not 1, so RAM layout is:
           word 0 = dir (u32)
           word 1 = steps (u32)
           repeated...
        *)
        int_to_uart_bytes_le ~n:4 dir @ int_to_uart_bytes_le ~n:4 steps))
  @ [ Uart_symbol.Rts true ]
;;
