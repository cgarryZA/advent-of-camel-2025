open! Core
open! Util

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  let u64_to_uart_words_le (x : int64) =
    let open Int64 in
    let lo = to_int_trunc (x land 0xFFFF_FFFFL) in
    let hi = to_int_trunc (shift_right_logical x 32) in
    int_to_uart_bytes_le ~n:4 lo
    @ int_to_uart_bytes_le ~n:4 hi
  in

  raw
  |> String.strip
  |> String.split ~on:','
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.concat_map ~f:(fun range ->
       match String.split range ~on:'-' with
       | [ lo; hi ] ->
           u64_to_uart_words_le (Int64.of_string lo)
           @ u64_to_uart_words_le (Int64.of_string hi)
       | _ ->
           failwithf "Invalid range format: %s" range ())
;;
