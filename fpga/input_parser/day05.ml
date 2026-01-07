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

  let sections =
    raw
    |> String.split ~on:'\n'
    |> List.group ~break:(fun a b -> String.is_empty a || String.is_empty b)
    |> List.filter ~f:(fun grp -> not (List.for_all grp ~f:String.is_empty))
  in

  let ranges, items =
    match sections with
    | [ r; i ] -> (r, i)
    | _ -> failwith "Expected ranges, blank line, then items"
  in

  let parsed_ranges =
    ranges
    |> List.map ~f:(fun line ->
         match String.split line ~on:'-' with
         | [ lo; hi ] -> (Int64.of_string lo, Int64.of_string hi)
         | _ -> failwith ("Bad range: " ^ line))
    |> List.sort ~compare:(fun (lo1, hi1) (lo2, hi2) ->
         match Int64.compare lo1 lo2 with
         | 0 -> Int64.compare hi1 hi2
         | c -> c)
  in

  let parsed_items =
    items |> List.map ~f:Int64.of_string
  in

  let bytes =
    List.concat
      [ (* Header: still counts, but send as u64 because Loader expects u64 words *)
        int64_to_uart_bytes_le ~n:8 (Int64.of_int (List.length parsed_ranges))
      ; int64_to_uart_bytes_le ~n:8 (Int64.of_int (List.length parsed_items))

        (* Ranges *)
      ; List.concat_map parsed_ranges ~f:(fun (lo, hi) ->
            int64_to_uart_bytes_le ~n:8 lo @ int64_to_uart_bytes_le ~n:8 hi)

        (* Items *)
      ; List.concat_map parsed_items ~f:(fun x -> int64_to_uart_bytes_le ~n:8 x)
      ]
  in

  (* DO NOT append RTS here if Run_wrapper already appends it.
     If you keep it here, ensure Run_wrapper does not add a second RTS. *)
  bytes
;;
