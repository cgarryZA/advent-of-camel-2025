open! Core
open! Util

(* Day 6: Trash Compactor
   Emit TWO streams back-to-back:

   Stream A (Part 1 - horizontal numbers):
     mode=0
     repeat: op(u8), cnt(u8), cnt*(u16 LE)
     delimiter: op=0, cnt=0

   Stream B (Part 2 - vertical numbers):
     mode=1
     repeat: op(u8), cnt(u8), cnt*(u16 LE)
     delimiter: op=0, cnt=0

   End signaled by [Rts true].
*)

let pad_to_word_boundary (syms : Util.Uart_symbol.t list)
  : Util.Uart_symbol.t list =
  let byte_count =
    List.count syms ~f:(function
      | Util.Uart_symbol.Byte _ -> true
      | Util.Uart_symbol.Rts _ -> false
      | Util.Uart_symbol.Stream_word _ -> false)
  in
  let rem = byte_count mod 4 in
  if rem = 0 then syms
  else
    let pad = 4 - rem in
    syms @ List.init pad ~f:(fun _ -> Util.Uart_symbol.Byte (Char.of_int_exn 0))
;;

let char_is_op = function
  | '+' | '*' -> true
  | _ -> false
;;

let op_to_u8 = function
  | '+' -> 0
  | '*' -> 1
  | c -> failwithf "Invalid op char: %c" c ()
;;

let u8_to_uart (x : int) : Util.Uart_symbol.t list =
  if x < 0 || x > 255 then failwithf "u8 out of range: %d" x ();
  [ Util.Uart_symbol.Byte (Char.of_int_exn x) ]
;;

let u16_to_uart (x : int) : Util.Uart_symbol.t list =
  if x < 0 || x > 0xFFFF then failwithf "u16 out of range: %d" x ();
  Util.int_to_uart_bytes_le ~n:2 x
;;

let pad_right (s : string) ~(len : int) : string =
  if String.length s >= len then s else s ^ String.make (len - String.length s) ' '
;;

(* Find contiguous "problem spans" as ranges of columns [lo..hi] where NOT every row is space.
   A separator column is a column where ALL rows have ' '.
*)
let find_spans (rows : string array) : (int * int) list =
  let n_rows = Array.length rows in
  if n_rows = 0 then []
  else (
    let width = String.length rows.(0) in
    let is_gap_col col =
      let rec loop r =
        if r = n_rows then true
        else if Char.(rows.(r).[col] = ' ') then loop (r + 1)
        else false
      in
      loop 0
    in
    let spans = ref [] in
    let c = ref 0 in
    while !c < width do
      while !c < width && is_gap_col !c do
        incr c
      done;
      if !c < width then (
        let lo = !c in
        while !c < width && not (is_gap_col !c) do
          incr c
        done;
        let hi = !c - 1 in
        spans := (lo, hi) :: !spans)
    done;
    List.rev !spans)
;;

let first_op_in_span (ops_row : string) ~(lo : int) ~(hi : int) : char option =
  let rec loop i =
    if i > hi then None
    else
      let c = ops_row.[i] in
      if char_is_op c then Some c else loop (i + 1)
  in
  loop lo
;;

(* Part 1 number: parse the horizontal substring [lo..hi] of a row *)
let int_in_span (row : string) ~(lo : int) ~(hi : int) : int option =
  let seg = String.sub row ~pos:lo ~len:(hi - lo + 1) |> String.strip in
  if String.is_empty seg then None
  else Some (Int.of_string seg)
;;

(* Part 2 number: parse vertically in a fixed column x by scanning down the digit rows *)
let int_in_column (digit_rows : string array) ~(x : int) : int option =
  let acc = ref 0 in
  let seen = ref false in
  Array.iter digit_rows ~f:(fun r ->
    let c = r.[x] in
    if Char.is_digit c then (
      seen := true;
      acc := (!acc * 10) + (Char.to_int c - Char.to_int '0')
    ) else if Char.(c = ' ') then
      ()
    else
      failwithf "Unexpected char '%c' in vertical digit parse" c ());
  if !seen then Some !acc else None
;;

let emit_delim () : Util.Uart_symbol.t list =
  (* delimiter is read as: op byte (dummy) then cnt=0 *)
  u8_to_uart 0 @ u8_to_uart 0
;;

let parse ?(verbose=false) (filename : string) : Util.Uart_symbol.t list =
  ignore verbose;

  let raw = Util.get_input_file filename in
  let lines =
    raw
    |> String.split_lines
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  if List.length lines < 2 then
    failwith "Day06: expected at least 2 non-empty lines (numbers + ops)";

  let ops_line = List.last_exn lines in
  let num_lines = List.drop_last_exn lines in

  let max_width =
    List.fold lines ~init:0 ~f:(fun acc s -> Int.max acc (String.length s))
  in

  let ops_row = pad_right ops_line ~len:max_width in
  let digit_rows =
    Array.of_list (List.map num_lines ~f:(fun s -> pad_right s ~len:max_width))
  in

  (* spans are based on the full grid including the ops row *)
  let full_rows =
    Array.of_list (List.map lines ~f:(fun s -> pad_right s ~len:max_width))
  in
  let spans = find_spans full_rows in

  let out = ref [] in

  (* -------- Part 1 stream (horizontal numbers) -------- *)
  out := !out @ u8_to_uart 0;
  List.iter spans ~f:(fun (lo, hi) ->
    match first_op_in_span ops_row ~lo ~hi with
    | None -> ()
    | Some op ->
      let nums =
        Array.filter_map digit_rows ~f:(fun r -> int_in_span r ~lo ~hi)
        |> Array.to_list
      in
      if not (List.is_empty nums) then (
        out := !out @ u8_to_uart (op_to_u8 op);
        out := !out @ u8_to_uart (List.length nums);
        List.iter nums ~f:(fun n -> out := !out @ u16_to_uart n)));

  out := !out @ emit_delim ();

  (* -------- Part 2 stream (vertical numbers) -------- *)
  out := !out @ u8_to_uart 1;
  List.iter spans ~f:(fun (lo, hi) ->
    match first_op_in_span ops_row ~lo ~hi with
    | None -> ()
    | Some op ->
      let nums =
        List.range lo (hi + 1)
        |> List.filter_map ~f:(fun x -> int_in_column digit_rows ~x)
      in
      if not (List.is_empty nums) then (
        out := !out @ u8_to_uart (op_to_u8 op);
        out := !out @ u8_to_uart (List.length nums);
        List.iter nums ~f:(fun n -> out := !out @ u16_to_uart n)));

  out := !out @ emit_delim ();

  pad_to_word_boundary !out @ [ Util.Uart_symbol.Rts true ]
;;
