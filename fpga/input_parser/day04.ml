open! Core
open! Util

(* ============================================================================ *)
(* Day 4 input parser                                                           *)
(* Pads arbitrary input to target rows/cols, THEN adds 1-cell border            *)
(* ============================================================================ *)

let lanes = 64

let target_rows = 135
let target_cols = 135

let dimensions _filename =
  (* Logical grid size seen by hardware (without border) *)
  target_rows, target_cols

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  let lines =
    raw
    |> String.split_lines
    |> List.filter ~f:(Fn.non String.is_empty)
  in

  let in_rows = List.length lines in
  let in_cols =
    match lines with
    | [] -> failwith "Empty grid"
    | l :: _ -> String.length l
  in

  if in_rows > target_rows || in_cols > target_cols then
    failwith "Input grid larger than target grid";

  (* -------------------------------------------------------------------------- *)
  (* Step 1: build padded logical grid (135x135)                                 *)
  (* -------------------------------------------------------------------------- *)

  let grid =
    Array.init target_rows ~f:(fun r ->
      Array.init target_cols ~f:(fun c ->
        if r < in_rows && c < in_cols then
          Char.equal (List.nth_exn lines r).[c] '@'
        else
          false))
  in

  (* -------------------------------------------------------------------------- *)
  (* Step 2: add 1-cell zero border                                              *)
  (* -------------------------------------------------------------------------- *)

  let ext_rows = target_rows + 2 in
  let ext_cols = target_cols + 2 in

  let ext =
    Array.init ext_rows ~f:(fun r ->
      Array.init ext_cols ~f:(fun c ->
        if r = 0 || c = 0 || r = ext_rows - 1 || c = ext_cols - 1 then
          false
        else
          grid.(r - 1).(c - 1)))
  in

  (* -------------------------------------------------------------------------- *)
  (* Step 3: pack into 64-bit words                                              *)
  (* -------------------------------------------------------------------------- *)

  let words_per_row = (ext_cols + lanes - 1) / lanes in

  let words =
    List.init ext_rows ~f:(fun r ->
      List.init words_per_row ~f:(fun w ->
        let acc = ref 0L in
        for b = 0 to lanes - 1 do
          let col = (w * lanes) + b in
          if col < ext_cols && ext.(r).(col) then
            acc := Int64.( !acc lor shift_left 1L b )
        done;
        !acc))
    |> List.concat
  in

  let total_words = List.length words in

  (* -------------------------------------------------------------------------- *)
  (* Step 4: emit stream words                                                   *)
  (* -------------------------------------------------------------------------- *)

  List.mapi words ~f:(fun i w ->
    Uart_symbol.Stream_word
      { data = w
      ; sof = (i = 0)
      ; last = (i = total_words - 1)
      })
;;
