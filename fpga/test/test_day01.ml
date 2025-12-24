(* test/test_day01.ml *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm

open Advent_of_caml.Day01
module Ulx3s = Advent_of_caml.Ulx3s
open Test_wrapper

let decode_pairs ~words =
  let rec go acc = function
    | dir :: steps :: tl -> go ((dir, steps) :: acc) tl
    | _ -> List.rev acc
  in
  go [] words
;;

let%expect_test "day01 end-to-end" =
  let sim = create ~hierarchical ~vcd_file:"/tmp/day01.vcd" () in

  let inputs =
    Advent_of_caml_input_parser.Day01.parse "sample1.txt"
  in

  (* Count loaded instructions from the UART symbol stream:
     each instruction is 8 bytes = 8 Byte symbols *)
  let byte_count =
    List.count inputs ~f:(function
      | Advent_of_caml_input_parser.Util.Uart_symbol.Byte _ -> true
      | Advent_of_caml_input_parser.Util.Uart_symbol.Rts _ -> false
      | Advent_of_caml_input_parser.Util.Uart_symbol.Stream_word _ -> false)
      
  in
  let instr_count = byte_count / 8 in
  let word_count = instr_count * 2 in

  feed_inputs sim inputs;

  let ram = read_memory_int sim "ram" in
  let slice_words = Array.slice ram 0 (min (Array.length ram) word_count) |> Array.to_list in

  print_s [%message "Loaded byte_count" (byte_count : int)];
  print_s [%message "Loaded instr_count" (instr_count : int)];
  print_s [%message "Loaded word_count" (word_count : int)];

  print_s [%message "RAM[0..loaded_words)" (slice_words : int list)];

  print_s
    [%message
      "Decoded loaded (dir,steps)"
      ~_:(decode_pairs ~words:slice_words : (int * int) list)];

  cycle ~n:100_000 sim;
  dump_uart_output sim;

  [%expect {|
    ("Loaded byte_count" (byte_count 80))
    ("Loaded instr_count" (instr_count 10))
    ("Loaded word_count" (word_count 20))
    ("RAM[0..loaded_words)"
     (slice_words (0 68 0 30 1 48 0 5 1 60 0 55 0 1 0 99 1 14 0 82)))
    ("Decoded loaded (dir,steps)"
     ((0 68) (0 30) (1 48) (0 5) (1 60) (0 55) (0 1) (0 99) (1 14) (0 82)))
    Part 1: 3
    Part 2: 6
    |}]
;;
