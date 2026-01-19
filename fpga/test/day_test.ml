(* test/day_test.ml *)
open! Core
open! Hardcaml

let run_sample
    ~day
    ~hierarchical
    ~parser
    ?(input = sprintf "sample%d.txt" day)
    ?(add_rts = true)
    ~cycles
    ()
  =
  let symbols =
    let xs = parser input in
    if add_rts
    then xs @ [ Advent_of_caml_input_parser.Util.Uart_symbol.Rts true ]
    else xs
  in

  Harness.run
    ~hierarchical
    ~input:symbols
    ~cycles
    ~vcd_file:(sprintf "/tmp/day%d.vcd" day)
    ()
;;

(* Helper to create standard day tests with minimal boilerplate *)
let make_day_test
    ~day
    ?(input = sprintf "sample%d.txt" day)
    ?(add_rts = true)
    ?(cycles = 100_000)
    ()
  =
  (* Dynamically load the hierarchical and parser modules
  let day_module_name = sprintf "Day%02d" day in *)
  let hierarchical =
    match day with
    | 1 -> Advent_of_caml.Day01.hierarchical
    | 2 -> Advent_of_caml.Day02.hierarchical
    | 3 -> Advent_of_caml.Day03.hierarchical
    | 4 -> Advent_of_caml.Day04.hierarchical
    | 5 -> Advent_of_caml.Day05.hierarchical
    | 6 -> Advent_of_caml.Day06.hierarchical
    | 7 -> Advent_of_caml.Day07.hierarchical
    | 8 -> Advent_of_caml.Day08.hierarchical
    | 9 -> Advent_of_caml.Day09.hierarchical
    | 10 -> Advent_of_caml.Day10.hierarchical
    | 11 -> Advent_of_caml.Day11.hierarchical
    | 12 -> Advent_of_caml.Day12.hierarchical
    | _ -> failwith (sprintf "Unknown day: %d" day)
  in
  let parser =
    match day with
    | 1 -> Advent_of_caml_input_parser.Day01.parse
    | 2 -> Advent_of_caml_input_parser.Day02.parse
    | 3 -> Advent_of_caml_input_parser.Day03.parse
    | 4 -> Advent_of_caml_input_parser.Day04.parse
    | 5 -> Advent_of_caml_input_parser.Day05.parse
    | 6 -> Advent_of_caml_input_parser.Day06.parse
    | 7 -> Advent_of_caml_input_parser.Day07.parse
    | 8 -> Advent_of_caml_input_parser.Day08.parse
    | 9 -> Advent_of_caml_input_parser.Day09.parse
    | 10 -> Advent_of_caml_input_parser.Day10.parse
    | 11 -> Advent_of_caml_input_parser.Day11.parse
    | 12 -> Advent_of_caml_input_parser.Day12.parse
    | _ -> failwith (sprintf "Unknown day: %d" day)
  in
  run_sample ~day ~hierarchical ~parser ~input ~add_rts ~cycles () |> print_endline
;;
