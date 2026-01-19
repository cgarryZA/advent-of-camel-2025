open! Core
open Run_wrapper

let () =
  let day =
    match Sys.get_argv () with
    | [| _; day_str |] ->
      (try Int.of_string day_str with
      | _ ->
        eprintf "Error: Invalid day number '%s'\n" day_str;
        eprintf "Usage: run_days <day_number>\n";
        eprintf "  where day_number is 1-12\n";
        exit 1)
    | _ ->
      eprintf "Usage: run_days <day_number>\n";
      eprintf "  where day_number is 1-12\n";
      exit 1
  in

  if day < 1 || day > 12 then (
    eprintf "Error: Day must be between 1 and 12, got %d\n" day;
    exit 1
  );

  (* Get the hierarchical and parser for the requested day *)
  let hierarchical, parser, run_cycles_opt =
    match day with
    | 1 -> Advent_of_caml.Day01.hierarchical, Advent_of_caml_input_parser.Day01.parse, Some 5_000
    | 2 -> Advent_of_caml.Day02.hierarchical, Advent_of_caml_input_parser.Day02.parse, Some 3_000_000
    | 3 -> Advent_of_caml.Day03.hierarchical, Advent_of_caml_input_parser.Day03.parse, Some 100
    | 4 -> Advent_of_caml.Day04.hierarchical, Advent_of_caml_input_parser.Day04.parse, Some 40_000 
    | 5 -> Advent_of_caml.Day05.hierarchical, Advent_of_caml_input_parser.Day05.parse, Some 100_000
    | 6 -> Advent_of_caml.Day06.hierarchical, Advent_of_caml_input_parser.Day06.parse, Some 50_000
    | 7 -> Advent_of_caml.Day07.hierarchical, Advent_of_caml_input_parser.Day07.parse, None
    | 8 -> Advent_of_caml.Day08.hierarchical, Advent_of_caml_input_parser.Day08.parse, Some 100
    | 9 -> Advent_of_caml.Day09.hierarchical, Advent_of_caml_input_parser.Day09.parse, Some 3_750_000
    | 10 -> Advent_of_caml.Day10.hierarchical, Advent_of_caml_input_parser.Day10.parse, Some 200_000
    | 11 -> Advent_of_caml.Day11.hierarchical, Advent_of_caml_input_parser.Day11.parse, Some 50_000
    | 12 -> Advent_of_caml.Day12.hierarchical, Advent_of_caml_input_parser.Day12.parse, Some 2_500
    | _ -> assert false
  in

  match run_cycles_opt with
  | None -> run_day ~day ~hierarchical ~parser ()
  | Some run_cycles -> run_day ~day ~hierarchical ~parser ~run_cycles ()
;;
