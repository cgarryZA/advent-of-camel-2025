open! Core
open Run_wrapper

let day = 2
module Design = Advent_of_caml.Day02
module Parser = Advent_of_caml_input_parser.Day02

let () =
  run_day
    ~day
    ~hierarchical:Design.hierarchical
    ~parser:Parser.parse
    ~run_cycles:40_000_000
    ()
;;
