open! Core
open Run_wrapper

let day = 11
module Design = Advent_of_caml.Day11
module Parser = Advent_of_caml_input_parser.Day11

let () =
  run_day
    ~day
    ~hierarchical:Design.hierarchical
    ~parser:Parser.parse
    ()
;;
