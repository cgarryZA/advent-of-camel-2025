open! Core
open Run_wrapper

let day = 12
module Design = Advent_of_caml.Day12
module Parser = Advent_of_caml_input_parser.Day12

let () =
  run_day
    ~day
    ~hierarchical:Design.hierarchical
    ~parser:Parser.parse
    ()
;;
