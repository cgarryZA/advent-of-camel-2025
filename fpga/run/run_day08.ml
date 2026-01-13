open! Core
open Run_wrapper

let day = 8
module Design = Advent_of_caml.Day08
module Parser = Advent_of_caml_input_parser.Day08

let () =
  run_day
    ~day
    ~hierarchical:Design.hierarchical
    ~parser:Parser.parse
    ()
;;
