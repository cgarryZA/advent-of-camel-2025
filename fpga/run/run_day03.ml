open! Core
open Run_wrapper

let day = 3
module Design = Advent_of_caml.Day03
module Parser = Advent_of_caml_input_parser.Day03

let () =
  run_day
    ~day
    ~hierarchical:Design.hierarchical
    ~parser:Parser.parse
    ()
;;
