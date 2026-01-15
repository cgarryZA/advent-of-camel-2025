open! Core
open Run_wrapper

let day = 7
module Design = Advent_of_caml.Day07
module Parser = Advent_of_caml_input_parser.Day07

let () =
  run_day
    ~day
    ~hierarchical:Design.hierarchical
    ~parser:Parser.parse
    ()
;;
