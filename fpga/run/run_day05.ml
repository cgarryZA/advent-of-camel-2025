open! Core
open Run_wrapper

let day = 5
module Design = Advent_of_caml.Day05
module Parser = Advent_of_caml_input_parser.Day05

let () =
  run_day
    ~day
    ~hierarchical:Design.hierarchical
    ~parser:Parser.parse
    ()
;;
