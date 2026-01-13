open! Core
open Run_wrapper

let day = 1
module Design = Advent_of_caml.Day01
module Parser = Advent_of_caml_input_parser.Day01

let () =
  run_day
    ~day
    ~hierarchical:Design.hierarchical
    ~parser:Parser.parse
    ()
;;
