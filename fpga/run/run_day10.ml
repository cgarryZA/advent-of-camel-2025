open! Core
open Run_wrapper

let day = 10
module Design = Advent_of_caml.Day10
module Parser = Advent_of_caml_input_parser.Day10

let () =
  run_day
    ~day
    ~hierarchical:Design.hierarchical
    ~parser:Parser.parse
    ()
;;
