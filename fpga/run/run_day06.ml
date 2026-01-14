open! Core
open Run_wrapper

let day = 6
module Design = Advent_of_caml.Day06
module Parser = Advent_of_caml_input_parser.Day06

let () =
  run_day
    ~day
    ~hierarchical:Design.hierarchical
    ~parser:Parser.parse
    ()
;;
