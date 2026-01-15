open! Core
open Run_wrapper

let day = 9
module Design = Advent_of_caml.Day09
module Parser = Advent_of_caml_input_parser.Day09

let () =
  run_day
    ~day
    ~hierarchical:Design.hierarchical
    ~parser:Parser.parse
    ~run_cycles:10_000_000
    ()
;;
