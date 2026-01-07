open! Core
open Run_wrapper

let day = 5
module Design = Advent_of_caml.Day05
module Parser = Advent_of_caml_input_parser.Day05

let () =
  run_day
    ~day
    ~hierarchical:Design.hierarchical
    ~input_path:(sprintf "inputs/input%d.txt" day)
    ~parser:Parser.parse
    ~vcd_file:(Some (sprintf "/tmp/day%02d_run.vcd" day))
    ()
;;
