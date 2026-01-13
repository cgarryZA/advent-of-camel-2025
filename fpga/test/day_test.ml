(* test/day_test.ml *)
open! Core
open! Hardcaml

let run_sample
    ~day
    ~hierarchical
    ~parser
    ?(input = sprintf "sample%d.txt" day)
    ?(add_rts = true)
    ~cycles
    ()
  =
  let symbols =
    let xs = parser input in
    if add_rts
    then xs @ [ Advent_of_caml_input_parser.Util.Uart_symbol.Rts true ]
    else xs
  in

  Harness.run
    ~hierarchical
    ~input:symbols
    ~cycles
    ~vcd_file:(sprintf "/tmp/day%d.vcd" day)
    ()
;;
