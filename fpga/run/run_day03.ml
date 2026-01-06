open! Core
open Run_wrapper

module Day03 = Advent_of_caml.Day03

let () =
  let inputs =
    parse_or_die
      ~day:3
      ~path:"inputs/input3.txt"
      Advent_of_caml_input_parser.Day03.parse
  in

  let sim =
    create
      ~hierarchical:Day03.hierarchical
      ~vcd_file:(Some "/tmp/day03_run.vcd")
      ()
  in

  feed_inputs sim inputs;
  cycle ~n:500_000 sim;
  dump_uart_output sim
;;
