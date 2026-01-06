open! Core
open Run_wrapper

module Day04 = Advent_of_caml.Day04

let () =
  let inputs =
    parse_or_die
      ~day:4
      ~path:"inputs/input4.txt"
      Advent_of_caml_input_parser.Day04.parse
  in

  let sim =
    create
      ~hierarchical:Day04.hierarchical
      ~vcd_file:(Some "/tmp/day04_run.vcd")
      ()
  in

  feed_inputs sim inputs;
  cycle ~n:500_000 sim;
  dump_uart_output sim
;;
