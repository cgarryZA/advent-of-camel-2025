open! Core
open! Hardcaml
open! Advent_of_caml

let generate_day1_dial_rtl () =
  let module C = Circuit.With_interface (Day1_dial.I) (Day1_dial.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"day1_dial_top" (Day1_dial.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let day1_dial_rtl_command =
  Command.basic
    ~summary:"Emit Verilog for AoC 2025 Day 1 dial"
    [%map_open.Command
      let () = return () in
      fun () -> generate_day1_dial_rtl ()]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Generate RTL"
       [  "day1-dial", day1_dial_rtl_command
       ])
;;
