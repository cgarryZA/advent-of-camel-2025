open! Core
open! Hardcaml
open! Advent_of_caml

let emit_verilog ~(name : string) (build : Scope.t -> Circuit.t) =
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit =
    let c = build scope in
    Circuit.with_name c ~name
  in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let generate_day1_rtl () =
  let module C = Circuit.With_interface (Day1.Engine.I) (Day1.Engine.O) in
  emit_verilog
    ~name:"Day1_top"
    (fun scope -> C.create_exn ~name:"Day1_top" (Day1.Engine.hierarchical scope))
;;

let day1_cmd =
  Command.basic
    ~summary:"Emit Verilog for AoC 2025 Day 1 dial"
    [%map_open.Command
      let () = return () in
      fun () -> generate_day1_rtl ()]
;;

let generate_day4_engine_rtl ~lanes ~rows ~cols =
  let module D4 =
    Day4.Engine.Make (struct
      let lanes = lanes
      let rows = rows
      let cols = cols
    end)
  in
  let module C = Circuit.With_interface (D4.I) (D4.O) in
  emit_verilog
    ~name:"Day4_engine_top"
    (fun scope -> C.create_exn ~name:"Day4_engine_top" (D4.hierarchical scope))
;;

let day4_engine_cmd =
  Command.basic
    ~summary:"Emit Verilog for AoC 2025 Day 4 engine"
    [%map_open.Command
      let lanes =
        flag "lanes" (optional_with_default 64 int)
          ~doc:"INT Packed bits per word"
      and rows =
        flag "rows" (required int)
          ~doc:"INT Grid rows"
      and cols =
        flag "cols" (required int)
          ~doc:"INT Grid cols"
      in
      fun () -> generate_day4_engine_rtl ~lanes ~rows ~cols]
;;

let generate_day4_stream_rtl ~lanes ~rows ~cols =
  let module D4s =
    Day4.Stream.Make (struct
      let lanes = lanes
      let rows = rows
      let cols = cols
    end)
  in
  let module C = Circuit.With_interface (D4s.I) (D4s.O) in
  emit_verilog
    ~name:"Day4_stream_top"
    (fun scope -> C.create_exn ~name:"Day4_stream_top" (D4s.hierarchical scope))
;;

let day4_stream_cmd =
  Command.basic
    ~summary:"Emit Verilog for AoC 2025 Day 4 stream top"
    [%map_open.Command
      let lanes =
        flag "lanes" (optional_with_default 64 int)
          ~doc:"INT Packed bits per word"
      and rows =
        flag "rows" (required int)
          ~doc:"INT Grid rows"
      and cols =
        flag "cols" (required int)
          ~doc:"INT Grid cols"
      in
      fun () -> generate_day4_stream_rtl ~lanes ~rows ~cols]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Generate RTL"
       [ "Day1", day1_cmd
       ; "Day4-engine", day4_engine_cmd
       ; "Day4-stream", day4_stream_cmd
       ])
;;
