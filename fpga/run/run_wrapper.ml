(* run/run_wrapper.ml *)

open! Core
open! Hardcaml

module Ulx3s = Advent_of_caml.Ulx3s
module Sim = Cyclesim.With_interface (Ulx3s.I) (Ulx3s.O)

(* ---------- Colours ---------- *)
module Color = struct
  let reset  = "\027[0m"
  let red    = "\027[31m"
  let green  = "\027[32m"
  let blue   = "\027[34m"
end

type t =
  { sim : Sim.t
  ; recv_buffer : char Queue.t
  ; cycles_between_bytes : int
  }

let create
    ~hierarchical
    ?(vcd_file : string option = None)
    ?(cycles_between_bytes = 30)
    ()
  =
  let scope =
    Scope.create
      ~flatten_design:true
      ~auto_label_hierarchical_ports:true
      ()
  in

  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (hierarchical scope)
  in

  let sim =
    match vcd_file with
    | None -> sim
    | Some file ->
        let oc = Out_channel.create file in
        let sim = Hardcaml.Vcd.wrap oc sim in
        Core.at_exit (fun () -> Out_channel.close oc);
        sim
  in

  let i = Cyclesim.inputs sim in
  i.uart_rx.valid := Bits.gnd;
  i.uart_rts := Bits.gnd;
  i.uart_rx_overflow := Bits.gnd;
  i.uart_tx_ready := Bits.vdd;

  (* reset *)
  i.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clear := Bits.gnd;
  Cyclesim.cycle sim;

  { sim; recv_buffer = Queue.create (); cycles_between_bytes }
;;

let cycle ?(n = 1) { sim; recv_buffer; _ } =
  let o = Cyclesim.outputs sim in
  for _ = 1 to n do
    Cyclesim.cycle sim;
    if Bits.to_bool !(o.uart_tx.valid) then
      Queue.enqueue recv_buffer (Bits.to_char !(o.uart_tx.value))
  done
;;

let write_byte ?(gap = true) t byte =
  let i = Cyclesim.inputs t.sim in
  let o = Cyclesim.outputs t.sim in
  i.uart_rx.value := Bits.of_char byte;
  i.uart_rx.valid := Bits.vdd;

  while not (Bits.to_bool !(o.uart_rx_ready)) do
    cycle t
  done;

  cycle t;
  i.uart_rx.valid := Bits.gnd;

  if gap then cycle ~n:t.cycles_between_bytes t
;;

let write_word64 t (w : Bits.t) =
  for k = 7 downto 0 do
    let hi = (k * 8) + 7 in
    let lo = k * 8 in
    let b = Bits.select w ~high:hi ~low:lo |> Bits.to_char in
    write_byte ~gap:false t b
  done;
  cycle ~n:t.cycles_between_bytes t
;;

let set_rts t v =
  let i = Cyclesim.inputs t.sim in
  i.uart_rts := Bits.of_bool v;
  cycle t
;;

let handle_uart_symbol t = function
  | Advent_of_caml_input_parser.Util.Uart_symbol.Byte b ->
      write_byte t b
  | Advent_of_caml_input_parser.Util.Uart_symbol.Rts v ->
      set_rts t v
  | Advent_of_caml_input_parser.Util.Uart_symbol.Stream_word { data; _ } ->
      write_word64 t (Bits.of_int64_trunc ~width:64 data)
;;

let feed_inputs t xs =
  List.iter xs ~f:(handle_uart_symbol t)
;;

let get_uart_output t =
  Queue.to_list t.recv_buffer |> String.of_char_list
;;

let dune_project_root () =
  Sys.getenv "DUNE_PROJECT_ROOT"
;;

let resolve_path path =
  if Filename.is_relative path then
    match dune_project_root () with
    | Some root -> Filename.concat root path
    | None -> path
  else
    path
;;

let parse_or_die
    ~(day : int)
    ~(path : string)
    ~(parse :
        ?verbose:bool ->
        string ->
        Advent_of_caml_input_parser.Util.Uart_symbol.t list)
  =
  let full_path = resolve_path path in
  try
    let inputs =
      parse full_path
      @ [ Advent_of_caml_input_parser.Util.Uart_symbol.Rts true ]
    in
    printf
      "Running %sDay %d%s found at %shttps://adventofcode.com/2025/day/%d%s\n%!"
      Color.green day Color.reset
      Color.blue day Color.reset;
    inputs
  with
  | _ ->
      eprintf
        "%sInput file not found.\n\n\
         Download your input from:\n\
         %shttps://adventofcode.com/2025/day/%d/input%s\n\n\
         Then save it as:\n\
         %s%s%s\n%s"
        Color.red
        Color.blue day Color.reset
        Color.blue path Color.reset
        Color.reset;
      exit 1
;;

let run_day
    ~(day : int)
    ~(hierarchical : Scope.t -> _)
    ~(input_path : string)
    ~(parser :
        ?verbose:bool ->
        string ->
        Advent_of_caml_input_parser.Util.Uart_symbol.t list)
    ?(vcd_file : string option = None)
    ?(run_cycles = 500_000)
    ()
  =
  let inputs =
    parse_or_die
      ~day
      ~path:input_path
      ~parse:parser
  in

  let sim =
    create
      ~hierarchical
      ~vcd_file
      ()
  in

  feed_inputs sim inputs;
  cycle ~n:run_cycles sim;

  (* Colour only the numeric results *)
  let output = get_uart_output sim in
  let coloured =
    output
    |> String.split_lines
    |> List.map ~f:(fun line ->
         if String.is_prefix line ~prefix:"Part " then
           match String.lsplit2 line ~on:':' with
           | Some (lhs, rhs) ->
               sprintf "%s:%s %s%s%s"
                 lhs
                 Color.reset
                 Color.green
                 (String.strip rhs)
                 Color.reset
           | None -> line
         else
           line)
    |> String.concat ~sep:"\n"
  in

  print_endline coloured
;;
