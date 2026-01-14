(* run/run_wrapper.ml *)

open! Core
open! Core_unix
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

let file_exists path =
  try Stdlib.Sys.file_exists path with
  | _ -> false
;;

let parse_or_die
    ~(day : int)
    ~(path : string)
    ~(parse :
        ?verbose:bool ->
        string ->
        Advent_of_caml_input_parser.Util.Uart_symbol.t list)
  =
  let alt_path =
    let dir = Filename.dirname path in
    let base = Filename.basename path in
    match String.chop_prefix base ~prefix:"input" with
    | None -> path
    | Some rest ->
        (match String.lsplit2 rest ~on:'.' with
         | None -> path
         | Some (d, ext) ->
             match Int.of_string_opt d with
             | None -> path
             | Some n ->
                 Filename.concat dir (sprintf "input%02d.%s" n ext))
  in

  let candidates =
    [ resolve_path path
    ; resolve_path alt_path
    ; path
    ; alt_path
    ; Filename.concat (Sys_unix.getcwd ()) path
    ; Filename.concat (Sys_unix.getcwd ()) alt_path
    ; Filename.concat (Sys_unix.getcwd ()) ("../" ^ path)
    ; Filename.concat (Sys_unix.getcwd ()) ("../" ^ alt_path)
    ; Filename.concat (Sys_unix.getcwd ()) ("../../" ^ path)
    ; Filename.concat (Sys_unix.getcwd ()) ("../../" ^ alt_path)
    ]
    |> List.dedup_and_sort ~compare:String.compare
  in

  match List.find candidates ~f:file_exists with
  | None ->
      eprintf
        "%sInput file not found.%s\n\n\
         Looked for:\n\
         %s\n\n\
         Download your input from:\n\
         %shttps://adventofcode.com/2025/day/%d/input%s\n\n\
         Then save it as (either name is accepted):\n\
         %sinputs/input%d.txt%s  or  %sinputs/input%02d.txt%s\n"
        Color.red Color.reset
        (candidates |> List.map ~f:(fun p -> "  - " ^ p) |> String.concat ~sep:"\n")
        Color.blue day Color.reset
        Color.blue day Color.reset Color.blue day Color.reset;
      exit 1
  | Some chosen_path ->
      let display_path =
        match dune_project_root () with
        | Some root when String.is_prefix chosen_path ~prefix:root ->
            String.drop_prefix chosen_path (String.length root + 1)
        | _ ->
            Filename.basename chosen_path
      in
      try
        let inputs =
          parse chosen_path
          @ [ Advent_of_caml_input_parser.Util.Uart_symbol.Rts true ]
        in
        printf
          "Running %sDay %d%s using %s%s%s\n%!"
          Color.green day Color.reset
          Color.blue display_path Color.reset;
        inputs
      with
      | exn ->
          eprintf
            "%sFailed to parse input file:%s %s\n\n%s\n"
            Color.red Color.reset chosen_path (Exn.to_string exn);
          raise exn
;;

let run_day
    ~(day : int)
    ~(hierarchical : Scope.t -> _)
    ~(parser :
        ?verbose:bool ->
        string ->
        Advent_of_caml_input_parser.Util.Uart_symbol.t list)
    ?(run_cycles = 500_000)
    ()
  =
  let input_path =
    sprintf "inputs/input%d.txt" day
  in

  let vcd_file =
    Some (sprintf "/tmp/day%02d_run.vcd" day)
  in

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
