(* test/test_wrapper.ml *)

open! Core
open! Hardcaml

module Ulx3s = Advent_of_caml.Ulx3s
module Sim = Cyclesim.With_interface (Ulx3s.I) (Ulx3s.O)

type t =
  { sim : Sim.t
  ; recv_buffer : char Queue.t
  ; cycles_between_bytes : int
  }

let create
    ~hierarchical
    ?(vcd_file = "/tmp/day04.vcd")
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

  (* ================= VCD WRAP ================= *)
  let oc = Out_channel.create vcd_file in
  let sim = Hardcaml.Vcd.wrap oc sim in
  Core.at_exit (fun () -> Out_channel.close oc);
  (* ============================================ *)

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

  (* Wait until DUT says it accepted RX byte *)
  while not (Bits.to_bool !(o.uart_rx_ready)) do
    cycle t
  done;

  (* One extra cycle to complete handshake *)
  cycle t;

  i.uart_rx.valid := Bits.gnd;

  if gap then cycle ~n:t.cycles_between_bytes t
;;

let write_word64 t (w : Bits.t) =
  (* Your packer makes the FIRST byte sent become bits [63:56],
     so send MSB byte first. *)
  for k = 7 downto 0 do
    let hi = (k * 8) + 7 in
    let lo = k * 8 in
    let b = Bits.select w ~high:hi ~low:lo |> Bits.to_char in
    write_byte ~gap:false t b
  done;
  (* optional spacing once per 64-bit word *)
  cycle ~n:t.cycles_between_bytes t
;;

(* IMPORTANT: keep the existential payload OUT of the match arm.
   We convert it here, after the match has returned a plain value. *)
let write_stream_word64 (t : t) (w : 'a) : unit =
  let bits : Bits.t = Obj.magic w in
  write_word64 t bits
;;

let set_rts t rts =
  let i = Cyclesim.inputs t.sim in
  i.uart_rts := Bits.of_bool rts;
  cycle t
;;

let handle_uart_symbol t sym =
  match sym with
  | Advent_of_caml_input_parser.Util.Uart_symbol.Byte b ->
      write_byte t b

  | Advent_of_caml_input_parser.Util.Uart_symbol.Rts v ->
      set_rts t v

  | Advent_of_caml_input_parser.Util.Uart_symbol.Stream_word { data; _ } ->
      write_word64 t (Bits.of_int64_trunc ~width:64 data)
;;

let feed_inputs t uart_symbols =
  List.iter uart_symbols ~f:(handle_uart_symbol t)
;;

let get_uart_output t =
  Queue.to_list t.recv_buffer |> String.of_char_list
;;

let dump_uart_output t =
  print_endline (get_uart_output t)
;;

let read_memory t name =
  Cyclesim.lookup_mem_by_name t.sim name
  |> Option.value_exn
  |> Cyclesim.Memory.read_all
;;

let read_memory_int t name =
  read_memory t name |> Array.map ~f:Bits.to_int_trunc
;;
