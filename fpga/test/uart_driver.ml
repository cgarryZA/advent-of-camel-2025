open! Core
open! Hardcaml

open Advent_of_caml_input_parser.Util

type t =
  { sim : Sim_env.t
  ; cycles_between_bytes : int
  }

let create ~sim ?(cycles_between_bytes = 30) () =
  { sim; cycles_between_bytes }
;;

let rec wait_for_ready t =
  let o = Sim_env.outputs t.sim in
  if not (Bits.to_bool !(o.uart_rx_ready)) then begin
    Sim_env.cycle t.sim;
    wait_for_ready t
  end
;;

let write_byte ?(gap = true) t (byte : char) =
  let i = Sim_env.inputs t.sim in
  i.uart_rx.value := Bits.of_char byte;
  i.uart_rx.valid := Bits.vdd;

  wait_for_ready t;
  Sim_env.cycle t.sim;

  i.uart_rx.valid := Bits.gnd;

  if gap then
    for _ = 1 to t.cycles_between_bytes do
      Sim_env.cycle t.sim
    done
;;

let write_word64 t (w : Bits.t) =
  for k = 7 downto 0 do
    let hi = (k * 8) + 7 in
    let lo = k * 8 in
    let b = Bits.select w ~high:hi ~low:lo |> Bits.to_char in
    write_byte ~gap:false t b
  done;

  for _ = 1 to t.cycles_between_bytes do
    Sim_env.cycle t.sim
  done
;;

let set_rts t v =
  let i = Sim_env.inputs t.sim in
  i.uart_rts := Bits.of_bool v;
  Sim_env.cycle t.sim
;;

let handle_symbol t = function
  | Uart_symbol.Byte b ->
      write_byte t b
  | Uart_symbol.Rts v ->
      set_rts t v
  | Uart_symbol.Stream_word { data; _ } ->
      write_word64 t (Bits.of_int64_trunc ~width:64 data)
;;

let send t symbols =
  List.iter symbols ~f:(handle_symbol t)
;;
