open! Core
open! Hardcaml

type t =
  { sim : Sim_env.t
  ; buffer : char Queue.t
  }

let create ~sim =
  { sim; buffer = Queue.create () }
;;

let poll t =
  let o = Sim_env.outputs t.sim in
  if Bits.to_bool !(o.uart_tx.valid) then
    Queue.enqueue t.buffer (Bits.to_char !(o.uart_tx.value))
;;

let contents t =
  Queue.to_list t.buffer |> String.of_char_list
;;

let clear t =
  Queue.clear t.buffer
;;
