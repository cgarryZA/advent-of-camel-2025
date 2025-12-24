open! Core

val parse : ?verbose:bool -> string -> Util.Uart_symbol.t list
val dimensions : string -> int * int
