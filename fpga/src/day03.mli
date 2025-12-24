open Hardcaml

val clock_freq : Ulx3s.Clock_freq.t
val uart_fifo_depth : int
val extra_synth_args : string list

val hierarchical
  :  Scope.t
  -> Signal.t Ulx3s.I.t
  -> Signal.t Ulx3s.O.t
