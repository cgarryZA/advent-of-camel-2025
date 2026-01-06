open Hardcaml

val clock_freq : Ulx3s.Clock_freq.t
val uart_fifo_depth : int
val extra_synth_args : string list

val hierarchical
  :  Scope.t
  -> Signal.t Ulx3s.I.t
  -> Signal.t Ulx3s.O.t

module Params : sig
  type t =
    { lanes : int
    ; rows : int
    ; cols : int
    }
end

module type DESIGN = sig
  module I : Hardcaml.Interface.S
  module O : Hardcaml.Interface.S
  val hierarchical : Hardcaml.Scope.t -> Hardcaml.Signal.t I.t -> Hardcaml.Signal.t O.t
end

val design : Params.t -> (module DESIGN)
