(* src/day_stub.ml *)
open! Core
open! Hardcaml
open! Signal

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 1
let extra_synth_args = []

let hierarchical _scope (_i : Signal.t Ulx3s.I.t) : Signal.t Ulx3s.O.t =
  { leds = zero 8
  ; uart_tx = { valid = gnd; value = zero 8 }
  ; uart_rx_ready = vdd
  }
;;
