open! Core
open! Hardcaml

module Ulx3s = Advent_of_caml.Ulx3s
module Sim = Cyclesim.With_interface (Ulx3s.I) (Ulx3s.O)

type t =
  { sim : Sim.t
  }

let create
    ~hierarchical
    ?(vcd_file = "/tmp/sim.vcd")
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

  let oc = Out_channel.create vcd_file in
  let sim = Hardcaml.Vcd.wrap oc sim in
  Core.at_exit (fun () -> Out_channel.close oc);

  let i = Cyclesim.inputs sim in
  i.uart_rx.valid := Bits.gnd;
  i.uart_rts := Bits.gnd;
  i.uart_rx_overflow := Bits.gnd;
  i.uart_tx_ready := Bits.vdd;

  i.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clear := Bits.gnd;
  Cyclesim.cycle sim;

  { sim }
;;

let cycle t =
  Cyclesim.cycle t.sim
;;

let inputs t =
  Cyclesim.inputs t.sim
;;

let outputs t =
  Cyclesim.outputs t.sim
;;

let read_memory t name =
  Cyclesim.lookup_mem_by_name t.sim name
  |> Option.value_exn
  |> Cyclesim.Memory.read_all
;;

let read_memory_int t name =
  read_memory t name |> Array.map ~f:Bits.to_int_trunc
;;
