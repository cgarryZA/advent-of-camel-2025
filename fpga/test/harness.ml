open! Core
open! Hardcaml

let run
    ~hierarchical
    ~input
    ?(cycles = 500_000)
    ?(vcd_file = "/tmp/run.vcd")
    ()
  =
  let sim =
    Sim_env.create
      ~hierarchical
      ~vcd_file
      ()
  in

  let uart_rx = Uart_driver.create ~sim () in
  let uart_tx = Uart_sink.create ~sim in

  Uart_driver.send uart_rx input;

  for _ = 1 to cycles do
    Sim_env.cycle sim;
    Uart_sink.poll uart_tx
  done;

  Uart_sink.contents uart_tx
;;
