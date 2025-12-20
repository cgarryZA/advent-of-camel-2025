# Advent of Code - Day 4: High-Throughput Hardware Solver

This project implements a hardware-accelerated solver for Advent of Code Day 4 using Hardcaml. It is designed to run on an FPGA or via high-performance RTL simulation, utilizing bit-sliced parallel processing to handle grid-based cellular automata.

## Architecture Overview

The solver uses a streaming, 2D-sliding window architecture to process grid bits in parallel. It is designed to find and remove specific patterns (cellular automata rules) until a steady state is reached.

### Key Features:
- Parallel Processing: Processes 32 bits (lanes) per clock cycle.
- Pipelined Windowing: Implements a 3x3 sliding window across packed data words using a stride-based delay line.
- Double Buffering: Utilizes two separate RAM banks (grid_mem and grid_next) to allow simultaneous read/write operations during state transitions.
- Bit-Sliced Logic: Uses a custom bit-sliced popcount and comparison tree to determine cell transitions for all 32 lanes in a single cycle.



## Data Layout & Padding

To simplify boundary conditions, the original 135x135 grid is transformed into an extended 137x137 grid (1-cell zero-padding on all sides).

- words_per_row: The number of packed 32-bit words needed to store one extended row.
- stride: Includes 2 additional "dummy" words per row to flush the horizontal pipeline registers, ensuring every real bit passes through the center of the 3x3 window.



## Hardware State Machine

The controller manages the lifecycle of the computation through five primary states:

1. Idle: Initial state, waits for the `start` signal. Handles memory loading.
2. Prime: Pre-fills the pipeline delay lines so that the first valid data reaches the processing window.
3. Scan: The main execution loop. It reads from the source RAM, applies the logic, writes to the destination RAM, and accumulates the "removed" count.
4. Check: Evaluates the total removals. If zero, the process is complete. If non-zero, it swaps the RAM roles (double buffering) and starts a new pass.
5. Finished: Asserts the `finished` signal and holds the results for Part 1 and Part 2.

## Performance (Tested on 135x135 Input)

## 16 Lanes
- Cycles: ~78,240
- Execution Time: ~0.144s
- Throughput:
- Throughput: 16 bits/cycle

## 32 Lanes
- Cycles: ~49,890
- Execution Time: ~0.096s (Simulation/Host overhead included)
- Throughput: 32 bits/cycle

## 64 Lanes
- Cycles: ~35,720
- Execution Time: ~0.074s
- Throughput: 64 bits/cycle

## Requirements

- OCaml / Opam
- Hardcaml
- Core
- Dune (Build system)

## Usage

To build the project:
$ dune build

To run the solver against your input:
$ dune exec -- bin/solve_day4.exe day4input.txt


Both 15 bits and updated adder
Part 1: 1389                        
Part 2: 9000
Cycles: 35716
Time  : 0.109s

32 bits and updated adder
Part 1: 1389                        
Part 2: 9000
Cycles: 35716
Time  : 0.107s