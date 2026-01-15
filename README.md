<div align="center">
  
  <img src="https://raw.githubusercontent.com/cgarryZA/advent-of-camel-2025/main/snowglobe_camel.png"
       alt="Advent of Camel logo"
       width="450"/>


<a name="readme-top"></a>

# Advent of Hardcaml 2025 — Christian Garry 

A hardware-first exploration of Advent of Code 2025, implementing each puzzle as a **cycle-accurate FPGA design** in **Hardcaml**, using streaming UART I/O, explicit memory pipelines, and finite-state schedulers instead of software control flow.

  <a href="#solutions"><img src="https://img.shields.io/badge/Solutions-View-1f6feb?style=for-the-badge"></a>
  <a href="fpga/src"><img src="https://img.shields.io/badge/Code-Browse-f85149?style=for-the-badge"></a>
  <a href="https://adventofcode.com/2025"><img src="https://img.shields.io/badge/AoC_2025-Problems-facc15?style=for-the-badge"></a>
  <a href="#execution-model"><img src="https://img.shields.io/badge/Design-Streaming_UART_FPGA-22c55e?style=for-the-badge"></a>

</div>

## Introduction

This repository contains synthesizable FPGA implementations of selected [Advent of Code 2025](https://adventofcode.com/) problems, targeting streaming execution on the ULX3S platform for the [Advent of FPGA Challenge](https://blog.janestreet.com/advent-of-fpga-challenge-2025/).

This project is based on the [Jane Street Hardcaml Template Project](https://github.com/janestreet/hardcaml_template_project/tree/with-extensions), with the structure and UART infrastructure rebuilt and updated from the [2024 Advent of Hardcaml](https://github.com/asinghani/advent-of-hardcaml-2024/) designs to match current versions of Hardcaml and dune.

I've started with AoC days that had solutions that seemed like they would map well to hardware implementations and will see how many I can finish before the deadline.

## Setup & Installation

### Prerequisites

This project uses **Hardcaml** and the Jane Street OCaml toolchain.

You will need:
- A Unix-like environment (Linux, macOS, or WSL recommended)
- `git`
- `opam`

### 1. Clone the repository
```bash
git clone https://github.com/cgarryZA/advent-of-camel-2025.git
cd advent-of-camel-2025
```


### 2. Set up the OCaml / Hardcaml environment

This project follows the environment and tooling conventions of the Jane Street Hardcaml Template Project.

Please follow the official setup instructions here to install the compiler, create the correct opam switch, and install Hardcaml and its dependencies:

👉 [Jane Street Hardcaml Template Project](https://github.com/janestreet/hardcaml_template_project/tree/with-extensions)

Once the environment is set up, verify that the correct compiler is active:

```bash
opam switch show
ocamlc -version
```

Once the correct opam switch is active, you can build and run the project as described below.

---

## Using the repo

```bash
cd fpga
dune runtest
```

This runs the testbench for each implemented day against the sample input provided in the Advent of Code problem description.  
Sample inputs live in:

`/inputs/sampleX.txt`

where `X` is the day number with no leading zeros.

```bash
cd fpga
make run-all
make run-01
```

These commands run all days (or a single day) against the full input from the Advent of Code website.

The AoC rules ban redistribution of input files, so they are `.gitignore`d and must be downloaded manually.  
If you attempt to run a day without its input present, the runner will print the correct download link and tell you exactly where to save it:

`https://adventofcode.com/2025/day/X/input`  
`/inputs/inputX.txt`

---

<a name="execution-model"></a>
## Execution Model

All designs process their inputs as UART streams and produce their outputs over UART.  
Each solution follows a deterministic **load–compute–report** structure: input data is received and prepared for processing, the problem-specific algorithm is executed in hardware, and the final results for part 1 and part 2 are formatted as decimal values and transmitted once computation completes.

---

<a name="solutions"></a>
## Solutions

<table>
<tr><th>Day</th><th>Navigate</th></tr>

<tr>
<td><b>Day 1</b></td>
<td>
<a href="#day-1"><img src="https://img.shields.io/badge/Read-Section-1f6feb"></a>
<a href="fpga/src/day01.ml"><img src="https://img.shields.io/badge/Code-day01.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/1"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</td>
</tr>

<tr>
<td><b>Day 2</b></td>
<td>
<a href="#day-2"><img src="https://img.shields.io/badge/Read-Section-1f6feb"></a>
<a href="fpga/src/day02.ml"><img src="https://img.shields.io/badge/Code-day02.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/2"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</td>
</tr>

<tr>
<td><b>Day 3</b></td>
<td>
<a href="#day-3"><img src="https://img.shields.io/badge/Read-Section-1f6feb"></a>
<a href="fpga/src/day03.ml"><img src="https://img.shields.io/badge/Code-day03.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/3"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</td>
</tr>

<tr>
<td><b>Day 4</b></td>
<td>
<a href="#day-4"><img src="https://img.shields.io/badge/Read-Section-1f6feb"></a>
<a href="fpga/src/day04.ml"><img src="https://img.shields.io/badge/Code-day04.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/4"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</td>
</tr>

<tr>
<td><b>Day 5</b></td>
<td>
<a href="#day-5"><img src="https://img.shields.io/badge/Read-Section-1f6feb"></a>
<a href="fpga/src/day05.ml"><img src="https://img.shields.io/badge/Code-day05.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/5"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</td>
</tr>

<tr>
<td><b>Day 6</b></td>
<td>
<a href="#day-6"><img src="https://img.shields.io/badge/Read-Section-1f6feb"></a>
<a href="fpga/src/day06.ml"><img src="https://img.shields.io/badge/Code-day06.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/6"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</td>
</tr>

<tr>
<td><b>Day 8</b></td>
<td>
<a href="#day-8"><img src="https://img.shields.io/badge/Read-Section-1f6feb"></a>
<a href="fpga/src/day08.ml"><img src="https://img.shields.io/badge/Code-day08.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/8"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</td>
</tr>

<tr>
<td><b>Day 9</b></td>
<td>
<a href="#day-9"><img src="https://img.shields.io/badge/Read-Section-1f6feb"></a>
<a href="fpga/src/day09.ml"><img src="https://img.shields.io/badge/Code-day09.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/9"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</td>
</tr>

<tr>
<td><b>Day 10</b></td>
<td>
<a href="#day-10"><img src="https://img.shields.io/badge/Read-Section-1f6feb"></a>
<a href="fpga/src/day10.ml"><img src="https://img.shields.io/badge/Code-day10.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/10"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</td>
</tr>

<tr>
<td><b>Day 11</b></td>
<td>
<a href="#day-11"><img src="https://img.shields.io/badge/Read-Section-1f6feb"></a>
<a href="fpga/src/day11.ml"><img src="https://img.shields.io/badge/Code-day11.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/11"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</td>
</tr>
</table>

---

<a name="day-1"></a>
### Day 1, Part 1 + Part 2

<p>
<a href="fpga/src/day01.ml"><img src="https://img.shields.io/badge/Code-day01.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/1"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design simulates the behaviour of a combination lock driven by a sequence of direction-and-step instructions.

Each instruction updates the lock position within the range \([0, 99]\) using modular arithmetic to account for wraparound. For part 1, the design counts how many instructions result in the lock position landing exactly on zero after the update.

For part 2, the design analytically determines how many times the lock passes through zero while executing each instruction. This is done by computing the distance to the first arrival at zero and then counting subsequent arrivals at fixed intervals, without iterating over individual steps.

Instructions are processed sequentially at a rate of one per cycle, and the per-instruction contributions are accumulated to produce the final results.

---

<a name="day-2"></a>
### Day 2, Part 1 + Part 2

<p>
<a href="fpga/src/day02.ml"><img src="https://img.shields.io/badge/Code-day02.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/2"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design evaluates numeric ranges stored in external memory and identifies values that satisfy digit-pattern constraints, using a fully streaming binary-to-BCD datapath.

Input ranges are loaded into on-chip RAM as pairs of 64-bit lower and upper bounds. Each range is processed independently. The lower bound is first converted from binary to BCD using a repeated divide-by-10 datapath; this conversion is performed once per range. Subsequent values are generated by incrementing the BCD representation directly, allowing the design to advance one candidate ID per cycle.

For part 1, the circuit checks whether an ID consists of exactly two identical halves (e.g. ABAB). For part 2, the design generalises this check to any repeated substring occurring at least twice. These checks are performed combinationally over the BCD digits using parallel substring comparisons.

The design avoids per-digit iteration in the main loop. All candidates are evaluated in a single pass, with one ID tested per cycle. Valid IDs contribute their numeric value to the corresponding accumulator. The final sums for part 1 and part 2 are emitted once all ranges have been processed.

---

<a name="day-3"></a>
### Day 3, Part 1 + Part 2

<p>
<a href="fpga/src/day03.ml"><img src="https://img.shields.io/badge/Code-day03.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/3"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design processes each input line as a stream of ASCII digits and computes one result per line for both parts of the problem.

As digits are received, the circuit maintains running candidates representing the largest values that can be formed using exactly \(n\) digits from the current line, for all relevant values of \(n\). When a new digit arrives, each candidate is updated by either retaining its previous value or by appending the new digit to the best candidate of length \(n-1\), selecting whichever option yields the larger result. These updates are performed in parallel for all candidate lengths.

When a line boundary is encountered, the candidates corresponding to sequence lengths 2 and 12 are committed to the running totals for part 1 and part 2 respectively, and the per-line state is reset before processing the next line.

The computation proceeds in a single pass over the input, advancing one digit per cycle with a fixed amount of state.


---

<a name="day-4"></a>
### Day 4, Part 1 + Part 2

<p>
<a href="fpga/src/day04.ml"><img src="https://img.shields.io/badge/Code-day04.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/4"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design implements a parametrised two-dimensional stencil engine operating over a padded grid domain.

The computation proceeds by repeatedly scanning the grid using a sliding 3×3 window. Neighbourhood values are constructed using line buffers and shift registers, allowing all eight neighbours of each cell to be evaluated concurrently. For each cell, the number of active neighbours is computed using a bit-sliced population count, and the cell is conditionally removed based on this count.

The algorithm runs iteratively. The first full scan records the number of removed cells for part 1. Subsequent scans continue updating the grid until no further removals occur, accumulating the total number of removed cells across all passes for part 2. The grid state alternates between two buffers between passes.

During each scan, one packed word is processed per cycle. The design is parameterised by grid dimensions and processing width.

---

<a name="day-5"></a>
### Day 5, Part 1 + Part 2

<p>
<a href="fpga/src/day05.ml"><img src="https://img.shields.io/badge/Code-day05.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/5"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design processes a set of numeric ranges and query items, performing range merging followed by membership testing.

Input data is streamed into RAM and interpreted as two sequences: a list of half-open ranges and a list of individual items. In the first phase, the circuit reads and merges overlapping or adjacent ranges. This is implemented as a single-pass merge engine that maintains a current active range and emits merged ranges back into RAM as they are completed.

For part 2, the design accumulates the total coverage of all merged ranges by summing their lengths during the merge phase.

For part 1, each item is tested against the merged ranges. Items and ranges are both traversed in ascending order, allowing early termination when an item is known to lie outside all remaining ranges. This avoids unnecessary comparisons and ensures the query phase remains linear in the size of the input.

The entire computation is performed without sorting in hardware, relying instead on the problem’s ordering guarantees. All memory accesses are sequential, and the design uses a small finite-state machine to coordinate merge, flush, and query phases.

---

<a name="day-6"></a>
### Day 6, Part 1 + Part 2

<p>
<a href="fpga/src/day06.ml"><img src="https://img.shields.io/badge/Code-day06.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/6"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design evaluates a stream of arithmetic expressions encoded as a compact byte format, supporting both additive and multiplicative reductions.

The input is parsed directly from RAM as a byte stream. Each expression begins with an operator mode (add or multiply), followed by a count and a sequence of 16-bit values. Expressions are processed sequentially using a small FSM that decodes operands, accumulates intermediate results, and commits completed expressions to a running total.

For part 1 and part 2, the same datapath is reused, with a phase flag selecting which stream of expressions contributes to which result. A zero count acts as a delimiter between the two phases.

The design avoids buffering entire expressions. Instead, each value is folded into an accumulator as soon as it is decoded, allowing the computation to proceed in a single pass with minimal state. Final results are emitted once the end-of-stream delimiter is encountered.

---

<a name="day-8"></a>
### Day 8, Part 1 + Part 2

<p>
<a href="fpga/src/day08.ml"><img src="https://img.shields.io/badge/Code-day08.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/8"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design implements a streaming hardware version of Kruskal’s algorithm using an on-chip union–find structure.

Point coordinates are preloaded into RAM over UART, with the number of points inferred implicitly from the preload phase. Candidate edges are then streamed in sorted order and processed one at a time. For each edge, the circuit performs iterative root finding using parent pointers stored in RAM, followed by union-by-size when the roots differ. The component count is tracked explicitly to detect when the spanning tree completes.

For part 1, the design captures a snapshot after a fixed number of processed edges and performs a sweep over the component-size RAM to identify the three largest connected components, whose sizes are multiplied to form the result.

For part 2, the final edge that completes the spanning tree is detected directly in hardware. The corresponding point coordinates are read from RAM and combined to produce the final output.

The entire computation is performed as a single pass over the edge stream, with no buffering or sorting in hardware. All memory access patterns are explicit and deterministic, and the design cleanly separates load, union–find traversal, and result extraction.

---

<a name="day-9"></a>
### Day 9, Part 1

<p>
<a href="fpga/src/day09.ml"><img src="https://img.shields.io/badge/Code-day09.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/9"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design computes the largest axis-aligned rectangle defined by two points in a set of integer coordinates.

All points are loaded into RAM as (x, y) pairs. The design performs a double nested iteration over point pairs (A, B), computing the inclusive rectangle area defined by their Manhattan extents. Absolute differences are computed combinationally, and the resulting width and height are multiplied to form a 128-bit area value.

A running maximum is maintained across all pairs. The nested iteration is implemented using two counters and a small FSM, ensuring each unique pair is evaluated exactly once.

Although the algorithm is quadratic in the number of points, it maps cleanly to hardware due to its regular memory access pattern and simple arithmetic. The final maximum area is emitted once all pairs have been processed.

<a name="day-10"></a>
### Day 10 Part 1

<p>
<a href="fpga/src/day10.ml"><img src="https://img.shields.io/badge/Code-day10.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/10"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design streams a sequence of independent “machines” from RAM and solves each one with a bounded brute-force subset search over up to 16 control inputs.

Input words are loaded into RAM over UART. The first word gives the machine count, followed by a per-machine header and a variable-length list of 64-bit masks. Each header encodes the number of active output bits m, the number of available buttons k, and a 48-bit target state. A mask is generated from m so equality checks only consider the low m bits.

For each machine, the first 16 masks are cached into registers and the circuit enumerates all subsets of the k buttons (clipped to 16 in hardware). Each subset produces an XOR-reduced 64-bit state by conditionally including each cached mask. The result is compared against the target under the m-bit mask, and the subset popcount is used as the cost. The best (minimum) popcount across all valid subsets is accumulated into the running total for part 1.

RAM access is explicitly scheduled as a 1-cycle read pipeline: addresses are set in one state, then consumed the next cycle. The design uses a small FSM to step through “read header”, “read masks”, “subset sweep”, and “advance to next machine” phases, emitting the final part 1 total once all machines have been processed.

<a name="day-11"></a>
### Day 11 Part 1 + Part 2

<p>
<a href="fpga/src/day11.ml"><img src="https://img.shields.io/badge/Code-day11.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/11"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design treats the input as a compact graph problem encoded entirely in RAM and executes a small number of structured graph traversals to extract the required path counts.

All data is loaded as 64-bit words. The first two header words define the number of nodes and the distinguished node indices (you, out, svr, fft, dac), along with a flag indicating whether part 2 is present. Each node entry stores a running count, a base address for its outgoing edges, and the number of children. Edge lists reference child node indices directly.

For part 1, the circuit performs a forward propagation starting from you, accumulating path counts by pushing each node’s current count into its children. This is implemented as a linear scan over nodes, followed by a nested scan over each node’s adjacency list. All updates are written back into RAM, so counts are reused without additional buffering.

For part 2, the same propagation engine is reused across a fixed set of source–target runs (e.g. svr→fft, fft→dac, dac→out). Before each run, node counts are reset in RAM with only the start node seeded. Each traversal captures the final count at the target node. The required result is then formed by combining these captured values with a small number of 64-bit multiplications and additions.

RAM accesses are explicitly staged: addresses are issued in one state and consumed the next, with a dual-port RAM allowing simultaneous read and write during propagation. A single FSM sequences header decode, per-run reset, traversal, capture, and final computation. Both part 1 and part 2 results are emitted once all runs complete.

## About

I’m Christian Garry, currently completing a Master’s degree in **Scientific Computing and Data Analysis**, with a background in electronic engineering and a strong focus on **hardware-aware computation**, **deterministic systems**, and **quantitative problem-solving**.

This repository reflects my interest in treating algorithmic problems as *hardware systems*: explicit state machines, cycle-accurate memory access, and streaming I/O protocols, rather than abstract software control flow. My broader interests include high-performance computing, numerical methods, and quantitative research.

You can find more about my work here:
<div align="center">
  <a href="https://www.linkedin.com/in/christian-tt-garry/">
    <img src="https://img.shields.io/badge/LinkedIn-Christian_Garry-0A66C2?style=for-the-badge&logo=linkedin">
  </a>
  <a href="https://christiangarry.com">
    <img src="https://img.shields.io/badge/Website-christiangarry.com-111827?style=for-the-badge">
  </a>
</div>
