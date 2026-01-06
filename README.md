# Christian Garry Advent of Hardcaml 2025
This repository contains synthesizable FPGA implementations of selected [Advent of Code 2025](https://adventofcode.com/) problems, targeting streaming execution on the ULX3S platform for the [Advent of FPGA Challenge](https://blog.janestreet.com/advent-of-fpga-challenge-2025/).

This project is based on the [Jane Street Hardcaml Template Project](https://github.com/janestreet/hardcaml_template_project/tree/with-extensions/), with the structure and UART infrastructure rebuilt and updated from the [2024 Advent of Hardcaml](https://github.com/asinghani/advent-of-hardcaml-2024/) designs to match current versions of Hardcaml and dune.

I've started with AoC days that had solutions that seemed like they would map well to hardware implementations and will see how many I can finish before the deadline.

## Using the repo

```bash
cd fpga
dune runtest
```

This runs the testbench for each implemented day against the sample input
provided in the Advent of Code problem description. Sample inputs live in:

`/inputs/sampleX.txt`

where `X` is the day number.

```bash
cd fpga
make run-all
make run-01
```

These commands run all days (or a single day) against the full input from the
Advent of Code website.

The AoC rules ban redistribution of input files, so they are `.gitignore`d and
must be downloaded manually. If you attempt to run a day without its input
present, the runner will print the correct download link and tell you exactly
where to save it:

`https://adventofcode.com/2025/day/X/input`
`/inputs/inputX.txt`

where `X` is the day number.

## Execution Model

All designs process their inputs as UART streams and produce their outputs over UART. Each solution follows a deterministic load–compute–report structure: input data is received and prepared for processing, the problem-specific algorithm is executed in hardware, and the final results for part 1 and part 2 are formatted as decimal values and transmitted once computation completes.

## Solutions

### Day 1, Part 1 + Part 2  
[Hardcaml solution](#) // [Problem Link](#)

This design simulates the behaviour of a combination lock driven by a sequence of direction-and-step instructions.

Each instruction updates the lock position within the range \([0, 99]\) using modular arithmetic to account for wraparound. For part 1, the design counts how many instructions result in the lock position landing exactly on zero after the update.

For part 2, the design analytically determines how many times the lock passes through zero while executing each instruction. This is done by computing the distance to the first arrival at zero and then counting subsequent arrivals at fixed intervals, without iterating over individual steps.

Instructions are processed sequentially at a rate of one per cycle, and the per-instruction contributions are accumulated to produce the final results.

### Day 3, Part 1 + Part 2  
[Hardcaml solution](#) // [Problem Link](#)

This design processes each input line as a stream of ASCII digits and computes one result per line for both parts of the problem.

As digits are received, the circuit maintains running candidates representing the largest values that can be formed using exactly \(n\) digits from the current line, for all relevant values of \(n\). When a new digit arrives, each candidate is updated by either retaining its previous value or by appending the new digit to the best candidate of length \(n-1\), selecting whichever option yields the larger result. These updates are performed in parallel for all candidate lengths.

When a line boundary is encountered, the candidates corresponding to sequence lengths 2 and 12 are committed to the running totals for part 1 and part 2 respectively, and the per-line state is reset before processing the next line.

The computation proceeds in a single pass over the input, advancing one digit per cycle with a fixed amount of state.

### Day 4, Part 1 + Part 2  
[Hardcaml solution](#) // [Problem Link](#)

This design implements a parametrised two-dimensional stencil engine operating over a padded grid domain.

The computation proceeds by repeatedly scanning the grid using a sliding 3×3 window. Neighbourhood values are constructed using line buffers and shift registers, allowing all eight neighbours of each cell to be evaluated concurrently. For each cell, the number of active neighbours is computed using a bit-sliced population count, and the cell is conditionally removed based on this count.

The algorithm runs iteratively. The first full scan records the number of removed cells for part 1. Subsequent scans continue updating the grid until no further removals occur, accumulating the total number of removed cells across all passes for part 2. The grid state alternates between two buffers between passes.

During each scan, one packed word is processed per cycle. The design is parameterised by grid dimensions and processing width.
