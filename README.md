# Christian Garry Advent of Hardcaml 2025

<p align="center">
  <a href="#solutions"><img src="https://img.shields.io/badge/Solutions-View-1f6feb?style=for-the-badge"></a>
  <a href="#execution-model"><img src="https://img.shields.io/badge/Architecture-Execution_Model-8b949e?style=for-the-badge"></a>
  <a href="fpga/src"><img src="https://img.shields.io/badge/Code-Browse-f85149?style=for-the-badge"></a>
  <a href="https://adventofcode.com/2025"><img src="https://img.shields.io/badge/AoC_2025-Problems-facc15?style=for-the-badge"></a>
  <a href="#execution-model"><img src="https://img.shields.io/badge/Design-Streaming_UART_FPGA-22c55e?style=for-the-badge"></a>
</p>

<p align="center">
  <a href="https://www.linkedin.com/in/christian-garry/">
    <img src="https://img.shields.io/badge/LinkedIn-Christian_Garry-0A66C2?style=for-the-badge&logo=linkedin">
  </a>
  <a href="https://christiangarry.com">
    <img src="https://img.shields.io/badge/Website-christiangarry.com-111827?style=for-the-badge">
  </a>
</p>

This repository contains synthesizable FPGA implementations of selected  
[Advent of Code 2025](https://adventofcode.com/) problems, targeting streaming execution on the ULX3S platform for the  
[Advent of FPGA Challenge](https://blog.janestreet.com/advent-of-fpga-challenge-2025/).

This project is based on the  
[Jane Street Hardcaml Template Project](https://github.com/janestreet/hardcaml_template_project/tree/with-extensions),  
with the structure and UART infrastructure rebuilt and updated from the  
[2024 Advent of Hardcaml](https://github.com/asinghani/advent-of-hardcaml-2024/) designs to match current versions of Hardcaml and dune.

I've started with AoC days that had solutions that seemed like they would map well to hardware implementations and will see how many I can finish before the deadline.

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

Each instruction updates the lock position within the range [0, 99] using modular arithmetic to account for wraparound. For part 1, the design counts how many instructions result in the lock position landing exactly on zero after the update.

For part 2, the design analytically determines how many times the lock passes through zero while executing each instruction, without iterating over individual steps.

---

<a name="day-2"></a>
### Day 2, Part 1 + Part 2

<p>
<a href="fpga/src/day02.ml"><img src="https://img.shields.io/badge/Code-day02.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/2"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design evaluates numeric ranges stored in external memory and identifies values that satisfy digit-pattern constraints, using a fully streaming binary-to-BCD datapath.

---

<a name="day-3"></a>
### Day 3, Part 1 + Part 2

<p>
<a href="fpga/src/day03.ml"><img src="https://img.shields.io/badge/Code-day03.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/3"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design processes each input line as a stream of ASCII digits and computes one result per line for both parts of the problem.

---

<a name="day-4"></a>
### Day 4, Part 1 + Part 2

<p>
<a href="fpga/src/day04.ml"><img src="https://img.shields.io/badge/Code-day04.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/4"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design implements a parametrised two-dimensional stencil engine operating over a padded grid domain.

---

<a name="day-5"></a>
### Day 5, Part 1 + Part 2

<p>
<a href="fpga/src/day05.ml"><img src="https://img.shields.io/badge/Code-day05.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/5"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design processes a set of numeric ranges and query items, performing range merging followed by membership testing.

---

<a name="day-6"></a>
### Day 6, Part 1 + Part 2

<p>
<a href="fpga/src/day06.ml"><img src="https://img.shields.io/badge/Code-day06.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/6"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design evaluates a stream of arithmetic expressions encoded as a compact byte format.

---

<a name="day-8"></a>
### Day 8, Part 1 + Part 2

<p>
<a href="fpga/src/day08.ml"><img src="https://img.shields.io/badge/Code-day08.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/8"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design implements a streaming hardware version of Kruskal’s algorithm using an on-chip union–find structure.

---

<a name="day-9"></a>
### Day 9, Part 1

<p>
<a href="fpga/src/day09.ml"><img src="https://img.shields.io/badge/Code-day09.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/9"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
<a href="#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
</p>

This design computes the largest axis-aligned rectangle defined by two points in a set of integer coordinates.
