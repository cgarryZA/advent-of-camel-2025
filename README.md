<div align="center">
  
  <img src="https://raw.githubusercontent.com/cgarryZA/advent-of-camel-2025/main/img/snowglobe_camel.png"
       alt="Advent of Camel logo"
       width="450"/>


<a name="readme-top"></a>

# Advent of Hardcaml 2025 — Christian Garry 

A hardware-first exploration of Advent of Code 2025, implementing each puzzle as a **cycle-accurate FPGA design** in **Hardcaml**, using streaming UART I/O, explicit memory pipelines, and finite-state schedulers instead of software control flow.

<a name="internal-nav"></a>
### Contents
<p align="center">
  <a href="#intro">
    <img src="https://img.shields.io/badge/Introduction-1f6feb?style=for-the-badge">
  </a><br/>
  <a href="#setup">
    <img src="https://img.shields.io/badge/Setup_Guide-1f6feb?style=for-the-badge">
  </a><br/>
  <a href="#user-guide">
    <img src="https://img.shields.io/badge/User_Guide-1f6feb?style=for-the-badge">
  </a><br/>
  <a href="#execution-model">
    <img src="https://img.shields.io/badge/Execution_Model-1f6feb?style=for-the-badge">
  </a><br/>
  <a href="#solutions">
    <img src="https://img.shields.io/badge/Solutions_Section-1f6feb?style=for-the-badge">
  </a><br/>
  <a href="#highlighted-sols">
    <img src="https://img.shields.io/badge/Highlighted_Days-1f6feb?style=for-the-badge">
  </a><br/>
  <a href="#about">
    <img src="https://img.shields.io/badge/About_Me-1f6feb?style=for-the-badge">
  </a>
</p>

### Related Links

<!-- Row 1 -->
<p align="center">
  <a href="https://adventofcode.com/2025">
    <img src="https://img.shields.io/badge/Problems-AoC_2025-facc15?style=for-the-badge">
  </a>
  &nbsp;&nbsp;
  <a href="fpga/src">
    <img src="https://img.shields.io/badge/Code-Github_File_Viewer-f85149?style=for-the-badge">
  </a>
  &nbsp;&nbsp;
  <a href="https://blog.janestreet.com/advent-of-fpga-challenge-2025/">
    <img src="https://img.shields.io/badge/Challenge-Advent_of_FPGA-0ea5e9?style=for-the-badge">
  </a>
</p>

<!-- Row 2 -->
<p align="center">
  <a href="https://github.com/janestreet/hardcaml_template_project/tree/with-extensions">
    <img src="https://img.shields.io/badge/Jane_Street-Template-64748b?style=for-the-badge">
  </a>
  &nbsp;&nbsp;
  <a href="https://github.com/asinghani/advent-of-hardcaml-2024/">
    <img src="https://img.shields.io/badge/Anish-2024_Solutions-64748b?style=for-the-badge">
  </a>
  &nbsp;&nbsp;
  <a href="https://github.com/asinghani/advent-of-hardcaml-2025/">
    <img src="https://img.shields.io/badge/Anish-2025_Solutions-64748b?style=for-the-badge">
  </a>
</p>


</div>


<a name="intro"></a>
## Introduction
<a href="#internal-nav"> <img src="https://img.shields.io/badge/Back_to-Navigation-8b949e"> </a>

This repository contains synthesizable FPGA implementations of selected [Advent of Code 2025](https://adventofcode.com/) problems, targeting streaming execution on the ULX3S platform for the [Advent of FPGA Challenge](https://blog.janestreet.com/advent-of-fpga-challenge-2025/).

This project is based on the [Jane Street Hardcaml Template Project](https://github.com/janestreet/hardcaml_template_project/tree/with-extensions), with the structure and UART infrastructure rebuilt and updated from the [2024 Advent of Hardcaml](https://github.com/asinghani/advent-of-hardcaml-2024/) designs to match current versions of Hardcaml and dune.

<br>
<p align="center">
  <img
    src="https://raw.githubusercontent.com/cgarryZA/advent-of-camel-2025/main/img/Animation.gif"
    alt="Advent of Code background animation"
    width="560"
  />
</p>
<br>

I've started with AoC days that had solutions that seemed like they would map well to hardware implementations and will see how many I can finish before the deadline.


<a name="setup"></a>
## Setup & Installation
<a href="#internal-nav"> <img src="https://img.shields.io/badge/Back_to-Navigation-8b949e"> </a>

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

<a name="user-guide"></a>
## Using the repo
<a href="#internal-nav"> <img src="https://img.shields.io/badge/Back_to-Navigation-8b949e"> </a>

```bash
cd fpga
dune runtest
```

This runs the testbench for each implemented day against the sample input provided in the Advent of Code problem description.
Sample inputs live in:

`/inputs/sampleX.txt`

where `X` is the day number (no leading zeros).

```bash
cd fpga
make run-all
```
This command runs all completed days against the full input from the Advent of Code website.

```bash
cd fpga
make run-XX
```

This command runs a specific day against the full input. 
Where `XX` is the day number (with leading zero).


The AoC rules ban redistribution of input files, so they are `.gitignore`d and must be downloaded manually.  
If you attempt to run a day without its input present, the runner will print the correct download link:

`https://adventofcode.com/2025/day/X/input`  

and tell you exactly where to save it:

`/inputs/inputX.txt`

<a name="execution-model"></a>
## Execution Model
<a href="#internal-nav"> <img src="https://img.shields.io/badge/Back_to-Navigation-8b949e"> </a>

All designs process their inputs as UART streams and produce their outputs over UART.  
Each solution follows a deterministic **load–compute–report** structure: input data is received and prepared for processing, the problem-specific algorithm is executed in hardware, and the final results for part 1 and part 2 are formatted as decimal values and transmitted once computation completes.

<a name="solutions"></a>

## Solutions Navigation
<a href="#internal-nav"> <img src="https://img.shields.io/badge/Back_to-Navigation-8b949e"> </a>
<div align="center">

<!-- Calendar GIF -->
<img src="https://raw.githubusercontent.com/cgarryZA/advent-of-camel-2025/main/img/calendar.gif"
     alt="Advent calendar progress"
     width="360"/>

<br/><br/>

<!-- Calendar-style grid -->
<table align="center">
<tr>
  <td><a href="docs/day01.md"><img src="https://img.shields.io/badge/01-Done-22c55e"></a></td>
  <td><a href="docs/day02.md"><img src="https://img.shields.io/badge/02-Done-22c55e"></a></td>
  <td><a href="docs/day03.md"><img src="https://img.shields.io/badge/03-Done-22c55e"></a></td>
  <td><a href="#day-4"><img src="https://img.shields.io/badge/04-Done-22c55e"></a></td>
  <td><a href="docs/day05.md"><img src="https://img.shields.io/badge/05-Done-22c55e"></a></td>
  <td><a href="#day-6"><img src="https://img.shields.io/badge/06-Done-22c55e"></a></td>
</tr>

<tr>
  <td><a href="#solutions"><img src="https://img.shields.io/badge/07-Pending-3f3f46"></td>
  <td><a href="docs/day08.md"><img src="https://img.shields.io/badge/08-Done-22c55e"></a></td>
  <td><a href="docs/day09.md"><img src="https://img.shields.io/badge/09-Done-22c55e"></a></td>
  <td><a href="docs/day10.md"><img src="https://img.shields.io/badge/10-Done-22c55e"></a></td>
  <td><a href="docs/day11.md"><img src="https://img.shields.io/badge/11-Done-22c55e"></a></td>
  <td><a href="#solutions"><img src="https://img.shields.io/badge/12-Pending-3f3f46"></td>
</tr>
</table>

</div>

<a name="highlighted-sols"></a>
## Highlighted Solutions
<a href="#internal-nav"> <img src="https://img.shields.io/badge/Back_to-Navigation-8b949e"> </a>


<a name="about"></a>
## About Me
<a href="#internal-nav"> <img src="https://img.shields.io/badge/Back_to-Navigation-8b949e"> </a>

I’m Christian Garry, a Graduate Communications Engineer at Siemens and an MSc student in **Scientific Computing and Data Analysis** at Durham University. I previously completed an **MEng in Electronic Engineering** at Durham.

This repository reflects how I tend to approach problems: decomposing them into small, well-defined components with explicit interfaces, then composing those components into complete systems.

You can find more about my work and background here:
<div align="center">
  <a href="https://www.linkedin.com/in/christian-tt-garry/">
    <img src="https://img.shields.io/badge/LinkedIn-Christian_Garry-0A66C2?style=for-the-badge&logo=linkedin">
  </a>
  <a href="https://christiangarry.com">
    <img src="https://img.shields.io/badge/Website-christiangarry.com-22c55e?style=for-the-badge">
  </a>
</div>

---

*Thanks to Jane Street and the Advent of Code team for publishing the Advent of FPGA challenge and accompanying visual material.*
