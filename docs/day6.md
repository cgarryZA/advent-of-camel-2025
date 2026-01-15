q<a name="day-6"></a>
### Day 6, Part 1 + Part 2

<p>
<a href="../README.md#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
<a href="fpga/src/day06.ml"><img src="https://img.shields.io/badge/Code-day06.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/6"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</p>

This design evaluates a stream of arithmetic expressions encoded as a compact byte format, supporting both additive and multiplicative reductions.

The input is parsed directly from RAM as a byte stream. Each expression begins with an operator mode (add or multiply), followed by a count and a sequence of 16-bit values. Expressions are processed sequentially using a small FSM that decodes operands, accumulates intermediate results, and commits completed expressions to a running total.

For part 1 and part 2, the same datapath is reused, with a phase flag selecting which stream of expressions contributes to which result. A zero count acts as a delimiter between the two phases.

The design avoids buffering entire expressions. Instead, each value is folded into an accumulator as soon as it is decoded, allowing the computation to proceed in a single pass with minimal state. Final results are emitted once the end-of-stream delimiter is encountered.

