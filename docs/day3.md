
<a name="day-3"></a>
### Day 3, Part 1 + Part 2

<p>
<a href="../README.md#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
<a href="fpga/src/day03.ml"><img src="https://img.shields.io/badge/Code-day03.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/3"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</p>

This design processes each input line as a stream of ASCII digits and computes one result per line for both parts of the problem.

As digits are received, the circuit maintains running candidates representing the largest values that can be formed using exactly \(n\) digits from the current line, for all relevant values of \(n\). When a new digit arrives, each candidate is updated by either retaining its previous value or by appending the new digit to the best candidate of length \(n-1\), selecting whichever option yields the larger result. These updates are performed in parallel for all candidate lengths.

When a line boundary is encountered, the candidates corresponding to sequence lengths 2 and 12 are committed to the running totals for part 1 and part 2 respectively, and the per-line state is reset before processing the next line.

The computation proceeds in a single pass over the input, advancing one digit per cycle with a fixed amount of state.
