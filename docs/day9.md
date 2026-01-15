<a name="day-9"></a>
### Day 9, Part 1

<p>
<a href="../README.md#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
<a href="fpga/src/day09.ml"><img src="https://img.shields.io/badge/Code-day09.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/9"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</p>

This design computes the largest axis-aligned rectangle defined by two points in a set of integer coordinates.

All points are loaded into RAM as (x, y) pairs. The design performs a double nested iteration over point pairs (A, B), computing the inclusive rectangle area defined by their Manhattan extents. Absolute differences are computed combinationally, and the resulting width and height are multiplied to form a 128-bit area value.

A running maximum is maintained across all pairs. The nested iteration is implemented using two counters and a small FSM, ensuring each unique pair is evaluated exactly once.

Although the algorithm is quadratic in the number of points, it maps cleanly to hardware due to its regular memory access pattern and simple arithmetic. The final maximum area is emitted once all pairs have been processed.
