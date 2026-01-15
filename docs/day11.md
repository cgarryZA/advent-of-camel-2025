<a name="day-11"></a>
### Day 11
<p>
<a href="../README.md#solutions"><img src="https://img.shields.io/badge/Back_to-Solutions-8b949e"></a>
<a href="fpga/src/day11.ml"><img src="https://img.shields.io/badge/Code-day11.ml-f85149"></a>
<a href="https://adventofcode.com/2025/day/11"><img src="https://img.shields.io/badge/AoC-Problem-facc15"></a>
</p>

This design treats the input as a compact graph problem encoded entirely in RAM and executes a small number of structured graph traversals to extract the required path counts.

All data is loaded as 64-bit words. The first two header words define the number of nodes and the distinguished node indices (you, out, svr, fft, dac), along with a flag indicating whether part 2 is present. Each node entry stores a running count, a base address for its outgoing edges, and the number of children. Edge lists reference child node indices directly.

For part 1, the circuit performs a forward propagation starting from you, accumulating path counts by pushing each node’s current count into its children. This is implemented as a linear scan over nodes, followed by a nested scan over each node’s adjacency list. All updates are written back into RAM, so counts are reused without additional buffering.

For part 2, the same propagation engine is reused across a fixed set of source–target runs (e.g. svr→fft, fft→dac, dac→out). Before each run, node counts are reset in RAM with only the start node seeded. Each traversal captures the final count at the target node. The required result is then formed by combining these captured values with a small number of 64-bit multiplications and additions.

RAM accesses are explicitly staged: addresses are issued in one state and consumed the next, with a dual-port RAM allowing simultaneous read and write during propagation. A single FSM sequences header decode, per-run reset, traversal, capture, and final computation. Both part 1 and part 2 results are emitted once all runs complete.
