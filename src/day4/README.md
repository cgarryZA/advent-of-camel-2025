# AoC 2025 — Day 4 (Hardcaml) — Streaming solver with ready/valid I/O

This Day 4 solution is implemented as a **parameterised Hardcaml generator** and exercised through a **stream-style (AXI4-Stream Compatible) ready/valid wrapper**.

The executable `solve_day4.exe`:
1. Reads the ASCII grid from a file (`@` = occupied, `.` = empty).
2. Pads it with a 1-cell `.` border on all sides.
3. Packs each extended row into `lanes`-bit words.
4. Streams those words into the hardware wrapper using `s_axis_tvalid/s_axis_tready` with:
   - `tuser[0]=1` only on the first beat (SOF)
   - `tlast=1` only on the final beat (end-of-frame)
5. Waits for a single-beat result on `m_axis_*` and prints:
   - **Part 1**: removals on the first pass
   - **Part 2**: total removals over all passes

---

## What’s in the design

### 1) Packed engine (`Engine`)
The engine operates on packed words (`lanes` cells per cycle) and runs repeated passes until no more removals occur.  
It stores the packed grid in RAM and updates it pass-by-pass.

### 2) Stream wrapper (`Stream`)
The stream wrapper exposes a simple ready/valid interface:

**Input (S_AXIS-like)**
- `s_axis_tvalid`
- `s_axis_tready`
- `s_axis_tdata[lanes-1:0]`
- `s_axis_tuser[0]` = SOF (start-of-frame), asserted only on first word
- `s_axis_tlast` = asserted only on last word

**Output (M_AXIS-like)**
- `m_axis_tvalid`
- `m_axis_tready`
- `m_axis_tdata` = `{p2, p1}` (MSB..LSB), single beat
- `m_axis_tlast` = `1` (single-beat packet)
- `m_axis_tuser[0]` = `1` (result beat marker)

**Protocol checking**
- `frame_error` is asserted if SOF/TLAST are incorrect (e.g. SOF mid-frame, missing TLAST on final beat).

---

## Build

From the repo root:

```bash
dune build
```

---

## Run

The solver infers `rows/cols` from the input file and defaults to `-lanes 64`.

### Run the included 135×135 and 512×512 examples

```bash
dune exec ./bin/solve_day4.exe -- inputs/day4/135x135.txt
dune exec ./bin/solve_day4.exe -- inputs/day4/512x512.txt
```

### Run your real input (if you have it in the same folder)

```bash
dune exec ./bin/solve_day4.exe -- inputs/day4/input.txt
```

### Change packing width

```bash
dune exec ./bin/solve_day4.exe -- -lanes 32 inputs/day4/135x135.txt
dune exec ./bin/solve_day4.exe -- -lanes 64 inputs/day4/135x135.txt
```

---

## Waveforms (VCD)

You can optionally dump a VCD:

```bash
dune exec ./bin/solve_day4.exe -- -vcd day4.vcd inputs/day4/135x135.txt
```

This writes `day4.vcd` in the current directory.

### Viewing the VCD

- **Linux with a GUI**: open with GTKWave:
  ```bash
  gtkwave day4.vcd
  ```
- **WSL**: GTKWave needs a working GUI (`$DISPLAY` or WSLg). If `echo $DISPLAY` is empty, you won’t be able to launch GUI apps from WSL.
  In that case, copy `day4.vcd` to Windows and open it there with a waveform viewer, or enable WSLg/X-server.

---

## Notes on what the runner is doing

The `solve_day4.ml` runner is a self-contained test harness:
- It performs the ASCII → padded grid → packed words conversion.
- It drives the wrapper using backpressure (`s_axis_tready`) correctly (it waits until ready before presenting each beat).
- It asserts SOF on the first beat and TLAST on the final beat.
- It waits for `m_axis_tvalid`, asserts `m_axis_tready` for one cycle to accept the result, then prints `{p1,p2}`.

