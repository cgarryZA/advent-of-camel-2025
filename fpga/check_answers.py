import json
import sys
from pathlib import Path

def die(msg):
    print(f"ERROR: {msg}")
    sys.exit(1)

def main():
    if len(sys.argv) != 2:
        die("Usage: python check_answers.py <day_number>")

    day = str(int(sys.argv[1]))  # normalize "01" → "1"

    base = Path(__file__).resolve().parent  # fpga/

    answers_path = base / "inputs" / "answers.json"
    outputs_path = base / "inputs" / "outputs.json"

    if not answers_path.exists():
        die(f"Canonical answers not found: {answers_path}")

    if not outputs_path.exists():
        die(f"Outputs not found: {outputs_path}")

    with answers_path.open() as f:
        answers = json.load(f)

    with outputs_path.open() as f:
        outputs = json.load(f)

    if day not in answers:
        die(
            f"Please enter the true solution for day {day} "
            f"in /inputs/answers.json"
        )

    if day not in outputs:
        die(f"Day {day} missing from outputs.json")

    ok = True

    for part in ("part1", "part2"):
        # Structural checks
        if part not in answers[day]:
            die(f"Missing key '{part}' for day {day} in answers.json")

        if part not in outputs[day]:
            die(f"Missing key '{part}' for day {day} in outputs.json")

        a = answers[day][part]
        o = outputs[day][part]

        # Semantic comparison
        if a is None:
            print(f"⚪ Day {day} {part} skipped (no canonical answer)")
            continue

        if a != o:
            print(f"❌ Day {day} {part} mismatch")
            print(f"   expected: {a}")
            print(f"   got     : {o}")
            ok = False
        else:
            print(f"✅ Day {day} {part} OK ({a})")

    if ok:
        print(f"\nDay {day} PASSED")
    else:
        print(f"\nDay {day} COMPLETED WITH MISMATCHES")

    sys.exit(0)

if __name__ == "__main__":
    main()
