#!/usr/bin/env python3
"""Re-baseline the AoC 2025 examples (examples/aoc25/PERFORMANCE.md).

Runs every day/part of examples/aoc25 against the real puzzle inputs in
examples/aoc25/inputs/dayNN.txt (not in the repository; see the README
there). Days with a `par-*` site run as interleaved pairs: a sequential arm
(EU_PP_THRESHOLD forced above any list size, so `par-*` takes the
sequential fallback) and a parallel arm (the default: `par-*` forks). The
other days run once per round in the sequential arm only; they make no
`par-*` call, so that is the same as running them with defaults.

Prints a Markdown table: wall median-of-N with min-max range, and ticks,
allocations, collections and peak stack from the median sequential run.
With --copy-stats, the median sequential run's statistics JSON replaces
examples/aoc25/stats/dayNN-part-N.json.

Build first, with the prelude blob that release binaries embed:

    cargo xtask prelude-compile && cargo clean && cargo build --release
    python3 scripts/aoc25-bench.py --runs 7 --out /tmp/aoc25-bench
"""

import argparse
import json
import os
import shutil
import statistics
import subprocess
import time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
AOC = os.path.join(ROOT, "examples", "aoc25")
PARTS = [f"{d:02d}-{p}" for d in range(1, 13) for p in (1, 2)][:-1]
PAR_DAYS = {"04", "08", "09", "10", "11"}


def run(eu, out, target, arm, i):
    day, part = target.split("-")
    env = dict(os.environ)
    env.pop("EU_PP_THRESHOLD", None)
    if arm == "seq":
        env["EU_PP_THRESHOLD"] = "999999999"
    stats = os.path.join(out, f"day{day}-part-{part}-{arm}-{i}.json")
    cmd = ["timeout", "600", eu, "--statistics-file", stats,
           f"run/day{day}.eu", "-t", f"part-{part}"]
    t0 = time.perf_counter()
    proc = subprocess.run(cmd, cwd=AOC, env=env, capture_output=True, text=True)
    wall = time.perf_counter() - t0
    return {"target": target, "arm": arm, "i": i, "wall": wall,
            "rc": proc.returncode, "answer": proc.stdout.strip(),
            "stats": stats, "load": os.getloadavg()[0]}


def median_run(runs):
    med = statistics.median(r["wall"] for r in runs)
    return med, min(runs, key=lambda r: abs(r["wall"] - med))


def wall(runs):
    med, _ = median_run(runs)
    ws = [r["wall"] for r in runs]
    return f"{med:.3f}s ({min(ws):.3f}–{max(ws):.3f})", med


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--runs", type=int, default=7)
    ap.add_argument("--out", required=True)
    ap.add_argument("--eu", default=os.path.join(ROOT, "target", "release", "eu"))
    ap.add_argument("--copy-stats", action="store_true")
    ap.add_argument("targets", nargs="*", default=PARTS)
    args = ap.parse_args()
    # eu runs with cwd=examples/aoc25, so both paths must be absolute.
    args.out = os.path.abspath(args.out)
    # A bare name (no directory part) is looked up on PATH.
    if os.path.dirname(args.eu):
        args.eu = os.path.abspath(args.eu)
    else:
        found = shutil.which(args.eu)
        if found is None:
            raise SystemExit(f"{args.eu}: not found on PATH")
        args.eu = found
    os.makedirs(args.out, exist_ok=True)

    results = {}
    with open(os.path.join(args.out, "raw.jsonl"), "w") as log:
        for target in args.targets:
            arms = ["seq", "par"] if target[:2] in PAR_DAYS else ["seq"]
            for i in range(args.runs):
                for arm in arms:
                    r = run(args.eu, args.out, target, arm, i)
                    log.write(json.dumps(r) + "\n")
                    log.flush()
                    if r["rc"] != 0:
                        raise SystemExit(f"{target} {arm} run {i}: exit {r['rc']}")
                    results.setdefault((target, arm), []).append(r)

    loads = [r["load"] for rs in results.values() for r in rs]
    print(f"load average range {min(loads):.2f}–{max(loads):.2f}")
    print("| Example | Sequential | Parallel | Speedup | Ticks | Allocs | GCs | Max stack |")
    print("|---|---|---|---|---|---|---|---|")
    for target in args.targets:
        seq = results[(target, "seq")]
        answers = {r["answer"] for rs in results.values() for r in rs
                   if r["target"] == target}
        if len(answers) != 1:
            raise SystemExit(f"{target}: answers differ between runs or arms")
        seq_s, seq_med = wall(seq)
        _, med = median_run(seq)
        with open(med["stats"]) as f:
            s = json.load(f)
        par_s, speedup = "—", "—"
        if (target, "par") in results:
            par_s, par_med = wall(results[(target, "par")])
            # PROTOCOL.md: nothing under ~200 ms enters ratio analysis.
            if min(seq_med, par_med) < 0.2:
                speedup = "— (sub-200ms)"
            else:
                speedup = f"{seq_med / par_med:.2f}×"
        day, part = target.split("-")
        print(f"| day{day}-p{part} | {seq_s} | {par_s} | {speedup} | "
              f"{s['machine_ticks']:,} | {s['machine_allocs']:,} | "
              f"{s['collections_count']} | {s['machine_max_stack']:,} |")
        if args.copy_stats:
            shutil.copy(med["stats"],
                        os.path.join(AOC, "stats", f"day{day}-part-{part}.json"))


if __name__ == "__main__":
    main()
