# Sets on dict nodes: benchmarks

- **Date:** 2026-11-14
- **Bead:** eu-oaz72.21 (dict PR 5; design note
  `docs/superpowers/plans/2026-11-12-dict-design.md` §6, §10 item 5, §15)
- **Confidence:** measured-single — one session machine (4 vCPU cloud
  container, not a quiet dedicated host), two release binaries (master at
  5c5378d and this branch), each with its own freshly compiled prelude blob
  (identical prelude source), configurations interleaved, seven rounds.
  Wall time is the median with min–max; ticks and peak blocks are
  deterministic per configuration. RSS is the maximum resident set over the
  rounds.

## 1e5 set operations

Programs (in the PR's session scratch space, reproduced here):

- **add:** `go(i, s): (i = 100000 ∨ (s set.size) < 0) then(s, go(i + 1, s
  set.add(i * 7919 % 100003)))` — the condition forces the set each step,
  the loop shape PR #1159 used for its 1e5 `dict.assoc` figure.
- **add baseline:** the same loop accumulating `+` into a number.
- **lookup:** `range(0, 100000) filter(_ ∈ st) count` over
  `st: range(0, 100000) set.from-list`.
- **lookup baseline:** `range(0, 100000) filter(_ >= 0) count`.

| program | binary | ticks | peak blocks | max RSS | wall median (min–max) |
|---|---|---:|---:|---:|---:|
| add baseline | master | 8,200,462 | 1,031 | 66 MB | 0.42 s (0.35–0.45) |
| add baseline | branch | 8,200,462 | 1,031 | 66 MB | 0.40 s (0.36–0.49) |
| 1e5 `set.add` | master | 10,300,626 | 1,031 | 133 MB | 16.6 s (14.5–17.1) |
| 1e5 `set.add` | branch | 10,300,626 | 1,988 | 112 MB | 0.96 s (0.85–1.20) |
| lookup baseline | master | 16,600,623 | 8,900 | 497 MB | 1.44 s (1.35–1.60) |
| lookup baseline | branch | 16,600,623 | 8,900 | 497 MB | 1.42 s (1.23–1.70) |
| 1e5 `set.contains?` | master | 27,700,691 | 11,161 | 564 MB | 2.23 s (2.09–2.67) |
| 1e5 `set.contains?` | branch | 27,700,691 | 11,210 | 543 MB | 2.25 s (2.06–2.75) |

Reading:

- 1e5 adds cost about 0.56 s over the same loop without a set (17× faster
  than master, where each add copied the whole array), with the heap
  bounded: each superseded version's path nodes become garbage. The peak
  heap is higher than master's (tree nodes are larger than array slots)
  but RSS is lower, since master's 1e5 array copies churned the allocator.
- A callgrind profile of 2e4 adds puts the tree code (`insert`, `balance`,
  `cmp_keys`) at about 3% of instructions; the rest is the engine and the
  heap allocator's hole search, not set work.
- Lookups are unchanged: membership was already a binary search. The
  lookup programs' heap is the 1e5-element range list, not the set.

## GC-churn benchmarks 002, 005–009

| bench | ticks (both) | peak blocks (both) | wall master | wall branch |
|---|---:|---:|---:|---:|
| 002 thunk updates | 582,836 | 0 | 0.114 s | 0.118 s |
| 005 drop cons | 1,820,429 | 1,024 | 0.140 s | 0.180 s |
| 006 block lookup | 5,487 | 0 | 0.072 s | 0.076 s |
| 007 short lived | 2,362,793 | 1,046 | 0.202 s | 0.213 s |
| 008 long lived graph | 1,811,599 | 1,024 | 0.152 s | 0.147 s |
| 009 fragmentation | 1,743,075 | 1,024 | 0.190 s | 0.181 s |

None of these programs uses sets. Ticks and peak blocks are identical on
both binaries, so the change does not alter their evaluation or heap; every
run is under the protocol's ~200 ms floor for ratio analysis, so the wall
differences (in both directions, within each pair's min–max spread) are
not evidence either way.

## Prelude mode and the "well under a second" criterion

The figures above use binaries built with the prelude blob
(`cargo xtask prelude-compile`), as installed binaries are. A clean release
build without the blob compiles the prelude from source on every run and
gives different code for the loop: in that mode the add loop takes
11,200,646 ticks (blob: 10,300,626) and, on this machine, 1.53 s against
0.56 s for the loop without a set (measured-single, one round). The first
reviewer, building without the blob, measured 1.45 s median (1.32–1.59)
against 0.46 s.

So 1e5 adds cost about 0.56 s over the bare loop with the blob and about
1.0 s without it. That is about 17× faster than master either way, but not
clearly "well under a second" as the bead's acceptance asks. A callgrind
profile of 2e4 adds (blob build) puts the tree code (`insert`, `balance`,
`cmp_keys`) at about 4.5% of instructions; the rest is the engine's
dispatch, environment access and heap allocation. Closing the gap is engine
performance work, which the performance protocol leaves to a separately
approved change.

## After list elements (OPEN 8)

- **Confidence:** measured-single — the same 4 vCPU session container, one
  release binary of the branch with list elements (built with the prelude
  blob, `cargo xtask prelude-compile && cargo build --release`), the three
  programs below interleaved, seven rounds. Max RSS from one further run
  of each.
- **Programs**, exactly as run (`eu FILE`):
  - add: `go(i, s): (i = 100000 ∨ (s set.size) < 0) then(s, go(i + 1, s set.add(i * 7919 % 100003)))` with `out: go(0, ∅) set.size`;
  - baseline: `go(i, s): (i = 100000 ∨ s < 0) then(s, go(i + 1, s + (i * 7919 % 100003)))` with `out: go(0, 0)`;
  - pairs: as add, adding `[i % 317, i * 7919 % 100003]`.

| program | ticks | peak blocks | max RSS | wall median (min–max) |
|---|---:|---:|---:|---:|
| baseline | 8,000,216 | 1,030 | — | 0.36 s (0.35–0.41) |
| 1e5 `set.add` | 10,600,226 | 1,467 | 86 MB | 0.89 s (0.86–1.00) |
| 1e5 `set.add` of `[x, y]` pairs | 15,900,227 | 2,539 | 133 MB | 1.92 s (1.87–2.11) |

The element wrappers now deep-force with `seqKey`, which costs 3 ticks more
per add than the earlier box case. 1e5 adds cost about 0.53 s over the bare
loop, which the coordinator reads as meeting "well under a second" for the
shipped (blob) configuration. Pair elements cost more: each add builds and
deep-forces a two-element list and allocates a key list. The engine-side
cost of persistent-tree updates, and `set.add`'s second tree walk (`lookup`
then `insert`), are follow-ups.
