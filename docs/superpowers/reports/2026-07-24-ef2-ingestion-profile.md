# EF2 filesystem-ingestion profiling spike: where the cost actually is

- **Date:** 2026-07-24
- **Bead:** eu-1tkk.3 (EF2: Native filesystem/IO capabilities) — profiling
  spike, analysis only, no implementation. Phase 1 of the Stopwatch
  two-phase workflow.
- **Worktree/commit:** `.claude/worktrees/agent-afb0e2e7dba00e3b4`, `master`
  at `694784ce` ("Merge pull request #1061 …").
- **Toolchain:** rustc 1.97.0 (`stable-aarch64-apple-darwin`), macOS 26.5.1
  (25F80), Apple aarch64 (M-series, 14 logical CPUs).
- **Build provenance:** `cargo clean`, `cargo build --release`, then `cargo
  run -p xtask --release -- prelude-compile` to regenerate
  `lib/prelude.blob`, then a **second** `cargo build --release` (confirmed
  "Compiling eucalypt" in the output, i.e. a genuine re-embed, not a stale
  binary — the `prelude-blob` `include_bytes!` build-time-input trap noted
  in prior sessions' memory). One binary, one blob, both engines
  (`EU_HEAPSYN=1` selects HeapSyn; default is bytecode/predecode).

## 0. Protocol-compliance disclosure — machine was not quiet

`uptime` load average ranged **3.9–9.7** through this session (six-plus
other agents active in this worktree's session: main, clarion,
clarion-7x0r, clarion-classifier, furnace, lantern, lantern-yoas, wicket,
wicket-1059, wicket-1061, stopwatch-bv3). This violates PROTOCOL §2's
quiet-machine precondition (load < 1). Per protocol, **every wall-clock
figure in this report is capped at measured-single, and is reported with
its spread**, not as a bare point. The deterministic layer — VM ticks and
allocation counts under `EU_HEAPSYN=1 -S` — is not sensitive to load and
**is measured-verified** (protocol §5: deterministic counters qualify as
measured-verified without needing repeat sessions). The central finding of
this report rests on the deterministic layer; wall-clock numbers are
corroborating context only.

## 1. The question

ROADMAP.md's EF2 note claims native filesystem intrinsics are **not** a
performance blocker: "the shell call and the `vec.of` build are cheap; the
slowness one hits aggregating the result is a separate `foldl` defect,
handled as a bug fix, not here." The owner's framing is that scale *is* the
point of EF2. This spike measures where the cost in the current
shell-out-and-parse idiom actually sits, and whether the "foldl defect" is
real, current, and narrow-or-general.

**One-line verdict: (C), with a correction to the ROADMAP's factual
premise.** The shell call and the vec build genuinely are cheap and linear
— that part of the ROADMAP note is correct and reconfirmed here. But the
"aggregating the result is a separate foldl defect, handled as a bug fix"
half is **not accurate on current master**: it is not a separate, narrow,
already-fixed defect. It is the *same* general, still-open,
architecturally-tracked O(n²) "shared env-walk ceiling" (ROADMAP Pillar CG
item 4; beads eu-2sa6.2 BV3, eu-gmdl5 CG4) that governs **any** use of a
prelude higher-order list function — `count`, `filter`, `sum`, `map` used
the ordinary way — not a foldl-specific corner case. It dominates
end-to-end wall time by two to three orders of magnitude at the tree sizes
tested, and it grows quadratically while ingestion stays flat. Native FS
intrinsics (EF2) do not touch this cost, because the aggregation happens
*after* ingestion regardless of how the file list arrived. EF2-for-*scale*
is misframed; EF2-for-*capability/ergonomics* (no shell dependency, typed
result, no string parsing) still stands on its own merits.

## 2. Method

### 2.1 Programs

Two `.eu` programs, written per `docs/reference/agent-reference.md` +
`docs/appendices/syntax-gotchas.md` (idioms: `io.shell` returns an IO
*action*, only usable inside a `{ :io r: … }.(…)` monadic block and only
with `-I`/`--allow-io`; `str.split-on(re, s)` is regex-based and native).

**`fs_ingest.eu`** — the realistic current idiom, staged into five targets
so each `-t` invocation forces only up to that stage (laziness means later
stages are never touched unless rendered):

```eu
dir: io.args head

` :target
stage-a-shell: { :io r: io.shell("find {dir} -type f") }.(r.exit-code)

` :target
stage-b-string: { :io r: io.shell("find {dir} -type f") }.(r.stdout)

` :target
stage-c-split: { :io r: io.shell("find {dir} -type f") }.(r.stdout str.trim str.split-on("\n") head)

` :target
stage-d-vec: { :io r: io.shell("find {dir} -type f") }.(r.stdout str.trim str.split-on("\n") vec.of vec.len)

` :target
stage-e-agg: { :io r: io.shell("find {dir} -type f") }.(r.stdout str.trim str.split-on("\n") filter(str.ends-with?("[.]txt$")) count)
```

- **(a)** forces only `exit-code` — `io.shell`'s `stdout` field is a lazy
  thunk, so this isolates "run the subprocess" alone.
- **(b)** forces the whole `stdout` string by rendering it directly
  (**not** via `str.len`, which is `count ∘ letters` in the prelude — see
  §3.3, this is itself part of the finding).
- **(c)** splits into a cons-list of lines and forces only `head`;
  `str.split-on` is a native regex-split intrinsic that builds the whole
  list eagerly in one pass, so this pays the full split cost without
  invoking any prelude list function downstream.
- **(d)** builds a `vec` from the line list and takes its length — both
  `vec.of`/`vec.len` are native intrinsics.
- **(e)** the representative aggregation: filter by extension, then count
  — the idiomatic prelude pipeline a user would actually write.

**`prelude_scale.eu` / `count_scale.eu` / `fold_lit_scale.eu` /
`fold_global_scale.eu`** — synthetic micro-benchmarks isolating individual
prelude functions (`count`, `sum`, `filter`) against a plain `range(0,n)`
list, and contrasting them against the literal-lambda `foldl((_+_),0)`
shape from the canonical suite's bench 022, to separate "is this foldl
specifically" from "is this any prelude HOF."

### 2.2 Trees

Three (later four) synthetic flat-with-3-subdirs trees generated under the
scratchpad, files 3–852 bytes of random content, extensions cycling
`.txt/.log/.dat/.csv` (so exactly 1/4 match `.txt`):

| Tree | Files | Generation wall time |
|---|--:|--:|
| `tree_1000` | 1,000 | 1.7s |
| `tree_5000` | 5,000 | 8.4s |
| `tree_20000` | 20,000 | 35.7s |
| `tree_40000` | 40,000 | 68.0s |

All `eu` invocations wrapped in `timeout`, `--heap-limit-mib 12288`, `-I`
(IO permission).

## 3. Results

### 3.1 Deterministic layer (ticks, HeapSyn `-S`) — measured-verified

**The realistic FS pipeline itself** (`fs_ingest.eu`, actual `find` output,
actual path strings):

| Stage | N=1,000 | N=5,000 | N=20,000 | Scaling |
|---|--:|--:|--:|---|
| **(d) shell→string→split→vec** | 20,724 | 76,724 | 286,724 | **linear** (×3.7 per ×5N, ×3.7 per ×4N) |
| **(e) …→filter(ext)→count** | 721,931 | 14,207,431 | 216,184,306 | **quadratic** (×19.7 per ×5N, ×15.2 per ×4N — ideal quadratic is ×25 / ×16) |

Stage (d) is flat/linear in ticks — confirms "shell call cheap, vec build
cheap" directly, on the real pipeline, deterministically. Stage (e) is
quadratic, and by N=20,000 costs **755× more ticks than the entire
ingestion pipeline it's appended to** (216,184,306 vs 286,724).

**Isolating which prelude function is responsible** (`range(0,n)`,
synthetic, HeapSyn ticks):

| Function under test | Shape | N=5,000 | N=10,000 | N=20,000 | Verdict |
|---|---|--:|--:|--:|---|
| `range(0,n) foldl((_+_), 0)` | literal lambda at call site (bench-022 shape) | 1,095,746 | 2,189,496 | 4,376,996 | **linear** |
| `range(0,n) foldl(+, 0)` (written directly, not via `sum`) | global op at call site | 1,095,746 | 2,189,496 | 4,376,996 | **linear**, identical to above |
| `range(0,n) sum` | prelude `sum(l): foldl(+, 0, l)` | 13,614,766 | 52,227,266 | 204,452,266 | **quadratic** |
| `range(0,n) count` | prelude `count(l): foldl({n:•el:•}.(n inc), 0, l)` | 13,554,528 | 52,107,028 | 204,212,028 | **quadratic**, timed out (>60s wall) at N=40,000 |
| `range(0,n) filter(> (n/2)) count` | prelude `filter` (`foldr` + local lambda) then `count` | 17,065,998 | 65,379,748 | 255,757,248 | **quadratic** |
| `range(0,n) map(str.of) vec.of vec.len` | prelude `map` (self-recursive, `f` re-resolved) | 23,002,284 | 89,752,284 | 354,502,284 | **quadratic** |

The pattern is unambiguous: **written directly at the call site with a
literal argument, `foldl` is linear — even with a plain global `+`.**
**Called through any of the prelude's own wrapper functions — `sum`,
`count`, `filter`, `map`, all with identical near-perfect ×4-per-doubling
scaling — it is quadratic.** `sum` and `count` produce numerically
near-identical tick counts at every N (13.6M/52.2M/204.5M vs
13.55M/52.1M/204.2M) despite different lambda shapes (bare global `+` vs a
locally-defined block-anaphora lambda) — the cost is not about *which*
literal is passed, it is about the *indirection through a named function
boundary* re-triggering the env-walk on every recursive step, regardless of
what's being resolved.

**Bonus finding, not asked for but directly relevant:** `str.len` is
defined in the prelude as `count ∘ letters` (`lib/prelude.eu:1035`). It
therefore inherits `count`'s O(n²). `str.len` on a 10,000-line (~49 KB)
`io.shell` result **hangs past a 10 s timeout**; the identical string
merely *rendered* (no `str.len`) takes 34 ms. Any EF2 or FS-adjacent design
work should route "how many bytes/lines" questions around `str.len`/`count`
until CG4 lands.

### 3.2 Wall clock (bytecode engine, default) — measured-single, load 3.9–9.7

| Stage | N=1,000 | N=5,000 | N=20,000 | N=40,000 |
|---|--:|--:|--:|--:|
| (a) shell only | 0.02–0.03s | 0.03–0.04s | 0.04–0.05s | 0.06–0.09s |
| (b) hold string | 0.02–0.03s | 0.03s | 0.05s | — |
| (c) split | 0.03s | 0.03s | 0.05s | — |
| (d) vec build | 0.02–0.03s | 0.03s | 0.05–0.06s | 0.09s |
| **(e) filter+count agg** | 0.04s (7/7 runs) | **0.39–0.40s** (7/7 runs) | **6.69–10.73s** (median 9.55s, 5 runs — this cell coincided with a load spike to 9.7) | **35.79–36.81s** (median 36.46s, 5 runs, tight ~3% spread) |

Stages (a)–(d) stay near the process-startup floor (tens of ms) all the
way to 40,000 files — consistent with the ticks showing them genuinely
linear and small. Stage (e) grows from 0.04s → 0.40s → ~9.55s → 36.46s.
The cleanest two adjacent points (20,000 → 40,000, both well clear of the
200 ms floor, tight spread on the 40,000 cell) give a ×3.81 wall-time
increase for a ×2 increase in N — matching the O(n²) prediction (×4)
closely given the noisy shared machine.

### 3.3 Attribution

Per the dispatch's five-way breakdown:

- **(a) shell/find subprocess:** cheap and ~flat. Direct `find | wc -l` on
  the 40,000-file tree takes 23 ms; `eu`'s stage (a) sits at 60–90 ms,
  consistent with subprocess spawn + `eu` startup overhead, not with the
  subprocess itself scaling badly.
- **(b) holding the stdout string:** cheap — `io.shell`'s `stdout` field is
  a lazy thunk (touching only `exit-code` never materialises it); once
  materialised (rendered), a 49 KB string costs ~34 ms once (native).
- **(c) line-split parsing:** cheap and linear — `str.split-on` is a native
  regex-split intrinsic, ticks scale linearly with input size.
- **(d) building the vec/list structure:** cheap and linear — `vec.of`,
  `vec.len` are native intrinsics; O(1) `len`.
- **(e) the downstream aggregation:** **dominant and quadratic.** By
  20,000 files it is ~150–250× the wall time of everything else combined;
  by 40,000 files, ~400–600×.

**Dominant cost, unambiguously: (e), the aggregation**, not (a)–(d).

## 4. Adjudicating the ROADMAP's three claims

1. **"The shell call is cheap."** ✅ **True, reconfirmed** (measured-verified
   via ticks, corroborated by wall clock). Not a bottleneck at any tested
   scale.
2. **"The `vec.of` build is cheap."** ✅ **True, reconfirmed.** `vec.of`/
   `vec.len` are native intrinsics; linear ticks, flat wall time to 40,000
   files.
3. **"The aggregating slowness is a separate `foldl` defect, handled as a
   bug fix, not here."** ❌ **Not accurate on current master.** It is real
   and current (reproduced directly, ticks and wall clock, on `master` at
   `694784ce`), but it is **not separate and not handled**:
   - It is the *same* mechanism as the canonical suite's documented
     "shared O(n²) env-walk ceiling" for benches 018/019 (class G/H,
     `docs/superpowers/engine-ab/PROTOCOL.md` §6) — re-confirmed present
     and unchanged *today* by a concurrent BV3 re-evaluation
     (`docs/superpowers/reports/2026-07-24-bv3-value-case-reeval.md`,
     019_list_scale ticks within 0.29% of the eu-98zg baseline).
   - The one thing that *is* fixed (PR #1016/#1008, the
     `EU_BLOB_INLINE_CLUSTER`/`tag_combinators` copy-specialisation
     mechanism) is narrow **by construction**: it specialises a `foldl`
     (or similarly self-recursive HOF) call site **only** when a literal
     function argument sits directly at that call site in the unit being
     compiled. It does not propagate through an ordinary named-function
     call boundary. `sum(l): foldl(+, 0, l)` and `count(l): foldl({…}, 0,
     l)` are each exactly this shape *inside the prelude's own source* and
     remain quadratic; `filter` (via `foldr`) and `map` (direct
     self-recursion) show the identical ceiling. This scope is confirmed
     independently and in more depth by bead **eu-gmdl5** ("CG4 selective
     lambda-lifting… `tag_combinators` only targets self-recursive
     combinators… structurally unreachable by construction" for a related
     but distinct population).
   - Practically: **no one writes `foldl(+, 0, list)` by hand to count
     files.** The idiomatic call is `list count`, `list filter(pred)
     count`, `list map(f) sum` — i.e. exactly the calls that are *not*
     covered by the narrow fix. The ROADMAP note's "handled as a bug fix"
     describes the bench-022 case only; it does not describe the
     realistic EF2 aggregation workload.

## 5. A / B / C

- **(A) Eliminate the string round-trip (eager native `io.walk`
  emitting structured values).** Would remove stages (a)–(d)'s already-tiny
  cost (tens of ms at 40,000 files) and would be a genuine ergonomics and
  robustness win (no shell/`find` dependency, typed result, no regex
  line-parsing). **It would not move the dominant cost.** Even if
  `io.walk` returned a native `vec` directly, there is currently no
  `vec.filter`/`vec.map`/`vec.fold` — an aggregation still has to go
  through `vec.to-list` and the same `filter`/`count`/`map`/`sum`
  functions that are quadratic today. Native output alone, without also
  natively implementing the aggregation combinators, buys almost nothing
  at the sizes that matter.
- **(B) Non-materialising / lazy-streamable walk.** Ties to the deferred
  lazy-streams work per project memory. For a *selective* consumer (stop
  after the first match, `take(k)`) this would help by not doing
  unnecessary directory-tree work — a real, orthogonal win. But it does
  **not** address the measured bottleneck: a full-tree aggregation (count
  all files, sum all sizes) still touches every element regardless of
  whether the underlying walk is eager or lazy, and the O(n²) is not "too
  much data touched" — it's a per-recursive-step re-resolution of the
  aggregation function's identity through a named-function call boundary
  (§3.1, §4). Laziness changes *when* work happens, not the shape of the
  aggregation's own cost curve.
- **(C) The cost is a foldl/HOF-resolution defect (general, not
  foldl-specific, not yet fixed for the realistic idiom); native FS would
  not materially help.** **This is the supported verdict.** EF2-for-*scale*
  as currently framed in ROADMAP.md is misframed: the premise "gathering a
  filesystem tree is currently impractical [at scale]" is not supported by
  measurement — ingestion is fine to 40,000 files; what's impractical is
  *aggregating* the result, and that would be exactly as impractical if the
  list of files arrived from a native `io.walk` instead of `find`+parsing.
  Fixing the actual bottleneck is compiler/runtime work already tracked
  under Pillar CG (`eu-2sa6.2` BV3 register frames / `eu-gmdl5` CG4
  selective lambda-lifting spike), not a filesystem-API concern.

**Recommendation to the coordinator:** EF2's capability/ergonomics case
(no shell dependency, typed structured result, no line-parsing brittleness)
stands on its own and is worth doing — but the ROADMAP's EF2 section
should be corrected to stop citing "a separate foldl defect, handled as a
bug fix" as the reason scale isn't EF2's problem, since that defect is
neither separate nor handled for the workloads EF2 would actually serve.
The *real* scale story is: **fix CG4/BV3 first (or in parallel) — that is
what turns `list count`/`list filter(...).count`/`list map(f) sum` linear
for every eucalypt user, not just filesystem consumers** — and re-evaluate
EF2's "success" criterion ("`io.walk` ingests a multi-thousand-file tree
into a structured value in well under a second") accordingly: on current
master, ingestion already clears that bar comfortably; it's the very next
line the user writes (`.filter(...).count`) that doesn't.

## 6. Artefacts

- Programs: `fs_ingest.eu`, `count_scale.eu`, `fold_lit_scale.eu`,
  `fold_global_scale.eu`, `prelude_scale.eu` — scratchpad copies (not
  checked into the repo; paths available on request, reproducible from
  the snippets in §2.1 and the scaling table's shapes in §3.1).
- Synthetic trees: generated fresh under the session scratchpad via a
  small shell script (flat, 3 subdirs, files 3–852 bytes, extensions
  cycling `.txt/.log/.dat/.csv`); not checked in.
- No source files changed. No new intrinsics or prelude changes proposed
  or made.
