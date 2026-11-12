# AoC 2026 preparation — plan

- **Status:** Plan of record (owner decisions to 2026-11-12)
- **Date:** 2026-11-12
- **Epic:** eu-oaz72 (with eu-ioz21 for grammar consolidation)
- **Roadmap:** [`ROADMAP.md`](../../../ROADMAP.md) §6.3 (0.15) and Pillar DS

The beads are the source of truth for scope and acceptance criteria. This
document records the shape of the work — lanes, order, constraints and the
decisions behind them — and links beads rather than restating them.

## 1. Goal and scope

Make eucalypt ready for Advent of Code 2026, which starts on **1 December
2026**: no wrong results or panics on idiomatic solutions, loops and
searches that run in bounded memory, a sorted dictionary that doubles as a
priority queue, and documentation and editor tooling good enough to work in
daily under time pressure.

- **0.15 is deferred** (owner decision 2026-11-11) to carry this work; the
  single-engine change already on master ships with it.
- **No fallbacks.** This is plan A only. If it fails, we reconsider then;
  no contingency branches are built in advance.
- Acceptance for the epic: every P0/P1 child closed before 1 December; the
  rest triaged.

## 2. Principles

1. **Every heap value's storage lives in the GC heap.** No Rust-owned
   allocations (`Rc`, `Vec`, `HashSet`, `String`, …) inside GC heap objects,
   no finalisers, and no `im`/`rpds`/`imbl`. This supersedes the
   finaliser-table option in ADR-001 (see
   [ADR-002](../../development/architectural-decisions.md)).
2. **GC and memory bugs are always P0** when found.
3. **AoC-specific helpers live outside this repository**, in a separate AoC
   helper library kept with the AoC 2026 solutions repository. They move to
   the prelude or to intrinsics only when they are generic or a measured
   performance case justifies it.
4. **Diagnostic hints must be evidenced.** A hint is shown only when the
   source gives evidence for it, or it states a fact about what was parsed
   or called (eu-oaz72.28, eu-oaz72.29).
5. **Collection constructor naming.** `vec.of` and `set.of` are the primary
   constructors, with `vec.from-list` / `set.from-list` as aliases; the new
   dictionary follows the same pattern (`dict.of`, alias `dict.from-list`).

## 3. Lanes

Work runs in parallel lanes by code area (bead labels `lane-*`). PRs within
a lane are sequential, to avoid file conflicts; lanes run concurrently.

| Lane | Main files | Beads, in order |
|---|---|---|
| compiler | `src/eval/stg/compiler.rs`, `src/core/` | eu-oaz72.34 → eu-oaz72.36 → eu-oaz72.29 |
| analysis | `src/core/analyse_demand.rs`, `src/core/demand.rs`, `lib/prelude.eu` | eu-oaz72.1 → eu-oaz72.27 |
| gc | `src/eval/memory/heap.rs` | eu-oaz72.4 — implement now; acceptance measured after eu-oaz72.34 and eu-oaz72.27 |
| native-storage | `src/eval/memory/{set,vec,ndarray}.rs`, `src/eval/stg/vec.rs` | eu-oaz72.5 → eu-oaz72.32 → eu-oaz72.30 |
| dict | new `src/eval/memory/` and `src/eval/stg/` modules | eu-oaz72.21 — design note now, implementation after eu-oaz72.5; eu-oaz72.22 folded in |
| strings | `src/eval/stg/printf.rs`, string intrinsics, `lib/prelude.eu` | eu-oaz72.31 early (migrates ~36 call sites) → eu-oaz72.25 → eu-oaz72.13 → eu-oaz72.11 |
| numbers | number intrinsics, `lib/prelude.eu` | eu-oaz72.35 → eu-oaz72.12 → eu-oaz72.24 → eu-oaz72.10 |
| prelude | `lib/prelude.eu` | eu-oaz72.26 |
| parser-lsp | `src/syntax/rowan/`, `src/driver/lsp/`, formatter | eu-oaz72.2 → eu-oaz72.3 → eu-oaz72.6 → eu-oaz72.9 |
| diagnostics | `src/eval/error.rs`, diagnostic renderers | eu-oaz72.28 |
| emacs | `editors/emacs/` | eu-oaz72.7 → eu-ioz21.3 → eu-oaz72.19 → eu-oaz72.8 |
| tree-sitter | `editors/tree-sitter-eucalypt/` | eu-oaz72.18 → eu-ioz21.5 → eu-oaz72.14 |
| vscode | `editors/vscode/`, consistency-check script | eu-ioz21.1 → eu-ioz21.2 |
| docs | `ROADMAP.md`, `docs/` | eu-oaz72.33 → eu-oaz72.17 → eu-oaz72.16 (skeleton now) → eu-ioz21.4 (after eu-oaz72.18) |
| profiler | `src/driver/`, `docs/reference/cli.md` | eu-oaz72.37 |
| examples | `examples/aoc25/` | eu-oaz72.15 now; eu-oaz72.38 last |
| external | the AoC 2026 solutions repository; the web playground | day template, runner, highlighted-HTML pages (via eu-oaz72.18), helper library (grid, search, cycle detection, `str.ints`, radix parsing); the web playground's eu-ioz21.6 after eu-ioz21.5 |

### Waves

- **Wave 1 (start now):** eu-oaz72.34, eu-oaz72.1, eu-oaz72.4, eu-oaz72.5,
  eu-oaz72.21 (design note), eu-oaz72.31, eu-oaz72.35, eu-oaz72.26,
  eu-oaz72.2, eu-oaz72.28, eu-oaz72.7, eu-oaz72.18, eu-ioz21.1,
  eu-oaz72.17, eu-oaz72.37, eu-oaz72.15, and the external lane.
- **Later waves** follow within each lane in the order above: a lane's next
  bead starts when its previous PR merges.

### Cross-lane orderings

- eu-oaz72.34 (constant-stack loops) is the prerequisite for eu-oaz72.4's
  measurements and for eu-oaz72.29; eu-oaz72.27 follows eu-oaz72.1.
- eu-oaz72.4 is implemented now, but its acceptance numbers are taken after
  eu-oaz72.34 and eu-oaz72.27 land: a volume-based trigger only pays off
  once the stack stops pinning the heap.
- eu-oaz72.5 precedes eu-oaz72.32, eu-oaz72.30 and the dict implementation
  (eu-oaz72.21).
- eu-oaz72.18 precedes eu-ioz21.4; eu-ioz21.5 precedes eu-ioz21.6;
  eu-oaz72.7 precedes eu-ioz21.3.
- eu-oaz72.38 (re-measure and revisit the 2025 solutions) depends on
  eu-oaz72.4, eu-oaz72.5, eu-oaz72.21, eu-oaz72.27, eu-oaz72.34,
  eu-oaz72.35 and eu-oaz72.36, and comes last.

### Post-December

Labelled `post-december` and not on the critical path: eu-oaz72.23
(persistent vector), eu-kc98.1 (mutable storage / transients / memo
design), eu-kc98.2 (bignum design), eu-oaz72.8 (Emacs fidelity),
eu-oaz72.14 (tree-sitter mis-parses), eu-oaz72.20 (WASM API), eu-oaz72.38
(2025 revisit).

## 4. Constraints

- **New intrinsics and prelude API** (eu-oaz72.21, eu-oaz72.24,
  eu-oaz72.25, eu-oaz72.26, eu-oaz72.30, eu-oaz72.31, eu-oaz72.32) are
  specified in advance by this plan's decisions (§6) and the owner
  decisions on their beads. Under `REVIEW.md`'s *Specified in advance*
  rule they merge after two independent reviews, labelled `api-review`
  for the owner's review before release. A deviation, or a choice this
  plan and the bead leave open, goes to the owner instead.
- **GC area.** PRs for eu-oaz72.4, eu-oaz72.5, eu-oaz72.21 and eu-oaz72.30 need two independent
  `rig-review`s recorded on the PR, and must pass `EU_GC_VERIFY=2`,
  `EU_GC_POISON=1` and `EU_GC_STRESS=1`.
- **Editor grammars.** Feature PRs do not add new prelude names to the
  tree-sitter, TextMate or Emacs name lists; the grammar lanes batch them.
- **Keep `lib/prelude.eu` edits small** — several lanes touch it.
- **Performance claims** follow
  [`docs/superpowers/engine-ab/PROTOCOL.md`](../engine-ab/PROTOCOL.md), with
  a confidence label on every figure. `par-*` forks by default, so
  sequential measurements set `EU_PP_THRESHOLD=999999999`.

## 5. Key evidence

All figures are **measured-single** (one run, one machine) and are recorded
in the beads.

- **GC collects only at the heap limit** (eu-oaz72.4). A 1e6-step loop does
  0 collections and allocates ~1.7 GB; 2025 day04/08/09/10 part 2 are
  OOM-killed in a 3 GB container. Under `--heap-limit-mib 1024` the same
  loop runs 39,326 collections and takes ~50 s against 3.7 s unbounded.
  The "0 collections" of the AoC 2025 profiles reflects this policy, not
  an absence of garbage.
- **Native storage leaks** (eu-oaz72.5). Set, vec and n-d array storage is
  Rust-owned and never freed: set add/remove churn reaches 1.79 GB RSS
  under a 512 MB managed heap; building a set by repeated `set.add` is
  O(n²).
- **Loops through `if`/`then` use O(n) stack** (eu-oaz72.34): 52–163
  ticks per iteration and a maximum stack of ~3 million frames at 1e6
  iterations; even a strict accumulator grows one frame per iteration.
- **`foldr`-built lists lose sharing** (eu-oaz72.36): a second traversal of
  a `filter` result costs 2× the ticks (15.6 M → 30.4 M).
- **`range(b, e)` with `e < b` hangs** (eu-oaz72.35), which stalls AoC 2025
  day10 part 2 on 26 of 185 machines.
- **2025 re-analysis.** The stack retention of eu-oaz72.34 pins the heap in
  every 2025 out-of-memory case, so it is the prerequisite for the GC
  policy fix to pay off. The dict is what the remaining slow parts need:
  day11's DP table (block merge per node) and day04's worklist of
  neighbour counts.

## 6. Decisions by area

**Dict (eu-oaz72.21; eu-oaz72.22 folded in).** A GC-native persistent
**ordered tree** (nodes are traced GC heap objects), opt-in and independent
of blocks. Values are stored lazily, so self-referential memo tables work.
Keys are primitives or lists of primitives, with a total order across types.
Iteration is sorted by key. `min`, `max`, `pop-min`, `pop-max` and
`range(lo, hi)` make it the priority queue (key `[priority, seq]`). Typed
`dict(k, v)` in the checker from the start; renders as a mapping in key
order. Sets are rebuilt on the same nodes, making add/remove O(log n). A
CHAMP may follow later, as a separate decision, if a hash structure is
needed (possibly for blocks, ROADMAP DS1).

**Strings.** `str.format(spec, x)` with pipeline-friendly argument order and
a `%b` specifier; `str.fmt` deprecated (eu-oaz72.31). `str.matches-with(re)`
gives all matches with groups (eu-oaz72.26). Code-point naming:
`str.code-points` / `str.from-code-points` and `ch.code-point` /
`ch.of-code-point`, plus `str.md5` (eu-oaz72.25).

**Numbers.** `mulmod`, `powmod` and `modinv` compute in i128 internally;
values stay i64 (eu-oaz72.24). Integer `^` stays an integer and raises a
numeric-overflow error, matching `+` and `*` (eu-oaz72.12). Arbitrary
precision is a separate design item (eu-kc98.2).

**Strictness.** `foldl` is unchanged; `foldl-strict` is added, and demand
analysis learns to prove self-recursive accumulators strict (eu-oaz72.27).

**Arrays.** No new array structure for December: Intcode-scale memory uses
an int-keyed dict. A GC-native persistent vector follows later
(eu-oaz72.23); O(1) in-place update for 1e7+ updates is the eu-kc98.1
design, gated on measured loop throughput.

**Grammars (eu-ioz21).** Consolidate to three highlighters: the rowan parser
(LSP semantic tokens) as the reference, tree-sitter with `highlights.scm`
for editors, docs, shared HTML and the web playground, and TextMate for VS
Code, held consistent by a CI check. highlight.js and the Emacs mode's own
queries are retired.

**Web playground (eu-ioz21.6).** It keeps running small examples written
and tested in the playground itself, as it can today through the WASM
evaluate API, and gains tree-sitter highlighting. AoC solutions it renders
from public repositories through `#gh=` links are shown highlighted and
read-only; running those (they need puzzle inputs and imports) is out of
scope, so the WASM API's extra-file imports (eu-oaz72.20) are
post-December.
