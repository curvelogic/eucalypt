# EF1 `do`-monad feasibility spike — `do-bind` over `io.bind` (eu-1tkk.2)

**Date:** 2026-07-25 · **Bead:** eu-1tkk.2 · **Spec:**
`docs/superpowers/specs/2026-07-24-ef1-combined-effect-monad-design.md`
(branch `docs/ef1-do-monad-spec`) · **Prototype:**
`docs/superpowers/spikes/ef1-do-bind-spike.eu` (branch `spike/ef1-do-bind`)

## Verdict

**GO.** The spec's §3 approach — `do-bind`/`do-return` composed over
`io.bind`, with the `{ state, seed }` context riding the IO value channel
under the driver's world-token path — works exactly as designed, on both
engines, with the context surviving thousands of forced-evacuation
collections under `EU_GC_VERIFY=2` + `EU_GC_STRESS=1`. No new syntax, no
driver changes, no intrinsic changes were needed: the prototype is pure
prelude-level eucalypt.

## What was built

A throwaway prototype (`docs/superpowers/spikes/ef1-do-bind-spike.eu`)
containing, per spec §3/§4:

- `do-ret(v, c): io.return({ value: v, ctx: c })`
- `do-bind(m, f, c): io.bind(m(c), resume(f))` with the named
  continuation `resume(f, r): f(r.value)(r.ctx)`
- Three lifts: `do-lift-io` (via `io.map`), `do-lift-state`,
  `do-lift-random` (both via `io.return` + context re-threading)
- A `do` namespace built with `monad{bind: do-bind, return: do-ret}` and
  `monad:` metadata, which **registers `{ :do … }` block metadata in the
  same unit** — the sugar form worked first try
- Two equivalent pipelines exercising **all three effects**:
  1. an explicit nested `do-bind` chain (core proof, no sugar), and
  2. the `{ :do … }` block-metadata form,
  each doing: shell append → state `put` → shell append → state `modify`
  → random `float` → shell `cat`, run via `do.run(pipeline, ctx0)` with
  `ctx0: { state: { count: 0 }, seed: random.stream(42) }`.

Effects are observable (appends to a log file, then `cat`), so ordering
and single-execution are asserted by the output itself. Two independent
allocation-churn phases are forced *mid io-run loop* (via string
interpolation inside successive shell-command spec blocks) so GC runs
while the `do` context sits inside a suspended continuation closure that
the driver has GC-stashed.

## Evidence (commands + observed results)

Binary: clean `cargo build --release` at `ce4128ca` (+ spike `.eu` only —
no Rust changes; `cargo test` baseline untouched). All runs from repo
root; every completing configuration produced **byte-identical output**:

```text
explicit:
  value:
    cat-out: "alpha-247500\nbeta-252500\n"
    rand: 0.7415648787718233
  state: { count: 0, alpha: 100 }
block:
  value:
    cat-out: "alpha-247500\nbeta-252500\nalpha-247500\nbeta-252500\n"
    rand: 0.7415648787718233
  state: { count: 0, alpha: 100 }
```

- effects fired **once each, in order** (log file holds exactly
  alpha, beta per pipeline run; `cat` snapshots agree);
- state threaded (`alpha: 100` = `put` of exit-code 0 then `modify (+ 100)`;
  untouched `count` preserved);
- seed threaded (deterministic float from `random.stream(42)`,
  identical across every engine and GC configuration).

| Run | Command core | Collections | Result |
|---|---|---|---|
| bc baseline | `timeout 60 ./target/release/eu -I --heap-limit-mib 12288 <spike>` | 0 | exit 0, reference output |
| hs baseline | same + `EU_HEAPSYN=1` | 0 | exit 0, `diff` clean → **ENGINES MATCH** |
| bc GC pressure | `--heap-limit-mib 128` (earlier, larger-churn variant) | 5,961 | exit 0, output matches |
| hs GC pressure | `EU_HEAPSYN=1 --heap-limit-mib 128` (same variant) | 12,211 | exit 0, output matches |
| bc verify+stress | `EU_GC_VERIFY=2 EU_GC_STRESS=1 --heap-limit-mib 72` | 2,647 | exit 0, no verifier panic, output matches |
| hs verify+stress | `EU_HEAPSYN=1 EU_GC_VERIFY=2 EU_GC_STRESS=1 --heap-limit-mib 168` | 1,691 | exit 0, no verifier panic, output matches |

`EU_GC_STRESS=1` forces SelectiveEvacuation on **every** collection, and
`EU_GC_VERIFY=2` runs the full multi-checkpoint structural verification
(header/pointer validity, line consistency, forwarding-pointer lifecycle,
block-list integrity) after each phase — so the suspended IO thunk
carrying `{ state, seed }` survived ~4,300 verified evacuating
collections across the two engines with zero faults and identical
results.

Note `EU_GC_STRESS=1` does **not** force collections (it only hardens the
ones that happen); collections trigger near `--heap-limit-mib`
(`policy_requires_collection`, `src/eval/memory/heap.rs:1771`). The
pressure runs therefore use a deliberately small heap limit — that is
what makes the GC evidence real rather than a zero-collection no-op.

## Blockers found: none for EF1. Risk register

| # | Risk / finding | EF1 impact | Mitigation |
|---|---|---|---|
| 1 | **Spec §8.1 (load-bearing risk): resolved.** `ctx` on the IO value channel composes correctly under the driver's stash/rooting discipline (`src/driver/bytecode_io_run.rs` IoBind path applies `cont(result, world)`; `resume(f)` slots in as that cont). | none | — |
| 2 | **Spec §3/§4 sketches contain two literal traps** (spec explicitly disclaims exact spelling, §8.2): (a) `pair-ctx(ctx, v): { value: v, ctx: ctx }` is the `{x: x}` self-reference gotcha — the parameter must not share the field name (`pair-ctx(c, v): { value: v, ctx: c }`); (b) `do.eval`'s sketch `io.map(a(ctx0), (.value))` has the argument order backwards for the prelude (`io.map(f, action)`) and `(.value)` is not a section form — a named projection is needed. | low — implementation papercuts | Implement from the prototype, not by transcribing spec code blocks; fix the two sketches when the spec lands. |
| 3 | **GC-pressure grind (pre-existing, NOT do-specific).** Any workload whose live set approaches `--heap-limit-mib` degrades brutally: the policy (`blocks_allocated >= limit && recycled < 25%`) triggers near-continuous full-heap marks (observed 30–100× slowdowns; the identical plain-`:io` control `ef1-control-io.eu` behaves the same, as does the pure control). No incorrectness observed — every run either completed with correct output or was killed by timeout mid-progress. | none for EF1 | File a bead for the near-limit collection policy (futile-collection backoff / emergency-growth or a hard error instead of grinding). |
| 4 | **`range(N) map(f) foldl(+, 0)` is pathological at N ≳ 30k** (n=10k instant, n=30k > 30s, unlimited heap, both `-e` and unit). Plain `range(0, 400000) foldl((_ + _), 0)` (bench 022) is linear post-PR #1016, so the interposed `map` defeats the HOF fold demand-analysis fusion and rebuilds the O(n²)-ish chain. Found while sizing the spike's churn. | none for EF1 | File a bead for demand analysis / fusion through `map` into `foldl`. |
| 5 | **Naming**: `do` works fine as an identifier and namespace; `{ :do … }` parses and desugars with same-unit registration. | none | Rename freely later per spec §9. |

## Recommendation

The spec's approach **holds without revision**: bind/return over
`io.bind` with world-token threading needs no runtime support, and the
delegation surface (`do.st`, lifted shell, derived combinators via
`monad(do)`) behaves as designed. Proceed to implementation per spec §4,
using the prototype as the reference for exact prelude spelling. Suggested
follow-ups (not blockers): beads for risk-register items 3 and 4, and
harness tests per spec §7 including a GC-pressure variant of the parity
test.

## Artefacts

- `docs/superpowers/spikes/ef1-do-bind-spike.eu` — the prototype (both
  pipeline forms + churn instrumentation)
- `docs/superpowers/spikes/ef1-control-pure.eu` /
  `docs/superpowers/spikes/ef1-control-io.eu` — the controls that
  isolate finding 3 as pre-existing
- Branch `spike/ef1-do-bind` (spike only — deliberately **not** a PR)
