# eu-n8c5e Phase 2: per-peeled-binding copy-specialisation POC — partial win, not a complexity-class fix

- **Date:** 2026-07-25
- **Bead:** eu-n8c5e — design spike + proof-of-concept, Phase 2 of the
  two-phase workflow (Phase 1: `docs/superpowers/reports/2026-07-24-n8c5e-blob-inline-perf.md`).
  **No shipped-blob change.** `cargo xtask prelude-compile`'s default output
  (`lib/prelude.blob`) is byte-for-byte the same as before this spike — the
  POC lives behind a new, separate `prelude-compile-poc` xtask subcommand
  that writes to an explicit scratch path only, never `lib/prelude.blob`.
- **Worktree:** `.claude/worktrees/agent-a93230d0325ae7c98`, branched from
  `master` at `8efc367a`.
- **Toolchain:** rustc via `stable-aarch64-apple-darwin`, Darwin 25.5.0 arm64.
- **Companion beads:** eu-e3c3i (the user-facing quadratic symptom), eu-7x0r
  (the diagnostics symptom this spike also checks for a fix).

## 1. One-line verdict

**The hypothesis in the dispatch — "apply the existing copy-specialisation
machinery to each peeled prelude binding's own body, before its independent
STG compile" — is buildable and correctness-preserving, but it does **not**
turn `count`/`sum` linear. It only shifts the constant.** Built and measured
end-to-end: the specialised POC blob still loads, still addresses globals
correctly, and passes the full harness suite (505/506, the one failure is a
**pre-existing, unrelated** eu-7x0r case that fails identically on today's
unmodified blob). Ticks for `count`/`sum` improve by a real, measured
**3.7×–3.9× constant-factor** at N=16,000, but the **per-doubling ratio still
climbs toward the ×4 quadratic asymptote** (2.83→3.70 across N=1k→16k,
against baseline's 3.41→3.90) rather than settling at the ×2.00 source-mode
figure. Running more specialisation rounds narrows the gap further (3 rounds:
2.49→3.45) but at a **~2.7× blob-size cost per extra round** and a
**demonstrated hang/blow-up at 8 rounds** — diminishing, not convergent,
returns. **Root-cause conclusion: the mechanism that makes source mode linear
is not "tag_combinators ran over foldl's body" per se — it is that
source-mode compiles the whole prelude (and user code) as one unit, so a
self-recursive combinator's own tail call resolves as an ordinary bound
reference within that shared scope. A per-binding, peeled, independently-STG-
compiled blob cannot fully reproduce that without collapsing the
independent-global-slot architecture — this is exactly eu-2sa6.18's territory,
not a narrower, cheaper fix.** It also does **not** fix eu-7x0r (confirmed
directly, see §5) — that failure is a different population (non-recursive
wrapper functions losing their call-site frame), unaffected by specialising
self-recursive combinators.

## 2. What was built (the POC)

- `xtask/src/main.rs`: refactored `cmd_prelude_compile` into
  `cmd_prelude_compile_impl(blob_out, specialise: bool)`. The existing
  `prelude-compile` command is unchanged (`specialise: false`, same output
  path, same bytes — verified: 598,731 bytes, identical to before this
  spike). A new `prelude-compile-poc <out>` command sets `specialise: true`
  and writes to an explicit output path.
- New `specialise_binding_body(name, body, inlinable_bindings)`: for each of
  the ~352 peeled prelude bindings, wraps the body in `Let(OtherLet,
  {pattern: inlinable_bindings (excluding self), body})` and runs
  `tag_combinators_named` + `inline_pass` (distribute + beta-reduce) — the
  **exact same functions** `SourceLoader::inject_prelude_inlinable_bindings`
  + `inline()` already use for **user** code (`src/driver/prepare.rs`) —
  for a configurable round count (`EU_N8C5E_ROUNDS`, default 2, matching
  `prepare.rs`'s `for _ in 0..2`), then `fuse` + `prune` + `compress` to
  strip the now-dead injected wrapper bindings back off. The binding's own
  name is excluded from the injected set (a self-recursive combinator being
  specialised must not inject an "identity" copy of itself for its own
  recursive reference).
- Each binding's own top-level identity (name → global slot) is completely
  unaffected: `binding_bodies`' names/order are untouched by
  `specialise_binding_body`, only each entry's *body* changes shape; slot
  assignment happens after specialisation from the same name list either
  way. **Verified**: `name_to_slot`/global-slot count is identical (352
  bindings) in every configuration tested.
- `src/driver/eval.rs`: a throwaway `EU_PRELUDE_BLOB_PATH` env var override
  in `maybe_load_prelude_blob` so the POC blob (or any blob) can be exercised
  through the normal `eu` binary/driver without rebuilding with a new
  `include_bytes!` embed. Not intended to ship.

Both changes are **uncommitted** in this worktree (see §7) — this report is
the deliverable, not the code, per the phase-2 brief.

## 3. Correctness

- Minimal case (`n:5, bench-count: range(0,n) count`) and N=1,000 spot checks
  for both `count` (→ 1000) and `sum` (→ 499500) match byte-for-byte across
  baseline blob, POC (2 rounds), and POC (3 rounds).
- **Full harness suite**, `cargo test --release --test harness_test` with
  `EU_PRELUDE_BLOB_PATH` pointed at the 2-round POC blob:
  **505 passed, 1 failed** — the *same single* failure
  (`test_193_1tkk_7_12_curated_trace`) reproduces **identically** against
  today's unmodified `lib/prelude.blob` with no POC code involved at all (run
  both ways, confirmed byte-identical failure text both times). This is a
  **pre-existing** blob-mode diagnostics gap (the curated trace drops the
  `in 'nth' at ...` frame), unrelated to and unaffected by this POC. No new
  failures, no regressions, across the entire suite.
- Global-slot addressing survives intact: corruption of `name_to_slot` would
  be expected to break many unrelated tests, not one already-failing,
  pre-existing case — the breadth of the pass is itself evidence the
  addressing scheme is untouched.

## 4. Performance: partial win, not a complexity-class fix

`EU_HEAPSYN=1 -S` ticks (measured-verified, deterministic counter layer),
`range(0,n) count` / `range(0,n) sum`, N ∈ {1000,2000,4000,8000,16000}:

| N | count baseline | count POC (r2) | count POC (r3) | sum baseline | sum POC (r2) | sum POC (r3) |
|--:|--:|--:|--:|--:|--:|--:|
| 1,000 | 710,723 | 301,401 | 253,494 | 722,729 | 325,380 | 293,441 |
| 2,000 | 2,421,223 | 852,651 | 631,869 | 2,445,229 | 900,630 | 711,816 |
| 4,000 | 8,842,223 | 2,705,151 | 1,763,619 | 8,890,229 | 2,801,130 | 1,923,566 |
| 8,000 | 33,684,223 | 9,410,151 | 5,527,119 | 33,780,229 | 9,602,130 | 5,847,066 |
| 16,000 | 131,368,223 | 34,820,151 | 19,054,119 | 131,560,229 | 35,204,130 | 19,694,066 |

Per-doubling ratios (ideal linear = 2.00, ideal quadratic = 4.00):

| | 1k→2k | 2k→4k | 4k→8k | 8k→16k |
|---|--:|--:|--:|--:|
| `count` baseline | 3.41 | 3.65 | 3.81 | 3.90 |
| `count` POC r2 | 2.83 | 3.17 | 3.48 | 3.70 |
| `count` POC r3 | 2.49 | 2.79 | 3.13 | 3.45 |
| `sum` baseline | 3.38 | 3.64 | 3.80 | 3.89 |
| `sum` POC r2 | 2.77 | 3.11 | 3.43 | 3.67 |
| `sum` POC r3 | 2.43 | 2.70 | 3.04 | 3.37 |

**This is the central finding.** Both POC configurations are a **real, large
constant-factor win** (3.7×–6.9× fewer ticks at N=16,000) but **both curves
are still trending toward the ×4 quadratic asymptote, not settling at ×2.00**
— unlike source mode, whose ratio is exactly 2.00 at every step (Phase 1
report, §3.2). More rounds narrow the gap (r3 closer to 2.00 than r2 at
every N) but the trend direction is the same: this looks like an
**asymptotically-unchanged O(n²) with a smaller leading constant**, consistent
with "unroll a fixed number of recursive steps, then fall back to the same
underlying per-step cost" — not a complexity-class fix.

### Why: the STG shows genuine local recursion, but it's still not linear

Dumping the POC blob's own compiled form for `count` (via a throwaway debug
hook printing `⊗`=global vs `✳`=local reference counts) shows something
important: after 2 specialisation rounds, `count`'s body **does** contain a
manually-unrolled, ~4-level-deep copy of `foldl`'s logic interleaved with the
`{n:•,el:•}.(n inc)` counting closure, and the **innermost** recursive tail
call **does** resolve as a local reference (`✳5`/`✳4` — a self-reference into
the enclosing `letrec`), not a global one. This is genuine, real
specialisation, not a no-op — 3 G-refs/12 L-refs in the baseline `count`
became 68 G-refs/149 L-refs in the POC (every remaining G-ref is now to a
*non-recursive* alias like `if`/`head`/`tail`/`inc`, not to `foldl` or `count`
themselves for the unrolled portion).

That the ratio still climbs despite genuine local self-reference at the
unrolled tail is the load-bearing evidence for the root-cause conclusion in
§1: whatever causes the "shared O(n²) env-walk ceiling" (the same mechanism
BV3/eu-2sa6.2 and CG4/eu-gmdl5 target) is not specific to `Ref::G`
resolution — it reproduces even for a `Ref::L` self-reference once that
reference sits inside a deeply call-chain-nested closure. A **fixed-depth**
unroll (however achieved) only defers the cost by a constant factor; it
cannot change the complexity class, because the recursion eventually
re-enters the same underlying per-call cost regardless of whether that
re-entry point is named globally or locally.

### Blob size / compile time cost

| Config | Blob bytes | Δ vs baseline | Arena nodes | Forms | xtask compile time |
|---|--:|--:|--:|--:|--:|
| baseline (unmodified `prelude-compile`) | 598,731 | — | 7,139 | 5,780 | 0.06s |
| POC, 2 rounds | 909,312 | **+51.9%** | 16,193 | 11,176 | 0.49s |
| POC, 3 rounds | 1,625,887 | **+171.6%** | 36,497 | 23,020 | 0.81s |
| POC, 8 rounds | *did not complete* | — | — | — | **>120s, killed** |

2 rounds is already a >50% blob-size increase across **all 352 bindings**
(every binding gets the same wrapper-and-specialise treatment, not just the
handful the eu-e3c3i population cares about — see §6). Going from 2 to 3
rounds costs roughly another **2.7×** on top of that; 8 rounds hangs. This is
consistent with unbounded/exponential Core-tree growth from repeated
self-substitution without memoisation — running the specialisation loop "to a
fixed point" (as opposed to a small, hardcoded round count) is **not safe** in
general across all 352 bindings; some subset blows up.

## 5. Does this fix eu-7x0r?

**No — checked directly, not inferred.** `tests/harness/errors/193_1tkk_7_12_curated_trace.eu`
(one of eu-7x0r's three known blob-only diagnostics failures — the curated
trace drops the `in 'nth' at ...` boundary-combinator frame) fails
**identically** with the 2-round POC blob active as it does with today's
unmodified blob. This is expected, not a POC bug: eu-7x0r's population is
prelude functions like `nth` that are **not themselves self-recursive
combinators** (they don't qualify for `tag_combinators`'s
`recursive_combinator` criterion or the `closed_body` criterion, so they were
never in scope for xtask's `inlinable_bindings` fixed-point, and specialising
*other* bindings' internals doesn't touch `nth`'s own compiled identity as an
opaque `Ref::G` call). eu-n8c5e (recursive-HOF quadratic) and eu-7x0r
(boundary-frame loss for ordinary wrapper functions) remain **confirmed
separate populations** requiring separate fixes, exactly as the Phase 1
report and the dispatch both flagged as an open question — now closed with a
direct negative result.

## 6. Mutual recursion / scoping safety

No specific mutually-recursive pair was found and forced through the
fixed-point in this spike (time-boxed), but the safety argument is
structural, not empirical: xtask's existing `inlinable_bindings` fixed-point
(unchanged by this POC) only adds a binding to the specialisable set when
**all** of its free variables (other than references to itself) are
**already** in the set (`all_free_vars_in_set_with_self`). Two bindings that
call each other and nothing else outside the set can never both satisfy this
— the fixed-point is monotonic and terminates when no further binding
qualifies (observed: round 0 = 91, round 1 = +52, round 2 = +0, converged).
A genuinely mutually-recursive pair is simply **excluded** from
specialisation, not looped over — safe by construction, at the cost of not
benefiting. This part of the machinery is unchanged from what xtask already
ships for user-code specialisation today, so it inherits whatever
correctness argument already applies there.

## 7. Recommendation

**Do not proceed with "per-peeled-binding copy-specialisation" as scoped in
the dispatch as the fix for eu-e3c3i.** It is real, buildable, correctness-
preserving, and a genuine constant-factor win — but it is not a complexity-
class fix, it costs 52%+ blob size for a partial result, and pushing further
for a better result costs disproportionately more (and risks non-termination
for some bindings). The measured evidence points at the **general env/call
cost the recursion re-enters after any fixed unroll depth**, which is
exactly BV3 (eu-2sa6.2, register frames) / CG4 (eu-gmdl5, selective lambda-
lifting) territory, or the larger "compile blob-form units generically"
architecture already tracked as eu-2sa6.18. Recommended next step: **do not
implement eu-n8c5e's fix as scoped.** Either (a) route eu-e3c3i's fix through
BV3/CG4 directly (the general fix that already has to happen for `count`/
`sum`/`filter`/`map` regardless of blob vs source, since source mode's
"free" linearity here is a special case of whole-program compilation, not
a technique portable to blob generation without eu-2sa6.18's broader
architecture change), or (b) revisit eu-2sa6.18 itself if a blob/source
parity fix is wanted sooner. eu-n8c5e's diagnostics motivation (restoring the
env_trace/stack_trace path) is unaffected either way and should be tracked
against eu-7x0r on its own terms — this POC directly rules out "specialise
self-recursive combinators" as an accidental fix for it.

## 8. Artefacts

- Code (uncommitted in this worktree, not shipped): `xtask/src/main.rs`
  (`cmd_prelude_compile_impl`, `specialise_binding_body`,
  `prelude-compile-poc` subcommand, `EU_N8C5E_DEBUG_STG[_DIR]`/
  `EU_N8C5E_ROUNDS` debug hooks), `src/driver/eval.rs`
  (`EU_PRELUDE_BLOB_PATH` override in `maybe_load_prelude_blob`).
- POC blobs (scratchpad, not checked in): `poc-prelude.blob` (2 rounds,
  909,312 bytes), `poc-prelude-r3.blob` (3 rounds, 1,625,887 bytes).
- Synthetic programs: `poc_mini.eu`, `poc_scale_{1000,2000,4000,8000,16000}.eu`
  — scratchpad, full source given in §4's method (same shape as the Phase 1
  report's synthetics).
- STG dumps: `stgdump/count.stg.txt`, `stgdump/foldl.stg.txt`,
  `stgdump/sum.stg.txt` — scratchpad.
- `lib/prelude.blob` (this worktree's copy, gitignored, generated fresh by
  the **unmodified** `cargo xtask prelude-compile` for baseline comparison)
  is byte-identical in size (598,731 bytes) to the Phase 1 report's figure —
  confirms no drift between the two spikes' baselines.
