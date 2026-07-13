# Blob prelude string/list penalty — predecode-engine profiling (eu-7xvv)

> **CORRECTION (2026-07-13, eu-qm7f):** the "deep runtime lexical-environment
> chains" mechanism this report treats as confirmed (§1, §5) has since been
> **refuted by direct measurement**. A follow-on spike instrumented the
> shared `EnvironmentFrame::get` walk with a hop-count histogram and found
> runtime chain depth shallow and near-identical between blob and source on
> both 018 and 019 (max depth 2-3, mean <1, lookup counts within ~0.3% of
> each other) — not deeper under blob, contrary to this report's central
> claim. See `docs/superpowers/reports/2026-07-13-env-walk-depth.md` for the
> full measurement. **What still stands:** the profile-share evidence below
> (§4 — `enter_local`'s self-time share genuinely grows under blob) is not
> contradicted; only the *causal explanation* (longer walks) is. The
> surviving candidate is a locality/per-hop cost, currently **projected**,
> not measured. This banner is a correction notice only — the body below is
> kept as the historical record and has not been rewritten; read it with
> the above in mind.

- **Date:** 2026-07-13
- **Bead:** eu-7xvv (diagnose the MECHANISM of the blob-prelude per-tick string
  penalty under the pre-decoded engine). Report-only; no code changes.
- **Worktree/branch:** `/tmp/eu-stopwatch-7xvv`, `spike/blob-string-profile` off
  `master` at `036258ba` (merge of PR #1002, pre-decoded execution IR).
- **Toolchain:** rustc 1.97.0 (`stable-aarch64-apple-darwin`), macOS 26.5.1
  (Darwin 25.5.0), Apple aarch64.
- **Machine:** quiet throughout this session — checked before every timed round
  via `ps -Ao pcpu,comm -r`; no `mediaanalysisd`/Spotlight/other-session
  contention observed at any check.
- **Scope:** this spike does **not** re-litigate eu-2sa6.12's two refuted
  generation-time fixes (per-binding `reflatten`, the `named_demand_sigs`
  user→prelude boundary — both implemented, measured, and refuted in PRs #994/
  #997). It profiles the **pre-decoded engine** (`EU_PREDECODE=1`, landed in
  PR #1002, never profiled before in either prelude config) and asks whether
  the standing mechanism (§3.1 of the original report: deep runtime
  lexical-environment chains) persists, shrinks, or grows there.

---

## 1. Headline finding

**The penalty persists under the pre-decoded engine, at essentially the same
magnitude as the classic byte engine, and for a newly-confirmed structural
reason: the pre-decoded engine's local-environment representation
(`BcEnvFrame`) is a **type alias** for the exact same generic,
recursive-chain-walking `EnvironmentFrame<C>` that HeapSyn uses.**
(`src/eval/bytecode/closure.rs:30`: `pub type BcEnvFrame =
EnvironmentFrame<BcValue>`.) Predecode changes **only** opcode decode and
dispatch (flat `Instr` records instead of a re-read byte stream); it does not
touch, and architecturally cannot touch without a redesign, how local variable
lookups walk the environment-frame linked list. Confidence: **measured-verified**
for the type-identity fact (read directly from source, not inferred); the
walk itself (`EnvironmentFrame::cell()` at `src/eval/machine/env.rs:370-379`)
is a plain recursive `match self.next { Some(env) => …cell(idx - len), None
=> None }` — genuinely O(chain depth), identical code path for HeapSyn and
both bytecode engines.

This directly answers the dispatch's central question: pre-decode is not a
mechanism-level fix for this defect, and cannot become one without changing
what gets stored in env frames — i.e. it inherits the problem from the blob
generation pipeline described in eu-2sa6.12, not from bytecode decode.

---

## 2. Baseline reproduction (sanity check before profiling)

HeapSyn ticks reproduced eu-2sa6.12's figures **exactly** on this build
(measured-verified — deterministic layer):

| Metric | blob | source |
|---|--:|--:|
| Ticks | 76,814,993 | 76,751,915 |

This confirms the worktree/build/blob are faithful to the historical baseline
before any new measurement is trusted.

---

## 3. Wall timing — four configs, interleaved median-of-5 (measured-single)

Round-robin interleaved (bc-blob, bc-source, pd-blob, pd-source) × 5 rounds,
`--heap-limit-mib 12288`, quiet machine, spread shown as min–max:

| Config | Median | Spread | Blob/source ratio |
|---|--:|--:|--:|
| bytecode, blob | 2.258s | 2.254–2.319s | — |
| bytecode, source | 1.597s | 1.596–1.657s | **1.414×** |
| predecode, blob | 2.050s | 2.014–2.076s | — |
| predecode, source | 1.588s | 1.510–1.607s | **1.291×** |

**The penalty shrinks modestly under predecode (1.41× → 1.29×) but does not
disappear.** This is a single interleaved session (not yet corroborated by a
second session), so labelled **measured-single**; it is internally consistent
(low spread, clean interleave) and matches the profile evidence in §4. The
modest shrink is consistent with PR #1002's own gate note that BV2's
`Op::Ann` elimination removes a fixed number of dispatch steps from both
configs — a constant subtracted from both raises the smaller-denominator
ratio less than the bigger one, so this is not evidence predecode addresses
the mechanism, only that it trims dispatch overhead generically.

Note this session's absolute bc-blob/bc-source ratio (1.414×) reads higher
than eu-2sa6.12's original measured-single figure (1.27×) taken under
elevated machine load. Both are measured-single; the direction (blob slower)
and rough magnitude agree, but neither should be treated as
measured-verified without a second quiet-machine session. Flagging the
discrepancy rather than picking whichever number is more convenient.

---

## 4. Profiles — top self-time leaves, four configs (`sample`, real child PID)

Sampled the real `eu` child (recovered via `pgrep -P <timeout-pid>`), 1 ms
interval, for the full run of each config on `018_string_scale`. Two
independent rounds were taken for the predecode pair (to check profile
stability, per PROTOCOL §4's warning that a single profile's share is never
load-bearing); one round each for the byte-engine pair, for continuity with
the eu-2sa6.12 record.

Percentages below exclude the idle-waiting leaves (`__ulock_wait`,
`semaphore_wait_trap` — these are the join-wait and ctrl-c-handler threads
blocked in a syscall, not eval-thread work) and are of the **eval-thread-only
total** (sum of the `eu`-internal leaf samples).

### 4.1 Predecode engine (the primary target — never profiled before)

| Leaf (self-time) | blob round 1 | blob round 2 | source round 1 | source round 2 |
|---|--:|--:|--:|--:|
| `enter_local` | 806 (52.1%) | 809 (51.7%) | 507 (42.5%) | 497 (43.9%) |
| `handle_op_predecoded` | 250 (16.2%) | 283 (18.1%) | 230 (19.3%) | 226 (20.0%) |
| `dispatch` | 232 (15.0%) | 222 (14.2%) | 237 (19.8%) | 262 (23.2%) |
| `drop_glue<ExecutionError>` | 142 (9.2%) | 165 (10.5%) | 123 (10.3%) | 100 (8.8%) |
| `step_predecoded` | 81 (5.2%) | 70 (4.5%) | 58 (4.9%) | 56 (5.0%) |
| `run` | 35 (2.3%) | 28 (1.8%) | 34 (2.8%) | 31 (2.7%) |
| eval-thread total | 1546 | 1567 | 1194 | 1131 |

**`enter_local`'s absolute sample count grows ~1.6× under blob, reproducibly
across both rounds** (806→809 blob; 507→497 source; ratio 1.59× round 1,
1.63× round 2). Its **share** of eval-thread work also grows materially
(≈52% blob vs ≈43% source) — same direction and similar magnitude to the
original HeapSyn finding (55.1% vs 38.4%), now confirmed on a completely
different engine implementation. This is the strongest, most stable signal
in the whole session (two independent samples agreeing to within a few
percentage points). Confidence: **measured-single** (properly interleaved
config pairs would need repeating for measured-verified, but the two-round
self-consistency here is a stronger evidentiary bar than a single sample).

The other leaves (`handle_op_predecoded`, `dispatch`, `drop_glue`) do **not**
show a consistent blob-vs-source direction across rounds — `dispatch`'s share
is actually *lower* under blob in both rounds, and `drop_glue`'s share is
roughly flat. This matches PROTOCOL §4's warning that non-dominant leaf
shares are noisy; only `enter_local` clears the bar for a load-bearing claim.

### 4.2 Byte engine (default; one round each, for continuity with eu-2sa6.12)

| Leaf (self-time) | blob | source |
|---|--:|--:|
| `enter_local` | 483 (27.5%) | 299 (25.2%) |
| `read_ref` | 397 (22.6%) | 236 (19.9%) |
| `handle_op` | 331 (18.9%) | 212 (17.8%) |
| `drop_glue<ExecutionError>` | 259 (14.8%) | 164 (13.8%) |
| `dispatch` | 197 (11.2%) | 170 (14.3%) |
| `step` | 63 (3.6%) | 76 (6.4%) |
| `run` | 24 (1.4%) | 31 (2.6%) |
| eval-thread total | 1754 | 1188 |

`enter_local` absolute count ratio: 483/299 = **1.62×** — closely matching
the predecode pair's ~1.6× (§4.1), and also matching (not exactly, see below)
eu-2sa6.12's original byte-engine figure (433 vs 310 = 1.40×). Direction and
rough magnitude agree across three independent sessions now (original report,
this session's byte pass, this session's predecode pass); the exact ratio is
session-noisy but consistently in the 1.4–1.6× band.

**Correction to the standing record:** eu-2sa6.12 reported `drop_glue`
jumping "5×" (257 vs 53) on the byte engine. This session's byte-engine
profile shows a materially smaller jump: 259 vs 164 = **1.58×**, in the same
band as `enter_local`'s ratio rather than a standalone 5× spike. Single
profiles are explicitly non-load-bearing per PROTOCOL §4 ("malloc share
swung 3–10× between sessions" — the same caution evidently applies to
self-time share, not just malloc), so this isn't a contradiction, just a
reminder that the original "5×" figure should not be quoted as a stable
number. Both this run's number and the original are **measured-single**.

### 4.3 A genuine surprise: `drop_glue<ExecutionError>` is hot at all

`drop_glue<ExecutionError>` sits in the top 4–6 leaves in **every** config,
which is odd on its face — dropping a `Result<_, ExecutionError>` should be
nearly free on the success path. Looking at the type
(`src/eval/error.rs:590`), `ExecutionError` is a large enum: most variants
carry a `Smid` plus a `Box<(…)>` of owned data (`String`, `Vec<String>`,
`Number` tuples, etc.), but several — notably the ones on the hottest paths,
`BadEnvironmentIndex(usize)`, `BadGlobalIndex(usize)` returned by
`enter_local`/`enter_global` (`src/eval/bytecode/machine.rs:1645-1697`) — are
small. The likely explanation (**projected**, not measured this session: no
`size_of::<ExecutionError>()` instrumentation was added, per the "diagnosis
only, no code changes" constraint) is that `Result<T, ExecutionError>`'s
in-memory size is set by its **largest** variant, so every `enter_local`/
`read_ref` call returning `Ok(())` or `Ok(DecodedRef)` still pays for
constructing/moving/dropping a value sized for the worst-case error variant —
a classic "large error enum inflates the happy path" Rust cost, independent of
blob vs source. It scales with **call count** (more `enter_local`/`read_ref`
calls under blob → more `drop_glue` samples in absolute terms, 259 vs 164),
but its **share** doesn't diverge blob-vs-source the way `enter_local`'s does
— consistent with it being a per-call constant overhead rather than part of
the chain-depth mechanism. Worth a separate, small investigation (boxing the
large error variants, à la the existing `Box<(…)>` pattern already used for
several variants) but out of scope for this diagnosis and **not** validated
by measurement here — flagged as a lead, not a finding.

---

## 5. Mechanism: what this session confirms, corrects, and leaves open

**Confirmed (measured-verified as a structural fact, via source read):**
`BcEnvFrame = EnvironmentFrame<BcValue>` — the pre-decoded engine's local
environment representation is the identical generic type to HeapSyn's, with
an identical O(chain-depth) recursive lookup (`cell()`,
`src/eval/machine/env.rs:370-379`). This was not previously established; the
original report treated HeapSyn's `EnvironmentFrame::get` and bytecode's
`enter_local` as *analogous* mechanisms based on profile-share similarity.
They are now shown to be **the same code**, called from three different
dispatch loops (HeapSyn tree-walker, byte-engine `handle_op`, predecode
`handle_op_predecoded`). This is the single most load-bearing new fact from
this session: it means no dispatch-loop-side optimisation (predecode
included) can fix the mechanism, because the mechanism lives one layer below
dispatch, in the shared environment representation the STG compiler
generates references into.

**Confirmed (measured-single, two independent samples):** `enter_local`'s
absolute sample count and share of eval-thread work both increase
substantially and reproducibly under blob, on the predecode engine (never
measured before this session) as well as the byte engine (consistent with,
though not numerically identical to, the original report).

**Not re-litigated (already refuted, per PRs #994/#997, standing record):**
neither of the two concrete generation-time causal theories tested so far —
(a) un-flattened per-binding Core `Let` nesting, (b) the missing
user→prelude demand-signature boundary — actually produces the deeper
chains. Both are byte-identical no-ops when implemented and measured.

**Left open, as it was before this session:** *why* blob's env chains are
walked more/deeper, at the STG-compilation level, remains uncharacterised
beyond the standing candidate already named in eu-2sa6.12 §7.6 — blob
generation permanently forgoes **cross-binding inlining** (substituting a
callee's body into its caller) to preserve one `LambdaForm` per prelude name,
so every call from user code (or prelude-to-prelude) into `map`, `foldl`,
`cons`, structural `=`, string `JOIN`, etc. remains a genuine indirect call
through the global table rather than being fused into the caller's compiled
unit the way `inline()` fuses it under source compilation. This session did
**not** implement or measure a third candidate fix along these lines — doing
so (real cross-binding inlining into the blob, or into user code at
compile time) is a materially larger change than the two already-refuted
"cheap" candidates, and was out of scope for a profiling-only spike. I did
**not** find a new, cheaper, previously-untried mechanism this session; I
narrowed the search space by (i) proving the defect is dispatch-loop-agnostic
(§5, `BcEnvFrame` identity) and (ii) reproducing the profile signal robustly
enough (two-round predecode agreement) to trust it as real rather than
session noise.

---

## 6. Does the penalty gate eu-2sa6.18 as "inherent to peel/global-slots form"?

**Leaning yes, but not proven this session.** The evidence assembled across
eu-2sa6.12 and this spike is consistent with the penalty being architectural:
- Both wire-format-preserving, generation-time fixes that could plausibly
  have been "cheap" are implemented and refuted (§5, "not re-litigated").
- The mechanism (deep/more-numerous env-frame walks) is now shown to be
  identical across all three engines/dispatch loops, so it cannot be fixed by
  any engine-side change, including the lever (a) pre-decode substrate this
  spike targeted.
- The remaining candidate (cross-binding inlining) is not a small patch: it
  is the one thing `inline()` does that blob generation *structurally cannot
  do* without abandoning the one-form-per-name property blob generation
  exists to guarantee (per eu-2sa6.12 §3.3's own root-cause line).

I have **not** implemented or measured that remaining candidate (real
cross-binding inlining into the blob), so I cannot say with certainty that it
is impossible within the current peel/global-slots form — only that the two
alternatives that preserve the form and are cheap to try have both failed,
and the one alternative that might work is the one the form's own design
principle rules out. This is a **projected** conclusion (reasoned from
architecture and the accumulated negative evidence), not a measured
one. I'd recommend the owner treat it as gating evidence for eu-2sa6.18 —
i.e., plan the generalisation design assuming the in-memory co-compilation
pipeline redesign is the real fix, not a further peel/global-slots patch —
while leaving the door open to a future, explicitly-scoped spike on
cross-binding inlining if the redesign timeline slips.

---

## 7. Ledger bookkeeping (eu-2sa6.13, separate commit)

Ran `cargo xtask engine-ab` (canonical 8-bench interleaved suite, blob
config, commit `036258ba`) and appended one ledger row per bench to
`docs/superpowers/engine-ab/results.jsonl`. `cargo xtask engine-ab --check`
reported one **REGRESSED** (`019_list_scale`, +23.6%) and one **WATCH**
(`020_lookup_curve` tripwire, +12.6%).

**Both are false alarms caused by a ledger-tooling gap, not real
regressions.** `--check`'s "compare the last two rows per bench" logic does
not filter by `prelude_config`. The 2026-07-12 session logged **both** a
blob row and a source row per bench (16 rows total for 8 benches), and for
some benches the source row sorts after the blob row in file order — so
`--check` picked up **source**-config rows as the "previous" value and
compared them against my **blob**-config row, reproducing the already-known
blob/source disparity as a spurious regression:

| Bench | "prev" used by --check | Actual prior blob-config value | Mine (blob) | True blob-vs-blob delta |
|---|--:|--:|--:|--:|
| `018_string_scale` | 0.923 (source, mislabelled prev) | 0.566 (blob, commit `97c4e450`) | 0.561 | **−0.9%**, noise |
| `019_list_scale` | 0.917 (source, mislabelled prev) | 1.105 (blob, `97c4e450`) | 1.133 | **+2.5%**, noise |
| `020_lookup_curve` | 12.08 (source, mislabelled prev) | 12.969 (blob, `97c4e450`) | 13.608 | **+4.9%**, within the class-E WATCH band regardless |

Checked every other bench the same way (block_merge +2.7%, yaml +4.9%, toml
+5.9%, io_loop ~0%, hof_fold ~0%) — **all within normal session noise**,
confirming master is healthy post-#1002 with no real perf regression. I did
not modify `xtask`'s `--check` comparison logic (diagnosis-only scope for
this spike); flagging it here as a real, ledger-verified finding for the
coordinator/owner to decide whether `--check` should filter by
`prelude_config` before trusting its automated verdict on a mixed-config
ledger.

---

## 8. Evidence index (this session)

- Profiles: scratchpad `profiles/{blob,source}-{predecode,byte}.txt` (byte
  engine), `profiles/{blob,source}-predecode{,-r2}.txt` (predecode, two
  rounds) — not committed (scratch artefacts; the self-time tables above are
  the durable record).
- Interleaved timing raw data: §3, computed from 20 individual timed runs
  (4 configs × 5 rounds), min/median/max reported.
- Source facts: `src/eval/bytecode/closure.rs:30` (`BcEnvFrame` type alias),
  `src/eval/machine/env.rs:370-379` (`cell()`/`get()` recursive chain walk),
  `src/eval/bytecode/machine.rs:1645-1667` (`enter_local`), `src/eval/error.rs:590`
  (`ExecutionError` enum).
- Ledger: `docs/superpowers/engine-ab/results.jsonl`, 8 new rows dated
  2026-07-13, commit `036258ba`.
