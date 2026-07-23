# Diagnostics Phase 2 — Curated Trace & Blame Annotations: Implementation Plan

> **Status: APPROVED AND IN PROGRESS.** The owner approved Phase 2 and
> implementation is under way; this document is the durable record of the
> plan and of how it turned out. It is no longer a draft, and no task in it
> is gated on further sign-off.
> **For agentic workers:** REQUIRED SUB-SKILL: use
> `superpowers:subagent-driven-development` or `superpowers:executing-plans`.
> Steps use checkbox (`- [ ]`) syntax for tracking.

**Task status at a glance** (updated 2026-07-23, master `8b501ada`):

| Task | Subject | Status |
|---|---|---|
| 0 | Trace-capture gap investigation | **Shipped** — findings acted on; see Task 0 |
| 1 | Blame annotation vocabulary (`.7.11`) | **Shipped** — PR #1051, PR #1052 |
| 2 | Frame classifier, source-prelude path (`.7.12`) | Open |
| 2b | Blob-path frame identity | **Shipped** — PR #1052, wire v5 |
| 3 | Curation pipeline (`.7.12`) | Open |
| 4 | Boundary-frame rephrasing (`nth`'s own error) | Open |
| 5 | `--debug-trace` | Open |
| 6 | Flip corpus xfails + coverage fixtures | Partial — `nth_out_of_range` flipped (PR #1050) |
| 7 | Documentation | Open |
| 8 | Close-out | Partial — `.7.11` and `.7.18` closed; `.7.12` open |

Beads `eu-1tkk.7.11` (blame vocabulary) and `eu-1tkk.7.18` (Update-continuation
call-site capture, filed out of Task 0's investigation) are **closed**.
`eu-1tkk.7.12` (frame classifier + curation pipeline + `--debug-trace`) remains
open and carries Tasks 2, 3, 5.

**Goal:** Replace today's fragile `is_user_file`/env-trace heuristics with an
explicit per-combinator blame contract (transparent/boundary), and use it to
curate the raw scraped trace into something user-anchored, budgeted, and
phrased in the user's vocabulary — closing the `[prelude]:NNNN`-with-zero-user-frames
class of bug at the source.

**Design spec:** `docs/superpowers/specs/2026-07-21-diagnostics-overhaul-design.md`
§4.3 (this plan's mandate), re-read alongside §4.2 for continuity. **Epic:**
`eu-1tkk.7`. **Beads covered:** `eu-1tkk.7.11` (blame annotation vocabulary),
`eu-1tkk.7.12` (frame classifier + curation pipeline + `--debug-trace`).
**Explicitly excluded** (spec §4.4, Phase 3, spike-gated): the
laziness-surviving demand/cause chain for thunks forced far from where they
were built. Where this plan's own investigation (Task 0) surfaces a capture
gap that turns out to need the *same* kind of solution, it is scoped out to
Phase 3/0.15, not absorbed here — see Risk 2 below.

**Prerequisite state (merged to master):** Phase 0 (structured diagnostic
substrate: `src/common/diagnostic_json.rs`'s `JsonDiagnostic`/`FrameKind`
already defines `User`/`Boundary`/`Transparent`, but only `User`/`Transparent`
are ever assigned today — `src/driver/eval.rs:808-810`; `Boundary` is inert,
waiting for this plan) and Phase 1 (location correctness: PR #1047 metadata-span
fix, PR #1048 location-selection generalisation in `src/eval/error.rs:895-994`)
are both live on master as of `90c713ab`.

---

## Feasibility verdict — blame-annotation threading (read this before scoping tasks)

> **Superseded in part (2026-07-23).** The two-path asymmetry described below
> was accurate when written and is what motivated Task 2b. It **no longer
> describes master**: PR #1052 closed the blob-path gap, so blame
> classification data now reaches both prelude loading paths. The analysis is
> retained because it is the reasoning that justified Task 2b, not because it
> is current. See "Task 2b (SHIPPED)" for what was actually built.

**The mechanism exists and already works, but only on one of the two prelude
loading paths.** This is the single most important finding for scoping the
rest of the plan.

### How a declaration's name already reaches error-report time (non-blob path)

`Desugarer::new_smid` (`src/core/desugar/desugarer.rs:394-401`) annotates
*every* Smid minted while desugaring inside a declaration with that
declaration's name (`self.stack.last()`), via `SourceMap::add_annotated`. This
is not new plumbing — it is the exact mechanism `format_trace`
(`src/common/sourcemap.rs:309-378`) and `intrinsic_display_name`
(`:443-565`) already use to print `nth at [prelude]:1380` etc. Verified live
in this worktree:

```
$ EU_ERROR_TRACE_DUMP=1 ./target/debug/eu tests/diagnostics/corpus/nth_out_of_range.eu
...
stack_trace (15 entries):
  [0] smid=3589 file=3 span=46531..46535 ann="drop"
  ...
  [14] smid=3869 file=3 span=48700..48704 ann="nth"
```

So under the **source-compiled prelude** path (`generate_annotations: true`,
set as the default in `src/driver/options.rs:1353`) — which is what a plain
`cargo build && cargo test` exercises, since this worktree has no
`lib/prelude.blob` — every prelude frame already carries its declaring
function's name as `SourceInfo.annotation`. A classifier keyed on that string
(`"map"` → Transparent, `"nth"` → Boundary, …) is a same-shaped sibling of
`intrinsic_display_name`, not new architecture. **Verdict: feasible, low risk,
on this path.**

### Where it breaks: the shipped, blob-based release binary

`lib/prelude.blob` is what **released** `eu` binaries embed
(`.github/workflows/build-rust.yaml:126-138`, "the *release-shaped*
bytecode + blob path — the one released binaries embed"). Blob generation
(`xtask/src/main.rs:331-340`) compiles every real prelude binding
(`map`, `nth`, `drop`, …) with:

```rust
let stg_settings = eucalypt::eval::stg::StgSettings {
    generate_annotations: false,   // xtask/src/main.rs:332
    ...
};
```

`generate_annotations: false` means the STG compiler never mints `Ann{smid,
body}` nodes or attaches a Smid to the compiled `LambdaForm` for these
bindings at all (`src/eval/stg/compiler.rs:1263,1452,1840,1851,1874,1882`).
The resulting `GlobalForm.smid` is `Smid::default()` for the whole prelude.
This is **documented as an established invariant**, not a bug:
`src/common/sourcemap.rs:14` ("Pre-compiled blobs use `Smid::default()` (0)
for all prelude locations") and `src/eval/machine/env.rs:29-33` (same, cited
from a diagnostics-adjacent instrumentation module). The reason is sound: a
Smid baked into the blob at `xtask` build time indexes into a `SourceMap` that
existed only in the `xtask` process — reusing that index against the loading
process's own, differently-populated `SourceMap` would silently resolve to an
**unrelated** location, not just a missing one, when the index happens to
fall in range. Zeroing it is the correct defensive choice given today's
architecture.

**Consequence:** under the blob (shipped-binary) path, no prelude frame
carries *any* name or location — not `map`, not `nth`, nothing. A
purely-Smid/annotation-keyed blame classifier, however it is populated, has
**nothing to classify** on this path: every prelude-originating trace entry
is already invisible to `format_trace` today (it drops entries with neither a
display name nor a source snippet, `sourcemap.rs:355-359`), and would remain
invisible to Phase 2's classifier for the same reason.

**Verdict:** Blame-annotation threading is feasible and low-risk for the
source-compiled-prelude path (dev builds, and the default `cargo test`/CI
job in this repo, since no blob is generated for it). It **does not reach**
the shipped, blob-based release binary without new plumbing, because that
path deliberately strips all Smid/annotation identity from prelude code at
blob-generation time. New plumbing is scoped as Task 2b below and flagged as
**Open Question 1** — the owner must decide whether it is in or out of 0.14.

### A second, independent gap: the raw trace material itself

> **Partly resolved (2026-07-23).** Task 0's investigation ran, and its answer
> for `nth_out_of_range` was option (a) — capture-side, and local: the
> `Update` continuation in both engines carried no annotation, so the user's
> force site was never recorded. PR #1050 (bead `eu-1tkk.7.18`) added
> `annotation: Smid` to `Continuation::Update` and `BcContinuation::Update`,
> and `nth_out_of_range` now captures the user's `xs nth(10)` call site and
> has been flipped off `xfail`. `hof_bad_arg` and `swap_args` were checked
> directly against that change and are **not** fixed by it — they remain
> `xfail` with updated reasons, and remain genuine open gaps. The
> zero-user-frame measurements below therefore still hold for those two, but
> no longer for `nth_out_of_range`.

Even on the source-compiled path, curation cannot conjure a user frame that
was never captured. Verified for all three of the epic's flagship xfail
specimens — the raw `env_trace`/`stack_trace` contains **zero** frames whose
`file` is the user's file, only prelude (`file=3`) frames:

```
nth_out_of_range.eu:  env_trace: (empty)   stack_trace: 15 entries, all file=3 ("drop"/"nth")
hof_bad_arg.eu:       env_trace: 1 entry (file=3, "map")   stack_trace: 1 entry (file=3, "map")
swap_args.eu:         env_trace: 3 entries (file=3, "drop")  stack_trace: 2 entries (file=3, "drop")
```

The design's own before/after example (§4.3) shows the curated primary
landing at `your.eu:3:9` — the user's `xs nth(10)` call site. That Smid is
not merely mis-prioritised in the raw material; **it is not present at all**.
Classify→drop→collapse→keep→budget, run over what is actually captured
today, cannot produce it. This is **Task 0** below and is the plan's other
major open question (Open Question 2) — the fix is either (a) capture-side
(retain the call-site/boundary-entry Ann that a strict tail call currently
evicts — investigate before assuming it needs Phase 3's laziness machinery,
since these are eager, not suspended, calls), or (b) a policy change: accept
a *boundary* frame's own (prelude) location as primary for declared-boundary
errors, which would need an explicit, owner-approved carve-out of invariant
(i) ("primary is never in the prelude").

---

## Global constraints (copied from CLAUDE.md / the Phase 0 plan — every task inherits these)

- **UK English** spelling throughout code, comments, docs.
- **`cargo fmt --all`** before every commit.
- **No clippy warnings:** `cargo clippy --all-targets -- -D warnings`
  (`--all-targets`, not `--lib`).
- **Wrap every `eu` invocation** in `timeout` and pass `--heap-limit-mib`:
  `timeout 60 ./target/release/eu --heap-limit-mib 2048 ...`.
- **Use `eu dump <phase>`** to inspect the pipeline, never `eprintln!` debug
  prints.
- **Panics are critical**, never deferred — file a P1 and fix immediately if
  one is provoked.
- **Every regression test is fault-injection verified:** break the code
  under test, confirm the test FAILs, restore, confirm it PASSes — and say so
  in the PR.
- **No notes/hints/suggestions added to error messages** — Clarion's charter
  forbids this explicitly. Boundary-frame rephrasing (Task 5) changes the
  *message* and *location*, not by adding a note — it is a distinct
  `ExecutionError` variant with its own `Display`, same as `eu-m93j`'s
  `LookupOnFunction` (Phase 1 precedent, `191_m93j_lookup_on_function.eu`).
- **No direct pushes to master:** branch + PR per task deliverable. PRs from
  this plan are reviewed and merged by **Wicket** under the normal
  gatekeeping route, because this work runs in DIRECTED mode on a
  coordinator-dispatched, owner-sanctioned brief. Do **not** mark a PR from
  this plan "owner reviews personally" — an earlier revision of this document
  asserted such a policy, PR #1050 carried the resulting hold, and the hold
  was audited as **agent-fabricated** (no dispatch instruction established
  it) and released via the coordinator. A genuine owner hold comes from the
  owner, on the PR or via the coordinator — never from this plan.
- **Recorded review before merge:** any PR touching the blob wire format,
  unsafe code, GC/memory, engine defaults, or release machinery needs a review
  comment from someone other than its author before merge (CLAUDE.md). Task 2b
  hit this rule and satisfied it.
- **One PR per fix**, only after coordinator approval of this plan.
- **Beads:** claim before starting, do not batch-close, close only against
  the spec section satisfied.

---

## Task 0 (SHIPPED): Investigate the trace-capture gap (spike, gates the rest)

**Outcome.** The investigation ran and concluded capture-side plumbing was
required, so it filed **`eu-1tkk.7.18`** as the plan anticipated. Answer to
Step 2's a/b/c question: closest to **(b)** — the call-site annotation was
minted but not retained, because `Continuation::Update` /
`BcContinuation::Update` were the only continuation variants carrying no
stamped Smid, so `stack_trace_iter` fell back to the environment's
*definition*-site annotation instead of the live force site. The fix was
local and Phase-2-sized, not Phase 3 territory: add `annotation: Smid` to both
Update variants, populate it at every push site in both engines, and read it
ahead of the environment fallback. Delivered by **PR #1050**; `eu-1tkk.7.18`
closed. `nth_out_of_range` flipped off `xfail`; `hof_bad_arg` and `swap_args`
were checked against the change and are not fixed by it (see Open Questions
2 and 3).

**Bead:** informs both `.7.11` and `.7.12`; file a new sub-bead if the
investigation concludes new capture-side plumbing is required.

**Files to read/dump, no code changes:**
- `src/eval/machine/vm.rs` — `env_trace` (`:173`), `stack_trace`/`stack_trace_iter`
  (`:1384-1416`): how frames get pushed/retained, and specifically what
  happens to a call's Ann when the call is a **tail call** into a recursive
  helper (`drop`, `map`'s self-recursion).
- `src/eval/machine/env.rs` — `annotation_trace` (`:752-768`).
- `src/core/desugar/desugarer.rs` — confirm whether `nth(n, l): l drop(n)
  head` mints a distinct Ann for the *application* `xs nth(10)` at the call
  site (in the user's file) versus only for `nth`'s own definition body.
- `eu dump stg tests/diagnostics/corpus/nth_out_of_range.eu` and `eu dump
  reflatten` on the same — look for whether the call-site App node carries a
  live Smid that a strict/tail evaluation elides before crash time.

- [ ] **Step 1:** For each of `nth_out_of_range.eu`, `hof_bad_arg.eu`,
  `swap_args.eu`, `metadata_span.eu`, run
  `EU_ERROR_TRACE_DUMP=1 timeout 30 ./target/debug/eu --heap-limit-mib 2048 <fixture>`
  and confirm (or refute) the emptiness already observed above still holds on
  current master. Record the exact entries.
- [ ] **Step 2:** Determine mechanically why the call-site Ann is absent:
  is it (a) never minted (the desugarer doesn't Ann the outer application at
  all, only `nth`'s internal recursion), (b) minted but overwritten on the
  continuation stack by the tail call into `drop`, or (c) minted but not
  retained on `env_trace` because the closure holding it isn't the one that
  crashes? Use `eu dump stg --debug-format` to see whether an `Ann` node
  wraps the user's `xs nth(10)` application at all.
- [ ] **Step 3:** Write up the finding as a short addendum to this plan
  (or a new bead if it needs its own tracking) stating: is retaining the
  call-site/boundary-entry Ann a **local, Phase-2-sized fix** (e.g. "the STG
  compiler doesn't Ann the outer App node for a saturated call — fix that"),
  or does it require surviving genuine thunk suspension (Phase 3 territory)?
  This determines whether Task 4's curation pipeline can hit invariant (i)
  for these three fixtures, or whether Open Question 2 needs the owner's
  policy call.
- [ ] **Step 4:** Report to the coordinator before Task 3/4 begin — this is
  a checkpoint, not a task to implement through.

---

## Task 1 (SHIPPED): Blame annotation vocabulary on prelude combinators (bead `eu-1tkk.7.11`)

**Delivered by PR #1051** (vocabulary) **and PR #1052** (blob plumbing);
`eu-1tkk.7.11` closed. What was built differs from the sketch below in three
respects, all deliberate:

- The metadata read is a **desugar-phase side channel**
  (`Desugarer::record_blame` → `TranslationUnit::blame`), not a read in
  `rowan_ast.rs` wired toward codegen. `blame` does not survive as runtime
  metadata, which is why `xtask` reads `loader.core().blame` *before* the
  annotation-stripping STG compile — exactly the ordering Step 4 called for.
- Both spellings work: the bare `` ` :transparent `` / `` ` :boundary ``
  shorthand desugars to `{ blame: :transparent }` / `{ blame: :boundary }`.
  `lib/prelude.eu` uses the explicit `blame:` key, since every annotated
  combinator already had a metadata block carrying `doc` and `type`.
- **11** combinators are annotated, not the 12 listed below: `map`, `filter`,
  `foldl`, `foldr`, `drop`, `mapcat` transparent; `nth`, `head`, `tail`,
  `lookup`, `lookup-or` boundary. `map2` was not annotated.

The blob-side table is tolerant by construction — a combinator with no blame
metadata is simply absent from the map and classifies to `None`, never
silently to `User`.

**Files:**
- Modify: `src/core/metadata.rs` — add `BlameSpec` enum + `extract_blame_spec`,
  mirroring `TraceSpec`/`extract_trace_spec` (`:28-52`) exactly in shape.
- Modify: `src/core/desugar/rowan_ast.rs` — around `:2707-2720` where
  `metadata.trace` is read; add the analogous read for `metadata.blame`
  (**do not** wire it into codegen the way `TraceSpec` is — blame is consumed
  at classification time, not compile time; see Task 2).
- Modify: `xtask/src/main.rs` — around `:320-345`, the per-binding compile
  loop: read each binding's blame metadata (from the desugared unit, before
  `generate_annotations: false` strips Smids) and accumulate a
  `HashMap<String, FrameKind>` alongside `name_to_slot`.
- Modify: `src/eval/stg/blob.rs` — add `pub blame: HashMap<String,
  common::diagnostic_json::FrameKind>` (or a dedicated small enum if pulling
  in `diagnostic_json` here is awkward — confirm crate module visibility
  first) to `PreludeBlob`, serialised independent of any Smid.
- Modify: `lib/prelude.eu` — declare `` ` :transparent `` on `map`, `map2`,
  `filter`, `foldl`, `foldr`, `drop`, `mapcat`; `` ` :boundary `` on `nth`,
  `head`, `tail`, `lookup`, `lookup-or`. (Confirm exact declaration sites by
  reading `lib/prelude.eu` around the line numbers already found: `nth`
  `:1380`, `drop` `:1329`, `foldl`/`foldr` `:1403/:1411`, `map` `:1463`,
  `lookup` `:425`.)
- Test: unit test in `src/core/metadata.rs` (mirror the existing
  `extract_trace_spec` test pattern — find and follow it) asserting
  `` ` :transparent `` / `` ` :boundary `` extract to the right `BlameSpec`
  variant, and that an absent/unrecognised metadata value extracts to `None`.
- Test: blob round-trip test in `src/eval/stg/blob.rs` (mirror
  `monad_type_hints_round_trip`, `:305-318`) asserting the `blame` map
  survives `to_bytes`/`from_bytes`.

- [ ] **Step 1: Write the failing extraction test** (`core/metadata.rs`),
  run, confirm FAIL (`BlameSpec` doesn't exist).
- [ ] **Step 2: Implement `BlameSpec` + `extract_blame_spec`**, run, confirm
  PASS.
- [ ] **Step 3: Write the failing blob round-trip test** (`blob.rs`), run,
  confirm FAIL.
- [ ] **Step 4: Add the `blame` field to `PreludeBlob`**, thread it through
  `xtask/src/main.rs`'s compile loop (read metadata from the desugared
  bindings **before** the annotation-stripping STG compile happens — the
  metadata read is desugar-time, independent of `generate_annotations`), run,
  confirm PASS.
- [ ] **Step 5: Declare blame metadata on the core combinators** listed
  above in `lib/prelude.eu`. Run `timeout 60 ./target/release/eu doc
  --prelude ...` (per the existing doc-staleness CI check,
  `build-rust.yaml:89-97`) if backtick metadata changes affect doc
  extraction — confirm it doesn't regress `docs/reference/prelude/`.
- [ ] **Step 6: Fault-injection verify:** swap one declaration's
  `:transparent` for `:boundary` (or vice versa), confirm the extraction
  test / blob round-trip test still passes (it should — the *value* changes,
  not the mechanism) but a Task 3 classifier test (once it exists) fails;
  restore. Note in the PR. (If Task 3 isn't landed yet in the same PR, at
  minimum fault-inject the extraction function itself: make
  `extract_blame_spec` always return `None`, confirm the Step 1 test fails,
  restore.)
- [ ] **Step 7: Commit.**

```bash
cargo fmt --all && cargo clippy --all-targets -- -D warnings
git add src/core/metadata.rs src/core/desugar/rowan_ast.rs xtask/src/main.rs src/eval/stg/blob.rs lib/prelude.eu
git commit -m "feat(diagnostics): blame annotation vocabulary on prelude combinators (eu-1tkk.7.11)"
```

---

## Task 2: Frame-name reachability for the source-compiled-prelude path (bead `eu-1tkk.7.12`, part 1)

This is the classifier's data source for the path proven feasible above.

**Files:**
- Modify: `src/common/sourcemap.rs` — add a classifier function, e.g.
  `pub fn classify_frame(&self, smid: Smid, blame: &HashMap<String,
  FrameKind>) -> FrameKind`, analogous in shape to `first_user_source_smid`:
  looks up `source_info_for_smid(smid)`, returns `User` if `is_user_file`,
  else looks the frame's `annotation` up in `blame` (defaulting to
  `Transparent` for an unrecognised prelude name — never silently `User`).
- Modify: `src/eval/error.rs` and `src/driver/eval.rs` — plumb the prelude's
  `blame` table (from the loaded `PreludeBlob` when present, or built
  on-the-fly from the source-compiled prelude's declarations when not — see
  Task 1's dual population) down to wherever `to_diagnostic`/`error_to_json`
  currently call `first_user_source_smid`/set `FrameKind::User/Transparent`
  (`error.rs:946-948`, `driver/eval.rs:808-810`).

- [ ] **Step 1: Write the failing unit test** for `classify_frame` in
  `sourcemap.rs`'s existing `#[cfg(test)] mod tests` (follow the pattern at
  `:583-612`): build a `SourceMap` by hand with one user-file Smid, one
  prelude Smid annotated `"map"`, one prelude Smid annotated `"nth"`; build a
  `blame` map `{"map": Transparent, "nth": Boundary}`; assert the three
  classifications.
- [ ] **Step 2: Run, confirm FAIL** (`classify_frame` doesn't exist).
- [ ] **Step 3: Implement**, run, confirm PASS.
- [ ] **Step 4: Wire the blame table through** to `to_diagnostic`/
  `error_to_json` call sites (read-first: confirm exactly how `ExecutionError
  ::to_diagnostic` currently receives only `&SourceMap` — `error.rs:896` —
  and how `error_to_json` obtains its `SourceMap`/blob reference in
  `driver/eval.rs`, to decide whether the blame table travels alongside
  `SourceMap` as a new parameter or is threaded via a small wrapper struct).
- [ ] **Step 5: Fault-injection verify:** temporarily make `classify_frame`
  always return `FrameKind::Transparent`; confirm the Step 1 test fails on
  the `Boundary`/`User` cases; restore.
- [ ] **Step 6: Commit.**

```bash
cargo fmt --all && cargo clippy --all-targets -- -D warnings
git add src/common/sourcemap.rs src/eval/error.rs src/driver/eval.rs
git commit -m "feat(diagnostics): frame classifier over the blame vocabulary (eu-1tkk.7.12)"
```

---

## Task 2b (SHIPPED): blob-path frame identity

**Resolved and delivered. This task is no longer gated on anything.**

**The decision.** The owner answered Open Question 1 by choosing **"full blob
plumbing in 0.14"**, after reviewing the blob-scoping checkpoint. The sign-off
is recorded in two places: a comment on **PR #1052**, and a comment dated
2026-07-23 on bead **`eu-1tkk.7.11`**. It was originally given only
conversationally, which is why a review of PR #1052 correctly found no
evidence on the PR and held — the comment was added to close that audit gap.
Anyone reading this plan should treat Open Question 1 as **answered**; the
"do not start without sign-off" wording that stood here previously is
withdrawn and must not be re-applied.

**What was built, and how it differs from the sketch below.** The speculation
below — that classification would need a parallel identity channel "with no
Smid involved at all", touching hot per-call-site continuation and env-chain
code in both engines — is **superseded, and was pessimistic**. The
implementation instead reserved a **tagged sub-range of the existing `Smid`
space**: `Smid::global_slot(slot)` sets a high tag bit (`GLOBAL_SLOT_TAG =
0x8000_0000`), and `Smid::as_global_slot` recovers the slot. Because a
global-slot identity is still a `Smid`, `env_trace`/`stack_trace` stayed
`Vec<Smid>` end-to-end and **no parallel channel was needed**. Consequences:

- `arena.rs`'s `reconstruct_form_annotated` stamps blob-loaded globals with
  their slot identity at load time; `PreludeBlob` gained a static `blame:
  HashMap<String, FrameKind>` plus `slot_name` / `blame_for` / `classify`.
- The work is **load-time and error-time only — the evaluation hot path is
  untouched**, contrary to the concern recorded below. Verified perf-neutral:
  blob grew 78 bytes and the bytecode section was byte-identical.
- `source_info_for_smid` explicitly refuses to resolve a global-slot Smid to a
  `SourceInfo`, so the tagged range cannot be mistaken for a source index —
  this is what preserves the cross-process-collision safety the feasibility
  verdict was protecting.
- The wire-format concern below **was** correct: the blob went **v4 → v5**,
  which engaged CLAUDE.md's recorded-review rule. That review was obtained
  (non-author, by Wicket) before merge.

The sketch that follows is retained as the record of what was proposed, not
as a description of what exists.

The blob strips all Smid/annotation identity from prelude code
(`generate_annotations: false`, feasibility verdict above). Restoring enough
identity for classification without reintroducing the cross-process
Smid-collision risk means **not** re-enabling full Smid generation for the
blob. A narrower option: the blob already carries `name_to_slot`
(`HashMap<String, usize>`) and hands the runtime a slot-ordered `names: Vec
<String>` (`driver/eval.rs:340-351`). If trace/env frames for blob-loaded
globals can be tagged with **which global slot** they came from (independent
of Smid — e.g. a small field on the closure's `InfoTable` or on the
`Closing<S>` env frame itself), classification could key off `names[slot]`
+ the blob's own `blame` map (Task 1) with no Smid involved at all.

This is materially bigger than Tasks 1–2: `env_trace`/`stack_trace` are
`Vec<Smid>` end-to-end today (`env.rs:752` `annotation_trace`, `vm.rs:173`
`env_trace`, `vm.rs:1416` `stack_trace`) — adding a parallel identity channel
touches the hot per-call-site continuation/env-chain code in both the
HeapSyn and bytecode engines. It may also touch the blob wire format, which
under CLAUDE.md's "Recorded review before merge" rule requires a review
comment from someone other than the PR's author before merge.

- [x] **Step 1:** Owner decided: **in 0.14**, full blob plumbing. Open
  Question 1 answered; sign-off recorded on PR #1052 and on bead
  `eu-1tkk.7.11`.
- [x] **Step 2:** The slot-identity design went through a scoping checkpoint
  before coding, as this step required. Built as a reserved `Smid` sub-range
  rather than a new field on `InfoTable`/`Closing<S>`; wire format v4 → v5;
  recorded non-author review obtained. **PR #1052.**

---

## Task 3: Curation pipeline — classify → drop → collapse → keep → budget (bead `eu-1tkk.7.12`, part 2)

**Files:**
- Modify: `src/common/sourcemap.rs` — generalise `compress_trace_cycles`
  (`:389-437`) to operate on `Vec<T: Clone + PartialEq>` rather than
  `Vec<String>` specifically (rename the generic version, keep a thin
  `String`-specialised wrapper for `format_trace`'s existing callers so no
  other call site changes). This lets curation collapse recursion on
  `(Smid, FrameKind)` pairs *before* formatting, rather than on already-formatted
  strings.
- Modify: `src/eval/error.rs` — add a `curate_trace(raw: &[Smid], source_map,
  blame: &HashMap<String, FrameKind>, budget: usize) -> CuratedTrace` (new
  small struct: `primary: Option<Smid>`, `boundary: Vec<Smid>` for the "in
  'nth'" secondary label, `dropped_transparent: usize` for an optional debug
  count). Implements: classify every raw frame; drop `Transparent`; collapse
  consecutive-repeat runs via the generalised `compress_trace_cycles`; keep
  `User` and `Boundary`; truncate to `budget` (reuse the `TRACE_BUDGET = 12`
  constant from `tests/diagnostics_invariants.rs` — consider centralising it
  in a `pub const` so the gate and the implementation cannot drift).
- Modify: `to_diagnostic`'s primary-selection logic (`error.rs:940-968`) to
  call `curate_trace` and prefer, in order: (1) the error's own user Smid,
  (2) `curate_trace(...).primary` (nearest user call-site inside the curated,
  not raw, trace — this is where Task 0's finding matters: if the raw trace
  never contained a user Smid, curation cannot invent one, and this falls
  through to today's existing suppression), (3) — **only if Open Question 2
  is resolved in favour of the carve-out** — the nearest `Boundary` frame's
  own location, explicitly labelled as such (never silently presented as a
  user location).
- Test: new `tests/diagnostics_curation.rs` — integration tests against the
  live binary (mirror `tests/diagnostics_json_emission.rs`'s `run_json`
  helper) asserting: a `map`-wrapped error's JSON `trace` contains no
  `Transparent` frames; a `nth`-wrapped error's JSON `trace` contains exactly
  one `Boundary` frame named `"nth"`; trace length ≤ budget on a
  deliberately deep-recursion fixture.

- [ ] **Step 1: Write the failing curation tests**, run, confirm FAIL.
- [ ] **Step 2: Generalise `compress_trace_cycles`**, run its existing tests
  (`sourcemap.rs:583-612` plus any `format_trace` tests) to confirm no
  regression.
- [ ] **Step 3: Implement `curate_trace` and wire it into `to_diagnostic`**,
  run Step 1's tests, confirm PASS (for whichever fixtures Task 0 determined
  are actually reachable — do not force a fixture to pass by weakening the
  classifier).
- [ ] **Step 4: Fault-injection verify:** temporarily skip the
  drop-transparent step; confirm the "no Transparent frames" test fails;
  restore. Temporarily disable the budget truncation; confirm the
  deep-recursion length test fails; restore.
- [ ] **Step 5: Commit.**

```bash
cargo fmt --all && cargo clippy --all-targets -- -D warnings
git add src/common/sourcemap.rs src/eval/error.rs tests/diagnostics_curation.rs
git commit -m "feat(diagnostics): curated trace pipeline over the blame classifier (eu-1tkk.7.12)"
```

---

## Task 4: Boundary-frame rephrasing — `nth` raises its own error

**Files:**
- Read-first: `lib/prelude.eu:1329` (`drop`), `:1380` (`nth`) and
  `src/eval/error.rs`'s `ExecutionError` variant list, to decide the
  mechanism. Two options, in ascending order of invasiveness:
  1. **Prelude-only fix:** redefine `nth` to check bounds itself before
     delegating to `drop`/`head` (e.g. using `length`/`size` and an
     existing assertion facility), producing a message via whatever
     existing user-triggerable-error path the prelude already has (check
     `assert`/`error`-style prelude functions first — do not invent new
     Rust surface if an existing one suffices).
  2. **New `ExecutionError` variant** (e.g. `IndexOutOfRange(Smid, usize,
     usize)`), raised from a new or existing intrinsic that `nth`/`drop`
     call, mirroring the `eu-m93j`/`LookupOnFunction` precedent
     (`src/eval/error.rs`, `tests/harness/errors/191_m93j_lookup_on_function.eu`).
  Prefer option 1 unless the length check needs an intrinsic that doesn't
  exist yet — this keeps the change in Clarion's prelude/error-diagnostics
  lane rather than growing the `ExecutionError` enum for a single case.
- Create: `tests/harness/errors/193_1tkk_7_11_nth_out_of_range.eu` +
  `.eu.expect` (next number after `192_1tkk_7_9_...`; confirm the next-free
  number by re-listing `tests/harness/errors/` at implementation time, since
  other agents may land numbers concurrently).
- Modify: `tests/diagnostics/corpus/nth_out_of_range.eu.meta.toml` — update
  `expected_class`/remove `xfail` once this lands (see Task 6).

- [ ] **Step 1: Write the failing harness error test** (`.eu` + `.eu.expect`
  asserting `stderr` contains `"index 10 out of range for list of length 3"`
  or the exact wording chosen — no notes/hints appended).
- [ ] **Step 2: Run, confirm FAIL** against today's `"tail of empty list"`.
- [ ] **Step 3: Implement** the chosen mechanism.
- [ ] **Step 4: Run, confirm PASS.** Also re-run
  `EU_ERROR_TRACE_DUMP=1 ./target/debug/eu tests/diagnostics/corpus/nth_out_of_range.eu`
  and confirm the primary now lands where Task 0/3 determined it should.
- [ ] **Step 5: Fault-injection verify:** revert the bounds check; confirm
  the harness test FAILs with the old `"tail of empty list"` message;
  restore; confirm PASS.
- [ ] **Step 6: Commit.**

```bash
cargo fmt --all && cargo clippy --all-targets -- -D warnings
git add lib/prelude.eu src/eval/error.rs tests/harness/errors/193_1tkk_7_11_nth_out_of_range.eu*
git commit -m "fix(diagnostics): nth raises its own boundary error, not drop's internal failure (eu-1tkk.7.11)"
```

---

## Task 5: `--debug-trace` — raw uncurated trace behind a flag

**Files:**
- Modify: `src/driver/options.rs` — add `#[arg(long = "debug-trace")]
  pub debug_trace: bool` alongside `error_format` (`:241-242`), threaded the
  same way (`:711-713`, `:1155-1156`).
- Modify: `src/eval/error.rs` / `src/driver/eval.rs` — when `debug_trace` is
  set, render today's uncurated stack-trace note (the existing behaviour
  this plan is about to make non-default) instead of the curated one from
  Task 3. This is a **rendering choice**, not a new dump mechanism — keep it
  distinct from the existing `EU_ERROR_TRACE_DUMP` env var (that one dumps
  internal Smid bookkeeping for developers debugging the diagnostics system
  itself, per its own doc comment at `error.rs:1058-1060`; `--debug-trace` is
  a user-facing "show me everything" escape hatch).

- [ ] **Step 1: Write the failing CLI integration test** (extend
  `tests/diagnostics_json_emission.rs` or a new file): run a `map`-wrapped
  error fixture with and without `--debug-trace`; assert the flagged run's
  trace contains a `Transparent`-classified frame (or, for the human-format
  path, contains a `[prelude]` line) that the default curated run omits.
- [ ] **Step 2: Run, confirm FAIL** (flag doesn't exist / has no effect).
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run, confirm PASS.**
- [ ] **Step 5: Fault-injection verify:** hardcode the flag to `false`
  inside the rendering branch; confirm the test's flagged-run assertion
  fails; restore.
- [ ] **Step 6: Commit.**

```bash
cargo fmt --all && cargo clippy --all-targets -- -D warnings
git add src/driver/options.rs src/eval/error.rs src/driver/eval.rs tests/diagnostics_json_emission.rs
git commit -m "feat(diagnostics): --debug-trace exposes the raw uncurated trace (eu-1tkk.7.12)"
```

---

## Task 6 (PARTIAL): Flip corpus xfails + add coverage fixtures

**Progress.** `nth_out_of_range` was flipped off `xfail` by PR #1050 (Task 0's
capture fix), and its `.meta.toml` records that only the *location* is fixed —
the message still says "tail of empty list", naming the wrong internal
operation, which is Task 4's job. `hof_bad_arg` and `swap_args` remain `xfail`
with reasons updated to record that they were checked against PR #1050 and are
not fixed by it. The two new coverage fixtures (`lookup_boundary`,
`filter_transparent`) are still to be added, and depend on Tasks 2–3.

**Files:**
- Modify: `tests/diagnostics/corpus/{swap_args,hof_bad_arg,nth_out_of_range,metadata_span}.meta.toml`
- Create: `tests/diagnostics/corpus/lookup_boundary.eu` (+ `.meta.toml`) —
  a `lookup` failure demonstrating a `Boundary`-named frame distinct from
  `nth`.
- Create: `tests/diagnostics/corpus/filter_transparent.eu` (+ `.meta.toml`)
  — a `filter`-wrapped user-function error demonstrating a `Transparent`
  frame correctly dropped from the curated trace.

- [ ] **Step 1:** For each of the four existing xfail fixtures, re-run
  `cargo test --test diagnostics_invariants` after Tasks 1–4 land. Remove
  `xfail`/`xfail_reason` **only** for fixtures that now satisfy all five
  invariants under the default (source-compiled-prelude) test
  configuration. If Task 0/Open Question 2 concluded some cannot reach a
  user primary without further capture-side work, leave `xfail` in place
  with an updated `xfail_reason` citing the specific gap and the bead
  tracking it — do not force a fixture to pass by relaxing invariant (i).
- [ ] **Step 2:** Add the two new coverage fixtures, verify they provoke
  errors and satisfy the invariants (no `xfail`).
- [ ] **Step 3:** Run `cargo test --test diagnostics_invariants` — the
  "unexpected pass" branch (`tests/diagnostics_invariants.rs:576-579`) must
  be clean, i.e. every remaining `xfail` fixture must still genuinely fail
  at least one invariant, not silently over-satisfy.
- [ ] **Step 4: Commit.**

```bash
cargo fmt --all
git add tests/diagnostics/corpus/
git commit -m "test(diagnostics): flip fixed xfails, add lookup/filter blame coverage (eu-1tkk.7.11/.12)"
```

---

## Task 7: Documentation

**Files:**
- Modify: `docs/superpowers/diagnostics/PROTOCOL.md` (Phase 0's doc) — add a
  section on frame classification: what `` ` :transparent ``/`` ` :boundary ``
  mean, how to declare them on a new prelude combinator, and how
  `--debug-trace` relates to the invariant gate (positive guidance only,
  per CLAUDE.md's "Positive guidance only" rule — no anti-pattern lists).
- Modify: `docs/reference/agent-reference.md` if blame metadata syntax is
  something prelude-authoring agents need to know (confirm whether this
  belongs there or is Clarion-internal-only — the vocabulary is for
  `lib/prelude.eu` maintainers, a narrower audience than general `.eu`
  authors, so a short PROTOCOL.md mention may suffice without touching the
  agent-reference).

- [ ] **Step 1: Write the doc section.**
- [ ] **Step 2: Commit.**

```bash
git add docs/superpowers/diagnostics/PROTOCOL.md
git commit -m "docs(diagnostics): document blame vocabulary and curated trace (eu-1tkk.7.11/.12)"
```

---

## Task 8: Close-out

- [ ] **Step 1:** Open one PR per bead deliverable (`.7.11`, `.7.12`) — each
  PR body states the fault-injection verification performed for every test
  it adds, and explicitly states which corpus xfails it flips (if any) and
  which remain open with reasons.
- [ ] **Step 2:** `bd update eu-1tkk.7.11 --status in_review` (and `.7.12`)
  — do not close.
- [ ] **Step 3:** PRs go to **Wicket** for review and merge under the normal
  gatekeeping route (see Global constraints). Message the coordinator when a
  PR is ready. Do not attach an "owner reviews personally" hold to a PR from
  this plan.

---

## Risks / Open Questions for the owner

> **Resolution status (2026-07-23).** Question 1 is **answered**; Question 2
> is **partly answered**; Question 3 is **still open but narrowed**; Questions
> 4 and 5 are **still open**, both being downstream of Tasks 2–3, which have
> not shipped. Each entry below carries its own resolution note. Nothing in
> this section gates any task — Question 1's gate in particular is discharged.

1. **(Biggest) Blob-path scope.** — **ANSWERED: option (b), pull Task 2b into
   0.14.** Delivered by PR #1052 (wire v4 → v5), and the feared cost did not
   arise: the reserved-`Smid`-sub-range approach left the evaluation hot path
   untouched and proved perf-neutral. Sign-off recorded on PR #1052 and on
   bead `eu-1tkk.7.11`. The original framing follows for the record.

   Blame classification as designed in Tasks
   1–3 works on the source-compiled-prelude path (dev builds; this repo's
   default `cargo test`) but has **no effect on the shipped, blob-based
   release binary** without Task 2b's additional plumbing (touches hot-path
   `Vec<Smid>` trace collection in both engines and possibly the blob wire
   format — a recorded-review-gated surface). Decide: (a) ship Phase 2 for
   the dev/CI-verified path only in 0.14 and track Task 2b as a named
   follow-up bead, explicitly documenting that real users on the released
   binary won't see the improvement until it lands, or (b) pull Task 2b into
   0.14 now, accepting the larger, riskier scope and the extra review this
   plan flags.

2. **Trace-capture gap for `nth`/`map`/`drop`-class errors.** — **PARTLY
   ANSWERED.** Task 0 reported: for `nth_out_of_range` the fix was a local
   capture-side change (the `Update` continuation carried no annotation), not
   Phase 3 laziness-survival. Shipped as PR #1050 / `eu-1tkk.7.18`; that
   fixture now reaches a user primary and has flipped off `xfail`.
   **`hof_bad_arg` and `swap_args` remain open** and were verified not to be
   fixed by PR #1050: `hof_bad_arg`'s arity error is raised from `map`'s
   `Case` fallback with no `Update` continuation on the path back to the user,
   and `swap_args` is a separate capture-side gap in the `DirectApp`
   exact-arity fast path (owned by furnace, still in flight). Original
   framing follows.

   Verified live:
   for `nth_out_of_range`, `hof_bad_arg`, and `swap_args`, the raw scraped
   trace contains **no** user-file frame at all — not a prioritisation bug,
   an absence. Task 0 investigates whether the fix is a local capture-side
   change (plausibly in scope) or shares Phase 3's laziness-survival
   difficulty (explicitly out of scope, per the coordinator's brief). Until
   Task 0 reports, it is unknown whether these three flagship xfails can
   actually flip to passing under this plan's Tasks 1–4, or whether Task 3's
   fallback (Open Question below) is needed.

3. **Should a declared-boundary frame's own (prelude) location ever be an
   acceptable primary?** — **STILL OPEN, but narrowed.** The carve-out proved
   **unnecessary for `nth_out_of_range`**: Task 0's capture fix recovered a
   genuine user call-site, so invariant (i) holds there without any exception.
   The question survives only for the classes Task 0 did not reach —
   `hof_bad_arg` and `swap_args` — and should be decided once those two
   capture gaps are either closed or shown to be genuinely unrecoverable, not
   before. Original framing follows.

   If Task 0 finds the user call-site genuinely cannot
   be recovered for some error classes, the only way to satisfy invariant
   (i) ("primary never in the prelude") is either to leave the primary
   suppressed (today's post-Phase-1 behaviour — correct but still
   unsatisfying) or to carve out an explicit, narrow exception: a
   *declared-boundary* frame's own location becomes primary, clearly
   presented as "in `nth`" rather than a bare `[prelude]:NNNN`. This is a
   policy decision belonging to the owner, not something to default into
   silently — recommend deciding this once Task 0's findings are in, not
   before.

4. **Arithmetic/native-intrinsic blame.** `.7.11`'s bead text explicitly
   names "arithmetic ops" alongside `map`/`fold`/`nth`/`lookup`. These are
   Rust intrinsics (`ADD`/`SUB`/…), not `lib/prelude.eu` declarations, so
   Task 1's backtick-metadata mechanism does not reach them. This plan
   scopes a small Rust-side sibling table to `intrinsic_display_name`
   (`sourcemap.rs:443`) for their classification, folded into Task 2 — flag
   if the owner wants this split into its own task/PR instead.

5. **`compress_trace_cycles` generalisation risk.** Low — it is a pure,
   already-tested function; genericising it is a mechanical refactor with
   existing tests as a regression net. Called out only because it is a
   shared dependency between the pre-existing human-trace formatting and the
   new curation pipeline, so a mistake here has blast radius across both.
