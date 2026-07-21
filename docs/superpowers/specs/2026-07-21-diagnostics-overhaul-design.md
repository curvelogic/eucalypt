# 0.14 Diagnostics Overhaul — "actionable errors"

Status: **DRAFT for owner review** · Author: coordinator (with Greg) · Date: 2026-07-21

## 1. Problem

Eucalypt's error diagnostics are unreliable in the two ways that matter most to a
user: they frequently attribute an error to the **wrong source location**, and
their traces are **large, prelude-dominated, and devoid of usable context**. The
owner reports the problem has worsened recently and, separately, that he has
"never seen a useful stack trace".

Evidence was gathered on `a29d33f3` (0.13.0) by three means: a 39-specimen
provocation sweep (verbatim outputs in the session record), a full architecture
map of the diagnostic path, and a literature review. Key findings:

- **Wrong location.** Any error raised inside a higher-order prelude combinator
  (`map`/`filter`/`fold`/`nth`/`lookup`) reports `[prelude]:NNNN` as the primary
  span with an all-prelude stack trace and **zero user frames**. Example:
  `xs nth(10)` on a 3-element list yields `tail of empty list` at
  `[prelude]:1329`, 15 prelude frames, naming the wrong operation.
- **Right location, wrong words.** `add.x` (lookup on a function) yields the
  self-contradicting `tried to call a function as a function`; catenation-precedence
  traps report a partial application as `found block` and then suggest
  `block.field`, actively misdirecting the user.
- **Doc metadata rendered as a source location.** Declaration Smids span the
  whole declaration *including its leading backtick metadata block*, so when such
  a Smid becomes an error label, codespan renders the doc comment as the location.
  Reproduced minimally in-session.
- **Traces are the wrong artefact.** What is printed as a "stack trace" is two
  Smid lists scraped off the continuation stack and the lexical environment chain
  at crash time. Under laziness the continuation stack is *the spine currently
  being forced*, not the causal chain that led there — so it structurally cannot
  answer "what was the program trying to do?".
- **The machinery can already do well.** An error inside an *imported user file*
  produces a correct cross-file `called from here` chain landing on the user's
  line. The failures are specific mechanisms, not universal breakage.

Two hypotheses for "got worse" were **falsified** in-session: the bytecode/predecode
engine migration (`EU_HEAPSYN=1` and the default engine produce byte-identical bad
output) and the prelude blob (not active in the test build; prelude *source* lines
still appear). The regression, if real, is elsewhere and remains unpinned — a
git-bisect of a fixed specimen across release tags is the tool to locate it, and
real owner-supplied specimens would settle representativeness. This is tracked as
an early task, not a blocker.

### Explicitly *not* this project

The sweep also surfaced cases where eucalypt silently accepts defective-looking
source and produces a wrong answer (exit 0). On review, most are **legitimate
language semantics** (adjacent-block catenation, cons, backtick metadata) rather
than diagnostic bugs. The genuine bugs among them — a dropped second `import`
block, a swallowed parse error in an imported file, an inert undefined type
alias, and the `eu check` ↔ `eu` disagreement — are **loader / type-checker
completeness** issues, a different effort. They are filed as separate beads and
are out of scope here. Conflating them with diagnostic quality would misdirect
this work toward "add errors where the language deliberately has none".

## 2. Goals and non-goals

**Goals**
1. Errors locate in a **user** file — never the prelude, never a metadata span —
   whenever any user frame is available.
2. Traces are **curated, budgeted, and user-anchored**: prelude internals skipped
   by default, recursion collapsed, phrased in terms the user wrote.
3. Diagnostic quality becomes **measurable and regression-proof**: an objective
   invariant gate in CI that can only tighten, plus behavioural signal on whether
   messages enable a fix.
4. Diagnostics become a **standing concern** in the roadmap and review process,
   not a one-off push.

**Non-goals for 0.14** (0.15 spikes, §7): output-document-path context, dynamic
witnesses, error slicing. Loader/type-checker completeness (§1). A validated
automatic quality *metric* — the literature says none exists; we will not pretend
otherwise (§5).

## 3. Design principles (from the literature)

The academic and practitioner consensus, developed largely independently, agrees:
locate precisely (smallest self-sufficient span), explain in the **reader's**
vocabulary not the implementer's, always offer a next action, **mark the
confidence** of that action, keep the default terse with depth behind an explain
flag, and emit machine-readable structured output alongside human text.

Anchors: Becker et al., *Compiler Error Messages Considered Unhelpful* (ITiCSE-WGR
2019); Barik et al., *Do Developers Read Compiler Error Messages?* (ICSE 2017 —
reading a message is as hard as reading source and predicts task performance);
Marceau/Fisler/Krishnamurthi (SIGCSE/Onward! 2011 — evaluate a message by the
**edit it provokes**, and vocabulary is a first-class failure mode); the rustc
diagnostics model (level/code/spans-with-labels/notes/help/suggestions +
`Applicability` confidence); Elm's *Compiler Errors for Humans*.

Language-specific leads: **laziness decouples failure from cause** (GHC's 20-year
struggle; Well-Typed's 2025 surviving stack annotations); **blame at the boundary**
(Wadler–Findler, *Well-Typed Programs Can't Be Blamed*, with Chen–Campora's
warning that inference-language blame is *biased* toward the cast site);
**scriptable per-function reporting** (Heeren) — the sleeper idea, because the
prelude is where most user-visible errors surface; and **dynamic witnesses**
(Seidel/Jhala/Weimer, ICFP 2016) as the strongest lead for a data-templating
language, deferred to a spike.

## 4. Architecture

### 4.1 Structured diagnostic substrate (Phase 0)

Introduce a single internal diagnostic representation — `level`, `code`,
`primary span`, `labelled spans`, `notes`, `help`, `suggestion` (with an
`Applicability` confidence enum), and an explicit **trace model** — that is
`serde`-serialisable to JSON. All renderers (runtime path in `driver/eval.rs`,
`eu check`/warnings, and the LSP converter in `driver/lsp/diagnostics.rs`) are
unified onto it. This directly closes the current **JSON-vs-human divergence**
(`error_to_json` bypasses the trace-fallback logic) and lets tests assert on
*structure* separately from *prose*.

Error **codes** with `eu explain <code>` fold in bead `eu-67v5` (promoted from
P4). Codes give the harness a stable identifier that survives rewording.

### 4.2 Location correctness (Phase 1)

Point fixes to the attribution bugs, each independently fault-injection tested:

- **Metadata-in-span.** A declaration's label span must start at the declaration
  *head*, not its leading backtick metadata. Either narrow the Smid the desugarer
  mints for the declaration body, or derive a body-only span at label time.
  (`core/desugar/desugarer.rs` `new_smid`; verify against the reproduction.)
- **Location selection.** In `ExecutionError::to_diagnostic`, generalise the
  existing `AssertionFailed`-only suppression into a rule: a prelude or
  metadata-only Smid is never the primary when any user frame exists; prefer the
  nearest **user call-site**. Re-examine the current env-trace-before-stack-trace
  preference, which promotes lexical *definition* sites over call sites.
- **Wrong-noun.** Partial applications / anaphoric lambdas get a distinct display
  kind ("a partial application" / "an anaphoric function"), and the misdirecting
  `did you mean block.field?` hint is gated off that kind.
- **`eu-m93j`.** `tried to call a function as a function` → e.g. `cannot look up
  key 'x' on a function value` + `help: '.' looks up keys in blocks`.
- **Panic-safety.** Guard `Smid::get()` (`sourcemap.rs:64`) so a future unfiltered
  trace source cannot panic *during error reporting*.

### 4.3 Curated trace (Phase 2) — blame annotations

We do not build a new trace; we **curate the raw material already scraped**, and
the act of curation fixes both location and wording at once.

**Frame classification.** Every frame's Smid is tagged:

1. **User frame** — resolves to a user file. *Always kept.* The anchors.
2. **Transparent library frame** — a combinator's internal plumbing (`map`/`fold`
   recursing on `tail`). *Dropped; blame passes through to the caller.*
3. **Boundary frame** — the combinator the user actually named (`nth`, `head`,
   `lookup`, an arithmetic op). *Kept as a named secondary* ("in `nth`"), while
   the **primary is reassigned to the nearest user call-site**.

The classifier is driven by a **blame annotation** declared on each prelude
combinator (Heeren-style scripted reporting): `map` declares *transparent* ("if my
function argument faults, blame my caller"); `nth` declares *boundary* ("an
out-of-range access is my contract — name me, attribute the call to the user, and
let me phrase the error in my own terms"). This replaces today's fragile
`is_user_file`/env-trace heuristics with an explicit, per-function contract, and
fixes the `[prelude]:NNNN` class **at the source**.

The boundary is where blame attribution and rephrasing coincide: because `nth` is
a declared boundary, it raises `index N out of range for list of length M` at its
own edge instead of leaking `drop`'s mechanical `tail of empty` fifteen frames
down. "Curated trace" and "better message" are the same mechanism.

**Curation pipeline** (over the scraped trace): classify → drop transparent frames
→ collapse recursion (reuse `compress_trace_cycles`) → keep user + boundary frames
→ **budget** the length. The raw continuation dump moves behind `--debug-trace`.

**Before / after** (specimen 25, `xs nth(10)` on a 3-element list):

```
# today
error: tail of empty list
  ┌─ [prelude]:1329:46                 ← drop's internal `l tail`
  = stack trace:
    - nth at [prelude]:1380
    - drop at [prelude]:1329            (×15, recursion elided)

# curated
error: index 10 out of range for list of length 3
  ┌─ your.eu:3:9                        ← xs nth(10), your call
  = in 'nth' (list access)
```

### 4.4 Cause-trace (Phase 3) — surviving the lazy offset

Curation improves the **common** case (direct pipelines, where the call chain is
on the continuation stack at force-time). It cannot help when a thunk built at
line 5 is only forced during line 40's evaluation — the crash-time stack need not
contain line 5's chain at all.

Phase 3 addresses this with a **demand/cause chain that survives thunk suspension**
(Well-Typed's surviving-annotation approach): user-named bindings carry a
lightweight provenance that is captured into the thunk when it is built and
reconstructed at force-time, yielding "this was forced because `report` needed
`servers` needed …". This is the technically hardest part — GHC has iterated on it
for two decades — and is **gated behind a spike checkpoint**. If the spike shows
it too deep for 0.14, it drops to a 0.15 item and Phase 2's curated trace ships
alone, which already beats the status quo.

## 5. The evaluation harness (Phase 0)

Four layers, in ascending speculativeness:

1. **Provocation corpus.** Valid programs + seeded, semantically-motivated
   mutations (rename to undefined, swap arg order, wrong literal type, break a
   lookup key), seeded from the existing 173-file `tests/harness/errors/` corpus.
   Literature-backed as cheap; doubles as a **panic-hunter** (serves the
   "panics are critical" rule directly).
2. **Objective invariant gate — the hard CI ratchet.** Every produced diagnostic
   MUST satisfy: (i) primary location in a **user** file — never prelude, never a
   metadata span; (ii) no panic; (iii) location inside the mutated region;
   (iv) trace contains ≥1 user frame; (v) trace length ≤ budget. These are
   objective and CI-enforceable; the gate can only tighten. This is the mechanical
   guarantee that diagnostics keep improving.
3. **Golden snapshots** (`--bless` workflow, insta-style) locking full presentation
   for regression, with **prelude source suppressed** from snapshots to avoid
   churn (rustc's `ignore-directory-in-diagnostics-source-blocks` analogue).
   Snapshot diffs are covered by PR review, per the anti-rubber-stamping norm.
4. **AI layers.** *AI-as-fixer* (tracked, **not** gated): an agent receives **only**
   the broken program + our diagnostic and attempts a repair; fix-success-rate is
   a **behavioural** quality signal over time — the automatable analogue of
   Marceau's "measure the edit" oracle. *AI-as-critic* (authoring aid, **firewalled
   from all scoring**): an LLM drafts and critiques message wording.

**Honesty guardrail.** There is no validated automatic quality metric, and
LLM-as-*judge* specifically failed to predict human outcomes (Santos & Becker,
*Not the Silver Bullet*, UKICER 2024 — automated proxies rated LLM messages good
while humans found them unhelpful). We therefore **never** gate on, or report as
validated, an LLM opinion score. The hard gate is the objective invariants; the AI
fix-rate is a tracked signal with stated limits; the AI critic is firewalled to
authoring.

## 6. Roadmap & process changes

- A standing **0.14 diagnostics epic** with the phase structure above.
- The **invariant gate as a CI ratchet**, mirroring the engine-ab and
  sensitive-surface-review mechanisms already in force.
- A **"diagnostics" review dimension** added to PR review.
- A **"confusing-message" bead label** (Elm's error-message-catalog model) for
  cheap intake of real cases as they are hit.

## 7. Scope, sequencing, risk

**Sequencing.** Phase 0 first: without the harness we cannot prove Phases 1–3
improved anything, and the invariant gate immediately catches the metadata/prelude
regressions as they are fixed. Then Phase 1 (fixes) → Phase 2 (blame + curation) →
Phase 3 (spike-gated cause-trace).

**Primary risk.** Phase 3's laziness-surviving annotations. Mitigated by the spike
gate and graceful degradation to Phase 2.

**Secondary risk.** Snapshot churn / rubber-stamping. Mitigated by prelude-source
suppression, aggressive normalisation, and mandatory review of `.stderr` diffs.

**0.15 backlog (out of scope):** output-document-path context
(`$.report.servers[2].port`), dynamic witnesses, error slicing.

**Separate beads (not this epic):** loader/type-checker completeness — dropped
import blocks, swallowed imported-file parse errors, inert undefined type aliases,
`eu check` ↔ `eu` disagreement.

## 8. Existing beads folded in

- `eu-m93j` (P2) — "function as a function" nonsense → Phase 1.
- `eu-67v5` (P4 → promote) — error codes & catalogue → Phase 0.
- `eu-5tl2`, `eu-1tkk.6` — lenient-parse *diagnostic* warnings (`=` vs `:`,
  body-parses-as-metadata) → candidate Phase 1 additions (diagnostic warnings, not
  loader changes).

## 9. Open questions for owner review

1. Does Phase 3 belong in 0.14 (spike-gated) or drop wholesale to a 0.15 spike?
2. Is a git-bisect to pin the "got worse" regression worth an early task, or do you
   have real specimens that would settle it faster?
3. Error-code scheme — namespaced by phase/subsystem (parse/name/type/eval), or a
   flat `E0001`-style sequence as in rustc?
