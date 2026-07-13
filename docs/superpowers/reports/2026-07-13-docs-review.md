# 0.13 Documentation Review — Phase 1 Audit Report

**Bead**: eu-2sa6.15 (absorbs eu-lyu9, eu-p7tk, eu-1n5z)
**Date**: 2026-07-13
**Scope**: everything under `docs/`, `README.md`, and the generated
artefacts (`docs/llms.txt`, `docs/llms-full.txt`, `docs/reference/prelude/`).
**Method**: every file was read; every runnable `eu`/`sh` example was
executed against a freshly built `eu 0.12.1` release binary
(`cargo build --release`, prelude blob compiled via
`cargo xtask prelude-compile`) and compared against its documented
output. `docs/reference/prelude/` was regenerated with `eu doc
--output-dir` and diffed against the committed files.
`docs/llms-full.txt` was regenerated with
`scripts/generate-llms-full.sh` and diffed. A repo-wide Python script
checked every `[text](path)` internal markdown link resolves. The
existing `scripts/test-doc-examples.py` harness (exit-code-only,
CI-run) was also executed for completeness.

This report is Deliverable 1 (audit). Deliverable 2 (the mechanical-fix
PR) is on the same branch, `docs/full-review-phase1`, off `master`.

---

## Headline numbers

- **31 confirmed factual/example errors** fixed in the mechanical-fix PR
  (see §3), across 24 documentation files, plus 6 harness test files
  and `CHANGELOG.md` (the eu-1n5z reflect-import idiom cleanup).
- **`docs/reference/prelude/*.md`**: **no drift** — byte-identical to a
  fresh `eu doc --output-dir` regeneration. eu-p7tk's premise ("badly
  stale") does not hold for the prelude reference pages specifically.
- **`docs/llms-full.txt`**: was **also already byte-identical** to a
  fresh regeneration *before* this review's other edits landed (so
  eu-p7tk's specific complaint about `llms-full.txt` itself appears to
  have been resolved by an earlier commit, `22390547`). It has now been
  regenerated again to absorb this review's ~30 content fixes across
  the source docs it concatenates.
- **`docs/llms.txt`** (hand-maintained, no generator) was genuinely
  stale: prelude counts were off by 40–70% per category (218 claimed
  vs. 370 actual total), and 6 of 20 guide chapters were missing from
  its index entirely. Fixed.
- **eu-lyu9** (idiot brackets / lens coverage): **substantially already
  resolved** by prior work — see §5.
- **eu-1n5z** (reflect.eu import idiom): was **not** resolved — fixed
  in this PR (6 harness files + CHANGELOG.md), harness re-verified
  green.
- **#1 remaining gap, unambiguously**: `docs/architecture.md` describes
  only the legacy HeapSyn tree-walk VM and has zero mention of the
  bytecode engine (BV1) that has been the *default* since 0.12.0. A
  light pointer/note was added as a mechanical fix (§3); a full
  rewrite of the "STG Compilation and Evaluation Model" section is
  phase-2 work (§6, ranked #1).

---

## 1. Per-file verdict

Legend: **accurate** (no issues found) / **stale** (correct-at-the-time
content now contradicts current behaviour) / **gap** (missing content,
nothing wrong with what's there) / **broken-example** (a code sample
errors or produces output that doesn't match what's claimed) /
**fixed** (was one of the above, corrected in this PR's mechanical-fix
commit).

### Getting Started (`welcome/`)

| File | Verdict | Notes |
|---|---|---|
| `welcome/index.md` | fixed | hyphenated-identifier example bug (`x-2` parsed as one identifier, not `x - 2`); stale YAML-merge-keys disclaimer ("eucalypt doesn't support this... yet") — it does, since the CSV/YAML import work. |
| `welcome/what-is-eucalypt.md` | accurate | |
| `welcome/quick-start.md` | fixed | frozen `eu --version`/`eu --help` transcripts were badly stale (`eu 0.3.0`; missing `check`/`doc` subcommands; listed global options that have moved under `eu run --help`). Regenerated against the current binary. |
| `welcome/by-example.md` | fixed | 4 examples had missing commas in multi-line list literals (§3.1), silently producing 1-element merged blocks instead of the claimed N-element lists; example 2's `eu -e 'map(.name)' <<JSON` doesn't bind stdin the way shown, fixed to `eu u=- -e 'u map(.name)'`. |

### The Eucalypt Guide (`guide/`)

| File | Verdict | Notes |
|---|---|---|
| `blocks-and-declarations.md` | accurate | |
| `expressions-and-pipelines.md` | fixed | missing-comma list bug (§3.1). |
| `lists-and-transformations.md` | fixed | missing-comma list bug (§3.1). |
| `string-interpolation.md` | fixed | missing-comma list bug; `"{42:%06d}"` collided with numbered string anaphora (bare numeral inside braces is an anaphor index, not a literal) — bound to a name instead. |
| `functions-and-combinators.md` | fixed | `flip` pipeline-adaptation example was unparseable (`merge flip ({...})`); corrected to `flip(merge)({...})`. `ops: {...}` example's expected output omitted the (correctly rendered) empty `ops: {}` line. |
| `operators.md` | fixed | `//=` was documented as "returns the value if true" — it returns `true` (a bool); `//=>` is the one that returns the value. Nullary-operator example's expected output omitted the rendered `∅: []` line (nullary declarations have no params, so — unlike functions — they *are* rendered). Precedence table was missing `~`, `✓`, `∘`/`;`, `<$>`, `=>`/`⇒`. |
| `anaphora.md` | accurate | |
| `block-manipulation.md` | accurate | |
| `navigating-nested-data.md` | fixed | 2× missing-comma list bug; `str.upper` doesn't exist (`str.to-upper`). |
| `lenses.md` | accurate | every example executed and matched; good eu-lyu9 coverage (§5). |
| `imports-and-modules.md` | accurate | |
| `working-with-data.md` | fixed | `unique` doesn't exist in the prelude — replaced with `set.from-list set.to-list`. |
| `command-line.md` | fixed | subcommand table was missing `check` and `doc`; zero mention anywhere of `-I`/`--allow-io`, the mandatory safety gate for all IO operations — added a new "IO Permission" section. |
| `yaml-embedding.md` | accurate (not independently re-executed against the YAML-tag pipeline — flagged, not a known issue) | |
| `testing.md` | accurate (not independently re-executed — flagged, not a known issue) | |
| `date-time-random.md` | fixed | `cal.format("%Y-%m-%d")` doesn't exist — `cal.format` takes no format-string argument, always ISO8601 (`lib/prelude.eu:1882-1883`). |
| `io.md` | fixed | `_(r): io.shell(...)` inline-lambda example doesn't parse (eucalypt has no lambda syntax) — replaced with a named function. |
| `monads.md` | fixed | added a one-line caveat that the `λr. ...` desugaring pseudocode isn't real eucalypt syntax (it directly contradicts the "no lambda syntax" rule with no disclaimer). |
| `state-monad.md` | fixed | the chapter's own opening/motivating example (`s0 merge({count: 1})`) silently no-ops due to the catenation-puts-receiver-last rule — `merge(a,b)` has `b` win, but catenation makes `s0` the *last* (winning) arg, backwards from intent. Fixed to direct-call form `merge(s0, {count: 1})`. |
| `type-checking.md` | fixed | opened with "Eucalypt includes an **optional**, advisory type checker" and showed `eu --type-check file.eu # check then evaluate` as the way to enable it. Both wrong: type checking is unconditional since 0.11, and `--type-check` is a deprecated no-op. Rewrote the intro and command trio. |
| `advanced-topics.md` | fixed | `group-by` example had the missing-comma list bug. |

### Reference (`reference/`)

| File | Verdict | Notes |
|---|---|---|
| `syntax.md` | accurate | |
| `operators-and-identifiers.md` | fixed | **structural gap**: `SUMMARY.md` titles this page "Operator Precedence Table" but the file contained no such table (it's entirely about identifier syntax/shadowing). Added the full verified precedence table (reusing already fact-checked content from `agent-reference.md` §2, not new research). |
| `cli.md` | fixed | "Type Checking" section claimed `--type-check` is required to run the checker — wrong, same issue as the guide chapter; rewrote. `--suppress-type-warnings` flag was completely undocumented — added. `-x html` export format was undocumented in the Outputs section — added, with a pointer to `export-formats.md`. |
| `error-messages.md` | gap | still 4 lines, self-labelled "under construction." No mechanical fix possible — this is a phase-2 content-authoring task (see §6). |
| `export-formats.md` | fixed | table was missing the `html` format (`-x html`, confirmed real via `src/export/mod.rs:29-40` and live test) — added. |
| `import-formats.md` | accurate (YAML anchors/merge-keys/timestamps content not independently re-verified against `src/import/yaml.rs` — flagged, not a known issue) | |
| `agent-reference.md` | accurate | this file is well-maintained; used throughout this review as a source of already-verified ground truth (its precedence table, type-form table, and prelude examples were all spot-checked and hold up). |
| `prelude/*.md` (11 category pages + index) | accurate | byte-identical to fresh `eu doc --output-dir` regeneration — **no drift**. |

### Appendices

| File | Verdict | Notes |
|---|---|---|
| `appendices/cheat-sheet.md` | fixed | same `--type-check` staleness as above — corrected the CLI quick-reference block. |
| `appendices/syntax-gotchas.md` | accurate | well-maintained; no stale facts found. See §6 for one recommended new-content addition. |
| `appendices/migration.md` | accurate | correctly scoped as a historical v0.2→v0.3 document; not confused with current version. |

### Understanding / meta / generated

| File | Verdict | Notes |
|---|---|---|
| `architecture.md` | fixed (partial) + flagged | **severe staleness**: the entire "STG Compilation and Evaluation Model" section describes only the tree-walk `MachineState` (`src/eval/machine/vm.rs`) with zero mention that this is now the *opt-in* engine (`EU_HEAPSYN=1`) and that the bytecode VM (`src/eval/bytecode/`) has been the *default* since 0.12.0. Added a factual note/pointer as a safe mechanical fix (does not invent the missing architectural description — that's phase-2, ranked #1, see §6). Also fixed: "Further Reading" section listed six bare filenames as plain text (not links), two of which (`implementation.md`, `anaphora-and-lambdas.md`) don't exist anywhere in the tree; converted to real relative links and dropped/redirected the phantom entries. |
| `eucalypt-style.md` | accurate | spot-checked examples run correctly; style guidance (prefer `then`/pipelines/point-free) still matches current idiom. |
| `faq.md` | accurate | no stale engine/type-checking claims found. |
| `understanding/philosophy.md` | accurate | |
| `understanding/lazy-evaluation.md` | accurate | |
| `README.md` | accurate | installation instructions and the one code example both verified correct. |
| `docs/SUMMARY.md` | fixed | `docs/guide/lsp.md` (94 lines, real content, matches current LSP source module-for-module) existed but was **completely absent from the table of contents** — unreachable in the built book, absent from `llms-full.txt`/`llms.txt`. Added. Everything else in `SUMMARY.md` resolves to a real file; every `docs/**/*.md` not listed is legitimately internal (`docs/development/*`, `docs/superpowers/*`, `docs/reference/prelude/supplements/*` raw fragments) — no other orphans. |
| `docs/llms.txt` | fixed | hand-maintained, no generator (confirmed via repo-wide grep — distinct from `llms-full.txt`, which has `scripts/generate-llms-full.sh`). Prelude category counts were stale by 40-70% (see headline numbers). Six guide chapters were missing from its index entirely: Navigating Nested Data, Lenses, IO and Shell Commands, Monads, The State Monad, Type Checking. Both fixed. |
| `docs/llms-full.txt` | fixed (regenerated) | was already byte-identical to a fresh regen before this review's other content fixes; regenerated once more at the end to pick up all ~30 source-doc corrections. |
| `docs/reference/prelude/*.md` | accurate | see above. |
| `docs/development/*.md` (14 files) | fixed (2 files) | `type-system-evolution.md` and `type-alias-shorthand-spec.md` linked to 4 spec files (`monad-type-checking-spec.md`, `row-polymorphism-and-dict-spec.md`, `prelude-type-cache-spec.md`, `literal-types-and-narrowing-spec.md`, `alias-reference-tooling-spec.md`) deleted in commit `30bad306` ("remove shipped specs, add ROADMAP.md" — the features shipped in 0.6.2/0.7.0). De-linked with a "shipped, spec removed, see ROADMAP.md" note rather than pointing at a 404. The other 12 development docs were not in scope for deep review (internal/dev-facing, not part of the mdbook) but no other broken links were found there. |
| `docs/superpowers/**` | out of scope | internal perf-engineering working docs, correctly excluded from the book and from `test-doc-examples.py`'s scan (see its `find_doc_files` exclusion list). Not reviewed for prose accuracy — see §6 for the one open question (engine-ab protocol / fusion / wire-format-v2 user-doc visibility). |

---

## 2. eu-p7tk status (docs/llms-full.txt stale)

**Verdict: not currently true, or already resolved.** A byte-for-byte
regeneration via `scripts/generate-llms-full.sh` against the
pre-this-review tree produced **zero diff**. Git blame shows a prior
commit (`22390547`, "docs: fix dangling doc links, add head-operator
mentions, regenerate llms-full.txt") already did this. Recommend
closing eu-p7tk as already resolved, or repurposing it to mean "keep
`llms-full.txt` regenerated as part of every docs PR" (there's no CI
check enforcing that today — see §6).

---

## 3. Fix inventory (mechanical-fix PR, same branch)

### 3.1 Missing-comma list-literal bug (cross-cutting)

A cross-cutting authoring habit: multi-line list literals of block
elements written one-per-line with **no trailing commas**:

```eu
people: [
  { name: "Alice" age: 30 }
  { name: "Bob" age: 25 }
]
```

Lists require commas (stated correctly elsewhere in the same docs).
Without them, adjacent `{...}{...}` block literals inside `[...]`
catenate as a **block merge** (later keys win) instead of forming list
elements — silently producing a 1-element list containing the
merged/last block, with no parse error. Found and fixed in **10
locations across 6 files** (`guide/expressions-and-pipelines.md`,
`guide/lists-and-transformations.md`, `guide/string-interpolation.md`,
`guide/navigating-nested-data.md` ×2, `guide/advanced-topics.md`,
`welcome/by-example.md` ×4). A full repo-wide regex sweep was run to
confirm no further instances remain in the reviewed tree (a handful of
matches in `agent-reference.md`, `cheat-sheet.md`, and
`syntax-gotchas.md` were checked individually and are **not** bugs —
either genuinely standalone one-liner examples, or, in
`syntax-gotchas.md`'s case, the deliberate "WRONG" illustration of
this exact gotcha for imports).

### 3.2 Type-checking staleness (cross-cutting)

Three files described `--type-check` as the flag that enables the
type checker and/or described type checking as optional/opt-in.
Ground truth (`src/driver/options.rs:82-85`, CLAUDE.md): type checking
is **unconditional** since 0.11; `--type-check` is a documented
no-op kept for backwards compatibility. Fixed in
`guide/type-checking.md`, `reference/cli.md`, `appendices/cheat-sheet.md`.
(`docs/development/gradual-typing-spec.md:679` has the same stale
line but was left untouched as a historical design document, same
treatment as `appendices/migration.md`.)

### 3.3 Everything else

See the per-file table in §1 for the itemised list; each entry there
names the specific error and fix. All fixes were verified by executing
the corrected example against the release binary and diffing against
the documented "expected output" block, except where noted as
cosmetic-only.

### 3.4 eu-1n5z (reflect.eu import idiom)

Bead's own acceptance criteria: "All reflect-library imports across
`tests/` and `CHANGELOG.md` use `{ import: "reflect.eu" }`; full
harness still green." Done — `resource:reflect` → `reflect.eu` in 6
harness test files (`174_sv1_typedata.eu`, `175_sv2_to_spec.eu`,
`177_sv_optional_fields_to_data.eu`, `178_sv_optional_fields_to_spec.eu`,
`182_typedata_alias_resolution.eu`, `183_widen_type_def_literals.eu`)
and `CHANGELOG.md:33`. All 6 affected tests re-run individually and
pass (`cargo test --release --test harness_test`); full suite
(`cargo test --release`) is green: 1274 + 111 + 7 + 1 + 3 + 489 + 12 +
111 + 11 + 3 = all passed, 0 failed.

### 3.5 Broken internal links

Repo-wide script found 4 raw matches; 2 were false positives (a regex
literal `\d+` inside fenced code being mis-parsed as a markdown link
target by the checker script itself) and 2 were real but already
correct in context (a prelude-reference *supplement fragment*'s
relative link resolves correctly once merged into its parent page,
which was confirmed by checking the actual generated
`docs/reference/prelude/index.md`, not the raw supplement file). The
5 **genuinely** broken links found and fixed were plain-text (not
markdown-syntax) filename references in `architecture.md`'s "Further
Reading" section and true `[text](path)` links to 4 now-deleted spec
files in `docs/development/type-system-evolution.md` and
`type-alias-shorthand-spec.md` (§1, "Understanding / meta / generated"
row for those files).

---

## 4. Verification performed

- `cargo build --release` (toolchain: `stable-aarch64-apple-darwin`,
  `RUSTC=.../rustc RUSTDOC=.../rustdoc` explicitly set — the `rustup`
  shim on this machine breaks `rustc -vV`/doctest sub-invocations
  without it).
- `cargo xtask prelude-compile` (prelude blob regenerated, used for
  all example verification).
- `eu doc --output-dir docs/reference/prelude` — zero diff before and
  after all other edits (prelude reference pages were and remain
  accurate).
- `scripts/generate-llms-full.sh` — zero diff before this review's
  other fixes; regenerated again after, to absorb them (308
  insertions / 85 deletions reflecting the ~30 source-doc fixes).
- `python3 scripts/test-doc-examples.py --eu ./target/release/eu
  --timeout 30` — 243 passed, 0 failed, 342 skipped (585 total), both
  before and after this review's fixes. This harness only checks exit
  code, not output content — it is why the missing-comma bugs (§3.1)
  and several others were invisible to CI despite being wrong. Three
  previously-`notest` blocks were fixed and converted to tested
  (`welcome/index.md`, `welcome/by-example.md` example 10,
  `guide/functions-and-combinators.md`'s `flip` example), raising the
  passed count from 240 to 243.
- `mdbook build` — clean, no warnings, `guide/lsp.html` now appears in
  the built site.
- Custom Python link-checker across every `.md` under `docs/` — 0 real
  broken links remaining (2 known false positives from the checker's
  own regex, documented in §3.5).
- `cargo test --release` (full suite) — every test binary green:
  `harness_test` 489/489, `lsp_test` 111/111, `property_test` 11/11,
  `bytecode_io_differential_test` 7/7, `conform_test` 1/1,
  `fuzz_regression_test` 3/3, `io_args_test` 12/12, lib tests
  1274/1274, doctests 3/3 (2 ignored, pre-existing). No regressions
  from the reflect.eu import-idiom fix or any other change in this PR.
- Every individual example fix in §1/§3 was independently re-run
  against the release binary and its output diffed against the
  documented expected output before being committed to the docs.

---

## 5. eu-lyu9 status (idiot brackets / lens coverage)

**Verdict: substantially already resolved.** The bead's three
sub-concerns:

1. **Idiot brackets undocumented** — no longer true. Documented in
   four consistent places: `reference/agent-reference.md` §1.3.1,
   `guide/operators.md` "## Idiot Brackets" (guide-level introduction
   with the bracket-pair table), `reference/syntax.md`, and
   `appendices/syntax-gotchas.md`'s "Idiot Bracket Gotchas" /
   "Monad Bracket Restrictions" sections. All examples tested and
   correct.
2. **agent-reference.md gotcha/style coverage re-review** — not
   independently re-verified line-by-line against
   `syntax-gotchas.md` in this pass (both files were read and
   spot-checked as part of this review and no gaps were found, but a
   systematic side-by-side diff was not performed). Low-priority
   remainder if still wanted.
3. **Lens library only a "side topic," no introduction** — no longer
   true. `guide/lenses.md` is a full 289-line narrative chapter,
   correctly sequenced in `SUMMARY.md`, and `guide/navigating-nested-data.md`
   independently introduces "what a lens is" before referencing the
   fuller chapter. Every example in both files was executed and
   matched documented output.

**Minor non-blocking observation**: the lens API surface (constructors,
`view`/`over`/`to-list-of`/`parts-of`, composition) is now documented in
*three* places (`agent-reference.md` §11, `guide/lenses.md`,
`guide/navigating-nested-data.md`'s own subsection) — currently
consistent, but three parallel expositions risk drifting apart on a
future API change. Not fixed in this pass (would require judgement
about which to trim — phase-2 material, see §6).

Recommend: close eu-lyu9, or narrow it to just item 2 above if the
owner still wants that specific side-by-side pass.

---

## 6. Phase-2 content plan (ranked)

Nothing below was implemented in this pass — per the bead's scope,
phase 1 is audit + safe mechanical fixes only; these all require new
prose/explanation, which is content-authoring work.

1. **`docs/architecture.md` bytecode engine rewrite (highest priority).**
   The "STG Compilation and Evaluation Model" section needs a real
   description of the bytecode VM (`src/eval/bytecode/`: `opcode.rs`,
   `program.rs`, `machine.rs`, `closure.rs`, `cont.rs`,
   `env_builder.rs`, `encode.rs`) alongside the existing HeapSyn
   tree-walk description — what gets compiled to bytecode, roughly how
   dispatch works, why it's the default, and where the perf story
   currently stands (`docs/superpowers/reports/2026-07-12-transition-review-*.md`
   has the source material). This is the single most consequential gap
   found in this review: the primary architecture document is silent
   on the default execution engine.
2. **`docs/reference/error-messages.md`.** Still a 4-line stub,
   self-labelled "under construction" since at least the last several
   releases. Needs actual content — a guide to reading eucalypt's
   diagnostic format, common error categories, and how to interpret
   stack traces / source locations.
3. **`lib/reflect.eu` (to-data/from-data/as-spec) has zero guide-level
   coverage.** Confirmed via grep: no mention in any `guide/*.md` or
   `reference/agent-reference.md`. `lens.eu` and `state.eu` both have
   dedicated guide chapters; `reflect.eu` ships alongside them with
   none. Needs at minimum a short guide chapter or a subsection of
   `type-checking.md` (since it's about projecting type-data values).
4. **New syntax-gotcha entries** (small, well-scoped, but genuinely new
   prose, so deferred): (a) a numeral inside string-interpolation
   braces (`"{42}"`) collides with numbered string anaphora rather than
   being treated as a literal — found while fixing
   `guide/string-interpolation.md`'s format-specifier example, not
   previously documented anywhere; (b) a hyphenated variable reference
   immediately after an operator with no surrounding whitespace
   (`x-2`) lexes as a single identifier rather than `x - 2` — found
   while fixing `welcome/index.md`'s virtual-functions example, also
   not previously documented; (c) the `!` type-string prefix
   ("asserted" annotation — checker trusts the type without verifying
   the body, `src/core/typecheck/check.rs:895-899`) is used in
   `guide/type-checking.md:261` with zero explanation anywhere,
   including the type-form tables in `agent-reference.md` and
   `cheat-sheet.md`.
5. **YAML cosmetic-output drift across many guide examples** (low
   priority, high volume). Many "expected output" blocks show
   flow-style or single-quoted YAML (`b: [2, 3]`, `label: '99'`) where
   the actual emitter always uses block style and double-quotes
   punctuation-bearing strings (`b:\n  - 2\n  - 3`,
   `label: "99"`). Semantically identical, cosmetically wrong —
   dozens of instances across the guide. A doctest harness that
   diffs *rendered output*, not just exit code, would catch these
   automatically and is the real fix (see item 7).
6. **`docs/superpowers/engine-ab/PROTOCOL.md` / fusion / wire-format-v2
   visibility** — flagged per the dispatch brief, not decided here.
   These are internal perf-engineering artefacts (confirmed: zero
   mentions anywhere in `guide/`, `reference/`, `understanding/`,
   `welcome/`, `faq.md`, `architecture.md`, `eucalypt-style.md`).
   Recommend they stay internal-only (they're not user-visible
   language/CLI surface), with the possible exception of a one-line
   mention of "bytecode wire format" in the architecture.md rewrite
   (item 1) if useful context. Owner call.
7. **`scripts/test-doc-examples.py` only checks exit code, not output
   content.** This is *why* the missing-comma bug (§3.1, 10 instances)
   and several other wrong-output-but-non-erroring examples survived
   in the tree undetected by CI across multiple releases. Extending
   the harness to optionally assert against a documented "expected
   output" fenced block (where one immediately follows the `eu` block)
   would have caught most of what a human reviewer had to find by
   hand in this pass. Not attempted here — a harness change, not a
   docs change, and a bigger lift than "phase 1 mechanical fix."
8. **Duplicate lens-API exposition** (§5) — decide whether
   `guide/navigating-nested-data.md`'s constructor subsection should
   forward-reference `guide/lenses.md` instead of restating the table,
   to reduce future drift risk. Low priority, cosmetic.
9. **`docs/development/gradual-typing-spec.md:679`** still shows the
   stale `eu --type-check` invocation trio. Left untouched in phase 1
   as a historical design document (same treatment as
   `appendices/migration.md`), but if the owner wants historical specs
   corrected too rather than left as a snapshot of design-time intent,
   this is a trivial follow-up.

---

## 7. Files changed in the mechanical-fix commit

```
CHANGELOG.md
docs/SUMMARY.md
docs/appendices/cheat-sheet.md
docs/architecture.md
docs/development/type-alias-shorthand-spec.md
docs/development/type-system-evolution.md
docs/guide/advanced-topics.md
docs/guide/command-line.md
docs/guide/date-time-random.md
docs/guide/expressions-and-pipelines.md
docs/guide/functions-and-combinators.md
docs/guide/io.md
docs/guide/lists-and-transformations.md
docs/guide/monads.md
docs/guide/navigating-nested-data.md
docs/guide/operators.md
docs/guide/state-monad.md
docs/guide/string-interpolation.md
docs/guide/type-checking.md
docs/guide/working-with-data.md
docs/llms-full.txt        (regenerated)
docs/llms.txt
docs/reference/cli.md
docs/reference/export-formats.md
docs/reference/operators-and-identifiers.md
docs/welcome/by-example.md
docs/welcome/index.md
docs/welcome/quick-start.md
tests/harness/174_sv1_typedata.eu
tests/harness/175_sv2_to_spec.eu
tests/harness/177_sv_optional_fields_to_data.eu
tests/harness/178_sv_optional_fields_to_spec.eu
tests/harness/182_typedata_alias_resolution.eu
tests/harness/183_widen_type_def_literals.eu
```

`docs/reference/prelude/*.md` was regenerated but produced no diff
(already accurate) and is therefore not in the changed-files list
above.
