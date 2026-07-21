# Diagnostics Phase 0 — Substrate & Harness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stand up the measurable, regression-proof foundation for eucalypt's diagnostics overhaul: a structured/JSON diagnostic representation, a provocation corpus of near-miss programs, and an objective invariant gate that runs in CI and can only tighten.

**Architecture:** Mirror the checked-in `engine-ab` measurement standard (a corpus + a runner + a gate). The corpus is versioned `.eu` fixtures with metadata sidecars declaring the injected mutation and its region. `eu --error-format json` is made to emit a faithful structured diagnostic (level/code/spans/labels/trace). A cargo integration test consumes that JSON and asserts five objective invariants per fixture, failing the build on violation. Error codes are added to the diagnostic model with an `eu explain` surface.

**Tech Stack:** Rust (existing crate), `serde`/`serde_json` (already deps), `codespan-reporting` 0.11 (existing renderer), cargo integration tests (`env!("CARGO_BIN_EXE_eu")`), eucalypt `.eu` fixtures.

**Design spec:** `docs/superpowers/specs/2026-07-21-diagnostics-overhaul-design.md` (owner-approved 2026-07-21). **Epic:** `eu-1tkk.7`. **Beads covered:** `eu-1tkk.7.2` (corpus), `eu-1tkk.7.1` (structured model), `eu-67v5` (error codes), `eu-1tkk.7.3` (invariant gate). **Deferred to a follow-on plan (by design):** `eu-1tkk.7.4` (golden snapshots + `--bless`), `eu-1tkk.7.5` (AI-as-fixer), `eu-1tkk.7.6` (AI-as-critic).

## How to read this plan

- **Infrastructure tasks (1, 5, 6, 7)** are new files — code is given in full.
- **Internals-refactor tasks (2, 3, 4)** modify existing compiler code that this plan's author mapped but did not read line-by-line. Each such task **begins with a mandatory read step** naming the exact files and the signatures to confirm, then specifies the change, the test specimen, and the expected invariant. Do **not** paste the change descriptions as literal code without first confirming the surrounding types — producing plausible-but-wrong diagnostics is the exact defect this epic exists to kill.
- The **architecture map** the tasks reference (file:line anchors for the diagnostic path) is reproduced in Appendix A. Treat its line numbers as *last-known* — confirm by reading, since master moves.

## Global Constraints

Copied verbatim from CLAUDE.md / the spec — every task's requirements include these:

- **UK English** spelling throughout code, comments, docs (optimisation, colour, behaviour).
- **`cargo fmt --all`** before every commit.
- **No clippy warnings:** `cargo clippy --all-targets -- -D warnings` must pass (`--all-targets`, not `--lib`).
- **Wrap every `eu` invocation** in `timeout` and pass `--heap-limit-mib`: `timeout 60 ./target/release/eu --heap-limit-mib 2048 ...` (harness runs may use a smaller limit; never unbounded in a loop).
- **Panics are critical** — never deferred. The invariant gate treats any panic as a hard failure (invariant ii). If a corpus fixture provokes a panic, that is a P1 bug to file and fix, not to suppress.
- **Every regression test is fault-injection verified:** break the code under test, confirm the test FAILs, restore, confirm it PASSes — and say so in the PR.
- **No direct pushes to master:** branch + PR for every task deliverable; the gatekeeper (Wicket) merges. This diagnostic work does not touch GC/unsafe/blob/engine-defaults/release machinery, so the recorded-review rule does not gate it — but CI must be green.
- **Beads:** claim the bead you are working (`bd update <id> --claim`), update status, do not batch-close. Only close a spec-linked bead against the spec section it satisfies.
- **Positive guidance only** in any docs authored: positive advice + doc links + good examples, never anti-pattern enumerations.

---

## File Structure

**New files:**
- `tests/diagnostics/corpus/*.eu` — provocation fixtures (valid-program-with-one-injected-mutation).
- `tests/diagnostics/corpus/*.meta.toml` — sidecar per fixture: mutation kind, mutated region, expected error class.
- `tests/diagnostics/README.md` — the corpus format + how to add a fixture (positive guidance).
- `tests/diagnostics_invariants.rs` — the cargo integration test: the objective invariant gate.
- `src/common/diagnostic_json.rs` — serde types for the structured JSON diagnostic (the shared schema).
- `docs/reference/error-codes.md` — the error-code catalogue (seeded; grows per code).

**Modified files (internals — read before touching):**
- `src/driver/eval.rs` — `error_to_json` (~:714) made faithful; wire `--error-format json` through the same location-selection logic as the human path.
- `src/eval/error.rs` — `ExecutionError` gets a `code()` method; `to_diagnostic` (~:833) sets `Diagnostic::code`.
- `src/common/sourcemap.rs` — expose a structured accessor (file/line/col/is-user-file + frame classification) for the JSON emitter to reuse, rather than re-deriving location.
- `src/bin/eu.rs` / `src/driver/options.rs` — add the `eu explain <code>` subcommand surface.
- `Cargo.toml` / `xtask` — none required for Phase 0 critical path (integration test needs no new deps).

---

## Task 1: Provocation corpus format + starter fixtures (bead `eu-1tkk.7.2`)

**Files:**
- Create: `tests/diagnostics/corpus/` (fixtures + sidecars, listed below)
- Create: `tests/diagnostics/README.md`

**Interfaces:**
- Produces: a corpus directory whose every entry is a pair `<name>.eu` + `<name>.meta.toml`. The sidecar schema (consumed by Task 5) is:

```toml
# tests/diagnostics/corpus/<name>.meta.toml
mutation   = "undefined-name"   # one of: undefined-name | swap-args | wrong-literal-type |
                                 #         broken-key | non-fn-call | catenation-trap |
                                 #         hof-bad-arg | empty-guard | metadata-span
description = "one line: what the user did wrong"
# The region the diagnostic's PRIMARY span should fall inside (1-indexed, inclusive).
# The whole point of invariant (iii): the error must point HERE, at user code.
region_start_line = 4
region_end_line   = 4
expected_class = "type-mismatch" # free-form class label, asserted as a substring of the message
# If this fixture is a KNOWN-CURRENTLY-FAILING case (documents a bug the epic will fix),
# mark it so the gate xfails it instead of hard-failing. Remove the marker when fixed.
xfail = true
xfail_reason = "eu-1tkk.7.8: error lands in [prelude], not user region"
```

The `xfail` mechanism is essential: the corpus documents *today's* failures as executable expectations that flip to PASS as Phases 1–3 land. A fixture without `xfail` is a live invariant the gate enforces now.

- [ ] **Step 1: Create the corpus directory and README**

Create `tests/diagnostics/README.md`:

```markdown
# Diagnostics provocation corpus

Each entry is a **valid eucalypt program with exactly one injected mistake**, plus a
`.meta.toml` sidecar describing the mistake and where the diagnostic should point.

The corpus feeds `tests/diagnostics_invariants.rs`, which runs every fixture through
`eu --error-format json` and asserts the five objective invariants (see that file's
header). It is the ratchet: adding a fixture, or removing an `xfail`, tightens the gate.

## Adding a fixture

1. Write `<name>.eu` — a short program with one clear mistake. Read
   `docs/reference/agent-reference.md` and `docs/appendices/syntax-gotchas.md` first so
   the mistake is the one you intend.
2. Run `timeout 60 ./target/release/eu --error-format json --heap-limit-mib 2048 <name>.eu`
   and read the JSON. Decide the region the primary span *should* fall in.
3. Write `<name>.meta.toml` (schema in the header of `tests/diagnostics_invariants.rs`).
4. If the diagnostic is already correct, leave `xfail` out — the gate now enforces it.
   If it is currently wrong (a bug this epic will fix), set `xfail = true` with a
   `xfail_reason` naming the bead. When that bead lands, delete the marker and the gate
   turns the fixture into a permanent guard.
```

- [ ] **Step 2: Create the starter fixtures**

Create these nine fixtures, one per mutation class, drawn from the in-session evidence sweep. For each, write the `.eu` and its `.meta.toml`. Examples (write all nine):

`tests/diagnostics/corpus/hof_bad_arg.eu`:
```eucalypt
xs: [1, 2, 3]
result: xs map(_ + _)
```
`tests/diagnostics/corpus/hof_bad_arg.meta.toml`:
```toml
mutation = "hof-bad-arg"
description = "2-arg lambda passed to map, which expects a 1-arg function"
region_start_line = 2
region_end_line = 2
expected_class = "function"
xfail = true
xfail_reason = "eu-1tkk.7.8/.11: primary lands in [prelude]:1463 with zero user frames"
```

`tests/diagnostics/corpus/nth_out_of_range.eu`:
```eucalypt
xs: [1, 2, 3]
result: xs nth(10)
```
`tests/diagnostics/corpus/nth_out_of_range.meta.toml`:
```toml
mutation = "empty-guard"
description = "index past end of list"
region_start_line = 2
region_end_line = 2
expected_class = "range"
xfail = true
xfail_reason = "eu-1tkk.7.11: reports 'tail of empty list' at [prelude]:1329, wrong op, 15 prelude frames"
```

`tests/diagnostics/corpus/metadata_span.eu`:
```eucalypt
` "documentation for scale — metadata, not code"
scale(factor, xs): xs map(* factor)
doubled(xs): scale("2", xs)
main: [1, 2] doubled
```
`tests/diagnostics/corpus/metadata_span.meta.toml`:
```toml
mutation = "metadata-span"
description = "runtime type error in a declaration carrying doc metadata"
region_start_line = 2
region_end_line = 3
expected_class = "number"
xfail = true
xfail_reason = "eu-1tkk.7.7: label span begins at the backtick metadata line (line 1)"
```

`tests/diagnostics/corpus/undefined_name.eu`:
```eucalypt
a: 1
b: 2
total: a + b + undefined-thing
```
`tests/diagnostics/corpus/undefined_name.meta.toml`:
```toml
mutation = "undefined-name"
description = "reference to an undefined variable"
region_start_line = 3
region_end_line = 3
expected_class = "unresolved variable"
# No xfail: this case is already correct today (specimen 08). It is a live guard.
```

Add the remaining classes analogously, choosing which are live guards vs `xfail`:
`lookup_on_function.eu` (`add.x`; `non-fn-call`; xfail — `eu-m93j`),
`catenation_arith.eu` (`n: xs foldl(+, 0) + 1`; `catenation-trap`; xfail — wrong noun `block`),
`missing_key.eu` (`config.hostname` on a block with `host`; `broken-key`; **live guard** — already good, specimen 21),
`string_plus_number.eu` (`a: "3"` then `a + 1`; `wrong-literal-type`; **live guard** — already good, specimen 17).

- [ ] **Step 3: Verify each fixture actually provokes an error**

Run (after a `cargo build --release`):
```bash
for f in tests/diagnostics/corpus/*.eu; do
  echo "== $f =="
  timeout 60 ./target/release/eu --heap-limit-mib 2048 "$f"; echo "exit: $?"
done
```
Expected: every fixture exits non-zero (it provokes a diagnostic). If any exits 0, it is a *silent-acceptance* case (legitimate language semantics, not a diagnostic) — remove it from the corpus; it belongs to the separate loader/type-checker effort, not here.

- [ ] **Step 4: Commit**

```bash
git add tests/diagnostics/
git commit -m "test(diagnostics): provocation corpus format + starter fixtures (eu-1tkk.7.2)"
```

---

## Task 2: Structured JSON diagnostic schema (shared types) — part of bead `eu-1tkk.7.1`

**Files:**
- Create: `src/common/diagnostic_json.rs`
- Modify: `src/common/mod.rs` (add `pub mod diagnostic_json;`)
- Test: unit tests inside `src/common/diagnostic_json.rs`

**Interfaces:**
- Produces (consumed by Tasks 3, 5): the serde schema below. This is the contract the JSON emitter fills and the invariant gate reads.

```rust
// src/common/diagnostic_json.rs
use serde::{Deserialize, Serialize};

/// A source position resolved for user-facing output. `in_user_file` is the
/// single most load-bearing field for the invariant gate: it distinguishes a
/// location in code the user wrote from one in the prelude/synthetic machinery.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct JsonPos {
    /// Display path, e.g. "config.eu" or "[prelude]". `None` for a locationless (synthetic) Smid.
    pub file: Option<String>,
    pub line: Option<u32>,
    pub column: Option<u32>,
    /// True iff `file` resolves to a user (non-resource) source. False for prelude/synthetic.
    pub in_user_file: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum FrameKind {
    User,        // a frame in user source
    Boundary,    // a named library combinator the user invoked (nth, head, lookup, +)
    Transparent, // library plumbing (map/fold internals); normally curated out
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct JsonFrame {
    pub name: Option<String>, // user-facing frame name (e.g. "nth", "map", or a binding name)
    pub pos: JsonPos,
    pub kind: FrameKind,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct JsonLabel {
    pub pos: JsonPos,
    pub message: String,
    pub primary: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum JsonLevel {
    Error,
    Warning,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct JsonDiagnostic {
    pub level: JsonLevel,
    /// Stable error code, e.g. "EU-EVAL-TYPE" (Task 4). `None` until a code is assigned.
    pub code: Option<String>,
    pub message: String,
    /// The primary label's position (duplicated out for the gate's convenience).
    pub primary: JsonPos,
    pub labels: Vec<JsonLabel>,
    pub notes: Vec<String>,
    /// Curated, outermost-first. Empty is legal (some errors have no trace).
    pub trace: Vec<JsonFrame>,
}
```

- [ ] **Step 1: Write the failing test**

In `src/common/diagnostic_json.rs`, below the types:
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrips_through_json() {
        let d = JsonDiagnostic {
            level: JsonLevel::Error,
            code: Some("EU-EVAL-TYPE".into()),
            message: "type mismatch: expected number, found string".into(),
            primary: JsonPos { file: Some("x.eu".into()), line: Some(3), column: Some(9), in_user_file: true },
            labels: vec![JsonLabel {
                pos: JsonPos { file: Some("x.eu".into()), line: Some(3), column: Some(9), in_user_file: true },
                message: "here".into(),
                primary: true,
            }],
            notes: vec!["use 'num' to convert".into()],
            trace: vec![JsonFrame {
                name: Some("nth".into()),
                pos: JsonPos { file: Some("[prelude]".into()), line: Some(1380), column: Some(22), in_user_file: false },
                kind: FrameKind::Boundary,
            }],
        };
        let s = serde_json::to_string(&d).unwrap();
        let back: JsonDiagnostic = serde_json::from_str(&s).unwrap();
        assert_eq!(d, back);
        assert!(s.contains("\"in-user-file\"") || s.contains("\"in_user_file\""));
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p eucalypt --lib diagnostic_json::tests::roundtrips_through_json`
Expected: FAIL — module does not exist yet.

- [ ] **Step 3: Write the schema**

Add the types above to `src/common/diagnostic_json.rs`, and `pub mod diagnostic_json;` to `src/common/mod.rs`. (Confirm the crate name for the `-p` flag by reading `Cargo.toml`'s `[package] name` — it may be `eucalypt` or `eu`.)

- [ ] **Step 4: Run the test to verify it passes**

Run: `cargo test --lib diagnostic_json`
Expected: PASS. Fix the `in_user_file` serde name assertion to match your chosen `rename` (prefer `#[serde(rename_all = "snake_case")]` on the struct or leave default — just make the test assert the real key).

- [ ] **Step 5: Commit**

```bash
cargo fmt --all
git add src/common/diagnostic_json.rs src/common/mod.rs
git commit -m "feat(diagnostics): structured JSON diagnostic schema (eu-1tkk.7.1)"
```

---

## Task 3: Faithful `eu --error-format json` emission (bead `eu-1tkk.7.1`)

> **READ-FIRST TASK.** Before writing any code, read: `src/driver/eval.rs` around `error_to_json` (Appendix A anchor ~:714) and the human path `diagnose_to_stderr` (~:695) and the note-appending block (~:649-664); `src/eval/error.rs` `ExecutionError::to_diagnostic` (~:833-978, the location-selection precedence); `src/common/sourcemap.rs` `first_user_source_smid` (~:285), `is_user_file` (~:261), `source_info` (~:173). Confirm: how the human path selects the primary Smid, how it builds the stack-trace note, and why `error_to_json` currently diverges (it resolves only `source_info(error)` with no trace fallback and no user-file preference — Appendix A, divergence #14).

**Files:**
- Modify: `src/driver/eval.rs` (`error_to_json` and its callers)
- Modify (if needed): `src/common/sourcemap.rs` (expose a reusable resolver returning `JsonPos`/frame classification so JSON and human paths share it)
- Test: `tests/diagnostics_json_emission.rs` (new integration test)

**Interfaces:**
- Consumes: `JsonDiagnostic` (Task 2), the existing `ExecutionError::to_diagnostic` location logic.
- Produces: `eu --error-format json <file>` emits exactly one `JsonDiagnostic` as JSON to stdout (or the existing error stream — confirm which the current `--error-format json` uses) whose `primary`/`trace` are derived from the **same** selection logic as the human renderer. The invariant gate (Task 5) depends on this parity.

- [ ] **Step 1: Write the failing integration test**

Create `tests/diagnostics_json_emission.rs`:
```rust
use std::process::Command;

fn run_json(src: &str) -> serde_json::Value {
    let dir = std::env::temp_dir().join(format!("eu-diag-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("case.eu");
    std::fs::write(&path, src).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(["--error-format", "json", "--heap-limit-mib", "2048"])
        .arg(&path)
        .output()
        .expect("run eu");
    // JSON may be on stdout or stderr depending on the current wiring — read the one that parses.
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    serde_json::from_str(stdout.trim())
        .or_else(|_| serde_json::from_str(stderr.trim()))
        .unwrap_or_else(|e| panic!("no JSON diagnostic parsed: {e}\nstdout={stdout}\nstderr={stderr}"))
}

#[test]
fn json_error_carries_user_location_for_direct_type_error() {
    // Direct user-site type error: JSON must locate it in the user file, not lose the location.
    let v = run_json("a: \"3\"\nresult: a + 1\n");
    let primary = &v["primary"];
    assert_eq!(primary["in_user_file"], serde_json::json!(true),
        "primary must be in the user file, got {v}");
    assert_eq!(primary["line"], serde_json::json!(2));
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --test diagnostics_json_emission json_error_carries_user_location`
Expected: FAIL — either no JSON parses (format not wired) or `in_user_file`/`line` absent/wrong (the divergence). Record the actual failure in the PR as the baseline.

- [ ] **Step 3: Implement faithful emission**

In `error_to_json`, replace the lossy `source_info(error)`-only resolution with the same primary-Smid selection the human path uses (reuse `to_diagnostic`'s result, or factor its selection into a shared function returning the chosen `Smid` + labels + notes, then map each chosen `Smid` to `JsonPos` via a `sourcemap` resolver that also sets `in_user_file`). Populate `trace` from the same stack-trace the human note uses, mapping each frame to `JsonFrame` (kind classification is refined in Phase 2 — for Phase 0 set `User` when `in_user_file`, else `Transparent`). Emit a `JsonDiagnostic`. **Do not** invent a second location path — the whole bug is that two paths disagree; converge them.

- [ ] **Step 4: Run the test to verify it passes**

Run: `cargo test --test diagnostics_json_emission`
Expected: PASS.

- [ ] **Step 5: Fault-injection verify**

Temporarily force `error_to_json` to emit `in_user_file: false` unconditionally; re-run — the test must FAIL. Restore; it must PASS. Note this in the PR.

- [ ] **Step 6: Commit**

```bash
cargo fmt --all && cargo clippy --all-targets -- -D warnings
git add src/driver/eval.rs src/common/sourcemap.rs tests/diagnostics_json_emission.rs
git commit -m "fix(diagnostics): --error-format json shares the human location logic (eu-1tkk.7.1)"
```

---

## Task 4: Error codes + `eu explain` (bead `eu-67v5`)

> **READ-FIRST TASK.** Read: `src/eval/error.rs` (the `ExecutionError` variants and `to_diagnostic`), `src/core/error.rs` (`CoreError`), `src/syntax/error.rs` (`ParserError`), `src/core/typecheck/error.rs` (`TypeWarning`), and `src/driver/error.rs` (the `EucalyptError` fan-in). Confirm there is no existing `Diagnostic::code` usage (Appendix A: `with_code` returns nothing). Decide the code namespace with the owner if unsure — the spec §9 open question offered "namespaced (parse/name/type/eval)"; this plan proceeds with namespaced codes `EU-<AREA>-<SLUG>` (e.g. `EU-EVAL-TYPE`, `EU-NAME-UNRESOLVED`, `EU-PARSE-UNTERMINATED`), which read better in `eu explain` and group naturally. Confirm before committing the scheme.

**Files:**
- Modify: `src/eval/error.rs` (add `fn code(&self) -> Option<&'static str>`; set `diag = diag.with_code(...)` in `to_diagnostic`)
- Modify: `src/core/error.rs`, `src/syntax/error.rs`, `src/core/typecheck/error.rs` (codes for their variants)
- Create: `docs/reference/error-codes.md` (the catalogue)
- Modify: `src/bin/eu.rs` + `src/driver/options.rs` (add `eu explain <code>`)
- Test: extend `tests/diagnostics_json_emission.rs`

- [ ] **Step 1: Write the failing test**

Append to `tests/diagnostics_json_emission.rs`:
```rust
#[test]
fn type_error_carries_a_stable_code() {
    let v = run_json("a: \"3\"\nresult: a + 1\n");
    assert_eq!(v["code"], serde_json::json!("EU-EVAL-TYPE"),
        "type mismatch must carry the stable EU-EVAL-TYPE code, got {v}");
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --test diagnostics_json_emission type_error_carries_a_stable_code`
Expected: FAIL — `code` is null.

- [ ] **Step 3: Assign the code**

Add `ExecutionError::code()` returning `Some("EU-EVAL-TYPE")` for the type-mismatch variant(s) (map each variant to a code; start with the type-mismatch family this test exercises, then fill the rest). Set `diag.with_code(...)` in `to_diagnostic` so both the human header and the JSON `code` are populated (confirm codespan renders the code in the header — it does when `Diagnostic::code` is set). Seed `docs/reference/error-codes.md` with the `EU-EVAL-TYPE` entry: what it means, a minimal example, how to fix — positive guidance only.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test --test diagnostics_json_emission`
Expected: PASS.

- [ ] **Step 5: Add `eu explain`**

Wire `eu explain EU-EVAL-TYPE` to print the catalogue entry (read the code→doc mapping from a compiled-in table or the markdown). Add a smoke test:
```rust
#[test]
fn explain_prints_catalogue_entry() {
    let out = Command::new(env!("CARGO_BIN_EXE_eu")).args(["explain", "EU-EVAL-TYPE"]).output().unwrap();
    assert!(out.status.success());
    assert!(String::from_utf8_lossy(&out.stdout).to_lowercase().contains("type"));
}
```

- [ ] **Step 6: Commit**

```bash
cargo fmt --all && cargo clippy --all-targets -- -D warnings
git add src/eval/error.rs src/core/error.rs src/syntax/error.rs src/core/typecheck/error.rs docs/reference/error-codes.md src/bin/eu.rs src/driver/options.rs tests/diagnostics_json_emission.rs
git commit -m "feat(diagnostics): stable error codes + eu explain (eu-67v5)"
```

---

## Task 5: The objective invariant gate (bead `eu-1tkk.7.3`)

**Files:**
- Create: `tests/diagnostics_invariants.rs`
- Test: this file *is* the test.

**Interfaces:**
- Consumes: Task 1 corpus (`*.eu` + `*.meta.toml`), Task 3's faithful `--error-format json`, Task 2's `JsonDiagnostic` shape (parsed as `serde_json::Value` to avoid a lib dependency from the test).

The five invariants (from spec §5.2), each asserted per non-`xfail` fixture:

- (i) `primary.in_user_file == true` — never prelude, never synthetic.
- (ii) no panic — the process did not abort; stderr contains no `panicked`/`RUST_BACKTRACE` marker; exit code is the ordinary error code, not a signal.
- (iii) `region_start_line <= primary.line <= region_end_line` — points inside the injected mistake.
- (iv) `trace` contains ≥1 frame with `kind == "user"` (or is empty *only if* the primary itself is user — some direct errors legitimately have no trace).
- (v) `trace.len() <= TRACE_BUDGET` (start at 12).

- [ ] **Step 1: Write the gate**

Create `tests/diagnostics_invariants.rs`:
```rust
//! Objective invariant gate for eucalypt diagnostics (spec 2026-07-21 §5.2).
//! Runs every corpus fixture through `eu --error-format json` and asserts the five
//! invariants. Fixtures marked `xfail = true` in their sidecar are expected to VIOLATE
//! (documenting a known bug); when a bead lands and flips one to passing, remove the
//! marker so the gate guards it forever.
use std::process::Command;

const TRACE_BUDGET: usize = 12;

struct Meta {
    region: (u32, u32),
    xfail: bool,
}

fn parse_meta(toml_src: &str) -> Meta {
    // Minimal hand-parse to avoid a toml dev-dep; the sidecar format is fixed and simple.
    let mut start = 0u32; let mut end = 0u32; let mut xfail = false;
    for line in toml_src.lines() {
        let l = line.trim();
        if let Some(v) = l.strip_prefix("region_start_line") { start = v.trim_start_matches([' ','=']).trim().parse().unwrap(); }
        else if let Some(v) = l.strip_prefix("region_end_line") { end = v.trim_start_matches([' ','=']).trim().parse().unwrap(); }
        else if let Some(v) = l.strip_prefix("xfail ") { xfail = v.contains("true"); }
        else if l == "xfail = true" { xfail = true; }
    }
    Meta { region: (start, end), xfail }
}

fn run(path: &std::path::Path) -> (serde_json::Value, String, Option<i32>) {
    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(["--error-format", "json", "--heap-limit-mib", "2048"])
        .arg(path).output().expect("run eu");
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
    let v = serde_json::from_str(stdout.trim())
        .or_else(|_| serde_json::from_str(stderr.trim()))
        .unwrap_or(serde_json::Value::Null);
    (v, format!("{stdout}{stderr}"), out.status.code())
}

fn violations(v: &serde_json::Value, all_output: &str, code: Option<i32>, m: &Meta) -> Vec<String> {
    let mut errs = vec![];
    // (ii) no panic — checked first; a panic makes the rest meaningless.
    if all_output.contains("panicked") || code.is_none() {
        errs.push(format!("(ii) panic/abort: code={code:?}"));
        return errs;
    }
    if v.is_null() { errs.push("no JSON diagnostic parsed".into()); return errs; }
    let primary = &v["primary"];
    if primary["in_user_file"] != serde_json::json!(true) {
        errs.push(format!("(i) primary not in user file: {primary}"));
    }
    if let Some(line) = primary["line"].as_u64() {
        let l = line as u32;
        if l < m.region.0 || l > m.region.1 {
            errs.push(format!("(iii) primary line {l} outside region {:?}", m.region));
        }
    } else {
        errs.push("(iii) primary has no line".into());
    }
    let trace = v["trace"].as_array().cloned().unwrap_or_default();
    let has_user = trace.iter().any(|f| f["kind"] == serde_json::json!("user"));
    let primary_is_user = primary["in_user_file"] == serde_json::json!(true);
    if !trace.is_empty() && !has_user && !primary_is_user {
        errs.push("(iv) trace present but no user frame".into());
    }
    if trace.len() > TRACE_BUDGET {
        errs.push(format!("(v) trace length {} > budget {TRACE_BUDGET}", trace.len()));
    }
    errs
}

#[test]
fn corpus_satisfies_invariants() {
    let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/diagnostics/corpus");
    let mut hard_failures = vec![];
    let mut unexpected_pass = vec![];
    for entry in std::fs::read_dir(dir).expect("corpus dir") {
        let p = entry.unwrap().path();
        if p.extension().and_then(|e| e.to_str()) != Some("eu") { continue; }
        let meta_path = p.with_extension("meta.toml");
        let m = parse_meta(&std::fs::read_to_string(&meta_path).expect("meta"));
        let (v, out, code) = run(&p);
        let errs = violations(&v, &out, code, &m);
        let name = p.file_name().unwrap().to_string_lossy().to_string();
        match (m.xfail, errs.is_empty()) {
            (false, false) => hard_failures.push(format!("{name}: {errs:?}")),
            (true, true)   => unexpected_pass.push(name),  // fixed! remove the xfail marker.
            _ => {}
        }
    }
    assert!(hard_failures.is_empty(), "invariant violations:\n{}", hard_failures.join("\n"));
    assert!(unexpected_pass.is_empty(),
        "these fixtures now PASS — remove their `xfail` marker to lock the gain:\n{}",
        unexpected_pass.join("\n"));
}
```

- [ ] **Step 2: Run the gate**

Run: `cargo test --test diagnostics_invariants`
Expected: PASS — the live-guard fixtures satisfy the invariants; the `xfail` fixtures violate them as expected. If a live-guard fixture fails, either the fixture region is wrong or you found a real bug — investigate, do not weaken the invariant.

- [ ] **Step 3: Fault-injection verify the gate bites**

Temporarily flip one live-guard fixture's `.meta.toml` region to a wrong line; re-run — `corpus_satisfies_invariants` must FAIL with an `(iii)` violation. Restore; it must PASS. Note in the PR.

- [ ] **Step 4: Commit**

```bash
cargo fmt --all && cargo clippy --all-targets -- -D warnings
git add tests/diagnostics_invariants.rs
git commit -m "test(diagnostics): objective invariant gate over the corpus (eu-1tkk.7.3)"
```

---

## Task 6: Wire the gate into CI + document the standard

**Files:**
- Modify: `.github/workflows/*.yml` (the CI workflow that runs `cargo test`)
- Create: `docs/superpowers/diagnostics/PROTOCOL.md` (mirror `engine-ab/PROTOCOL.md`)

- [ ] **Step 1: Confirm the gate runs in CI**

Read the workflow that runs the test suite. `tests/diagnostics_invariants.rs` and `tests/diagnostics_json_emission.rs` are ordinary integration tests, so a plain `cargo test` job already runs them. Confirm the corpus dir is present in the checkout (it is — it's committed). If the suite is sharded/filtered, ensure the `--test diagnostics_invariants` target is included. No separate job is required for Phase 0.

- [ ] **Step 2: Document the standard**

Create `docs/superpowers/diagnostics/PROTOCOL.md`: what a diagnostic must satisfy (the five invariants), how the corpus + `xfail` ratchet works, and the rule — *a PR may add fixtures or remove `xfail` markers; it may not add an `xfail` to an existing live guard without owner sign-off.* Positive guidance only. Link the design spec.

- [ ] **Step 3: Commit**

```bash
git add docs/superpowers/diagnostics/PROTOCOL.md .github/workflows/
git commit -m "ci(diagnostics): run the invariant gate + document the standard (eu-1tkk.7.3)"
```

---

## Task 7: Close-out — bead status + PR

- [ ] **Step 1:** Open the PR(s) for the above (one per bead deliverable is cleanest: `.2`, `.1`, `eu-67v5`, `.3`). Each PR body states the fault-injection verification performed.
- [ ] **Step 2:** `bd update eu-1tkk.7.2 --status in_review` (etc.) — do **not** close until merged and verified against the spec section.
- [ ] **Step 3:** After merge, `bd close <id>` with a reason citing the spec section satisfied (e.g. "eu-1tkk.7.3: spec §5.2 invariant gate live in CI; N live-guard fixtures, M xfail documenting Phase 1/2 bugs"). Then `bd dolt commit && bd dolt push`.

---

## Appendix A — Diagnostic-path architecture map (last-known anchors)

Confirm by reading; master moves. Source: in-session architecture map, 2026-07-21.

- **Diagnostic library:** `codespan-reporting` 0.11 + `codespan` 0.11 (`Cargo.toml:34-35`). Rendering: `driver/eval.rs:695-710` (runtime), `driver/source.rs:1029-1046` (load), `bin/eu.rs:188-196` / `driver/check.rs:170-192` (check/warnings).
- **JSON path (the bug):** `driver/eval.rs:714+` `error_to_json` resolves location only via `source_map.source_info(error)` — **no** trace fallback, **no** user-file preference. Diverges from the human path for most runtime errors.
- **Smid / SourceMap:** `common/sourcemap.rs`. `Smid(Option<NonZeroU32>)`; `Smid::get()` **panics** on invalid (`:64`). `SourceInfo { file, span, annotation }` (`:93`). `add`/`add_annotated`/`add_synthetic` (`:120`/`:131`/`:142`). `mark_resource_file`/`is_user_file` (`:255`/`:261`). `first_source_smid` (`:270`), `first_user_source_smid` (`:285`). `format_trace` (`:305`), `compress_trace_cycles` (`:385`), `intrinsic_display_name` (`:439`).
- **ExecutionError:** `eval/error.rs`. `to_diagnostic` (`:833-978`) — location precedence: Compile short-circuit (`:837`); user-file own Smid; else `first_user_source_smid(env_trace)` **then** `(stack_trace)` (`:862-888`); `AssertionFailed` suppresses non-user primary (`:898-915`). Secondary labels from env_trace, ≤3 (`:917-978`). No `Diagnostic::code` set anywhere.
- **Traces:** env trace `machine/env.rs:752-768`; stack trace `machine/vm.rs:1363-1403`; `Traced` wrap unconditional (`vm.rs:2006/2056`, `bytecode/machine.rs:396`); `to_diagnostic` unwraps only one level (`:842-847`).
- **Smid minting:** desugar `core/desugar/desugarer.rs:394-407` (`new_smid` uses declaration span incl. leading metadata — the metadata-in-span bug). Intrinsics: `eval/stg/runtime.rs:270-275` `add_synthetic` (locationless). Cook synthesises App/Lookup Smids from sub-expressions (`cook/shunt.rs:205-227`).
- **Suggestions:** `levenshtein_distance`/`suggest_similar` (`eval/error.rs:211-262`), used only at `stg/block.rs:1310` (lookup fail).
- **Existing error tests:** `tests/harness/errors/*.eu` + `.eu.expect` (assert `exit:` + a stderr **substring** only — location/labels/trace unasserted; that gap is what this plan closes).
