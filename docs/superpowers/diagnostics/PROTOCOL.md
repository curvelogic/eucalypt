# Diagnostics Invariant Protocol

- **Status:** the checked-in correctness standard for eucalypt's error
  diagnostics (bead eu-1tkk.7; design spec
  `docs/superpowers/specs/2026-07-21-diagnostics-overhaul-design.md`).
- **Scope:** every diagnostic `eu` produces for a user program — via
  `--error-format json`, the human-rendered form, or `eu error <CODE>` — is
  expected to satisfy the five invariants below. The corpus in
  `tests/diagnostics/corpus/` (see `tests/diagnostics/README.md`) is the
  living evidence base; `tests/diagnostics_invariants.rs` is the objective
  gate that checks it on every `cargo test` run.

This document is operational: it states the five invariants exactly as the
gate enforces them, explains how the corpus and its `xfail` ratchet work, and
states the one rule that keeps the gate tightening rather than loosening.

---

## 1. The five invariants

Every diagnostic the gate checks is run through `eu --error-format json` and
parsed as a JSON object with a `primary` location and a `trace` (an array of
frames, each tagged with a `kind`). Against a fixture's declared mutated
region (`region_start_line`..`region_end_line` in its `.meta.toml` sidecar),
the diagnostic must satisfy:

- **(i) Primary location is in a user file.** `primary.in_user_file` is
  `true` — never the prelude, never a synthetic location.
- **(ii) No panic or abort.** The process must not panic, and must exit with
  a recognisable status code. A panic makes every other invariant
  meaningless, so it is checked first and short-circuits the rest.
- **(iii) The primary line falls inside the fixture's declared mutated
  region.** `region_start_line ≤ primary.line ≤ region_end_line`, inclusive.
  This is what proves the diagnostic points at the actual mistake, not just
  *some* user line.
- **(iv) The trace is user-anchored.** The trace contains at least one frame
  with `kind == "user"`, **or** the trace is empty **and** the primary is a
  user location. This is a strict, independent check: a non-empty trace with
  no user frame is a violation even when the primary passes (i). The
  invariant is deliberately **not** a fallback on (i) — a user-anchored
  primary alone does not satisfy (iv); the trace itself must be inspected on
  its own terms, because primary and trace are produced by different parts
  of the diagnostic pipeline and can diverge.
- **(v) Trace length is within budget.** `len(trace) ≤ 12` (`TRACE_BUDGET` in
  `tests/diagnostics_invariants.rs`) — a diagnostic that dumps dozens of
  prelude frames is not actionable, even if every frame it contains is
  technically accurate.

These five are copied verbatim from the enforcement logic in
`tests/diagnostics_invariants.rs::violations()` — that function is the
source of truth; this document explains it, not the other way round.

---

## 2. How the corpus and the `xfail` ratchet work

Each fixture in `tests/diagnostics/corpus/` is a **valid eucalypt program
with exactly one injected mistake** (an `.eu` file) plus a `.meta.toml`
sidecar recording the mutated region, a description, and — where relevant —
an `xfail` marker.

- **A fixture without `xfail`** is a **live guard**: the gate enforces all
  five invariants on it right now, on every `cargo test`. Any regression that
  breaks one of the five invariants for that fixture fails the build
  immediately.
- **A fixture with `xfail = true`** documents a *currently known* violation —
  a real bug the diagnostics overhaul is working through, named by an
  `xfail_reason` citing the responsible bead (for example
  `tests/diagnostics/corpus/hof_bad_arg.meta.toml`, which cites
  `eu-1tkk.7.8/.11` for a primary that currently lands in `[prelude]`
  instead of the user's call site). The gate tolerates the fixture's current
  violations but does not ignore the fixture — it still runs it every time.
- **When a bugfix lands and an `xfail` fixture starts satisfying all five
  invariants**, the gate reports it as an **unexpected pass** — a distinct,
  named failure mode (`corpus_satisfies_invariants` prints the fixture name
  under "these fixtures now PASS — remove their `xfail` marker to lock the
  gain"). This is deliberate: an unexpected pass is not silently accepted,
  because a silent xfail-but-passing fixture is a guard that has quietly
  stopped guarding anything. The fix is to remove the `xfail` marker in the
  same PR as the underlying bugfix, which converts the fixture into a live
  guard and permanently locks in the improvement.

See `tests/diagnostics/README.md` for the mechanics of adding a new fixture.

---

## 3. The rule

The gate is a **ratchet**: it may only tighten.

- A PR **may add fixtures** to the corpus (new provocations, new coverage).
- A PR **may remove an `xfail` marker** once the underlying bug is fixed and
  the fixture genuinely passes all five invariants — this locks the gain in
  as a live guard.
- A PR **may not add `xfail` to an existing live guard**, and **may not
  weaken an invariant or lower the trace budget**, without explicit owner
  sign-off. If a change would otherwise need to do either of those things to
  pass CI, that is a signal the change itself needs rework, not the gate.

---

## 4. CI

The gate lives in `tests/diagnostics_invariants.rs`, an ordinary Cargo
integration test — no separate CI job or registration is needed. The `Test
Suite` job (`test`, `.github/workflows/build-rust.yaml`) runs a plain,
unfiltered `cargo test` on every push and pull request, which picks up
`tests/diagnostics_invariants.rs` and `tests/diagnostics_json_emission.rs`
automatically, corpus and all, since `tests/diagnostics/corpus/` is
committed to the repository.

---

## 5. Further reading

- Design spec:
  `docs/superpowers/specs/2026-07-21-diagnostics-overhaul-design.md` — the
  problem statement, evidence, and design behind this overhaul.
- Corpus guide: `tests/diagnostics/README.md` — how to add a fixture.
- Gate implementation: `tests/diagnostics_invariants.rs`.
- JSON emitter tests: `tests/diagnostics_json_emission.rs`.
