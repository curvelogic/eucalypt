# TY: Typing Default-On

- **Bead:** eu-1nxh
- **Pillar:** TY — Typing default-on
- **Release:** 0.11
- **Date:** 2026-06-25

---

## 1. Problem

The type checker is complete, cheap (`eu check` ≈ `eu eval`), and
quiet on real code (0 warnings across the AoC corpus; 56/60 harness
files clean). But it only runs when explicitly requested via
`--type-check` or `eu check` — most users never see its output. Its
value goes unbanked, and the forcing function that would surface and
close checker gaps (e.g. `1 + "hello"` is not yet caught) is absent.

## 2. Design

### 2.1 Phase 1: Warning inventory

Before flipping the default, triage all existing warnings across the
full corpus:

1. Run the checker across the harness suite, AoC corpus, and any
   conformance tests.
2. For each warning, classify:
   - **Intentional** — genuine type issue the user should see.
   - **False positive** — checker bug; fix the checker.
   - **Test-specific** — the test deliberately exercises an untyped
     pattern; add a suppression annotation or accept.
3. Fix all false positives. The bar: zero spurious warnings on the
   harness and AoC corpus after this phase.

### 2.2 Phase 2: Flip the default

The type checker runs on every `eu` invocation — evaluation, dump,
test, etc. This is not gated by any flag; it is unconditional.

**Changes to the flag surface:**

- **Remove `--type-check`** — no longer needed. The checker always
  runs. Existing scripts using `--type-check` are unaffected (the
  flag can be kept as a silent no-op during a deprecation period if
  needed).
- **Add `--suppress-type-warnings`** — suppresses warning output to
  stderr. The checker still runs; warnings are computed but not
  printed. This is the escape hatch for users who find warnings
  distracting.
- **`--strict` unchanged** — when combined with type checking (which
  is now always-on), promotes warnings to errors and aborts before
  evaluation with exit code 1. Intended for CI gates via
  `eu check --strict`.

**Warning output:**

- Warnings are emitted to stderr via `codespan_reporting`, as today.
- Warnings never affect stdout output or exit code (unless
  `--strict`).
- Warnings are always emitted regardless of whether stderr is a TTY
  — `--suppress-type-warnings` is the explicit suppression.

### 2.3 Pipeline integration

The checker is invoked from `src/bin/eu.rs` (currently lines
149–168) after cook + eliminate, before STG compilation. This
location is unchanged. The only change is removing the
`if opt.type_check()` guard — the checker always runs.

The prelude type cache (`src/driver/check.rs:40–71`) already handles
performance: the prelude is checked once per process and the result
is reused. No architectural changes needed.

### 2.4 Performance budget

Type checking must not add measurable latency to the config path.
The roadmap states `eu check` ≈ `eu eval` (verified); the checker
rides the front-end pipeline that is already paid. Confirm with
before/after timing on `eu -e 'true'` and on a representative
config file.

## 3. Scope

### In scope

- Warning inventory across harness + AoC + conformance.
- Fixing false positives in the checker.
- Making type checking unconditional (always runs).
- Removing `--type-check` flag (or keeping as silent no-op).
- Adding `--suppress-type-warnings` flag.
- Verifying no latency regression.

### Out of scope

- Fixing checker gaps (e.g. `1 + "hello"` not caught) — those
  become tracked issues that default-on surfaces. They are
  follow-up work, not blockers.
- Changes to the type system itself.
- Type-directed codegen (CG5) — that consumes type facts but is a
  separate pillar.
- Warning stability guarantees — the type system remains
  Experimental (§4.1 of ROADMAP.md); warning prose and coverage may
  change in any release.

## 4. Success Criteria

1. **Zero spurious warnings** on the harness and AoC corpus after
   the inventory phase.
2. **Type checking runs on every invocation** — verified by adding a
   type error to a test file and confirming the warning appears
   without `--type-check`.
3. **`--suppress-type-warnings` works** — same test, warning is
   suppressed.
4. **`--strict` works** — same test, exit code 1, evaluation does
   not run.
5. **No latency regression** — `eu -e 'true'` timing within noise
   of baseline (< 5% increase).
6. **Output identity** — rendered output byte-identical across the
   conformance corpus (type checking is advisory and erased).
7. **Checker gaps surfaced** — any new warnings discovered during
   the inventory are filed as tracked issues.

## 5. Testing

- Full harness run with default-on checking, confirming zero
  spurious warnings and byte-identical output.
- Targeted test for `--suppress-type-warnings` behaviour.
- Targeted test for `--strict` abort behaviour.
- Before/after timing comparison on `eu -e 'true'` and a
  representative config file.
- Existing `eu check` tests continue to pass.

## 6. Risks

- **Low:** the checker is already stable and quiet on real code.
  The main risk is undiscovered false positives, which the inventory
  phase addresses.
- **Low:** performance is already verified as negligible. The
  prelude cache ensures the cost is one-time per process.
- **Medium:** removing `--type-check` could break existing scripts.
  Mitigation: keep the flag as a silent no-op for one release cycle,
  then remove.
