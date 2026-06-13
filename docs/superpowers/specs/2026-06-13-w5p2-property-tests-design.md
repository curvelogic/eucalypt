# W5 Phase 2: Property Tests (proptest)

- **Date:** 2026-06-13
- **Status:** Draft
- **Bead:** eu-kgsi.7
- **Roadmap:** W5 — Conformance corpus, property tests & fuzzing (Phase 2)

---

## 1. Problem

The 300+ harness tests are example-based — they verify specific inputs
produce specific outputs. There are no property-based tests that verify
invariants across randomly generated inputs. Entire classes of bugs
(round-trip failures, subtyping violations, GC invariants) can hide
between the specific cases the harness covers.

## 2. Goal

A `proptest`-based test suite verifying structural invariants of the
parser, renderer, type system, and GC across randomly generated inputs.

## 3. Design

### 3.1 Properties

Seven property groups, ordered by priority:

**Group 1: Round-trip and idempotence**

1. **Render-parse round-trip**: for any generated core expression that
   renders to valid eucalypt source, re-parsing the rendered text
   produces a structurally equivalent expression.
2. **Render determinism**: rendering the same expression twice produces
   identical output bytes.
3. **`eu fmt` idempotence**: formatting an already-formatted file
   produces identical output (`fmt(fmt(x)) == fmt(x)`).

**Group 2: Type system invariants**

4. **Subtyping reflexivity**: for any generated type `T`,
   `is_subtype(T, T)` holds.
5. **Subtyping transitivity**: for generated types `A ≤ B` and `B ≤ C`,
   `A ≤ C` holds.
6. **Consistency symmetry**: for any generated types `T` and `U`,
   `is_consistent(T, U) == is_consistent(U, T)` (the §4.2 property).

**Group 3: GC invariants**

7. **GC mark completeness**: for generated heap structures, after a
   mark phase, all reachable objects are marked (this is what
   `EU_GC_VERIFY=1` checks, but as a property test over random
   allocation patterns).

### 3.2 Generators (`Arbitrary` implementations)

**`Type` generator** — must cover:
- Base types: `String`, `Number`, `Symbol`, `Null`, `Bool`, `Any`, `Never`
- `List(T)`, `NonEmpty(T)`, `Dict(T)`, `Vec`
- `Record(fields, open/closed)` with generated field names and types
- `Union(T, U)` and `Partial(T)`
- `LiteralString(s)` and `LiteralSymbol(s)`
- `Con(name)`, `App(T, U)` — higher-kinded
- `Forall(var, kind, body)`
- `Mu(var, body)` — recursive types
- Depth-limited to prevent blow-up (max depth ~5)

**`RcExpr` generator** — for round-trip tests:
- Literals (string, number, symbol, bool, null)
- Lists, blocks (with generated keys)
- Simple applications and let-bindings
- Depth-limited (max depth ~4)

**Heap structure generator** — for GC tests:
- Random allocation sequences (closures, env frames, cons cells)
- Random pointer graphs between allocated objects
- Parameterised by heap size and pointer density

### 3.3 Test infrastructure

Add `proptest` as a dev-dependency. Property tests live in
`tests/property_test.rs` (separate from the harness tests) or as
`#[cfg(test)]` modules in the relevant source files.

Configuration:
- Default: 256 cases per property (fast enough for CI).
- `PROPTEST_CASES=10000` env var for deeper runs.
- Regression files (`*.proptest-regressions`) checked in for
  reproducibility.

### 3.4 Integration with CI

Property tests run as part of `cargo test`. No separate CI job needed —
they are fast at the default case count. The GC property tests run under
`EU_GC_VERIFY=2` in the existing GC-verified CI job.

## 4. Implementation sketch

### Phase 1: Dependency and infrastructure
- Add `proptest` to `[dev-dependencies]`.
- Create `tests/property_test.rs`.
- Set up `ProptestConfig` with env-var-driven case count.

### Phase 2: Type generators
- `Arbitrary for Type` with depth limiting.
- Unit tests for the generator itself (ensure all variants reachable).

### Phase 3: Type system properties
- Subtyping reflexivity, transitivity.
- Consistency symmetry.

### Phase 4: Expression generators
- `Arbitrary for RcExpr` (literal-heavy, depth-limited).
- Render-to-string function for generated expressions.

### Phase 5: Round-trip properties
- Render → parse → compare.
- Render determinism.
- `eu fmt` idempotence.

### Phase 6: GC properties
- Heap structure generator.
- Mark completeness property.

## 5. Test plan

- All properties pass at 256 cases in CI.
- A 10,000-case run passes locally before merge.
- Regression files are checked in.
- Any property failure produces a minimal shrunk counterexample.

## 6. Success criteria

- Seven properties pass across the type, expression, and GC domains.
- At least one real bug is found by the property tests (historically
  likely for type system invariants).
- CI runs property tests on every push without meaningful slowdown.
