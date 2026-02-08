# STG Case Optimisation Design

Date: 2026-02-03

## Overview

Four independent optimisations to STG case expression handling, grouped
under an STG Optimisation epic alongside the existing eu-bpe (inlining
into STG wrappers) bead.

The four case optimisations can be implemented in any order. Each is a
self-contained change that preserves semantics and is validated by
existing harness tests.

### Optimisations

1. **Redundant single-branch case elimination** (eu-knx, compile-time)
2. **Case-of-known-constructor folding** (new, compile-time)
3. **O(1) tag dispatch** (new, runtime)
4. **Skip empty env frames for 0-arity branches** (new, runtime)

### Success criteria

- All existing harness tests pass after each change
- No semantic changes to program output
- Measurable reduction in VM ticks, allocation count, or dispatch
  overhead for relevant workloads

---

## 1. Redundant Single-Branch Case Elimination (eu-knx)

### Problem

Pointless case statements are generated when pattern matching on a
constructor with only one branch handling that same constructor:

```
Case scrutinee
  Constructor(a, b, c) -> body(a, b, c)
```

This should be simplified to direct field access without case analysis.

### Location

New pass in `src/eval/stg/optimiser.rs`, or extension of the existing
`AllocationPruner`.

### Transformation

Replace the single-branch case with a `force` (evaluate scrutinee to
WHNF) followed by direct binding of the constructor's fields into the
body:

```
// Before:
Case scrutinee
  BoxedNumber(x) -> body(x)

// After:
force(scrutinee, body)
```

The `force` construct evaluates the scrutinee to WHNF, then the body
receives the fields as local bindings -- which is exactly what `force`
already does (a case with empty branches and a fallback that receives
the value).

For constructors with known arity, the fields are already bound by
position in the environment frame, so the body's local references
remain valid.

### When NOT to transform

- Multiple branches -- not a single-constructor case
- Fallback present alongside the branch -- suggests the scrutinee's
  type is not statically known
- Zero-arity constructors like `BoolTrue` or `ListNil` -- already
  trivial, but could still be simplified (case becomes `force` +
  discard)

### Implementation

Add a pass over `StgSyn` that pattern-matches `Case { branches,
fallback }` where `branches.len() == 1` and `fallback.is_none()`.
Replace with the equivalent `force` + rebinding. Run after the existing
`AllocationPruner` in the optimisation pipeline.

### Testing

- Unit test in `optimiser.rs`: construct a single-branch case, run the
  pass, verify it produces `force`
- All harness tests pass (correctness preservation)
- Boolean-heavy harness tests should show slight tick reduction

---

## 2. Case-of-Known-Constructor Folding

### Problem

When the scrutinee of a case expression is a literal data constructor
application (not a variable or function call), we know at compile time
which branch will be taken. The entire case can be replaced with the
matching branch body.

### Location

Same optimiser pass as the single-branch elimination, or a separate
pass in `src/eval/stg/optimiser.rs`.

### Transformation

```
// Before:
Case DataCon(BoolTrue)
  BoolTrue  -> expr_a
  BoolFalse -> expr_b

// After:
expr_a
```

For constructors with arguments:

```
// Before:
Case DataCon(BoxedNumber, [42])
  BoxedNumber(n) -> body(n)

// After:
let n = 42 in body(n)
```

### When this arises

- Constant folding after inlining -- an inlined function returns a
  known constructor, which is then cased on
- Boolean expressions with literal arguments -- `if true then a else b`
- Generated code from the desugarer that constructs then immediately
  destructs

### When NOT to transform

- Scrutinee is a variable reference, function application, or anything
  requiring evaluation
- Scrutinee is a `let` binding wrapping a constructor -- would need to
  look through the binding (keep it simple, do not chase through lets)

### Implementation

Pattern-match `Case { scrutinee, branches, fallback }` where
`scrutinee` is `StgSyn::Cons { tag, args }` (or the equivalent data
construction form). Look up `tag` in `branches`, substitute `args` as
local bindings in the matched body. If no branch matches, use
`fallback`. If neither matches, leave the case unchanged.

### Testing

- Unit test: construct a case with a literal constructor scrutinee,
  verify folding produces the correct branch body
- All harness tests pass
- May not fire often in current code unless combined with inlining, but
  is a standard compiler transform worth having

---

## 3. O(1) Tag Dispatch

### Problem

`match_tag()` in `src/eval/machine/cont.rs:61-68` does a linear scan
through branch pairs:

```rust
pub fn match_tag(tag: Tag, branches: &[(Tag, RefPtr<HeapSyn>)]) -> Option<RefPtr<HeapSyn>> {
    for (t, body) in branches {
        if *t == tag {
            return Some(*body);
        }
    }
    None
}
```

Tags are `u8` (0-255). With 12 defined constructors and typical
branches of 2-3 entries, the linear scan is fast in practice. But this
is on the hot path -- every case dispatch calls it.

### Approach

**Small inline array indexed by tag range.** At continuation
construction time, compute `min_tag` and `max_tag` from the branches.
Allocate an array of size `max_tag - min_tag + 1`, populate by tag
offset. Lookup is `table[tag - min_tag]`.

For typical cases (`BoolTrue`=1/`BoolFalse`=2, or
`ListNil`=6/`ListCons`=7), this is a 2-entry array with O(1) lookup.
Worst case for all 12 constructors is a 12-entry array.

### Alternatives considered

- **256-entry array**: O(1) but 2KB per continuation. Wasteful for
  typical 2-3 branch cases.
- **Sorted + binary search**: O(log n) but more overhead than linear
  scan for 2-3 entries.

### Location

`src/eval/machine/cont.rs` and `src/eval/machine/vm.rs`.

### Implementation

- Change `Continuation::Branch` to store `min_tag: Tag`,
  `branch_table: Array<Option<RefPtr<HeapSyn>>>` instead of the pairs
  array. `fallback` stays unchanged.
- Construction: in `vm.rs` where `Continuation::Branch` is created
  (line 223-237), build the indexed table from the branch pairs.
- Lookup: replace `match_tag(tag, branches)` with bounds check + direct
  index: `if tag >= min_tag && (tag - min_tag) < table.len() {
  table[(tag - min_tag) as usize] } else { None }`.
- Remove `match_tag()` function.

### Risk

Low. The change is localised to continuation construction and dispatch.
The only subtlety is ensuring the `Array` allocation for the table goes
through the GC-managed heap correctly (it already does for the current
pairs array).

### Testing

- All harness tests pass (semantic equivalence)
- Unit test: construct branches with various tag combinations, verify
  correct dispatch
- Criterion benchmarks may show slight improvement since case dispatch
  is on the hot path

---

## 4. Skip Empty Env Frames for 0-Arity Branches

### Problem

When a case branch matches a 0-arity constructor (`BoolTrue`,
`BoolFalse`, `ListNil`, `Unit`), the current code allocates an
environment frame with zero bindings. There is an existing TODO at
`vm.rs:426`: "skip empty frames".

```rust
let closures = args.iter()                    // args is empty
    .map(|r| self.nav(view).resolve(r))
    .collect::<Result<Vec<SynClosure>, _>>()?;
let len = closures.len();                      // len is 0
self.closure = SynClosure::new(
    body,
    view.from_closures(closures.into_iter(), len, environment, self.annotation)?,
    //                 ^^^ allocates an env frame with 0 bindings
);
```

### Location

`src/eval/machine/vm.rs`, in `return_data()` (around line 418-432).

### Implementation

Before calling `from_closures`, check if `args` is empty. If so, reuse
the parent `environment` directly:

```rust
if args.is_empty() {
    self.closure = SynClosure::new(body, environment);
} else {
    let closures = args.iter()
        .map(|r| self.nav(view).resolve(r))
        .collect::<Result<Vec<SynClosure>, _>>()?;
    let len = closures.len();
    self.closure = SynClosure::new(
        body,
        view.from_closures(
            closures.into_iter(), len, environment, self.annotation
        )?,
    );
}
```

### Why this is safe

The branch body's local references (`L(0)`, `L(1)`, etc.) refer to
bindings in the environment frame. If the constructor has 0 arity, the
body will not reference any locally-bound constructor fields -- it can
only reference bindings from the enclosing scope, which are already in
`environment`. No new frame is needed.

### Impact

Boolean dispatch is extremely common in eucalypt (every `if`, `and`,
`or`, `not` uses `BoolTrue`/`BoolFalse` branches). Each avoided frame
saves a heap allocation. This reduces both allocation pressure and GC
load.

### Testing

- All harness tests pass
- Boolean-heavy programs should show reduced `machine_allocs` count in
  `-S` output
- Criterion benchmarks should show reduced allocation overhead

---

## Existing Bead: STG Inline into Wrappers (eu-bpe)

This bead is underspecified and needs investigation before it can be
designed. It remains in the epic as-is. Investigation should identify:

- What specific wrapper functions exist in the STG implementation
- Which inlining opportunities would provide meaningful performance
  gains
- How inlining interacts with the case optimisations above

eu-bpe is not blocked by any of the four case optimisations and can be
investigated independently.

---

## Beads

Existing:

- **eu-knx** -- STG: Case optimisation (redundant single-branch
  elimination). Update description with design from section 1.
- **eu-bpe** -- STG: Inline into STG wrappers. Remains as-is pending
  investigation.

New beads to create:

- **Case-of-known-constructor folding** -- compile-time, section 2
- **O(1) tag dispatch** -- runtime, section 3
- **Skip empty env frames for 0-arity branches** -- runtime, section 4
- **STG Optimisation epic** -- parent for all five beads
