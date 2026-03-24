# Debug Functions, Unified Expectations, and Error Source Locations

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> **MANDATORY**: Before writing ANY eucalypt (.eu) code, read: `docs/reference/agent-reference.md`, `docs/appendices/syntax-gotchas.md`, `docs/appendices/cheat-sheet.md`

**Goal:** Add debug tracing, unify assertion/expectation operators with diagnostics, and ensure all errors carry source locations.

**Architecture:** Three features sharing a common `__DBG_REPR` BIF for rendering values as diagnostic strings. Debug functions (eu-x6ry) and expectations (eu-92pk) both emit to stderr. Error source locations (eu-gwse) is a systematic audit of existing code. Implementation order: shared foundation → debug → expectations → error locations.

**Tech Stack:** Rust (BIF intrinsics, VM settings), eucalypt prelude

---

## Chunk 1: Shared Foundation — `__DBG_REPR` BIF

The `__DBG_REPR` BIF renders any eucalypt value to a compact, human-readable string for diagnostic output. Used by both debug functions and expectation failure messages.

### Task 1: `__DBG_REPR` intrinsic

**Files:**
- Create: `src/eval/stg/debug.rs`
- Modify: `src/eval/stg/mod.rs` (register module)
- Modify: `src/eval/intrinsics.rs` (add intrinsic entry)
- Modify: `src/eval/stg/support.rs` (add `dbg_repr` helper if needed)
- Test: `tests/harness/124_debug.eu`
- Test: `tests/harness_test.rs` (add test entry)

- [ ] **Step 1: Create `src/eval/stg/debug.rs` with `DbgRepr` struct**

The BIF takes one argument (any value) and returns a string representation. It must handle all `Native` variants and data constructors (Block, List, Cons).

```rust
//! Debug representation and tracing intrinsics

use crate::eval::emit::Emitter;
use crate::eval::error::ExecutionError;
use crate::eval::machine::intrinsic::{CallGlobal1, IntrinsicMachine, StgIntrinsic};
use crate::eval::memory::mutator::MutatorHeapView;
use crate::eval::memory::syntax::{Native, Ref};
use crate::eval::stg::support::machine_return_str;
use crate::eval::stg::tags::DataConstructor;

/// __DBG_REPR(value) — render any value to a compact debug string
pub struct DbgRepr;

impl StgIntrinsic for DbgRepr {
    fn name(&self) -> &str {
        "DBG_REPR"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let repr = render_debug_repr(machine, &view, &args[0], 0);
        machine_return_str(machine, view, &repr)
    }
}

impl CallGlobal1 for DbgRepr {}

const MAX_DEPTH: usize = 4;
const MAX_ITEMS: usize = 10;

/// Render a value to a compact debug string, with depth limiting.
pub fn render_debug_repr(
    machine: &dyn IntrinsicMachine,
    view: &MutatorHeapView<'_>,
    r: &Ref,
    depth: usize,
) -> String {
    if depth > MAX_DEPTH {
        return "...".to_string();
    }

    let nav = machine.nav(view.clone());
    match nav.resolve_native(r) {
        Ok(Native::Num(n)) => format!("{n}"),
        Ok(Native::Str(s)) => format!("{s:?}"),  // quoted with escapes
        Ok(Native::Sym(id)) => {
            let pool = machine.symbol_pool();
            format!(":{}", pool.resolve(id).unwrap_or("?"))
        }
        Ok(Native::Zdt(zdt)) => format!("@{zdt}"),
        _ => {
            // Try to resolve as data constructor (block, list, etc.)
            match nav.resolve(r) {
                Ok(closure) => {
                    let code = view.scoped(closure.code());
                    use crate::eval::memory::syntax::HeapSyn;
                    match &*code {
                        HeapSyn::Cons { tag, args } => {
                            render_data_constructor(machine, view, *tag, args, depth)
                        }
                        _ => "<thunk>".to_string(),
                    }
                }
                Err(_) => "<error>".to_string(),
            }
        }
    }
}

fn render_data_constructor(
    machine: &dyn IntrinsicMachine,
    view: &MutatorHeapView<'_>,
    tag: u8,
    args: &crate::eval::memory::array::Array<Ref>,
    depth: usize,
) -> String {
    match DataConstructor::try_from(tag) {
        Ok(DataConstructor::ListCons) => {
            // Render as list [...] by walking cons cells
            let mut items = vec![];
            // Walk the list structure
            // (simplified — full implementation needs to follow tail refs)
            format!("[...list...]")
        }
        Ok(DataConstructor::ListNil) => "[]".to_string(),
        Ok(DataConstructor::Block) => {
            format!("{{...block...}}")
        }
        Ok(DataConstructor::BoolTrue) => "true".to_string(),
        Ok(DataConstructor::BoolFalse) => "false".to_string(),
        Ok(DataConstructor::Unit) => "()".to_string(),
        _ => format!("<data tag={tag}>"),
    }
}
```

Note: The `render_debug_repr` function above is a skeleton. The full implementation needs to:
- Walk cons-list structures to render `[1, 2, 3]` (up to MAX_ITEMS)
- Walk block pair lists to render `{key: value, ...}` (up to MAX_ITEMS)
- Resolve symbol IDs via the symbol pool for block keys
- Handle truncation with `...` for large structures

The implementor should study `src/eval/stg/render.rs` for how RENDER_DOC walks block/list structures, and adapt a simplified version.

- [ ] **Step 2: Register the module and intrinsic**

In `src/eval/stg/mod.rs`, add `pub mod debug;` to the module list.

In `src/eval/intrinsics.rs`, add at the next available index:

```rust
Intrinsic {
    name: "DBG_REPR",
    ty: function(vec![unk()]).unwrap(),
    strict: vec![0],
},
```

In `src/eval/stg/mod.rs` `register_intrinsics()`, add:

```rust
runtime.register(Box::new(debug::DbgRepr));
```

- [ ] **Step 3: Write harness test**

Create `tests/harness/124_debug.eu`:

```eu
{ title: "124 debug repr" }

` { target: :test }
test: {
  num-repr: __DBG_REPR(42) //= "42"
  str-repr: __DBG_REPR("hello") //= "\"hello\""
  sym-repr: __DBG_REPR(:foo) //= ":foo"
  bool-repr: __DBG_REPR(true) //= "true"
  null-repr: __DBG_REPR(null) //= "()"
  RESULT: [num-repr, str-repr, sym-repr, bool-repr, null-repr] all-true? then(:PASS, :FAIL)
}
```

Add to `tests/harness_test.rs`:

```rust
harness_test!(test_harness_124, "124_debug");
```

- [ ] **Step 4: Build and run test**

```bash
cargo test test_harness_124
```

- [ ] **Step 5: Commit**

```bash
git add src/eval/stg/debug.rs src/eval/stg/mod.rs src/eval/intrinsics.rs tests/harness/124_debug.eu tests/harness_test.rs
git commit -m "feat: add __DBG_REPR intrinsic for debug value rendering"
```

---

## Chunk 2: Debug Functions (eu-x6ry)

### Task 2: Test mode flag on MachineSettings

This flag is needed by both debug and expectations. The tester sets it; it controls whether `__EXPECT` panics or returns false.

**Files:**
- Modify: `src/eval/machine/vm.rs` (MachineSettings, Machine::new)
- Modify: `src/eval/machine/intrinsic.rs` (IntrinsicMachine trait)
- Modify: `src/driver/tester.rs` (set flag when creating machine)

- [ ] **Step 1: Add `test_mode` to MachineSettings**

In `src/eval/machine/vm.rs`:

```rust
pub struct MachineSettings {
    pub trace_steps: bool,
    pub dump_heap: bool,
    pub test_mode: bool,
}
```

Update `Machine::new()` to accept the flag (add parameter) and default it to `false`.

- [ ] **Step 2: Expose test_mode via IntrinsicMachine**

In `src/eval/machine/intrinsic.rs`, add to the trait:

```rust
fn test_mode(&self) -> bool;
```

Implement in `MachineState` (or wherever `IntrinsicMachine` is implemented):

```rust
fn test_mode(&self) -> bool {
    // This needs access to MachineSettings — may need to store
    // the flag in MachineState or pass it through
    false
}
```

Note: The implementor needs to check how `IntrinsicMachine` is implemented for `MachineState` and route the `test_mode` flag through appropriately. It may require adding a field to `MachineState` or changing how `facilities()` splits the Machine.

- [ ] **Step 3: Set test_mode in tester**

In `src/driver/tester.rs`, find where `standard_machine()` or `Machine::new()` is called and pass `test_mode: true`. Also update `StgSettings` if needed.

- [ ] **Step 4: Build and verify**

```bash
cargo build
cargo test --lib
```

- [ ] **Step 5: Commit**

```bash
git commit -m "feat: add test_mode flag to MachineSettings"
```

### Task 3: `__DBG` BIF and `dbg` prelude function

**Files:**
- Modify: `src/eval/stg/debug.rs` (add Dbg struct)
- Modify: `src/eval/intrinsics.rs` (register)
- Modify: `src/eval/stg/mod.rs` (register)
- Modify: `lib/prelude.eu` (add dbg function)
- Test: `tests/harness/124_debug.eu` (extend)

- [ ] **Step 1: Add `Dbg` BIF to `debug.rs`**

```rust
/// __DBG(opts_has_label, opts_has_trace, label_str, value, smid)
///
/// The prelude wires this up so that:
/// - opts.label becomes the label_str (or "" if absent)
/// - opts.trace becomes a bool
/// - value is the value to debug
///
/// Actually simpler: __DBG(label, trace_flag, value)
/// where label is a string (possibly empty) and trace_flag is a bool.
pub struct Dbg;

impl StgIntrinsic for Dbg {
    fn name(&self) -> &str {
        "DBG"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args[0] = label (string, may be empty)
        // args[1] = value to debug
        let label = str_arg(machine, view, &args[0])?;
        let repr = render_debug_repr(machine, &view, &args[1], 0);
        let smid = machine.annotation();

        // Format: ▶ [file.eu:12] value
        // or:     ▶ [file.eu:12] "label": value
        // TODO: resolve smid to file:line using sourcemap
        // For now, just use the smid numeric value
        if label.is_empty() {
            eprintln!("▶ {repr}");
        } else {
            eprintln!("▶ {label}: {repr}");
        }

        // Return the original value (transparent)
        // Set closure to the resolved value of args[1]
        let nav = machine.nav(view);
        let closure = nav.resolve(&args[1])?;
        machine.set_closure(closure)
    }
}

impl CallGlobal2 for Dbg {}
```

Note: Source location formatting (file:line from Smid) is not trivial from inside a BIF because the source map is not directly accessible from `IntrinsicMachine`. The implementor should check whether it's feasible to add source map access to the trait, or whether the Smid numeric value (or no location) is acceptable for now. The crash handler and error system both resolve Smids — study those patterns.

- [ ] **Step 2: Register intrinsic**

Add to `src/eval/intrinsics.rs`:

```rust
Intrinsic {
    name: "DBG",
    ty: function(vec![str(), unk()]).unwrap(),
    strict: vec![0],  // label is strict, value is NOT strict (we want to see thunks)
},
```

Note: Whether arg 1 (value) should be strict needs consideration. If strict, the value is forced before the BIF sees it — useful for seeing the actual value. If not strict, we see the thunk. Probably strict is correct since we want to see the evaluated result.

Register in `src/eval/stg/mod.rs`.

- [ ] **Step 3: Add `dbg` to prelude**

In `lib/prelude.eu`, add after the assertion operators section:

```eu
#
# Debug tracing
#

` { doc: "`dbg(opts, v)` - print value `v` to stderr with optional label and trace, return `v`.
    opts keys: label (string), trace (bool). Use dbg{} for simple debug."  }
dbg(opts, v): __DBG(lookup-or(:label, "", opts), v)
```

Note: The `trace` option requires stack trace access from the BIF — this may need to be deferred to a follow-up or implemented via a separate `__DBG_TRACE` BIF. Start without trace support.

- [ ] **Step 4: Add `▶` prefix operator to prelude**

```eu
` { doc: "`▶v` - debug-print value `v` to stderr and return it. Prefix operator."
    export: :suppress
    precedence: 85 }
(▶ v): __DBG("", v)
```

Note: Check that 85 is the right numeric value and that prefix operators at this precedence work as expected. Test with `x + ▶y` and `data map(f) ▶head`.

- [ ] **Step 5: Extend harness test**

Add to `tests/harness/124_debug.eu`:

```eu
# dbg returns value transparently
` { target: :test-dbg-transparent }
test-dbg-transparent: {
  result: 42 dbg{} //= 42
  labelled: "hello" dbg{label: "greeting"} //= "hello"
  RESULT: [result, labelled] all-true? then(:PASS, :FAIL)
}
```

- [ ] **Step 6: Add ▶ to emacs eucalypt-mode**

In `editors/emacs/eucalypt-mode.el`:
- Add `▶` to syntax highlighting (operator face)
- Add to Quail input method (e.g. `\tri` or `\play` → `▶`)
- Add to transient menu if there is one for operators

- [ ] **Step 7: Build and test**

```bash
cargo test test_harness_124
```

- [ ] **Step 8: Commit**

```bash
git commit -m "feat: add dbg function and ▶ prefix debug operator"
```

---

## Chunk 3: Unified Expectations (eu-92pk)

### Task 4: `__EXPECT` BIF

Replace the current `__ASSERT_FAIL` and metadata-based `//=` mechanism with a single context-aware BIF.

**Files:**
- Modify: `src/eval/stg/panic.rs` (add Expect struct, or create separate file)
- Modify: `src/eval/intrinsics.rs` (register)
- Modify: `src/eval/stg/mod.rs` (register)

- [ ] **Step 1: Add `Expect` BIF**

In `src/eval/stg/panic.rs` (or a new `src/eval/stg/expect.rs`):

```rust
/// __EXPECT(actual_value, expected_repr_str, pass_bool)
///
/// Core expectation BIF. Behaviour depends on test_mode:
///
/// On success (pass_bool is true):
///   - Both modes: return actual_value transparently
///
/// On failure (pass_bool is false):
///   - Both modes: emit diagnostic to stderr
///   - Normal mode: raise AssertionFailed error
///   - Test mode: return false
pub struct Expect;

impl StgIntrinsic for Expect {
    fn name(&self) -> &str {
        "EXPECT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args[0] = actual value (to return on success)
        // args[1] = expected representation string (for diagnostics)
        // args[2] = pass boolean
        let expected_repr = str_arg(machine, view, &args[1])?;
        let pass = bool_arg(machine, view, &args[2])?;

        if pass {
            // Success — return the actual value
            let nav = machine.nav(view);
            let closure = nav.resolve(&args[0])?;
            machine.set_closure(closure)
        } else {
            // Failure — emit diagnostic
            let actual_repr = render_debug_repr(machine, &view, &args[0], 0);
            let smid = machine.annotation();

            // TODO: resolve smid to file:line
            eprintln!(
                "EXPECT FAILED: expected {expected_repr}, got {actual_repr}"
            );

            if machine.test_mode() {
                // Test mode: return false, continue execution
                machine_return_bool(machine, view, false)
            } else {
                // Normal mode: panic
                Err(ExecutionError::AssertionFailed(
                    smid,
                    actual_repr,
                    expected_repr,
                ))
            }
        }
    }
}
```

Note: `bool_arg` may not exist as a helper yet. Check `support.rs` — if not, add one that resolves a Ref to a bool by checking for BoolTrue/BoolFalse data constructors. Study how the existing code checks boolean values.

- [ ] **Step 2: Register intrinsic**

Add to `src/eval/intrinsics.rs`:

```rust
Intrinsic {
    name: "EXPECT",
    ty: function(vec![unk(), str(), unk()]).unwrap(),
    strict: vec![1, 2],  // expected_repr and pass are strict; actual is lazy
},
```

Register in `src/eval/stg/mod.rs`.

- [ ] **Step 3: Build and verify**

```bash
cargo build
```

- [ ] **Step 4: Commit**

```bash
git commit -m "feat: add __EXPECT context-aware assertion BIF"
```

### Task 5: Rewrite prelude operators to use `__EXPECT`

**Files:**
- Modify: `lib/prelude.eu` (rewrite //=, //=?, //!, remove //=>, //!?, //!!)
- Test: `tests/harness/125_expectations.eu`
- Test: `tests/harness_test.rs`

- [ ] **Step 1: Rewrite `//=` operator**

Replace the current metadata-based implementation:

```eu
# OLD:
# (e //= v): e with-meta({ assert: (= v)}) expect.check

# NEW:
` { doc: "`e //= v` - check that `e` equals `v`.
    In test mode: emit diagnostic on failure, return boolean.
    In normal mode: emit diagnostic on failure, panic.
    On success: return `e`."
    export: :suppress
    associates: :left
    precedence: :meta }
(e //= v): __EXPECT(e, __DBG_REPR(v), e = v)
```

- [ ] **Step 2: Rewrite `//=?` operator**

```eu
` { doc: "`e //=? f` - check that `e` satisfies predicate `f`.
    In test mode: emit diagnostic on failure, return boolean.
    In normal mode: emit diagnostic on failure, panic.
    On success: return `e`."
    export: :suppress
    associates: :left
    precedence: :meta }
(e //=? f): __EXPECT(e, "to satisfy predicate", e f)
```

- [ ] **Step 3: Rewrite `//!` operator**

```eu
` { doc: "`e //!` - check that `e` is true.
    In test mode: emit diagnostic on failure, return boolean.
    In normal mode: emit diagnostic on failure, panic.
    On success: return `e`."
    export: :suppress
    precedence: :meta }
(e //!): __EXPECT(e, "true", e = true)
```

- [ ] **Step 4: Remove deprecated operators**

Remove `//=>`, `//!?`, `//!!` definitions. Keep `//=`, `//=?`, `//!` only.

Also remove the `expect` namespace (the metadata-based validation mechanism) since `//=` no longer uses it. But check whether anything else references `expect.check` or `expect.validator` first — search the codebase and test files.

- [ ] **Step 5: Add prelude helpers for RESULT boilerplate**

```eu
` "`expect.all-pass(block)` - return :PASS if all values in block are truthy, :FAIL otherwise.
    Convenience for the common RESULT aggregation pattern."
expect-all-pass(b): b values all-true? then(:PASS, :FAIL)
```

- [ ] **Step 6: Write harness test for unified expectations**

Create `tests/harness/125_expectations.eu`:

```eu
{ title: "125 expectations" }

` { target: :test }
test: {
  eq-pass: (2 + 3 //= 5) //= true
  eq-fail-returns-false-in-test: (2 + 3 //= 6) //= false
  pred-pass: (42 //=? (> 0)) //= true
  pred-fail: (42 //=? (< 0)) //= false
  true-pass: (true //!) //= true
  true-fail: (false //!) //= false
  RESULT: [eq-pass, eq-fail-returns-false-in-test, pred-pass, pred-fail, true-pass, true-fail] all-true? then(:PASS, :FAIL)
}
```

Note: This test runs in test mode (eu -T), so `//=` on failure returns false rather than panicking. The outer `//=` checks that the inner one returned the expected boolean. The implementor should verify this nesting works correctly.

Add to `tests/harness_test.rs`:

```rust
harness_test!(test_harness_125, "125_expectations");
```

- [ ] **Step 7: Build and test**

```bash
cargo test test_harness_125
```

Note: Existing tests (especially 049_tester.eu and 010_prelude.eu) use `//=` extensively. Run the full test suite to verify backwards compatibility:

```bash
cargo test
```

The key behavioural change: `//=` now returns the value on success (not just `true`). Tests that rely on `//=` returning `true` (e.g. `[a //= 1, b //= 2] all-true?`) will break because `//=` now returns `a` or `b` on success, not `true`. The implementor needs to audit existing tests and either:
- Make `//=` return `true` on success in test mode (different from normal mode)
- Update existing tests to use a different aggregation pattern

This is a critical design decision — discuss with the project owner.

- [ ] **Step 8: Commit**

```bash
git commit -m "feat: unify expectation and assertion operators via __EXPECT"
```

### Task 6: Tester relaxations

**Files:**
- Modify: `src/driver/tester.rs` or `lib/test.eu` (accept true/false, optional RESULT)

- [ ] **Step 1: Accept true/false as PASS/FAIL**

In `lib/test.eu`, modify `default-expectation`:

```eu
# Currently:
# default-expectation(context): context.result.RESULT

# New: accept true → :PASS, false → :FAIL, or :PASS/:FAIL directly
default-expectation(context): {
  r: context.result.RESULT
  mapped: if(r = true, :PASS, if(r = false, :FAIL, r))
}.mapped
```

Or handle in the Rust tester validation logic if simpler.

- [ ] **Step 2: Optional RESULT for target tests**

When a target test has no `RESULT` key, treat the whole output as the result. Truthy (non-null, non-false, non-empty) means pass.

This change is in `lib/test.eu`'s `default-expectation` or in the tester's validation logic. The implementor should check which is cleaner.

- [ ] **Step 3: Test the relaxations**

Add test cases that use `true`/`false` instead of `:PASS`/`:FAIL` and targets without explicit `RESULT`.

- [ ] **Step 4: Commit**

```bash
git commit -m "feat: tester accepts true/false and optional RESULT for targets"
```

---

## Chunk 4: Error Source Locations (eu-gwse)

This is a systematic audit. Each sub-task is one file or one category of errors.

### Task 7: Add Smid to `Panic` variant

**Files:**
- Modify: `src/eval/error.rs` (change Panic variant)
- Modify: all files that construct `Panic(String)` — 39 call sites across stg/

- [ ] **Step 1: Change Panic variant to carry Smid**

In `src/eval/error.rs`:

```rust
// OLD: Panic(String),
// NEW:
#[error("panic: {1}")]
Panic(Smid, String),
```

Update `HasSmid::smid()` match arm:

```rust
ExecutionError::Panic(s, _) => *s,
```

- [ ] **Step 2: Fix all Panic construction sites**

Search for `ExecutionError::Panic(` across the codebase. Each site needs to add `machine.annotation()` or `Smid::default()` as the first argument.

For sites in BIF `execute()` methods (most of stg/): use `machine.annotation()`.
For sites outside BIFs (driver code, etc.): use `Smid::default()`.

Run: `grep -rn 'ExecutionError::Panic(' src/ | grep -v test`

Fix each one. This is mechanical — add the Smid parameter.

- [ ] **Step 3: Build and test**

```bash
cargo build
cargo test
```

- [ ] **Step 4: Commit**

```bash
git commit -m "fix: add source location to Panic error variant"
```

### Task 8: Add Smid to arithmetic and numeric error variants

**Files:**
- Modify: `src/eval/error.rs` (6 variants)
- Modify: `src/eval/stg/arith.rs` (construction sites)

- [ ] **Step 1: Add Smid to variants**

Change these variants in `src/eval/error.rs`:

```rust
// Add Smid as first parameter to each:
DivisionByZero(Smid, String),
NumericDomainError(Smid, Number, Number),
NumericRangeError(Smid, Number, Number),
ComplexResult(Smid, Number, Number),
```

Update `HasSmid::smid()` for each.
Update `#[error(...)]` format strings (shift field indices).

- [ ] **Step 2: Fix construction sites in arith.rs**

Each `ExecutionError::NumericRangeError(x, y)` becomes `ExecutionError::NumericRangeError(machine.annotation(), x, y)`.

The `machine` parameter is available in `execute()` methods.

For `DivisionByZero`, check where it's raised — it may be in a helper function that doesn't have machine access. If so, pass the Smid through or restructure.

- [ ] **Step 3: Build and test**

```bash
cargo build
cargo test
```

- [ ] **Step 4: Commit**

```bash
git commit -m "fix: add source locations to arithmetic error variants"
```

### Task 9: Add Smid to format and datetime error variants

**Files:**
- Modify: `src/eval/error.rs` (6 variants)
- Modify: `src/eval/stg/string.rs` (format errors)
- Modify: `src/eval/stg/time.rs` (datetime errors)

- [ ] **Step 1: Add Smid to variants**

```rust
BadFormatString(Smid, String),
FormatFailure(Smid),
BadNumericTypeForFormat(Smid),
FormatError(Smid, String, Number),
BadNumberFormat(Smid, String),
BadDateTimeComponents(Smid, Number, Number, Number, Number, Number, Number, String),
BadTimeZone(Smid, String),
BadTimestamp(Smid, Number),
```

Update `HasSmid::smid()` and error format strings for each.

- [ ] **Step 2: Fix construction sites**

In `src/eval/stg/string.rs` and `src/eval/stg/time.rs`, add `machine.annotation()` to each construction site.

Note: Some of these errors are raised from helper functions that may not have `machine` in scope. The implementor will need to pass the Smid through from the calling `execute()` method.

- [ ] **Step 3: Build and test**

```bash
cargo build
cargo test
```

- [ ] **Step 4: Commit**

```bash
git commit -m "fix: add source locations to format and datetime error variants"
```

### Task 10: Remove `annotated_lambda` from intrinsic wrappers

This is the most impactful change for source location accuracy. Each `annotated_lambda` in a BIF wrapper masks the user's call-site annotation.

**Files:**
- Modify: 17 files in `src/eval/stg/` (see list below)

- [ ] **Step 1: Audit and categorise**

Run: `grep -rn 'annotated_lambda' src/eval/stg/ | grep -v test`

For each call site, determine:
- Is this a BIF wrapper? (Most are — these should switch to plain `lambda`)
- Is this the compiler generating code? (`compiler.rs` — may need to keep annotation)
- Does the wrapper have a custom annotation label? (Check the Smid passed)

- [ ] **Step 2: Replace in BIF wrappers**

For each BIF wrapper method:

```rust
// OLD:
fn wrapper(&self, annotation: Smid) -> LambdaForm {
    annotated_lambda(N, body, annotation)
}

// NEW:
fn wrapper(&self, annotation: Smid) -> LambdaForm {
    let _ = annotation;
    lambda(N, body)
}
```

Files to modify (BIF wrappers only — skip compiler.rs):
- `arith.rs` — arithmetic_wrapper, comparison_wrapper
- `array.rs`
- `block.rs` (most already fixed, check for remaining)
- `boolean.rs` — 4 wrappers
- `eq.rs` — 1 wrapper
- `force.rs`
- `graph.rs` — 3 wrappers
- `io.rs` — 3 wrappers
- `list.rs` — 4 wrappers
- `meta.rs`
- `render.rs`
- `running.rs`
- `set.rs`
- `string.rs`
- `syntax.rs`
- `time.rs`

- [ ] **Step 3: Write error test to verify source locations**

Create `tests/harness/errors/073_source_locations.eu`:

```eu
# Test that errors carry source locations
bomb: 1 / 0
```

With `.expect`:
```yaml
exit: 1
stderr: "073_source_locations.eu"
```

This verifies that the error message includes the source file name — confirming the source location propagated correctly.

Add more targeted tests for specific error types (type mismatch, lookup failure, etc.) to verify locations are correct.

- [ ] **Step 4: Build and run full test suite**

```bash
cargo test
```

This is critical — changing wrappers from `annotated_lambda` to `lambda` changes the annotation flow for ALL intrinsics. Some may rely on the wrapper annotation for correct behaviour. Run the full suite and investigate any failures carefully.

- [ ] **Step 5: Commit**

```bash
git commit -m "fix: remove annotated_lambda from BIF wrappers for correct source locations"
```

### Task 11: Audit remaining error paths

- [ ] **Step 1: Check coverage**

After all the above changes, run:

```bash
grep '_ => Smid::default()' src/eval/error.rs
```

This catch-all in `HasSmid::smid()` should only match internal/structural errors that genuinely can't carry a source location (AllocationError, EmptyEnvironment, etc.).

Verify that every user-triggerable error variant now has a Smid by:
1. Listing all variants without Smid in the match
2. For each, confirm it's truly an internal error the user can't trigger

- [ ] **Step 2: Verify fallback trace mechanism**

With all errors now carrying Smid, verify the fallback mechanism in `to_diagnostic()` (searching env_trace/stack_trace when Smid has no file) still works correctly. It should now be needed less often since errors carry their own source location.

- [ ] **Step 3: Update documentation**

Update `docs/reference/agent-reference.md` if it references the old assertion operators or error behaviour.

- [ ] **Step 4: Final commit**

```bash
git commit -m "fix: audit and verify all error paths carry source locations"
```

---

## Implementation Order and Dependencies

```
Task 1: __DBG_REPR BIF (shared foundation)
  ↓
Task 2: test_mode flag (shared by debug + expectations)
  ↓
  ├── Task 3: __DBG BIF + dbg prelude + ▶ operator (eu-x6ry)
  │
  └── Task 4: __EXPECT BIF (eu-92pk)
        ↓
      Task 5: Rewrite prelude operators
        ↓
      Task 6: Tester relaxations

Task 7: Panic Smid (eu-gwse, independent)
  ↓
Task 8: Arithmetic Smid
  ↓
Task 9: Format/datetime Smid
  ↓
Task 10: Remove annotated_lambda
  ↓
Task 11: Final audit
```

Tasks 3 and 4-6 can be parallelised after Tasks 1-2.
Tasks 7-11 are independent of Tasks 1-6 and can be parallelised.
