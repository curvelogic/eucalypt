# Thunk Memoisation Fix for Lazy Streams

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the STG machine so that Let-bound tail thunks in cons cells are properly memoised (updated to indirections after first force), enabling truly lazy streaming of large files instead of eager draining.

**Architecture summary:** Currently, `STREAM_NEXT` eagerly drains the entire stream producer into a complete list (`stream_drain`), because the original lazy cons-cell approach failed — Let-bound thunks in the cons tail position were not being memoised across multiple traversals. The root cause is that the `STREAM_NEXT` intrinsic builds its result using `LambdaForm::value()` bindings in a `LetRec`, which have `update: false`. This means the VM never pushes an `Update` continuation when entering them, so the thunk is re-evaluated on every access. The fix is to make `STREAM_NEXT` return a lazy cons cell where the tail is a genuine thunk (`update: true`) containing a recursive `STREAM_NEXT(handle)` call. Each tail thunk is forced at most once; the `Update` continuation replaces it with the computed value (the next cons cell or nil), achieving standard STG memoisation semantics.

**Tech stack:** Rust, existing STG machine infrastructure, harness test framework

---

### Task 1: Add `stream_next` single-advance function to `stream.rs`

**Files:**
- Modify: `src/eval/stg/stream.rs` (add function after `stream_drain`, around line 88)

**Step 1: Read the file**

Read `src/eval/stg/stream.rs` to confirm current state.

**Step 2: Add `stream_next` function**

After the `stream_drain` function (line 88), add a new function that advances a stream by exactly one element:

```rust
/// Advance a stream producer by one element.
///
/// Returns `Some(stg_value)` for the next element, or `None` when
/// the stream is exhausted. This is used by the lazy `STREAM_NEXT`
/// intrinsic to produce one cons cell at a time rather than eagerly
/// draining the entire stream.
pub fn stream_next(handle: u32) -> Option<Rc<StgSyn>> {
    STREAM_TABLE.with(|table| {
        let table = table.borrow();
        match table.get(handle) {
            Some(producer) => producer.borrow_mut().next(),
            None => None,
        }
    })
}
```

**Step 3: Verify it compiles**

```bash
cargo check --lib
```

Expected: compiles cleanly. The function is not yet called.

---

### Task 2: Rewrite `StreamNext` intrinsic to produce lazy cons cells

**Files:**
- Modify: `src/eval/stg/stream_intrinsic.rs` (complete rewrite of `execute` method)

**Step 1: Read the current implementation**

Read `src/eval/stg/stream_intrinsic.rs` to confirm current state.

**Step 2: Replace the `execute` method**

The key change: instead of eagerly draining all values via `stream_drain` and building a complete list, call `stream_next` to get one value. If a value is available, construct a cons cell where:
- The head is the loaded value (a `LambdaForm::value`)
- The tail is a `LambdaForm::thunk` containing `Bif { STREAM_NEXT, [handle] }` — a genuine updatable thunk that will be memoised after first force

Replace the entire file content with:

```rust
//! __STREAM_NEXT intrinsic implementation
//!
//! Lazily produces one cons cell per call: the head is the next
//! value from the stream producer, and the tail is a thunk that
//! will call STREAM_NEXT again when forced. Thunk memoisation
//! ensures each element is read from the IO source exactly once.

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    intrinsics,
    machine::{
        env::SynClosure,
        intrinsic::{CallGlobal1, IntrinsicMachine, StgIntrinsic},
    },
    memory::{
        array::Array,
        loader::load,
        mutator::MutatorHeapView,
        syntax::{LambdaForm, Ref, StgBuilder},
    },
};

use super::{stream::stream_next, support::num_arg, tags::DataConstructor};

/// STREAM_NEXT(handle)
///
/// Advances the stream producer identified by the numeric handle by
/// one element. Returns `Cons(value, thunk)` where the thunk lazily
/// calls `STREAM_NEXT(handle)` again, or returns `Nil` when the
/// stream is exhausted.
///
/// The tail thunk uses `LambdaForm::thunk` (update flag = true) so
/// the STG machine pushes an `Update` continuation when it is first
/// forced. After evaluation, the environment slot is overwritten
/// with the result (the next cons cell or nil), ensuring
/// memoisation — subsequent accesses return the cached value
/// without re-reading from the IO source.
pub struct StreamNext;

impl StgIntrinsic for StreamNext {
    fn name(&self) -> &str {
        "STREAM_NEXT"
    }

    /// Disable wrapper inlining — the side-effecting nature of stream
    /// advancement means the Bif node must not be duplicated by the
    /// compiler's `ProtoInline` substitution.
    fn inlinable(&self) -> bool {
        false
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let handle_num = num_arg(machine, view, &args[0])?;
        let handle = handle_num.as_u64().ok_or_else(|| {
            ExecutionError::Panic("stream handle must be a positive integer".to_string())
        })? as u32;

        match stream_next(handle) {
            None => {
                // Stream exhausted — return Nil
                let nil = view.nil()?;
                machine.set_closure(SynClosure::new(nil.as_ptr(), machine.root_env()))
            }
            Some(value_stg) => {
                // Load the value into the heap
                let value_ptr = load(&view, machine.symbol_pool_mut(), value_stg)?;

                // Build: Let [value, tail_thunk, cons_cell] in Atom(L(2))
                //
                //   binding 0: value       (LambdaForm::value)
                //   binding 1: tail_thunk  (LambdaForm::thunk — updatable!)
                //              body = Bif { STREAM_NEXT, [handle] }
                //   binding 2: cons_cell   (LambdaForm::value)
                //              body = Cons(ListCons, [L(0), L(1)])

                let stream_next_index = intrinsics::index("STREAM_NEXT")
                    .expect("STREAM_NEXT intrinsic must be registered");

                // The tail thunk calls STREAM_NEXT(handle) when forced.
                // Using Ref::V for the handle embeds it directly — no
                // environment reference needed.
                let handle_ref = Ref::num(handle as i64);
                let tail_body = view.app_bif(
                    stream_next_index as u8,
                    Array::from_slice(&view, &[handle_ref]),
                )?;

                let cons_node = view.data(
                    DataConstructor::ListCons.tag(),
                    Array::from_slice(&view, &[Ref::L(0), Ref::L(1)]),
                )?;

                let bindings = Array::from_slice(
                    &view,
                    &[
                        LambdaForm::value(value_ptr),
                        LambdaForm::thunk(tail_body.as_ptr()),
                        LambdaForm::value(cons_node.as_ptr()),
                    ],
                );

                let body = view.atom(Ref::L(2))?;
                let result = view.let_(bindings, body)?;
                machine.set_closure(SynClosure::new(result.as_ptr(), machine.root_env()))
            }
        }
    }
}

impl CallGlobal1 for StreamNext {}
```

**Step 3: Remove the `stream_drain` import**

The `stream_drain` function is no longer used by the intrinsic. Check if anything else imports it. If not, it can be removed in Task 3. For now, just ensure the intrinsic compiles without it.

**Step 4: Verify it compiles**

```bash
cargo check --lib
```

Expected: compiles cleanly.

---

### Task 3: Remove `stream_drain` if unused

**Files:**
- Modify: `src/eval/stg/stream.rs` (remove `stream_drain` function, lines 69-88)

**Step 1: Check for other users of `stream_drain`**

```bash
cargo check --lib 2>&1 | grep -i "unused\|dead_code\|stream_drain"
```

Search the codebase for references to `stream_drain` outside `stream_intrinsic.rs`. If none, remove the function.

**Step 2: Remove `stream_drain`**

Delete the `stream_drain` function (lines 69-88 of `src/eval/stg/stream.rs`) and its doc comment.

**Step 3: Verify it compiles**

```bash
cargo check --lib
```

Expected: compiles cleanly with no dead code warnings.

---

### Task 4: Add a `Let` variant to `StgBuilder` trait (if missing)

**Files:**
- Check: `src/eval/memory/syntax.rs` — verify `let_` method exists on `StgBuilder`

**Step 1: Verify `let_` exists on `StgBuilder`**

Read `src/eval/memory/syntax.rs` and confirm the `let_` method is available. It should already be present (around line 772). The `stream_intrinsic.rs` rewrite calls `view.let_(bindings, body)`, which requires this method.

**Step 2: If not present, add it**

This should already exist as `fn let_` on the `StgBuilder` trait (line 772 in `syntax.rs`). If it somehow does not exist, add it following the pattern of `letrec`.

**Step 3: Verify**

```bash
cargo check --lib
```

---

### Task 5: Run existing stream harness test

**Files:**
- Test: `harness/test/079_streams.eu`

**Step 1: Run the existing stream test**

```bash
cargo test test_harness_079 -- --nocapture
```

Expected: passes. The existing test verifies that `jsonl-stream`, `csv-stream`, and `text-stream` produce correct results. With the lazy implementation, these should still work because the test forces all elements (via `count`, `map`, `head`).

**Step 2: Run all tests to check for regressions**

```bash
cargo test
```

Expected: all tests pass.

---

### Task 6: Write a harness test for multi-traversal memoisation

**Files:**
- Create: `harness/test/080_stream_memo.eu`
- Modify: `tests/harness_test.rs` (add test entry)

This test verifies that accessing the same stream element multiple times returns the same value — the hallmark of correct thunk memoisation. Without memoisation, each access would advance the underlying IO cursor, producing different (wrong) values.

**Step 1: Create the test file**

Create `harness/test/080_stream_memo.eu`:

```eucalypt
{ import: ["jl=jsonl-stream@aux/aux_import.jsonl"] }

# Accessing the same stream position twice should return the same
# value, proving the tail thunks are memoised (not re-read from IO).

first-name: jl head .name
first-name-again: jl head .name

# If memoisation is broken, the second traversal would advance the
# file cursor and return a different (or missing) value.
same-first: first-name = first-name-again //= true

# Similarly, taking elements via two independent traversals should
# produce identical results.
names-a: jl map(_.name)
names-b: jl map(_.name)
same-names: names-a = names-b //= true

# The count should be consistent across multiple traversals.
count-a: jl count
count-b: jl count
same-count: count-a = count-b //= true

RESULT: [same-first, same-names, same-count] all-true? then(:PASS, :FAIL)
```

**Step 2: Add test entry to harness**

Add to `tests/harness_test.rs` after the `test_harness_079` entry:

```rust
#[test]
pub fn test_harness_080() {
    run_test(&opts("080_stream_memo.eu"));
}
```

**Step 3: Run the new test**

```bash
cargo test test_harness_080 -- --nocapture
```

Expected: PASS. If memoisation is working, both traversals of the same stream return identical results.

---

### Task 7: Clippy, format, and final verification

**Files:**
- All modified files

**Step 1: Run rustfmt**

```bash
cargo fmt --all
```

**Step 2: Run clippy**

```bash
cargo clippy --all-targets -- -D warnings
```

Fix any warnings.

**Step 3: Run full test suite**

```bash
cargo test
```

Expected: all tests pass, including the new `080_stream_memo` test and the existing `079_streams` test.

---

### Task 8: Commit all changes

**Files:**
- All modified and created files

**Step 1: Review changes**

```bash
git diff --stat
git diff
```

**Step 2: Commit**

```bash
git add src/eval/stg/stream_intrinsic.rs src/eval/stg/stream.rs harness/test/080_stream_memo.eu tests/harness_test.rs
git commit -m "fix: enable thunk memoisation for lazy stream cons cells

Replace eager stream draining (stream_drain) with lazy one-at-a-time
advancement (stream_next). STREAM_NEXT now returns Cons(value, thunk)
where the tail is a genuine updatable thunk (LambdaForm::thunk with
update=true). The STG machine pushes an Update continuation on first
force and replaces the thunk with its computed value, ensuring each
stream element is read from the IO source exactly once.

Fixes eu-xxrx."
```

---

## Root Cause Analysis

The bug exists in the interaction between how `STREAM_NEXT` constructs its result and how the STG machine handles thunk updates.

### How thunk memoisation works in the STG machine

1. **Thunk creation:** A `LambdaForm` with `update: true` (created by `LambdaForm::thunk()`) signals that the binding should be updated in-place after its first evaluation.

2. **Entering a thunk:** When the VM enters a local reference (`Atom { evaluand: Ref::L(i) }`), it retrieves the closure from the environment and checks `closure.update()`. If true, it:
   - Overwrites the environment slot with a `BlackHole` to detect cycles
   - Pushes an `Update { environment, index: i }` continuation
   - Enters the thunk's code

3. **Completing evaluation:** When the thunk's code reaches WHNF (a value, data constructor, or native), the VM pops continuations. When it encounters the `Update` continuation, it calls `env.update(index, current_closure)`, replacing the thunk in the environment with the computed value.

4. **Subsequent accesses:** The environment slot now holds the computed value (not a thunk), so `closure.update()` returns false, and no `Update` continuation is pushed — the value is returned directly.

### Why the original lazy approach failed

The original `STREAM_NEXT` implementation (before the eager workaround) likely used `LambdaForm::value()` for all bindings, including the tail thunk. `LambdaForm::value()` sets `update: false`, meaning:
- The VM never pushes an `Update` continuation when entering the tail
- The tail's body (`Bif { STREAM_NEXT, [handle] }`) is re-executed on every access
- Each re-execution advances the IO cursor, producing wrong values on subsequent traversals
- The workaround was to eagerly drain everything upfront

### The fix

Use `LambdaForm::thunk()` for the tail binding. This sets `update: true`, enabling the standard STG memoisation path:
- First access pushes `Update`, evaluates `STREAM_NEXT(handle)`, and replaces the thunk with the resulting cons cell (or nil)
- Second access finds the updated value and returns it directly
- The IO cursor advances exactly once per stream position

### The `suppress_next_update` concern

The VM has a `suppress_next_update` flag (set at `vm.rs:506`) that suppresses the `Update` push when a case branch body is a plain `Atom { evaluand: Ref::L(_) }`. This is an optimisation for tail recursion through conditionals. It does NOT affect the stream case because:
- The flag is consumed (reset to false) at the start of each `step()` call
- Stream thunks are entered via `Let` body evaluation, not via case branch tails
- The thunk is entered when user code forces the list tail (e.g. via `map`, `count`), which goes through normal `Atom { Ref::L(i) }` entry with `suppress_update = false`
