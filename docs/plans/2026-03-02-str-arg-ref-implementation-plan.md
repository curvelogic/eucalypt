# `str_arg_ref()` Zero-Copy Heap String Borrowing — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Introduce a `str_arg_ref()` helper that returns `&str` borrowed directly from the heap string, eliminating the intermediate Rust `String` allocation that the current `str_arg()` performs. Migrate SPLIT, MATCH, MATCHES, and LETTERS intrinsics to use the new zero-copy helper.

**Architecture:** The current `str_arg()` in `src/eval/stg/support.rs` resolves a `Ref` to a `Native::Str(RefPtr<HeapString>)`, then calls `view.scoped(s).as_str().to_string()` — creating an owned `String` copy. The new `str_arg_ref()` returns a `&str` that borrows directly from the heap `HeapString`, using the block pinning API (eu-skjg) to pin the containing block and prevent the GC from moving it during the borrow. Although the current GC does not evacuate objects, pinning is REQUIRED as a correctness-by-construction principle — introducing zero-copy borrows without pinning would create a latent safety hole that could cause unsound behaviour if a future GC change introduces evacuation.

**Tech Stack:** Rust, existing STG intrinsic infrastructure, `HeapString` heap type, block pinning API (eu-skjg)

**Dependency on eu-skjg:** This plan REQUIRES the block pinning API (eu-skjg) to be implemented first. The `str_arg_ref()` helper uses `PinGuard` to pin the heap block containing the borrowed string, ensuring correctness even under future GC evolution. Do not implement this plan until eu-skjg is complete.

---

### Task 1: Add `str_arg_ref()` helper to support.rs

**Files:**
- Modify: `src/eval/stg/support.rs`

**Step 1: Read the existing `str_arg()` for context**

Read `src/eval/stg/support.rs` lines 56–71 to confirm the current implementation.

**Step 2: Add `str_arg_ref()` function**

After the existing `str_arg()` function (after line 71), add a new function:

```rust
/// Helper for intrinsics to borrow a &str directly from the heap.
///
/// Unlike `str_arg()`, this returns a reference into the heap-allocated
/// `HeapString` without copying to a Rust `String`. The containing heap
/// block is pinned via `PinGuard` to prevent the GC from moving it
/// during the borrow.
pub fn str_arg_ref<'guard>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'guard>,
    arg: &Ref,
) -> Result<&'guard str, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::Str(s) = native {
        view.pin(s);
        Ok(view.scoped(s).as_ref().as_str())
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            IntrinsicType::String,
            native_type(&native),
        ))
    }
}
```

**Step 3: Add `as_str_ref()` to `ScopedPtr<HeapString>`**

The issue is that `view.scoped(s)` returns a `ScopedPtr<'guard, HeapString>` and `as_str()` returns `&str` tied to the `HeapString`'s lifetime. We need a way to get a `&'guard str` from the scoped pointer. The `ScopedPtr` already derefs to `&'guard T`, so `(*view.scoped(s)).as_str()` already returns `&'guard str` (the string data is on the heap alongside the `HeapString`). However, `view.scoped(s)` is a temporary — we need to ensure the borrow extends to `'guard`.

Actually, looking more carefully: `ScopedPtr<'guard, T>` implements `Deref<Target = T>` with the return type `&T` (not `&'guard T`). The `value` field is `&'guard T`. So `(*scoped).as_str()` borrows from the temporary `ScopedPtr`. We need to extract the `&'guard str` properly.

The cleanest approach is to add a free function or method that takes the `RefPtr<HeapString>` and `MutatorHeapView<'guard>` and returns `&'guard str`. We can do this by adding a method to `MutatorHeapView`:

In `src/eval/memory/mutator.rs`, add:

```rust
/// Borrow a &str directly from a heap string for the duration of the
/// mutator scope.
///
/// # Safety considerations
///
/// The caller MUST pin the containing block (via `view.pin(ptr)`)
/// before calling this method. Pinning prevents the GC from moving
/// the block during the borrow.
pub fn str_ref(self, ptr: RefPtr<HeapString>) -> &'guard str {
    let scoped = self.scoped(ptr);
    // SAFETY: The HeapString data lives on the heap for at least 'guard.
    // ScopedPtr.value is &'guard HeapString, so the str data within has
    // the same lifetime. The block is pinned by the caller.
    let heap_str: &'guard HeapString = scoped.value;
    heap_str.as_str()
}
```

Wait — `ScopedPtr.value` is private. Let us look at the `Deref` impl more carefully. The `Deref` returns `&T` which has the lifetime of `&self` (the `ScopedPtr`), not `'guard`. So we need a different approach.

The simplest and safest approach is to add a public method to `ScopedPtr` that returns a reference with the guard lifetime:

In `src/eval/memory/alloc.rs`, add to `impl<'guard, T: Sized> ScopedPtr<'guard, T>`:

```rust
/// Return a reference with the guard lifetime.
///
/// This is equivalent to `Deref::deref` but the returned reference
/// is explicitly tied to the `'guard` lifetime rather than the
/// lifetime of the ScopedPtr itself.
pub fn as_ref(&self) -> &'guard T {
    self.value
}
```

Then in `str_arg_ref`, use:

```rust
Ok(view.scoped(s).as_ref().as_str())
```

Wait, `as_ref()` on `ScopedPtr` still returns `&'guard T` but the temporary `ScopedPtr` must live long enough. Actually — `as_ref` returns `&'guard T` which is independent of the `ScopedPtr`'s own lifetime. That is the key point. Let me reconsider.

Actually `ScopedPtr::as_ref(&self) -> &'guard T` does work: the `'guard` lifetime comes from the struct parameter, not from `&self`. So the returned `&'guard T` is valid even after the `ScopedPtr` is dropped. This is the correct approach.

**Revised Step 3: Add `as_ref()` to `ScopedPtr`**

In `src/eval/memory/alloc.rs`, add to the `impl<'guard, T: Sized> ScopedPtr<'guard, T>` block:

```rust
/// Return a reference with the guard lifetime.
///
/// Unlike `Deref::deref` (which ties the result to `&self`), this
/// method returns a reference explicitly tied to `'guard`, allowing
/// it to outlive the `ScopedPtr` wrapper.
pub fn as_ref(&self) -> &'guard T {
    self.value
}
```

Then in `str_arg_ref()`, the full implementation becomes:

```rust
pub fn str_arg_ref<'guard>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'guard>,
    arg: &Ref,
) -> Result<&'guard str, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::Str(s) = native {
        view.pin(s);
        Ok(view.scoped(s).as_ref().as_str())
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            IntrinsicType::String,
            native_type(&native),
        ))
    }
}
```

**Step 4: Verify compilation**

```bash
cargo check --lib
```

Expected: compiles with no errors. No tests to run yet — no callers.

**Step 5: Commit**

```bash
cargo fmt --all
cargo clippy --all-targets -- -D warnings
git add src/eval/stg/support.rs src/eval/memory/alloc.rs
git commit -m "feat: add str_arg_ref() zero-copy heap string borrowing helper"
```

---

### Task 2: Write a unit test for `str_arg_ref()` via LETTERS

**Files:**
- Modify: `harness/test/034_letters.eu` (add test cases for confidence)

We cannot write a direct unit test for `str_arg_ref()` in isolation easily because it requires an `IntrinsicMachine`. Instead, the best approach is to migrate one intrinsic (LETTERS — the simplest) and verify the existing harness tests pass. We do this in the next task, but first let us confirm the existing tests pass as a baseline.

**Step 1: Run the string-related harness tests to establish baseline**

```bash
cargo test test_harness_016
cargo test test_harness_034
```

Expected: both pass (PASS).

**Step 2: Commit (no changes needed, this is a verification step)**

No commit needed.

---

### Task 3: Migrate LETTERS to use `str_arg_ref()`

**Files:**
- Modify: `src/eval/stg/string.rs` (LETTERS intrinsic)

**Step 1: Read the current LETTERS implementation**

In `src/eval/stg/string.rs`, the LETTERS `execute` method (lines 427–436):

```rust
fn execute(
    &self,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    _emitter: &mut dyn Emitter,
    args: &[Ref],
) -> Result<(), ExecutionError> {
    let string = str_arg(machine, view, &args[0])?;
    let iter = string.chars().map(|c| c.to_string());
    machine_return_str_iter(machine, view, iter)
}
```

**Step 2: Update the import in string.rs**

Add `str_arg_ref` to the import from `super::support`:

Change:
```rust
    support::{
        machine_return_num, machine_return_str, machine_return_str_iter, machine_return_str_list,
        machine_return_sym, str_arg, str_list_arg,
    },
```

To:
```rust
    support::{
        machine_return_num, machine_return_str, machine_return_str_iter, machine_return_str_list,
        machine_return_sym, str_arg, str_arg_ref, str_list_arg,
    },
```

**Step 3: Replace `str_arg` with `str_arg_ref` in LETTERS**

Change the LETTERS `execute` method to:

```rust
fn execute(
    &self,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    _emitter: &mut dyn Emitter,
    args: &[Ref],
) -> Result<(), ExecutionError> {
    let string = str_arg_ref(machine, view, &args[0])?;
    let iter = string.chars().map(|c| c.to_string());
    machine_return_str_iter(machine, view, iter)
}
```

The only change is `str_arg` to `str_arg_ref`. The type changes from `String` to `&str`, but `.chars()` works on both.

**Step 4: Verify tests pass**

```bash
cargo test test_harness_034
```

Expected: passes (LETTERS test).

**Step 5: Run clippy and commit**

```bash
cargo fmt --all
cargo clippy --all-targets -- -D warnings
git add src/eval/stg/string.rs
git commit -m "refactor: migrate LETTERS intrinsic to str_arg_ref() zero-copy borrowing"
```

---

### Task 4: Migrate SPLIT to use `str_arg_ref()`

**Files:**
- Modify: `src/eval/stg/string.rs` (SPLIT intrinsic)

**Step 1: Read the current SPLIT implementation**

In `src/eval/stg/string.rs`, the SPLIT `execute` method (lines 261–278):

```rust
fn execute(
    &self,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    _emitter: &mut dyn Emitter,
    args: &[Ref],
) -> Result<(), ExecutionError> {
    let string = str_arg(machine, view, &args[0])?;
    let regex = str_arg(machine, view, &args[1])?;

    if regex.is_empty() {
        machine_return_str_list(machine, view, vec![string])
    } else {
        let re = cached_regex(machine, regex)?;
        let v: Vec<String> = re.split(&string).map(|it| it.to_string()).collect();
        machine_return_str_iter(machine, view, v.into_iter())
    }
}
```

**Step 2: Migrate to `str_arg_ref()`**

There is a subtlety with SPLIT: in the empty-regex case, `string` is returned as a `String` via `machine_return_str_list`. With `str_arg_ref`, we get `&str` instead. We need to convert to owned `String` only in that path.

Also, the regex argument is passed to `cached_regex()` which takes `T: AsRef<str>`, so `&str` works directly.

Replace the SPLIT `execute` method:

```rust
fn execute(
    &self,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    _emitter: &mut dyn Emitter,
    args: &[Ref],
) -> Result<(), ExecutionError> {
    let string = str_arg_ref(machine, view, &args[0])?;
    let regex = str_arg_ref(machine, view, &args[1])?;

    if regex.is_empty() {
        machine_return_str_list(machine, view, vec![string.to_string()])
    } else {
        let re = cached_regex(machine, regex)?;
        let v: Vec<String> = re.split(string).map(|it| it.to_string()).collect();
        machine_return_str_iter(machine, view, v.into_iter())
    }
}
```

Note: `cached_regex` takes `T: AsRef<str>` so passing `&str` works. The `re.split()` takes `&str` directly. The `to_string()` calls on the split fragments are necessary because `machine_return_str_iter` takes `Iterator<Item = String>`.

**Step 3: Check that `cached_regex` works with `&str`**

The `cached_regex` function signature is:
```rust
fn cached_regex<T: AsRef<str>>(
    machine: &mut dyn IntrinsicMachine,
    text: T,
) -> Result<&Regex, ExecutionError>
```

This takes `T: AsRef<str>`. Passing `&str` works because `&str` implements `AsRef<str>`. However, there is a lifetime issue: `cached_regex` borrows `machine` mutably and `text` is `&str` with lifetime `'guard`. The `rcache.contains(text_ref)` call borrows `text_ref` (a `&str` derived from the input). This should work fine since `text_ref` does not need to outlive the function call.

But wait — `cached_regex` calls `rcache.put(text_ref.to_string(), re)` which creates an owned `String`. And `rcache.get(text_ref)` looks up by `&str`. So passing `&str` as the `text` parameter works correctly.

**Step 4: Verify tests pass**

```bash
cargo test test_harness_016
```

Expected: passes (string functions test including split, join, match, matches).

**Step 5: Run clippy and commit**

```bash
cargo fmt --all
cargo clippy --all-targets -- -D warnings
git add src/eval/stg/string.rs
git commit -m "refactor: migrate SPLIT intrinsic to str_arg_ref() zero-copy borrowing"
```

---

### Task 5: Migrate MATCH to use `str_arg_ref()`

**Files:**
- Modify: `src/eval/stg/string.rs` (MATCH intrinsic)

**Step 1: Read the current MATCH implementation**

In `src/eval/stg/string.rs`, the MATCH `execute` method (lines 191–213):

```rust
fn execute(
    &self,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    _emitter: &mut dyn Emitter,
    args: &[Ref],
) -> Result<(), ExecutionError> {
    let string = str_arg(machine, view, &args[0])?;
    let regex = str_arg(machine, view, &args[1])?;
    let re = cached_regex(machine, regex)?;
    let v: Vec<String> = if let Some(captures) = re.captures(&string) {
        captures
            .iter()
            .map(|m| match m {
                Some(mch) => mch.as_str().to_string(),
                None => String::new(),
            })
            .collect()
    } else {
        vec![]
    };
    machine_return_str_iter(machine, view, v.into_iter())
}
```

**Step 2: Migrate to `str_arg_ref()`**

Replace:

```rust
fn execute(
    &self,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    _emitter: &mut dyn Emitter,
    args: &[Ref],
) -> Result<(), ExecutionError> {
    let string = str_arg_ref(machine, view, &args[0])?;
    let regex = str_arg_ref(machine, view, &args[1])?;
    let re = cached_regex(machine, regex)?;
    let v: Vec<String> = if let Some(captures) = re.captures(string) {
        captures
            .iter()
            .map(|m| match m {
                Some(mch) => mch.as_str().to_string(),
                None => String::new(),
            })
            .collect()
    } else {
        vec![]
    };
    machine_return_str_iter(machine, view, v.into_iter())
}
```

Changes: `str_arg` to `str_arg_ref`, `&string` to `string` (already a `&str`).

**Step 3: Verify tests pass**

```bash
cargo test test_harness_016
```

Expected: passes.

**Step 4: Run clippy and commit**

```bash
cargo fmt --all
cargo clippy --all-targets -- -D warnings
git add src/eval/stg/string.rs
git commit -m "refactor: migrate MATCH intrinsic to str_arg_ref() zero-copy borrowing"
```

---

### Task 6: Migrate MATCHES to use `str_arg_ref()`

**Files:**
- Modify: `src/eval/stg/string.rs` (MATCHES intrinsic)

**Step 1: Read the current MATCHES implementation**

In `src/eval/stg/string.rs`, the MATCHES `execute` method (lines 226–242):

```rust
fn execute(
    &self,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    _emitter: &mut dyn Emitter,
    args: &[Ref],
) -> Result<(), ExecutionError> {
    let string = str_arg(machine, view, &args[0])?;
    let regex = str_arg(machine, view, &args[1])?;
    let re = cached_regex(machine, regex)?;

    let v: Vec<String> = re
        .find_iter(&string)
        .map(|m| m.as_str().to_string())
        .collect();
    machine_return_str_iter(machine, view, v.into_iter())
}
```

**Step 2: Migrate to `str_arg_ref()`**

Replace:

```rust
fn execute(
    &self,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    _emitter: &mut dyn Emitter,
    args: &[Ref],
) -> Result<(), ExecutionError> {
    let string = str_arg_ref(machine, view, &args[0])?;
    let regex = str_arg_ref(machine, view, &args[1])?;
    let re = cached_regex(machine, regex)?;

    let v: Vec<String> = re
        .find_iter(string)
        .map(|m| m.as_str().to_string())
        .collect();
    machine_return_str_iter(machine, view, v.into_iter())
}
```

Changes: `str_arg` to `str_arg_ref`, `&string` to `string`.

**Step 3: Verify tests pass**

```bash
cargo test test_harness_016
```

Expected: passes.

**Step 4: Run clippy and commit**

```bash
cargo fmt --all
cargo clippy --all-targets -- -D warnings
git add src/eval/stg/string.rs
git commit -m "refactor: migrate MATCHES intrinsic to str_arg_ref() zero-copy borrowing"
```

---

### Task 7: Run full test suite and bench tests

**Files:**
- No modifications

**Step 1: Run the full harness test suite**

```bash
cargo test --test harness_test
```

Expected: all tests pass.

**Step 2: Run the string-specific bench tests to verify correctness**

```bash
cargo test test_bench_011
cargo test test_bench_012
cargo test test_bench_014
```

Expected: all pass. (These are the string split/join, regex match, and text transform benchmarks.)

**Step 3: Run lib tests to catch any compilation issues**

```bash
cargo test --lib
```

Expected: all pass.

**Step 4: Run clippy one final time**

```bash
cargo clippy --all-targets -- -D warnings
```

Expected: no warnings.

---

### Task 8: Remove unused `str_arg` import if applicable

**Files:**
- Modify: `src/eval/stg/string.rs` (only if `str_arg` is no longer used)

**Step 1: Check whether `str_arg` is still used in string.rs**

After migrating SPLIT, MATCH, MATCHES, and LETTERS, check which intrinsics in `string.rs` still use `str_arg`:

- `Sym::execute` — uses `str_arg` (line 73) — NOT migrated (returns symbol, not list)
- `Join::execute` — uses `str_arg` (line 171) — NOT migrated (single string output)
- `NumParse::execute` — uses `str_arg` (line 298) — NOT migrated (parses number)
- `Fmt::execute` — uses `str_arg` (line 405) — NOT migrated (single string output)
- `Upper::execute` — uses `str_arg` (line 471) — NOT migrated (single string output)
- `Lower::execute` — uses `str_arg` (line 491) — NOT migrated (single string output)

So `str_arg` is still used. The import should remain. No changes needed.

**Step 2: Verify that `str_arg_ref` is imported and used**

Confirm that the import added in Task 3 Step 2 (`str_arg_ref`) is present and used by at least one of the four migrated intrinsics.

**Step 3: Commit if any cleanup was needed**

No commit needed if no changes were made.

---

## Summary

| Task | Description | Est. Time |
|------|-------------|-----------|
| 1 | Add `str_arg_ref()` helper and `ScopedPtr::as_ref()` | 5 min |
| 2 | Verify baseline harness tests pass | 2 min |
| 3 | Migrate LETTERS to `str_arg_ref()` | 3 min |
| 4 | Migrate SPLIT to `str_arg_ref()` | 3 min |
| 5 | Migrate MATCH to `str_arg_ref()` | 3 min |
| 6 | Migrate MATCHES to `str_arg_ref()` | 3 min |
| 7 | Full test suite verification | 3 min |
| 8 | Import cleanup check | 2 min |

**Total estimated time: ~24 minutes**

## Future Work

Once the zero-copy pattern is proven with these four intrinsics, consider
migrating additional string intrinsics (`Join`, `Upper`, `Lower`, `Fmt`,
`Sym`, `NumParse`) where the owned `String` copy is unnecessary.

GC stress tests with string-heavy workloads would provide additional
confidence in the pinning-based borrowing model under pressure.
