# Resolve Imports Relative to Source File Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Resolve import paths relative to the importing `.eu` file's directory, not just `lib_path` and CWD.

**Architecture:** When `SourceLoader::load_tree()` processes an `.eu` file's imports, push the parent directory of that file onto `lib_path` before recursively loading imports, then pop it afterwards. This mirrors exactly what `tester.rs` already does manually (lines 93-101, 117-118). The change is small and localised to `src/driver/source.rs`.

**Tech Stack:** Rust

---

### Task 1: Write the failing test

**Files:**
- Create: `harness/test/aux/sub/081_sub_import.eu`
- Create: `harness/test/aux/sub/sub_local.eu`
- Modify: `harness/test/081_relative_import.eu` (create new)
- Modify: `tests/harness_test.rs`

**Step 1: Create a subdirectory test file that imports a sibling**

Create `harness/test/aux/sub/sub_local.eu`:

```eucalypt
local-value: 42
```

Create `harness/test/aux/sub/081_sub_import.eu` which imports `sub_local.eu` using a relative path:

```eucalypt
` { import: "sub_local.eu" }
result: local-value
```

This import should resolve relative to `aux/sub/` (the importing file's directory), not relative to the harness test root.

**Step 2: Create the main test file**

Create `harness/test/081_relative_import.eu`:

```eucalypt
` { import: "aux/sub/081_sub_import.eu" }

test-relative-import:
  ` { doc: "imports resolve relative to importing file" target: :test }
  result
```

The expected evaluation: `081_relative_import.eu` imports `aux/sub/081_sub_import.eu`, which in turn imports `sub_local.eu`. That second import must resolve as `aux/sub/sub_local.eu` because the importing file is in `aux/sub/`.

**Step 3: Add the harness test**

Add to `tests/harness_test.rs` alongside other test declarations:

```rust
harness_test!(test_harness_081, "081_relative_import");
```

**Step 4: Run the test to verify it fails**

```bash
cargo test test_harness_081 -- --nocapture
```

Expected: FAIL — `sub_local.eu` not found because current resolution only checks `lib_path` (harness root) and CWD.

**Step 5: Commit**

```bash
git add harness/test/081_relative_import.eu harness/test/aux/sub/ tests/harness_test.rs
git commit -m "test: add failing test for source-relative import resolution"
```

---

### Task 2: Implement source-relative import resolution

**Files:**
- Modify: `src/driver/source.rs` (the `load_tree` method, ~lines 223-236)

**Step 1: Read `load_tree` and `load_source`**

Read `src/driver/source.rs` to understand the current flow:

```rust
fn load_tree(&mut self, input: &Input) -> Result<usize, EucalyptError> {
    let locator = input.locator();
    let file_id = self.load_eucalypt(locator)?;
    let ast = self.asts.get(&file_id).expect("AST was just loaded");
    let inputs = self.imports.analyse_rowan_ast(input.clone(), ast)?;
    self.imports.check_for_cycles()?;
    for import_input in inputs {
        self.load(&import_input)?;
    }
    Ok(file_id)
}
```

**Step 2: Add parent directory to lib_path during recursive loading**

Modify `load_tree` to push the parent directory of the current file onto `lib_path` before recursively loading imports, and pop it after:

```rust
fn load_tree(&mut self, input: &Input) -> Result<usize, EucalyptError> {
    let locator = input.locator();
    let file_id = self.load_eucalypt(locator)?;
    let ast = self.asts.get(&file_id).expect("AST was just loaded");
    let inputs = self.imports.analyse_rowan_ast(input.clone(), ast)?;
    self.imports.check_for_cycles()?;

    // Resolve imports relative to the importing file's directory
    let parent_dir = if let Locator::Fs(path) = locator {
        path.canonicalize()
            .ok()
            .and_then(|p| p.parent().map(|d| d.to_path_buf()))
    } else {
        None
    };

    if let Some(ref dir) = parent_dir {
        self.lib_path.push(dir.clone());
    }

    for import_input in inputs {
        self.load(&import_input)?;
    }

    if parent_dir.is_some() {
        self.lib_path.pop();
    }

    Ok(file_id)
}
```

This ensures that when processing imports from `/some/path/to/file.eu`, the directory `/some/path/to/` is temporarily on the search path. Nested imports stack correctly because `load_tree` is called recursively.

**Step 3: Run the new test**

```bash
cargo test test_harness_081 -- --nocapture
```

Expected: PASS

**Step 4: Run full test suite**

```bash
cargo test
```

Verify no regressions — existing imports still resolve via `lib_path` and CWD as before.

**Step 5: Run clippy**

```bash
cargo clippy --all-targets -- -D warnings
```

**Step 6: Commit**

```bash
git add src/driver/source.rs
git commit -m "feat: resolve imports relative to importing source file directory"
```

---

### Task 3: Close bead and push

**Step 1: Close the bead**

```bash
bd close eu-v1dr --reason="Imports now resolve relative to importing file's directory via lib_path push/pop in load_tree"
bd sync
```

**Step 2: Push**

```bash
git push
```
