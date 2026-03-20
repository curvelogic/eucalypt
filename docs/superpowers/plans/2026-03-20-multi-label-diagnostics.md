# Multi-label Diagnostics and Trace Improvements

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Improve error diagnostics by (1) adding secondary source labels from the env trace, (2) filtering internal machinery from the stack trace, and (3) reversing the stack trace to read top-down.

**Bead:** eu-knck

**Background:** A spike with `EU_ERROR_TRACE_DUMP=1` (see `to_diagnostic()` in `src/eval/error.rs`) revealed:

- **Env trace** contains a mix of intrinsic annotations (HEAD, EQ, CONS) and user source locations. The user source locations form a call chain that's very useful: e.g. `+` → `map` → `transform`. These could be rendered as secondary labels in the diagnostic.
- **Stack trace** is dominated by render pipeline intrinsics (RENDER_KV, RENDER_BLOCK_ITEMS, RENDER_DOC, AND, EQ). These are internal evaluation machinery and meaningless to users. The trace is also presented innermost-first (upside down relative to user expectation).
- **For simple errors** (direct type mismatch at top level), the env trace is mostly redundant — it repeats the error's own Smid. Secondary labels only help when there's a call chain (function calls, pipelines, map/filter).

**Tech Stack:** Rust, codespan-reporting (already supports `Label::secondary`)

---

## Chunk 1: Secondary Labels from Env Trace

### Task 1: Add secondary labels for user source locations in env trace

**Files:**
- Modify: `src/eval/error.rs` (`to_diagnostic()`)

- [ ] **Step 1: Extract user-source Smids from env trace**

After the existing primary label logic, filter the env trace to entries that:
1. Have `file.is_some()` (real source location, not intrinsic)
2. Have a different span from the primary label's span (not redundant)
3. Are deduplicated (same span shouldn't appear twice)

Collect these as `(file, span, Option<annotation>)` tuples.

- [ ] **Step 2: Render as secondary labels**

For each qualifying env trace entry, create a `Label::secondary(file, span)` with a contextual message:
- If the entry has an annotation (e.g. `ann="transform"`): `"in 'transform'"`
- If no annotation: `"called from here"` or similar

Limit to at most 3-4 secondary labels to avoid overwhelming the output.

- [ ] **Step 3: Order labels sensibly**

`codespan_reporting` renders labels in source order (by file and span), which is natural. No explicit ordering needed — just collect and add them.

- [ ] **Step 4: Handle the fallback case**

When the error's own Smid has no source location and the fallback mechanism promotes an env trace entry to primary, the remaining env trace entries should still be rendered as secondary labels (minus the one that became primary).

**Expected output for test 7 (pipeline error):**
```
error: type mismatch: expected number, found string
  ┌─ test.eu:2:31
  │
2 │ transform(items): items map(_ + 1)
  │                         --- -----^
  │                         |   |    |
  │                         |   |    expected number
  │                         |   called from here
  │                         in 'transform'
4 │ result: transform(data)
  │         ------- called from here
```

(Exact rendering depends on codespan_reporting's label layout — this is aspirational.)

**Verification:** Run the spike test cases with the new code. Compare output quality.

---

## Chunk 2: Filter and Reverse Stack Trace

### Task 2: Filter internal machinery from stack trace display

**Files:**
- Modify: `src/common/sourcemap.rs` (`format_trace()`)
- Modify: `src/driver/eval.rs` (stack trace rendering)

- [ ] **Step 1: Define a filter for internal intrinsics**

The following intrinsic annotations are render pipeline internals and should be filtered from user-visible stack traces:
- `RENDER_KV`, `RENDER_BLOCK_ITEMS`, `RENDER_DOC`, `RENDER`
- `AND`, `EQ` (when from render pipeline — these are the `==` equality checks used by the renderer)

Create a function `is_internal_intrinsic(annotation: &str) -> bool` or use a set.

Alternatively, take the simpler approach: only include stack trace entries that have a **user source location** (`file.is_some()`). Intrinsic-only entries (no file, only annotation) are almost always internal. This avoids maintaining an explicit filter list.

- [ ] **Step 2: Reformat trace entries to lead with the name**

Currently `format_trace()` produces entries like:
```
- /tmp/test.eu:2:31 (+)
- [prelude]:1103:26 (cons)
```

The most important thing (what's at that location) is parenthesised as an afterthought. Change to lead with the name:
```
- + at test.eu:2:31
- cons at prelude:1103:26
- transform at test.eu:1:1
```

Update the entry formatting in `format_trace()`:
- For entries with a source location and intrinsic name: `"{name} at {file}:{line}:{col}"`
- For entries with a source location and source snippet: `"{snippet} at {file}:{line}:{col}"`
- For entries with only a name (no file): `"{name}"` (or skip — see step 1 filtering)

Use `intrinsic_display_name()` consistently to map internal names (EQ, HEAD, CONS) to user-facing names (==, head, cons).

- [ ] **Step 3: Reverse the stack trace order**

Currently `format_trace()` iterates the trace slice in order, which is innermost-first (from `stack_trace_iter` which iterates `stack.iter().rev()`).

Reverse it so the output reads top-down: outermost call first, innermost (closest to error) last. This matches the conventional stack trace format users expect.

- [ ] **Step 4: Consider rendering the stack trace as secondary labels too**

For entries that have file locations, they could be rendered as `Label::secondary` instead of (or in addition to) text notes. This would show them inline in the source, which is more useful than a separate "stack trace:" section.

However, this may overlap with the env trace labels from Chunk 1. Need to deduplicate across both traces — don't show the same source location twice.

- [ ] **Step 5: Simplify the text stack trace**

If secondary labels handle the source-located entries, the text stack trace only needs to show entries that *don't* have source locations — i.e. named intrinsics in the call chain. Even these may not be useful. Consider whether the text stack trace section should be dropped entirely in favour of secondary labels.

**Verification:** Compare before/after for the spike test cases. The stack trace should be shorter, user-relevant, and read in the expected order.

---

## Chunk 3: Clean Up Spike Code

### Task 3: Remove or gate the trace dump

**Files:**
- Modify: `src/eval/error.rs`

- [ ] **Step 1: Keep `EU_ERROR_TRACE_DUMP` as a debug tool**

The spike code is useful for debugging error diagnostics. Keep it but document it alongside the other debug env vars in CLAUDE.md.

- [ ] **Step 2: Remove `format_smid_detail` if not needed by the dump**

If the dump is kept, `format_smid_detail` stays. If removed, clean it up.

**Verification:** `cargo clippy --all-targets -- -D warnings` passes.

---

## Implementation Order

1. **Chunk 1** (secondary labels) — the main user-facing improvement
2. **Chunk 2** (stack trace filtering/reversal) — cleanup of existing output
3. **Chunk 3** (spike cleanup) — housekeeping

Chunks 1 and 2 are independent and could be done in parallel or either order.

## Risk Assessment

- **Low risk:** All changes are in error formatting — no changes to evaluation, compilation, or runtime behaviour.
- **Medium risk:** Secondary labels may produce cluttered output for some error patterns. Mitigation: limit to 3-4 labels, deduplicate aggressively, test with varied examples.
- **Low risk:** Filtering stack trace entries may hide useful information in edge cases. Mitigation: keep `EU_ERROR_TRACE_DUMP` as a debug escape hatch.

## Open Questions

- Should secondary labels show the **call site** (`transform(data)` on line 4) or the **definition** (`transform(items): ...` on line 2), or both? The env trace currently contains definition-site Smids (the let frame annotations). Call-site Smids come from Ann nodes in the stack trace. Both are valuable but serve different purposes.
- For errors where env_trace has no user source locations (simple top-level errors), should we skip secondary labels entirely or look harder (e.g. in stack_trace)?
- Is there value in distinguishing "called from here" (call site) from "defined here" (definition site) in the label messages?
