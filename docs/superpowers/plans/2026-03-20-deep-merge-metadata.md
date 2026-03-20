# Fix: Deep Merge Strips Block Metadata Tags

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> **MANDATORY**: Before writing ANY eucalypt (.eu) code, read: `docs/reference/agent-reference.md`, `docs/appendices/syntax-gotchas.md`, `docs/appendices/cheat-sheet.md`

**Goal:** Preserve block metadata through merge (catenation), merge-with, and deep merge (`<<`) operations.

**Bead:** eu-s10s

**Root cause:** All three merge wrappers (`Merge`, `MergeWith`, `DeepMerge` in `src/eval/stg/block.rs`) use `switch`/`case` to pattern-match their block arguments. These evaluate through `Meta` nodes, discarding metadata before the block cons-list reaches the merge logic.

**Fix strategy:** Use `demeta` to capture metadata from both operands before pattern-matching the block, then `with_meta` to re-attach the correct metadata to the result.

**Metadata merging rules:**
- Neither operand has metadata → return bare merged block
- Only LHS has metadata → preserve LHS metadata on result
- Only RHS has metadata → preserve RHS metadata on result
- Both have metadata → for deep merge (`<<`), deep-merge the metadata blocks; for shallow merge (catenation), RHS metadata replaces LHS

**Tech Stack:** Rust, STG DSL (`src/eval/stg/syntax.rs`)

---

## Chunk 1: Merge Wrapper (shallow merge / catenation)

### Task 1: Restructure Merge wrapper to preserve metadata

**Files:**
- Modify: `src/eval/stg/block.rs` (Merge::wrapper, lines 1097–1160)
- Create: `tests/harness/125_merge_metadata.eu`
- Modify: `tests/harness_test.rs` (add test entry)

**Current structure:**
```
lambda(2, [l r]
  switch(local(0),     # evaluates l, strips meta
    Block.tag() => [lcons lindex] [l r]
      switch(local(3), # evaluates r, strips meta
        Block.tag() => [rcons rindex] [lcons lindex] [l r]
          ... merge lcons rcons ...
      )
  )
)
```

**Target structure:**
```
lambda(2, [l r]
  demeta(local(0),
    # l has meta: [l_meta l_body] [l r]
    demeta(local(3),
      # both have meta: [r_meta r_body] [l_meta l_body] [l r]
      # merge bodies, attach r_meta to result
      <merge l_body r_body, with_meta(r_meta, result)>,
      # only l has meta: [r] [l_meta l_body] [l r]
      # merge l_body and r, attach l_meta to result
      <merge l_body r, with_meta(l_meta, result)>
    ),
    # l has no meta: [l] [l r]
    demeta(local(2),
      # only r has meta: [r_meta r_body] [l] [l r]
      # merge l and r_body, attach r_meta to result
      <merge l r_body, with_meta(r_meta, result)>,
      # neither has meta: [r] [l] [l r]
      # merge l and r, no meta
      <merge l r>
    )
  )
)
```

- [ ] **Step 1: Write the test first**

Create `tests/harness/125_merge_metadata.eu`:
```eucalypt
# Merge preserves metadata

left: {x: 1} // {tag: :left}
right: {y: 2} // {tag: :right}
plain-l: {x: 1}
plain-r: {y: 2}

# Shallow merge (catenation): RHS meta wins when both present
both-merged: merge(left, right)

# Only LHS has meta
left-only: merge(left, plain-r)

# Only RHS has meta
right-only: merge(plain-l, right)

# Neither has meta
neither: merge(plain-l, plain-r)

both-meta-tag:   both-merged meta lookup-or(:tag, :none) //= :right
left-meta-tag:   left-only meta lookup-or(:tag, :none) //= :left
right-meta-tag:  right-only meta lookup-or(:tag, :none) //= :right
neither-meta:    neither meta lookup-or(:tag, :none) //= :none

# Values still merge correctly
both-has-x: both-merged.x //= 1
both-has-y: both-merged.y //= 2
```

Add `test_harness_125` to `tests/harness_test.rs`.

- [ ] **Step 2: Restructure the Merge wrapper**

The key challenge is de Bruijn index management. The `demeta` form binds 2 variables (`[meta, body]`) in the handler branch, and 1 variable (`[value]`) in the or_else branch. Every outer reference must be adjusted.

Strategy: extract the core merge logic (the `switch` + `pack_items` + `bif::merge` sequence) into a helper function that takes two block arguments at known positions, then call it from each of the four demeta branches with appropriate `lref` adjustments. The helper can be a `let_`-bound lambda.

However, the simpler approach is: restructure so demeta extracts metadata, then delegate to a "merge bodies" inner lambda that does the existing switch logic, then wrap result with appropriate metadata.

Concrete approach:
1. Keep the existing `pack_items` lambda and `switch` logic as an inner `let_`-bound `merge_blocks` lambda
2. Wrap the outer lambda with demeta on both args
3. In each branch, call `merge_blocks` with the (possibly unwrapped) body refs
4. Wrap the result with `with_meta` when metadata exists

- [ ] **Step 3: Verify de Bruijn indices**

The index arithmetic must be verified carefully. Write out the environment stack for each branch:

**Both have meta branch:** `[r_meta, r_body, l_meta, l_body, l, r]`
- l_body is at offset 3 from innermost → `lref(3)`
- r_body is at offset 1 → `lref(1)`
- r_meta is at offset 0 → `lref(0)` (for shallow merge, RHS wins)

**Only LHS meta branch:** `[r, l_meta, l_body, l, r]`
- l_body is at offset 2 → `lref(2)`
- r is at offset 0 → `lref(0)` (but this is the unwrapped r, still needs switch)
- l_meta is at offset 1 → `lref(1)`

**Only RHS meta branch:** `[r_meta, r_body, l, l, r]`
- l is at offset 2 → `lref(2)` (unwrapped, still needs switch)
- r_body is at offset 1 → `lref(1)`
- r_meta is at offset 0 → `lref(0)`

**No meta branch:** `[r, l, l, r]`
- l is at offset 1 → `lref(1)`
- r is at offset 0 → `lref(0)`

- [ ] **Step 4: Run tests and fix any index errors**

`cargo test test_harness_125` — iterate until passing.

**Verification:** Test passes, existing merge tests (`062_yaml_merge`, `085_deep_merge_dynamic`, `015_block_fns`) still pass.

---

## Chunk 2: DeepMerge Wrapper (`<<`)

### Task 2: Restructure DeepMerge wrapper to preserve metadata

**Files:**
- Modify: `src/eval/stg/block.rs` (DeepMerge::wrapper, lines 1329–1358)
- Modify: `tests/harness/125_merge_metadata.eu` (add deep merge tests)

The DeepMerge wrapper currently does:
```
lambda(2, [l r]
  case(local(0), Block.tag() =>
    case(local(3), Block.tag() =>
      MergeWith(l_orig, r_orig, self)  # recurse
    , local(0))                        # r not block: return l
  , local(2))                          # l not block: return r
)
```

**Metadata rules for deep merge:**
- Both have block metadata → deep-merge the metadata (recursive `<<` on metas)
- Only one has metadata → preserve it
- Neither → bare result

- [ ] **Step 1: Add deep merge metadata tests**

Add to `tests/harness/125_merge_metadata.eu`:
```eucalypt
# Deep merge metadata
deep-left: {x: {a: 1}} // {tag: :deep-left}
deep-right: {x: {b: 2}} // {tag: :deep-right}

deep-merged: deep-left << deep-right

# Deep merge: metadata is deep-merged too (both are blocks)
# When both metas are blocks, they get deep-merged
deep-both-x: deep-merged.x.a //= 1
deep-both-y: deep-merged.x.b //= 2

# Deep merge with block metadata on both sides
left-meta-block: {x: 1} // {m: 1, n: 2}
right-meta-block: {y: 2} // {m: 10, p: 3}
deep-meta-merged: left-meta-block << right-meta-block

# Metadata is deep-merged: m from right wins, n preserved, p added
deep-meta-m: deep-meta-merged meta lookup-or(:m, 0) //= 10
deep-meta-n: deep-meta-merged meta lookup-or(:n, 0) //= 2
deep-meta-p: deep-meta-merged meta lookup-or(:p, 0) //= 3

# Non-block metadata: RHS symbol tag replaces LHS
tag-left: {x: 1} // {tag: :left}
tag-right: {y: 2} // :override
deep-tag: tag-left << tag-right
# When RHS meta is not a block, it replaces (cannot deep-merge symbol into block)
```

- [ ] **Step 2: Restructure DeepMerge wrapper**

The DeepMerge wrapper is simpler because it delegates to MergeWith. The restructuring needs to:
1. `demeta` both args to capture metadata
2. Pass unwrapped bodies to the existing `case`/`MergeWith` logic
3. Combine metadata:
   - Both are blocks → `DeepMerge.global(l_meta, r_meta)` (recursive)
   - Otherwise → prefer RHS if present, else LHS
4. `with_meta` the combined metadata onto the merged result

For the metadata combination, we need an `ISBLOCK` check on the metadata values to decide whether to deep-merge or replace. The `IsBlock` intrinsic already exists in block.rs.

- [ ] **Step 3: Verify and test**

`cargo test test_harness_125` — all merge metadata tests pass.

**Verification:** All existing merge/deep-merge tests still pass. New metadata tests pass.

---

## Chunk 3: MergeWith Wrapper

### Task 3: Restructure MergeWith wrapper to preserve metadata

**Files:**
- Modify: `src/eval/stg/block.rs` (MergeWith::wrapper, lines 1204–1267)

MergeWith is the most complex wrapper (3 args: `[l, r, f]`). It's used internally by DeepMerge.

**Metadata rule for MergeWith:** Since MergeWith is primarily an internal mechanism (DeepMerge passes itself as `f`), the metadata handling should follow the same pattern as Merge — RHS metadata replaces LHS for shallow semantics. When called from DeepMerge, the DeepMerge wrapper handles metadata separately before calling MergeWith on the unwrapped bodies.

- [ ] **Step 1: Assess whether MergeWith needs metadata handling at all**

If DeepMerge always strips metadata before calling MergeWith (which it will after Chunk 2's restructuring), then MergeWith may not need its own metadata handling. Check whether MergeWith is exposed to users directly or only used via DeepMerge.

If MergeWith is only internal: skip metadata handling, document that it operates on bare blocks.

If MergeWith is user-accessible: add demeta wrapping following the same pattern as Merge.

- [ ] **Step 2: Implement if needed**

Follow the same demeta pattern as Chunk 1, but with 3 args instead of 2. The index arithmetic is more complex:
- `lambda(3, [l r f])` → demeta l → `[l_meta, l_body, l, r, f]` etc.
- The function `f` reference must be adjusted in all branches

- [ ] **Step 3: Test**

Add MergeWith-specific tests if it's user-accessible.

**Verification:** All merge tests pass. DeepMerge still works correctly (since it calls MergeWith).

---

## Implementation Order

1. **Chunk 1** (Merge) — simplest case, establishes the pattern
2. **Chunk 2** (DeepMerge) — builds on Chunk 1, adds recursive metadata merging
3. **Chunk 3** (MergeWith) — assess necessity first, may be skippable

Each chunk is independently testable and committable.

## Risk Assessment

- **High risk:** De Bruijn index arithmetic errors — these produce subtle runtime bugs (wrong values, not crashes). Mitigation: write out full environment stacks for each branch, test thoroughly.
- **Medium risk:** Performance — additional `demeta` checks add evaluation steps even when no metadata is present. The `or_else` branch of `demeta` should be cheap (no allocation). Acceptable for correctness.
- **Low risk:** Breaking existing behaviour — the `or_else` branches should replicate current behaviour exactly for non-metadata blocks.
