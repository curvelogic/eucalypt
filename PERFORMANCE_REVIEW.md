# Performance Review

## Summary

This review examines the eucalypt codebase (~44,500 lines of Rust) for performance issues. The codebase implements a data templating language with an STG (Spineless Tagless G-machine) execution model and Immix-inspired garbage collection. While the overall architecture is sound, several areas could benefit from optimization, particularly around allocation patterns in hot paths and algorithmic complexity in core data structures.

The most significant issues are: (1) GC scanning allocates a new Vec for every object during collection, (2) environment variable access has O(depth) complexity, and (3) block/object key lookup uses linear search. These compound to create performance challenges at scale.

## Critical Issues

(P0 - Must fix before merge)

No critical issues requiring immediate fix were identified. The codebase is functional and correctly implements its design.

## Major Issues

(P1 - Should fix before merge)

### 1. GC Scan Allocates Vec Per Object
**File:** `src/eval/memory/collect.rs:48-52`
**Impact:** During garbage collection, every object's `scan()` method allocates a new `Vec<ScanPtr>`. For a heap with millions of objects, this creates millions of temporary allocations during GC pauses.

```rust
fn scan<'a>(&'a self, scope: &'a dyn CollectorScope, marker: &mut CollectorHeapView<'a>) -> Vec<ScanPtr<'a>>;
```

**Suggested fix:** Use a pre-allocated work buffer passed to `scan()` or use a bump allocator for scan results. Alternatively, implement an iterator-based API to avoid allocation entirely.

### 2. O(depth) Environment Frame Traversal
**File:** `src/eval/machine/env.rs:220-229`
**Impact:** Every local variable access traverses the environment chain. The `cell()` function is called from `get()` (line 238) which is invoked during every `Ref::L(index)` resolution in the VM. For deeply nested closures, this creates O(depth) overhead per variable access.

```rust
fn cell(&self, guard: &dyn MutatorScope, idx: usize) -> Option<(Array<C>, usize)> {
    let len = self.bindings.len();
    if idx < len {
        Some((self.bindings.clone(), idx))
    } else {
        match self.next {
            Some(ref env) => (*ScopedPtr::from_non_null(guard, *env)).cell(guard, idx - len),
            None => None,
        }
    }
}
```

**Suggested fix:** Consider flattening environments or caching computed indices. The `bindings.clone()` on line 223 is also concerning as it clones the Array on every access.

### 3. Linear Search for Block Key Lookup
**File:** `src/eval/stg/block.rs:462-491`
**Impact:** The `LookupOr` intrinsic (block field access) performs a linear search through key-value pairs. For blocks with many fields, this is O(n) per access. Repeated lookups (common in data transformation pipelines) compound to O(n*m) where m is the number of accesses.

```rust
let find = lambda(4, // [list k d find]
    case(local(0), vec![
        (DataConstructor::ListCons.tag(), // [h t] [list k d find]
            switch(MatchesKey.global(lref(0), lref(3)), vec![
                (DataConstructor::BoolTrue.tag(), ExtractValue.global(lref(0))),
                (DataConstructor::BoolFalse.tag(), app(lref(5), vec![...])  // recursive
            ])
        ),
        ...
    ])
);
```

**Suggested fix:** Consider using a hash map representation for blocks with more than a threshold number of keys, or implement a sorted representation with binary search.

### 4. LinkedList Block Management with O(n) Removal
**File:** `src/eval/memory/heap.rs:437-458`
**Impact:** `find_best_recycled_block` iterates through all recycled blocks (O(n)), then reconstructs the entire LinkedList to remove a single element (another O(n)). This occurs on every head block replacement when the current block is exhausted.

```rust
fn find_best_recycled_block(&mut self, allocation_size: usize) -> Option<BumpBlock> {
    // O(n) scan to find best block
    for (index, block) in self.recycled.iter().enumerate() { ... }

    // O(n) reconstruction to remove the found block
    let mut remaining = LinkedList::new();
    while let Some(block) = self.recycled.pop_front() {
        if current_index == index {
            target_block = Some(block);
        } else {
            remaining.push_back(block);
        }
        current_index += 1;
    }
    self.recycled = remaining;
}
```

**Suggested fix:** Use `Vec` instead of `LinkedList` with `swap_remove` for O(1) removal, or maintain a priority queue based on suitability score.

## Minor Issues

(P2 - Nice to fix)

### 5. Redundant Key Cloning in Regex Cache
**File:** `src/eval/stg/string.rs:45-58`
**Impact:** The regex caching function clones the key string twice when inserting a new entry.

```rust
let key = text.as_ref().to_string();  // First allocation
if !rcache.contains(&key) {
    let re = Regex::new(text.as_ref()).map_err(|_| ExecutionError::BadRegex(key.clone()))?;
    rcache.put(key.clone(), re);  // Second clone
}
```

**Suggested fix:** Use `entry` API or restructure to avoid double cloning.

### 6. String Operations Create Intermediate Allocations
**File:** `src/eval/stg/string.rs` (various)
**Impact:** String intrinsics like `Join`, `Match`, `Split`, and `Letters` create intermediate `Vec<String>` collections. The `Letters` intrinsic (line 432) is particularly expensive as it allocates one String per character.

```rust
let letters: Vec<String> = string.chars().map(|c| iter::once(c).collect()).collect();
```

**Suggested fix:** Consider lazy evaluation or streaming where possible. For `Letters`, consider returning indices or a custom iterator rather than materializing all characters.

### 7. VM Stack Trace Allocates During Error Handling
**File:** `src/eval/machine/vm.rs:563-581`
**Impact:** The `stack_trace` function allocates intermediate structures during error handling. While errors should be rare, this adds overhead when they occur.

```rust
pub fn stack_trace(&self, view: &MutatorHeapView) -> Vec<Smid> {
    self.stack.iter().rev()
        .filter_map(|cont| { ... })
        .filter(|smid| *smid != Smid::default())
        .dedup()
        .collect()
}
```

### 8. GC Sweep Moves All Blocks Between Lists
**File:** `src/eval/memory/heap.rs:519-531`
**Impact:** The sweep phase pops all blocks from `rest`, checks recyclability, and pushes to either `recycled` or `unusable`. This is O(blocks) with poor cache locality due to LinkedList traversal.

```rust
pub fn sweep(&mut self) {
    let mut unusable: LinkedList<BumpBlock> = LinkedList::default();
    while let Some(mut block) = self.rest.pop_front() {
        if block.recycle() {
            self.recycled.push_back(block);
        } else {
            unusable.push_back(block);
        }
    }
    self.rest.append(&mut unusable);
}
```

## Observations

(Non-blocking notes and suggestions)

### Positive Patterns

- **LRU Regex Cache:** The regex caching in `vm.rs:141` using `lru::LruCache` is a good pattern that prevents repeated regex compilation.
- **Block-based GC:** The Immix-inspired design with 32KB blocks and line maps is solid for memory locality.
- **Bump Allocation:** O(1) bump allocation for small objects is efficient.
- **GC Check Frequency:** Checking GC policy every 500 steps (`vm.rs:800`) balances responsiveness with overhead.

### Scale Considerations

**At 10x scale:**
- Block key lookup becomes noticeable (100 keys * 10 accesses = 1000 comparisons)
- Environment depth starts to matter for heavily nested code

**At 100x scale:**
- GC scan allocation overhead becomes significant (100K objects = 100K Vec allocations per GC)
- LinkedList block management degrades

**At 1000x scale:**
- All identified issues compound significantly
- Memory fragmentation from scan allocations may trigger additional GCs
- Consider moving to generational or incremental collection

### Missing Optimizations

1. **No object pooling:** Frequently allocated types (closures, environment frames) could benefit from object pooling.
2. **No inline caching:** Repeated block key lookups could use inline caches (common in JS engines).
3. **No tail call optimization verification:** Ensure TCO is happening where expected to prevent stack growth.
4. **LinkedList vs Vec:** The codebase uses `LinkedList` for block management, but `Vec` often performs better due to cache locality.

### Performance vs Readability

The current code appropriately trades performance for readability in most places. The STG machine implementation is clean and maintainable. The main areas where more aggressive optimization would be warranted are the hot paths identified above: GC scanning, variable access, and block lookup.
