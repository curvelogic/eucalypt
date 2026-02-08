# DCE Inside Blocks — Design

## Problem

Dead code elimination currently marks ALL block members as reachable. For
namespace-style blocks like `str` and `cal`, this means every function is
retained even when only a few are used. This bloats the compiled program.

## Solution

Static-access DCE — track which block members are accessed via static dot
syntax (`ns.member`) and eliminate unreferenced members when only static
access patterns are used.

## Key Properties

- **Conservative** — only eliminate when provably safe
- **Static access only** — `ns.member` patterns, not dynamic `lookup`
- **Namespace focus** — greatest benefit for prelude-style blocks
- **Opt-out available** — `--no-dce` flag for debugging
- **No new errors** — purely an optimisation, same semantics

## Syntax Examples

```eu
# Before DCE: both `used` and `unused` retained
ns: { used: 42, unused: 99 }
result: ns.used

# After DCE: only `used` retained
ns: { used: 42 }
result: ns.used
```

## Safe vs Unsafe Patterns

**Safe patterns (DCE applies):**

| Pattern | Example | Reasoning |
|---------|---------|-----------|
| Static dot access | `ns.member` | Known member at compile time |
| Chained access | `ns.foo.bar` | Still statically resolvable |
| Binding | `let x: ns.member` | Static access in binding |

**Unsafe patterns (preserve all members):**

| Pattern | Example | Reasoning |
|---------|---------|-----------|
| Dynamic lookup | `ns lookup(key)` | Key unknown at compile time |
| Iteration | `ns elements` | All members potentially accessed |
| Merge | `ns merge(other)` | Block structure modified |
| Passed as argument | `f(ns)` | Unknown usage in callee |
| Block as value | `ns` alone | Entire block escapes |

## Implementation

**Location**: Extend `src/core/simplify/prune.rs`

**New component**: `BlockAccessTracker`

```rust
struct BlockAccessTracker {
    /// Blocks that have ONLY static access patterns
    static_only: HashSet<LocalId>,
    /// For static-only blocks: which members are accessed
    accessed_members: HashMap<LocalId, HashSet<String>>,
    // Note: uses String keys (pre-interning identifiers in the core
    // AST). SymbolId is a runtime/VM concept; DCE runs at compile time
    // before symbols are interned.
}
```

**Algorithm**:

1. **First pass**: Identify blocks and track access patterns
   - Mark block as "static-only" initially
   - On `block.member` access: record member in `accessed_members`
   - On dynamic access: remove block from `static_only` set

2. **Second pass**: Mark reachability
   - For static-only blocks: only mark accessed members as reachable
   - For other blocks: mark all members reachable (current behaviour)

**Integration with existing DCE**:

The current `prune.rs` has `mark_block_reachable` which marks ALL members.
Add a check:

```rust
fn mark_block_reachable(&mut self, block_id: LocalId) {
    if self.tracker.is_static_only(block_id) {
        // Only mark accessed members
        for member in self.tracker.accessed_members(block_id) {
            self.mark_reachable(member);
        }
    } else {
        // Current behaviour: mark all members
        for member in block.all_members() {
            self.mark_reachable(member);
        }
    }
}
```

## Edge Cases

**Nested blocks**: `outer.inner.member` — track access through the full
chain. If `outer` is static-only and `inner` is accessed, then check if
`inner` is also static-only.

**Shadowing**: `let ns: other` — stop tracking original `ns` from that
point. The shadowed binding may have different access patterns.

**Re-export**: `{ x: ns.foo }` — `foo` is marked as used in `ns`. Other
members of `ns` can be eliminated if no other access exists.

## Testing

**Correctness tests** (harness):
- Programs produce same results with DCE enabled
- Dynamic access patterns preserve all members

```eu
# Static access - DCE safe
test-dce-static: { ns: { used: 42, unused: 99 } result: ns.used RESULT: 42 }

# Dynamic access - must preserve all
test-dce-dynamic: {
  ns: { a: 1, b: 2 }
  key: :a
  result: ns lookup(key)
  RESULT: 1
}

# Block escapes - must preserve all
test-dce-escape: {
  ns: { x: 1, y: 2 }
  result: ns elements length
  RESULT: 2
}
```

**Verification**: Correctness tests plus code review. A `--debug-dce` flag
can be added later to print eliminated members if needed.

## Error Handling

- No new errors — DCE is purely an optimisation
- If analysis is uncertain, default to preserving members (safe fallback)
- Feature flag `--no-dce` disables optimisation for debugging

## Out of Scope

- DCE for dynamically accessed blocks (would need runtime analysis)
- Cross-module DCE (only same compilation unit)
- Metrics/statistics collection (avoid performance impact)
- Automatic verification of elimination (trust + code review)

## Dependencies

None — can be implemented independently.
