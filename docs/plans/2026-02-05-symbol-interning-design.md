# Symbol Interning — Design

## Problem

Symbols are stored as individual heap strings. The same symbol appearing N
times results in N separate heap allocations. Comparison requires full
string comparison.

## Solution

Symbol interning — a per-machine pool where each unique symbol string is
stored once, referenced by a compact `SymbolId(u32)`.

## Key Properties

- **Per-machine pool** — owned by VM, clear lifetime, no thread safety
- **Interning at heap load** — loader interns instead of allocating
- **`Native::Sym(SymbolId)`** — symbols are just IDs, strings live in pool
- **Fast comparison** — `id1 == id2` instead of string comparison
- **Pool structure** — `HashMap<String, SymbolId>` + `Vec<String>`

## Benefits

- Memory: each unique symbol stored once
- Speed: integer comparison vs string comparison
- Enables: block indexing (eu-brj) with `HashMap<SymbolId, usize>`

## Data Structures

**SymbolId** — newtype wrapper for type safety:

```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SymbolId(u32);
```

**SymbolPool** — owned by the machine:

```rust
pub struct SymbolPool {
    /// String → ID lookup for interning
    to_id: HashMap<String, SymbolId>,
    /// ID → String lookup for resolving
    to_str: Vec<String>,
}

impl SymbolPool {
    pub fn intern(&mut self, s: &str) -> SymbolId {
        if let Some(&id) = self.to_id.get(s) {
            return id;
        }
        let id = SymbolId(self.to_str.len() as u32);
        self.to_str.push(s.to_string());
        self.to_id.insert(s.to_string(), id);
        id
    }

    pub fn resolve(&self, id: SymbolId) -> &str {
        &self.to_str[id.0 as usize]
    }
}
```

**Native::Sym change**:

```rust
// Before
Native::Sym(RefPtr<HeapString>)

// After
Native::Sym(SymbolId)
```

## Integration Points

### Loader (`src/eval/memory/loader.rs`)

Currently allocates HeapString for each symbol. Change to intern:

```rust
// Before
stg::syntax::Native::Sym(s) => {
    let ptr = self.alloc(HeapString::from_str(mem, s.as_str()))?;
    memory::syntax::Ref::V(memory::syntax::Native::Sym(ptr))
}

// After
stg::syntax::Native::Sym(s) => {
    let id = symbol_pool.intern(s.as_str());
    memory::syntax::Ref::V(memory::syntax::Native::Sym(id))
}
```

### Equality (`src/eval/stg/eq.rs`)

Currently compares strings. Change to integer comparison:

```rust
// Before
(Native::Sym(sx), Native::Sym(sy)) => str_eq(view, sx, sy)

// After
(Native::Sym(id_x), Native::Sym(id_y)) => id_x == id_y
```

### Display and errors

Anywhere that needs the symbol string calls `pool.resolve(id)`. This
includes error messages, rendering, and debugging output.

### Block key matching

`MatchesKey` in block.rs compares SymbolIds directly instead of string
comparison.

## Machine Ownership

The `SymbolPool` is owned by the machine/evaluator:

```rust
pub struct Machine {
    // ... existing fields
    symbol_pool: SymbolPool,
}
```

Pool access via `IntrinsicMachine` trait — add `symbol_pool(&self) ->
&SymbolPool` and `symbol_pool_mut(&mut self) -> &mut SymbolPool`
methods. Intrinsics already receive the machine reference, so no
signature changes needed. Intrinsics that don't use symbols are
unaffected.

## Testing

**Correctness**: Existing harness tests pass unchanged.

**Interning**: Same symbol interned twice returns same ID.

**Resolve round-trip**: `pool.resolve(pool.intern("foo")) == "foo"`.

**Comparison**: Two symbols with same string have equal IDs.

**Memory**: Verify single allocation per unique symbol.

## Benchmarking

- Memory usage before/after on symbol-heavy programs
- Symbol comparison performance
- Block lookup performance (benefits from fast key comparison)

## Out of Scope

- Thread-safe pool (single-threaded execution)
- Garbage collection of unused symbols (pool lives with machine)
- Interning at parse time (would require larger changes)
