# Unified Stream Producers

**Status**: Spec  
**Bead**: eu-0a1w  
**Date**: 2026-04-19

## 1. Overview

Replace the import-specific `StreamProducer` / `StreamTable` /
`STREAM_NEXT` mechanism with a unified `LazyProducer` trait and
`ProducerTable`. All handle-based lazy producers (file imports, future
IO streams) register in one table and use one BIF (`PRODUCER_NEXT`)
to build memoised lazy cons cells.

Rename `Native::Stream(u64)` to `Native::Prng(u64)` — the pure PRNG
state stays separate from handle-based producers.

User-visible behaviour is unchanged — imports still produce lazy lists.

## 2. LazyProducer Trait

New trait replacing `StreamProducer`:

```rust
/// A lazy producer of STG values, accessed via a handle in ProducerTable.
pub trait LazyProducer {
    /// Produce the next value, or None if exhausted.
    fn next(&mut self) -> Option<Rc<StgSyn>>;

    /// Whether this producer is pure (same state → same output).
    /// Pure producers can safely be forked/shared in future.
    /// Import and IO producers are not pure.
    fn is_pure(&self) -> bool { false }
}
```

Existing implementations (`JsonlProducer`, `CsvProducer`,
`TextProducer`) implement `LazyProducer` instead of `StreamProducer`.
The trait method signature is identical — `next(&mut self) ->
Option<Rc<StgSyn>>` — so migration is mechanical.

## 3. ProducerTable

Replaces `StreamTable`. Same thread-local pattern:

```rust
pub struct ProducerTable {
    handles: HashMap<u32, Rc<RefCell<Box<dyn LazyProducer>>>>,
    next_id: u32,
}

thread_local! {
    static PRODUCER_TABLE: RefCell<ProducerTable> = RefCell::new(ProducerTable::new());
}

pub fn register_producer(producer: Box<dyn LazyProducer>) -> u32;
pub fn producer_next(handle: u32) -> Option<Rc<StgSyn>>;
```

## 4. Native Type Changes

### Rename: Native::Stream → Native::Prng

```rust
// Before
Native::Stream(u64)

// After
Native::Prng(u64)
```

All references updated: `stream_arg()` → `prng_arg()`, pattern
matches, Display impl, GC scanning. The PRNG intrinsics
(`STREAM_NEW`, `STREAM_VALUE`, `STREAM_ADVANCE`, `STREAM_FLOAT`,
`STREAM_INT`, `STREAM_SPLIT`) are unchanged in behaviour — only the
Native variant name changes. The intrinsic names stay as `STREAM_*`
to avoid breaking the prelude.

### New: Native::Producer

```rust
Native::Producer(u32)  // handle into ProducerTable
```

A u32 handle — no heap pointers, no GC scanning needed (same as the
u64 in Prng). The ProducerTable owns the actual producer via
`Rc<RefCell<Box<dyn LazyProducer>>>`.

## 5. PRODUCER_NEXT BIF

Replaces `STREAM_NEXT`. Same proven cons-cell building pattern:

```rust
pub struct ProducerNext;

impl StgIntrinsic for ProducerNext {
    fn name(&self) -> &str { "PRODUCER_NEXT" }

    fn execute(&self, ..., args: &[Ref]) -> Result<(), ExecutionError> {
        let handle = num_arg(..., &args[0])? as u32;
        match producer_next(handle) {
            Some(value_syn) => {
                // Build: Let([value, tail_thunk], Cons(ListCons, [L(0), L(1)]))
                // - value: non-updatable binding holding current element
                // - tail_thunk: updatable thunk calling PRODUCER_NEXT(handle)
                // Update continuation memoises — producer advances at most once
                // per cons cell position
            }
            None => {
                // Exhausted — return Nil
            }
        }
    }

    fn inlinable(&self) -> bool { false }  // side-effecting
}
```

The cons-cell construction logic is copied from the current
`StreamNext` implementation. The only change is the function name
and the handle lookup going through `ProducerTable` instead of
`StreamTable`.

## 6. Import Registration Path

Current path:
```
driver/source.rs: load_stream()
  → import/mod.rs: create_stream_import(format, path)
    → creates JsonlProducer / CsvProducer / TextProducer
    → register_stream(Box::new(producer)) → u32 handle
  → returns core expr: App(Bif("STREAM_NEXT"), [Num(handle)])
```

New path:
```
driver/source.rs: load_stream()
  → import/mod.rs: create_stream_import(format, path)
    → creates JsonlProducer / CsvProducer / TextProducer
    → register_producer(Box::new(producer)) → u32 handle
  → returns core expr: App(Bif("PRODUCER_NEXT"), [Num(handle)])
```

Mechanical change — only the function name and BIF name differ.

## 7. Files Changed

| File | Change |
|------|--------|
| `src/eval/memory/syntax.rs` | Rename `Stream(u64)` → `Prng(u64)`, add `Producer(u32)`. Update Display, GC scan, mark_ref_heap_pointers (Producer has no heap pointers). |
| `src/eval/stg/stream.rs` | Replace `StreamProducer` trait with `LazyProducer`. Replace `StreamTable` with `ProducerTable`. Rename public functions. |
| `src/eval/stg/stream_intrinsic.rs` | Replace `StreamNext` with `ProducerNext`. Update BIF registration. |
| `src/eval/stg/stream_prng.rs` | Update `stream_arg()` → `prng_arg()`, `Native::Stream` → `Native::Prng` in all intrinsics. |
| `src/eval/stg/support.rs` | Rename `stream_arg()` → `prng_arg()`. |
| `src/eval/stg/mod.rs` | Update intrinsic registration: `PRODUCER_NEXT` replaces `STREAM_NEXT`. |
| `src/eval/intrinsics.rs` | Update BIF name mapping. |
| `src/import/stream.rs` | `impl LazyProducer for JsonlProducer/CsvProducer/TextProducer` (was `StreamProducer`). |
| `src/import/mod.rs` | `register_producer()` instead of `register_stream()`. BIF name `PRODUCER_NEXT`. |
| `src/driver/source.rs` | Call `register_producer()`, reference `PRODUCER_NEXT`. |
| `src/eval/stg/debug.rs` | Update display for `Native::Prng`. |
| `src/eval/stg/assert.rs` | Update display for `Native::Prng`. |
| `src/eval/stg/expect.rs` | Update display for `Native::Prng` if referenced. |
| `src/eval/types.rs` | Update `IntrinsicType` if `Stream` is referenced. |
| `src/common/sourcemap.rs` | Update BIF name mapping if `STREAM_NEXT` is listed. |
| `lib/prelude.eu` | No changes — prelude uses `__STREAM_NEW` etc. which still work (intrinsic names unchanged). |

## 8. What Does NOT Change

- PRNG intrinsic names: `STREAM_NEW`, `STREAM_VALUE`, `STREAM_ADVANCE`,
  `STREAM_FLOAT`, `STREAM_INT`, `STREAM_SPLIT` — all unchanged
- Prelude random namespace — unchanged
- User-visible import behaviour — unchanged (imports produce lazy lists)
- Random `as-list` bridge — unchanged
- `is_stream_format()` in import — unchanged (or renamed to
  `is_producer_format()` for consistency)

## 9. Testing

### Acceptance criteria

1. `cargo test --test harness_test` — all tests pass (imports and
   random streams work as before)
2. Specifically verify streaming import tests if any exist, or create
   a test with `jsonl-stream` format
3. `cargo test --lib` — unit tests pass
4. Random stream tests (harness tests using `random.*`) pass

### Manual verification

```sh
# Streaming JSONL import
echo '{"a":1}\n{"a":2}' > /tmp/test.jsonl
echo '{ import: "jsonl-stream@/tmp/test.jsonl" } main: map(_.a)' > /tmp/test.eu
eu /tmp/test.eu
# Expected: [1, 2]

# Random streams still work
eu -e 'random.stream(42) random.as-list take(5)'
# Expected: list of 5 floats
```

## 10. Future: Adding New Producer Types

To add a new lazy producer (e.g. streaming IO):

```rust
struct IoLineProducer {
    reader: BufReader<ChildStdout>,
}

impl LazyProducer for IoLineProducer {
    fn next(&mut self) -> Option<Rc<StgSyn>> {
        let mut line = String::new();
        match self.reader.read_line(&mut line) {
            Ok(0) => None,
            Ok(_) => Some(dsl::box_str(line.trim_end())),
            Err(_) => None,
        }
    }
}

// Registration:
let handle = register_producer(Box::new(IoLineProducer { reader }));
// Returns lazy list via PRODUCER_NEXT(handle)
```

One trait implementation, one registration call. No new Native
variants, no new BIFs, no GC changes.
