# Lazy Streams and Random Number Facilities

Design for lazy streaming infrastructure and random number generation in eucalypt.

## Overview

Eucalypt is purely functional, so exposing randomness as a function breaks referential transparency. Instead, we model randomness as an **infinite lazy list** of floats — a value, not an effect. Functions that need randomness consume from the stream and return the unconsumed remainder.

This design uses **two separate architectures** for the two features:

1. **Random numbers**: Pure functional — two arithmetic BIFs + prelude recursive lazy list. No mutable state, no runtime changes. Leverages existing thunk memoisation.
2. **Streaming file imports**: Native::Stream with `Rc<RefCell<Box<dyn StreamProducer>>>` — mutable state needed for IO-backed producers. Follows existing `Native::Index(Rc<BlockIndex>)` pattern for off-heap Rc types.

## Part 1: Random Numbers

### API

#### Prelude

```
io.random                      # Infinite stream, seeded from system entropy (or --seed)

random-stream(seed)            # Explicit seeding for reproducibility

random-int(n, stream)          -> {value: 0..n-1, rest: stream}
random-choice(list, stream)    -> {value: element, rest: stream}
shuffle(list, stream)          -> {value: shuffled-list, rest: stream}
sample(n, list, stream)        -> {value: n-elements, rest: stream}
```

#### CLI

```
eu --seed 12345 myfile.eu      # Reproducible io.random
```

### Architecture

#### Pure Functional PRNG

Random numbers use **no mutable state**. The PRNG state is a plain integer, threaded through the prelude as a value:

```
random-stream(seed): cons(__PRNG_FLOAT(seed), random-stream(__PRNG_NEXT(seed)))
```

This produces an infinite lazy list because:
1. `cons(head, tail)` creates a list cell where `tail` is a thunk
2. The recursive `random-stream(__PRNG_NEXT(seed))` is not evaluated until `tail` is forced
3. STG thunk memoisation ensures each position is computed at most once
4. No special runtime support needed — standard lazy evaluation handles everything

#### Intrinsics (BIFs)

Only two new built-in functions, both pure arithmetic:

| Intrinsic | Args | Returns | Algorithm |
|-----------|------|---------|-----------|
| `__PRNG_NEXT` | seed (int) | Next seed state (int) | SplitMix64 state transition |
| `__PRNG_FLOAT` | seed (int) | Float in [0,1) | SplitMix64 output → double |

**SplitMix64** was chosen because:
- Single u64 state (fits in eucalypt's existing integer representation)
- Simple arithmetic (no arrays, no SIMD)
- Good statistical quality for non-cryptographic use
- Widely used as a seed generator (used by Java's SplittableRandom)

#### --seed CLI Flag

- Add `--seed <integer>` to `src/driver/options.rs` (clap argument)
- Wire into `create_io_pseudoblock()` in `src/driver/io.rs` as `__io.RANDOM_SEED`
- If `--seed` not provided, generate from `std::time::SystemTime::now()` epoch nanos
- The seed is a regular integer value, available at runtime as `io.RANDOM_SEED`

#### Prelude Functions

```eucalypt
` "Infinite lazy stream of random floats in [0,1), seeded by the given integer."
random-stream(seed): cons(__PRNG_FLOAT(seed), random-stream(__PRNG_NEXT(seed)))

` "Infinite lazy stream of random floats, seeded from system entropy or --seed flag."
io.random: random-stream(io.RANDOM_SEED)

` "Generate a random integer in [0, n) from the stream. Returns {value, rest}."
random-int(n, stream): {
  value: floor(stream head * n)
  rest: stream tail
}

` "Choose a random element from a list. Returns {value, rest}."
random-choice(list, stream): {
  r: random-int(list length, stream)
  value: list nth(r.value)
  rest: r.rest
}

` "Shuffle a list using Fisher-Yates. Returns {value, rest}."
shuffle(list, stream): __shuffle-impl(list, list length, stream)

` "Sample n elements from a list without replacement. Returns {value, rest}."
sample(n, list, stream): {
  shuffled: shuffle(list, stream)
  value: shuffled.value take(n)
  rest: shuffled.rest
}
```

The `__shuffle-impl` helper implements Fisher-Yates iteratively, consuming one random value per swap.

### Files Changed

**Modified:**
- `src/eval/machine/intrinsic.rs` — Register `__PRNG_NEXT`, `__PRNG_FLOAT`
- `src/eval/intrinsics.rs` — BIF implementations (SplitMix64 arithmetic)
- `src/driver/options.rs` — `--seed` CLI arg
- `src/driver/io.rs` — `__io.RANDOM_SEED` in pseudo-block
- `lib/prelude.eu` — random-stream, io.random, random-int, random-choice, shuffle, sample
- `harness/test/077_random.eu` (or next available number) — harness tests

### Dependency Order

1. `eu-cxom`: PRNG BIFs (`__PRNG_NEXT`, `__PRNG_FLOAT`)
2. `eu-mbps`: CLI `--seed` flag + `__io.RANDOM_SEED` (parallel with step 1)
3. `eu-qghh`: Prelude functions + `io.random` (depends on steps 1 + 2)
4. `eu-64eq`: Harness tests (depends on step 3)

---

## Part 2: Streaming File Imports

### API

```
eu jsonl-stream@large.jsonl    # Lazy line-by-line JSON processing
eu csv-stream@data.csv         # Lazy row-by-row CSV processing
eu text-stream@log.txt         # Lazy line-by-line text processing
eu text-stream@-               # Lazy streaming from stdin
```

### Architecture

#### Why Mutable State is Needed

Unlike random numbers, file streaming requires genuine mutable IO state:
- A `BufReader` cursor advances as lines are read
- A CSV parser maintains internal state between rows
- Stdin is inherently sequential

This cannot be modelled purely functionally like PRNG, so we use a different approach.

#### StreamProducer Trait

Core abstraction in `src/eval/stg/stream.rs`:

```rust
/// A producer that yields values lazily from an IO source
trait StreamProducer: Send + Sync {
    /// Produce next value, or None if exhausted
    fn next(&mut self) -> Option<RcExpr>;
}
```

No `fork()` method — streams are inherently single-consumer (you can't rewind a file cursor or stdin). If forking is needed later, it can be added.

#### Native::Stream Variant

Add to `src/eval/memory/syntax.rs`:

```rust
enum Native {
    Sym(SymbolId),
    Str(RefPtr<HeapString>),
    Num(Number),
    Zdt(DateTime),
    Index(Rc<BlockIndex>),
    Set(RefPtr<HeapSet>),
    Stream(Rc<RefCell<Box<dyn StreamProducer>>>),  // NEW
}
```

This follows the existing `Rc<BlockIndex>` pattern. The `Rc` lives off-heap, so:
- GC evacuation (byte-level memcpy) is safe — the Rc pointer is copied, refcount is correct
- No special GC scanning needed (no heap pointers inside)
- `RefCell` provides interior mutability for advancing the producer

#### Stream Handle Table

Producers are stored in a handle table on the VM side:

```rust
// In the VM or machine context
struct StreamTable {
    handles: HashMap<u32, Rc<RefCell<Box<dyn StreamProducer>>>>,
    next_id: u32,
}
```

- Import phase creates a producer, registers it in the table, gets a handle ID
- The handle ID is embedded in the initial thunk's STG code
- `__STREAM_NEXT(handle)` intrinsic looks up the handle to advance the producer

#### __STREAM_NEXT Intrinsic

```rust
// Intrinsic execution
fn execute_stream_next(handle: u32, table: &StreamTable) -> StgValue {
    let producer = table.get(handle);
    match producer.borrow_mut().next() {
        Some(value) => Cons(value, thunk_calling_stream_next(handle)),
        None => Nil,
    }
}
```

The tail thunk is a standard STG thunk containing a call to `__STREAM_NEXT(handle)`. When forced, it advances the producer again. Thunk update semantics ensure each position is forced at most once, so the producer advances exactly once per list position.

#### Concrete Producers

| Producer | Source | Yields | Finite? |
|----------|--------|--------|---------|
| `JsonlProducer` | `BufReader<File>` | Parsed JSON value per line | Yes (EOF) |
| `CsvProducer` | `csv::Reader<File>` | Block with column-name keys per row | Yes (EOF) |
| `TextProducer` | `BufReader<File>` or `BufReader<Stdin>` | String per line | Yes (EOF/Ctrl-D) |

#### Import System Integration

New format specifiers in `src/import/mod.rs`:

```
jsonl-stream  →  JsonlProducer  →  register in handle table  →  thunk(__STREAM_NEXT(handle))
csv-stream    →  CsvProducer    →  register in handle table  →  thunk(__STREAM_NEXT(handle))
text-stream   →  TextProducer   →  register in handle table  →  thunk(__STREAM_NEXT(handle))
```

Existing eager imports (`jsonl`, `csv`, `text`) remain unchanged.

### Files Changed

**New:**
- `src/eval/stg/stream.rs` — StreamProducer trait, concrete producers

**Modified:**
- `src/eval/memory/syntax.rs` — `Native::Stream` variant
- `src/eval/machine/intrinsic.rs` — Register `__STREAM_NEXT`
- `src/eval/intrinsics.rs` — `__STREAM_NEXT` implementation
- `src/eval/machine/vm.rs` — StreamTable in VM context
- `src/import/mod.rs` — `*-stream` format dispatch
- `src/import/jsonl.rs` — `JsonlProducer` (lazy variant alongside existing eager)
- `src/import/csv.rs` — `CsvProducer`
- `harness/test/078_streams.eu` (or next available number) — harness tests

### Dependency Order

1. `eu-4yb6`: Stream infrastructure (trait, Native::Stream, handle table)
2. `eu-ln9r`: `__STREAM_NEXT` intrinsic + thunk construction (depends on step 1)
3. `eu-5rsn`: JSONL producer + format dispatch (depends on step 2)
4. `eu-fh73`: CSV producer (parallel with step 3, depends on step 2)
5. `eu-g6d6`: Text/stdin producer (parallel with steps 3-4, depends on step 2)
6. `eu-0g85`: Harness tests (depends on steps 3-5)

---

## Error Handling

**Fail fast** — errors terminate the stream immediately rather than returning inline error values. Consumers rarely handle inline errors properly.

| Scenario | Behaviour |
|----------|-----------|
| Invalid seed | Treat as `seed.abs() as u64` or hash to u64 |
| Malformed JSONL line | Error terminates stream |
| File deleted mid-stream | Error on next `tail` force |
| Stdin EOF | Stream returns `Nil` (finite) |
| CSV parse error | Error terminates stream |

## Testing

### Unit Tests (Rust)

- SplitMix64 yields deterministic sequence for known seed
- `__PRNG_FLOAT` output is in [0, 1)
- `JsonlProducer` parses lines correctly
- `CsvProducer` yields blocks with correct keys
- `TextProducer` yields lines as strings
- Stream thunk memoisation (same head on repeat access)

### Harness Tests (eucalypt)

**Random (077_random.eu or similar):**
- `random-stream(42) head` is deterministic
- `random-stream(42) head` is in [0,1)
- `random-int(10, io.random).value` is in [0,9]
- `shuffle([1,2,3], io.random).value length` is 3
- `sample(2, [1,2,3,4], io.random).value length` is 2
- `--seed 42` produces same results across runs
- Different seeds produce different sequences

**Streaming (078_streams.eu or similar):**
- `jsonl-stream@file` processes lines lazily, yields correct count
- `csv-stream@file` yields row blocks with correct keys
- `text-stream@file` yields correct line count
- Streams terminate with empty list at EOF
- Malformed JSONL line produces error

## Team Assignment

| Feature | Agent | Worktree | Beads |
|---------|-------|----------|-------|
| Random Numbers | Callum | eucalypt-prelude | eu-778o (epic), eu-cxom, eu-mbps, eu-qghh, eu-64eq |
| Streaming Imports | Niamh | eucalypt-vm | eu-n50 (epic), eu-4yb6, eu-ln9r, eu-5rsn, eu-fh73, eu-g6d6, eu-0g85 |
| Review (both) | Seren | eucalypt | PR review gate |
