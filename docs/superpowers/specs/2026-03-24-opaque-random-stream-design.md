# Opaque Splittable Random Stream

## Summary

Replace the current cons-list random stream with an opaque Rust-native `Stream` type backed by SplitMix64. The stream is consumable from both Rust intrinsics and eucalypt prelude code, uses a single PRNG family throughout, supports deterministic splitting, and enables efficient reservoir sampling and shuffling.

## Motivation

The current random stream is a lazy eucalypt cons-list: `cons(__PRNG_FLOAT(seed), random-stream(__PRNG_NEXT(seed)))`. This has two problems:

1. **Rust intrinsics cannot consume it** — intrinsics can only navigate already-evaluated data, not force lazy thunks. `vec.sample` and `vec.shuffle` work around this by pre-extracting a known number of floats into a list, but algorithms like reservoir sampling need an unknown number of random values.

2. **Performance** — traversing a 10,000-element cons-list for `count` alone takes >10 seconds. Building a vec from a large list is dominated by the list traversal cost, not the vec construction.

## Design

### Native type

A new `Native` variant:

```rust
Native::Stream(u64)  // SplitMix64 state
```

Stored on the GC heap alongside existing native types (Vec, Set). Opaque to eucalypt — no list operations, no direct access to the state. Interacted with only through the `random.*` API.

### SplitMix64 PRNG

Implemented in `src/eval/stg/splitmix.rs` (~30 lines, no external dependencies):

- `next(state) → state'` — advance the state
- `output(state) → u64` — generate output from current state
- `float(state) → (f64, state')` — generate float in [0, 1), advance
- `int(n, state) → (i64, state')` — generate int in [0, n), advance
- `split(state) → (state_a, state_b)` — fork into two independent streams using output mixing

Split derives two new states deterministically from the current state, producing statistically independent sub-sequences.

### Intrinsics

| Intrinsic | Signature | Description |
|-----------|-----------|-------------|
| `__STREAM_NEW` | `int → Stream` | Create stream from integer seed |
| `__STREAM_FLOAT` | `Stream → [float, Stream]` | Consume one float in [0,1) |
| `__STREAM_INT` | `int, Stream → [int, Stream]` | Consume one int in [0,n) |
| `__STREAM_SPLIT` | `Stream → [Stream, Stream]` | Fork into two independent streams |

All consuming intrinsics return a two-element list `[value, advanced_stream]`.

### Prelude API

The `random` namespace is rewritten to use Stream natives:

```eucalypt
random-ret(v, stream): { value: v, rest: stream }

random-bind(m, f, stream): {
  r: m(stream)
  run: f(r.value)(r.rest)
  value: run.value
  rest: run.rest
}

` { doc: "..." monad: true }
random: monad{bind: random-bind, return: random-ret} {
  stream(seed): __STREAM_NEW(seed)

  float(stream): { s: __STREAM_FLOAT(stream)  value: s first  rest: s second }

  int(n, stream): { s: __STREAM_INT(n, stream)  value: s first  rest: s second }

  split(stream): { s: __STREAM_SPLIT(stream)  value: s first  rest: s second }

  # Derived combinators (unchanged API):
  choice(lst, stream): { ... }
  shuffle(lst): ...
  sample(n, lst): ...
  # etc.
}
```

The monadic API is unchanged from the user's perspective: `random.int(6)`, `random.float`, `random.sequence([...])` etc. all work as before with the same `{value, rest}` pattern.

### Splitting policy

Splitting is always explicit in eucalypt code. The monadic `bind` threads the stream linearly (as today). Eucalypt code calls `random.split` when it needs to fork a stream. Intrinsics that consume from a stream advance it and return the remainder — they do not split internally.

### Migration: vec.sample / vec.shuffle / set.sample

These intrinsics change from taking a pre-extracted float list to taking a Stream directly:

**Before:**
```eucalypt
# Wrapper pre-extracts floats via SeqNumList
vec.sample(n, vec, stream) — stream is a list of floats
```

**After:**
```eucalypt
# Intrinsic consumes from Stream directly in Rust
vec.sample(n, vec, stream) — stream is an opaque Stream native
```

The `execute` method reads the `Native::Stream(state)` from the arg, generates the needed random values by advancing the state in Rust, builds the result, and returns `[sampled_vec, Stream(advanced_state)]`.

The `SeqNumList` wrappers currently used to deep-force the float list before `vec.sample`/`vec.shuffle` are no longer needed for these intrinsics. Their wrappers simplify to force the vec argument and pass the stream through.

### New intrinsic: reservoir sampling

```
__LIST_RESERVOIR_SAMPLE(k, list, stream) → [sampled_list, stream']
```

Walks the (SeqList-forced) cons-list in Rust, consuming from the Stream at each step. Returns a k-element list sampled uniformly. O(n) time, O(k) space, single pass.

The list must be pre-forced (by a SeqList wrapper, same pattern as `vec.of`). The intrinsic traverses WHNF cons-cells with `navigate_local` — no thunk forcing needed.

Exposed in the prelude as:
```eucalypt
random.sample-list(k, list, stream): {
  s: __LIST_RESERVOIR_SAMPLE(k, list, stream)
  value: s first
  rest: s second
}
```

### What's removed

- `__PRNG_NEXT(seed)` and `__PRNG_FLOAT(seed)` intrinsics
- `random-stream(seed): cons(...)` prelude definition
- `io.random` as a lazy cons-list (becomes `random.stream(io.RANDOM_SEED)`)
- `SeqNumList` wrappers on `vec.sample` and `vec.shuffle` (replaced by direct Stream consumption)

### What's preserved

- The entire monadic API surface: `random.int`, `random.float`, `random.choice`, `random.shuffle`, `random.sample`, `random.sequence`, `random.map-m`
- The `{value, rest}` return pattern
- Deterministic reproducibility from a given seed
- `random.stream(42)` as the entry point

### Backward compatibility

**Breaking**: code that treats `random.stream(seed)` as a cons-list (`head`/`tail`, `take`, direct list operations) will fail — the stream is no longer a list.

**Non-breaking**: code using the monadic API (`random.int(6)(stream).value`, `{ :random a: random.int(6) }`, etc.) works unchanged.

### Testing

- Deterministic output: `random.int(6)(random.stream(42)).value` must produce a consistent value
- Split independence: two split streams produce different sequences
- Split determinism: splitting the same stream always produces the same sub-streams
- `vec.sample` and `vec.shuffle` produce correct results with the new stream
- Reservoir sampling produces uniform samples
- Existing test 120 (`random_monad`) migrated to new API
- Existing test 130 (`vec`) sampling tests migrated
