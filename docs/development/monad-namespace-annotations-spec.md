# Explicit Record Type Annotations on Monad Namespaces

**Status**: Spec  
**Bead**: eu-meir  
**Date**: 2026-05-02

## 1. Overview

Annotate `for`, `io`, `random`, and `state` namespace declarations
with full record types declaring the types of all combinator fields.
Uses the `!` asserted annotation prefix since the `monad()` call
returns `{..}` and the checker cannot verify the body.

`let` is excluded — its bind is identity application and its types
are all trivially `any`.

## 2. Annotations

### 2.1 `for` (M = [_])

```eu
` { doc: "..."
    monad: "[a]"
    type: "!{{bind: [a] → (a → [b]) → [b], return: a → [a], map: (a → b) → [a] → [b], then: [a] → [b] → [b], and-then: (a → [b]) → [a] → [b], join: [[a]] → [a], sequence: [[a]] → [[a]], map-m: (a → [b]) → [a] → [[b]], filter-m: (a → [bool]) → [a] → [[a]]}}" }
for: monad({bind(m, f): m mapcat(f), return(v): [v]})
```

### 2.2 `io` (M = IO(_))

```eu
` { doc: "..."
    monad: "IO(a)"
    type: "!{{bind: IO(a) → (a → IO(b)) → IO(b), return: a → IO(a), map: (a → b) → IO(a) → IO(b), then: IO(a) → IO(b) → IO(b), and-then: (a → IO(b)) → IO(a) → IO(b), join: IO(IO(a)) → IO(a), sequence: [IO(a)] → IO([a]), map-m: (a → IO(b)) → [a] → IO([b]), filter-m: (a → IO(bool)) → [a] → IO([a]), ..}}" }
io: monad{...} { ... }
```

Note: `io` has additional fields (env, shell, exec, etc.) beyond the
monad combinators, so the record type is open (`..`).

### 2.3 `random` (M = stream → {value: a, rest: stream})

The random monad is a state monad over PRNG streams.  Let
`R(a)` stand for `stream → {{value: a, rest: stream}}`:

```eu
` { doc: "..."
    monad: "stream → {{value: a, rest: stream}}"
    type: "!{{bind: (stream → {{value: a, rest: stream}}) → (a → stream → {{value: b, rest: stream}}) → stream → {{value: b, rest: stream}}, return: a → stream → {{value: a, rest: stream}}, ..}}" }
random: monad{...} { ... }
```

The full expansion of all combinators is very verbose.  Annotate
`bind`, `return`, and leave the record open (`..`) for the derived
combinators and extra fields (`stream`, `int`, `float`, etc.).

### 2.4 `state` (M = s → {value: a, state: s})

Same structure as random.  In `lib/state.eu`:

```eu
` { doc: "..."
    monad: "state → {{value: a, state: state}}"
    type: "!{{bind: ({{..}} → {{value: a, state: {{..}}}}) → (a → {{..}} → {{value: b, state: {{..}}}}) → {{..}} → {{value: b, state: {{..}}}}, return: a → {{..}} → {{value: a, state: {{..}}}}, ..}}" }
state: monad{...} { ... }
```

State is a block (`{..}`) rather than a named type, so the
annotations use open records.

## 3. Implementation Notes

- All annotations use `!` prefix (asserted) since `monad()` returns
  `{..}` and the body cannot be verified
- `io` and `random` have extra fields beyond the monad combinators;
  their record types are open (`..`)
- `for` has no extra fields; its record type can be closed
- Type variables `a`, `b` are freshened per use as normal
- The `monad:` metadata field is unchanged — it declares the wrapper
  type for binding checks (eu-ggr9), separate from the record type

## 4. Acceptance Criteria

1. `eu check lib/prelude.eu` — zero warnings
2. `eu check lib/state.eu` — zero warnings (with prelude)
3. All existing harness tests pass
4. After annotation, the checker knows `for.bind` has type
   `[a] → (a → [b]) → [b]` — hover in LSP shows it
5. After annotation, `for.bind(42, identity)` warns
   "expected [a], found number"
6. After annotation, `io.return("hello")` has type `IO(string)`
7. `for.map` has type `(a → b) → [a] → [b]`

## 5. Files Changed

| File | Change |
|------|--------|
| `lib/prelude.eu` | Add `type:` annotations to `for`, `io` declarations |
| `lib/state.eu` | Add `type:` annotation to `state` declaration |

## 6. Dependencies

None — this is pure annotation work on existing infrastructure.
The `!` asserted annotation prefix already exists.  Benefits
increase once record type propagation (eu-z9zz.6) lands, as
`for.bind` lookups will resolve the field type from the record.
