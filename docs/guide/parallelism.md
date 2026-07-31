# Parallel Evaluation

> **HIGHLY EXPERIMENTAL.** This feature is new, and its semantics may change
> without notice in a future release.
>
> **macOS/Linux only.** The mechanism is a POSIX `fork()`, so it is not
> available on Windows. `par-*` still works there — every call falls back to
> the ordinary sequential form, exactly as it does below the size threshold on
> a supported platform — it just never forks, so there is no parallelism
> speed-up, and `EU_PP_TRACE` has no effect on Windows. See [What is different
> from `map`](#what-is-different-from-map) below: the value is identical
> either way.

Eucalypt can evaluate a map over independent elements in parallel worker
processes. The vocabulary is small and deliberately boring: each combinator
computes the **same value** as its sequential form, so adding one is a
performance decision, never a semantic one.

| Parallel | Same value as |
|----------|---------------|
| `xs par-map(f)` | `xs map(f)` |
| `xs par-sum(f)` | `xs map(f) sum` |
| `xs par-max(f)` | `xs map(f) max-of` |
| `xs par-min(f)` | `xs map(f) min-of` |
| `xs par-concat(f)` | `xs map(f) concat` |

```eu,notest
scores: candidates par-map(expensive-score)
best:   candidates par-max(expensive-score)
```

Results come back in input order, always. The mechanism is deterministic:
each worker is given a contiguous range of indices and the parent reassembles
the ranges in order, so two runs of the same program produce byte-identical
output.

## When it helps

Parallel evaluation pays when `f` is genuinely expensive and there are many
elements. Below a threshold (1024 elements by default) eucalypt does not
bother: it runs the sequential map instead, because forking costs more than
it saves on a small list. It also stays sequential when only one core is
available, on platforms without `fork`, and inside hosts that have not
declared themselves safe to fork from — the language server, the WASM build,
and the test harness all run `par-*` sequentially. The answer is the same in
every case.

## What is different from `map`

Three things. They are the whole contract.

**`par-*` is strict.** `map` is lazy: it builds a list of thunks and forces
only what the consumer looks at. `par-*` forces the spine of `xs` and every
result before returning. So:

- an infinite or unbounded list works with `map` and does not with `par-map`;
- an error in `f` that `map` would never have reached — because nothing looked
  at that element — is raised by `par-map`;
- taking the head of a parallel map costs the whole map.

```eu,notest
g(x): if(x = 3, panic("boom"), x)

[1, 2, 3] map(g) head        # 1 — the third element is never forced
[1, 2, 3] par-map(g) head    # error: boom
```

**`f` must be pure.** Workers run in separate processes, so a side effect in
`f` happens in a child and is lost, and two workers sharing a file descriptor
would each advance the other's read position. Eucalypt catches the case it can
see — advancing a lazy impure producer, such as a streaming import, inside a
worker raises a boundary error rather than corrupting the stream — but it
cannot detect every impurity, so keep `f` pure.

**Results must be data.** Only the serialisable subset crosses between
processes: numbers, strings, symbols, booleans, null, lists of those, and
blocks of those. A function, an IO action or an opaque value (a set, a vector,
a PRNG, a producer handle) is a boundary error naming the value's kind and the
combinator, not a silent misinterpretation:

```
par-map: cannot serialise a function across the parallel boundary
```

That is a programmer error at the boundary and it is reported as one, on the
sequential fallback path too, so a program cannot pass in testing and fail
only when it grows large enough to fork.

Metadata travels with its value, so a `:suppress` or `:doc` annotation on a
result still applies after a `par-map` exactly as it does after a `map`. The
consequence of carrying it rather than discarding it is that metadata must be
data too: metadata holding a function raises the same boundary error. `map`
would have carried that function along unevaluated, so this is a real
difference — but the alternative is dropping the annotation silently, which
would let a `par-map` re-expose a key its author marked internal, with nothing
to show for it. Values compare equal with or without metadata, so nothing else
would notice.

## Tuning and diagnosis

Because every parallel path has a sequential fallback that produces the
identical answer, a build that silently never forks looks exactly like one
that does. `EU_PP_TRACE=1` says which path each call took:

```
par-map: 20000 elements — forked 13 workers
par-sum: 12 elements — sequential (below threshold)
```

`EU_PP_WORKERS` sets the worker count and `EU_PP_THRESHOLD` the element count
below which `par-*` stays sequential — chiefly useful for forcing one path or
the other while investigating. See
[Debugging](../development/debugging.md) for the full list.
