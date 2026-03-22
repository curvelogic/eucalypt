# Vecs

Vecs are compact, indexed collections of primitive values (numbers, strings,
symbols) that provide O(1) indexed access and efficient slicing. They are
designed for large datasets where random access matters.

## Construction

| Function | Description |
|----------|-------------|
| `vec.of` | Convert a list of primitives to a vec |
| `vec.to-list` | Convert a vec back to a cons-list |

```eu
v: [1, 2, 3, 4, 5] vec.of
l: v vec.to-list          # [1, 2, 3, 4, 5]
```

## Indexed Access

| Function | Description |
|----------|-------------|
| `vec.len` | Return the number of elements |
| `vec.nth(n)` | Return element at 0-based index `n` (error if out of bounds) |
| `vec.slice(from, to)` | Return sub-vec `[from, to)`, indices clamped to length |

```eu
v: [10, 20, 30, 40, 50] vec.of
n: v vec.len              # 5
x: v vec.nth(2)           # 30
s: v vec.slice(1, 4)      # vec of [20, 30, 40]
```

## Sampling and Shuffling

These are random monad actions — they accept a random stream and return
a `value`/`rest` block. Use within a `:random` block or call directly
with a stream.

| Function | Description |
|----------|-------------|
| `vec.sample(n, v, stream)` | Pick `n` elements without replacement |
| `vec.shuffle(v, stream)` | Return a new vec with all elements in random order |

```eu,notest
v: [1, 2, 3, 4, 5] vec.of
sampled: vec.sample(2, v, random.stream(42)).value
shuffled: vec.shuffle(v, random.stream(99)).value
```

## Notes

- Vecs store primitive values only (numbers, strings, symbols); blocks and
  lists cannot be stored in a vec.
- Vecs render as YAML/JSON sequences (same as lists).
- Use `vec.of` to convert an existing list, and `vec.to-list` to convert back.
