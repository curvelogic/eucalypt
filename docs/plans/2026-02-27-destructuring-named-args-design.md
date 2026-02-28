# Destructuring and Named Arguments

## Summary

Add destructuring in function parameters for blocks and lists, a cons
operator (`‖`) for list construction in expressions, and juxtaposed
call syntax (`f{...}`, `f[...]`) that together give eucalypt named
arguments as an emergent feature with no new concept.

## Motivation

When functions receive structured data (blocks or lists), extracting
fields currently requires binding the whole argument and using property
access:

```eucalypt
process(data): {
  x: data.x
  y: data.y
  result: x + y
}.result
```

Destructuring in parameters removes this boilerplate:

```eucalypt
process({x y}): x + y
```

Combined with juxtaposed call syntax, this gives named arguments for
free:

```eucalypt
process{x: 1 y: 2}   # named argument call
```

## Design

### 1. Block Destructuring

Extract named fields from a block argument. Field names become local
bindings. Commas are optional (as in block literals).

```eucalypt
# Extract fields by name
f({x y z}): x + y + z

# Rename: field: local-binding (same direction as block literals)
f({x: a  y: b}): a + b

# Mixed shorthand and rename
f({x  y: b}): x + b
```

Runtime error if a required field is missing from the argument.

### 2. List Destructuring

Match a list argument by position. Commas are required (as in list
literals).

```eucalypt
# Fixed length — matches exactly three elements
f([a, b, c]): a + b + c
```

**Head/tail splitting** uses `:` inside brackets to separate fixed
elements from the tail binding:

```eucalypt
# Head and tail
f([x : xs]): x

# Multiple fixed elements + rest
f([a, b : rest]): a + b

# Single-element match
f([x : _]): x
```

Colon is already a syntax character in eucalypt (not an operator), so
its use here is consistent with its role in blocks (`key: value`) and
block patterns (`field: binding`). Context (`{}` vs `[]`) always
disambiguates:

- Block literal: `{x: 1}` — key: value
- Block pattern: `{x: a}` — field: binding
- List pattern: `[x : xs]` — head: tail

Runtime error if the list is too short for the pattern.

### 3. Cons Operator `‖` (U+2016)

A new right-associative operator for list construction in expressions.
Defined in the prelude as a regular operator (like `+` or `*`).

```eucalypt
1 ‖ [2, 3]        #=> [1, 2, 3]
1 ‖ 2 ‖ [3]       #=> [1, 2, 3]
```

`‖` has **no role in patterns** — list destructuring uses `:` inside
brackets (Section 2). This avoids the library/language dual-nature
problem: `‖` is purely a prelude operator, `:` is purely syntax.

### 4. Juxtaposed Call Syntax

Eucalypt already distinguishes `a(b)` (application) from `a (b)`
(catenation). Extend this to blocks and lists: juxtaposing a function
with a block or list literal (no space) is sugar for passing it as a
single argument.

```eucalypt
f{x: 1  y: 2}    # sugar for f({x: 1  y: 2})
f[1, 2, 3]        # sugar for f([1, 2, 3])
```

**Backwards compatibility**: This is theoretically breaking — existing
code where an identifier is immediately followed by `{` or `[` would
change meaning. A search of all library code, harness tests, and
benchmarks found zero occurrences of this pattern (only string
interpolation like `"{data.name}"` which is lexically distinct). The
risk is effectively nil for known eucalypt code.

### 5. Named Arguments (Emergent)

Combining juxtaposed call syntax with block destructuring gives named
arguments with no new language concept:

```eucalypt
greet({name greeting}): "{greeting}, {name}!"
greet{name: "Greg"  greeting: "Hello"}   #=> "Hello, Greg!"
```

Positional and named arguments cannot be mixed — a function either
takes positional args `f(a, b)` or a single block arg `f({x y})`.
This is an acceptable trade-off for conceptual simplicity.

### 6. Call-Site / Callee Fusion

When the compiler can see that a call site constructs a block (or
list) literal and the callee immediately destructs it in its
parameters, the intermediate data structure is elided entirely. Values
are bound directly — no allocation, no lookups at runtime.

```eucalypt
# The compiler sees both sides:
greet({name greeting}): "{greeting}, {name}!"
greet{name: "Greg"  greeting: "Hello"}

# Fused to equivalent of:
#   let name = "Greg"; greeting = "Hello" in "{greeting}, {name}!"
```

This is a required deliverable, sequenced after basic destructuring
and lowering are working.

## Implementation Strategy

### Pipeline Position

Destructuring patterns are represented as first-class forms in the
core expression language and lowered in a core-to-core pass, keeping
the STG compiler unchanged.

```
Parse → Desugar → Cook → Verify → Simplify → Transform → Inline
                                                            ↓
                                              [Pattern lowering pass]
                                                            ↓
                                                      STG compile
```

### Phase 1: Parse

Recognise destructuring patterns in function parameter positions:

- `{x y}` and `{x: a  y: b}` — block patterns
- `[a, b, c]` — fixed-length list patterns
- `[x : xs]` and `[a, b : rest]` — head/tail list patterns
- `f{...}` and `f[...]` — juxtaposed call syntax (desugared to
  `f({...})` and `f([...])` in the parser or desugar phase)

### Phase 2: Core Representation

Add new core expression forms for destructuring parameters:

- `DestructureBlock(fields)` — where each field is a key name and
  optional local binding name
- `DestructureList(elements)` — fixed-length positional bindings
- `DestructureList(heads, tail)` — fixed prefix plus tail binding

These appear in lambda parameter positions alongside simple name
bindings.

### Phase 3: Core-to-Core Lowering

A dedicated core-to-core pass that:

1. **Fuses** call sites where a block/list literal is passed to a
   destructuring parameter — elide construction, bind values directly
2. **Lowers** remaining (unfused) patterns to mechanical expansions:
   - Block fields → `.key` lookups
   - List positions → `!! index` indexing
   - List head/tail → `!! 0` etc. for heads, `drop(n, ...)` for tail

After this pass, no destructuring patterns remain in the core — the
STG compiler sees only plain lambdas.

### Sequencing

1. Parse destructuring patterns and represent in core
2. Implement mechanical lowering (expand to lookups/indexing)
3. Add `‖` as expression-level cons operator in prelude
4. Add juxtaposed call syntax
5. Implement call-site/callee fusion in the lowering pass
6. Harness tests throughout

## Error Handling

- **Missing block field**: runtime error with diagnostic naming the
  missing field
- **List too short**: runtime error when list has fewer elements than
  the pattern requires
- **Empty list on head/tail**: runtime error when head/tail pattern
  applied to empty list

## Testing

Harness tests covering:

- Block destructuring with and without renaming
- List destructuring (fixed length)
- List destructuring (head/tail with `:`)
- Cons `‖` as expression operator
- Juxtaposed call syntax
- Named argument pattern (call + destructure)
- Fusion (verify no intermediate block allocation where possible)
- Error cases (missing fields, short lists, empty head/tail)
- Interaction with existing features (anaphora, pipelines, metadata)
