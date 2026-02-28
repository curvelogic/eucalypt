# Monadic Blocks

## Summary

Allow block declarations to be desugared as monadic bind chains
instead of let-bindings, using unit metadata tagged `:monad` to
specify the bind and return operations. User-defined Unicode bracket
pairs can be assigned to specific monads for concise syntax.

## Motivation

Eucalypt's random number functions currently require manual stream
threading:

```eucalypt
result: {
  r1: random-int(6, io.random)
  r2: random-int(6, r1.rest)
  r3: random-int(6, r2.rest)
  dice: [r1.value, r2.value, r3.value]
}
```

Each call returns `{ value: ..., rest: stream }` and the programmer
must manually pass `rest` to the next call. This is the classic
problem that monads solve — sequencing computations with hidden
plumbing.

With monadic blocks:

```eucalypt
three-dice: ⟦
  a: random-int(6)
  b: random-int(6)
  c: random-int(6)
⟧.[a, b, c]
```

The stream threading is invisible. Each declaration binds the *value*;
the stream flows automatically via the monad's bind operation.

## Design

### 1. Monad Specification

A monad is specified as a block with `:monad` unit metadata containing
`bind` and `return` declarations:

```eucalypt
{ :monad  bind: rand-bind  return: rand-return }
```

This is completely static and regular eucalypt syntax — an initial
expression in a block is unit metadata, and the declarations are
ordinary key-value pairs. The desugarer recognises the `:monad` tag
and extracts the `bind` and `return` names.

### 2. Inline Usage (Block Metadata)

Any block can be made monadic by attaching a monad specification as
unit metadata:

```eucalypt
{ ` { :monad  bind: list-bind  return: list-return }
  x: [1, 2, 3]
  y: [10, 20]
  z: x + y }.[x, y, z]
```

The backtick attaches the monad spec as metadata to the block. The
desugarer sees the `:monad` tag and desugars the declarations as bind
chains.

### 3. Bracket Pair Assignment

A Unicode bracket pair can be assigned to a specific monad via a
property declaration using an empty block destructure `{}` as the
parameter:

```eucalypt
⟦{}⟧: { :monad  bind: rand-bind  return: rand-return }
```

The `{}` in the definition signals that this bracket pair expects
**block content** (declarations). The parser uses the definition's
parameter shape to determine parse mode:

- **Block parameter** (`⟦{}⟧:`) → block mode (declarations with
  colons, currently only monadic)
- **List parameter** (`⟦[x : xs]⟧:`) → expression mode (idiot
  brackets, spaces collect items into a list)
- **Plain parameter** (`⟦xs⟧:`) → expression mode (idiot brackets)

This cleanly resolves the parse-time disambiguation problem: the
parser inspects the bracket definition to determine how to parse
content within the brackets.

The `{}` parameter is forward-compatible — a future non-monadic block
bracket override would also use `{}` but with a different body (no
`:monad` tag).

Bracket pairs follow operator scoping rules — defined in any block
scope, visible in the defining scope and nested scopes.

### 4. Desugaring

A monadic block's declarations are desugared to nested bind calls.
The generalised lookup expression after the block becomes the return
expression.

**Source:**

```eucalypt
⟦ a: action1
  b: action2(a)
  c: action3(a, b)
⟧.expr
```

**Desugars to:**

```
bind(action1, λa.
  bind(action2(a), λb.
    bind(action3(a, b), λc.
      return(expr))))
```

Where `bind` and `return` are the names from the monad specification
and `expr` is the generalised lookup expression evaluated in scope of
all the bindings.

### 5. Generalised Lookup as Return Expression

The dot-expression after a monadic block specifies the result. It is
evaluated in the scope of all the block's bindings and implicitly
wrapped in the monad's `return`:

```eucalypt
# Single binding value
⟦ ... ⟧.x

# List of values
⟦ ... ⟧.[a, b, c]

# Computed expression
⟦ ... ⟧.(a + b)

# Block of values
⟦ ... ⟧.{sum: a + b  product: a * b}
```

This uses eucalypt's existing generalised lookup syntax — `block.name`
for field access and `block.(expr)` for expression evaluation in block
scope.

### 6. Static Constraint

The monad specification must be syntactically determinable at desugar
time. The desugarer inspects the block metadata (or bracket pair
definition) and extracts `bind` and `return` as AST names. These names
are emitted as unresolved `Name` references and resolved later during
cook/verify.

This means:
- The monad spec must be a literal block with `:monad` metadata
- The `bind` and `return` values must be names (or dotted paths)
- Computed or conditional monad specs are not supported

## Examples

### List Monad

The list monad represents non-deterministic computation — each
declaration draws from all values in a list.

```eucalypt
# Define bind and return for list monad
list-bind(xs, f): xs mapcat(f)
list-return(x): [x]

# Assign bracket pair
⟦{}⟧: { :monad  bind: list-bind  return: list-return }

# Usage: all combinations of x and y
combinations: ⟦
  x: [1, 2, 3]
  y: [10, 20]
⟧.[x, y]

# Desugars to:
#   list-bind([1, 2, 3], λx.
#     list-bind([10, 20], λy.
#       list-return([x, y])))
#
# Result: [[1, 10], [1, 20], [2, 10], [2, 20], [3, 10], [3, 20]]

# Filtered combinations
pythagorean-triples(n): ⟦
  a: range(1, n)
  b: range(a, n)
  c: range(b, n)
  ok: if(a * a + b * b = c * c, [true], [])
⟧.[a, b, c]
```

### Random Monad

The random monad threads an infinite random stream through a sequence
of random operations. A "random action" is a function
`stream → { value: result, rest: stream }`.

```eucalypt
# Define bind and return for random monad
rand-bind(action, f): (stream): {
  r: action(stream)
  next: f(r.value)(r.rest)
  value: next.value
  rest: next.rest
}

rand-return(x): (stream): { value: x  rest: stream }

# Lift existing random functions (they already have the right shape)
# random-int(n) returns stream → { value: ..., rest: ... }
# so random-int(6) is already a random action

# Assign bracket pair
⟦{}⟧: { :monad  bind: rand-bind  return: rand-return }

# Roll three dice
three-dice: ⟦
  a: random-int(6)
  b: random-int(6)
  c: random-int(6)
⟧.[a, b, c]

# Run it
three-dice(io.random).value   #=> [3, 1, 5] (or similar)

# More complex: roll until we get a six
# (would need recursive monadic action — future work)

# Shuffle and sample
random-hand(deck, n): ⟦
  shuffled: shuffle(deck)
  hand: sample(n, shuffled)
⟧.hand
```

## Implementation Strategy

### Desugarer Changes

The desugarer already processes block declarations. When it encounters
a block with `:monad` metadata:

1. Extract `bind` and `return` names from the metadata block
2. Identify the generalised lookup expression (the dot-expression)
3. Desugar declarations right-to-left into nested bind calls
4. Wrap the lookup expression in `return`
5. Emit the nested application as the block's value

### Bracket Pair Integration

Monadic bracket pairs share the same lexer and parser infrastructure
as idiot brackets (Unicode Ps/Pe recognition). The distinction is in
the definition's parameter shape:

- **Block parameter** (`⟦{}⟧:`) → parse contents as declarations
  (block mode)
- **List or plain parameter** (`⟦[...]⟧:` or `⟦xs⟧:`) → parse
  contents as expressions (expression mode)

The parser inspects the bracket definition before parsing the
contents, using the parameter shape to determine the parse mode.
Any bracket defined with `{}` parses its contents as block
declarations; all other definitions parse as expression lists.

### Pipeline Position

Monadic block desugaring happens during the desugar phase, alongside
regular block desugaring. The emitted `bind` and `return` name
references are resolved during cook and verified normally.

```
Parse → Desugar (monadic blocks expanded here) → Cook → ...
```

No new core-to-core pass needed — the desugaring produces standard
core expressions (applications, lambdas, names).

## Dependencies

- **Idiot brackets design** (`2026-02-28-idiot-brackets-design.md`):
  Shares the Unicode bracket pair lexer/parser infrastructure.
- **Destructuring design** (`2026-02-27-destructuring-named-args-design.md`):
  The generalised lookup expression (`.expr`) and list destructuring
  may be used in return expressions and bind functions.

## Error Handling

- **Missing bind or return in monad spec**: desugar-time error when
  `:monad` block lacks required declarations
- **Monadic block without lookup**: desugar-time error — the result
  expression is required
- **Undefined bracket pair**: error when brackets are used but no
  definition is in scope
- **Non-static monad spec**: desugar-time error when bind/return
  cannot be extracted as names

## Testing

Harness tests covering:

- List monad: combinations, filtering
- Random monad: stream threading, multiple draws
- Inline metadata (no bracket pair)
- Bracket pair assignment and usage
- Various return expressions (`.name`, `.[list]`, `.(expr)`)
- Nested monadic blocks (same and different monads)
- Scoping of bracket pair definitions
- Error cases (missing bind/return, no lookup, undefined brackets)
