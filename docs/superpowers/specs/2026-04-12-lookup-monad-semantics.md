# Lookup and Monadic Block Semantics

**Bead**: eu-461s
**Status**: Design
**Date**: 2026-04-12

## 1. Lookup Semantics

### 1.1 Simple Lookup

**Syntax**: `A.name` — dot followed by a bare identifier.

**Semantics**: Key lookup restricted to the block's own bindings. The
name is resolved as a symbol key within the block. Outer scope is
never consulted. May be resolved at compile time or runtime depending
on whether `A` is a known block literal.

```eu,notest
{ x: 1, y: 2 }.x        # => 1 (key :x found)
{ x: 1 }.y               # => key error (no :y in block)
dyn-block.x               # => runtime key lookup of :x
```

### 1.2 Generalised Lookup

**Syntax**: `A.{ block }`, `A.(expr)`, `A."string"`, `A.[list]` —
dot followed by any delimited expression (not a bare identifier).

**Semantics**: The RHS expression is evaluated in a scope where `A`'s
bindings are visible, shadowing outer scope. Names not found in `A`
fall through to the enclosing scope. The result of evaluating the
RHS is the result of the lookup.

```eu,notest
y: 5
{ x: 1 }.(x + y)         # => 6 (x from block, y from outer)
{ x: 1 }.{ sum: x + y }  # => { sum: 6 }
{ x: 1 }."x is {x}"      # => "x is 1"
{ x: 1 }.[x, y]           # => [1, 5]
```

### 1.3 Syntactic Rule

The dot operator `.` is left-associative at precedence 90. The
distinction between simple and generalised lookup is purely syntactic
— determined by the form of the RHS:

| RHS form | Lookup type | Scope |
|----------|-------------|-------|
| `name` | Simple | Block bindings only |
| `{ block }` | Generalised | Block bindings + outer scope |
| `(expr)` | Generalised | Block bindings + outer scope |
| `"string"` | Generalised | Block bindings + outer scope |
| `[list]` | Generalised | Block bindings + outer scope |

### 1.4 Left-Associative Chaining

`A.B.C` is always `(A.B).C`. Scopes do not accumulate — each `.`
evaluates its RHS in the scope of the LHS result only.

```eu,notest
{ x: 1 }.{ a: x + 10 }.{ r: a + 1 }.(r)   # => 12
# (a is in scope, x is NOT — only a's block's bindings are visible)

{ x: 1 }.{ a: x + 10 }.{ r: a + x }.(r)   # key error: x not found
# scopes don't accumulate through the chain
```

## 2. Monadic Blocks in Lookup Chains

### 2.1 Standalone Monadic Block

When a monadic block is NOT the RHS of a dot, it is desugared with
explicit or implicit return as usual:

```eu,notest
# Explicit return — .(expr) is the return expression
{ :let x: 1, y: 2 }.(x + y)                # => 3

# Block return — .{ block } is the return expression
{ :let x: 1, y: 2 }.{ sum: x + y }          # => { sum: 3 }

# Implicit return — synthesised block of non-underscore bindings
{ :let x: 1, y: 2 }                          # => { x: 1, y: 2 }
```

### 2.2 Monadic Block as Generalised Lookup RHS

When a monadic block appears as the RHS of `.`, it is a generalised
lookup expression. The monadic block is evaluated in the scope of the
LHS block's bindings, and **always uses implicit return**.

```eu,notest
{ x: 1 }.{ :let y: x + 10 }
# generalised lookup: :let block sees x from LHS
# implicit return: { y: 11 }
```

Any subsequent `.` is a lookup on the monadic result, NOT a return
expression:

```eu,notest
{ x: 1 }.{ :let y: x + 10 }.(y)
# parses as: (({ x: 1 }).{ :let y: x + 10 }).(y)
# monadic result: { y: 11 } (implicit return)
# .(y) is generalised lookup: 11

{ x: 1 }.{ :let y: x + 10 }.y
# parses as: (({ x: 1 }).{ :let y: x + 10 }).y
# monadic result: { y: 11 }
# .y is simple lookup: 11
```

### 2.3 Why Implicit Return

Left-associativity requires that `.` always separates the monadic
block from what follows. The monadic block in lookup position cannot
"see past" the dot to consume a return expression — that would
violate left-associativity. Implicit return is the only consistent
choice.

To use an explicit return with a monadic block in lookup position,
wrap in parens:

```eu,notest
{ x: 1 }.(({ :let y: x + 10 }.(y + 100)))   # => 111
# parens force the whole monadic block+return into one expression
```

### 2.4 Applicability Across All Monads

These rules apply uniformly to all monads: `:let`, `:io`, `:random`,
`:state`, and any user-defined monads.

```eu,notest
# :random in lookup position
{ n: 100 }.{ :random d: random.int(n) }
# generalised lookup: random block sees n from LHS
# implicit return: action returning { d: <random> }

# :state in lookup position
{ init: {} }.{ :state _: state.put(:x, 42) }
# generalised lookup: state block sees init from LHS
# implicit return: action returning { ... }

# :io in lookup position
{ cmd: "echo hello" }.{ :io r: io.shell(cmd) }
# generalised lookup: io block sees cmd from LHS
# implicit return: io action returning { r: <result> }
```

## 3. Bugs to Fix

### 3.1 Static `.name` Outer Scope Leak

**Current behaviour**: `{ x: 1 }.y` where `y = 5` in outer scope
gives `5`. The compiler desugars `.name` on a static block literal as
generalised lookup (scope resolution), allowing outer scope to leak
through.

**Required behaviour**: `.name` is always simple lookup — restricted
to block bindings. `{ x: 1 }.y` should be a key error if the block
has no `:y` key, regardless of whether `y` exists in outer scope.

**Location**: The compiler's handling of `.name` on block literals
in the desugarer. It should emit a key lookup (as it does for
dynamic values) not a scope-resolution let binding.

### 3.2 Monadic Block Escapes Generalised Lookup

**Current behaviour**: `A.{ :monad ... }` where `A` is any
expression — the monadic block is desugared independently, not as
the generalised lookup RHS of `A`. Bindings from `A` are not
visible inside the monadic block.

**Required behaviour**: `.{ :monad ... }` is generalised lookup.
The monadic block is desugared within the lookup scope of `A`,
seeing `A`'s bindings.

**Affected cases** (all monads):
- `{ x: 1 }.{ :let y: x + 10 }` — static block LHS
- `dyn-block.{ :let y: x + 10 }` — dynamic value LHS
- `make(5).{ :let y: x + 10 }` — function call LHS

**Location**: Soup desugaring in `rowan_ast.rs`. The monadic block
detection (around line 2135) fires before the generalised lookup
chain is recognised. When a monadic block is preceded by `.` in the
soup, it should be desugared within the generalised lookup lambda.

### 3.3 `.(expr)` Lost After Monadic Block Return

**Current behaviour**: `{ :monad ... }.{ block }.(expr)` — the
monadic return consumes `.{ block }` but `.(expr)` is silently
dropped. Result is the block, not the lookup result.

**Required behaviour**: Left-associative. The monadic block in
standalone position (not in lookup RHS) has `.{ block }` as its
return expression. Then `.(expr)` is a separate generalised lookup
on the result.

`{ :let x: 1 }.{ a: x }.(a)` should give `1`:
- `({ :let x: 1 }).{ a: x }` — monadic return is `{ a: 1 }`
- `.( a )` — generalised lookup — `1`

**Location**: The soup desugarer's return-expression consumption
loop (around line 2152). It consumes `.name` and `.(paren)` chains
after the first return element, but it seems the parser may have
already bound `{ block }.(expr)` into a single AST node before the
desugarer sees it. Investigation needed.

## 4. Test Matrix

### 4.1 Simple Lookup

| Test | Expected | Bug? |
|------|----------|------|
| `{ x: 1 }.x` | `1` | No |
| `{ x: 1 }.y` where `y = 5` outer | key error | **Yes (3.1)** |
| `dyn.x` | key lookup | No |
| `dyn.y` where no `:y` | key error | No |
| `make(5).x` | key lookup | No |
| `{ :let x: 42 }.x` | `42` | No |

### 4.2 Generalised Lookup

| Test | Expected | Bug? |
|------|----------|------|
| `{ x: 1 }.(x + y)` y=5 | `6` | No |
| `dyn.(x + y)` y=5 | dyn.x + 5 | No |
| `{ x: 1 }.{ s: x + y }` y=5 | `{ s: 6 }` | No |
| `dyn.{ s: x + y }` y=5 | `{ s: dyn.x + 5 }` | No |
| `{ x: 1 }."val={x}"` | `"val=1"` | No |
| `{ x: 1 }.[x, y]` y=5 | `[1, 5]` | No |

### 4.3 Monadic in Lookup Position

| Test | Expected | Bug? |
|------|----------|------|
| `{ x: 1 }.{ :let y: x+10 }` | `{ y: 11 }` | **Yes (3.2)** |
| `{ x: 1 }.{ :let y: x+10 }.y` | `11` | **Yes (3.2)** |
| `{ x: 1 }.{ :let y: x+10 }.(y)` | `11` | **Yes (3.2)** |
| `dyn.{ :let z: x+y }` | implicit return | **Yes (3.2)** |
| `make(5).{ :let z: x+y }.(z)` | z value | **Yes (3.2)** |
| `{ x: 1 }.{ :io r: io.return(x) }` | io action | **Yes (3.2)** |
| `{ n: 100 }.{ :random d: random.int(n) }` | random action | **Yes (3.2)** |
| `{ init: {} }.{ :state _: state.put(:x, 1) }` | state action | **Yes (3.2)** |

### 4.4 Monadic Return and Chaining

| Test | Expected | Bug? |
|------|----------|------|
| `{ :let x: 1 }.(x + 10)` | `11` | No |
| `{ :let x: 1 }.{ a: x }` | `{ a: 1 }` | No |
| `{ :let x: 1 }.x` | `1` | No |
| `{ :let x: 1 }.{ a: x }.(a)` | `1` | **Yes (3.3)** |
| `{ :let x: 1 }.{ a: x, b: 99 }.b` | `99` | No |
| `{ :let x: 1 }.{ :let y: x+10 }.(y)` | `{ y: 11 }` then 11 | **Yes (3.3)** |
| `{ :io r: io.return(1) }.{ a: r }.(a)` | `1` | **Yes (3.3)** |

### 4.5 Dynamic and Call Chains

| Test | Expected | Bug? |
|------|----------|------|
| `dyn.{ sum: x+y }.(sum)` | sum value | No |
| `dyn.{ sum: x+y }.sum` | sum value | No |
| `make(5).{ sum: x+y }.(sum)` | `15` | No |
| `{ :let x: 1 }.(make-ctx(x))` | block from make-ctx(1) | No |

## 5. Implementation Notes

### 5.1 Fix 3.1 — Static `.name` Scope Leak

The compiler currently desugars `{ block }.name` as `let bindings in name`
when the LHS is a block literal. It should instead emit a key lookup
(symbol `:name` in the block), consistent with the dynamic case.

### 5.2 Fix 3.2 — Monadic Block in Lookup Position

The soup desugarer detects monadic blocks greedily (around line 2135 of
`rowan_ast.rs`). When a monadic block is preceded by `.` in the soup,
the desugarer should:

1. Recognise this as generalised lookup, not a standalone monadic block
2. Desugar the monadic block with implicit return (no `.expr` consumption)
3. Wrap the monadic desugared expression in the generalised lookup scope
   (the lambda that provides access to LHS bindings)

For dynamic LHS, this means the monadic bind chain is evaluated inside
the `lambda(__g). ...` where `__g` provides access to LHS block keys
via `__g.key ? key` fallback references.

### 5.3 Fix 3.3 — `.(expr)` After Monadic Block Return

The monadic block return expression consumption needs to respect
left-associativity. When the desugarer consumes `{ :monad ... }` as
a standalone monadic block (LHS position), the return expression is
the single element after the first `.`:

- `.name` — NOT a return expression (simple lookup on result)
- `.(expr)` — return expression
- `.{ block }` — return expression
- `."string"` — return expression
- `.[list]` — return expression

After consuming the return expression, any further `.` is a separate
lookup on the monadic result and should NOT be consumed by the monadic
desugarer.

Investigation needed: the parser may bind `{ block }.(expr)` as a
single AST node, in which case the fix is in how that node is
decomposed during soup desugaring.

## 6. Documentation

After fixes are implemented:

- Update `docs/guide/monads.md` with generalised lookup interaction
- Update `docs/reference/agent-reference.md` section 5 with lookup rules
- Update `docs/eucalypt-style.md` with guidance on when to use parens
- Add examples showing monadic blocks in lookup chains
