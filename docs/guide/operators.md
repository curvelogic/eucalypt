# Operators

In this chapter you will learn:

- How to define custom nullary, binary, prefix, and postfix operators
- How precedence and associativity work
- How to control operator behaviour with metadata
- The built-in operators provided by the prelude

## Defining Binary Operators

A binary operator is declared by writing the operand names and the
operator symbol in parentheses:

```eu
(x <+> y): x + y + 1

result: 3 <+> 4
```

```yaml
result: 8
```

Operator names use symbolic characters: `+`, `-`, `*`, `/`, `<`, `>`,
`|`, `&`, `!`, `@`, `#`, `~`, `^`, and any Unicode symbol or
punctuation characters.

## Nullary Operators

A nullary operator takes no operands — it is a constant written as a
symbol:

```eu
(∅): '__SET.EMPTY'
result: ∅ set.to-list
```

```yaml
result: []
```

The empty set `∅` is the only nullary operator in the standard prelude,
but you can define your own:

```eu,notest
(★): 42
answer: ★
```

## Prefix and Postfix Operators

Prefix operators have the operator before the operand:

```eu
(¬ x): not(x)
result: ¬ true
```

```yaml
result: false
```

Postfix operators have the operator after the operand:

```eu
(x !!): x * x
result: 5 !!
```

```yaml
result: 25
```

## Precedence

Without precedence rules, operator expressions would be ambiguous. In
eucalypt, precedence determines which operators bind more tightly.

The prelude defines the standard precedence levels:

| Level | Name | Operators |
|-------|------|-----------|
| 95 | prefix | `↑` (head) |
| 90 | lookup | `.` |
| 88 | bool-unary | `!`, `¬` |
| 85 | exp | `^`, `!!` (nth) |
| 80 | prod | `*`, `/`, `÷`, `%` |
| 75 | sum | `+`, `-` |
| 60 | shift | (shift operators) |
| 55 | bitwise | (bitwise operators) |
| 50 | cmp | `<`, `>`, `<=`, `>=` |
| 45 | append | `++`, `<<` |
| 40 | eq | `=`, `!=` |
| 35 | bool-prod | `&&`, `∧` |
| 30 | bool-sum | `\|\|`, `∨` |
| 20 | cat | (catenation) |
| 10 | apply | `@` |
| 5 | meta | `//`, `//=`, `//=>`, `//=?`, `//!`, `//!!` |

Higher numbers bind more tightly:

```sh
eu -e '1 + 2 * 3'
```

```yaml
7
```

Because `*` (precedence 80) binds tighter than `+` (precedence 75),
this is parsed as `1 + (2 * 3)`, not `(1 + 2) * 3`.

## Associativity

When the same operator (or operators at the same precedence) appear
in sequence, associativity determines the grouping.

- **Left-associative**: `1 - 2 - 3` = `(1 - 2) - 3` = `-4`
- **Right-associative**: `a -> b -> c` = `a -> (b -> c)`

Most arithmetic and comparison operators are left-associative.

## Setting Precedence and Associativity

Use declaration metadata to control your operator's precedence and
associativity:

```eu
` { associates: :left
    precedence: :sum }
(x +++ y): x + y

` { associates: :right
    precedence: :prod }
(x *** y): x * y
```

Precedence can be specified as:
- A named level: `:sum`, `:prod`, `:exp`, `:cmp`, `:eq`, `:bool-prod`,
  `:bool-sum`, `:append`, `:map`, `:bool-unary`, `:cat`, `:apply`,
  `:meta`, `:shift`, `:bitwise`
- A numeric value: any integer (higher binds tighter)

Associativity can be `:left`, `:right`, or omitted (defaults to
`:left`).

## The Assertion Operators

Two special operators are provided for testing:

### `//=` (assert equals)

Asserts that the left side equals the right side at runtime, and
returns the value if true. Panics if false:

```eu
result: 2 + 2 //= 4
```

### `//=>` (assert equals with metadata)

Like `//=` but also attaches the assertion as metadata:

```eu
checked: 2 + 2 //=> 4
```

Both are useful for embedding tests and sanity checks in code.

## The Metadata Operator `//`

Attach metadata to any value:

```eu
tagged: 42 // { note: "the answer" }
```

The metadata can be retrieved with `meta`:

```eu,notest
note: meta(tagged).note
```

```yaml
note: the answer
```

See [Advanced Topics](advanced-topics.md) for more on metadata.

## The Deep Merge Operator `<<`

Deep merge combines two blocks, recursively merging nested blocks:

```eu
base: { a: { x: 1 y: 2 } b: 3 }
overlay: { a: { y: 9 z: 10 } }
result: base << overlay
```

```yaml
base:
  a:
    x: 1
    y: 2
  b: 3
overlay:
  a:
    y: 9
    z: 10
result:
  a:
    x: 1
    y: 9
    z: 10
  b: 3
```

## The Append Operator `++`

Concatenate two lists:

```sh
eu -e '[1, 2] ++ [3, 4]'
```

```yaml
- 1
- 2
- 3
- 4
```

## Dot Sections

The dot operator can be used as a section to create lookup functions:

```sh
eu -e '[{x: 1}, {x: 2}, {x: 3}] map(.x)'
```

```yaml
- 1
- 2
- 3
```

## Idiot Brackets

Eucalypt lets you define custom Unicode bracket pairs that wrap and
transform an expression. These are called *idiot brackets* (inspired
by *idiom brackets* from applicative functor notation, but they are a
general bracket overloading mechanism).

```eu
⌈ x ⌉: x * 2

doubled: ⌈ 3 + 4 ⌉
```

```yaml
doubled: 14
```

The declaration `⌈ x ⌉: body` defines a function named `⌈⌉` that
takes one argument. Using `⌈ expr ⌉` in an expression calls that
function with `expr`.

Any of the built-in Unicode bracket pairs can be used:

| Open | Close | Name |
|------|-------|------|
| `⟦`  | `⟧`   | Mathematical white square brackets |
| `⟨`  | `⟩`   | Mathematical angle brackets |
| `⟪`  | `⟫`   | Mathematical double angle brackets |
| `⌈`  | `⌉`   | Ceiling brackets |
| `⌊`  | `⌋`   | Floor brackets |
| `«`  | `»`   | French guillemets |

(and several others — see the
[syntax reference](../reference/syntax.md) for the full list.)

Idiot brackets can also be given a monadic interpretation for
sequencing — see [Monads and the monad() Utility](monads.md) for
details on bracket pair definitions with `:monad` metadata.

## Key Concepts

- Operators are declared with symbolic names in parentheses:
  `(x op y):`, `(op x):`, `(x op):`
- **Precedence** controls binding strength; higher numbers bind
  tighter
- **Associativity** determines grouping for equal-precedence
  operators
- Use metadata to set `precedence` and `associates` on custom
  operators
- The prelude provides standard arithmetic, comparison, boolean, and
  utility operators
