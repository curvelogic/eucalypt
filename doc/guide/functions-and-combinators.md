# Functions and Combinators

In this chapter you will learn:

- How to define and call functions
- How currying and partial application work
- The standard combinators: `identity`, `const`, `compose`, `flip`
- How to build functions from other functions without lambdas

## Defining Functions

A function declaration has a parameter list in parentheses:

```eu
square(x): x * x
add(x, y): x + y
greet(name, greeting): "{greeting}, {name}!"
```

Functions can be called with arguments in parentheses:

```eu
a: square(5)
b: add(3, 4)
c: greet("Alice", "Hello")
```

```yaml
a: 25
b: 7
c: Hello, Alice!
```

Or via catenation (see [Expressions and
Pipelines](expressions-and-pipelines.md)):

```eu
a: 5 square
b: 4 add(3)
```

## Functions are Values

Functions are first-class values. They can be passed as arguments,
returned from other functions, and stored in blocks:

```eu
apply-twice(f, x): x f f
result: apply-twice(inc, 5)
```

```yaml
result: 7
```

```eu
ops: {
  double: * 2
  negate: 0 -
}

result: 5 ops.double ops.negate
```

```yaml
result: -10
```

## Currying

All functions are automatically curried. Providing fewer arguments
than expected returns a partially applied function:

```eu
add(x, y): x + y

add-five: add(5)      # partially applied
result: add-five(3)   # completes the application
```

```yaml
result: 8
```

This is particularly useful with `map` and `filter`:

```eu
multiply(x, y): x * y
triple: multiply(3)

results: [1, 2, 3] map(triple)
```

```yaml
results:
- 3
- 6
- 9
```

## Sections

Operators can be partially applied too. When an operator has a missing
operand, eucalypt fills in an implicit parameter:

```sh
eu -e '[1, 2, 3] map(+ 10)'
```

```yaml
- 11
- 12
- 13
```

Here `+ 10` is a section: a function that adds 10 to its argument.
Similarly:

```sh
eu -e '[1, 2, 3, 4, 5] filter(> 3)'
```

```yaml
- 4
- 5
```

Sections can be used as standalone values:

```eu
add: +
sub: -

result: add(2, 3)
diff: sub(8, 3)
```

```yaml
result: 5
diff: 5
```

## Passing operators to higher-order functions

Operators can be passed as arguments using their operator name or
as a section in parentheses:

```eu
total: foldl(+, 0, [1, 2, 3, 4, 5])
```

```yaml
total: 15
```

## Standard Combinators

The prelude provides several fundamental combinators.

### `identity`

Returns its argument unchanged:

```sh
eu -e '42 identity'
```

```yaml
42
```

### `const`

Returns a function that always produces the given value:

```sh
eu -e ':x const(99)'
```

```yaml
99
```

Useful for replacing every element with a fixed value:

```sh
eu -e '[1, 2, 3] map(const(:done))'
```

```yaml
- done
- done
- done
```

### `compose` and `∘`

Compose two functions: `compose(f, g)` produces a function that
applies `g` first, then `f`:

```sh
eu -e '1 compose(zero?, dec)'
```

```yaml
true
```

The `∘` operator is an infix form:

```sh
eu -e '(str.prefix("<") ∘ str.suffix(">"))("x")'
```

```yaml
<x>
```

### `flip`

Swap the first two arguments of a function:

```sh
eu -e 'flip(-, 1, 3)'
```

```yaml
2
```

`flip` is useful for adapting functions to a pipeline:

```eu
` :suppress
with-tags: merge flip ({ tags: [:a, :b] })

result: { name: "foo" } with-tags
```

### `complement`

Negate a predicate:

```sh
eu -e '0 complement(zero?)'
```

```yaml
false
```

### `apply`

Apply a function to a list of arguments:

```sh
eu -e 'apply(+, [3, 4])'
```

```yaml
7
```

### `uncurry`

Convert a curried function to one that takes a pair (two-element list):

```sh
eu -e 'uncurry(+)([3, 4])'
```

```yaml
7
```

### `curry`

The inverse of `uncurry` -- convert a function expecting a pair to a
curried function:

```sh
eu -e 'curry(first)("a", "b")'
```

```yaml
a
```

## Building Functions without Lambdas

Eucalypt does not have a lambda syntax. Instead, you build functions
from:

1. **Named functions** -- the clearest approach
2. **Partial application** -- `add(5)`, `* 2`
3. **Sections** -- `(+ 1)`, `(> 0)`
4. **Composition** -- `f ∘ g`
5. **Anaphora** -- `_ + 1`, `_0 * _0` (see next chapters)

These compose naturally:

```eu
` :suppress
process: filter(> 0) ∘ map(dec)

result: [3, 1, 0, 5, 2] process
```

## Practical Example: Transforming Data

```eu
people: [
  { name: "alice" age: 30 }
  { name: "bob" age: 25 }
  { name: "charlie" age: 35 }
]

` :suppress
format(p): "{p.name str.to-upper}: age {p.age}"

directory: people
  filter(_.age >= 30)
  map(format)
```

```yaml
people:
- name: alice
  age: 30
- name: bob
  age: 25
- name: charlie
  age: 35
directory:
- 'ALICE: age 30'
- 'CHARLIE: age 35'
```

## Key Concepts

- Functions are first-class values
- All functions are **curried**: partial application is automatic
- **Sections** give partial application for operators: `(+ 1)`,
  `(> 3)`
- **Combinators** like `identity`, `const`, `compose`, `flip` build
  new functions from existing ones
- Prefer named functions for anything complex; use partial application
  and sections for simple cases
