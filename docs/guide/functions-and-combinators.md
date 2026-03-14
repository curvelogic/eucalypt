# Functions and Combinators

In this chapter you will learn:

- How to define and call functions
- How destructuring parameters work
- How currying and partial application work
- The standard combinators: `identity`, `const`, `compose`, `flip`,
  and forward composition with `;`
- How to build functions from other functions without lambdas

## Defining Functions

A function declaration has a parameter list in parentheses:

```eu
square(x): x * x
add(x, y): x + y
greet(name, greeting): "{greeting}, {name}!"
```

Functions can be called with arguments in parentheses:

```eu,notest
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

```eu,notest
a: 5 square
b: 4 add(3)
```

## Destructuring Parameters

Function parameters can be **destructuring patterns** that extract
structure from an argument inline, binding its components as named
variables in the function body.

### Block destructuring

A block pattern binds named fields from a block argument. Shorthand
form binds the field name directly:

```eu,notest
sum-of-point({x y}): x + y

p: { x: 3 y: 4 }
result: sum-of-point(p)
```

```yaml
result: 7
```

A rename form binds a field under a different local name, using a
colon between the field name and the binding name:

```eu
scaled({x: a  y: b}, scale): a * scale + b * scale

result: scaled({x: 2  y: 3}, 10)
```

```yaml
result: 50
```

Shorthand and rename can be mixed freely:

```eu
describe({x  y: height}): "x={x} h={height}"

result: describe({x: 1  y: 5})
```

```yaml
result: x=1 h=5
```

### Fixed-length list destructuring

A list pattern binds positional elements from a list argument:

```eu
add-pair([a, b]): a + b

result: add-pair([10, 20])
```

```yaml
result: 30
```

Multiple elements at any position are supported:

```eu
third([a, b, c]): c

result: third([1, 2, 3])
```

```yaml
result: 3
```

### Head/tail list destructuring

A head/tail pattern separates a list into its first element and the
remaining list. A colon inside square brackets separates the fixed
elements (heads) from the tail binding:

```eu
first-of([x : xs]): x
rest-of([x : xs]): xs

a: first-of([1, 2, 3])
b: rest-of([1, 2, 3])
```

```yaml
a: 1
b: [2, 3]
```

Multiple heads are separated by commas before the colon:

```eu
sum-first-two([a, b : rest]): a + b

result: sum-first-two([10, 20, 30])
```

```yaml
result: 30
```

### Juxtaposed call syntax

When a function takes a block or list argument, you may call it by
placing the block or list immediately after the function name with no
space:

```eu
sum-xy({x y}): x + y

a: sum-xy{x: 3 y: 4}    # same as sum-xy({x: 3 y: 4})
```

```yaml
a: 7
```

Similarly for list arguments:

```eu
add-pair([a, b]): a + b

b: add-pair[10, 20]      # same as add-pair([10, 20])
```

```yaml
b: 30
```

Combined with block destructuring, juxtaposed calls give named
arguments as an emergent pattern — no extra language concept needed:

```eu
greet({name greeting}): "{greeting}, {name}!"

result: greet{name: "Alice" greeting: "Hello"}
```

```yaml
result: Hello, Alice!
```

### Juxtaposed definition syntax

The juxtaposed bracket syntax also works on the definition side. Writing
the bracket or brace directly against the function name (no space) is
sugar for the parenthesised destructuring form:

```eu
# These pairs are equivalent:
add-pair[a, b]: a + b         # sugar for add-pair([a, b]): a + b
add-block{x y}: x + y        # sugar for add-block({x y}): x + y
my-head[h : t]: h             # sugar for my-head([h : t]): h
```

### The cons operator `‖`

The `‖` operator (U+2016, DOUBLE VERTICAL LINE) prepends a single
element to a list. It is right-associative, so chains build lists
left-to-right without parentheses:

```eu
a: 1 ‖ [2, 3]          # [1, 2, 3]
b: 1 ‖ 2 ‖ [3]         # [1, 2, 3]
c: 1 ‖ []              # [1]
```

```yaml
a: [1, 2, 3]
b: [1, 2, 3]
c: [1]
```

The precedence of `‖` (55) is between comparison (50) and arithmetic
(75), so it binds more tightly than comparisons but less tightly than
addition or multiplication.


### Mixing patterns

Normal parameters and destructuring patterns can be combined in any
order:

```eu
weighted-sum(w, [a, b, c]): w * a + w * b + w * c

result: weighted-sum(2, [1, 3, 5])
```

```yaml
result: 18
```

Multiple destructuring parameters are also allowed:

```eu
combine({x}, [a, b]): x + a + b

result: combine({x: 10}, [3, 7])
```

```yaml
result: 20
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

### Forward composition `;`

The `;` operator composes in the other direction: `f ; g` applies `f`
first, then `g`. This reads naturally in pipelines — left to right:

```sh
eu -e '"hello" (str.letters ; count)'
```

```yaml
5
```

Forward composition is often simpler than `∘` because it follows the
data flow:

```eu
` :suppress
shout: str.to-upper ; str.suffix("!")

result: "hello" shout
```

```yaml
result: HELLO!
```

Use `;` when building a pipeline from smaller steps:

```sh
eu -e '[3, 1, 4, 1, 5] map(inc ; (* 2))'
```

```yaml
- 8
- 4
- 10
- 4
- 12
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

```eu,notest
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
4. **Composition** -- `f ∘ g` or `g ; f`
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
  { name: "alice", age: 30 },
  { name: "bob", age: 25 },
  { name: "charlie", age: 35 }
]

` :suppress
format(p): "{p.name}: age {p.age}"

directory: people
  filter(.age >= 30)
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
- 'alice: age 30'
- 'charlie: age 35'
```

## Key Concepts

- Functions are first-class values
- All functions are **curried**: partial application is automatic
- **Sections** give partial application for operators: `(+ 1)`,
  `(> 3)`
- **Combinators** like `identity`, `const`, `compose` (`∘`),
  forward-compose (`;`), and `flip` build new functions from existing
  ones
- Prefer named functions for anything complex; use partial application
  and sections for simple cases
