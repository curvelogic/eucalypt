# Functions and Combinators

Functions are the primary tool for abstraction in eucalypt. This
chapter covers how to define, apply, and compose them.

## Defining functions

A function declaration adds a parameter list to a name:

```eu
double(x): x * 2
add(x, y): x + y

a: double(21)  //=> 42
b: add(3, 4)   //=> 7
```

The body is any expression. There are no explicit `return` statements
-- the body's value is the result.

## Currying and partial application

All functions are automatically curried. Applying fewer arguments than
expected returns a new function:

```eu
add(x, y): x + y
increment: add(1)
result: increment(9) //=> 10
```

This is fundamental to the pipeline style:

```eu
add(x, y): x + y
result: 3 add(5) //=> 8
```

Here `add(5)` creates a function that adds 5, and `3` is applied to
it via catenation.

## Sections

A section is a partially applied operator written in parentheses:

```eu
xs: [1, 2, 3]
doubled: xs map (* 2)  //=> [2, 4, 6]
bumped: xs map (+ 10)  //=> [11, 12, 13]
```

The missing operand becomes the parameter. Both left and right
sections are valid:

```eu
halve: (/ 2)
result: halve(10) //=> 5
```

## Catenation as application

Juxtaposition (placing values next to each other) is function
application. The left value becomes the first argument to the right:

```eu
double(x): x * 2
negate(x): 0 - x

result: 21 double negate //=> -42
```

This reads naturally as a pipeline: take 21, double it, negate it.

## Inline functions

You can define anonymous functions inline using a parameter-colon
syntax:

```eu
xs: [1, 2, 3, 4]
evens: xs filter(n: n % 2 == 0) //=> [2, 4]
```

Multi-parameter inline functions:

```eu
xs: [3, 1, 4, 1, 5]
desc: xs qsort(a b: b < a) //=> [5, 4, 3, 1, 1]
```

## No lambda syntax

Eucalypt has no standalone lambda syntax like `\x -> x + 1`. Instead,
use:

- **Named functions**: `double(x): x * 2`
- **Inline functions**: `xs map(x: x * 2)`
- **Sections**: `xs map(* 2)`
- **String anaphora**: `xs map("{_}")`
- **Block anaphora**: `xs map(_.name)`

## Composition operators

### Forward composition (`;`)

The semicolon composes functions left to right:

```eu
double(x): x * 2
negate(x): 0 - x
double-then-negate: double ; negate
result: double-then-negate(5) //=> -10
```

This creates a new function that first doubles, then negates.

### Backward composition

Use `∘` (Unicode) for right-to-left composition, matching
mathematical convention:

```eu,notest
negate-then-double: double ∘ negate
```

## Identity and const

`id` returns its argument unchanged:

```eu
result: id(42) //=> 42
```

`const` takes two arguments and returns the first:

```eu
result: const(1, 2) //=> 1
```

These are useful as defaults or placeholders in higher-order
functions.

## Flip

`flip` swaps the first two arguments of a function:

```eu,notest
sub(x, y): x - y
bus: flip(sub)       # now takes y first, then x
```

## Complement

`complement` negates a predicate:

```eu
xs: [1, 2, 3, 4, 5, 6]
odds: xs filter(complement(n: n % 2 == 0)) //=> [1, 3, 5]
```

## The `@` operator

The `@` operator applies a function to a value. It is useful for
passing a function as a pipeline step:

```eu,notest
transform(f, x): f @ x
```

## Higher-order patterns

### Mapping and filtering

```eu
data: [1, 2, 3, 4, 5]
result: data filter(> 2) map(* 10) //=> [30, 40, 50]
```

### Folding

```eu
xs: [1, 2, 3, 4]
total: xs foldl(+, 0) //=> 10
```

### Function factories

Functions that return functions:

```eu
multiplier(n): (* n)
triple: multiplier(3)
result: triple(7) //=> 21
```

## Next steps

- [Operators](operators.md) -- defining custom operators
- [Anaphora](anaphora.md) -- shorthand for common function patterns
