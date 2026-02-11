# Blocks and Declarations

Blocks are the fundamental structuring mechanism in eucalypt. They
represent key-value mappings -- the same data that YAML mappings or
JSON objects encode -- but with the addition of functions and
expressions.

## What is a block?

A block is a collection of **declarations** enclosed in curly braces:

```eu
point: { x: 3 y: 4 }
```

Each declaration binds a name to a value. Commas between declarations
are optional:

```eu
a: { x: 1, y: 2 }
b: { x: 1 y: 2 }
```

## Top-level blocks (units)

The top level of a `.eu` file is itself a block -- you do not need
braces around it. This is called a **unit**.

```eu
name: "Alice"
age: 30
```

This is equivalent to writing `{ name: "Alice" age: 30 }`.

## Property declarations

The simplest declaration binds a name to a value:

```eu
x: 42
greeting: "hello"
flag: true
```

The value can be any expression, including other blocks or lists:

```eu
config: {
  db: {
    host: "localhost"
    port: 5432
  }
  debug: true
}
```

## Function declarations

Add a parameter list to create a function:

```eu
double(x): x * 2
add(x, y): x + y

result: double(21) //=> 42
sum: add(3, 4) //=> 7
```

Functions are curried, so applying fewer arguments than expected
returns a new function:

```eu
add(x, y): x + y
increment: add(1)
result: increment(9) //=> 10
```

## Operator declarations

Binary operators use symbolic names between their parameters:

```eu
(l <+> r): l + r + 1
result: 3 <+> 4 //=> 8
```

Prefix and postfix unary operators are also possible:

```eu,notest
(! x): not(x)          # prefix
(x ******): "maybe {x}"  # postfix
```

To control precedence and associativity, attach metadata:

```eu,notest
` { associates: :right precedence: 75 }
(l <+> r): l + r
```

See [Operators](operators.md) for more on defining and using operators.

## Nesting and scope

Declarations are visible within their enclosing block and in any
nested blocks:

```eu
outer: {
  x: 10
  inner: {
    y: x + 5
    result: y //=> 15
  }
}
```

A declaration in a nested block **shadows** the same name from an
outer block:

```eu
x: 1
inner: { x: 2 result: x //=> 2 }
```

Be careful: shadowing can lead to infinite recursion if you try to
reference the outer value:

```eu,notest
name: "foo"
x: { name: name }   # infinite recursion! inner 'name' refers to itself
```

## Accessing block values with lookup

The dot operator (`.`) looks up a key in a block:

```eu
point: { x: 3 y: 4 }
px: point.x //=> 3
py: point.y //=> 4
```

Chained lookups work for nested blocks:

```eu
config: { db: { host: "localhost" } }
host: config.db.host //=> "localhost"
```

## Generalised lookup

The dot operator can take an arbitrary expression on the right-hand
side. That expression is evaluated in the scope of the block on the
left:

```eu
point: { x: 3 y: 4 }
sum: point.(x + y) //=> 7
coords: point.[x, y] //=> [3, 4]
label: point."{x},{y}" //=> "3,4"
```

This is a powerful feature but can become hard to read if overused.
Keep it simple.

## Metadata annotations

A backtick (`` ` ``) before a declaration attaches metadata to it:

```eu
` "Compute the square of a number"
square(x): x * x

result: square(5) //=> 25
```

Metadata can be a string (documentation) or a structured block:

```eu,notest
` { doc: "Custom operator" associates: :left precedence: 75 }
(l <+> r): l + r
```

Special metadata keys include:
- `:target` -- marks a declaration as a render target
- `:suppress` -- hides a declaration from output
- `:main` -- marks the default render target
- `import` -- specifies imports (see [Imports](imports-and-modules.md))

## Unit-level metadata

If the first item in a unit is an expression rather than a
declaration, it is treated as metadata for the whole unit:

```eu
{ :doc "An example unit" }
a: 1
b: 2
```

## Block merge

When two blocks are combined by catenation (juxtaposition), they
merge. The second block's values override the first:

```eu
base: { a: 1 b: 2 }
overlay: { b: 3 c: 4 }
merged: base overlay //=> { a: 1 b: 3 c: 4 }
```

This is a **shallow** merge. For recursive deep merge of nested
blocks, use the `<<` operator:

```eu
base: { x: { a: 1 b: 2 } }
extra: { x: { c: 3 } }
result: base << extra
```

See [Block Manipulation](block-manipulation.md) for more merge and
transformation functions.

## Next steps

- [Expressions and Pipelines](expressions-and-pipelines.md) -- how to
  write expressions and chain operations
- [Functions and Combinators](functions-and-combinators.md) -- more on
  defining and composing functions
