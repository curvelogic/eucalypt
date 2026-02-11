# Expressions and Pipelines

In this chapter you will learn:

- The primitive value types in eucalypt
- How function application works via catenation (pipelining)
- How partial application and currying work
- How to compose pipelines of transformations

## Primitive Values

Eucalypt has the following primitive types:

| Type | Examples | Notes |
|------|----------|-------|
| Numbers | `42`, `-7`, `3.14` | Integers and floats |
| Strings | `"hello"`, `"it's"` | Double-quoted only |
| Symbols | `:name`, `:active` | Colon-prefixed identifiers |
| Booleans | `true`, `false` | |
| Null | `null` | Renders as YAML `~` or JSON `null` |

## Lists

Lists are comma-separated values in square brackets:

```eu
numbers: [1, 2, 3, 4, 5]
mixed: [1, "two", :three, true]
nested: [[1, 2], [3, 4]]
empty: []
```

## Calling Functions

Functions can be called by placing arguments in parentheses directly
after the function name (with no intervening space):

```eu
add(x, y): x + y
result: add(3, 4)
```

```yaml
result: 7
```

## Catenation: The Pipeline Style

The distinctive feature of eucalypt is **catenation**: applying a
function by writing the argument before the function name, separated
by whitespace.

```eu
result: 5 inc
```

This is equivalent to `inc(5)` and produces `6`.

Catenation lets you chain operations into readable pipelines:

```sh
eu -e '[1, 2, 3, 4, 5] reverse head'
```

```yaml
5
```

Each step in the pipeline passes its result to the next function. You
can read it left to right: "take the list, reverse it, take the head."

## Combining Catenation with Arguments

When a function takes multiple arguments, you can supply some in
parentheses and the rest via catenation. The catenated value becomes
the *last* argument:

```eu
result: [1, 2, 3] map(inc)
```

Here `map` takes two arguments: a function and a list. `inc` is
provided in parentheses and `[1, 2, 3]` is provided by catenation.
The result is `[2, 3, 4]`.

This is the standard eucalypt pattern for data processing pipelines:

```sh
eu -e '[1, 2, 3, 4, 5] filter(> 3) map(* 10)'
```

```yaml
- 40
- 50
```

## Currying and Partial Application

All functions in eucalypt are curried: if you provide fewer arguments
than a function expects, you get back a partially applied function.

```eu
add(x, y): x + y
add-five: add(5)
result: add-five(3)
```

```yaml
result: 8
```

Curried application also works with multi-argument calls:

```eu
f(x, y, z): x + y + z

a: f(1, 2, 3)   # all at once
b: f(1)(2)(3)    # one at a time
c: f(1, 2)(3)    # mixed
```

All three produce `6`.

## Lookup: The Dot Operator

The dot operator (`.`) accesses a named property within a block:

```eu
person: { name: "Alice" age: 30 }
name: person.name
```

```yaml
person:
  name: Alice
  age: 30
name: Alice
```

Lookups can be chained:

```eu
config: { db: { host: "localhost" port: 5432 } }
host: config.db.host
```

```yaml
config:
  db:
    host: localhost
    port: 5432
host: localhost
```

> **Warning:** The dot operator binds very tightly (precedence 90).
> Writing `list head.name` is parsed as `list (head.name)`, not
> `(list head).name`. Use explicit parentheses when combining lookup
> with catenation: `(list head).name`.

## Generalised Lookup

Lookup can be generalised: any expression after the dot is evaluated
in the context of the block to the left.

```eu
point: { x: 3 y: 4 }
sum: point.(x + y)
pair: point.[x, y]
label: point."{x},{y}"
```

```yaml
point:
  x: 3
  y: 4
sum: 7
pair:
- 3
- 4
label: 3,4
```

This is particularly useful for creating temporary scopes:

```eu
result: { a: 10 b: 20 }.(a * b)
```

```yaml
result: 200
```

## Building Pipelines

Combining catenation, partial application, and the standard prelude
creates powerful data processing pipelines:

```sh
eu -e '["alice", "bob", "charlie"] map(str.to-upper) filter(str.matches?("^[AB]"))'
```

```yaml
- ALICE
- BOB
```

A more complete example:

```eu
people: [
  { name: "Alice" age: 30 }
  { name: "Bob" age: 25 }
  { name: "Charlie" age: 35 }
]

over-thirty: people filter(_.age > 30) map(_.name)
```

```yaml
people:
- name: Alice
  age: 30
- name: Bob
  age: 25
- name: Charlie
  age: 35
over-thirty:
- Charlie
```

## The `then` Function

The `then` function provides a pipeline-friendly conditional:

```sh
eu -e '5 > 3 then("yes", "no")'
```

```yaml
yes
```

It is equivalent to `if` with the condition as the last argument,
making it natural in pipelines:

```eu
result: [1, 2, 3] count (> 2) then("many", "few")
```

```yaml
result: many
```

## Key Concepts

- **Catenation** applies a function by writing the argument before the
  function name: `5 inc` means `inc(5)`
- Pipelines are built by chaining catenation: `data f g h`
- Functions are **curried**: partial application is automatic
- The **dot operator** looks up properties: `block.key`
- **Generalised lookup** evaluates expressions in a block's scope:
  `block.(expr)`
- Combine these techniques for concise data processing pipelines
