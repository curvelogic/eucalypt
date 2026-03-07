# Blocks and Declarations

In this chapter you will learn:

- What blocks are and how they relate to structured data formats
- The three types of declarations: property, function, and operator
- How top-level files work as implicit blocks (units)
- How to annotate declarations with metadata

## Blocks

A **block** is eucalypt's fundamental data structure. It corresponds to
a JSON object, a YAML mapping, or a TOML table: an ordered collection
of named values.

Blocks are written with curly braces:

```eu
person: {
  name: "Alice"
  age: 30
  role: "engineer"
}
```

Running this file produces:

```yaml
person:
  name: Alice
  age: 30
  role: engineer
```

Blocks can be nested:

```eu
config: {
  database: {
    host: "localhost"
    port: 5432
  }
  cache: {
    host: "localhost"
    port: 6379
  }
}
```

## Property Declarations

The simplest declaration is a **property declaration**: a name followed
by a colon and an expression.

```eu
greeting: "Hello, World!"
count: 42
pi: 3.14159
active: true
nothing: null
```

These declare names bound to values. The values can be any expression:
numbers, strings, booleans, `null`, lists, blocks, or computed
expressions.

### Commas are Optional

Declarations can be separated by commas or simply by whitespace.
Line endings are not significant. All of these are equivalent:

```eu
a: { x: 1 y: 2 z: 3 }
b: { x: 1, y: 2, z: 3 }
c: { x: 1, y: 2, z: 3, }
```

```sh
eu -e '{ x: 1 y: 2 z: 3 }'
```

```yaml
x: 1
y: 2
z: 3
```

### Symbols

Symbols are written with a colon prefix and behave like interned
strings. They are used as keys and as lightweight identifiers:

```eu
status: :active
tag: :important
```

```yaml
status: active
tag: important
```

## Function Declarations

Adding a parameter list creates a **function declaration**:

```eu
greet(name): "Hello, {name}!"
double(x): x * 2

message: greet("World")
result: double(21)
```

```yaml
message: Hello, World!
result: 42
```

Functions are not rendered in the output -- only property values
appear. Functions can take multiple parameters:

```eu
add(x, y): x + y
total: add(3, 4)
```

```yaml
total: 7
```

## Operator Declarations

You can define custom infix operators using symbolic names:

```eu
(x <+> y): [x, y]
pair: 1 <+> 2
```

```yaml
pair:
- 1
- 2
```

Prefix and postfix unary operators are also possible:

```eu
(!! x): x * x
squared: !! 5
```

```yaml
squared: 25
```

Operator precedence and associativity are controlled through metadata
annotations (covered below). See the
[Operators](operators.md) chapter for full details.

> **Note:** While function declarations are namespaced to their block,
> operators do not have a namespace and are available only where they
> are in scope.

## Units: Top-Level Blocks

The top-level of a `.eu` file is itself a block, called a **unit**.
It does not need surrounding braces. So this file:

```eu
name: "Alice"
age: 30
```

...is equivalent to a block `{ name: "Alice" age: 30 }` and produces:

```yaml
name: Alice
age: 30
```

## Comments

Comments start with `#` and continue to the end of the line:

```eu
# This is a comment
name: "Alice"  # inline comment
```

## Declaration Metadata

Metadata can be attached to any declaration by placing it between a
leading backtick and the declaration:

```eu
` "A friendly greeting"
greeting: "Hello!"

` { doc: "Add two numbers" }
add(x, y): x + y
```

A bare string is shorthand for documentation metadata.

Some metadata keys activate special behaviour:

- `:suppress` -- hides the declaration from output
- `:target` -- marks the declaration as an export target
- `:main` -- marks the default target

```eu
` :suppress
helper(x): x + 1

` { target: :my-output }
output: {
  result: helper(41)
}
```

Running `eu file.eu -t my-output` renders only the `output` block.

## Block and Unit Metadata

A single expression may precede the declarations in any block and is
treated as metadata for that block. At the top level of a file (the
unit), this means the first item, if it is an expression rather than a
declaration, becomes metadata for the entire unit:

```eu
{ doc: "Configuration generator" }

host: "localhost"
port: 8080
```

## Scope and Visibility

Names declared in a block are visible within that block and in any
nested blocks:

```eu
x: 99
inner: {
  y: x + 1  # x is visible here
}
```

```yaml
x: 99
inner:
  y: 100
```

Names in nested blocks can shadow outer names:

```eu
x: 1
inner: {
  x: 2
  y: x  # refers to inner x
}
```

```yaml
x: 1
inner:
  x: 2
  y: 2
```

> **Warning:** Be careful with self-reference. Writing `name: name`
> inside a block creates an infinite recursion, because the
> declaration `name` refers to itself. This is true regardless of
> whether `name` is defined in an outer scope.

## Key Concepts

- **Blocks** are ordered collections of named values (like JSON
  objects or YAML mappings)
- **Property declarations** bind a name to a value
- **Function declarations** bind a name to a function (not rendered in
  output)
- **Operator declarations** define custom infix, prefix, or postfix
  operators
- **Metadata** annotations control export, documentation, and other
  special behaviour
- The top-level file is a unit: an implicit block without braces
