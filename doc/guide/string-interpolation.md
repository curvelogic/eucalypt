# String Interpolation

Eucalypt strings support embedded expressions, making it easy to build
formatted output from data.

## Basic strings

Double-quoted strings support interpolation with curly braces:

```eu
name: "world"
greeting: "hello, {name}" //=> "hello, world"
```

## Interpolation syntax

Names and lookups can appear inside curly braces in a double-quoted
string:

```eu
x: 10
a: "x is {x}"           //=> "x is 10"
b: "flag: {true}"       //=> "flag: true"
```

Nested blocks and lookups work too:

```eu
point: { x: 3 y: 4 }
label: "({point.x}, {point.y})" //=> "(3, 4)"
```

## String anaphora

The numbered anaphora `{0}`, `{1}` etc. turn a string into a
function, where `{0}` is the first argument:

```eu
xs: [1, 2, 3]
result: xs map("item {0}") //=> ["item 1", "item 2", "item 3"]
```

See [Anaphora](anaphora.md) for more on string anaphora.

## Multi-line strings

Use triple double-quotes for multi-line strings:

```eu,notest
text: """
  This is a
  multi-line string
"""
```

Leading indentation is stripped based on the closing delimiter.

## String functions

### Case conversion

```eu
a: "hello" str.to-upper //=> "HELLO"
b: "HELLO" str.to-lower //=> "hello"
```

### Length

```eu
n: "hello" str.len //=> 5
```

### Splitting and joining

`str.split-on` and `str.join-on` are pipeline-friendly (the delimiter
is the first argument):

```eu
parts: "a,b,c" str.split-on(",")   //=> ["a", "b", "c"]
joined: ["a", "b", "c"] str.join-on(",") //=> "a,b,c"
```

### Pattern matching

```eu
a: str.matches?("^hello", "hello world") //=> true
```

### Conversion

Convert values to strings with `str.of`:

```eu
a: str.of(42)   //=> "42"
b: str.of(true) //=> "true"
```

## Regular expressions

Use `str.matches` to test a string against a regex pattern:

```eu,notest
valid: "abc123" str.matches("[a-z]+[0-9]+")
```

Use `str.replace` for regex substitution:

```eu,notest
result: "foo bar" str.replace("o+", "0")
```

## Encoding and hashing

```eu,notest
encoded: "hello" str.base64-encode
decoded: encoded str.base64-decode
hashed: "hello" str.sha256
```

## Practical example

Build a formatted report from data:

```eu
describe(p): "{p.name} is {p.age}"
people: [
  { name: "Alice", age: 30 },
  { name: "Bob", age: 25 }
]
result: people map(describe) //=> ["Alice is 30", "Bob is 25"]
```

## Next steps

- [Functions and Combinators](functions-and-combinators.md) -- defining
  and composing functions
- [Anaphora](anaphora.md) -- more on `_` and other anaphoric
  expressions
