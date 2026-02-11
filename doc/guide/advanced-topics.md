# Advanced Topics

This chapter covers features that go beyond everyday eucalypt usage.

## The metadata system

Every declaration can carry metadata, attached with a backtick
(`` ` ``):

```eu
` "Compute the square of a number"
square(x): x * x

result: square(5) //=> 25
```

### Documentation metadata

A string before a declaration serves as documentation:

```eu,notest
` "Returns the full name"
full-name(first, last): "{first} {last}"
```

### Structured metadata

A block provides richer metadata:

```eu,notest
` { doc: "Custom operator" associates: :left precedence: 75 }
(l <+> r): l + r
```

### Special metadata keys

| Key          | Effect                                      |
|--------------|---------------------------------------------|
| `:target`    | Marks a declaration as a render target       |
| `:suppress`  | Hides a declaration from output              |
| `:main`      | Marks the default render target              |
| `import`     | Specifies imports for the declaration scope  |
| `associates` | Sets operator associativity (`:left`, `:right`, `:none`) |
| `precedence` | Sets operator precedence (number)            |

### Suppress and target

```eu,notest
` :suppress
helper(x): x * 2

` :target
result: helper(21)
```

`helper` is hidden from output. Only `result` is rendered.

### Unit-level metadata

If the first item in a unit is an expression (not a declaration), it
becomes metadata for the whole unit:

```eu,notest
{ import: "helpers.eu" }

result: helper-function(42)
```

## Sets

Sets are unordered collections of unique values. Convert a list to a
set with `set.from-list`:

```eu,notest
s: set.from-list([1, 2, 3, 2, 1])
```

### Set operations

```eu,notest
a: set.from-list([1, 2, 3])
b: set.from-list([2, 3, 4])

union: set.union(a, b)           # {1, 2, 3, 4}
inter: set.intersection(a, b)   # {2, 3}
diff: set.difference(a, b)      # {1}
```

### Membership

```eu,notest
s: set.from-list([1, 2, 3])
has-two: set.member(2, s)   # true
has-five: set.member(5, s)  # false
```

### Converting back to a list

```eu,notest
s: set.from-list([3, 1, 2])
xs: set.to-list(s)
```

## Deep queries

### deep-find

Search nested structures for all values at a given key:

```eu,notest
data: {
  a: { name: "Alice" nested: { name: "inner" } }
  b: { name: "Bob" }
}

names: data deep-find("name")
# => ["Alice", "inner", "Bob"]
```

### deep-query

Apply a predicate to extract values from nested structures:

```eu,notest
data: { a: 1 b: { c: "hello" d: { e: 2 } } }
nums: data deep-query(number?)
# => [1, 2]
```

## Lazy evaluation

Eucalypt uses lazy evaluation. Values are only computed when needed.
This allows working with potentially infinite structures:

```eu,notest
ones: cons(1, ones)          # infinite list of 1s
first-five: ones take(5)     # [1, 1, 1, 1, 1]
```

Natural numbers:

```eu,notest
nats-from(n): cons(n, nats-from(n + 1))
nats: nats-from(0)
first-ten: nats take(10)     # [0, 1, 2, ..., 9]
```

Laziness also means unused declarations are never evaluated, so you
can define helpers without cost if they are not referenced.

## String formatting

Use `str.fmt` for formatted output:

```eu,notest
pi: 3.14159
formatted: str.fmt("%.2f", pi)  # "3.14"
```

## Version assertions

Assert a minimum eucalypt version in source files:

```eu,notest
_ : eu.requires(">=0.3.0")
```

If the running version does not satisfy the constraint, an error is
raised. Access build metadata with:

```eu,notest
v: eu.build.version
```

## Encoding and hashing

```eu,notest
encoded: "hello" str.base64-encode
decoded: encoded str.base64-decode
hashed: "hello" str.sha256
```

## Cross product

`cross` produces all combinations from two lists:

```eu,notest
xs: [1, 2]
ys: ["a", "b"]
pairs: cross(xs, ys)
# => [[1, "a"], [1, "b"], [2, "a"], [2, "b"]]
```

## Discriminate

`discriminate` groups list elements by a predicate:

```eu,notest
xs: [1, 2, 3, 4, 5, 6]
grouped: xs discriminate(n: n % 2 == 0)
# => { true: [2, 4, 6] false: [1, 3, 5] }
```

## Numeric conversion

Convert strings to numbers:

```eu
a: num("42")   //=> 42
b: num("3.14") //=> 3.14
```

## Type predicates

Test the type of a value:

```eu
a: number?(42)     //=> true
b: string?("hi")   //=> true
c: list?([1, 2])   //=> true
d: bool?(true)     //=> true
e: null?(null)     //=> true
```

## Error handling

Eucalypt does not have try/catch. Instead, use defensive patterns:

```eu
items: [1, 2, 3]
safe: lookup-or(:missing, "default", {x: 1}) //=> "default"
```

Use `head-or` and `lookup-or` to provide defaults for potentially
missing values.

## Further reading

- [CLI Reference](../reference/cli.md) -- complete command-line
  documentation
- [Syntax Reference](../reference/syntax.md) -- formal syntax
  description
- [Prelude Reference](../reference/prelude/index.md) -- all built-in
  functions
