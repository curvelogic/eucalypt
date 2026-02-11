# String Interpolation

In this chapter you will learn:

- How to embed expressions in strings using `{...}` syntax
- How strings with anaphora become functions
- Format specifiers for controlling output
- The string functions available in the `str` namespace

## Basic Interpolation

Embed any expression inside a string using curly braces:

```eu
name: "World"
greeting: "Hello, {name}!"
```

```yaml
name: World
greeting: Hello, World!
```

Expressions inside the braces are evaluated:

```eu
x: 3
y: 4
result: "{x} + {y} = {x + y}"
```

```yaml
x: 3
y: 4
result: 3 + 4 = 7
```

## Nested Lookups in Interpolation

You can use dotted paths inside interpolation:

```eu
data: { foo: { bar: 99 } }
label: "{data.foo.bar}"
```

```yaml
data:
  foo:
    bar: 99
label: '99'
```

## Escaping Braces

To include a literal brace in a string, double it:

```eu
example: "Use {{braces}} for interpolation"
```

```yaml
example: Use {braces} for interpolation
```

This is also needed in regular expressions within interpolated
strings:

```eu
pattern: "01234" str.match-with("\d{{4}}")
```

## Format Specifiers

Add a format specifier after a colon inside the interpolation braces.
These use printf-style format codes:

```eu
pi: 3.14159
formatted: "{pi:%.2f}"
padded: "{42:%06d}"
```

```yaml
pi: 3.14159
formatted: '3.14'
padded: '000042'
```

## String Anaphora

When a string interpolation contains `{}` (empty braces) or `{0}`,
`{1}`, etc., the string becomes a function:

```sh
eu -e '["a", "b", "c"] map("item: {}")'
```

```yaml
- 'item: a'
- 'item: b'
- 'item: c'
```

Numbered anaphora control argument order:

```eu
reverse-pair: "{1},{0}"
result: reverse-pair(:a, :b)
```

```yaml
result: b,a
```

You can mix named references and anaphora:

```eu
prefix: "Hello"
greet: "{prefix} {}!"
result: greet("World")
```

```yaml
prefix: Hello
result: Hello World!
```

## String Functions

The `str` namespace contains functions for working with strings.

### Conversion

```sh
eu -e '42 str.of'
```

```yaml
'42'
```

### Case Conversion

```sh
eu -e '"hello" str.to-upper'
```

```yaml
HELLO
```

```sh
eu -e '"GOODBYE" str.to-lower'
```

```yaml
goodbye
```

### Splitting and Joining

Split a string on a pattern:

```sh
eu -e '"one-two-three" str.split-on("-")'
```

```yaml
- one
- two
- three
```

Join a list of strings:

```sh
eu -e '["a", "b", "c"] str.join-on(", ")'
```

```yaml
a, b, c
```

### Prefix and Suffix

```sh
eu -e '"world" str.prefix("hello ")'
```

```yaml
hello world
```

```sh
eu -e '"hello" str.suffix("!")'
```

```yaml
hello!
```

### Characters and Letters

```sh
eu -e '"hello" str.letters'
```

```yaml
- h
- e
- l
- l
- o
```

```sh
eu -e '"hello" str.letters count'
```

```yaml
5
```

## Regular Expressions

### Testing a Match

```sh
eu -e '"hello" str.matches?("^h.*o$")'
```

```yaml
true
```

### Extracting Matches

`str.match-with` returns the full match and capture groups:

```sh
eu -e '"192.168.0.1" str.match-with("(\d+)[.](\d+)[.](\d+)[.](\d+)") tail'
```

```yaml
- '192'
- '168'
- '0'
- '1'
```

`str.matches-of` returns all occurrences of a pattern:

```sh
eu -e '"192.168.0.1" str.matches-of("\d+")'
```

```yaml
- '192'
- '168'
- '0'
- '1'
```

### Replacing Text

```sh
eu -e '"hello world" str.replace("world", "there")'
```

```yaml
hello there
```

### Base64 and SHA-256

```sh
eu -e '"hello" str.base64-encode'
```

```yaml
aGVsbG8=
```

```sh
eu -e '"hello" str.sha256'
```

```yaml
2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
```

## Practical Examples

### Generating URLs

```eu
base: "https://api.example.com"
endpoints: ["users", "posts", "comments"] map("{base}/{}")
```

```yaml
base: https://api.example.com
endpoints:
- https://api.example.com/users
- https://api.example.com/posts
- https://api.example.com/comments
```

### Formatting a Table

```eu
rows: [
  { name: "Alice" score: 85 }
  { name: "Bob" score: 92 }
]

` :suppress
format-row(r): "{r.name:%10s} | {r.score:%3d}"

table: rows map(format-row)
```

## Key Concepts

- Interpolation uses `{expression}` inside double-quoted strings
- Empty braces `{}` and numbered braces `{0}`, `{1}` create string
  functions (anaphora)
- Format specifiers follow a colon: `{value:%06d}`
- Escape literal braces by doubling them: `{{` and `}}`
- The `str` namespace provides splitting, joining, case conversion,
  matching, and more
