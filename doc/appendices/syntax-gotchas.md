# Syntax Gotchas

This document records unintuitive consequences of Eucalypt's syntax
design decisions that can lead to subtle bugs or confusion.

## Operator Precedence Issues

Eucalypt's catenation (juxtaposition) operator has **very low
precedence (20)** — lower than all arithmetic and comparison
operators. This means infix operators bind more tightly than
catenation, which can produce surprising parses.

The precedence hierarchy (highest to lowest):

| Precedence | Name         | Operators                    |
|------------|--------------|------------------------------|
| 95         |              | `↑` (head prefix)            |
| 90         | lookup/call  | `.` (lookup), `f(x, y)`      |
| 88         | bool-unary   | `!` (not)                    |
| 85         | exp          | `^` (power)                  |
| 80         | prod         | `*`, `/`, `÷`, `%`           |
| 75         | sum          | `+`, `-`                     |
| 50         | cmp          | `<`, `>`, `<=`, `>=`         |
| 45         | append       | `++`                         |
| 40         | eq           | `=`, `!=`                    |
| 35         | bool-prod    | `∧` (and)                    |
| 30         | bool-sum     | `∨` (or)                     |
| 20         | cat          | catenation (juxtaposition)   |
| 10         | apply        |                              |
| 5          | meta         | `` ` `` (metadata)           |

### Field Access vs Catenation

**Problem**: The lookup operator (`.`) has higher precedence (90) than
catenation (precedence 20), which can lead to unexpected parsing.

**Gotcha**: Writing `objects head.id` is parsed as `objects (head.id)`
rather than `(objects head).id`.

**Example**:
```eu,notest
# This doesn't work as expected:
objects: range(0, 5) map({ id: _ })
result: objects head.id  # Parsed as: objects (head.id)

# Correct syntax requires parentheses:
result: (objects head).id  # Explicitly groups the field access
```

**Error Message**: When this occurs, you may see confusing errors like:
- `cannot return function into case table without default`
- `bad index 18446744073709551615 into environment` (under memory pressure)

**Solution**: Always use parentheses to group the expression you want to
access fields from:
- Use `(expression).field` instead of `expression target.field`
- Be explicit about precedence when combining catenation with field access

### Arithmetic After a Pipeline

**Problem**: Infix operators like `+`, `-`, `*` have higher precedence
than catenation. When an infix operator follows a pipeline (catenation
chain), it binds to the last function in the chain, not to the result
of the whole pipeline.

**Gotcha**: Writing `xs foldl(f, 0) + 1` is **not** parsed as
`(xs foldl(f, 0)) + 1`. Instead, the shunting-yard algorithm sees:

```
[xs, cat@20, foldl, call@90, (f,0), +@75, 1]
```

Because `+` (75) has higher precedence than `cat` (20), the `+`
binds first: `foldl(f, 0) + 1` is evaluated (adding 1 to a partial
application — a type error), and then the result is applied to `xs`.

**Example**:
```eu,notest
# WRONG — fails at runtime:
n: xs foldl(+, 0) + 1        # Parsed as: (foldl(+, 0) + 1)(xs)

# CORRECT — use parentheses:
n: (xs foldl(+, 0)) + 1      # Explicit grouping

# CORRECT — split into two bindings:
m: xs foldl(+, 0)
n: m + 1
```

**Error Messages**:
- `cannot return function into case table without default` — the `+`
  intrinsic receives a function (the partial application) instead of a
  number.

**Rule of thumb**: If a pipeline result feeds into an infix operator,
always use parentheses around the pipeline or bind it to a name first.

## Anaphora and Function Syntax

### Lambda Syntax Does Not Exist

**Problem**: Eucalypt does **not** have lambda expressions or arrow
functions. There is no syntax for anonymous multi-parameter functions.

**Gotcha**: The `->` token is the **`const` operator** (returns its
left operand, ignoring the right). Writing `x -> x + 1` does not
create a function — it evaluates `x`, discards `x + 1`, and returns
`x`.

**Invalid Examples**:
```eu,notest
# NONE of these create functions in Eucalypt:
map(\x -> x + 1)     # Invalid syntax
map(|x| x + 1)       # Invalid syntax
map(fn(x) => x + 1)  # Invalid syntax
filter(x -> x > 0)   # -> is const, not lambda!
```

**Correct Approach**: Use sections, anaphora (`_`, `_0`, `_1`), named
functions, or partial application:
```eu,notest
# Operator sections (preferred for simple cases):
map(+ 1)             # Section: adds 1
map(^ 2)             # Section: squares
filter(> 0)          # Section: positive?

# Using anaphora (single use of _ only):
map(_ + 1)           # Anonymous single-parameter function

# Using named function + partial application:
add-one(x): x + 1
map(add-one)

# For multi-parameter needs, define a named helper:
has-y(y, h): first(h) = y
filter(has-y(target-y))   # Partial application
```

**Important**: `_` can only appear **once** in an anonymous function.
Two occurrences of `_` is an error. Use a named function instead.

**Reference**: See [Anaphora](../guide/anaphora.md) for detailed
explanation of anaphora usage.

## Metadata vs Comments

### Backtick Is Metadata, Not a Comment

**Problem**: The backtick (`` ` ``) attaches metadata to the **next**
declaration. It is not a comment syntax.

**Gotcha**: Writing `` ` "some text" `` with no declaration following
it causes a parse error, often reported at an unexpected location
(e.g., line 1).

**Example**:
```eu,notest
# WRONG — dangling metadata with nothing to attach to:
` "This is not a comment"

# CORRECT — use # for comments:
# This is a comment

# CORRECT — metadata attached to a declaration:
` "Documentation for my-fn"
my-fn(x): x + 1
```

**Rule**: Use `#` for comments. Only use `` ` `` when you intend to
attach metadata to the immediately following declaration.

## Single Quote Identifiers

### Single Quotes Are Not String Delimiters

**Problem**: Single quotes (`'`) in Eucalypt are used to create
identifiers, not strings.

**Gotcha**: Coming from languages where single quotes delimit strings,
developers might expect `'text'` to be a string literal.

**Key Rules**:
- Single quotes create **normal identifiers** that can contain any characters
- The identifier name is the content *between* the quotes (quotes are stripped)
- This is the only use of single quotes in Eucalypt
- String literals use double quotes (`"`) only

**Examples**:
```eu,notest
# Single quotes create identifiers (variable names):
'my-file.txt': "content"     # Creates identifier: my-file.txt
home: {
  '.bashrc': false           # Creates identifier: .bashrc
  '.emacs.d': false          # Creates identifier: .emacs.d
  'notes.txt': true          # Creates identifier: notes.txt
}

# Access using lookup:
z: home.'notes.txt'          # Looks up identifier: notes.txt

# NOT string literals:
'hello' = 'hello'            # Compares two variable references (not strings)
"hello" = "hello"            # Compares two string literals (correct)
```

## Division Operators

### `/` Is Floor Division

**Problem**: The `/` operator performs **floor division** (integer
division), not precise division.

**Gotcha**: `7 / 2` evaluates to `3`, not `3.5`.

**Example**:
```eu,notest
7 / 2    # => 3 (floor division)
7 ÷ 2    # => 3.5 (exact division)
```

**Rule**: Use `÷` for exact/precise division. Use `/` only when you
want integer (floor) division.

## Future Improvements

These gotchas highlight areas where the language could benefit from:

1. **Better Error Messages**: More specific error messages when
   precedence issues occur
2. **Linting Rules**: Static analysis to catch common precedence
   mistakes
3. **IDE Support**: Syntax highlighting and warnings for ambiguous
   expressions
4. **Documentation**: Better examples showing correct precedence usage
