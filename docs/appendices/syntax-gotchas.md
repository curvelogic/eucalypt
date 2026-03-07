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

# Identity anaphor — passes an identity function:
map(_)               # Same as map(identity)

# Anaphora in expressions:
map(_ + 1)           # Anonymous single-parameter function
filter(_ > 0)        # Anonymous predicate

# Multiple `_` — each is a separate parameter:
f: (_ * _)           # f is a 2-arg multiply function
f(3, 4)              # => 12

# Numbered anaphora for same-param reuse:
sq: (_0 * _0)        # sq(5) => 25

# Using named function + partial application:
add-one(x): x + 1
map(add-one)

# For multi-parameter needs, define a named helper:
has-y(y, h): first(h) = y
filter(has-y(target-y))   # Partial application
```

**Important**: Each `_` introduces a **separate** parameter. `_ * _`
is a 2-arg function (like `(_0 * _1)`), not `(_0 * _0)`. Use numbered
anaphora `_0 * _0` when you need to reference the same parameter twice.

**Reference**: See [Anaphora](../guide/anaphora.md) for detailed
explanation of anaphora usage.

### Anaphor Scoping: Parens Are Opaque

**Problem**: Parentheses create an anaphor scope boundary. Anaphors
inside parens form a lambda at the paren level, not at the enclosing
expression.

**Scoping rules**:

1. **Parens are opaque by default** — anaphors inside parens create a
   lambda scoped to that paren group. This applies to both `_` and
   `_0`/`_1`.

2. **Subsumption** — if the enclosing expression already has direct
   anaphors, inner paren groups become transparent (their anaphors
   join the outer scope).

3. **ArgTuples follow the same rules** — function call arguments are
   opaque unless subsumed by an outer anaphoric scope.

**Examples**:
```eu,notest
# Parens opaque — paren group forms its own lambda:
(_ + 1)               # λ(a). a + 1
(_ * _)               # λ(a, b). a * b
(_ = :quux) ∘ tag     # (λ(a). a = :quux) ∘ tag  ✓ composition works

# Without parens — whole expression is the lambda body:
_ + 1                 # λ(a). a + 1   (same result here)
_ * _                 # λ(a, b). a * b

# Parens opaque breaks cross-operator average:
(_0 + _1) / 2         # (λ(a,b). a+b) / 2  — type error!
# Correct idiom (no parens):
_0 + _1 / 2           # λ(a, b). a + b/2  — note: b is halved, not sum

# Subsumption: outer _ makes inner (_ * _) transparent:
_ + (_ * _)           # λ(a, b, c). a + (b * c)  ✓

# ArgTuple opaque by default:
map(_ + 1)            # map(λ(a). a + 1)   ✓
filter(_ > 0)         # filter(λ(a). a > 0)  ✓

# ArgTuple subsumed when outer has anaphors:
_0 * (_1 + 2)         # λ(a, b). a * (b + 2)  ✓
```

**Common mistake**: Expecting `(_0 + _1) / 2` to be a 2-argument
averaging function. Under the opaque parens model, `(_0 + _1)` forms
its own lambda and `/ 2` tries to divide that lambda by 2 — a type
error. Use a named helper instead:
```eu,notest
avg2(a, b): (a + b) / 2
zip-with(avg2, xs, ys)
```

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

## Cons Patterns vs Normal Lists

The `[h : t]` cons pattern is only valid in a **function parameter position**.
A colon inside a list literal in an expression context is not valid:

```eu
# Valid — cons pattern in a function parameter
list-head([h : t]): h

# Invalid — colon is not a list separator in expression context
bad: [1 : rest]   # parse error
```

In expression context, use `cons`, `head`, and `tail` from the prelude
instead:

```eu
xs: [1, 2, 3]
first: xs head    # = 1
rest: xs tail     # = [2, 3]
```

## Future Improvements

These gotchas highlight areas where the language could benefit from:

1. **Better Error Messages**: More specific error messages when
   precedence issues occur
2. **Linting Rules**: Static analysis to catch common precedence
   mistakes
3. **IDE Support**: Syntax highlighting and warnings for ambiguous
   expressions
4. **Documentation**: Better examples showing correct precedence usage
