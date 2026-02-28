# Idiot Brackets

## Summary

Add user-defined Unicode bracket pairs that collect their top-level
space-separated contents into a list and pass it to a bracket
function. Inspired by idiom brackets (McBride & Paterson, 2008) but
generalised — any Unicode bracket pair can be defined with arbitrary
list-processing semantics.

## Motivation

Certain patterns require folding a binary operation across a sequence
of values. In eucalypt, this currently requires verbose nested calls:

```eucalypt
ap(ap(ap(f pure, x), y), z)
```

Idiot brackets provide a concise syntax by overriding the meaning of
catenation (spaces) inside user-defined bracket pairs:

```eucalypt
«f x y z»
```

The brackets collect the items as a list and pass them to a bracket
function that defines the combination semantics.

## Design

### 1. Bracket Definition

Bracket pairs are defined like operators — as named declarations in
any block scope. The "name" of the bracket function is the open+close
characters concatenated. The parameter uses list destructuring from the
destructuring design.

```eucalypt
# Idiom brackets using guillemets
«[x : xs]»: foldl(ap, xs, x pure)

# Monadic sequencing using double square brackets
⟦[x : xs]⟧: foldl(bind, xs, x)

# Custom bracket function
⌈[a, b, c]⌉: a + b + c
```

Metadata can be attached like any other declaration:

```eucalypt
` { doc: "Applicative idiom brackets" }
«[x : xs]»: foldl(ap, xs, x pure)
```

### 2. Bracket Usage

Inside a user-defined bracket pair, top-level catenation (spaces)
changes meaning: items are collected into a list rather than being
pipeline-applied.

```eucalypt
«f x y z»        # bracket function receives [f, x, y, z]
⟦action1 action2 action3⟧  # receives [action1, action2, action3]
```

**Operators bind normally inside brackets:**

```eucalypt
«a + 1  b * 2»   # receives [(a + 1), (b * 2)]
```

Catenation has the lowest precedence, so all infix operators bind
tighter. Only top-level spaces become list separators.

**Parentheses protect inner catenation:**

```eucalypt
«a (b c) d»      # receives [a, c(b), d]
                  # (b c) is normal pipeline application
```

**Nesting works:**

```eucalypt
«a «b c» d»      # inner «b c» evaluates first
                  # outer receives [a, result_of_inner, d]
```

### 3. Bracket Pair Recognition

Bracket pairs are recognised using Unicode general categories:

- **Opening**: Any character in Unicode category Ps (Punctuation, Open)
- **Closing**: The corresponding character in Unicode category Pe
  (Punctuation, Close) via Unicode mirroring properties
- **Excluded**: `()`, `{}`, `[]` — reserved by the language

This makes the set of available bracket pairs open-ended. Users can
choose from guillemets, angle brackets, ceiling/floor brackets, and
many others without language changes.

### 4. Scoping

Bracket functions follow the same scoping rules as operators:

- Defined in any block scope
- Visible in the defining scope and nested scopes
- Can be overridden in inner scopes
- Resolved during name resolution (cook phase)

### 5. Desugaring

`«a b c»` desugars to:

```
App(Var("«»"), List([a, b, c]))
```

The bracket function is a normal function taking a single list
argument. All list-processing semantics (folding, destructuring, etc.)
are expressed in the function body using standard eucalypt.

### 6. Fusion

The destructure fusion pass from the destructuring design applies
directly. Since the bracket function will typically destructure its
list argument immediately (e.g. `«[x : xs]»: ...`), the fusion pass
elides the list construction — values are bound directly without
allocating an intermediate list.

This means idiot brackets have zero overhead beyond the bracket
function's own computation.

## Implementation Strategy

### Lexer

Extend the lexer to recognise Unicode Ps/Pe characters as bracket
tokens. Use Rust's Unicode category detection (e.g. the
`unicode-general-category` crate or `char` methods) to classify
characters. Emit distinct token types for user-defined opening and
closing brackets (separate from the built-in `OPEN_PAREN` etc.).

### Parser

When the parser encounters a user-defined opening bracket:

1. Record the bracket type
2. Parse the contents, but treat top-level catenation as list
   collection rather than pipeline application
3. Expect the matching closing bracket
4. Emit an application node: apply the bracket name to a list of the
   collected items

### Cook / Name Resolution

The bracket function name (e.g. `«»`) is resolved during the cook
phase, like operator names. No special handling needed beyond
recognising bracket pair names as valid identifiers.

### Declaration Parsing

Recognise bracket pair declarations:

```eucalypt
«[x : xs]»: body
```

As a unary operator-like declaration where:
- The name is the concatenated bracket pair (`«»`)
- The parameter is the expression between the brackets
- The body follows the colon

### Existing Infrastructure Reused

- List destructuring (from destructuring design) for bracket function
  parameters
- Destructure fusion pass for eliding list construction
- Operator scoping and name resolution for bracket functions
- Standard function application for bracket invocation

## Dependencies

- **Destructuring design** (`2026-02-27-destructuring-named-args-design.md`):
  List destructuring and fusion are prerequisites. The bracket function
  takes a list argument and destructures it.

## Error Handling

- **Undefined bracket pair**: runtime error (or cook-phase error) when
  brackets are used but no definition is in scope
- **Mismatched brackets**: parse error when opening and closing brackets
  don't correspond
- **Empty brackets**: `«»` with no items — bracket function receives
  empty list `[]`

## Testing

Harness tests covering:

- Basic bracket definition and usage
- Multiple bracket pair types in the same scope
- Bracket functions with various list destructuring patterns
- Operators inside brackets (precedence correctness)
- Parenthesised sub-expressions inside brackets
- Nested same-type brackets
- Nested different-type brackets
- Bracket scoping (inner scope override)
- Fusion verification (no intermediate list allocation)
- Error cases (undefined, mismatched, empty)
- Idiom bracket example: applicative style
