# Operators and Identifiers

Eucalypt distinguishes two different types of identifier, *normal*
identifiers, like `x`, `y`, `α`, `א`, `ziggety-zaggety`, `zoom?`, and
*operator identifiers* like `*`, `@`, `&&`, `∧`, `∘`, `⊙⊙⊙`, `<>` and
so on.

It is entirely a matter of the component characters which category an
identifier falls into. Normal identifiers contain letters (including
non-ASCII characters), numbers, "-", "?", "$". Operator identifiers
contain the usual suspects and anything identified as an operator or
symbol in unicode. Neither can contain ":" or "," or brackets which
are special in eucalypt.

Any sequence of characters at all can be treated as a normal
identifier by surrounding them in single quotes. This is the only use
of single quotes in eucalypt. This can be useful when you want to use
file paths or other external identifiers as block keys for instance:

```eu,notest
home: {
  '.bashrc': false
  '.emacs.d': false
  'notes.txt': true
}

z: home.'notes.txt'
```

## Normal identifiers

Normal identifiers are brought into scope by declarations and can be
referred to without qualification in their own block or in more
nested blocks:

```eu
x: {
  z: 99
  foo: z //=> 99
  bar: {
    y: z //=> 99
  }
}
```

They can be accessed from within other blocks using the lookup
operator:

```eu
x: {
  z: 99
}

y: x.z //=> 99
```

They can be overridden using generalised lookup:

```eu
z: 99
y: { z: 100 }."z is {z}" //=> "z is 100"
```

They can be shadowed:

```eu
z: 99
y: { z: 100 r: z //=> 100 }
```

But beware trying to access the outer value:

```eu,notest
name: "foo"
x: { name: name } //=> infinite recursion
```

Accessing shadowed values is not yet easily possible unless you can
refer to an enclosing block and use a lookup.

## Prefix operators

Some operators are defined as prefix (unary) operators rather than
infix (binary) operators. These bind tightly to the expression that
follows.

For example, the `↑` operator is a tight-binding prefix form of `head`:

```eu
xs: [1, 2, 3]
first: ↑xs  //=> 1
```

Because it binds tightly (precedence 95), it works naturally in
pipelines without parentheses:

```eu
xs: [[1, 2], [3, 4]]
result: xs map(↑)  # map head over list of lists
```

Other prefix operators include `!` and `¬` for boolean negation, and
`∸` for numeric negation.

## Operator identifiers

Operator identifiers are more limited than normal identifiers.

They are brought into scope by operator declarations and available
without qualification in their own block and more nested blocks:

```eu
( l -->> r): "{l} shoots arrow at {r}"

x: {
  y: 2 -->> 3 //=> "2 shoots arrow at 3"
}
```

...and can be shadowed:

```eu
(l !!! r): l + r

y: {
  (l !!! r): l - r
  z: 100 !!! 1 //=> 99
}
```

But:

- they cannot be accessed by lookup, so there is no way of forming a
  qualified name to access an operator
- they cannot be overridden by generalised lookup

## Operator Precedence Table

Operator precedence determines how operator expressions are parsed
when parentheses are omitted — higher numbers bind more tightly. This
table is verified against `named_precedence` in
`src/core/metadata.rs` and the operator declarations in
`lib/prelude.eu`:

| Prec | Name | Assoc | Operators | Description |
|------|------|-------|-----------|-------------|
| 95 | -- | prefix | `↑` | Head (tight prefix) |
| 90 | lookup | left | `.` (built-in) | Property lookup |
| 90 | lookup | left | `~` | Safe key lookup (null-propagating) |
| 90 | call | left | (built-in) | Function call |
| 88 | bool-unary | prefix | `!`, `¬` | Boolean negation |
| 88 | bool-unary | postfix | `✓` | Not-null check (true if not null) |
| 88 | -- | -- | `∘`, `;` | Composition |
| 85 | exp | right | `^` | Power |
| 85 | exp | -- | `!!` (nth) | Indexing |
| 80 | prod | left | `*`, `/`, `÷`, `%` | Multiplication, floor division, precise division, floor modulo |
| 75 | sum | left | `+`, `-` | Addition, subtraction |
| 60 | shift | -- | (shift ops) | Reserved |
| 55 | bitwise | -- | (bitwise ops) | Reserved — `‖` (cons) also uses this numeric level, `precedence: 55` in `lib/prelude.eu` |
| 50 | cmp | left | `<`, `>`, `<=`, `>=` | Comparison |
| 45 | append | left | `++` | List concatenation |
| 45 | append | left | `<<` | Deep merge |
| 42 | map | left | `<$>` | Functor map |
| 40 | eq | left | `=`, `!=` | Equality |
| 35 | bool-prod | left | `&&`, `∧` | Logical AND |
| 30 | bool-sum | left | `\|\|`, `∨` | Logical OR |
| 20 | cat | left | *(catenation)* | Juxtaposition / pipeline |
| 15 | clause | left | `=>`, `⇒` | Cond clause builder (`condition => result`) |
| 10 | apply | left | `@` | Function application |
| 5 | meta | left | `//`, `//<<`, `//=`, `//=>`, `//=?`, `//=?>`, `//!` | Metadata and assertions |

**User-defined operators** default to left-associative, precedence 50.
Set custom values via metadata:

```eu,notest
` { associates: :right precedence: :sum }
(x +++ y): x + y
```

See [Agent Reference §2](agent-reference.md) for the full named-level
list and more examples.
