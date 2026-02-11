# Language Syntax Reference

Eucalypt has a native syntax which emphasises the mappings-and-lists
nature of its underlying data model but adds enhancements for
functions and expressions. Eucalypt is written in `.eu` files.

While `eu` happily processes YAML inputs with embedded expressions,
many features are not yet available in the YAML embedding and the
embedded expressions are themselves in Eucalypt syntax, so it is
necessary to have an overview of how the syntax works to do anything
interesting with Eucalypt.

A few aspects are unorthodox and experimental.

## Overview

Eucalypt syntax comes about by the overlapping of two sub-languages.

- the *block DSL* is how you write blocks and their declarations
- the *expression DSL* is how you write expressions

They are entwined in a fairly typical way: block literals (from the
*block DSL*) can be used in expressions (from the *expression DSL*)
and expressions (from the *expression DSL*) appear in declarations
(from the *block DSL*).

Comments can be interspersed throughout. Eucalypt only has line level
comments.

```eu
foo: bar # Line comments start with '#' and run till the end of the line
```

> **Note:** If you feel you need a block comment, you can use an
> actual block or a string property within a block and mark it with
> annotation metadata `:suppress` to ensure it doesn't appear in
> output.

Eucalypt has two types of names:

- normal names, which are largely alphanumeric (e.g. `f`, `blah`,
  `some-thing!`, `ॵ`) and are used to name properties and functions
- operator names, which are largely symbolic (e.g. `&&&`, `∧`, `-+-|`,
  `⊚`) and are used to name operators

See [Operator Precedence Table](operators-and-identifiers.md) for more.

## The block DSL

A **block** is surrounded by curly braces:

```eu
... { ... }
```

...and contains declarations...

```eu
... {
  a: 1
  b: 2
  c: 3
}
```

...which may themselves have blocks as values...

```eu
... {
  foo: {
    bar: {
      baz: "hello world"
    }
  }
}
```

The top-level block in a file (a **unit**) does not have braces:

```eu
a: 1
b: 2
c: 3
```

So far all these declarations have been **property declarations** which
contain a name and an expression, separated by a colon.

Commas are entirely optional for delimiting declarations. Line endings
are not significant. The following is a top-level block of three
**property declarations**.

```eu
a: 1 b: 2 c: 3
```

There are other types of declarations. By specifying a parameter list,
you get a **function declaration**:

```eu
# A function declaration
f(x, y): x + y

two: f(1, 1)
```

...and using some brackets and suitable names, you can define
operators too, either binary:

```eu
# A binary operator declaration
(x ^|^ y): "{x} v {y}"
```

...or prefix or postfix unary operators:

```eu
# A prefix operator declaration
(¬ x): not(x)

# A postfix operator declaration
(x ******): "maybe {x}"
```

Eucalypt should handle unicode gracefully and any unicode characters
in the symbol or punctuation classes are fine for operators.

To control the precedence and associativity of user defined operators,
you need metadata annotations.

**Declaration annotations** allow us to specify arbitrary metadata
against declarations. These can be used for documentation and similar.

To attach an annotation to a declaration, squeeze it between a leading
backtick and the declaration itself:

```eu
` { doc: "This is a"}
a: 1

` { doc: "This is b"}
b: 2
```

Some metadata activate special handling, such as the `associates` and
`precedence` keys you can put on operator declarations:

```eu
` { doc: "`(f ∘ g)` - return composition of `f` and `g`"
    associates: :right
    precedence: 88 }
(f ∘ g): compose(f,g)
```

Look out for other uses like `:target`, `:suppress`, `:main`.

Finally, you can specify metadata at a unit level. If the first item
in a unit is an expression, rather than a declaration, it is treated
as metadata that is applied to the whole unit.

```eu
{ :doc "This is just an example unit" }
a: 1 b: 2 c: 3
```

## The expression DSL

Everything that can appear to the right of the colon in a declaration
is an expression and defined by the expression DSL.

### Primitives

First there are primitives.

...numbers...

```eu
123
```

```eu
-123
```

```eu
123.333
```

...double quoted strings...

```eu
"a string"
```

...**symbols**, prefixed by a colon...

```eu
:key
```

...which are currently very like strings, but used in circumstances
where their internal structure is generally not significant (i.e. keys
in a block's internal representation).

Finally, booleans (`true` and `false`) are pre-defined constants. As
is (`null`) which is a value which renders as YAML or JSON's version
of null but is not used by Eucalypt itself.

### Block literals

Block literals (in braces, as defined in the *block DSL*) are
expressions and can be the values of declarations or passed as
function arguments or operands in any of the contexts below:

```eu
foo: { a: 1 b: 2 c: 3}
```

### List literals

List literals are enclosed in square brackets and contain a comma
separated sequence of expressions:

```eu
list: [1, 2, :a, "boo"]
```

### Names

Then there are **names**, which refer to the surrounding context. They
might refer to properties:

```eu
x: 22
y: x
```

...or *functions*:

```eu
add-one(x): 1 + x
three: add-one(2)
```

...or *operators*:

```eu
(x &&& y): [x, x, x, y]
z: "da" &&& "dum"
```

### Calling functions

Functions can be applied by suffixing an argument list in parens, with
*no intervening whitespace*:

```eu
f(x, y): x + y
result: f(2, 2) # no whitespace
```

In the special case of applying a single argument, *"catenation"* can
be used:

```eu
add-one(x): 1 + x
result: 2 add-one
```

...which allows succinct expressions of pipelines of operations.

In addition, functions are curried so can be partially applied:

```eu
add(x, y): x + y
increment: add(1)
result: 2 increment
```

...and placeholder underscores (or *expression anaphora*) can be used
to define simple functions without the song and dance of a function
declaration:

```eu
f: if(tuesday?, (_ * 32 / 12), (99 / _))
result: f(3)
```

In fact, in many cases the underscores can be omitted, leading to a
construct very similar to Haskell's *sections* only even brackets
aren't necessary.

> **Note:** Eucalypt uses its knowledge of the fixity and
> associativity of each operator to find "gaps" and fills them with the
> unwritten underscores. This is great for simple cases but worth
> avoiding for complicated expressions.

```eu
increment: + 1
result: 2 increment (126 /)
```

Both styles of function application together with partial application
and sectioning can all be applied together:

```eu
result: [1, 2, 3] map(+1) filter(odd?) //=> [3]
```

(`//=>` is an assertion operator which causes a panic if the left and
right hand expressions aren't found to be equal at run time, but
returns that value if they are.)

> **Note:** There are no explicit lambda expressions in Eucalypt right
> now. For simple cases, expression or string anaphora should do the
> job. For more involved cases, you should use a named function
> declaration. See [Anaphora](../guide/anaphora.md) for more.
