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

```eu,notest
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

```eu,notest
... {
  a: 1
  b: 2
  c: 3
}
```

...which may themselves have blocks as values...

```eu,notest
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

Blocks may also have a single expression, preceding its declarations
which constitutes **block metadata**. This is optional.

So the block pattern is braces (except for the unit level) containing,
optionally metadata, then any number of declarations.

```
{ «metadata expression»?  [«declaration»]* }
```

Both the metadata expression and the declarations may contain blocks.
This recursive application of the block pattern defines the major
structure of any eucalypt code.

So far all the declarations we have seen have been **property
declarations** which contain a name and an expression, separated by a
colon.

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

Function parameters can be **destructuring patterns** as well as
simple names. A block pattern extracts named fields from a block
argument; a list pattern extracts positional elements from a list
argument:

```eu
# Block destructuring — shorthand binds field name as variable name
sum-xy({x y}): x + y

# Block destructuring — rename binds field under a new variable name
product-ab({x: a  y: b}): a * b

# Mixed shorthand and rename
mixed({x  y: b}): x + b

# Fixed-length list destructuring
f-sum([a, b, c]): a + b + c
f-first([a, b]): a

# Head/tail list destructuring — colon separates fixed heads from tail
get-head([x : xs]): x
get-tail([x : xs]): xs
drop-two([a, b : rest]): rest
```

Juxtaposed call syntax passes a block or list literal as a single
argument without parentheses. No space between the function name and
the opening bracket:

```eu,notest
# f{...} is sugar for f({...})
sum-xy{x: 10 y: 20}

# f[...] is sugar for f([...])
add-pair[1, 2]
```

Combined with block destructuring, this gives named arguments:

```eu
greet({name greeting}): "{greeting}, {name}!"
greet{name: "Alice" greeting: "Hello"}    # => "Hello, Alice!"
```

Juxtaposed syntax also works in definitions — the bracket or brace
is written directly against the function name with no space. This is
sugar for the parenthesised destructuring form:

```eu
# f[x, y]: ...  is sugar for  f([x, y]): ...
add-pair[a, b]: a + b

# f{x y}: ...   is sugar for  f({x y}): ...
add-block{x y}: x + y

# f[h : t]: ... is sugar for  f([h : t]): ...
my-head[h : t]: h
```

Destructuring patterns can be mixed with normal parameters:

```eu
f(n, [a, b]): n * (a + b)
```

The `‖` operator (U+2016, DOUBLE VERTICAL LINE) prepends an element to
a list. It is right-associative:

```eu
1 ‖ [2, 3]        # => [1, 2, 3]
1 ‖ 2 ‖ [3]       # => [1, 2, 3]
```

See [Functions and Combinators](../guide/functions-and-combinators.md)
for more detail on destructuring.

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

In addition to named operators, you can define **idiot brackets** —
custom Unicode bracket pairs that wrap an expression and apply a
function to it. The name is inspired by *idiom brackets* from
applicative functor notation, but they are a general bracket
overloading mechanism. A bracket pair declaration uses a Unicode
bracket pair wrapping a single parameter directly (paren-free style):

```eu
# Ceiling brackets double
⌈ x ⌉: x * 2

# Floor brackets increment
⌊ x ⌋: x + 1
```

The older paren-wrapped style is still supported for backwards compatibility:

```eu
(⌈ x ⌉): x * 2    # paren style — still valid
```

Once declared, the bracket pair can be used as an expression:

```eu,notest
doubled: ⌈ 3 + 4 ⌉    # => 14
bumped:  ⌊ 5 ⌋         # => 6
```

The declaration `⌈ x ⌉: body` defines a function named `⌈⌉` (open
then close bracket) that takes one argument `x` and returns `body`.
Using `⌈ expr ⌉` in an expression calls that function with `expr`.

The following Unicode bracket pairs are built-in and can be used for
idiot brackets without any registration:

| Open | Close | Name |
|------|-------|------|
| `⟦`  | `⟧`   | Mathematical white square brackets |
| `⟨`  | `⟩`   | Mathematical angle brackets |
| `⟪`  | `⟫`   | Mathematical double angle brackets |
| `⌈`  | `⌉`   | Ceiling brackets |
| `⌊`  | `⌋`   | Floor brackets |
| `⦃`  | `⦄`   | Mathematical white curly brackets |
| `⦇`  | `⦈`   | Mathematical white tortoise shell brackets |
| `⦉`  | `⦊`   | Mathematical flattened parentheses |
| `«`  | `»`   | French guillemets |
| `【` | `】`  | CJK lenticular brackets |
| `〔` | `〕`  | CJK tortoise shell brackets |
| `〖` | `〗`  | CJK white lenticular brackets |
| `〘` | `〙`  | CJK white tortoise shell brackets |
| `〚` | `〛`  | CJK white square brackets |

### Monadic blocks

Eucalypt supports monadic sequencing — analogous to Haskell `do`-notation —
through two mechanisms: **bracket pair definitions** and **block metadata**.

#### Bracket pair definitions

A bracket pair gains a **monad spec** when declared with an empty block `{}`
as its parameter and a body marked with `:monad` metadata, supplying `bind`
and `return` function names:

```eu,notest
# Explicit bind/return functions
⟦{}⟧: { :monad bind: my-bind  return: my-return }

# Namespace reference — delegates to a block with bind and return members
⟦{}⟧: { :monad namespace: my-monad }
```

A bracket pair declared with an empty block `{}` as its parameter (e.g.
`⟦{}⟧: …`) is registered as **block-mode**.  When such a bracket pair is used,
its content is parsed as a sequence of `name: monadic-action` declarations
(a **bracket block**).  The closing bracket must be followed by a dot and a
return expression:

```eu,notest
result: ⟦ a: ma  b: mb ⟧.return_expr
```

The parser determines the parse mode from a registry built by pre-scanning the
source file for `⟦{}⟧:` declarations.  Only bracket pairs explicitly declared
in this way are parsed as block-mode; other bracket pairs are always
expression-mode regardless of their content.

#### Block metadata forms

Regular blocks can also be desugared monadically when the block carries monad
metadata **and** is immediately followed by `.return_expr` in an expression.
Five forms are accepted:

| Form | Syntax | Monad source |
|------|--------|-------------|
| 1 | `{ :name decls }.expr` | Namespace `name` in scope |
| 2 | `{ { monad: name } decls }.expr` | Namespace `name` in scope |
| 3 | `{ { :monad namespace: name } decls }.expr` | Namespace `name` in scope |
| 4 | `{ { :monad bind: f return: r } decls }.expr` | Explicit `f`/`r` functions |
| 5 | `⟦{}⟧: { :monad namespace: name }` (bracket def) | Namespace `name` in scope |

For namespace forms (1–3 and 5), the named value must be a block in scope with
`bind` and `return` member functions.  The desugarer emits `name.bind(…)` and
`name.return(…)` lookup expressions.

#### Desugaring

All monadic block forms desugar to a right-to-left bind chain:

```
bind(ma, (a): bind(mb, (b): return(return_expr)))
```

All declarations are bind steps.  Each bound name is in scope for later actions
and for the return expression.  The return expression may be any single element:
a name (`.r`), a parenthesised expression (`.(x + y)`), a list, or a block.

**Example — identity monad (bracket pair, explicit functions):**

```eu
id-bind(ma, f): f(ma)
id-return(a): a

⟦{}⟧: { :monad bind: id-bind  return: id-return }

result: ⟦ x: 10  r: x + 5 ⟧.r     # => 15
```

**Example — maybe monad (namespace reference via block metadata):**

```eu
maybe: { bind(ma, f): if(ma = [], [], f(ma head))  return(a): [a] }

just:    { :maybe x: [1]  y: [2] }.(x + y)   # => [3]
nothing: { :maybe x: []   y: [2] }.(x + y)   # => []
```

**Example — maybe monad (bracket pair with namespace reference):**

```eu
maybe: { bind(ma, f): if(ma = [], [], f(ma head))  return(a): [a] }

⌈{}⌉: { :monad namespace: maybe }

just:    ⌈ x: [1]  y: [2] ⌉.(x + y)   # => [3]
nothing: ⌈ x: []   y: [2] ⌉.(x + y)   # => []
```

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

```eu,notest
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

```eu,notest
result: [1, 2, 3] map(+1) filter(odd?) //=> [3]
```

(`//=>` is an assertion operator which causes a panic if the left and
right hand expressions aren't found to be equal at run time, but
returns that value if they are.)

> **Note:** There are no explicit lambda expressions in Eucalypt right
> now. For simple cases, expression or string anaphora should do the
> job. For more involved cases, you should use a named function
> declaration. See [Anaphora](../guide/anaphora.md) for more.
