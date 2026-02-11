# Operators

Eucalypt has a rich set of built-in operators and lets you define your
own with custom precedence and associativity.

## Built-in operators

### Arithmetic

```eu
a: 3 + 4    //=> 7
b: 10 - 3   //=> 7
c: 6 * 7    //=> 42
d: 15 / 4   //=> 3
e: 15 % 4   //=> 3
```

### Comparison

```eu
a: 3 < 4   //=> true
b: 3 > 4   //=> false
c: 3 <= 3  //=> true
d: 3 >= 4  //=> false
e: 3 = 3   //=> true
```

### Boolean

```eu
a: true && false  //=> false
b: true || false  //=> true
c: not(true)      //=> false
```

### List append

```eu
result: [1, 2] ++ [3, 4] //=> [1, 2, 3, 4]
```

### Map operator

The `map` function can also be used as an operator with `|`:

```eu,notest
result: [1, 2, 3] | (* 2)
```

### Lookup

The dot operator looks up a key in a block:

```eu
point: { x: 3 y: 4 }
result: point.x //=> 3
```

### Head prefix operator

The `^` prefix operator extracts the head of a list:

```eu,notest
first: ^[1, 2, 3]  # => 1
```

## Precedence table

Operators bind at these precedences (highest binds tightest):

| Precedence | Category        | Operators                          | Associativity |
|------------|-----------------|-------------------------------------|---------------|
| 90         | Lookup          | `.`                                 | Left          |
| 88         | Boolean unary   | `not`                               | --            |
| 80         | Product         | `*`, `/`, `%`                       | Left          |
| 75         | Sum             | `+`, `-`                            | Left          |
| 50         | Comparison      | `<`, `>`, `<=`, `>=`                | Left          |
| 45         | Append          | `++`                                | Right         |
| 42         | Map             | `\|`                                | Left          |
| 40         | Equality        | `=`                                 | Left          |
| 35         | Boolean and     | `&&`                                | Left          |
| 30         | Boolean or      | `\|\|`                              | Left          |
| 20         | Catenation      | (juxtaposition)                     | Left          |
| 10         | Apply           | `@`                                 | Right         |
| 5          | Meta            | `` ` ``                             | Right         |

## Defining custom operators

### Binary operators

Declare a binary operator by placing a symbolic name between two
parameters:

```eu
(l <+> r): l + r + 1
result: 3 <+> 4 //=> 8
```

Operator names are sequences of symbolic characters.

### Prefix operators

Place the operator before the parameter:

```eu,notest
(! x): not(x)
```

### Postfix operators

Place the operator after the parameter:

```eu,notest
(x !!): x * x
```

## Setting precedence and associativity

Attach metadata with a backtick before the operator declaration:

```eu,notest
` { associates: :right precedence: 75 }
(l <+> r): l + r
```

Without metadata, custom operators default to a low precedence. The
`associates` key accepts `:left`, `:right`, or `:none`. The
`precedence` key accepts a number (higher binds tighter).

## Operator scoping

Operators follow the same scoping rules as other declarations. An
operator defined in a block is visible within that block and any
nested blocks:

```eu,notest
tools: {
  (l <+> r): l + r + 1
  result: 3 <+> 4  # works here
}
```

To use an operator from another block, import it.

## Composition operators

### Forward composition (`;`)

Compose two functions left to right:

```eu
double(x): x * 2
negate(x): 0 - x
f: double ; negate
result: f(5) //=> -10
```

### Backward composition

`∘` composes right to left (mathematical order):

```eu,notest
g: negate ∘ double   # double first, then negate
```

## Merge operators

### Shallow merge (catenation)

Juxtapose two blocks to merge them:

```eu
base: { a: 1 b: 2 }
overlay: { b: 3 c: 4 }
result: base overlay //=> { a: 1 b: 3 c: 4 }
```

### Deep merge (`<<`)

Recursively merge nested blocks:

```eu,notest
base: { x: { a: 1 b: 2 } }
extra: { x: { c: 3 } }
result: base << extra
```

## Practical patterns

### Sections in pipelines

Wrap an operator with one argument in parentheses to create a function:

```eu
data: [1, 2, 3, 4, 5]
result: data filter(> 3) map(* 10) //=> [40, 50]
```

### Fold with operators

Pass operators directly to fold:

```eu
xs: [1, 2, 3, 4]
total: xs foldl(+, 0) //=> 10
```

## Next steps

- [Anaphora](anaphora.md) -- shorthand expressions using `_`
- [Block Manipulation](block-manipulation.md) -- working with blocks
  as data
