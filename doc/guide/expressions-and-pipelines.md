# Expressions and Pipelines

Eucalypt is an expression language -- almost everything you write
evaluates to a value. This chapter covers the kinds of expressions
available and how to chain them together.

## Primitives

The basic value types:

```eu
n: 42            # number (integer)
f: 3.14          # number (float)
s: "hello"       # string
b: true          # boolean
x: null          # null
sym: :keyword    # symbol
```

Symbols are lightweight identifiers written with a leading colon. They
are often used as tags or enumeration values.

## Arithmetic

Standard numeric operators:

```eu
a: 3 + 4     //=> 7
b: 10 - 3    //=> 7
c: 6 * 7     //=> 42
d: 15 / 4    //=> 3.75
e: 15 % 4    //=> 3
f: 2 ** 10   //=> 1024
```

## Comparison

```eu
a: 3 < 4     //=> true
b: 3 > 4     //=> false
c: 3 <= 3    //=> true
d: 3 >= 4    //=> false
e: 3 == 3    //=> true
f: 3 != 4    //=> true
```

## Boolean logic

```eu
a: true && false  //=> false
b: true || false  //=> true
c: not(true)      //=> false
```

## String interpolation

Double-quoted strings support embedded expressions in curly braces:

```eu
name: "world"
greeting: "hello, {name}" //=> "hello, world"
calc: "2 + 2 = {2 + 2}"  //=> "2 + 2 = 4"
```

See [String Interpolation](string-interpolation.md) for more detail.

## Lists

Square brackets create lists:

```eu
xs: [1, 2, 3]
ys: ["a", "b", "c"]
mixed: [1, "two", true]
nested: [[1, 2], [3, 4]]
```

## Function application

There are two ways to apply functions:

**Parenthesised application** passes arguments in parentheses:

```eu
double(x): x * 2
result: double(21) //=> 42
```

**Catenation (juxtaposition)** applies a single argument by placing
it next to the function:

```eu
double(x): x * 2
result: 21 double //=> 42
```

Catenation applies the value on the **left** as the first argument to
the function on the **right**. This enables a natural pipeline style:

```eu
double(x): x * 2
negate(x): 0 - x
result: 21 double negate //=> -42
```

Read this as: take 21, double it, negate it.

## Whitespace matters

Catenation is sensitive to whitespace. The expression `f(x)` is
parenthesised application, but `f (x)` is catenation of `f` and `(x)`.

```eu
add(x, y): x + y
increment: add(1)

a: increment(9)  //=> 10
b: 9 increment   //=> 10
```

## Sections

A **section** is a partially applied operator. Wrap an operator with
one argument in parentheses to create a function:

```eu
xs: [1, 2, 3]
doubled: xs map (* 2)  //=> [2, 4, 6]
bumped: xs map (+ 10)  //=> [11, 12, 13]
```

The missing operand becomes the parameter. Both left and right
sections work:

```eu
halve: (/ 2)
result: halve(10) //=> 5
```

## Partial application and currying

All functions are curried. Apply fewer arguments to get a new
function:

```eu
add(x, y): x + y
add5: add(5)
result: add5(3) //=> 8
```

This works naturally with catenation:

```eu
add(x, y): x + y
result: 3 add(5) //=> 8
```

## Operator precedence

Eucalypt operators have standard precedences (highest binds tightest):

| Precedence | Operators                    |
|------------|------------------------------|
| 90         | `.` (lookup)                 |
| 85         | `**` (exponentiation)        |
| 80         | `*`, `/`, `%` (product)      |
| 75         | `+`, `-` (sum)               |
| 50         | `<`, `>`, `<=`, `>=` (comparison) |
| 40         | `==`, `!=` (equality)        |
| 35         | `&&` (boolean and)           |
| 30         | `||` (boolean or)            |
| 20         | catenation                   |

See [Operators](operators.md) for the full table and custom operator
definitions.

## Conditionals

The `if` expression chooses between two branches:

```eu
abs(x): if(x < 0, 0 - x, x)
a: abs(5)  //=> 5
b: abs(-3) //=> 3
```

`if` is a regular function, not special syntax. Both branches are
always present (there is no `if` without `else`).

## Pipelines in practice

Combine catenation with sections and partial application to build
readable data pipelines:

```eu
data: [3, 1, 4, 1, 5, 9]
result: data filter(> 3) map(* 10) //=> [40, 50, 90]
```

Read left to right: start with `data`, keep elements greater than 3,
multiply each by 10.

## Let expressions

Use `let ... in ...` to bind intermediate values:

```eu,notest
result: let(x: 10, y: 20, x + y)
```

The final argument is the body expression. Earlier bindings are in
scope for later ones and for the body.

## Next steps

- [Lists and Transformations](lists-and-transformations.md) -- working
  with lists in depth
- [Functions and Combinators](functions-and-combinators.md) -- more on
  defining and composing functions
