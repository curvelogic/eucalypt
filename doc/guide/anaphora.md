# Anaphora (Implicit Parameters)

Eucalypt doesn't have a lambda syntax in itself and prefers to
encourage other approaches in most cases where you would use a lambda.

- named functions
- function values from composites, combinators, partials
- anaphoric expressions, blocks or strings

However, through the combination of two Eucalypt features, namely
*block anaphora* and *generalised lookup*, you can express arbitrary
lambdas as we'll see below.

The various alternatives are considered one by one.

## Named functions

Very likely, the clearest way to square a list of numbers is to map an
explicitly named `square` function across it.

```eu
square(x): x * x
squares: [1, 2, 3] map(square) //=> [1, 4, 9]
```

The drawbacks of this are:
- polluting a namespace with a name that is needed only once
- arguably, a slightly tedious verbosity

The first can be dealt with as follows:

```eu
squares: { square(x): x * x }.([1, 2, 3] map(square)) //=> [1, 4, 9]
```

This exploits a feature called *generalised lookup*.

Why "generalised lookup"? In the simple case below, the dot signifies
the "lookup" of key `a` in the block preceding the dot:

```eu
x: { a: 3 b: 4 }.a //=> 3
```

We can generalise this by allowing arbitrary expressions in place of
the `a` by evaluating the expression after the dot in the context of
the namespace introduced by the block to the left.

```eu
x: { a: 3 b: 4 }.(a + b) //=> 7
```

It works for any expression after the dot:

```eu
x: { a: 3 b: 4 }.[a, b] //=> [3, 4]
y: { a: 3 b: 4 }.{ c: a + b } //=> { c: 7 }
z: { a: 3 b: 4 }."{a} and {b}" //=> "3 and 4"
```

> **Warning:** This is very effective for short and simple expressions
> but quickly gets very complicated and hard to understand if you use
> it too much. Nested or iterated generalised lookups are usually a
> bad idea.

In the `squares` example above, generalised lookup is used to restrict
the scope in which `square` is visible right down to the only
expression which needs it.

However in the case of a simple expression like the squaring example,
a neater approach is to use *expression anaphora*.

## Expression Anaphora

Any expression can become a function by referring to implicit
parameters known as expression anaphora.

These parameters are called `_0`, `_1` `_2`, and so on. There is also
an unnumbered anaphor, `_`, which we'll come back to.

Just referring to these parameters is enough to turn an expression
into a lambda.

So an expression that refers `_0` and `_1` actually defines a function
accepting two parameters:

```eu,notest
xs: zip-with(f, [1, 2, 3], [1, 2, 3]) //=> [3, 6, 9]

# or more succinctly
xs: zip-with(_0 + 2 * _1, [1, 2, 3], [1, 2, 3]) //=> [3, 6, 9]
```

> **Warning:** Anaphora are intended for use in simple cases where
> they are readable and readily understood. The scope of the implicit
> parameters is not easy to work out in complicated contexts. (It does
> not extend past catenation or commas in lists or function application
> tuples.) Anaphoric expressions are not, and not intended to be, a
> fully general lambda syntax. Unlike explicit lambda constructions,
> you cannot nest anaphoric expressions.

```eu
squares: [1, 2, 3] map(_0 * _0) //=> [1, 4, 9]
```

In cases where the position of the anaphora in the expression matches
the parameter positions in the function call, you can omit the
numbers. So, for instance, `_0 + _1` can simply be written `_ + _`,
and `_0 * _1 + x * _2` can be written `_ * _ + x * _`.

Each `_` represents a *different* implicit parameter, which is why we
had to write `_0 * _0` in our squares example - it was important that
the same parameter was referenced twice.

Sometimes you need explicit parentheses to clarify the scope of
expression anaphora:

```eu
block: { a: 1 b: 2 }

x: block (_.a) //=> 1
y: block lookup(:a) //=> 1
#
# BUT NOT: block _.a
#
```

## Sections

Even more conciseness is on offer in some cases where the anaphora can
be entirely omitted. Eucalypt will automatically insert anaphora
when it detects *gaps* in an expression based on its knowledge of an
operator's type.

So it will automatically read `(1 +)` as `(1 + _)`, for example,
defining a function of one parameter. Or `(*)` as `(_ * _)`, defining
a function of two parameters. The parentheses may not even be
necessary to delimit the expression:

```eu
x: foldl(+, 0, [1, 2, 3]) = 6
```

Again, use of sections is recommended only for short expressions or
where the intention is obvious. This level of terseness can lead to
baffling code if abused.

## Block Anaphora

Expression anaphora are scoped by an expression which is roughly
defined as something within parentheses or something which can be the
right hand side of a declaration.

Sometimes however you would like to define a block-valued function.
Imagine you wanted a two-parameter function which placed the
parameters in a block with keys `x` and `y`:

```eu
f(x, y): {x: x y: y }
```

An attempt to define this using expression anaphora would fail. This
defines a block with two identity functions:

```eu,notest
f: {x: _ y: _ }
```

Instead, you can use *block anaphora* which are scoped by the block
that contains them.

The block anaphora are named `•0`, `•1`, `•2` with a special
unnumbered anaphor `•`, playing the same role as `_` does for
expression anaphora.

`•` is the BULLET character (usually Option-8 on a Mac but you may
find other convenient ways to type it). The slightly awkward character
is chosen firstly because it looks like a hole and therefore makes
sense as a placeholder, and secondly to discourage overuse of the
feature...

The following defines the function we want:

```eu
f: { x: • y: • }
```

...and can, of course, be used:

```eu
x: [[1, 2], [3, 4], [5, 6]] map({ x: • y: • } uncurry)
```

## Pseudo-lambdas

Astute observers may realise that by combining generalised lookup and
block anaphora you end up with something that's not a million miles
away from a lambda syntax:

```eu
f: { x: • y: • }.(x + y)
```

Indeed this does allow declaration of anonymous functions with named
parameters and can occasionally be useful but it still falls short of
a fully general lambda construction because it cannot (at least for
now) be nested.

## String Anaphora

Analogously, Eucalypt's string interpolation syntax allows the use of
anaphora `{0}`, `{1}`, `{2}` and the unnumbered `{}` to define
functions which return strings.

```eu
x: [1, 2, 3] map("#{}") //=> ["#1", "#2", "#3"]
```

## Summary

There are lots of ways to define functions but the clearest is just
defining them with names using function declarations and for anything
even slightly complicated this should be the default. The only things
you should be tempted to define on the spot are things that are simple
enough that the various species of anaphora can handle them neatly.
