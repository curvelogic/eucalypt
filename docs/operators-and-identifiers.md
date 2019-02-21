# Operators and Identifiers

Eucalypt distinguishes two different types of identifier, *normal*
identifiers, like `x`, `y`, `α`, `א`, `ziggety-zaggety`, `zoom?`, and
*operator identifiers* like `*`, `@`, `&&`, `∧`, `∘`, `⊙⊙⊙`, `<>` and
so on.

It is entirely a matter of the component characters which category and
identifier falls into. Normal identifiers contain letters (including
non-ascii characters), numbers, "-", "?", "$". Operator identifiers
contain the usual suspects and anything identified as an operator or
symbol in unicode. Neither can contain ":" or "," or brackets which
are special in eucalypt.

Any sequence of characters at all can be treated as a normal
identifier by surrounding them in single quotes. This is the only use
of single quotes in eucalypt. This can be useful when you want to use
file paths or other external identifiers as block keys for instance:

```eu

home: {
  '.bashrc': false
  '.emacs.d': false
  'notes.txt': true
}

z: home.'notes.txt'

```

## Normal identifiers

Normal operators are brought into scope by declarations and can be
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

```eu
name: "foo"
x: { name: name } //=> infinite recursion
```

Accessing shadowed values is not yet easily possible unless you can
refer to an enclosing block and use a lookup.

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
