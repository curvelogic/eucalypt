# YAML Embedding

Eucalypt can be embedded in YAML files via the following tags:

- `eu`
- `eu::suppress`
- `eu::fn`

The YAML embedding is not as capable as the native Eucalypt syntax but
it is rich enough to be used for many YAML templating use cases,
particularly when combined with the ability to specify several inputs
on the command line.

## Evaluating eucalypt expressions

As you would expect, YAML mappings correspond to Eucalypt blocks and
bind names just as Eucalypt blocks do and YAML sequences correspond to
Eucalypt lists.

YAML allows a wide variety of forms of expressing these (block styles
and flow styles), to the extent that JSON is valid YAML.

Eucalypt expressions can be evaluated using the `!eu` tag and have
access to all the names defined in the YAML unit and any others
brought into scope by specifying inputs on the command line.

```yaml
values:
  x: world
  y: hello

result: !eu "{values.y} {values.x}!"
```

...will render as:

```yaml
values:
  x: world
  y: hello

result: Hello World!
```

## Suppressing rendering

Items can be hidden using the `eu::suppress` tag. This is equivalent
to `:suppress` metadata in the eucalypt syntax.

```yaml
values: !eu::suppress
  x: world
  y: hello

result: !eu "{values.y} {values.x}!"
```

...will render as:

```yaml
result: Hello World!
```

## Defining functions

Functions can be defined using `eu::fn` and supplying an argument
list:

```yaml
values: !eu::suppress
  x: world
  y: hello
  greet: !eu::fn (h, w) "{h} {w}!"

result: !eu values.greet(values.y, values.x)
```

...will render as:

```yaml
result: Hello World!
```

## The escape hatch

Larger chunks of eucalypt syntax can be embedded using YAML's support
for larger chunks of text, combined with `!eu`. Using this workaround
you can access capabilities of eucalypt that are not yet available in
the YAML embedding. (Although operators cannot be made available in
YAML blocks because of the way that operator names are bound - see
[Operator Precedence Table](../reference/operators-and-identifiers.md).)

```yaml
block: !eu |
  {
    x: 99
    (l ^^^ r): "{l} <_> {r}"
    f(n): n ^^^ x
  }

result: block.f(99)
```
