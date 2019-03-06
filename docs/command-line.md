# `eu` command line

Eucalypt is available as a command line tool, `eu`, which reads inputs
and writes outputs.

Everything it does in between is purely functional and there is no
mutable state.

It is intended to be simple to use in unix pipelines.

```sh
eu --version # shows the current eu version
eu --help # lists command line options
```

## Inputs

### Files / *stdin*

`eu` can read several inputs, specified by command line arguments.

Inputs specify text data from:

 - files
 - stdin
 - internal resources (ignored for now)
 - (in future) HTTPS URLS or Git refs

...of which the first two are the common case. In the simplest case,
file inputs are specified by file name, stdin is specified by `-`.

So

```sh
eu a.yaml - b.eu
```

...will read input from `a.yaml`, stdin and `b.eu`.
Each will be read into **eucalypt**'s core representation and merged
before output is rendered.

### Input format

Inputs must be one of the formats that **eucalypt** supports, which
at present, are:

 - yaml
 - json
 - toml
 - csv
 - text

Of these the first three (yaml, json, toml) return blocks and the last
two return lists. Inputs that return lists frequently to be named (see
below) to allow them to be used.

Usually the format is inferred from file extension but it can be
overridden on an input by input basis using a `format@` prefix.

For instance:

```sh
eu yaml@a.txt json@- yaml@b.txt
```

...will read YAML from `a.txt`, JSON from stdin and YAML from `b.txt`.

### Named inputs

Finally inputs can be *named* using a `name=` prefix. This alters the
way that data is merged by making the contents of an input available
in a block or list with the specified name, instead of at the top
level.

Suppose we have two inputs:

```yaml
foo: bar
```

```eu
x: 42
```

then

```sh
eu a.yaml b.eu
```

would generate:

```yaml
foo: bar
x: 42
```

but

```sh
eu data=a.yaml b.eu
```

would generate:

```yaml
data:
  foo: bar

x: 42
```

This can be useful for various reasons, particularly when:

- the form of the input's content is not known in advance
- the input's content is a list rather than a block

### Full input syntax

The full input syntax is therefore:

```
[name=][format@][URL/file]
```

This applies at the command line and also when specifying
[imports](imports.md) in `.eu` files.

### *stdin* defaulting

When no inputs are specified and `eu` is being used in a pipeline, it
will accept input from *stdin* by default, making it easy to pipe JSON
or YAML from other tools into eu.

For example, this takes JSON from the `aws` CLI and formats it as YAML
to stdout.

```sh
aws s3-api list-buckets | eu
```

### How inputs are merged

When several inputs are listed, names from earlier inputs become
available to later inputs, but the content that will be rendered is
that of the final input.

The common use case is a final input which contains logic to inspect
or process data provided by the previous inputs (potentially coming in
from previous processing on stdin).

If you want to render contents of earlier inputs verbatim, you need a
named input to provide a name for that content which you can then use.

## Outputs

In the current version, `eu` can only generate one output.

### Output format

Output is rendered as YAML by default. Other formats can be specified
using the `-x` command line option:

```sh
eu -x json # for JSON
eu -x text # for plain text
```

JSON is such a common case that there is a shortcut: `-j`.

### Output targets

By default, **eucalypt** renders all the content of the final input to
output.

There are various ways to override this. First, `:target` metadata can
be specified in the final input to identify different parts for
potential export.

To list the **targets** found in the specified inputs, use the `-t`
flag.

```sh
eu -l
```

...and a particular target can be selected for render using `-t`.

```sh
eu -t my-target
```

If there is a **target** called "main" it will be used by default
unless another target is specified.

## Evaluands

In addition to inputs, an *evaluand* can be specified at the command
line. This is a **eucalypt** expression which has access to all names
defined in the inputs and replaces the input body or targets as the
data to export.

It can be used to select content or derive values from data in the
inputs:

```console
$ aws s3api list-buckets | eu -e 'Buckets map(lookup(:CreationDate)) head'
2016-12-25T14:22:30.000Z
```

...or just to test out short expressions or command line features:

```console
$ eu -e '{a: 1 b: 2 * 2}' -j
{"a": 1, "b": 4}
```

## Suppressing prelude

A standard *prelude* containing many functions and operators is
automatically prepended to the input list.

This can be suppressed using `-Q` if it is not required or if you
would like to provide an alternative.

!!! warning

	Many very basic facilities - like the definition of `true` and
	`false` and `if` - are provided by the prelude so suppressing it
	leaves a very bare environment.

## Debugging

`eu` has a variety of command line switches for dumping out internal
representations or tracing execution. `eu --help` lists them all.
