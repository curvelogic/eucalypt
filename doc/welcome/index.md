# eucalypt

**eucalypt** is a tool, and a little language, for generating and
transforming structured data formats like YAML, JSON and TOML.

If you use text-based templating to process these formats or you pipe
these formats through several different tools or build steps,
eucalypt might be able to help you generate your output more cleanly
and with fewer cognitive somersaults.

**eucalypt** is a purely functional language that can be used quickly and
easily from the command line.

It has the following features:

  - a concise native syntax that allows you to define data, functions,
    and operators
  - a simple embedding into YAML files to support in-place
    manipulation of the data (a la templating)
  - facilities for manipulating blocks (think JSON objects, YAML
    mappings)
  - facilities for manipulating text including string interpolation
    and regular expressions
  - an ergonomic command line interface and access to environment
    variables
  - metadata annotations and numerous extension points
  - a [*prelude*](../reference/prelude/index.md) of built-in functions, acting like a standard library

It can currently read YAML, JSON, JSON Lines, TOML, EDN, XML, CSV and
plain text and eucalypt's own ("eu") syntax and it can export YAML, JSON,
TOML, EDN or plain text.

> **Warning:** eucalypt is still in an early phase of development and
> subject to change.

## A lightning tour

Eucalypt has a native [syntax](../reference/syntax.md) for writing blocks,
lists and expressions. The YAML embedding consists of a few YAML tags
used to embed eucalypt expression in YAML so a basic understanding of
the native syntax is helpful.

A few micro-examples should help give a flavour of eucalypt's
native syntax. If you want to follow along, see [Quick
Start](quick-start.md) for notes on installation.

### Example 1

Here is a simple one:

```eu
target-zones: ["a", "b", "c"] map("eu-west-1{}")
```

You can put this in a file named `test.eu` and run it with just:

```shell
eu test.eu
```

This outputs the following YAML:

```yaml
target-zones:
  - eu-west-1a
  - eu-west-1b
  - eu-west-1c
```

As an aside, although we're looking at the native eucalypt syntax
here, this example could just as easily be embedded directly in a YAML
file using the `!eu` tag. Pop the following in a `test.yaml` file and
process it with: `eu test.yaml`. You'll get the same result.

```yaml
target-zones: !eu ["a", "b", "c"] map("eu-west-1{}")
```

First, this example illustrates how we apply transformations like
`map` simply by concatenation. This "pipelining" or "catenation" is
the natural way to apply transformations to values in eucalypt.

In fact this is simply a function call with the arguments rearranged a
bit. In this example, `map` is a function of two parameters. Its first
argument is provided in parentheses and its second argument is the
value of what came before.

> **Note:** Users of languages like Elixir or OCaml may recognise an
> implicit `|>` operator here. Clojure users may see an invisible
> threading macro. Note that writing elements next to each other like
> this gives you the _reverse_ of what you might expect in Haskell or
> OCaml or Lisp: we write `x f` *not* `f x`.

There is a lot of freedom in eucalypt to express ideas in different
ways and develop colourful and cryptic expressions. In a larger or
more ambitious language this could be viewed as rope to hang yourself
with. Please be careful.

The string template, `"eu-west-1{}"`, actually defines a function of
one argument that returns a string. The key ingredients here are:

- the interpolation syntax `"{...}"` which allows values to be inserted
into the string
- the (hidden) use of numeric *anaphora* in the interpolation syntax
(`{0}`, `{1}`, `{2}`, ...) which cause the string to define a
function, not just sequence of characters
- the use of the *unnumbered anaphor* (`{}`) which is numbered
  automatically for us, so in this case, `{}` is a convenient synonym
  for `{0}` - the first argument

> **Note:** Anaphora crop up in various contexts in eucalypt and are
> generally preferable to the full generality of lambdas. If the idea
> is too complex to be expressed with anaphora, it should generally be
> explicitly named.

So:

```eu
a: 42 "The answer is {0}"
```

renders as

```yaml
a: The answer is 42
```

eucalypt also has *expression anaphora* and *block anaphora*

> **Note:** Users of Groovy or Kotlin may recognise an equivalent of
> the `it` parameter. Seasoned Lisp hackers are familiar with
> anaphoric macros. Clojure users will recognise the `%`, `%1`, `%2`
> forms from `#(...)` contexts. Unlike `%` repeated uses of unnumbered
> anaphora in eucalypt refer to different parameters. `"{}{}"` is a
> two-argument function which concatenates strings.

Back to:

```eu
target-zones: ["a", "b", "c"] map("eu-west-1{}")
```

The whole line is a **declaration**. Declarations come in several
types - this one is a **property declaration**. A **block** is written
as a sequence of declarations enclosed in braces. For example:

```eu,notest
{
  w: "foo" # a string
  x: 3     # a whole number
  y: 22.2  # a floaty number
  z: true  # the truth
}
```

(The `#` character introduces a comment which is ignored.)

Unlike YAML, indentation is never significant.

Unlike JSON, commas are not needed to separate declarations. Instead,
the eucalypt parser determines the declarations mainly based on the
location of colons. You can write:

```eu,notest
{ x: 1 increment negate y: 2 }
```

...and eucalypt knows it's two declarations.

If that's a bit too crazy for you, then feel free to insert the
commas. Eucalypt will accept them. Any of these are okay:

```eu
ok1: { a: 1 b: 2 c: 3 }
ok2: { a: 1, b: 2, c: 3 }
ok3: { a: 1, b: 2, c: 3, }
```

> **Note:** Unlike Clojure which makes commas optional by treating
> them as whitespace, Eucalypt demands that if you are going to put
> commas in, they have to be in the right place, at the end of
> declarations. So you can use them if you believe it makes things
> clearer but you are prevented from using them in ways which would
> misguide.

Our `target-zones` property declaration is at the **top level** so
need not be surrounded by braces. Nevertheless it is in a block: the
top level block, known as a **unit**, that is defined by the file that
contains it. You can imagine the braces to be there if you like.

As a final point on this example, it is probably worthwhile
documenting declarations. eucalypt offers an easy way to do that using
**declaration metadata** which we squeeze in between a leading
backtick and the declaration itself:

```eu
` "AZs to deploy alien widgets in"
target-zones: ["a", "b", "c"] map("eu-west-1{}")
```

In fact, all sorts of things can be wedged in there, but if a string
appears on its own, it is interpreted as documentation.

### Example 2

Let's look at another small example:

```eu
character(name): {
  resource-name: name
  created: io.epoch-time
}

prentice: character("Pirate Prentice") {
  laser-colour: "red"
}

slothrop: character("Tyrone Slothrop") {
  eye-count: 7
}
```

We've introduced a new type of declaration here of the form `f(x):`.
This is a **function declaration**.

Remember we saw a **property declaration** earlier. Eucalypt also has
**operator declarations** but we'll ignore those for now.

The function declaration declares a function called `character`, which
accepts a single parameter (`name`) and returns a block containing two
properties.

Functions, like everything else in eucalypt, are declared in and live
in blocks but they are left out when output is rendered, so you won't
see them in the YAML or JSON that eucalypt produces.

The braces in the definition of `character` are there to delimit the
resulting block - *not* to define a function body. A function that
returned a number would not need them:

```eu
inc(x): x + 1 # this defines an increment function
```

The next important ingredient in this example is *block catenation*.

Blocks can be treated as functions of a single parameter. When they
are applied as functions, the effect is a *block merge*.

We've already seen that functions can be applied to arguments by
concatenation.

So writing one block after another produces a merged block. It
contains the contents of the second block merged "on top" of the
first.

There is more to be said on block merge, but for now:


`{ a: 1 } { b: 2 }` evaluates to `{ a: 1 b: 2 }`.

and

`{ a: 1 } { a: 2 }` evaluates to `{ a: 2 }`.

In our example, the resulting YAML is just:

```yaml
prentice:
  resource-name: Pirate Prentice
  created: 1526991765
  laser-colour: red

slothrop:
  resource-name: Tyrone Slothrop
  created: 1526991765
  eye-count: 7
```

As you can see, `io.epoch-time` evaluates to a unix timestamp.

This metadata is generated once at launch time, *not* each time the
expression is evaluated. eucalypt the language is a pure functional
language, and there are no side-effects or non-deterministic functions
(although its command line driver can perform all sorts of
side-effects as input to the evaluation and as output from the
evaluation and there are one or two dirty tricks in the debugging
functions). For this reason, `prentice` and `slothrop` will have the
same timestamps.

Block merge can be a useful means of generating common content in
objects. The common content can appear first as in this case, allowing
it to be overridden. Or it could be applied second allowing it to
override the existing detail. Or a mixture of both. Many more
sophisticated means of combining block data are available too.

> **Note:** This merge is similar to the effect of *merge keys* in
> YAML, where a special `<<` mapping key causes a similar merge to
> occur. Not all YAML processors support this and nor does eucalypt at
> present, but it probably will some day.

Be aware that eucalypt has nothing like virtual functions. The
functions in scope when an expression is created are the ones that are
applied. So if you redefine an `f` like this, in an overriding
block...

```eu,notest
{ f(x): x+1 a: f(2) } { f(x): x-2 }
```

...the definition of `a` will not see it.

```yaml
a: 3
```

So block merge is only very loosely related to object oriented
inheritance. Also by default you only get a _shallow_ merge - deep
merges are provided in the standard prelude. It is possible that a
deep merge will become the default for block catenation in future.

Many more complicated ways of processing blocks are possible using
functions, block anaphora and standard prelude functions.

## Quick tour of the command line

On macOS you can install the `eu` command line tools using Homebrew
with:

```shell
brew install curvelogic/homebrew-tap/eucalypt
```

Check the version you are running with:

```shell
eu version
```

`eu` is intended to be easy to use for common tasks and does its best
to allow you to say what you want succinctly. The intention is to be
easy to use in pipelines in combination with other tools like `jq`.

By default, it runs in ergonomic mode which will make a few
assumptions in order to allow you to be a little less explicit. It
also pulls in user-specific declarations from `~/.eucalypt`. For
repeatable builds and scripted usage, it is better to turn ergonomic
mode *off* using the `-B (--batch)` switch.

The simplest usage is to specify a eucalypt file to evaluate and leave
the default render format (YAML) and output (standard out) alone.

```shell
> eu test.eu
```

`eu` with no arguments will generally be taken to specify that input
is coming from standard in. So the above is equivalent to:

```shell
> cat test.eu | eu
```

There is an `-x` switch to control output format explicitly (setting
"yaml", "json", "text", "csv" or "eu") but for the very common case of
requiring JSON output there is a shortcut:

```shell
> eu test.eu -j
```

You can, of course, redirect standard output to a file but if you
specify the output file explicitly (with `-o`), `eu` will infer the
output format from the extension:

```shell
> eu test.eu -o output.json # equivalent to eu test.eu -j > output.json
```

Small snippets of eucalypt can be passed in directly using the `-e`
switch.

```shell
> eu -e '{ a: 8 * 8 }'
```

The fact that eucalypt makes relatively infrequent use of single
quotes makes this straightforward for most shells.

By default, `eu` evaluates the entirety of the loaded source and uses
all of it to render the result, leaving out any function values and
other non-renderable content.

It is possible to select just parts of the eucalypt for rendering:

  1. A declaration in the source may be identified as the **main**
     target using the `:main` declaration metadata and we become the
     part rendered by default.
  2. **targets** may be defined and named using the `:target`
     declaration metadata and those targets can then be specified
     using the `-t` option to `eu`
  3. The `-e` option can be used in addition to other
     source file(s) to identify an expression to be rendered (e.g. `eu
     test.eu -e x.y.z`)

So `eu`'s ability to read JSON and YAML natively combined with the
last options give a simple way to pick values out of structured data
which can be very handy for "querying" services that return YAML or
JSON data.

```shell
> aws s3api list-buckets | eu -e 'Buckets map(lookup(:Name))'
```

There is much more to this story. For instance `eu` can:

- accept several inputs to make definitions in earlier inputs
  available to subsequent inputs `eu test1.eu test2.eu test3.eu`
- accept YAML and JSON files as pure data to be merged in: `eu
  data.yaml tools.eu`
- accept YAML or JSON annotated with eucalypt to execute: `eu
  data.yaml`
- override the default extensions: `eu yaml@info.txt`
- automatically use `Eufile` files in the current folder hierarchy

See [CLI Reference](../reference/cli.md) for more complete documentation.
