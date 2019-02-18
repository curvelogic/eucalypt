**eucalypt** is a tool, and a little language, for generating and
transforming structured data formats like YAML and JSON.

If you use text-based templating to process these formats or you pipe
this these formats through several different tools or build steps,
**eucalypt** should be able to help you generate your output more cleanly
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

It can currently read YAML, JSON, TOML and eucalypt syntax and export
YAML, JSON or text.

If you're generating or processing YAML or JSON, you should give it a
try.

!!! warning

	**eucalypt** is in an early phase of development and highly subject to
	change. Not all features are fully implemented.

# A lightning tour

A few micro-examples should help give a flavour of **eucalypt**'s native
syntax.

If you want to try out the tool, see [Getting
Started](getting-started.md).

## Example 1

Here is a simple one:

```eu
target-zones: ["a", "b", "c"] map("eu-west-1{}")
```

You can put this in a file named `test.eu` and run it with `eu test.eu`. This generates the following YAML:

```yaml
target-zones:
  - eu-west-1a
  - eu-west-1b
  - eu-west-1c
```

Although we're looking at the native **eucalypt** syntax here, this
example could just as easily be embedded directly in a YAML file using
the `!eu` tag. Pop the following in a `test.yaml` file and process it
with `eu test.yaml`. You'll get the same result.

```yaml
target-zones: !eu ["a", "b", "c"] map("eu-west-1{}")
```

First, this example illustrates how we apply transformations like
`map` simply by concatenation.

In fact this is nothing more than a function call but this feature
allows us to write things in an order which makes the processing
clear.

In this example, `map` is just a function of two parameters. Its first
argument is provided in parentheses and its second argument is the
value of what came before.

!!! note

	Users of languages like Elixir or OCaml may recognise an implicit
	`|>` operator here. Clojure users may see an invisible threading
	macro. Note that writing elements next to each other like this gives
	you the _reverse_ of what you might expect in Haskell or OCaml or
	Lisp: we write `x f` *not* `f x`.

There is a lot of freedom in **eucalypt** to express ideas in different
ways and develop colorful and cryptic expressions. In a larger or
more ambitious language this could be viewed as rope to hang yourself
with. You will be careful won't you?

The string template, `"eu-west-1{}"` actually defines a function of
one argument that returns a string. The key ingredients here are:

- the interpolation syntax `"{...}"` which allows values to be inserted
into the string
- the (hidden) use of numeric *anaphora* in the interpolation syntax
(`{0}`, `{1}`, `{2}`, ...) which cause the string to define a
function, not just sequence of characters
- the use of the *unnumbered anaphor* (`{}`) which is numbered
  automatically for us, so in this case, `{}` is a convenient synonym
  for `{0}` - the first argument

!!! note

	Anaphora crop up in various contexts in **eucalypt** and are generally
	preferable to the full generality of lambdas. If the idea is too
	complex to be expressed with anaphora, it should generally be
	explicitly named.

So:

```eu
a: 42 "The answer is {0}"
```

renders as

```yaml
a: The answer is 42
```

**eucalypt** also has *expression anaphora* which use underscores -
`x: _0 + _1`.

!!! note

	Users of Groovy or Kotlin may recognise an equivalent of the `it`
	parameter. Seasoned Lisp hackers are familiar with anaphoric macros.
	Clojure users will recognise the `%`, `%1`, `%2` forms usable in
	`#(...)` contexts. Unlike Clojure's `%` repeated uses of unnumbered
	anaphora in **eucalypt** refer to different parameters. `"{}{}"` is a
	two-argument function which concatenates strings, `_ * _` is a two
	argument function that multiplies numbers.

Back to:

```eu
target-zones: ["a", "b", "c"] map("eu-west-1{}")
```

The whole line is a **declaration**. Declarations come in several
types - this one is a **property declaration**. A **block** is written
as a sequence of declarations enclosed in braces. For example:

```eu
{
  w: "foo" # a string
  x: 3 # a number
  y: 22.2 # a floaty number
  z: true # the truth
}
```

(The `#` character introduces a comment which is ignored by **eucalypt**.)

Unlike YAML, indentation is never significant.

Unlike JSON, commas are not needed to separate declarations. Instead,
the **eucalypt** parser determines the declarations mainly based on the
location of colons. You can write:

```eu
{ x: 1 increment negate y: 2 }
```

...and **eucalypt** knows it's two declarations.

Our `target-zones` property declaration is at the **top level** so is
not surrounded by braces explicitly. Nevertheless it is in a block:
the top level block, known as a **unit**, that is defined by the file
it is in. You can imagine the braces to be there if you like.

As a final point on this example, it is probably worthwhile
documenting declarations. **eucalypt** offers an easy way to do that using
**declaration metadata** which we squeeze in between a leading
backtick and the declaration itself:

```eu
` “AZs to deploy alien widgets in”
target-zones: [“a”, “b”, “c”] map(“eu-west-1{}”)
```

All sorts of things can be wedged in there, but if a string appears on
its own is interpreted as documentation.

## Example 2

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

We've introduced a new type of declaration here: `base(name): ...`.
This is a **function declaration**. (Remember we saw a **property
declaration** earlier.)

!!! note

	**eucalypt** also has **operator declarations** and a **splice syntax**
	but we'll ignore those for now. One day they will be covered in the
	user guide...

The function declaration declares a function called `character`, which
accepts a single parameter (`name`) and returns a block containing two
property declarations.

Functions, like everything else in **eucalypt**, are declared in and live
in blocks but they are left out when output is generated, so you won't
see them in the YAML or JSON that **eucalypt** produces.

The braces in the definition of `character` are there to delimit the
resulting block - *not* to define a function body. A function that
returned a number would not need them:

```eu
inc(x): x + 1 # this defines an increment function
```

...nor would any function that returns a block without using a block
literal:

```eu
identity(x): x # this returns its argument, which could be a block
```

The next important ingredient in this example is *block catenation*.

Blocks can be treated as functions of a single parameter. When they
are applied as functions, the effect is a *block merge*.

We've already seen that functions can be applied to arguments by
concatenation.

So writing one block after another produces a merged block. It
contains the contents of the second block merged "on top" of the
first.

There is much more to be said on block merging, but for now:


`{ a: 1 } { b: 2 }` evaluates to `{ a: 1 b: 2 }`.

and

`{ a: 1 } { a: 2 }` evaluates to `{ a: 2 }`.

In our example, the resulting YAML would be"

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

As you can see, `io.epoch-time` evaluates to a timestamp.

This metadata is generated once at launch time, *not* each time the
expression is evaluated. **eucalypt** the language is a pure functional
language, and there are no side-effects or non-deterministic functions
(although its command line driver can perform all sorts of
side-effects as input to the evaluation and as output from the
evaluation). For this reason, `prentice` and `slothrop` will have the
same timestamps.

Block merge can be a useful means of generating common content in
objects.

The common content can appear first as in this case, allowing it to be
overridden. Or it couple be applied second allowing it to override the
existing detail. Or a mixture of both. Many more sophisticated means
of combining block data are available too.

!!! note

	This merge is similar to the effect of *merge keys* in YAML, where a
	special `<<` mapping key causes a similar merge to occur. Not all
	YAML processors support this - but **eucalypt** does.

Be aware that **eucalypt** has nothing like virtual functions. The
functions in scope when an expression is created are the ones that are
applied. So if you redefine an `f` like this, in an overriding
block...

```eu
{ f(x): x+1 a: f(2) } { f(x): x-2 }
```

...the definition of `a` will not see it.

```yaml
a: 3
```

So block merge is only very loosely related to object oriented
inheritance. Also by default you only get a _shallow_ merge - deep
merges are provided in the standard prelude and covered in the user
guide.

Just like we have string anaphora for turning simple strings into
string-valued functions we have block anaphora, which appear as
numbers prefixed with a leading underscore. `b: { a: _ }` defines a
function, `b`, of one parameter returning a block. `b(2)` (or `2 b`)
evaluates to `{ a: 2 }`. Applying a block with anaphora to another
block will evaluate the function defined rather than falling back to
block merge which is really just the default behaviour of applying a
block.

**eucalypt** does not have a general lambda syntax (yet). If anaphora
cannot do what you want, consider using less nesting and defining
intermediate functions explicitly using function declaration syntax.

## Quick tour of the command line

Let's shift now to the pragmatics of using **eucalypt** from the command
line.

On macOS you can install the `eu` command line tools using Homebrew
with:

```shell
brew install eucalypt
```

Check the version you are running with:

```shell
eu -v
```

`eu` is intended to be easy to use for common tasks and does its best
to allow you to say what you want succinctly. It should be easy to use
in pipelines in combination with other tools like `jq`.

By default, it runs in **ergonomic** mode which will make a few
assumptions in order to allow you to be a little less explicit.

It also pulls in user-specific declarations from `~/.eucalypt`. For
repeatable builds and scripted usage, it is normally more appropriate
to turn ergonomic mode *off* using the `-B (--batch-mode)` switch.

The simplest usage is to specify a **eucalypt** file to evaluate and leave
the default render format (YAML) and output (standard out) alone.

```shell
> eu test.eu
```

In ergonomic mode, `eu` with no arguments will generally be taken to
specify that input is coming from standard in. So the above is
equivalent to:

```shell
> cat test.eu | eu
```

There is a switch to control output format explicitly (`-x yaml`, `x
json`, `x toml`, ...) but for the very common case of requiring JSON
output there is a shortcut:

```shell
> eu test.eu -j
```

You can, of course, redirect standard output to a file but if you
specify the output file explicitly (with `-o`), `eu` will infer the
output format from the extension:

```shell
> eu test.eu -o output.json # broadly equivalent to eu test.eu -j > output.json
```

Small snippets of **eucalypt** can be passed in directly using the `-e
(--evaluate)` switch.

```shell
> eu -e '{ a: 8 * 8 }'
```

The fact that **eucalypt** makes relatively infrequent use of single
quotes makes this usage straightforward for most shells.

By default, `eu` evaluates the entirety of the loaded source and uses
all of it to render the result, leaving out any function values and
other non-renderable content.

It is possible though to select more targeted content for output by
several means:

  1. A declaration in the source may be identified as the **main** target
	 using the `:main` declaration metadata
  2. **targets** may be named using the `:target` declaration metadata
	 and those targets can then be specified using the `-t (--target)`
	 option to `eu`
  3. The `-e (--evaluate)` option can be used in addition to other
	 source file(s) to identify an expression to be rendered (e.g. `eu
	 test.eu -e x.y.z`)

In passing, we should note that `eu`'s ability to read JSON and YAML
natively combined with the last options give a simple way to pick
values out of structured data:

```shell
> aws s3api list-buckets | eu -e 'Buckets map(lookup(:Name))'
```

There is much more to this story. For instance `eu` can:

- accept several inputs to make definitions in earlier inputs
  available to subsequent inputs `eu test1.eu test2.eu test3.eu`
- accept YAML and JSON files as pure data to be merged in: `eu
  data.yaml tools.eu`
- accept YAML or JSON annotated with **eucalypt** to execute: `eu
  data.yaml`
- override the default extensions: `eu yaml@info.txt`
- automatically use `Eufile` files in the current folder hierarchy

The fabled user guide will contain more on all these usages.
