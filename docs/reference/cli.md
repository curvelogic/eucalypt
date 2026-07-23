# CLI Reference

Eucalypt is available as a command line tool, `eu`, which reads inputs
and writes outputs.

Everything it does in between is purely functional and there is no
mutable state.

It is intended to be simple to use in unix pipelines.

```sh
eu --version # shows the current eu version
eu --help # lists command line options
```

### Global Options

- `--source-prelude` - Force the source-prelude pipeline even when a pre-compiled
  prelude blob is available. Use this when you need to trace through prelude source
  during debugging, or when the cached blob is suspected to be stale. Note this
  pipeline is correctness-equivalent to the blob path but carries a documented
  performance handicap (~10% on arithmetic-dense code) — see
  [`docs/development/prelude-blob.md`](../development/prelude-blob.md).

## Command Structure

The `eu` command uses a subcommand structure for clarity and extensibility:

```sh
eu [GLOBAL_OPTIONS] [SUBCOMMAND] [SUBCOMMAND_OPTIONS] [FILES...]
```

### Subcommands

- `run` (default) - Evaluate eucalypt code
- `test` - Run tests
- `check` - Type-check eucalypt source files
- `doc` - Extract documentation from eucalypt source files
- `dump` - Dump intermediate representations
- `error <CODE>` - Print the catalogue entry for a stable error code (e.g.
  `eu error EU-EVAL-TYPE`); see the [error-code catalogue](error-codes.md)
- `version` - Show version information
- `explain` - Explain what would be executed
- `list-targets` - List targets defined in the source
- `fmt` - Format eucalypt source files
- `lsp` - Start the Language Server Protocol server

When no subcommand is specified, `run` is used by default, so these are equivalent:

```sh
eu file.eu
eu run file.eu
```

## Inputs

### Files / *stdin*

`eu` can read several inputs, specified by command line arguments.

Inputs specify text data from:

 - files
 - stdin
 - internal resources (ignored for now)
 - git repository files (via `{ git: … }` import blocks — see [Import Formats](import-formats.md#git-imports))

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
 - jsonl (JSON Lines)
 - toml
 - edn
 - xml
 - csv
 - text

Of these yaml, json, toml, edn and xml return blocks; jsonl, csv and
text return lists. Inputs that return lists frequently need to be named (see
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
[imports](import-formats.md) in `.eu` files.

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

So for instance:

a.eu
```eu
x: 4
y: 8
```

b.eu

```eu,notest
z: x + y
```

```sh
eu a.eu b.eu
```

will output

```yaml
z: 12
```

The common use cases are:
- a final input containing logic to inspect or process data
  provided by previous inputs
- a final input which uses functions defined in earlier inputs to
  process data provided in previous inputs

If you want to render contents of earlier inputs, you need a named
input to provide a name for that content which you can then use.

For instance:

```sh
eu r=a.eu b.eu -e r
```

will render:

```yaml
x: 4
y: 8
```

#### `--collect-as` and `--name-inputs`

Occasionally it is useful to aggregate data from an arbitrary number
of sources files, typically specified by shell wildcards. To refer to
this data we need to introduce a name for the collection of data.

This is what the command line switch `--collect-as` / `-c` is for.

```sh
eu --collect-as inputs *.eu
```

...will render:

```yaml
inputs:
  - x: 4
    y: 8
  - z: 12
```

It is common to use `-e` to select an item to render:

```sh
eu -c inputs *.eu -e 'inputs head'
```

...renders:

```yaml
x: 4
y: 8
```

If you are likely to need to refer to inputs by name, you can add
`--name-inputs` / `-N` to pass inputs as a block instead of a list:

```sh
eu --collect-as inputs --name-inputs *.eu
```

...renders:

```yaml
inputs:
  a.eu:
    x: 4
    y: 8
  b.eu:
    z: 12
```

This makes it easier to invoke specific functions from named inputs
although you will need single-quote name syntax to use the generated
names which contain `.`s.

## Outputs

`eu` produces a single output stream per invocation.

### Output format

Output is rendered as YAML by default. Other formats can be specified
using the `-x` command line option:

```sh
eu -x json # for JSON
eu -x text # for plain text
eu -x html # for HTML
```

JSON is such a common case that there is a shortcut: `-j`. See
[Export Formats](export-formats.md) for the complete list (yaml, json,
toml, edn, text, html, eu).

### Output targets

By default, **eucalypt** renders all the content of the final input to
output.

There are various ways to override this. First, `:target` metadata can
be specified in the final input to identify different parts for
potential export.

To list the **targets** found in the specified inputs, use the
`list-targets` subcommand.

```sh
eu list-targets file.eu
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

## Passing Arguments to Programs

You can pass command-line arguments to your eucalypt program using the
`--` separator. Arguments after `--` are available via `io.args`:

```console
$ eu -e 'io.args' -- foo bar baz
---
- foo
- bar
- baz
```

This is useful for writing eucalypt scripts that accept parameters:

```eu
# greet.eu
name: io.args head-or("World")
greeting: "Hello, {name}!"
```

```console
$ eu greet.eu -e greeting -- Alice
---
Hello, Alice!
```

Arguments are passed as strings. Use `num` to convert numeric arguments:

```eu
# sum.eu
total: io.args map(num) foldl((+), 0)
```

```console
$ eu sum.eu -e total -- 1 2 3 4 5
---
15
```

When no arguments are passed, `io.args` is an empty list:

```console
$ eu -e 'io.args nil?'
---
true
```

## Random Seed

By default, random numbers are seeded from system entropy and produce
different results on each run. Use `--seed` for reproducible output:

```sh
eu --seed 42 template.eu
```

This sets `io.RANDOM_SEED` and seeds the `io.random` stream. See
[Random Numbers](prelude/random.md) for the full random API.

## IO Monad Operations

By default, eucalypt is a pure functional language with no side effects. To
enable IO monad operations — specifically shell command execution — you must
pass the `--allow-io` / `-I` flag:

```sh
eu -I script.eu
eu --allow-io script.eu
```

Without this flag, any program that attempts to execute an IO action will fail
with an error:

```
IO operations require the --allow-io (-I) flag
```

### Why this flag exists

The flag is a deliberate security measure. Eucalypt files are often used as
configuration or data templates, and it would be unsafe for arbitrary `.eu`
files loaded from the filesystem or network to execute shell commands without
explicit consent. The `--allow-io` flag is your explicit acknowledgement that
the program you are running may perform shell execution.

### Usage with IO targets

IO programmes typically use the `:io` monadic block syntax and are run with a
named target:

```sh
eu -I --target main script.eu
eu -I -t main script.eu
```

See the IO monad design documentation for full details of the IO API.

## Statistics and Diagnostics

### Performance Statistics

Use `-S` / `--statistics` to print execution metrics to stderr when the run
completes. This includes heap usage, GC cycle counts, and evaluation time:

```sh
eu -S file.eu
```

To capture statistics as machine-readable JSON:

```sh
eu --statistics-file stats.json file.eu
```

The statistics file is written regardless of whether evaluation succeeds.

### Heap Limit

By default, `eu` allows the managed heap to grow up to 32 GiB. To cap it at
a lower value (e.g., for CI or resource-constrained environments):

```sh
eu --heap-limit-mib 1024 file.eu    # limit to 1 GiB
eu --heap-limit-mib 0    file.eu    # unbounded
```

### Error Output Format

By default, errors are formatted for human reading. For programmatic use:

```sh
eu --error-format json file.eu      # structured JSON errors
eu --error-format human file.eu     # default human-readable (explicit)
```

### Error Traces

The `stack trace:` an execution error carries is **curated** by default:
library plumbing (a combinator recursing on its own tail) is dropped,
recursion is collapsed, and the trace is anchored on your own call sites,
with the named library combinator you invoked kept as context — for
example `in 'nth'`. The same curation applies to the `trace` array of
`--error-format json`.

To see the raw, uncurated continuation stack instead:

```sh
eu run --debug-trace file.eu
```

This is a debugging aid for working on the diagnostics machinery itself,
or for cases where the curated trace has hidden the frame you need.

### Type Checking

The type checker runs unconditionally on every `eu` invocation. Type
warnings are reported to stderr but do not abort evaluation unless
`--strict` is also passed:

```sh
eu file.eu                          # warn on type issues, then evaluate
eu file.eu --strict                 # abort before evaluation on first type warning
eu file.eu --suppress-type-warnings # evaluate, silencing warning output (checker still runs)
```

`--type-check` is a deprecated no-op flag (kept for backwards
compatibility) — type checking already runs whether or not it is
passed.

For checking without evaluating, use the `check` subcommand (see below).

### Debugging: Dead Code Elimination

Disable the dead code elimination pass to inspect the full compiled expression:

```sh
eu --no-dce file.eu
```

This is primarily useful when investigating the output of `eu dump` passes.

## Suppressing prelude

A standard *prelude* containing many functions and operators is
automatically prepended to the input list.

This can be suppressed using `-Q` if it is not required or if you
would like to provide an alternative.

> **Warning:** Many very basic facilities -- like the definition of
> `true` and `false` and `if` -- are provided by the prelude so
> suppressing it leaves a very bare environment.

## Debugging

`eu` has a variety of command line switches for dumping out internal
representations or tracing execution. The `dump` subcommand provides
access to intermediate representations:

```sh
eu dump ast file.eu          # Parse and dump syntax tree
eu dump desugared file.eu    # Dump core expression
eu dump stg file.eu          # Dump compiled STG syntax
eu list-targets file.eu      # List available targets
```

Use `eu --help` and `eu <subcommand> --help` for complete option lists.

## Formatting Source Files

The `fmt` subcommand formats eucalypt source files for consistent style:

```sh
eu fmt file.eu              # Print formatted output to stdout
eu fmt --write file.eu      # Format in place
eu fmt --check file.eu      # Check formatting (exit 1 if not formatted)
eu fmt *.eu --write         # Format multiple files in place
```

### Options

- `-w, --width <WIDTH>` - Line width for formatting (default: 80)
- `--write` - Modify files in place
- `--check` - Check if files are formatted (exit 1 if not)
- `--reformat` - Full reformatting mode (instead of conservative)
- `--indent <INDENT>` - Indent size in spaces (default: 2)

The formatter has two modes:

- **Conservative mode** (default) - Preserves original formatting choices
  where possible, only reformatting where necessary
- **Reformat mode** (`--reformat`) - Full reformatting that applies
  consistent style throughout

## Type Checking

The `check` subcommand runs the type checker without evaluating:

```sh
eu check file.eu             # report type annotation warnings
eu check --strict file.eu    # treat warnings as errors (exit 1 if any)
```

Type annotations in eucalypt are gradual — the checker only reports issues
where annotations are present and their constraints are violated. Use `check`
in CI to enforce annotation coverage across a codebase.

See the [type checking guide](../guide/type-checking.md) for full syntax and
examples.

## Documentation Extraction

The `doc` subcommand extracts documentation from backtick docstrings in
eucalypt source:

```sh
eu doc file.eu                    # Markdown output to stdout
eu doc --format json file.eu      # JSON Schema output
eu doc --prelude                  # Document the embedded prelude
eu doc --check file.eu            # Report documentation coverage (exit 1 if < 100%)
eu doc --output-dir out/ file.eu  # Write categorised Markdown files to a directory
```

### Options

- `--format <md|json>` - Output format: `md` (Markdown, default) or `json` (JSON Schema)
- `--prelude` - Generate documentation for the built-in prelude
- `--check` - Report documentation coverage rather than generating docs
- `--output-dir <DIR>` - Write categorised multi-file Markdown output to a directory
  (implies `--prelude`)

## Language Server Protocol

The `lsp` subcommand starts an LSP server for use with editors that
support the Language Server Protocol (e.g., VS Code, Neovim):

```sh
eu lsp
```

The LSP server provides:

- Syntax error diagnostics
- Formatting support (via `textDocument/formatting`)

Configure your editor to use `eu lsp` as the language server command
for `.eu` files. A VS Code extension is available in the `editors/vscode/`
directory of the repository.

## Version Assertions

The `eu.requires` function allows eucalypt source files to assert a
minimum version of the eucalypt executable:

```eu
{ import: [] }  # unit-level metadata not required for eu.requires

# Assert that eu version satisfies semver constraint
_ : eu.requires(">=0.3.0")
```

If the running version of `eu` does not satisfy the constraint, an
error is raised immediately. This is useful for library code that
depends on features introduced in a particular version.

The `eu` namespace also provides build metadata:

```eu
version: eu.build.version    # e.g., "0.3.0"
```

## Backward Compatibility

All existing command patterns continue to work unchanged:

```sh
eu file.eu                   # Still works (uses run subcommand)
eu -e "expression"           # Still works (uses run subcommand)
eu -j file.eu                # Still works (JSON output)
eu -S -Q file.eu             # Still works (statistics, no prelude)
```
