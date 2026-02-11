# Import Formats

Eucalypt supports importing content from other units in a variety of
ways.

Imported names can be scoped to specific declarations, they may be
made accessible under a specific namespace, and they may be imported
from disk or direct from git repositories.

## Import scopes

Imports are specified in declaration metadata and make the names in
the imported unit available within the declaration that is annotated.

```eu,notest
{ import: "config.eu" }
data: {
  # names from config are available here
  x: config-value
}
```

As described in [Syntax Reference](syntax.md), declaration metadata can
be applied at a unit level simply by including a metadata block as the
very first thing in a eucalypt file:

```eu,notest
{ import: "config.eu" }

# names from config are available here

x: config-value
```

## Import syntax

Imports are specified using the key `import` in a declaration metadata
block. The value may be a single import specification:

```eu,notest
{ import: "dep-a.eu"}
```

or a list of import specifications:

```eu,notest
{ import: ["dep-a.eu", "dep-b.eu"]}
```

The import specification itself can be either a *simple import* or a
*git import*.

### Simple imports

Simple imports are specified in exactly the same way as *inputs* are
specified at the command line (see [CLI Reference](cli.md)).

So you can override the format of the imported file when the file
extension is misleading:

```eu,notest
{ import: "yaml@dep.txt" }
```

...and provide a name under which the imported names will be
available:

```eu,notest
{ import: "cfg=config.eu" }

# names in config.eu are available by lookup in cfg:

x: cfg.x
```

In cases where the import format delivers a list rather than a block
("text", "csv", "jsonl", ...) a name is mandatory:

```eu,notest
{ import: "txns=transactions.csv" }
```

Simple imports support exactly the same inputs as the command line,
with the proviso that the stdin input ("-") will not be consumable if
it has already been specified in the command line or another unit.

### Git imports

Git imports allow you to import eucalypt direct from a git repository
at a specified commit, combining the convenience of not having to
explicitly manage a git working copy and a library path with the
repeatability of a git SHA. A git import is specified as a block with
the keys "git", "commit" and "import", all of which are mandatory:

```eu,notest
{ import: { git: "https://github.com/gmorpheme/eu.aws"
            commit: "0140232cf882a922bdd67b520ed56f0cddbd0637"
            import: "aws/cloudformation.eu" } }
```

The `git` URL may be any format that the git command line expects.

`commit` is required and should be a SHA. It is intended to ensure the
import is repeatable and cacheable.

`import` identifies the file within the repository to import.

Just as with simple imports, several git imports may be listed:

```eu
{ import: [{ git: ... }, { git: ... }]}
```

...and simple imports and git imports may be freely mixed.

## YAML import features

When importing YAML files, eucalypt supports several YAML features that
help reduce repetition and express data more naturally.

### Anchors and aliases

YAML anchors (`&name`) and aliases (`*name`) allow you to define a value
once and reference it multiple times. When eucalypt imports a YAML file
with anchors and aliases, the aliased values are resolved to copies of
the anchored expression.

```yaml
# config.yaml
defaults: &defaults
  timeout: 30
  retries: 3

development:
  <<: *defaults
  debug: true

production:
  <<: *defaults
  debug: false
```

Anchors can be applied to any YAML value: scalars, lists, or mappings.

```yaml
# Anchor on a scalar
name: &author "Alice"
books:
  - title: "First Book"
    author: *author
  - title: "Second Book"
    author: *author

# Anchor on a list
colours: &primary [red, green, blue]
palette:
  primary: *primary
  secondary: [yellow, cyan, magenta]

# Anchor on a mapping (block)
base: &base
  x: 1
  y: 2
ref: *base  # ref now has { x: 1, y: 2 }
```

Nested anchors are supported -- an anchored structure can itself contain
anchored values:

```yaml
outer: &outer
  inner: &inner 42
ref_outer: *outer   # { inner: 42 }
ref_inner: *inner   # 42
```

If you reference an undefined alias, eucalypt reports an error:

```yaml
# This will fail: *undefined is not defined
value: *undefined
```

### Merge keys

The YAML merge key (`<<`) allows you to merge entries from one or more
mappings into another. This is useful for creating configuration
variations that share a common base.

**Single merge:**

```yaml
base: &base
  host: localhost
  port: 8080

server:
  <<: *base
  name: main
# server = { host: localhost, port: 8080, name: main }
```

**Multiple merge:**

When merging multiple mappings, later ones override earlier ones:

```yaml
defaults: &defaults
  timeout: 30
  retries: 3

overrides: &overrides
  timeout: 60

config:
  <<: [*defaults, *overrides]
  name: myapp
# config = { timeout: 60, retries: 3, name: myapp }
```

**Explicit keys override merged values:**

Keys defined explicitly in the mapping (before or after the merge)
always take precedence over merged values:

```yaml
base: &base
  x: 1
  y: 2

derived:
  <<: *base
  y: 99
# derived = { x: 1, y: 99 }
```

**Inline merge:**

You can also merge an inline mapping directly:

```yaml
config:
  <<: { timeout: 30, retries: 3 }
  name: myapp
```

The merge key value must be a mapping (or list of mappings). Attempting
to merge a non-mapping value (e.g., `<<: 42`) results in an error.

### Timestamps

Eucalypt automatically converts YAML timestamps to ZDT (zoned date-time)
expressions. Plain scalar values matching timestamp patterns are parsed
and converted; quoted strings are left as strings.

**Supported formats:**

| Format | Example | Notes |
|--------|---------|-------|
| Date only | `2023-01-15` | Midnight UTC |
| ISO 8601 UTC | `2023-01-15T10:30:00Z` | |
| ISO 8601 offset | `2023-01-15T10:30:00+05:00` | |
| Space separator | `2023-01-15 10:30:00` | Treated as UTC |
| Fractional seconds | `2023-01-15T10:30:00.123456Z` | |

**Examples:**

```yaml
# These are converted to ZDT expressions:
created: 2023-01-15
updated: 2023-01-15T10:30:00Z
scheduled: 2023-06-01 09:00:00

# This remains a string (quoted):
date_string: "2023-01-15T10:30:00Z"
```

**Invalid timestamps fall back to strings:**

If a value looks like a timestamp but has invalid date components
(e.g., month 13 or day 45), it remains a string:

```yaml
invalid: 2023-13-45  # Remains string "2023-13-45"
```

**To keep timestamps as strings:**

If you need to preserve a timestamp-like value as a string rather than
converting it to a ZDT, quote it:

```yaml
# As ZDT:
actual_date: 2023-01-15

# As string:
date_label: "2023-01-15"
```

## Streaming imports

For large files, eucalypt supports streaming import formats that read
data lazily without loading the entire file into memory. Streaming
formats produce a lazy list of records.

| Format | Description |
|--------|-------------|
| `jsonl-stream` | JSON Lines (one JSON object per line) |
| `csv-stream` | CSV with headers (each row becomes a block) |
| `text-stream` | Plain text (each line becomes a string) |

Streaming formats are specified using the `format@path` syntax:

```sh
# Stream a JSONL file
eu -e 'data take(10)' data=jsonl-stream@events.jsonl

# Stream a large CSV
eu -e 'data filter(_.age > 30) count' data=csv-stream@people.csv

# Stream lines of text
eu -e 'data filter(str.matches?("ERROR"))' log=text-stream@app.log
```

Streaming imports can also be used via the import syntax in eucalypt
source files:

```eu,notest
{ import: "events=jsonl-stream@events.jsonl" }

recent: events take(100)
```

> **Note:** Streaming imports require a name binding (e.g., `data=`) because
> they produce a list, not a block.

> **Note:** `text-stream` supports reading from stdin using `-` as the path:
> `eu -e 'data count' data=text-stream@-`
