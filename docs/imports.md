# Imports

Eucalypt supports importing content from other units in a variety of
ways.

Imported names can be scoped to specific declarations, they may be
made accessible under a specific namespace, and they may be imported
from disk or direct from git repositories.

## Scope of imported names

Imports are specified in declaration metadata and make the names in
the imported unit available within the declaration that is annotated.

```eu

` { import: "config.eu" }
data: {
  # names from config are available here
  x: config-value
}
# names from config.eu are not available here
#
# names are *not* re-exported
#   i.e. `data.config-value` is invalid
#
# names defined in the block are visible as normal so
result: data.x # is valid
```

As described in [syntax](syntax.md), declaration metadata can be
applied at a unit level simply by including a metadata block as the
very first thing in a eucalypt file:

```eu
{ import: "config.eu" }

# names from config are available throughout this file

x: config-value

```

!!! warning

	There is currently ambiguity around precedence of conflicting
	bindings which needs to be resolved. Names defined in the file
	take precedence over imported names even in the tighter scope of the
	import which is counter-intuitive and probably going to change.

## Import syntax

Imports are specified using the key `import` in a declaration metadata
block. The value may be a single import specification:

```eu
{ import: "dep-a.eu"}
```

or a list of import specifications:

```eu
{ import: ["dep-a.eu", "dep-b.eu"]}
```

The import specification itself can be either a *simple import* or a
*git import*.

### Simple imports

Simple imports are specified in exactly the same way as *inputs* are
specified at the command line (see [command line](command-line.md)).

So you can override the format of the imported file when the file
extension is misleading:

```eu
{ import: "yaml@dep.txt" }
```

...and provide a name under which the imported names will be
available:

```eu
{ import: "cfg=config.eu" }

# names in config.eu are available by lookup in cfg:

x: cfg.x
```

In cases, where the import format delivers a list rather than a block
("text", "csv", ...) a name is mandatory:

```eu
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


```eu
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
