# IO and Shell Commands

As of version 0.5.0 Eucalypt can execute IO by invoking shell
commands, via the *IO monad*. 

Eucalypt is not a scripting language, it is a small, lazy, dynamically
typed, pure functional language for transformation and templating of
semi-structured data formats and that remains its sweet spot. Do not
get too ambitious.

However, prior to the IO monad, arranging the sources of data in and
out of the program was limited to what could be named or streamed in
from the shell, and piped to pre-planned destinations. Allowing more
direct control over IO within the process (while staying within the
functional paradigm) allows much more flexibility.

IO operations are sequenced strictly by the runtime and the monad
idiom. 

**All IO operations require the `--allow-io` / `-I` flag.**

---

## The IO monad

This is not a monad tutorial. Suffice it to say than an IO action
(which may have side effects) is represented by an object in Eucalypt.
The monad and the runtime together, ensure that the action is run at the
appropriate time and its output is safely incorporated into the
computation without imperiling the referential transparency of non-IO
code. 

In initial versions of this functionality, the *only* IO capability
available is running shell programs or binaries from eucalypt, and
passing data in and out of them. Data passed into and out of the
shelled program is not streamed incrementally, but buffered whole.
This is not suitable for large data pipelines.

The monad provides several ways to create IO actions, and
string them together. When you run a eucalypt program, if the value of
the named target (or `:main` target) is an IO action, that is run.

Just embedding an IO action in a data structure is not sufficient to
have it execute. It must either be the value of the top-level target
of a program, or invoked in some way by that action.

The key monad functions `bind` and `return` are defined in the `io`
namespace, along with several traditional monad combinators. This
supports the use of `{ :io ... }` monadic blocks to combine several
monad actions into one. [Monads and the monad() Utility](monads.md) explains Eucalypt's monad
machinery in a little more detail.

A simple IO action that doesn't perform any IO at all but represents a
constant value, can be created with `io.return`.  e.g.

``` eu,notest
greeting: io.return("hello")
```

To create more interesting IO actions, read on.

## Running a shell command

The simplest way to run a command is `io.shell`:

```eu,notest
result: "echo hello" io.shell
```

This creates an IO action which runs the specified command via `sh -c`
and returns a block:

```yaml
stdout: "hello\n"
stderr: ""
exit-code: 0
```

To extract a specific field, either use generalised lookup syntax on
an `:io` monad block...

```eu,notest
{ :io r: io.shell("echo hello") }.(r.stdout)
```

... or use `io.map` to apply a section within the IO monad chain.

``` eu,notest
"echo hello" io.shell io.map(.stdout)
```

---

## The { :io ... } monadic block

A block tagged `:io` desugars into nested `io.bind` calls. Each field
is a bind step; the name becomes available in all subsequent steps.
The `.()` expression after the closing brace is the return value.

```eu,notest
{ :io
  r: io.shell("echo hello")
  _: io.check(r)
}.(r.stdout)
```

**Important:** unlike normal blocks, monadic blocks bind names
sequentially — each step can only refer to names from earlier steps,
not later ones. See [Monads and the monad() Utility](monads.md) for
full details on monadic block syntax, forms, and the sequential
binding constraint.

---

## Checking for errors

`io.check` inspects a command result. If the exit code is non-zero,
it fails the IO monad with the stderr message. Otherwise it returns
the result unchanged:

```eu,notest
{ :io
  r: io.shell("grep pattern file.txt")
  _: io.check(r)
}.(r.stdout)
```

If `grep` finds no matches (exit code 1), this produces an
`io.fail` error with whatever was on stderr.

---

## Exec: running a binary directly

`io.exec` runs a binary without going via the shell. The argument is a
list where the first element is the command and the rest are
arguments:

```eu,notest
{ :io
  r: io.exec(["git", "rev-parse", "HEAD"])
}.(r.stdout)
```

If the binary does not exist, `io.exec` returns a result block with
exit-code 127 and the OS error in stderr, rather than failing outright.

---

## Options: stdin, timeout

Both `io.shell-with` and `io.exec-with` accept an options block as
the first argument. This is merged into the spec block, overriding
defaults:

```eu,notest
"cat" io.shell-with{stdin: "hello world", timeout: 60} io.map(.stdout)
```

Available options:

| Option | Default | Description |
|--------|---------|-------------|
| `stdin` | (none) | String to pipe to the command's standard input |
| `timeout` | 30 | Maximum seconds before the command is killed |

The pipeline style reads naturally: the command string flows into
`shell-with` which receives the options.

---

## Combining IO actions

### Sequencing with bind

`io.bind` chains two actions. The continuation receives the result of
the first action:

```eu,notest
io.bind(io.shell("echo hello"),
  _(r): io.shell("echo got: {r.stdout}"))
```

The `{ :io ... }` block is almost always preferable to explicit
`io.bind` calls.

### Mapping over a result

`io.map` applies a pure function to the result of an action without
needing a new IO step. The function can, of course, be a composition
of several functions:

```eu,notest
` :main
result: "curl https://example.com/test.json" io.shell io.map((.stdout) ; parse-as(:json))
```

### Failing explicitly

`io.fail` aborts the IO monad with an error message:

```eu,notest
{ :io
  r: io.shell("some-command")
  _: (r.exit-code = 0) then(io.return(r), io.fail("command failed: {r.stderr}"))
}.(r.stdout)
```

This is what `io.check` does internally — it is a convenience wrapper
around this pattern:

``` eu,notest
` :main
greeting: "echo hello world" io.shell io.check io.map(.stdout)
```

---

## Practical examples

### Git commit hash

```eu,notest
` :main
hash: "git rev-parse --short HEAD" io.shell io.check io.map(.stdout)
```

### Run a command and parse the output as JSON

```eu,notest
` :main
data: "curl -s https://api.example.com/data" 
  io.shell 
  io.check 
  io.map((.stdout) ; parse-as(:json))
```

### Pipe data through a command

```eu,notest
` :main
text: { :io
  r: "jq '.name'" io.shell-with({stdin: render-as(:json, data)})
}.(r.stdout)
```

### Multiple commands in sequence

```eu,notest
` :main
main: { :io
  a: io.shell("date +%s")
  b: io.shell("hostname")
}.(
  { timestamp: a.stdout 
    host: b.stdout }
)
```

or, equivalently:

``` eu,notest
io.sequence[io.shell("date +%s") io.map(.stdout),
            io.shell("hostname") io.map(.stdout)] with-keys[:timestamp, :host]

```

---

## Testing IO code

Test targets that use IO should include `requires-io: true` in their
target metadata. The test runner (`eu test`) skips these tests
gracefully when `--allow-io` is not set:

```eu,notest
` { target: :test requires-io: true }
test:
  { :io r: io.exec(["echo", "hello"]) }.(
    if(r.stdout str.matches?("hello.*"),
      { RESULT: :PASS },
      { RESULT: :FAIL }))
```

---

## Reference

For the full API table, see the
[IO prelude reference](../reference/prelude/io.md).

For the monadic programming model, `monad()` utility, and how to
build your own monads, see
[Monads and the monad() Utility](monads.md).
