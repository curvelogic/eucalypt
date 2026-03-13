# IO

## Prelude Versioning

| Function | Description |
|----------|-------------|
| `eu.prelude` | Metadata about this version of the standard prelude |
| `eu.build` | Metadata about this version of the eucalypt executable |
| `eu.requires` | Assert that the eucalypt version satisfies the given semver constraint (e.g. '>=0.2.0') |

## Runtime IO Values

| Name | Description |
|------|-------------|
| `io.env` | Read access to environment variables at time of launch |
| `io.epoch-time` | Unix epoch time at time of launch |
| `io.args` | Command-line arguments passed after `--` separator |
| `io.RANDOM_SEED` | Seed for random number generation (from `--seed` or system time) |
| `io.random` | Infinite lazy stream of random floats in [0,1) |

## IO Monad

The `io` namespace is a monad. Blocks tagged with `:io` are desugared
into monadic bind chains automatically:

```eu,notest
result: { :io
  r: io.shell("ls -la")
  _: io.check(r)
}.r.stdout
```

...desugars to `io.bind(io.shell("ls -la"), λr. io.bind(io.check(r), λ_. io.return(r.stdout)))`.

IO operations require the `--allow-io` / `-I` flag at the command line.

### Monad primitives

| Function | Description |
|----------|-------------|
| `io.return(a)` | Wrap a pure value in the IO monad |
| `io.bind(action, continuation)` | Sequence two IO actions |

### Shell execution

| Function | Description |
|----------|-------------|
| `io.shell(cmd)` | Run `cmd` via `sh -c`. Returns `{stdout: Str, stderr: Str, exit-code: Num}` |
| `io.shell-with(opts, cmd)` | Run `cmd` via `sh -c` with extra options merged in (e.g. `{stdin: s, timeout: 60}`). Pipeline: `"ls" shell-with({timeout: 60})` |
| `io.exec([cmd : args])` | Run `cmd` directly (no shell). Argument is a single list: first element is the command, rest are args |
| `io.exec-with(opts, [cmd : args])` | Run `cmd` directly with extra options merged in. Pipeline: `["git", "rev-parse", "HEAD"] exec-with({timeout: 60})` |

Default timeout is 30 seconds. Override with `{timeout: N}` in `opts`.
Optional `{stdin: s}` pipes string `s` to the command's standard input.

### Combinators

| Function | Description |
|----------|-------------|
| `io.check(result)` | If `exit-code` is non-zero, fail with the stderr message; otherwise return the result |
| `io.map(f, action)` | Apply a pure function to the result of an IO action (fmap) |
| `io.then(a, b)` | Sequence two actions, discarding the result of the first |
| `io.join(mm)` | Flatten a nested IO action |
| `io.sequence(ms)` | Run a list of IO actions in order, collecting results into a list |
| `io.map-m(f, xs)` | Apply `f` to each element of `xs` (producing IO actions), then sequence |
| `io.filter-m(p, xs)` | Monadic filter: keep elements where `p` returns a truthy IO action |

The combinators `map`, `then`, `join`, `sequence`, `map-m`, and
`filter-m` are derived automatically via `monad()`. See the
[Monads guide](../../guide/monads.md) for details on the derivation
pattern and the [IO guide](../../guide/io.md) for practical usage.

| Function | Description |
|----------|-------------|
| `monad(m)` | Derive standard monad combinators from `m.bind` and `m.return` |
| `monad(m).map(f, action)` | Apply pure function `f` to the result of a monadic action (fmap) |
| `monad(m).then(a, b)` | Sequence two monadic actions, discarding the result of the first |
| `monad(m).join(mm)` | Flatten a nested monadic value |
| `monad(m).sequence(ms)` | Sequence a list of monadic actions, collecting results into a list |
| `monad(m).map-m(f, xs)` | Apply `f` to each element of `xs` (producing actions), then sequence |
| `monad(m).filter-m(p, xs)` | Monadic filter: apply predicate `p` (returning a monadic bool) to each element |

## Other

| Function | Description |
|----------|-------------|
| `alter?(k?, v!, k, v)` | If `k` satisfies `k?` then `v!` else `v` |
| `update?(k?, f, k, v)` | If `k` satisfies `k?` then `v!` else `v` |
