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

**Current limitation**: The `timeout` and `stdin` fields in `opts` for
`shell-with` and `exec-with` are accepted by the parser but not yet applied
by the io-run driver (they default to 30 s and no stdin). This will be fixed
in a future driver update. The `cmd` argument is always applied correctly.

### Combinators

| Function | Description |
|----------|-------------|
| `io.check(result)` | If `exit-code` is non-zero, fail with the stderr message; otherwise return the result |
| `io.map(f, action)` | Apply a pure function to the result of an IO action (fmap) |

## Other

| Function | Description |
|----------|-------------|
| `alter?(k?, v!, k, v)` | If `k` satisfies `k?` then `v!` else `v` |
| `update?(k?, f, k, v)` | If `k` satisfies `k?` then `v!` else `v` |
