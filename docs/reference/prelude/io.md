# IO

## Prelude Versioning

| Function | Description |
|----------|-------------|
| `eu.prelude` | Metadata about this version of the standard prelude |
| `eu.build` | Metadata about this version of the eucalypt executable |
| `eu.requires` | Assert that the eucalypt version satisfies the given semver constraint (e.g. '>=0.2.0') |
| `eu.os` | Operating system: linux, macos, windows, etc |
| `eu.arch` | CPU architecture: x86_64, aarch64, etc |

## IO Functions

> IO related declarations. The io namespace is a monad: io.bind, io.return, io.sequence, io.map-m, io.filter-m, io.then, io.join are derived automatically.

| Function | Description |
|----------|-------------|
| `io.env` | Read access to environment variables at time of launch |
| `io.epoch-time` | Seconds since the Unix epoch at launch time |
| `io.args` | Command-line arguments passed after -- separator |
| `io.RANDOM_SEED` | Seed for random number generation (from --seed or system time) |
| `io.random` | Opaque random stream seeded from system entropy or --seed flag. Use random.* actions or random.as-list to consume |
| `io.shell(c)` | Run a shell command via sh -c. Returns a block with stdout, stderr, and exit-code fields |
| `io.shell-with(opts, c)` | Run a shell command via sh -c with extra options merged in. Options may include stdin and timeout |
| `io.exec` | Run a command directly without a shell. Returns a block with stdout, stderr, and exit-code fields |
| `io.exec-with(opts)` | Run a command directly without a shell, with extra options merged in. Options may include stdin and timeout |
| `io.check(result)` | Check a command result: if exit-code is non-zero, fail with the stderr message; otherwise return the result |
| `io.checked` | Pipeline-friendly check: bind the preceding IO action through check |
| `io.fail(msg)` | Fail the IO action with the given error message |

## Other

| Function | Description |
|----------|-------------|
| `monad(m)` | Derive standard monad combinators from a block m with bind and return fields. Returns a block with bind, return, map, then, and-then, join, sequence, map-m, and filter-m |
| `deep-fold(emit, next-state, s, b)` | Depth-first fold over nested blocks and lists. `emit(state, block)` returns a list of results at each block node. `next-state(state, child-key)` updates state for recursion into a child. `s` is the initial state |
| `deep-transform(rule, data)` | Recursively transform a nested structure. At each node, call `rule(node)`. If it returns non-null, use that as the replacement (stop recursing into that node). If it returns null, recurse into children (block values and list elements) |
| `any?` | Predicate that matches any value. For use in match? patterns |
| `match?(pat, target)` | Structural predicate. Returns true if `target` conforms to `pattern`. Pattern values are interpreted by type: blocks and lists recurse as sub-patterns, functions are applied as predicates, literals are exact equality checks. Open matching: extra keys in target are ignored |
| `render(value)` | Serialise value to a YAML string |
| `render-as(fmt, value)` | Serialise value to a string in the named format. Pipeline-friendly: data render-as(:json). Supported formats: :yaml, :json, :toml, :text, :edn, :html, :eu |
| `parse-as(fmt, str)` | Parse a string of structured data in the named format and return eucalypt data. The inverse of render-as. Supported formats: :json, :yaml, :toml, :csv, :xml, :edn, :jsonl. Content is parsed as inert data; embedded eucalypt expressions (e.g. YAML !eu tags) are never evaluated |
| `assert(p?, s, v)` | If `v p?` is true then return `v` otherwise error with message `s` |
| `__dbg-render(v)` | Print value `v` to stderr and return `v` unchanged. opts keys: label (string) |
| `dbg(opts, v)` |  |
| `__dbg-val(v)` | Debug-trace value or function. On a value, prints it to stderr and returns it. On a function, returns a wrapped version that traces each output |
| `__dbg-after(f, x)` |  |
| `(▶ x)` |  |
| `alter?(k?, v!, k, v)` | If `k` satisfies `k?` then `v!` else `v` |
| `update?(k?, f, k, v)` | If `k` satisfies `k?` then `v!` else `v` |
| `is-array?(x)` | True if `x` is an n-dimensional array |
| `let` | Identity monad for sequential let-bindings. Use :let blocks for sequential evaluation without the self-reference gotcha |
| `for` | List monad for list comprehensions. Each binding draws from a list; subsequent bindings can depend on earlier ones. Use [x] filter(pred?) for guards |
| `parse-args(defaults, args)` | Parse command-line argument list against a defaults block. Each key in `defaults` defines an option with its default value. Field metadata configures parsing: `short` (symbol for short flag), `doc` (description), `flag` (true for boolean toggle). Returns the `defaults` block updated with parsed values, plus an `args` key containing positional arguments as a list. Unknown options cause a runtime error. Use `--help` for auto-generated help text |
