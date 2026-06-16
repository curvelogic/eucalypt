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
| `monad.bind` | Sequence two monadic actions: run action and pass its result to continuation |
| `monad.return` | Wrap a pure value in the monad |
| `monad.map(f, action)` | Apply pure function f to the result of a monadic action (fmap) |
| `monad.then(b, a)` | Sequence two monadic actions, discarding the result of the first. Pipeline: `a io.then(b)` |
| `monad.and-then(f, action)` | Pass the result of action to f (bind with flipped args, for pipeline use) |
| `monad.join(mm)` | Flatten a nested monadic value |
| `monad.sequence(ms)` | Sequence a list of monadic actions, collecting results into a list |
| `monad.map-m(f, xs)` | Apply f to each element of xs (producing actions), then sequence |
| `monad.filter-m(p, xs)` | Monadic filter: apply predicate p (returning a monadic bool) to each element |
| `random-ret.value` |  |
| `random-ret.rest` |  |
| `random-bind.r` |  |
| `random-bind.run` |  |
| `random-bind.value` |  |
| `random-bind.rest` |  |
| `lookup-across.f(b, acc)` |  |
| `deep-fold.walk-child(s)` |  |
| `deep-fold.walk(s, v)` |  |
| `deep-find.emit(s, v)` |  |
| `deep-find.next(s, ck)` |  |
| `deep-find-paths.emit(path, v)` |  |
| `deep-find-paths.next(path, ck)` |  |
| `deep-query-fold.segs` |  |
| `deep-query-fold.norm` |  |
| `deep-query-fold.child-pair(path, el)` |  |
| `deep-query-fold.desc-child(path, el)` |  |
| `deep-query-fold.desc-pairs` |  |
| `deep-query-fold.expand-star` |  |
| `deep-query-fold.match-sym(s)` |  |
| `deep-query-fold.apply-segment(pairs, seg)` |  |
| `deep-query-fold.result` |  |
| `deep-transform.attempt` |  |
| `deep-transform.recurse` |  |
| `any?` | Predicate that matches any value. For use in match? patterns |
| `match?.mv?(p, actual)` |  |
| `match?.mb?(p, t)` |  |
| `match?.me?(t)` |  |
| `match?.ml?(p, t)` |  |
| `bit.and(l, r)` | Bitwise AND of integers `l` and `r` |
| `bit.or(l, r)` | Bitwise OR of integers `l` and `r` |
| `bit.xor(l, r)` | Bitwise XOR of integers `l` and `r` |
| `bit.not(n)` | Bitwise NOT of integer `n` |
| `bit.shl(n, k)` | Left shift `n` by `k` bits |
| `bit.shr(n, k)` | Arithmetic right shift `n` by `k` bits |
| `bit.popcount(n)` | Count set bits in integer `n` |
| `bit.ctz(n)` | Count trailing zeros in integer `n` (position of lowest set bit). Returns 64 if n is 0 |
| `bit.clz(n)` | Count leading zeros in integer `n` |
| `max-of-by.step(acc, x)` |  |
| `max-of-by.init` |  |
| `min-of-by.step(acc, x)` |  |
| `min-of-by.init` |  |
| `render(value)` | Serialise value to a YAML string |
| `render-as(fmt, value)` | Serialise value to a string in the named format. Pipeline-friendly: data render-as(:json). Supported formats: :yaml, :json, :toml, :text, :edn, :html, :eu |
| `parse-as(fmt, str)` | Parse a string of structured data in the named format and return eucalypt data. The inverse of render-as. Supported formats: :json, :yaml, :toml, :csv, :xml, :edn, :jsonl. Content is parsed as inert data; embedded eucalypt expressions (e.g. YAML !eu tags) are never evaluated |
| `assert(p?, s, v)` | If `v p?` is true then return `v` otherwise error with message `s` |
| `__dbg-render(v)` | Print value `v` to stderr and return `v` unchanged. opts keys: label (string) |
| `dbg.result` |  |
| `__dbg-val.result` |  |
| `__dbg-after(f, x)` |  |
| `(▶ x)` |  |
| `rotate.s` |  |
| `split-at.aux(n, xs, prefix)` |  |
| `transpose.aux(rs)` |  |
| `take-while.aux(xs, prefix)` |  |
| `split-after.aux(xs, prefix)` |  |
| `update-nth.rejoin` |  |
| `update-first.go` |  |
| `window.chunk` |  |
| `window-all.chunk` |  |
| `discriminate.acc(a, e)` |  |
| `group-by.acc(a, e)` |  |
| `nub-by.step(acc, x)` |  |
| `split-on.step(acc, x)` |  |
| `sort-by.lt(a, b)` |  |
| `alter?(k?, v!, k, v)` | If `k` satisfies `k?` then `v!` else `v` |
| `update?(k?, f, k, v)` | If `k` satisfies `k?` then `v!` else `v` |
| `update-value-or.acc(st, el)` |  |
| `update-value-or.final-state` |  |
| `update-value-or.final-block` |  |
| `vec.of` | Convert list `xs` of primitive values to a vec for O(1) indexed access |
| `vec.len` | Return the number of elements in vec `v` |
| `vec.nth` | Return the element at index `n` (0-based) in vec `v` |
| `vec.slice` | Return a new vec with elements in the range `[from, to)` of `v` |
| `vec.sample(n, v, stream)` | Random monad action: pick `n` random elements from vec `v` without replacement |
| `vec.shuffle(v, stream)` | Random monad action: return vec `v` in random order |
| `vec.to-list` | Convert vec `v` back to a cons-list |
| `arr.zeros` | Create an array of zeros with the given shape list |
| `arr.fill` | Create an array filled with `val` with the given shape list |
| `arr.from-flat` | Create an array from a flat list of numbers with the given shape |
| `arr.get` | Get element at coordinate list `coords` in array `a`. Pipeline: `a arr.get(coords)` |
| `arr.set` | Return new array with element at `coords` set to `val`. Pipeline: `a arr.set(coords, val)` |
| `arr.shape` | Return shape of array `a` as a list of integers |
| `arr.rank` | Return number of dimensions of array `a` |
| `arr.length` | Return total number of elements in array `a` |
| `arr.to-list` | Return flat list of elements in row-major order |
| `arr.array?` | True if `x` is an array |
| `arr.transpose` | Reverse all axes of array `a` |
| `arr.reshape` | Reshape array `a` to new shape (total elements must match). Pipeline: `a arr.reshape(shape)` |
| `arr.slice(axis, idx, a)` | Take a slice along `axis` at `idx`, reducing rank by 1. Pipeline: `a arr.slice(axis, idx)` |
| `arr.add` | Element-wise addition; `b` may be a scalar |
| `arr.sub` | Element-wise subtraction; `b` may be a scalar |
| `arr.mul` | Element-wise multiplication; `b` may be a scalar |
| `arr.div` | Element-wise division; `b` may be a scalar |
| `arr.indices` | Return list of coordinate lists for all elements in row-major order |
| `arr.map(f, a)` | Apply `f` to each element, return new array of same shape |
| `arr.map-indexed(f, a)` | Apply `f(coords, val)` to each element, return new array of same shape |
| `arr.fold(f, init, a)` | Left-fold `f` over all elements of `a` in row-major order |
| `arr.neighbours(coords, offsets, a)` | Return list of values at valid neighbouring coordinates. Pipeline: `a arr.neighbours(coords, offsets)`. `offsets` is a list of offset lists (one per neighbour direction). Out-of-bounds neighbours are omitted |
| `arr.unit-arrays(shape)` | List of arrays with a single 1 at each coordinate position |
| `is-array?(x)` | True if `x` is an n-dimensional array |
| `graph.union-find(n, edges)` | Connected components via union-find.
  Given `n` nodes and `edges` as `[[i,j],...]`, return list of n component labels |
| `graph.topo-sort(n, edges)` | Topological ordering via Kahn's algorithm.
  Given `n` nodes and directed `edges` as `[[from,to],...]`, return node ordering.
  Partial result if cycle exists (check count(result) = n) |
| `graph.kruskal-edges(n, edges)` | Kruskal-order merge history.
  Process `edges` in order via union-find. Return flat list of
  `[edge-index, remaining-components, ...]` for each edge that merges
  two distinct components |
| `let` | Identity monad for sequential let-bindings. Use :let blocks for sequential evaluation without the self-reference gotcha |
| `for` | List monad for list comprehensions. Each binding draws from a list; subsequent bindings can depend on earlier ones. Use [x] filter(pred?) for guards |
| `parse-args.build-short-lookup(defs)` | Build lookup table mapping short-flag symbols to option key symbols |
| `parse-args.is-flag(defs, key)` | Return true if option key has `flag: true` metadata |
| `parse-args.has-key(k)` | Predicate: does element have key `k`? |
| `parse-args.coerce-value(defs, key, str-val)` | Coerce string value to match type of default for `key` |
| `parse-args.set-opt(key, val, st)` | Update opts with a key-value pair |
| `parse-args.resolve-pending(defs, val, st)` | Resolve pending option by coercing and setting its value.
  If `st` has a `:buf` key, uses that; otherwise uses `val`.
  If neither `:buf` nor `val` is available, returns `st` unchanged |
| `parse-args.process-long(defs, state, arg)` | Process a long option (starting with `--`) against defaults |
| `parse-args.process-short-chars(defs, sl, state, chars)` | Process short options string (list of single-char strings).
  Handles combined flags like `-vo` where `v` is a flag and `o` takes a value |
| `parse-args.process-one(defs, sl, state, arg)` | Process a single argument in state |
| `parse-args.generate-help(defs)` | Generate help text for `--help` |
| `parse-args.parse(defs, a)` | Main parse logic |
