# EF1 — Combined effect monad (`do`) — design

**Status:** draft for owner review · **Date:** 2026-07-24 · **Bead:** eu-1tkk.2 (EF1) · **Epic:** eu-1tkk (0.14)

> Working name `do` throughout. Not final — see §9. Code blocks show the
> *shape and code-sharing structure*; exact prelude spelling (argument order,
> `map` vs `io.map`, etc.) is finalised against prelude conventions during
> implementation, not asserted here.

## 1. Purpose & scope

Today `io`, `state`, `random`, `let` and `for` are each an independent
`monad{bind, return}` with no supported path to use their capabilities
*together* — a stateful, seeded, IO-driven pipeline has to hand-thread
`(state, seed)` through every step. EF1's near-term goal is **a convenient,
elegant way to use IO + state + random together**.

**In scope (0.14):**
- A single combined effect monad (working name `do`) that threads a
  `{ state, seed }` context alongside IO, usable via block metadata:
  `{ :do … }`.
- Its operation surface delegates to the existing `io` / `state` / `random`
  monads (no re-implementation of effects).
- A terse `do.st` lift so the existing state lens operators (`=!` / `%!`) and
  any state action are usable inside a `do` block.
- Deprecate the redundant `exec` *runners* on `state` and `random` (advisory
  `deprecated` metadata) so `exec` uniformly means "execute a command".
- Pure `:let` and list `:for` compose by **nesting**, not by mixing block
  metadata.

**Explicitly out of scope:**
- Mixing different block metadata within one block (`{ :io …; :state … }`) —
  rejected: it cannot be done without endangering syntactic integrity, and the
  combined monad makes it unnecessary.
- Parallel `do`-native operators for lens set/modify (e.g. `=!!` / `%!!`) —
  considered and deferred (§8); `do.st(:k =! v)` reuses the state operators for
  now.
- A custom bracket pair for `do` — deferred until the monad exists and earns
  one (§9).
- The 0.15+ unified/typed effect context and post-1.0 algebraic effect-rows
  (ROADMAP EF1.2 / EF1.3).
- Any type-checker effect tracking (`do` is an ordinary parametric type via the
  existing `__type_hint` mechanism, like `IO(a)` today).

## 2. Verified mechanics this builds on

Everything below is confirmed against the current tree; the design uses only
these, no new syntax:

1. **Monadic block form.** `{ :name decls }.expr` — `:name` is *block metadata*
   (shorthand for `{ { monad: name } decls }`), not a per-declaration marker.
   Each declaration `n: action` under it is one `bind` step. Return is `.(expr)`
   (explicit) or implicit (a block of the bound names). Example
   (`docs/guide/state-monad.md`):

   ```
   action: { :state
     n: state.query(_.count)
     _: state.put(:count, n + 1)
   }.(n)
   ```

2. **Defining a monad + enabling its block metadata.** A block carrying
   `monad:` metadata registers a namespace (`lib/state.eu`):

   ```
   `{ monad: s"state → {value: a, state: state}" }
   state: monad{bind: state-bind, return: state-ret} { get: … put: … run(a,i): a(i) … }
   ```

3. **Derived combinators for free.** `monad(m)` derives `map`, `then`,
   `and-then`, `join`, `sequence`, `map-m`, `filter-m` from `bind`/`return`
   (`lib/prelude.eu`). The combined monad reuses this — the combinators are
   derived, not written.

4. **Pure value-threading monads.** `state` is `state → {value, state}`;
   `random` is `stream → {value, rest}` (a state monad over the seed stream).
   Both are pure — only `io` is driver-interpreted (its `IoBind`/`IoAction`
   tree is walked by the driver *after* pure evaluation).

5. **Generalised-lookup nesting.** A monadic block on the RHS of a lookup
   inherits the LHS bindings (`docs/guide/monads.md`):
   `{ x: 1, y: 2 }.{ :let z: x + y }`.

6. **Partial application & non-lambda functions.** `f(a, b): …` called as
   `f(a)` yields a function awaiting `b`; anonymous behaviour uses anaphora
   (`(_ + 1)`, `(.value)`) and sections (`(+ 1)`) — there are **no arrow
   lambdas** (`->` is `const`).

## 3. The `do` monad

**A `do` action is a function `ctx → IO<{ value, ctx }>`**, where the context
is `ctx = { state: <block>, seed: <random stream> }`. IO carries the context
through its *value* channel, so `io.bind` already handles the delicate
`world`-token sequencing and `do` only threads `ctx` on top. This is a
state+seed layer over IO (a small, fixed RWS-over-IO shape — **not** a general
transformer stack).

`return` / `bind` (shape; written with named continuations, no arrow lambdas):

```
do-return(v, ctx): io.return({ value: v, ctx: ctx })

do-bind(m, f, ctx): io.bind(m(ctx), resume(f))
resume(f, r):       f(r.value)(r.ctx)          # r is the IO-produced {value, ctx}

do: monad{ bind: do-bind, return: do-return } { … members from §4 … }
```

Partial application makes `do.return(v)` = `do-return(v)` (the `ctx →` action)
and `do.bind(m, f)` = `do-bind(m, f)`.

**Runners** (each yields an IO action the driver then runs). Only `run` and
`eval` are named — `exec` is reserved for the io command capability (§4/§9), so
the rarely-needed final-context result is just `run` projected:

```
do.run(a, ctx0):  a(ctx0)                      # IO<{value, ctx}>   — both
do.eval(a, ctx0): io.map(a(ctx0), (.value))    # IO<value>          — usual entry
# final context only (niche): run then project .ctx
```

## 4. Operation surface — delegation, not duplication

The **only genuinely new code** is `do-bind`/`do-return` (§3) plus three lift
functions. Every actual effect stays in `io`/`state`/`random`; `do`'s members
are thin wrappers.

Lifts (shape — same partial-application style as §3; `do.lift-io(a)` is
`do-lift-io(a)` awaiting `ctx`, i.e. the `ctx →` action — **no** anonymous
`(ctx): …` form is assumed):

```
do-lift-io(action, ctx):     io.map(action, pair-ctx(ctx))               # ctx unchanged
do-lift-state(saction, ctx):  io.return(thread-state(ctx, saction(ctx.state)))
do-lift-random(raction, ctx): io.return(thread-seed(ctx, raction(ctx.seed)))

pair-ctx(ctx, v):       { value: v, ctx: ctx }
thread-state(ctx, s):   { value: s.value, ctx: { state: s.state, seed: ctx.seed } }
thread-seed(ctx, r):    { value: r.value, ctx: { state: ctx.state, seed: r.rest  } }
```

`do.lift-io` / `do.lift-state` / `do.lift-random` are these helpers exposed as
members; the surface below applies them partially (`do.lift-state(state.get)`
is a `do` action).

Surface members (names proposed; delegate to existing ops):

```
# io capabilities
do.shell(c):     do.lift-io(io.shell(c))
do.shell-with(o,c): do.lift-io(io.shell-with(o, c))
do.exec(args):   do.lift-io(io.exec(args))        # exec kept for the io command

# state capabilities
do.get:          do.lift-state(state.get)
do.put(k, v):    do.lift-state(state.put(k, v))
do.modify(k, f): do.lift-state(state.modify(k, f))
do.query(f):     do.lift-state(state.query(f))
do.st:           do.lift-state                     # terse alias — lift any state action

# random capabilities (underlying random action names to be confirmed vs the random namespace)
do.random:       do.lift-random(random.<next>)     # the primitive random draw
do.choose(xs):   do.lift-random(random.<choose>(xs))

# pure
do.pure(v):      do.return(v)
```

`map` / `then` / `and-then` / `sequence` / `map-m` / `filter-m` are **derived**
via `monad(do)` — no new code.

**State lens operators reused via `do.st`.** The state monad's `=!` (set a lens
focus) and `%!` (modify a lens focus) operators produce state actions, so they
work inside a `do` block through the terse lift — no new operators, no
duplication:

```
{ :do  _: do.st(:count =! 0)   _: do.st(:total %! (+ 1)) }
```

Parallel `do`-native operators (e.g. `=!!` / `%!!`) were considered and deferred
(§8); `do.st` is the near-term surface.

**Sharing strategy chosen: (a) thin delegating wrappers** (above). Considered
(b) structural auto-derivation — since monads are blocks, map each namespace's
members through its lift and merge — but a uniform "lift after the op" across
members of *different arities* (`get`=0, `shell`=1, `exec`=list) is an unproven
generic-composition problem; not banked for 0.14. Left as a possible future
tidy-up (§8).

## 5. `:let` / `:for` compose by nesting, not metadata-mixing

- **Pure intermediate value** inside a `do` pipeline: `n: do.pure(expr)` — or,
  post-hoc on a plain result, generalised-lookup nesting `result.{ :let … }`
  (§2.5).
- **Effectful iteration / traversal** (the effectful analogue of `:for`):
  `do.map-m(f, xs)` / `do.sequence(actions)` — derived, reused.
- **Pure comprehension**: an ordinary `{ :for … }` block as a sub-expression
  (it evaluates to a plain list, usable as any value).

No `do`-block ever carries foreign block metadata; `:let`/`:for` stay their own
blocks, embedded as expressions.

## 6. Worked example (canonical io + state + random)

```
# proposed do.* names; block form / .(…) / runner shape are verified
gather: { :do
  files: do.shell("ls")
  pick:  do.choose(files.stdout lines)
  _:     do.modify(:seen, (+ 1))
}.(pick)

main: do.eval(gather, { state: { seen: 0 }, seed: io.RANDOM_SEED })
```

One block, one monad, all three capabilities as native `do.*` actions, state
and seed threaded automatically, no hand-rolled `(state, seed)` plumbing and no
mixed block metadata.

## 7. Testing

- Harness tests under `tests/harness/` following the gating conventions
  (`docs/guide/testing.md`): a `{ :do … }` pipeline exercising all three
  effects, asserting both the returned value and the threaded final state/seed
  via `do.run(a, ctx0)` projected with `.ctx`.
- Determinism: seed via `--seed` so `random` steps are reproducible.
- IO steps use the existing `--allow-io` gate and `requires_io` test marker.
- Parity: assert `do`-mediated effects match the same effects run through the
  underlying `io`/`state`/`random` monads directly (proves delegation added no
  semantic drift).
- Each test computes its `RESULT` from its checks; every new test is
  fault-injection verified.

## 8. Open questions / to prototype (not asserted)

1. **Feasibility of `do-bind` over `io.bind`** — the load-bearing risk. IO's
   `world`-token threading is documented as delicate; confirm `ctx` riding the
   IO value channel composes correctly under the driver's stashing/GC-rooting,
   on **both** engines. Prototype first.
2. **Exact prelude spelling** — argument order and whether `io.map`/`io.bind`
   take `(action, f)` or `(f, action)`; finalise against prelude idiom.
3. **Structural auto-derivation (§4b)** — is a clean arity-generic lift
   feasible, to shrink the wrapper list to near-zero?
4. **`:let`/`:for` embedding ergonomics** — validate the nesting forms in §5
   read as well in practice as on paper.
5. **Parallel `do`-native lens operators** (`=!!` / `%!!`, lexer-confirmed a
   single token distinct from `=!` and `!!`) — considered and **deferred**;
   `do.st(:k =! v)` reuses state's operators for now. Revisit if `do.st` proves
   too heavy in real use, or as part of the 0.15 unified context.

## 9. Naming (resolved for the draft; `do` remains a working name)

- **`do`** (working name): reads well as block metadata `{ :do … }`; reads
  slightly oddly as a namespace, `do.choose(…)`. Owner can live with it, and
  nothing downstream depends on it — we spec against `do` and can rename at the
  end.
- Alternatives floated and set aside: `io*`, `IO`, `eu` — `io*`/`IO` imply
  *IO-only* and under-sell that this equally carries state and random.
- **`exec` resolved:** `exec` belongs to the **io command** capability
  (`do.exec(args)`, matching `io.exec`). The runners are **`do.run`** (value +
  context) and **`do.eval`** (value only); the niche final-context result is
  `do.run` projected with `.ctx`, so no runner needs the `exec` name.
- **Deprecate the `exec` *runners* on the other monads** so `exec` means
  "execute a command" everywhere. `state.exec` / `random.exec` get advisory
  `deprecated` metadata (warns on stderr; error under `eu check --strict`; still
  works):

  ```
  ` { deprecated: "use state.run(a, i).state" replaced-by: "run" }
  exec(action, initial): action(initial).state
  ```

  Migration: `state.run(a, i).state` / `random.run(a, s).rest`.
- Bracket pair: deferred until `{ :do … }` is in use.

## 10. Summary

A single `do` monad threads a `{ state, seed }` context over IO. New code is
only `bind`/`return` + three lifts; all effects and all derived combinators are
reused from `io`/`state`/`random`/`monad()`. Capabilities appear as native
`do.*` actions; `:let`/`:for` compose by nesting. No mixed block metadata, no
new syntax, no transformer stack — the ergonomic win with eucalypt's syntactic
integrity intact.
