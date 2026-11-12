# Lists built by foldr and filter lose sharing (eu-oaz72.36)

Date: 2026-11-12. All figures are VM tick counts (deterministic for a given
binary and prelude mode), **measured-single**, source prelude,
`EU_PP_THRESHOLD=999999999`.

## Symptom

The bead's repro: `xs: range(0, 20) filter(p?)` with an expensive `p?`.
`xs sum` cost 15.2M ticks and `(xs sum) + (xs sum)` 29.6M: the second
traversal recomputed the list.

## Two independent causes

**1. Engine: constructor fields copied out of a deeper frame.** Minimal
repro, with no `foldr` and no `if`: `f(x, a): cons(slow(x), a)`, `k: f(1, [])`.
The STG optimiser strips alias lets, so the body compiles to
`λ{2} let [0] thunk slow(x) in Cons(✳0, ✳2)`; field `✳2` lives in the
argument frame. When a `case` takes the value apart, `env_from_data_args`
shares the constructor's backing array only if every field is in its
immediate frame. Otherwise `env_from_data_args_copy` copied each field's
value into a fresh frame, so an unevaluated update thunk was duplicated.
Forcing it updated the copy, the original slot stayed unevaluated, and the
next traversal re-entered the `Cons` and copied the unevaluated thunk again.

Fix: like `make_arg_array` (eu-wpswc), the copy path binds an unevaluated
update thunk (local or global) as a lazy alias closure over the field's
`OP_ATOM` node in the constructor's env. Forcing it enters, black-holes and
updates the original slot. Evaluated values are still copied. The
pre-decoded `Cons` instruction keeps its field-atom ordinals in the `offsets`
pool (`instr.b`) for this.

**2. Demand analysis: parameters reached through an alias.** `filter`'s
operator `{x: • xs: •}.(if(x p?, cons(x, xs), xs))` desugars to
`λ(a, b). let x = a in let xs = b in …`. `xs` is Multi (it is stored in a
cons), but a let binding's RHS demands kept the RHS's own cardinality
("an updated thunk evaluates its RHS once"), so `b` was credited one use and
the signature was `[(Lazy,AtMostOnce), (Lazy,AtMostOnce)]`. The STG optimiser
then erases the alias, so no thunk of its own stands between `b` and the
cons. The caller (the inlined fold) let-bound the recursive result as a
non-updatable value, and every traversal re-entered it, re-running the
predicate. Fix: a binding whose RHS is a bare bound variable passes its own
cardinality on to that variable.

The un-updatable `if` branch closures noted in the bead are a symptom of
cause 2, not a separate defect: a branch entered once per call needs no
update.

## Relationship to eu-oaz72.34

Independent. eu-oaz72.34 was stack growth in `if`/`then` loops (curried
`then` in the inliner). Neither fix here touches `src/core/inline/`.

## Measurements

| Program | master | this branch |
|---|---|---|
| bead repro, one traversal | 15,179,573 | 14,579,701 |
| bead repro, two traversals | 29,598,736 | 14,580,439 |
| `foldr(f, [])` with `f` above, two traversals | 30,356,525 | 14,580,196 |

Benches `tests/harness/bench/001–022` (ticks, master → branch): most are
unchanged or fall. `003_smarter_fib` falls from 89,556 to 7,175, `022_hof_fold`
from 43.1M to 40.7M, `016` from 6.05M to 5.93M and `019` from 911k to 875k.
The largest rises are `002_thunk_updates` (515,229 → 520,228) and
`004_generations` (465,418 → 470,408), both about 1%. No wall-clock claims
are made.
