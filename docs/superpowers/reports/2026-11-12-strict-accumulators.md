# Strict accumulators by demand analysis (eu-oaz72.27)

Date: 2026-11-12. Bead: eu-oaz72.27. All figures **measured-single**: one
machine, one session, release build, blob-mode prelude,
`EU_PP_THRESHOLD=999999999`. Wall time is the median of 7 interleaved
master/branch pairs, with the range.

## Change

`src/core/analyse_demand.rs`:

1. A let group's lambda signatures are a fixpoint over their own calls,
   starting optimistic (every parameter strict and used once). A saturated
   call through a let-bound variable uses its signature; partial
   applications stay lazy. On reaching the iteration cap the signatures
   are reset to pessimistic for one final round. The STG compiler
   (`src/eval/stg/compiler.rs`) matches this: it applies a signature's
   strict demands only to a call with at least as many arguments as the
   signature, so a partial application such as `loop-a(panic("x"))`
   forces nothing.
2. A variable both branches of an `IF` force is strict.
3. A lambda value (not applied where it is built) places only lazy demand
   on what it captures. On master `k(a): { y: • }.(a + y)` was strict in
   `a`, so `k(panic("x")) block?` panicked: the open note from PR #1156
   ("strictness flows out of a lambda body unscaled") was observable.

## Loop A

`loop(a, n): if(n = 0, a, loop(a + 1, n - 1))`, `loop(0, 1000000)`; the
same via `(n = 0) then(...)` gives the same figures.

| | master | branch |
|---|---|---|
| ticks | 37,000,191 | 40,000,191 |
| max stack | 2,000,002 | 7 |
| wall | 3.485 s [3.211, 3.579] | 1.468 s [1.372, 1.511] |
| peak RSS | 1010 MiB | 64 MiB |

Ticks rise by 3 per iteration: a strict argument is still allocated as a
thunk and then forced (`let x = thunk in seq x in f(x)`). Before, the same
thunks were forced at the end through a 2-million-frame stack.

## foldl-strict: not in this change

The owner fixed the name and the plan (§6) the intent, but the acceptance
criterion ("constant stack and fewer ticks than foldl") is not reachable
without an owner decision:

- Demand analysis cannot make the accumulator strict: it is passed to an
  unknown `op`. `foldl` stays lazy, as intended.
- A source-level guard that forces the accumulator (`number?(i) ∧ false`,
  `if(i nil?, l, l)` and others) gives constant stack, but costs 11–20
  ticks per element. With the guard, `range(0, 1e6) foldl-strict(+, 0)`
  costs 113–121 M ticks in source mode against `foldl`'s 101.75 M. A lazy
  fold with the same guard on `l` costs the same, so the cost is the guard
  and not the strictness. Moving `sum` and `count` to it raised blob-mode
  `sum` from 136.0 M to 159.0 M ticks (it is already constant-stack there).
- Fewer ticks than `foldl` needs both a cheap force (a `seq` primitive:
  owner decision) and call-by-value compilation of strict arguments
  (evaluate in place rather than thunk-then-`seq`). The second also
  removes the +3 ticks per iteration of loop A above.

Master baselines over `range(0, 1e6)`:

| | source mode ticks / max stack | blob mode ticks / max stack |
|---|---|---|
| `sum` | 101.75 M / 2,000,006 | 136.0 M / 11 |
| `foldl(+, 0)` | 101.75 M / 2,000,006 | 129.25 M / 2,000,005 |
| `count` | 87.75 M / 2,000,003 | 132.0 M / 11 |

## Known limit: signatures shared by name

The STG compiler finds a callee's signature by name, so every binding of a
name shares one entry. Two let-bound lambdas of the same arity (two local
`go` loops) share the lub of their signatures, which is sound for both.
Before, the last one recorded won, and with strict accumulators that forced
a lazy loop's argument. The lub alone is not enough when a name is also
bound to something whose signature is unknown: an alias (`step: f`), a
lambda parameter, or a lambda of another arity, where in blob mode the
compiler would fall back to a prelude signature of the same name. Such a
name gets an empty signature, so no call through it forces an argument
(`ambiguous_names` in `src/core/analyse_demand.rs`). The cost is precision:
a strict loop that shares its name with a lazy one, or with a parameter,
keeps an O(n) stack. Keying signatures by binder in the compiler would
remove this.
