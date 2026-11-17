# eu-oaz72.175: why lazy `foldl` still retains the list under closed-binding

Status: diagnosis only, no source change. Figures are measured-single (one
release build, source prelude, 200k cells, `eu -S`), with the closed-binding and
tracer prototypes from this lane applied locally and not committed.

## Finding

`foldl(+, 0)` is not pinned by a stray frame reference of the kind
closed-binding removes. It retains because `foldl` is **lazy in its
accumulator**: each iteration allocates an accumulator thunk `op(i, l head)`
that is never forced until the loop ends, so 200k thunks stay live, and each
thunk's environment is the iteration's whole frame `(op, i, l)`. The frame holds
`l`, the current cell, so the chain of thunks keeps every cell live.

Evidence (peak blocks, closed-binding applied):

| program | peak blocks |
|---|---|
| `range(0,200000) foldl-strict(+, 0)` | 1,026 |
| `range(0,200000) count` | 1,026 |
| `range(0,200000) foldl(+, 0)` (`a`) | 11,001 |
| user clone `fl(op,i,l): if(l nil?, i, fl(op, op(i, l head), l tail))` with global `+` | 10,954 |
| list-free lazy chain `fc(op,i,n): if(n=0, i, fc(op, op(i,n), n-1))` | 3,513 |
| lazy chain, list walked but **not used by the accumulator**: `fz(op,i,n,l): if(l nil?, i, fz(op, op(i,n), n+1, l tail))` | 11,513 |
| `fl` that never builds an accumulator (`fl(op,i,l): if(l nil?, i, fl(op, i, l tail))`) | 1,026 |

Reading: the strict fold with the same list is constant, so the list itself is
not pinned by the call shape. The list-free lazy chain costs 3.5k blocks on its
own (the unavoidable thunk chain). Adding a list that the accumulator does not
even read (`fz`) takes it to 11.5k, same as `foldl`: the dead slot `l` is
captured by the accumulator thunk's frame. The user clone `fl` with a global `+`
leaks identically, so this is independent of any `op` closure.

Tracer (`EU_DBG=1`, op 6M, `a.eu`): the pending Branch continuation's env
reaches 480k frames; the BFS path from the current env runs next/next, a Cons
closure slot (op 3, the list cell), then next frames, then an Atom alias slot
(op 1). That is the list reached through frame chains, as in the earlier notes.
I did not isolate the chain further because the controls above already
discriminate the cause.

Side note: `eu dump inlined` shows `foldl` unrolled once at the call site, with
the recursion going to the global `foldl`; the "Let of fs/+/0" shape in the brief
is not what the source prelude produces for `foldl(+, 0)`.

## What would fix it, and what would not

1. **Closed-binding cannot.** The accumulator thunk is not closed; it reads
   `op`, `i` and `l` from the iteration frame.
2. **Flat (capture-trimmed) thunks** fix `fz`: the accumulator reads only
   `op, i, n`, so a thunk closed over a copy of just those slots drops `l`. This
   generalises closed-binding (a closed closure is the zero-capture case) but
   is the flat-closure design Decision 33 declined, with per-thunk allocation
   cost on every lazy call. It requires a capture list in the form header (blob
   wire-format change) and only helps when all free refs live in the immediate
   frame.
3. It does **not** fix real `foldl`: there the accumulator reads `l head`, so
   the thunk legitimately holds `l` (a nested `HEAD(l)` thunk captured by the
   accumulator). Fixing it needs a *selector-thunk* rule as well: when an
   argument thunk is `HEAD(x)`/`TAIL(x)` of a local already evaluated to a Cons,
   allocate an alias of that field's closure instead of the selector thunk
   (GHC's selector-thunk optimisation). Sound (same value, no new forcing),
   detectable in the predecoder from the thunk body, no wire change by itself,
   but the accumulator's own env must also be flat or it pins `l` through its
   frame. So 2 and 3 are both required.
4. Prelude-only rewrites cannot help: any lazy thunk selecting from `l` pins `l`,
   and a call cannot return an unevaluated value.

## Recommendation

Treat lazy `foldl` retention as inherent to cactus-environment thunks plus the
documented lazy accumulator, not as a defect in the pinning fix. Land
closed-binding for `count`, `foldl-strict`, `sum` and wrapper shapes (all
constant space). Keep the 022 bench as is (it is an env-walk benchmark, and the
chain is O(n) by design); do not gate a retention test on lazy `foldl`. Gate on
`count`, `foldl-strict` and the `c3` wrapper. If lazy `foldl` space is wanted,
open a separate, owner-reviewed bead for flat thunks plus selector thunks
(medium-to-large, touches the encoder, predecoder, both dispatch paths and the
blob wire format; sensitive-area review rules apply).

## Not done

- No experiment applying flat thunks or the selector rule (only reasoning plus
  the controls above).
- No timing claims; blocks and ticks only.
