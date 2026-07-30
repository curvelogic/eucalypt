# eu-bc34x replication attempt: does map's captured closure recompute a shared outer thunk K times?

Stopwatch, 2026-07-30. Directed replication task, independent worktree/fixtures (not quill-6xgtk's).
No production code change, no PR — measurement only.

## Verdict: DOES NOT REPLICATE

Built fresh from `origin/master` (642b5e2d, PR #1088/eu-wpswc already merged) in
`/Users/greg/dev/curvelogic/eucalypt-worktrees/stopwatch-bc34x`, clean release
build, "Compiling eucalypt" confirmed both before and after `cargo xtask
prelude-compile` + rebuild. Also built the pre-#1088 commit (a1af1f0b) in a
second worktree for comparison. All numbers are TICKS (`-S`), all `eu` calls
wrapped in `timeout 60` with `--heap-limit-mib 4096`.

Fixture (as specified in the dispatch):
```
slow(n): if(n <= 0, 0, 1 + slow(n - 1))
ys: range(0, K)
x0: slow(N)
result: ys map(_ + x0) sum
```
`dump pruned --embed` confirms, at every K tested including K=80: exactly one
`_e_a[..]` lambda literal and `x0` a genuine `Var::Bound` — matches the bead's
own structural claim. `map` itself is inliner-unrolled a small FIXED number of
times (8 `c-lam` copies, independent of K) then falls back to genuine runtime
self-recursion — so this is a real runtime-recursion test, not a fully-unrolled
artefact. Result values checked against the closed form `K*x0 + K(K-1)/2` and
matched exactly at every grid point (e.g. N=300,K=80 → 27160).

### Grid, post-fix (642b5e2d), BLOB mode, ticks

| N\K | 1 | 5 | 20 | 80 |
|---|---|---|---|---|
| 50  | 2398 | 3449 | 7390 | 23155 |
| 150 | 5598 | 6649 | 10590 | 26355 |
| 300 | 10398 | 11449 | 15390 | 31155 |
| 600 | 19998 | 21049 | 24990 | 40755 |

N-slope at K=1: 32.0 ticks/N-unit. N-slope at K=80: **32.0 ticks/N-unit,
identical**. Standalone `slow(N)` alone: 1780/4980/9780/19380 at
N=50/150/300/600 → 32.0 ticks/N-unit — matches exactly. Per-K slope at every N
is a CONSTANT 262.7 ticks/K, independent of N. This is the textbook
properly-shared signature: `total ≈ slow(N) + c·K`, not `c·N·K`. The
discriminator the dispatch asked for (N-slope vs K) shows **no** dependence.

Allocs: N=300 row is 2611/2730/3176/4961 at K=1/5/20/80 — ~29.75 allocs/K,
flat in N, consistent with ordinary map/sum bookkeeping, not re-execution of
`slow`.

### Same grid, post-fix SOURCE-prelude mode (N=300 row)

K=1/5/20/80 → 10227/10974/13776/24981 ticks. Per-K slope 186.75, again
constant — additive, not N-multiplied. Absolute constant differs from blob
(no prelude inlining in source mode) but the qualitative pattern is identical.

### Same grid, PRE-fix (a1af1f0b, before PR #1088), SOURCE and BLOB modes

N-slope at K=1 and K=80 is 32.0 ticks/N-unit in **both** cases, in both source
and blob mode — i.e. the x0-sharing property already held before eu-wpswc's
fix. Per-K slope is NOT constant pre-fix (208.75→225.2→277.8 ticks/K growing
with K in source mode; 291.75→305.3→357.9 in blob mode) — that mild
superlinear-in-K growth is eu-wpswc's own defect (map's curried self-call
alias chain, unrelated to x0), fully flattened to a constant post-#1088. But
N-slope-vs-K, the specific thing eu-bc34x claims, was never present, before
or after #1088.

### Not map-specific: user-written walker, non-anaphoric closure

```
adder(x, v): v + x
x0: slow(N); f: adder(x0)
walk(l): if(l nil?, l, cons(l head f, l tail walk))
result: range(0, K) walk sum
```
Post-fix, blob, N=300: K=1/5/20/80 → 10357/11261/14651/28211, per-K slope
exactly 226.0 throughout. N-slope at K=1 and K=80 (N=50,600 checked): 32.0 in
both. Same additive signature with no `map` involved and a PAP-of-named-
function closure instead of an anaphoric lambda.

## Relationship to eu-wpswc / PR #1088

Distinct question, not the same defect. eu-wpswc was O(depth²) in map's OWN
recursion depth with FLAT allocations, fixed by settled-slot pass-through in
`env_builder.rs`/`bytecode/machine.rs`. eu-bc34x's claimed signature (N-slope
scaling with K) does not appear pre- or post-#1088 in these fixtures — there
is nothing here for #1088 to have fixed, and nothing left to fix.

## Root cause

None found — not reachable, so nothing to root-cause. `env_from_data_args`
copying closures remains a real architectural concern per other lazy-streams
notes, but this specific fixture shape does not exercise a defect in it.

## What would change the verdict

Could not rule out: a shape closer to `eu-6xgtk`'s curried-lambda-under-lambda
case, or non-map/non-walker consumers (folds, generators). Grid script and all
fixtures are namespaced `stopwatch-bc34x-*` under
`/private/tmp/claude-501/-Users-greg-dev-curvelogic-eucalypt/3e1c2e8f-0286-4e9c-97ff-07b7206fe27a/scratchpad/`
for anyone who wants to extend the grid.

## ADDENDUM: found the real trigger by diffing against the original fixture

The dispatch's fixture text (no metadata) does not replicate (above). But the
actual `.eu` files the original report was measured from
(`quill-6xgtk-iso-N*-K*.eu`, read for comparison only, not reused for
measurement) carry a `` ` :suppress `` metadata annotation on every binding —
not shown in the bead's prose. Running that exact file on this clean binary
DOES reproduce the reported numbers precisely (K=1/10/40 → 20552/109209/
404731 ticks, N=300).

Isolated by selectively annotating one binding at a time (N=300,K=10): metadata
on `x0` alone → 109,160 ticks (matches full-file). Metadata on `N`/`K`/`ys`
but NOT `x0` → 12,963 ticks (matches the no-metadata baseline). Not
`:suppress`-specific — a plain doc string on `x0` reproduces it too (118,904
ticks). So the trigger is: **a metadata/backtick annotation on the captured
binding**, not "any lambda captured once and invoked K times" as titled.

Full N×K grid with metadata on `x0` (blob, post-#1088, N∈{50,150,300,600},
K∈{1,5,20,80}): now genuinely multiplicative. N-slope at K=1 is 64.0
ticks/N-unit; at K=80 it is 2592.0 ticks/N-unit (40.5× larger, not 80×, but
clearly K-dependent, unlike the flat 32.0 in every non-metadata variant
above). Per-N-per-K converges toward ~32 as K grows (K=80: 32.4), in the same
ballpark as the ~53 figure in the original report. Checked pre-#1088
(a1af1f0b) at N=300,K=20: 208,878 ticks vs post-fix's 207,977 — statistically
identical, so this is pre-existing and unrelated to eu-wpswc/PR #1088.

`dump pruned --embed` still shows one shared `_e_a[..]` lambda and a genuine
`Var::Bound` reference to `x0` at every K — identical core structure between
the metadata and no-metadata cases except `x0`'s own RHS is `[c-meta, ...]`
instead of `[c-app, ...]`. So this is not core-level duplication.

STRONG ROOT-CAUSE CANDIDATE (not fully traced to the VM instruction level):
`StgSyn::is_whnf()` in `src/eval/stg/syntax.rs:244-253` unconditionally
matches `StgSyn::Meta { .. }` as WHNF:
```rust
pub fn is_whnf(&self) -> bool {
    matches!(self, StgSyn::Cons { .. } | StgSyn::Meta { .. }
        | StgSyn::Atom { evaluand: Reference::V(_) })
}
```
This feeds `ProtoSyntax::take_lambda_form` (`src/eval/stg/compiler.rs:559-577`),
which skips the update thunk (`dsl::value` instead of `dsl::thunk`) whenever
`syntax.is_whnf()` is true. A `Meta{meta, body}` node is only actually WHNF if
`body` itself already denotes a forced value — here `body` is a `Ref` to a
genuine `slow(N)` application thunk (confirmed still compiled correctly as
`thunk @[..] -> ...` in `dump stg`), not yet evaluated. Compiling `x0`'s
binding as a Value form instead of a Thunk means the K separate entries this
closure sees (via whatever settled-slot / arg-array path applies) each
re-execute the `Meta` node's evaluation rather than sharing one memoised
result — consistent with, but not proven identical to, the
`env_from_data_args`-copies-closures architectural blocker. I did not trace
the exact VM instruction sequence that turns "no update thunk on the Meta
wrapper" into "the *inner* `slow(N)` thunk gets fully re-run" rather than
"cheap re-force of an already-memoised inner thunk" — that's the missing link
for a complete proof.

PRACTICAL IMPLICATION: every prelude/user binding with a doc string, type
annotation, or other metadata that is captured by a closure invoked
repeatedly is a candidate victim. This is arguably far more common in real
code than the bead's un-annotated framing suggests (prelude bindings
routinely carry doc metadata on the source-prelude path).
