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
code than the bead's un-annotated framing suggests. Confirmed in BOTH
prelude modes: source-mode at N=300,K=20 gives 206,363 ticks (vs 13,947 for
the no-metadata control at the same point) — statistically identical to
blob mode's 207,977 vs 12,963. Blob mode is NOT immune.

## ADDENDUM 2: eu-6xgtk re-measured clean (no metadata anywhere)

Team-lead follow-up: eu-6xgtk's own treatment/control pair both carried
`` ` :suppress `` on every binding (confirmed by reading, not measuring
from, quill's iso-* fixtures), so its withdrawn tick evidence was confounded
by the SAME defect as above, not a clean test of beta_reduce's occ_in_lam
gap. Rebuilt both fixtures with zero metadata:

```
slow(n): if(n <= 0, 0, 1 + slow(n - 1))
build(mk, x, ys, xs):
  if(xs nil?, [], cons(mk(ys map(_ + x)), build(mk, 0, ys, tail(xs))))
N: <N>; K: <K>; ys: range(0, K); xs: [0]
# treatment
result: build(identity, slow(N), ys, xs) head sum
# control adds: x0: slow(N)   and calls build(identity, x0, ys, xs)
```

Full N×K grid (blob, post-#1088), same N/K sets as above:

TREATMENT — N-slope scales exactly linearly with K: 32.0 (K=1), 160.0/K=5
(32.0/K), 640.0/K=20 (32.0/K), 2560.0/K=80 (32.0/K). Every K gives
**N-slope = 32.0 × K, exact to 4 significant figures at every grid point**
(e.g. N=600,K=80 = 1,559,331 ticks vs N=50,K=80 = 151,331; diff/550 = 2560.0).

CONTROL — flat: N-slope is 32.0 at K=1 AND 32.0 at K=80, identical, matching
the additive signature throughout — e.g. N=600,K=80 = 41,108 vs N=50,K=80 =
23,508, diff/550 = 32.0.

This is the clean, unconfounded tick proof eu-6xgtk's acceptance criterion
(a) asked for: the raw-expression-substituted-under-a-lambda treatment costs
exactly K× the shared-Var control, with no metadata involved. `dump pruned
--embed` was inconclusive by manual inspection (the nested unrolled
map/build core is too deep to eyeball reliably) but the tick evidence alone
is dispositive and matches the acceptance criterion's own bar ("measured
tick counts showing the recomputation"). eu-6xgtk's reachability verdict (a)
should be considered CONFIRMED at the performance level too, not just
structurally.

## ADDENDUM 3: narrowing the root cause for the metadata trigger

Traced further into the VM. `StgSyn::is_whnf()` (syntax.rs:244-253) feeds
`take_lambda_form` (compiler.rs:559-577): a metadata-annotated binding
compiles as `dsl::value(...)` (non-updateable) instead of `dsl::thunk(...)`.
Checked whether eu-wpswc's settled-slot pass-through (`is_settled`,
`env_builder.rs:87-98`) is involved: it is NOT — `is_settled` requires
either `arity() > 0` or `code` matching `HeapSyn::Atom{..}`; x0's Value-form
closure has arity 0 and code `HeapSyn::Meta{..}`, so `is_settled` returns
false and x0 keeps the OLD alias-wrapping path, ruling out the mechanism I
originally suspected.

`HeapSyn::Meta{meta,body}` is handled by `vm.rs:706-708` →
`return_meta` (`vm.rs:907-948`): when the continuation on top of the stack
is neither `DeMeta` nor `Update` (the common case — e.g. an ordinary strict
force from `+`), it does `self.closure = resolve(body); self.stack.push(other)`
— i.e. it re-resolves `body`'s `Ref` and continues, WITHOUT ever pushing an
`Update` continuation for x0's own slot (there is none, because x0 is a
Value form). So every one of the K times `_+x0` reads x0, the VM genuinely
re-executes `return_meta` from scratch. Confirmed `body` itself is still
correctly compiled as a real `Thunk` (`dump stg` shows `thunk @[..] -> ...`
for it), which under normal update semantics should memoise itself on first
force regardless of how many times its containing Meta wrapper is
re-entered — so on this trace alone, only the FIRST of the K reads should
pay `slow(N)`'s full cost and the rest should be cheap once `body`'s own
update-slot is written. That is NOT what is measured (full O(N) cost on
every one of the K reads), so `body`'s `Ref` must not be resolving to one
stable, shared physical slot across the K reads — consistent with, but not
proven to be, `env_from_data_args` (or the Value-closure's own synthetic-let
re-materialisation in `take_syntax`, compiler.rs:583-597) rebuilding a fresh
copy of the frame containing that slot at each of the K entries. **I could
not close this final link within this task's scope — it is confirmed to be
`return_meta`'s no-update-continuation path plus something that stops
`body`'s target slot count as "the same slot" across K entries, but I did
not identify which allocation site re-creates the frame.** Named candidate,
not a proven mechanism.

## ADDENDUM 5: allocation-delta test confirms fresh re-materialisation, not re-forcing

Team-lead's reframe: eu-wpswc was ticks-quadratic with allocations FLAT
(re-evaluation, no re-allocation); here allocations grow proportionally with
K, which points at fresh allocation per entry, not merely a cheap re-force of
an already-memoised thunk. Tested directly. N=300, metadata-on-x0, allocs at
K=1/2/5/20/80: 5,040 / 7,474 / 14,775 / 51,281 / 197,306. Per-K delta is a
constant **2,433.7 allocs/K** at every consecutive pair (K2−K1, K5−K2/3,
K20−K5/15, K80−K20/60 all agree to 1 decimal place). Standalone `slow(300)`
alone allocates **2,427**. The two numbers match to within 7 allocs (a small
constant, plausibly one extra `Atom`/`Sym` node per read) — **each additional
K unit allocates almost exactly one full fresh `slow(N)` computation graph**,
not a cheap re-force of a shared one. This settles the "why" question the
missing link was chasing: there is no already-memoised thunk being redundantly
re-entered — a fresh, never-forced copy of the whole computation is minted
per entry, and each copy dutifully self-memoises once into a slot that's then
discarded.

Named the strongest structural candidate for the allocation site:
`create_arg_array` in `env_builder.rs:347-377`. For a `Ref::L(i)` argument
where `is_settled` returns false (x0's case — arity 0, code
`HeapSyn::Meta{..}`, not `HeapSyn::Atom{..}`), it does:
```rust
array.push(self, SynClosure::new(self.atom(syn.clone())?.as_ptr(), environment));
```
(`env_builder.rs:370-373`) — `self.atom(...)` allocates a fresh
`HeapSyn::Atom{evaluand: Ref::L(i)}` node on every call. This is the
documented eu-wpswc alias-wrapping default path, firing here because x0 fails
`is_settled`. **Not proven**: I could not trace why entering this alias
repeatedly across the K reads fails to bottom out at `body`'s *same* physical
update-thunk slot after the first force — by the code read alone, the alias's
captured `environment` pointer and `body`'s `Ref::L` look like fixed
references to one frame, which should still memoise after entry 1. Either
`create_arg_array` (or the closure-capture step that builds `_e_a`'s own
free-variable environment, not directly inspected) runs once per K
application rather than once per closure creation as the core-level "one Lam
literal" evidence suggested, or something else re-severs the reference —
which one is unresolved. Stopping here per scope; handing to furnace with the
allocation evidence, the named candidate line, and this open question.

## ADDENDUM 4: eu-frf2o checked and ruled out

Team-lead suggested eu-frf2o (P0, CLOSED, PRs #1081/#1087, commits 6484bcc3/
de47fa17: "STG compiler emitted a Let where a LetRec was needed for the Meta
binder group") as a candidate for the same corner of the compiler. Built a
worktree at 1bd8bf6f (parent of #1081's merge — the commit immediately
before both PRs), source-prelude mode (blob not regenerated at that commit
for this check). Ran the metadata-on-x0 trigger and its no-metadata control
at N=300,K=20:

pre-eu-frf2o (1bd8bf6f): metadata trigger 207,137 ticks; no-metadata control
14,719 ticks.
post-eu-frf2o (master 642b5e2d, source mode): metadata trigger 206,363
ticks; no-metadata control 13,947 ticks.

Statistically identical before and after both PRs (well within run-to-run
noise). **eu-frf2o is RULED OUT** — it fixed a different defect (a
freshly-synthesised 2-entry Let for an INLINE block-literal-as-call-operand's
own meta/body pair) that doesn't touch the mechanism causing x0's outer
binding to lose memoisation when captured by a repeatedly-invoked closure.
Confirmed by reading `compile_binding`'s `Expr::Meta` arm
(compiler.rs:1867-1905): it already calls `binder.ensure_recursive()` before
compiling `m`/`b` and adds them as siblings in the SAME (already-LetRec)
top-level binder frame as `x0`'s other siblings, not a separate synthetic
Let group — so eu-frf2o's specific fix does not apply to this binding shape
in either direction. The missing link in ADDENDUM 3 remains open.
