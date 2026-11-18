# eu-oaz72.175: why the `then` wrapper still pins the list (diagnosis)

Status: cause found, one-line prototype measured. Figures are measured-single
(one release build, one machine, deterministic peak blocks and ticks; no wall
times, no performance claim). Base: PR #1268 head f0dbf81. 200k cells.

## Result

| wrapper shape | base (closed bindings) | prototype |
|---|---|---|
| `if(l nil?, i, …)`, source prelude | 1,026 | 1,026 |
| `l nil? then(i, …)`, source prelude | 11,278 | **1,026** |
| `then` shape, ticks (predecoded / byte) | 23,200,268 / 28,200,268 | 23,200,259 / 28,200,268 |
| `if` and `then`, prelude blob | not re-measured | 6,552 / 6,528 (both shapes, known blob residual) |

Benches 015..022 (source prelude): ticks and peak blocks identical to base in
every file. `cargo test --release --test harness_test`: 712 passed.

## Cause

The hypothesis (branches become closures of a prelude call) is only half right.
`then` is already inlined; the bug is that the inlined body is left as an
**unreduced curried redex**.

`eu dump inlined` of the `then` fixture shows, in `c3`'s copy of `fs`:

    [c-app, [c-app, [c-lam [t f c] IF(c, t, f)], [c-args X-else-branch …]], [c-args nil?(l)]]

i.e. `then(i, X)(nil? l)` with the `then` lambda spliced in but never applied.
At runtime `X` (the recursive branch, referencing `l`, `op`, `i`) is an
ordinary non-closed thunk argument allocated in the wrapper frame, the
`λt f c` is allocated and called, and the `IF` is inside that lambda. The
branch thunk captures the wrapper frame (which holds the list head), so it is
not closed and pins the frame. With `if` there is no lambda and no thunk: the
`IF` intrinsic compiles to a case in the wrapper's own code.

Why it is left unreduced: `distribute` (`src/core/inline/reduce.rs`) first
pre-expands the prelude's inlinables into each other, so `fs`'s value already
contains the `then` lambda in curried form. It deliberately does not substitute
`fs` into itself (eu-oaz72.147), but it does substitute `fs`'s value into
`c3`'s call site. `beta_reduce`'s saturated-`Lam` case reduces the *arguments*
and then `substs_depth`es them into the callee body, but never re-walks the
result, so any redex inside the copied body survives. `fs`'s own standalone
binding is reduced (it is walked by `beta_reduce` as a binding value), which is
why only the specialised copy pins. `saturate_curried_inline` (eu-oaz72.34)
would have merged and reduced it had `beta_reduce` visited it.

This generalises beyond `then`: any inlinable combinator (`when`, operator
sections, `.result` forms) used inside a recursive combinator that is then
copy-specialised at a saturated call site stays as an unreduced lambda
application in the copy.

Not a blob issue: this reproduces with no `prelude.blob`. Bead
eu-oaz72.106 (blob user calls of `if`/`then` get signatures but no branch join)
is a different, demand-analysis matter and is not involved here; `if` is not
special-cased in the inliner (it is the `__IF` intrinsic, tagged by name in
`tag.rs` and `analyse_demand.rs` only for demand).

## Option 1 (recommended): reduce the copied body — prototyped

Throwaway one-line change in `beta_reduce`, saturated-`Lam` case, before
counting occurrences and substituting:

    let body = open_lam_scope(scope);
    let body = if binders.len() == xs.len() { beta_reduce(&body)? } else { body };

The recursive reference in the copy is still a variable (self-reference is not
substituted), so there is no unbounded unrolling. Reduction of an already
reduced body is a no-op walk (`try_walk_safe` preserves sharing), so cost is
one extra traversal per inlined call site; compile time was not measured.

- Size: ~5 lines plus a harness/gate test (the gate fixture goes back to the
  idiomatic `then` form, which restores style-guide conformance); the closed
  bindings change itself is untouched.
- Risk: low-medium. It changes the shape of core for every program with a
  copy-specialised recursive combinator containing inlinable calls, so
  snapshots, demand signatures and `eu dump` outputs could shift. In the
  prototype the harness (712) passes and benches 015..022 are identical, but
  `cargo test` in full, blob mode (`scripts/blob-mode-tests.sh`) and the
  diagnostics snapshots were not run. Treat as an intentional-semantics-adjacent
  change for `REVIEW.md` purposes (core transform, owner-visible).
- Blob wire format: **no change.** The fix is in the source-side inline pass.
  If `src/core/inline/reduce.rs` is a blob freshness input the blob regenerates,
  but the format version does not move. (`xtask`'s baked `inlinable_bindings`
  are pre-expanded the same way and would shift; regenerate and re-run both
  modes.)
- Blob mode: the fix does not change the ~6,500 residual (a separate second pin,
  also present for the `if` shape), so blob mode is no better or worse.

## Option 2: capture-trimmed branch thunks (not prototyped)

Would make the unreduced form harmless by giving the branch thunk only the
slots it uses. This is flat closures, which decision 33 declined; it needs
encoder, predecoder, both dispatch paths and a blob wire format change. It
would also leave the per-call `λt f c` allocation and extra ticks. Not
recommended while Option 1 removes the redex entirely.

## Reproduce

Fixtures: `tests/fixtures/list_retention_wrapper.eu` (the `if` shape) and the
`then` variant from commit d0930be. `eu -S <file>` reports peak blocks and
ticks; `eu dump inlined --embed <file>` shows the unreduced `[c-lam [t f c]…]`
inside the specialised copy. Fault injection for a real fix: revert the
`beta_reduce` line and the `then` gate must FAIL (11,2xx blocks).
