# Non-integer counts and indices are an error (eu-oaz72.58)

2026-11-13. Implements the owner decision on bead eu-oaz72.58 (AoC 2026
preparation epic eu-oaz72), recorded 2026-11-12: a non-integer count or
index to the list count/index functions is an error, as a negative `nth`
index became in eu-oaz72.43.

## What changed

`take`, `drop`, `split-at`, `rotate`, `nth`, `update-nth`, `window`,
`window-all` (and so `partition`, `partition-all`) reject a numeric count,
index, size or step that is not an integer. The diagnostic names the
function and the value, e.g. `take count 1.5 must be an integer`,
`nth index 0.5 must be an integer`, `window step 0.5 must be an integer`.

Before this change `take`/`drop`/`split-at`/`rotate`/`update-nth` acted as
if the count were rounded up (`take(0.5)` kept one element) and
`nth(0.5)`/`window` gave an unrelated diagnostic.

A single private helper `__require-int(what, n, v)` returns `v`, or panics
`"{what} {n} must be an integer"` when `n` is a number whose `floor` is not
itself. It guards non-numbers through — a symbol passed to `nth` keeps its
existing type-mismatch diagnostic. The check runs once per call, before the
existing negative/zero checks, never per recursion step: `take` and `drop`
delegate their recursion to private `__take`/`__drop`, which `window` and
`window-all` also use in their loops once `n` and `step` are validated. The
three helpers carry `export: :suppress`, so they stay out of the generated
prelude reference and its counts. An integral float such as `2.0` is
accepted.

## Incidental bug fix: `number?` on arithmetic results

The guard uses `n number?`. That exposed a latent bug: `number?` (the
`ISNUMBER` intrinsic) only recognised a *boxed* number, so an unboxed
native produced by arithmetic was reported as a non-number —
`(2.5 - 0) number?` and `(1 + 2) number?` both returned `false`. Without
the fix `take(3 - 1)` would skip the integer guard. `ISNUMBER` now also
accepts a bare `Native::Num`, matching how `number?` is documented and how
boxed/native numbers are otherwise interchangeable. This is a bug fix to an
existing intrinsic, not a new API.

## Verification

- Harness checks: integer counts and integral floats still work, and a
  computed count (`take(3 - 1)`) works, in `tests/harness/036_takes_and_drops.eu`;
  `number?` on arithmetic results in `tests/harness/126_type_predicates.eu`.
- Ten error fixtures `tests/harness/errors/274_non_integer_*`, one per
  function/parameter, each fault-injection verified: removing the
  `__require-int` guard makes all ten FAIL; restoring it makes them PASS.
  The `number?` fix was fault-injection verified against `126`.
- Blessed diagnostics snapshots for the ten fixtures (new fixtures only; no
  existing diagnostic changed). `DIVERGENCE.md` updated.
- `cargo fmt`, `cargo clippy --workspace --all-targets -D warnings`,
  `cargo test` all clean. Prelude blob regenerated; prelude reference and
  cheat sheet / agent reference updated.

## `range` and the `sample` functions

Coordinator ruling under the owner decision ("all prelude count/index
parameters"): the `n` of `random.sample`, `set.sample` and `vec.sample` is
a count, so a non-integer `n` is an error (`random.sample count 1.5 must be
an integer`, and likewise `set.sample count`, `vec.sample count`), with an
error fixture each. `range(b, e)` takes bounds, not a count, so it keeps
master's behaviour via the unchecked `__take` (`range(0, 2.5)` is
`[0, 1, 2]`); a harness check in `036_takes_and_drops.eu` pins it.

## Also in this PR (coordinator follow-ups from eu-oaz72.71)

- CHANGELOG entry for `drop(n)` past the end returning `[]` (PR #1172).
- The local nil-safe `drop` in `tests/harness/014_numeric_combinators.eu`
  is removed; the test now uses the prelude's `drop`.
