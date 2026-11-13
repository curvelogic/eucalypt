# Prelude demand signatures: where they should come from

**Bead:** eu-oaz72.75 (design review, AoC 2026 preparation epic eu-oaz72)
**Date:** 2026-11-13, at master `b8bfb43`
**Status:** design proposal for the owner's decision. No code changes are proposed in this PR.

## Summary

The hand-written prelude signature table in `build_prelude_signatures()`
claims more strictness than the prelude has for **31 of its 64 entries**, and one more (`nth`) changes which error is reported. In
blob mode, 23 small programs that are correct and lazy give a different
answer from source mode: 22 panic where source mode returns a value, and
one reports a different error (§2.2). Two further defects come from looking signatures up by name. A
user's helper can lend its signature to the prelude function of the same
name. And whether the table applies at all depends on unrelated bindings
elsewhere in the program (§2.3).

**Recommendation.**

1. Stop the unsoundness now with a bug fix: an audited table, plus a
   scoping fix to the lookup.
2. Then replace the table with signatures **derived by the same demand
   analysis when the blob is built**. Store them in the blob per global
   slot and look them up by slot, not name (options (a) and (d) below).
3. Then key user-code signatures by binder (eu-oaz72.66), so that
   `ambiguous_names` and every other name-keyed table go away.

Intrinsic signatures stay where they are, in the intrinsic catalogue.

Removing the over-claims costs nothing in ticks: the 22 benches in
`tests/harness/bench/` use up to 5% fewer ticks with the audited table and
up to 8% fewer with no table (one bench +0.1%). It does cost stack in one pattern that
matters for AoC: `range(…) map(…) sum` goes from constant stack to O(n) in
blob mode. That is what source mode already does today, so the fast blob
result was coming from an unsound claim (§4).

## 1. How it works today

| Piece | Where (at `b8bfb43`) | Keyed by |
|---|---|---|
| Intrinsic signatures, derived from the catalogue's `strict_indices` plus hand refinements for `IF`/`AND`/`OR`/`LOOKUPOR`/`IO_BIND` | `src/core/analyse_demand.rs:139` `build_intrinsic_signatures` | intrinsic name (only for an `Expr::Intrinsic` callee) |
| User signatures from demand analysis, lub-merged across same-named let-bound lambdas | `analyse_demand.rs` `record_named_signature`, returned as `NamedSignatureTable`; `src/driver/eval.rs:503` | binding **name** |
| Names whose user signature would be unsound (parameters, non-lambda bindings, mixed arity) get an empty signature, program-wide | `analyse_demand.rs:67` `ambiguous_names` | name |
| Prelude table, 64 hand-written entries, built only when a blob is in use | `analyse_demand.rs:220` `build_prelude_signatures`; `src/eval/stg/compiler.rs:1664` | prelude **name** |

The consumer is `ProtoAppGroup::take_syntax` (`compiler.rs:1206–1219`). It
tries the intrinsic table first, then the user table, then the prelude table,
always by the callee's name. A `Var::Bound` callee and a `Var::Free` callee
are treated alike. A signature applies to any call with at least as many
arguments as it has entries. A complex argument in a `Strict` position is
bound and then `seq`-forced before the call. A signature of exactly the
call's arity also selects `DirectApp` dispatch (`compiler.rs:1312`, call at 1318).

Three facts about the blob path matter:

- **The table is also used to compile the prelude itself.** `cargo xtask
  prelude-compile` compiles each peeled prelude binding with
  `prelude_globals: Some(..)` (`xtask/src/main.rs:388–436`). So the table
  shapes intra-prelude calls too, and demand analysis is **not** run on
  prelude bodies at blob-build time.
- **Small prelude functions are inlined into user code** in blob mode
  (`inlinable_bindings`). Calls to `map`, `foldl`, `foldr`, `compose`,
  `flip`, `when`, `map2` and `drop-while` therefore reach the compiler as analysed bodies,
  and their table entries never apply. Whether an entry matters depends on
  the inliner's decisions.
- **In source mode** (`--source-prelude`, or a checkout with no
  `lib/prelude.blob`) the prelude table is not built at all. Prelude
  functions get signatures from the analysis, like user code. Local
  `cargo test` without a blob therefore never exercises the table.

## 2. Measured behaviour

All programs were run with a release build from `b8bfb43` and a freshly
generated blob (`cargo xtask prelude-compile`). Each run was wrapped in
`timeout`, with `-e r -j`, in blob mode and with `--source-prelude`. Source
mode is the reference: it uses the analysis, which is designed to be sound.

### 2.1 Audit of the table against the definitions

Each entry was checked against its definition in `lib/prelude.eu`. An
argument is strict only if *every* path that forces the result to WHNF also
forces that argument. Source-mode analysis (`eu --source-prelude dump
demands`, named signature table) agreed with the audit wherever it reported
a signature. It never claimed a strictness the audit rejected.

Entries that claim strictness the definition does not have (S = strict
claimed, should be lazy):

| Name | Table | Correct | Why the claim is wrong |
|---|---|---|---|
| `map`, `filter`, `take-while`, `drop-while`, `sort-by-num`, `sort-by-str`, `mapcat`, `over-sliding-pairs`, `group-consecutive-by`, `discriminate`, `group-by` | function S | lazy | not called on `[]` |
| `map2`, `zip-with` | S S S | L S L | `nil?(l1) ∥ …` short-circuits |
| `foldl`, `foldr` | S S S | L L S | `op` unused on `[]`; `i` not forced when `op` ignores it |
| `take` | S S | S L | `(n <= 0) ∨ (l nil?)`: `take(0, ⊥) = []` |
| `append` | S S | S L | `foldr(cons, l2, l1)` never forces `l2` |
| `zip` | S S | S L | `zip-with(pair)` (see `map2`) |
| `snoc` | S S | L S | `l ++ [x]` |
| `interleave` | S S | S L | `b` only reached as a lazy tail |
| `cross` | S S S | L S L | |
| `sort-by` | S S S | L L S | |
| `compose`, `flip` | S S S | S L L | `f(g(x))`, `f(y, x)` |
| `when` | S S S | S L L | `if(x p?, x f, x)` |
| `deep-transform` | S S | S L | `rule` may ignore `data` |
| `head-or`, `tail-or` | S S | L S | `then(d, …)`: the default is a lazy branch |
| `iterate` | S S | L L | `cons(i, …)` (also being fixed by eu-oaz72.72) |
| `repeat`, `iota` | S | L | `cons(i, …)` |
| `deep-find`, `deep-query` | S S | not verified (left lazy in §4's audited table) | depend on `deep-fold` |
| `nth` | S S | S L | `nth(-1, ⊥)` should report the index error, not force `l` |

Length errors, which the #1158 round-4 review raised: `zip` is an arity-0
binding with a 2-entry signature. `mapcat`, `sort-by-num` and `sort-by-str`
are arity-1 bindings that are called with two arguments, so their one entry
describes a partial application.

The remaining entries were checked and are sound: `drop`, `reverse`,
`count`, `sum`, `product`, `cons` (lazy), `sort-nums`, `sort-strs`, the
numeric functions, `identity`, `apply`, `then`, `has`, the `lookup*`
functions, `render*`, `parse-as` and `range`.

### 2.2 Over-claims observed (blob vs source)

| Program (`r: …`) | Blob | Source |
|---|---|---|
| `head-or(panic("d"), [1])` | **panic** | `1` |
| `tail-or(panic("d"), [1, 2])` | **panic** | `[2]` |
| `append([1], panic("t")) head` | **panic** | `1` |
| `take(0, panic("l"))` | **panic** | `[]` |
| `filter(panic("p"), [])` | **panic** | `[]` |
| `zip([], panic("b"))` | **panic** | `[]` |
| `zip-with(+, [], panic("l2"))` | **panic** | `[]` |
| `snoc(panic("x"), [1]) head` | **panic** | `1` |
| `interleave([1], panic("b")) head` | **panic** | `1` |
| `take-while(panic("p"), [])` | **panic** | `[]` |
| `sort-by(panic("k"), <, [])` | **panic** | `[]` |
| `sort-by-num(panic("k"), [])` | **panic** | `[]` |
| `group-by(panic("k"), [])` | **panic** | `{}` |
| `discriminate(panic("p"), [])` | **panic** | `[[], []]` |
| `group-consecutive-by(panic("f"), [])` | **panic** | `[]` |
| `over-sliding-pairs(panic("f"), [])` | **panic** | `[]` |
| `mapcat(panic("f"), [])` | **panic** | `[]` |
| `cross(panic("f"), [], panic("ys"))` | **panic** | `[]` |
| `deep-transform(const(1), panic("d"))` | **panic** | `1` |
| `repeat(panic("v"))` then test `nil?` | **panic** | `1` |
| `iota(panic("n")) tail nil?` | **panic** | `false` |
| `head-or(panic("d"), [1]) block?` | **panic** | `false` |
| `nth(-1, panic("l"))` | panic **"l"** | "index -1 out of range" |

`map`, `map2`, `foldl`, `foldr`, `compose`, `flip`, `when` and `drop-while`
have equally wrong entries, but give the right answer in these programs
because they are inlined (§1). In a context where the inliner declines,
they will misbehave in the same way.

`panic` stands in for any argument that is expensive, erroneous or
divergent. `xs head-or(expensive-default)` is a common idiom. Blob mode
always evaluates the default, and a default that fails takes the whole
program down.

### 2.3 Name-keyed lookup: aliasing and shadowing

| # | Program | Blob | Source | Defect |
|---|---|---|---|---|
| L1 | `helpers: { deep-find-first(k, d, b): d + d }`<br>`used: helpers.deep-find-first(:a, 1, {})`<br>`r: deep-find-first(:a, panic("d"), {a: 1})` | **panic** | `r: 1` | A user helper's signature is applied to the *prelude* `deep-find-first` (a `Var::Free` global with the same name). |
| L2 | `helpers: { head-or(d, xs): 0 }`, `used: helpers.head-or(1, [])`, `r: head-or(panic("d"), [1])` | `r: 1` rendering the whole document; **panic** with `-e r` | `1` both ways | With the whole document, the lazy user helper masks the (wrong) prelude entry. `-e r` prunes the helper, the table entry comes back, and the result changes. The meaning of `r` depends on whether an unrelated binding survives pruning. |
| L3 | `head-or(d, xs): if(xs nil?, 0, head-or(d, xs tail))`, `r: head-or(panic("d"), [1])`, run with `--suppress-demand-analysis` | **panic** | `0` | With no user table, the *prelude* entry is applied to the user's own local `head-or` (a `Var::Bound` callee). This is a debug flag, but it shows the lookup ignores which binding the name refers to. |
| L4 | `hd: head-or`, `r: hd(panic("d"), [1])` | `1` | `1` | An alias is safe but loses all strictness: `hd` is a non-lambda binding and so "ambiguous". |
| L5 | A block key `take: 5` anywhere in the program | — | — | `take` becomes ambiguous program-wide, and every prelude `take` call loses its signature (precision only; from code reading). |

Source mode is sound in all five cases, because name-merged signatures are
lub-merged, which only weakens them. Its cost is precision (eu-oaz72.66).

## 3. Options

| | (0) Keep the table, audited | (a) Derive at blob build, store per slot | (b) Declare in prelude metadata | (c) Table plus CI check against source-mode analysis | (d) Key by identity (global slot / binder) |
|---|---|---|---|---|---|
| **Never stricter than the definition** | Only as good as the audit; it was wrong for 31 of 64 entries | Yes, if the analysis is sound; the same analysis already defines source-mode meaning | No: a hand claim again, just moved | Detects drift, but analysis precision gaps force an allowlist of hand-verified exceptions, which is the same trust problem | Orthogonal: it fixes *which* signature applies, not what it says |
| **Aliasing, shadowing** | L1–L5 remain unless the lookup is also fixed | With slot keying: a `Var::Free` resolves to exactly one global, so no L1/L3. Aliases (`zip-with: map2`) can inherit the target's signature | Same as the table | Same as the table | Removes L1–L5 and `ambiguous_names` |
| **Blob and source agree** | No (different sources) | Yes: one analysis | No | Partly | — |
| **Alternative preludes** | Hard-codes one prelude's names | Each blob carries its own signatures; source mode needs nothing | Each prelude declares its own | Hard-coded | — |
| **Drift** | Silent (how 31 entries went wrong) | None: signatures are regenerated with the blob. CI already regenerates the blob on every build | Next to the definition; still unchecked | Caught in CI | — |
| **Blob wire format** | None | New `#[serde(default)]` field, parallel to `binding_entries`, like `binding_spans` and `blame`. An old blob yields no signatures (sound). Sensitive area: two `rig-review`s | Metadata must survive into the blob as well | None | Slot keying needs only `name_to_slot`, which already exists |
| **User-visible surface** | None | None | A new metadata key is an API change, reserved for the owner | None | None |
| **Effort** | S | M | M plus an owner decision | S–M | M (this is eu-oaz72.66's scope) |

The alternatives were rejected for these reasons:

- **(b)** keeps the core problem, a hand-written claim that nothing checks,
  and adds language surface.
- **(c)** is worth having only if the table stays. Once signatures are
  derived, there is nothing left to check.
- **(0)** is the right *interim* fix, not a design.

One more variant was considered: deriving at *run time* from the
`desugared_unit_cores` the blob already carries. It adds a whole-prelude
analysis to every run, so it is not preferred. Storing the result at build
time costs one serde field.

**Freshness caveat for (a).** The blob's source hash covers the prelude
source, the intrinsic catalogue and the bytecode wire version, but not the
analysis code. Signatures from an older analysis would persist in a stale
local blob until it is regenerated. Release and CI builds regenerate the
blob, so only local development is exposed. Adding an
analysis-version constant to the hash closes the gap.

## 4. Performance

Ticks and max stack are deterministic, so one run of each is enough. Wall
times are not quoted. The figures are measured-single, on the 22 benches in
`tests/harness/bench/`, all in blob mode. Each variant was built as a local,
uncommitted change, with the blob regenerated for each.

| Bench | Master: ticks / max stack | Audited table | No prelude table |
|---|---|---|---|
| 019 list_scale (`range map sum`, n=6000) | 1,079,258 / **16** | 1,031,233 / **12,009** | 995,230 / 12,009 |
| 015 block_merge | 2,027,082 / 3,032 | 1,983,912 / 6,017 | 1,950,897 / 6,017 |
| 016 yaml | 5,547,807 / 25 | 5,505,968 / 2,175 | 5,401,883 / 2,175 |
| 017 toml | 5,491,964 / 25 | 5,450,125 / 2,175 | 5,346,040 / 2,175 |
| 022 hof_fold | 51,700,735 / 800,006 | 49,700,710 / 800,006 | 48,500,710 / 800,006 |
| 005 drop_cons | 2,020,356 / 9 | 1,940,350 / 11 | 1,880,350 / 12 |
| other 16 | — | ticks −5% to +0.1%; stack +0 to +29, except 020 lookup_curve 47 → 304 | ticks −0 to −6%; same stacks as audited, except 003 (138 → 188) |

`range(0, 1000000) map(_ + 1) sum`:

| Variant | Ticks | Max stack |
|---|---|---|
| Master, blob | 179.8M | 13 |
| Audited table, blob | 171.8M | 2,000,006 |
| Master, source mode | 145.8M | 2,000,007 |

What the numbers show:

- **Correct strictness costs ticks today.** A strict argument is compiled as
  thunk-then-`seq` (eu-oaz72.67), and the sound entries cost 0–4% in ticks
  against no table at all, for the same max stack on 21 of the 22 benches.
  Once eu-oaz72.67 compiles strict arguments in place, the sign should
  flip. Derived signatures inherit that benefit for free.
- **The stack wins came from over-claims, most likely `take`'s.** On the
  019 path (`range`, `take`, `map`, `sum`), the only data argument whose
  claim changed is `take`'s list. `take(n, l)` forces `l` only when `n > 0`,
  so claiming it strict seq-forces each `l tail` inside `range`, which keeps
  the pipeline at constant stack. (`foldl`'s accumulator is *not* forced in
  blob mode at master, despite its entry: `foldl(op, 0, range(0, 10))` with
  an `op` that panics on one intermediate accumulator returns 9 in both
  modes.) Source mode never had the constant stack. Recovering it soundly needs cheap-and-total speculation,
  not strictness: in the branch where `l` has been tested non-nil, `l tail`
  is total, cheap and safe to evaluate eagerly. This is the most important
  item in the follow-up list for AoC-style pipelines.
- **Lost `DirectApp` dispatch** for prelude globals with no signature is
  already included in the "no table" ticks, and is not a regression. Under
  (a)/(d), the arity for `DirectApp` should come from the global's form
  (`global_forms` holds it), independent of strictness.

## 5. Recommendation and migration path

1. **Bug fix, now (P1).** Correct the 31 entries (and `nth`): the audited table in §2.1,
   with `deep-find` and `deep-query` left lazy unless verified. Fix the
   lookup scoping in `compiler.rs`:
   - consult the user table only for `Var::Bound` callees;
   - consult the prelude table only for a `Var::Free` that
     `resolve_free_var` maps to a prelude global.

   This fixes L1–L3. Turn the §2.2 and §2.3 programs into harness tests, run
   in both prelude modes and fault-injection verified. Regenerate the blob.
   Normal review: this is a bug fix with no semantic intent. The 019
   stack change must be stated in the PR, because it brings blob mode into
   line with source mode. The eu-oaz72.72 lane is already editing these
   entries (repeat, iterate), so this step is best folded into it or queued
   after it.
2. **Derive (P2, sensitive: blob format).** At `prelude-compile`, run
   `analyse_demands` over the whole prelude core before peeling (the same
   input source mode analyses). Record each top-level binding's lambda
   signature by binder, and store it as a `demand_signatures` field parallel
   to `binding_entries`. Compile the blob's own bodies with these signatures
   too, so intra-prelude calls match source mode. At run time, look up by
   global slot. Delete `build_prelude_signatures`. Add the analysis version
   to the freshness hash. Optionally, propagate signatures through aliases
   (`zip-with: map2`) and through partial applications (`zip:
   zip-with(pair)`). Needs two `rig-review`s, and ticks under the protocol
   on the eight canonical benches.
3. **Identity keying for user code (P2, eu-oaz72.66).** Carry each let-bound
   lambda's signature to its call sites by binder. Remove
   `NamedSignatureTable` and `ambiguous_names`. This closes L4/L5's
   precision loss and the strict/lazy same-name case.
4. **Precision (P3).** Close the analysis gaps that the old table papered
   over soundly: `take-while`, `sort-by`, `sort-strs`, `group-by`,
   `discriminate` and `cross` are strict in their list; `apply` is strict
   in `f`; `range` is strict in both. Most of these go through a local
   helper `{ aux(…) }.aux(…)` or `qsort`.
5. **Speculation for cheap, total expressions (P2 for AoC).** Recover
   constant stack for `range`/`take` pipelines soundly, as described in §4.

## 6. Proposed follow-up beads

| Title | Priority | Scope |
|---|---|---|
| (BUG) Prelude demand table over-claims strictness for 31 names; user signatures leak onto same-named prelude globals | P1 | Step 1 above: audited table, scoped lookup, harness tests in both modes. Could be folded into eu-oaz72.72. |
| Derive prelude demand signatures at blob build and key them by global slot | P2 | Step 2; blob format, two reviews; replaces `build_prelude_signatures`. |
| Speculate cheap total expressions (`tail`/`head` of a scrutinised cons) to keep `range`/`take` pipelines at constant stack | P2 | Step 5; recovers 019's stack soundly; measure under the protocol. |
| Demand analysis precision: local-helper and `qsort` patterns (`take-while`, `sort-by`, `group-by`, …) | P3 | Step 4. |
| `DirectApp` eligibility from global arity, not from signature presence | P3 | Decouple dispatch from strictness. |

eu-oaz72.66 already covers step 3, and eu-oaz72.67 makes strictness pay for
itself in ticks.

## Appendix: reproducing

To reproduce, generate the blob, build, and run each program in both modes:

```sh
cargo xtask prelude-compile
cargo build --release
timeout 30 eu f.eu -e r -j
timeout 30 eu --source-prelude f.eu -e r -j
```

For L1 and L2, render the whole document (no `-e`).

To get the source-mode derived signatures, list the prelude names as values
in a file (so pruning keeps them) and run:

```sh
eu --source-prelude dump demands that-file.eu
```
