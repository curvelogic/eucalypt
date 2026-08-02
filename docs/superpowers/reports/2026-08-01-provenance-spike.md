# Feasibility spike: laziness-surviving provenance (eu-1tkk.7.13)

**Date**: 2026-08-01
**Bead**: eu-1tkk.7.13 (P3 spike), blocking eu-1tkk.7.14
**Spec**: `docs/superpowers/specs/2026-07-21-diagnostics-overhaul-design.md` §4.4
**Verdict**: (a) full demand/cause chain — **escalate; recommend 0.15**. (b) sub-binding
value provenance — **in scope for 0.14, but as a separate, smaller, non-spike-gated
bead**, not as part of eu-1tkk.7.13/.14.

## 1. The specimen, reproduced

The bead's four-file import chain (`main.eu → lib/report.eu → lib/geometry.eu →
lib/units.eu`, one bad datum: `height: "8"` as the second of three list elements)
was rebuilt from the bead's own listing and run against master (`8f5322f7`) built
with `cargo build --release`. Source (line numbers shifted by one throughout versus
the bead's transcript because each file needed an explicit `{ import: ... }` line
that the bead's illustrative snippet omitted; the shape is otherwise identical):

```eu,notest
# lib/units.eu
factors: { mm: 1, cm: 10, m: 1000 }
to-mm(unit, n): n * (factors lookup(unit))
```

```eu,notest
# main.eu
{ import: "lib/report.eu" }
shapes: [
  { width: 10, height: 20, units: :mm },
  { width: 5,  height: "8", units: :cm },
  { width: 2,  height: 3,  units: :m },
]
main: shapes summarise
```

Default output:

```text
error[EU-EVAL-TYPE]: type mismatch: expected number, found string "8"
  ┌─ geometry.eu:3:48
3 │ side-mm(key, shape): (shape lookup(key)) to-mm(shape.units)
  │                                                ^^^^^
  ┌─ units.eu:3:1
3 │ to-mm(unit, n): n * (factors lookup(unit))
  = stack trace:
    - main at main.eu:8:14
    - summarise at report.eu:3:1
    - area at geometry.eu:4:38
```

`--debug-trace` (uncurated) adds only `foldl`/`+`/`foldl` prelude frames and drops
them again in the curated view above. **No frame, primary or secondary, carries
`main.eu:5`** — the reproduction confirms the bead's claim exactly: the defect is
that the offending value's provenance was never captured, not that curation is
hiding it.

## 2. Verdict on (a): does the full demand/cause chain fit 0.14?

**No — recommend re-filing to 0.15. This needs owner sign-off**, because the
2026-08-01 ruling put all ten diagnostics beads in 0.14 unconditionally; §4.4's own
graceful-degradation clause ("drops to a 0.15 item... Phase 2 ships alone") may no
longer be authorised without that sign-off, even though it was pre-agreed in the
design doc.

**Reasoning.** "Surviving annotation" (Well-Typed's approach) needs, at minimum, a
*chain* — "report needed servers needed configs" — not a single location. A single
`Smid` cannot represent a multi-hop chain; GHC's own mechanism (and any analogue
here) allocates a linked frame structure captured at thunk-*creation* time and
walked at force time. Concretely, in this codebase that means a new pointer field
on the closure/thunk representation:

- `Closing<S>` is `(InfoTagged<S>, RefPtr<EnvironmentFrame<Closing<S>>>)`
  (`src/eval/machine/env.rs:285`) — 16 bytes, no slack.
- `InfoTagged<L>` packs `{ info: InfoFlags(u64), body: L }`
  (`src/eval/memory/infotable.rs:42-49`) — again no slack; `InfoFlags` is a single
  u64 already fully committed (32 bits Smid, 8 bits arity, 1 bit update flag,
  `infotable.rs:12-38`).
- A cause-chain link is not a scalar Smid, it is "a reference to the enclosing
  demand context" — i.e. a new heap pointer. Unlike a plain `Smid` (see §4 below),
  a pointer field **is** a GC-scanning change: every place `HeapSyn`/`Closing`
  fields are traced (`src/eval/memory/syntax.rs` scan match arms, `mark.rs`,
  `collect.rs`, `env.rs` `GcScannable` impls for `SynClosure`) would need a new
  arm, and the field must survive evacuation/forwarding correctly — exactly the
  category of change that caused the 220–580% regressions during the persistent-
  blocks GC work (`docs/development/architectural-decisions.md`, ADR-001).
- The design spec calls this itself "the technically hardest part — GHC has
  iterated on it for two decades" (§4.4) and gates it behind this very spike.

**Rough cost**: new GC-scanned field on the hot closure/thunk path (touches
`memory/env.rs`, `memory/syntax.rs`, `memory/mark.rs`, `memory/collect.rs`,
`stg/compiler.rs` thunk construction, plus `error.rs`/trace rendering to consume
the chain) with mandatory engine-ab verification before it could be accepted
(per CLAUDE.md's engine-performance protocol) and a "recorded review" gate (GC/
memory PRs require non-author review). This is multi-week, delicately-scoped work
that legitimately risks the kind of regression this project has been burned by
before — not a 0.14-cycle-sized item on the evidence available, independent of
the sibling beads' status.

## 3. Verdict on (b): is sub-binding value provenance in scope, and does the
bead's own cost analysis hold up?

**The bead's analysis is right that per-*named-binding* demand chain would not
resolve the specimen** (it would land on `shapes` at the list's own location, not
the individual row) — confirmed independently below. **But its cost claim does
not hold up**: it states sub-binding value provenance "is a materially larger
cost... a Smid carried on values... rather than on named-binding thunks", implying
it is roughly as expensive as, or more expensive than, the demand chain. Reading
the actual compiler and VM code shows the opposite: **a `Smid` slot per
thunk/closure already exists, is already zero-cost, and is already the mechanism
that makes the *existing* "summarise at report.eu:3:1" frame possible** — the gap
is a narrow compiler omission, not a new architectural cost.

### 3.1 What already exists (verified by reading code, not assumed)

- `InfoFlags` (`infotable.rs:12-38`) already reserves 32 bits for a `Smid` on
  **every** closure — thunk or value — with zero additional heap layout cost;
  `InfoTagged::thunk()`/`::value()` (`infotable.rs:96-110`) simply stamp
  `Smid::default()` into that pre-existing slot today.
- `VM::handle_instruction` (`vm.rs:474-482`) already reads
  `self.closure.annotation()` on **every** closure entry and, if valid, sets
  `vm.annotation` — which `env_trace`/`stack_trace`/`to_diagnostic` already
  consult. This is exactly the channel that produces `summarise at report.eu:3:1`
  today for named lambda bindings (`compile_lambda`'s explicit annotation param).
- That annotation survives argument-passing indirection: `create_arg_array`
  (`src/eval/machine/env_builder.rs:347-377`) either passes a "settled" closure
  straight through unchanged (eu-wpswc optimisation, preserving its InfoTable
  intact) or wraps it in an alias `Atom{Ref::L(i)}` that, on the next tick,
  chases straight back to the *original* closure and its original annotation
  (`vm.rs:489-518`). Aliasing does not erase it.
- `ExecutionError::to_diagnostic` (`error.rs:1206-1266`) already renders **up to
  three secondary labels**, not just one — the rendering substrate has headroom
  for an additional "value originated here" label without restructuring.

### 3.2 What is actually missing

Verified against `eu dump stg --debug-format` on the reproduced specimen: each
list element (`{ width: ..., height: ..., units: ... }`) compiles to its own
`thunk letrec [...] in Block(...)` — i.e. each row **is already its own separate
thunk** with its own `InfoTable` slot — but that slot shows no `@[smid]` marker at
all, i.e. `Smid::default()`. Tracing why: `compile_list_binding` /
`compile_list_body` / `compile_block` (`src/eval/stg/compiler.rs:2014-2083`) do
carry a `smid: Smid` parameter, but for `Expr::Block`/`Expr::List` sub-items this
parameter is a dead argument — `compile_binding`'s `Expr::Block`/`Expr::List` arms
(`compiler.rs:1852-1856`) re-derive their own smid from the item's own AST node
(`*s`) rather than consuming the passed-in one, and `Expr::Literal` discards its
smid entirely (`compiler.rs:1851`). The real gap is one level further down: the
`ProtoLet`/block-binding construction that eventually calls
`InfoTagged::thunk(body)` (via `binder.add_deferred`) never threads *any* smid
into the resulting `InfoTable`, regardless of what the compiler passes around —
only `compile_lambda` (named bindings) and `ProtoAppGroup` (call sites, via a
separate `HeapSyn::Ann` node, not the `InfoTable`) currently populate it.

So: the specimen's row (`main.eu:5`) is already individually thunked at compile
time, and the smid to stamp on it (`Expr::Block`'s own `*s`) is already present
in the AST — it simply is not written into that thunk's `InfoTable` today. Fixing
this is a **compiler-only change**: thread the block/list item's own smid into
its `LambdaForm`'s `InfoTable` when the binding is constructed, and capture that
smid as a new labelled span in `ExecutionError` at the point a scrutinised value
fails a type check (rather than letting it silently overwrite `last_annotation`
and disturb the existing, correct, call-site primary/secondary labels — this
last part needs new, small plumbing in `error.rs`, not new heap layout).

### 3.3 Refined verdict on (b)

- **Row/element granularity** (which of the three list entries) is achievable via
  the mechanism above with no GC or heap-layout change, no new pointer field, and
  no interaction with laziness at all. It would resolve the specimen fully: the
  user would be told "data literal at main.eu:5" alongside the existing library
  labels. Rough cost: a contained compiler change (a handful of call sites in
  `compiler.rs`, one small addition to `LetBinder`/block-binding construction)
  plus a small addition to `ExecutionError` to carry and render one extra label
  without disturbing the existing primary/secondary — plausibly a few days for
  Clarion/Quill jointly, with the usual snapshot-corpus fallout to review.
- **Field/column granularity** (distinguishing `height` from `width`/`units`
  within the same row) is a materially bigger version of the same idea — literal
  scalar values (`Num`, `Str`, `Sym`) are currently compiled as bare inline
  values inside the block's own letrec, sharing the block's single `InfoTable`,
  not individually thunked. Giving each field its own annotation would mean
  threading `Expr::Literal`'s own smid through `compile_boxed_literal` (which
  today takes no smid at all) and using it in `InfoTagged::value()` (today
  hard-coded to `Smid::default()`, `infotable.rs:105-109`) — touching every
  eagerly-evaluated literal, not just list/block members. Still zero-cost in heap
  layout (same reused slot), but a wider-reaching compiler change with more
  surface for regressions. Not needed to resolve the motivating specimen; a
  reasonable stretch goal for a follow-up, not a blocker.
- The demand chain itself, even if built, would add nothing here: the curated
  trace already shows `main at main.eu:9:14` and the missing information is
  *where the value came from*, not *why it was forced* — this matches the bead's
  own observation and nothing found here overturns it.

**So: (b) is in scope for 0.14, cheaply, but is a different, smaller, and
independently-implementable feature from what design §4.4 and eu-1tkk.7.14
describe.** It should not wait on the demand-chain spike outcome.

## 4. The value-provenance lead, assessed directly

The bead's own lead — "a literal's Smid exists in the source map at desugar time;
the difficulty is that runtime values do not carry one" and "may not need the
laziness machinery at all" — **is correct, and considerably more so than the
bead itself suspected**. It is not merely "separable" from the demand chain; the
storage it would use (`InfoFlags`'s packed Smid) is not a new cost at all —
it is existing, paid-for infrastructure that is *already in production use* for
named-binding call-site annotations, just not yet wired up for anonymous
container elements. Measured facts, not estimates:

| Structure | Measured size | Notes |
|---|---|---|
| `Native` (`src/eval/memory/syntax.rs:37-58`) | 24 bytes | inline scalars (`Num`, `Sym`, `Zdt`) embedded directly, no separate heap allocation |
| `Ref` = `Reference<Native>` | 24 bytes | discriminant absorbed into `Native`'s own padding — wrapping costs nothing |
| `Smid` (`src/common/sourcemap.rs:26`) | 4 bytes | `Option<NonZeroU32>`, niche-optimised |
| `AllocHeader` (`src/eval/memory/header.rs:67-75`) | 16 bytes, **fully packed, no slack** | shared by every GC-managed heap object; NOT where the Smid lives or would need to live |
| `InfoFlags`/`Closing<S>` closure info | 16 bytes total (8 info + 8 env ptr), **already carries a Smid** | this is the correct, already-used location |

The important distinction the spike had to get right: a Smid-on-values scheme
that tried to add a field to the shared `AllocHeader` (every heap object) or to
`Native` itself (every inline scalar, copied constantly through environments and
the argument stack) would indeed be expensive — `AllocHeader` has zero slack, so
any addition doubles it to 32 bytes for *every* heap object, and `Native`/`Ref`
at 24 bytes are on the hottest path in the interpreter. **That is not the scheme
this spike recommends.** The scheme that actually fits the architecture attaches
the smid to the *closure* (the `InfoTable`, already reserving the bits, already
per-binding-site rather than per-instance) — which is precisely what the
existing named-binding call-site annotation already does, successfully, in
production, today.

## 5. Recommendation

1. **Escalate (a) to the owner.** The evidence supports re-filing the full
   demand/cause chain to 0.15: it needs a new GC-scanned pointer field on the
   closure/thunk hot path, the design document itself calls it the hardest part
   of the whole effort, and it is exactly the shape of change (GC/memory,
   engine-ab-gated, recorded-review-gated) that has caused serious regressions
   before. This conflicts with the 2026-08-01 "all ten ship in 0.14
   unconditionally" ruling and needs the owner's explicit call, not mine —
   raised here per the constraint on this spike, not decided.
2. **(b) does not need escalation as a blocking risk**, but does need a scope
   decision: split sub-binding *row-level* value provenance out of
   eu-1tkk.7.13/.14 into its own, smaller, non-GC-touching bead targeting 0.14,
   since it is cheap, high-value (it is what actually fixes the motivating
   specimen), and demonstrably independent of the demand-chain spike outcome.
   Recommend filing it against Clarion/Quill jointly (compiler-side smid
   threading + `ExecutionError` label plumbing).
3. If the owner insists the *full* Phase 4.4 (demand chain over named bindings,
   as specced) ships in 0.14 regardless, that is buildable — it is well-scoped
   and low-risk (reuses existing per-closure annotation, no new pointer field) —
   but it should be understood going in that it will **not** resolve the
   motivating specimen on its own; (b) above is what resolves the specimen.

No Rust code was changed by this spike (a size-probing test used for the
measurements in §4 was written, run, and reverted; not included in this PR).
