# Flat closures: design report (eu-oaz72.84)

Status: design only, revised 2026-11-16 after the owner's note that flat
closures were tried before and found too complex or not a win. No code
changes. **Recommendation: do not build flat closures for this bead.** The
narrower change worth doing is in section 6.

Problem source: dict design note §12 (`docs/superpowers/plans/2026-11-12-dict-design.md`),
"Remaining limit"; PR #1168 (eu-oaz72.70) detached evaluated constructors only.

## 1. How thunk environments are built today

- **Syntax.** `StgSyn::Let`/`LetRec` carry `LambdaForm`s (`Lambda`, `Thunk`,
  `Value`). Variables are `Ref::L(n)`: a flat index along the environment
  chain, produced by bumping de Bruijn indices by frame size at each binder
  (`src/eval/stg/compiler.rs`).
- **Frames.** `BcEnvFrame = EnvironmentFrame<BcValue>`: slot array,
  annotation, `next` (a cactus stack); `get(n)` walks `next`.
- **Closures.** `BcClosure { info(arity, update, smid, code), env }`.
- **Let / LetRec** (`machine.rs`, `Op::Let`, `Op::LetRec`): every binding
  closes over the **whole current chain** (`BcClosure::close(&header, env)`).
  Form headers are `kind, arity, smid, body`; no capture list exists.
- **GC.** `BcClosure::scan` marks its one `env` pointer, so the whole chain
  stays live.
- **Update.** `BcContinuation::Update { environment, index }` writes the
  result **into the frame slot** naming the thunk: a thunk's identity is its
  slot.
- **Blob.** Wire format `BYTECODE_WIRE_FORMAT_VERSION = 10`; form headers are
  part of it.

Any lazily stored closure (block literal, `[i + 1, i]`, value with metadata,
section) therefore retains every frame it was built in. In a dict-threading
loop the caller's frame holds the previous dict. `memo_cell` works around this
for *evaluated* constructors only.

## 2. Earlier attempts

Searched: git history and messages (`flat clos`, closure conversion, free
variable, env trim, capture list, space leak), remote branches, beads
including closed ones (`bd search`), `docs/superpowers/{reports,plans,specs}`,
`ROADMAP.md`, `CHANGELOG.md` and the engine A/B ledger
(`results.jsonl`, which has no flat-closure rows: the ledger post-dates the
attempt).

| # | What | What was measured | Why dropped | Where |
|---|---|---|---|---|
| 1 | **eu-fwlt** (Feb 2026): benchmark O(depth) env traversal before building anything | Max env depth 2–4, average steps per lookup under 2.0 on day01, day12, deep_find; SCC splitting no effect | "Flat closures not warranted": the cactus stack is effectively O(1) | bead eu-fwlt (closed) |
| 2 | **eu-9tah.18** (June 2026, 0.10.0): *built* flat closures: capture frames of `(frame_ptr, physical_index)` pairs, `Ref::Capture`, per-form capture recipes from a free-variable analysis, feature-flagged | Free variables per closure average 1.0, max 5, 28–50% capture nothing. `EnvironmentFrame::get` self-time fell 27%, but **net wall time regressed 2–10% on every program** | No win: avg depth 2 means the flat path costs about the same as the cactus path, while every frame creation pays a capture-recipe tax and the wider `Ref` dispatch hurt branch prediction. Reverted | bead eu-9tah.18 (closed), spec `docs/superpowers/specs/2026-06-21-flat-closures.md` and branch `archive/flat-closures` (only on the remote), ROADMAP §10.2 |
| 3 | **Runtime depth measurement** (eu-qm7f, 2026-07-13) | Mean lookup depth 0.656, max 2 (blob) or 3 (source) on benches 018 and 019; measured-verified | Refuted the "deep chains" claim again: locality, not depth | `docs/superpowers/reports/2026-07-13-env-walk-depth.md` |
| 4 | **SCC splitting / deeper Let nesting** (eu-9tah, 2026): the opposite experiment, more scopes | Env-chain traversal regressions of 4–12× on AoC days, timeouts elsewhere | Disabled (commit `1c70db0e`); shows chain depth *can* matter when nesting grows, which is why a shallow chain is worth protecting | commit `1c70db0e` |
| 5 | **BV3 register frames** (eu-2sa6.2, open P3): the successor idea, "selective, compiler-decided" | Reassessed 2026-08-11: three motivating cases were each fixed more cheaply (copy-specialisation #1010, eager args #1068, stream fix); only a narrow value case remains | Not built; ROADMAP §6.4 item 5 and the predecoded-IR design (§8 risk 1) cite attempt 2 as the cautionary tale | `docs/superpowers/reports/2026-08-11-assessment-bv3-register-frames.md`, beads eu-2sa6.2 / eu-98zg / eu-gmdl5 |

Common thread: every attempt targeted **speed** (env-walk cost). Each found
chains shallow and a universal capture representation costing more than it
saved. None targeted **memory retention**, which is what this bead is about.
No earlier record mentions thunk sharing, because attempt 2 kept captures as
`(frame_ptr, index)` pairs, so updates still landed in the source slot.

## 3. Does this proposal change that verdict?

It differs in goal and in design, and neither difference helps.

- **Goal.** Retention, not speed. The earlier design could not have met it:
  `(frame_ptr, index)` captures keep the source frame alive, exactly the
  property this bead wants to remove. So attempt 2's result is not a verdict
  on retention, but it also offers no design that works.
- **Design.** Retention needs captures *by value* (a closure holds the values,
  not the frame). By-value capture of an updatable thunk loses sharing
  (updates write into the original slot), so it also needs thunks with heap
  identity (indirection cells): one allocation per thunk, which costs nothing
  today beyond its slot. That is a heavier per-closure cost than attempt 2's
  recipe tax, applied universally, and attempt 2 lost 2–10% to the lighter
  one. Section 4 lists the work (compiler, wire format v11, `Update`, GC
  layout), and the risk now has direct evidence behind it.
- **Net.** The verdict is unchanged in direction: a universal change of this
  kind is expected to regress benches 015–022 and is not justified by a
  dict-loop retention case. No measurement here changes that; no new figure is
  claimed (all numbers above are cited, with their original labels).

## 4. What the change would involve (if the owner wants it anyway)

Compiler free-variable analysis and re-indexing; capture lists in form headers
(blob wire format v10 to v11, encoder, predecoder, differential tooling);
`Op::Let`/`Op::LetRec` building capture frames; thunk cells and a new
`Update`; new GC object kind; auditing hand-built closures (`apply1_thunk`,
`memo_cell`, `saturate*`, fixed templates). Sensitive areas under
`REVIEW.md`: GC layout, blob wire format, engine representation. Several PRs,
each needing two reviews, with a throwaway prototype first to measure cell
cost on benches 015–022 under `docs/superpowers/engine-ab/PROTOCOL.md`.

## 5. Why not the cheaper trick inside the dict

Scrubbing a copied chain at store time, unconditionally, has the same sharing
problem (a thunk slot's own `env` is the original frame). The variant in
section 6 avoids it by refusing to copy anything that is not already a value.

## 6. Recommendation and the narrower change worth doing

1. **Do not implement flat closures for eu-oaz72.84.** Close or re-scope the
   bead; record the attempts above on it.
2. **Document the existing workaround** (dict design note §12, and the dict
   guide): a loop that threads a dict should store evaluated values
   (`update-strict`, or force before `assoc`), which §12 already says
   detaches them. No engine change.
3. **Candidate targeted fix, worth a prototype if the owner wants the
   limit lifted:** at `memo_cell` time only, for an unevaluated stored
   closure, compute the closure's free variables (derivable at program decode
   from the bytecode: no wire change, no compiler change) and, **only if every
   free variable is already evaluated** (a value, so there is no thunk
   identity to lose), rebuild the closure over a copy of the chain in which
   unused slots are tombstoned and the chain is truncated past the deepest
   free index. If any free variable is an unevaluated thunk, store as today.
   Properties: no semantic change (a copied value is a value), no thunk
   sharing lost, selective rather than universal (the lesson of attempt 2),
   cost paid only at dict stores. It covers the stated cases (`{a: i}`,
   `[i + 1, i]` where `i` is evaluated) but not a stored closure over an
   unevaluated variable, which genuinely needs the thunk to stay shared.
   Open questions for the prototype: the free-variable scan must follow
   binders (let, letrec, case fields, lambda arguments, demeta); chain-copy
   cost on deep chains; interaction with `memo_cell`'s 64-constructor cap.
   It touches `src/eval` allocation, so it is a second-review area, but it is
   a retention fix with identical observable behaviour, not a new construct.

## 7. Decisions needed from the owner

- Agree to drop flat closures for this bead (recommended)?
- Want the section 6.3 prototype, as a separate bead with the acceptance gate
  (bounded memory under a small `--heap-limit-mib`, benches 015–022 not
  regressed) and a retry of the eu-oaz72.21 fault injection?
- Otherwise accept the documented limit plus the `update-strict` workaround.
