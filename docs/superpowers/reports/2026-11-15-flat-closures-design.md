# Flat closures: design report (eu-oaz72.84)

Status: design only. No code changes. Needs an owner decision before any
implementation (engine representation, blob wire format; `REVIEW.md`
sensitive areas).

Source for the problem: dict design note §12 (`docs/superpowers/plans/2026-11-12-dict-design.md`),
"Remaining limit"; PR #1168 (eu-oaz72.70) detached evaluated constructors only.

## 1. How thunk environments are built today

- **Syntax.** `StgSyn::Let`/`LetRec` carry `LambdaForm`s (`Lambda`, `Thunk`,
  `Value`). Variables are `Ref::L(n)`: a *flat index along the environment
  chain*, not a depth/slot pair. The compiler turns core de Bruijn variables
  into these by bumping indices by the frame size at each binder
  (`src/eval/stg/compiler.rs`, `Ref::bump` call sites near line 510).
- **Frames.** `BcEnvFrame = EnvironmentFrame<BcValue>`: an array of slots, an
  annotation, and `next`, the enclosing frame (a cactus stack).
  `get(n)` walks `next` subtracting frame lengths.
- **Closures.** `BcClosure { info(arity, update, smid, code), env }`. A slot is
  `BcValue::Closure` or `BcValue::Native`.
- **Let / LetRec** (`machine.rs`, `Op::Let` ≈ l.2361, `Op::LetRec` ≈ l.2382).
  Every binding closure is `BcClosure::close(&form_header, env)`: it closes
  over the **entire current environment chain** (for `LetRec`, over the new
  frame, whose `next` is the current env). The encoded form header is
  `kind, arity, smid, body` only; there is no capture list.
- **GC.** `BcClosure::scan` marks its one `env` pointer; the whole chain then
  stays live (`closure.rs`). Code offsets are inert.
- **Update.** A thunk is entered with `BcContinuation::Update { environment,
  index }`; the result is written **into the frame slot** that named the
  thunk. A thunk therefore has identity *as a slot*, not as a heap object.
- **Blob.** `lib/prelude.blob` embeds the encoded bytecode; wire format
  `BYTECODE_WIRE_FORMAT_VERSION = 10`. Form headers are part of it.

Consequence: any lazily stored closure (block literal, `[i + 1, i]`, a value
with metadata, a partial application) retains every frame it was created in.
In a dict-threading loop the caller's frame holds the previous dict, so each
version stays alive. `memo_cell` (PR #1168) works around this by rebuilding
*evaluated* constructors over fresh frames; unevaluated closures cannot be
rebuilt because their free variables are not recorded anywhere.

## 2. What free-variable-only capture would change

A flat closure is `{ code, captured: [BcValue; k] }` with the body compiled
against `[args…, captured…]` instead of the chain.

1. **Compiler.** A free-variable analysis per `LambdaForm` (set of outer
   `Ref::L` indices used by the body, transitively through nested forms), and
   a re-indexing of the body's local refs onto a capture frame. `Ref::G`/`Ref::V`
   are unaffected. Each `Let`/`LetRec` binding gains a capture list of outer
   indices.
2. **Encoding / wire format.** The form header gains a capture list
   (`u32` count plus indices, or an offset into a side table); body refs change
   meaning. This is a **blob wire-format change** (v10 → v11), invalidating
   every cached blob, plus `predecode.rs` and the differential tooling.
3. **Machine.** `Op::Let`/`Op::LetRec` allocate a capture frame per binding (or
   per group) instead of closing over `env`; `Update` and `OP_ATOM` alias
   handling must be revisited; `LetRec` needs sibling slots inside the capture
   frame (cyclic).
4. **GC.** If captures are ordinary frames, tracing is unchanged. If thunks
   become heap cells (see risk 1), a new traced object kind and layout is
   required: `src/eval/memory/` and `GcScannable` (second-review area).
5. **Intrinsics.** Anything that builds closures by hand (`apply1_thunk`,
   `memo_cell`, `saturate*`, the fixed apply/producer/cell templates) assumes
   the chain model and needs auditing.

## 3. Risks

1. **Thunk sharing (the central obstacle).** Updates write into a frame slot.
   Copying a thunk-valued slot into a capture frame by value gives the copy
   its own slot: evaluating through the copy neither updates the original nor
   is updated by it, so work is duplicated and sharing is lost (potentially
   exponentially for nested lets). Correct flat closures need thunks to have
   stable heap identity (an indirection cell, as in GHC), so captures copy a
   pointer to the cell. That adds one allocation per thunk, which today costs
   no allocation beyond its slot, and changes what `Update` does.
2. **Performance.** Capture copying plus cell allocation versus shorter
   environment walks (`EU_ENV_DEPTH_HISTOGRAM` exists to quantify the latter).
   Unknown without a prototype; benches 015–022 are the gate and must be run
   under `docs/superpowers/engine-ab/PROTOCOL.md`. No figure is claimed here.
3. **Semantics.** Evaluation order, error paths and source locations must stay
   identical (annotations live on frames; `Ann`/`target_annotation` look
   through `Let`).
4. **Blob compatibility, dual prelude modes, and `EU_GC_VERIFY=2` /
   `EU_GC_STRESS=1`.** All must pass; the errors corpus must be unchanged in
   source and blob modes.
5. **Why not a cheaper trick.** Scrubbing unused slots in a copied chain at
   `memo_cell` time keeps indices valid and needs no wire change, but it has the
   same sharing problem transitively (a thunk slot's own `env` is the original
   frame), and it is a mechanism for one caller rather than a general one
   (`REVIEW.md` Architecture). Not recommended.

## 4. Minimal sound first step

Even the smallest sound version (flat capture for `Let` thunks and cell-boxed
thunks) touches the compiler, the encoding and wire format, the machine's
`Let`/`LetRec`/`Update`, and GC object layout, and cannot be validated against
the acceptance criteria (bounded memory under `--heap-limit-mib`, benches
015–022 not regressed) until the whole path exists. It does not fit one PR and
it changes the blob wire format and an engine representation, which need an
owner decision (and two reviews per sub-PR).

Proposed phasing, each step behind its own PR:

0. **Prototype measurement** (throwaway branch, not merged): thunk-cell
   allocation cost on benches 015–022, and env-depth histogram, to decide
   whether to proceed at all.
1. **Free-variable analysis** in the compiler, emitting a capture list per form
   but *not yet used* by the engine (no behaviour change; unit-tested;
   wire format v11 if the list is encoded).
2. **Thunk cells**: heap-identity thunks and the new `Update`, still over the
   chain model (behaviour-preserving; bench gate).
3. **Flat capture** for `Let`/`LetRec` thunks and then lambdas, using 1 and 2.
4. **Re-try** the fault injection recorded on eu-oaz72.21 (`dict.update` over
   the caller's frame) and add the acceptance gate: a dict-threading loop
   storing block literals and lazy lists, run under a small `--heap-limit-mib`
   as a harness test, fault-injection verified.

## 5. Decisions needed from the owner

- Is the cost of boxed thunks acceptable in principle, given no prototype data
  (phase 0 answers it)?
- Is the AoC 2026 horizon worth this, versus documenting the limit (design
  note §12) and recommending that dict-threading loops store evaluated values (for
  example via `update-strict`), which §12 says detaches them?
- Approval of a wire-format bump (v11) if phase 1 encodes capture lists.
