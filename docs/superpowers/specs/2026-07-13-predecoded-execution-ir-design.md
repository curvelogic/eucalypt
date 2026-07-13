# Production design — pre-decoded execution IR (lever a, BV2 folded in)

- **Bead:** eu-2sa6.10 (0.13, parent epic eu-2sa6)
- **Author:** Furnace
- **Date:** 2026-07-13
- **Status:** DESIGN NOTE ONLY — no implementation. Owner sign-off gates the
  build; the build is additionally gated on Tier-0 (eu-2sa6.6/.7, canonical
  suite + clean-room re-baseline) landing first, per the ratified plan
  (`docs/superpowers/specs/2026-07-12-bytecode-transition-review.md` §7).
- **Depends on / supersedes nothing**: extends the spike
  (`docs/superpowers/reports/2026-07-13-predecode-spike.md`, throwaway code on
  `spike/predecode-ir`, draft PR #988 kept as reference implementation) from a
  falsification experiment into a buildable design. The spike's own
  simplifications (lazy cache-by-offset, `SmallVec`-in-enum records, operand-only
  pre-decode) are each revisited below and either adopted or explicitly
  upgraded for production, with the reasoning shown.

## 0. Executive summary

The spike (eu-2sa6.9, CONFIRMED, coordinator-verified) proved the mechanism:
decoding each opcode once into a typed record and dispatching over typed fields
removes the entire 14.3% byte-decode bucket and cuts per-tick mutator cost
18–22% uniformly, with bit-identical ticks/allocs. This note specifies the
**production** version: (1) a genuinely fixed-width `Instr` record with
variable-length operands (branch tables, arg lists, form headers) moved into
**side pools** rather than inline `SmallVec`s — the spike's inline approach
would blow the per-instruction record up 3–5× if shipped as-is; (2) a
**dense, ordinal-indexed** instruction array for the always-hot prelude (eager
decode at blob-load, ~350 µs projected, must be measured) and a **lazy
first-touch** array for per-run user code (the spike's proven strategy,
avoiding paying to decode code that never executes in one-shot scripts); (3)
BV2 folded in as a `Vec<Smid>` side table keyed by ordinal, with `Op::Ann`
**eliminated from the dispatch loop entirely**, not merely retyped; (4) a
GC-invariant restatement (zero pointers in the new structures, matching
ROADMAP §7 decision 2's constant/heap split); (5) a flag-gated, soak-then-delete
migration mirroring how BV1 itself shipped; (6) per-workload-class projections
against the Phase-4 falsifiable gate (review spec §6), with the honest
admission that alloc-bound (drop_cons-class) workloads need the branch-table
side pool — not measured by the spike — to plausibly clear the ≤1.15× bar.

---

## 1. Record layout

### 1.1 Why the spike's layout cannot ship as-is

The spike's `Instr` (`spike/predecode-ir`,
`src/eval/bytecode/machine.rs:2148+` in that branch — see the `Instr` enum
added after `step`) used `SmallVec<[T; 8]>` fields *inline in the enum*:
`PdRefs`/`PdOffs`/`PdHeaders`/`PdBranches`, each an 8-element inline buffer
(`DecodedRef` is 8 bytes tag+payload, `FormHeader` is 12 bytes). A Rust enum is
sized to its largest variant plus a discriminant, so `Instr::Let`/`::LetRec`
(a `SmallVec<[FormHeader; 8]>`, ~112 bytes with the `SmallVec` header) dominates
— **every** `Instr`, including a bare `Atom` (which needs 8 bytes), pays that
~120-byte tax. Over the prelude's ~7,061 STG nodes (`arena: 7061 nodes, 5726
forms`, the prelude-compile build log) that is **~850 KB just for the
prelude's instruction array**, more than 3× the packed byte stream (265,438
bytes today). This was an acceptable shortcut for a 1–2 day throwaway spike
(correctness and mechanism validation, not footprint); it is not acceptable for
production. It is also the concrete instance of the ROADMAP §10.2 lesson
(flat closures: a uniformly wider representation lost 2–10% wall time despite a
27% self-time win on the operation it targeted, because most closures are
small and the wider record cost more than it saved) — a wide `Instr` risks the
same trap on the many single-operand opcodes (`Atom`, `Ann`, `Seq`,
`BlackHole`) that dominate instruction *counts* even when `Let`/`Case` dominate
instruction *hot-path time*.

### 1.2 Production layout: fixed-width record + side pools

Move every variable-length operand list out of the record into a typed side
pool, and make `Instr` itself a small `Copy` struct, not an enum sized to its
largest variant:

```rust
/// One decoded instruction, fixed width regardless of operand-list length.
/// `op` reuses the existing `Op` enum (opcode.rs:11-33, `#[repr(u8)]`, already
/// exhaustive over the 15 kinds). `a`/`b`/`c` are opcode-dependent: a raw
/// `CodeRef` (now an *ordinal*, see §2), a packed `DecodedRef` (tag in the top
/// byte of `flags`, payload in the u32), or — for variable-length operands —
/// the start index into the relevant side pool, paired with `len`.
#[derive(Clone, Copy)]
struct Instr {
    op: Op,        // 1 byte (opcode.rs Op::Atom..Op::FusedPrimop)
    flags: u8,      // eager bit (FLAG_EAGER), has_fallback bit, ref-tag bits for a/b
    len: u16,       // side-pool run length (arg/branch/header count); 0 for fixed-arity ops
    a: u32,         // primary operand — meaning is opcode-dependent (see table)
    b: u32,         // secondary operand
    c: u32,         // tertiary operand / side-pool start index
}
// size_of::<Instr>() == 16 bytes (1+1+2+4+4+4, no padding) on a 4-byte-aligned struct.
```

> **Post-sign-off correction (2026-07-13, eu-2sa6.13 implementation).** As
> originally written this struct also listed a `smid_idx: u32` field, which made
> the record `1+1+2+4+4+4+4 = 20` bytes, not the 16 the surrounding prose (and
> the bead title) claims. That field is redundant with §3's decision — smids
> live in a **side table keyed by ordinal** (`smids[ord]`), *not* inline on the
> record — so it is dropped here. The record is `1+1+2+4+4+4 = 16` bytes exactly
> (enforced by a `const _: () = assert!(size_of::<Instr>() == 16)` in the
> implementation). This resolves the arithmetic in favour of §3; no other change
> to the field semantics. Owner-signed.

Per-opcode field mapping (mirrors `handle_op`, `machine.rs:1716-2090`, and the
byte layout each `read_*` helper already decodes):

| Op (opcode.rs:12-32) | a | b | c | len/side-pool |
|---|---|---|---|---|
| `Atom` | `DecodedRef` packed (tag in flags, payload in `a`) | — | — | — |
| `Cons` | tag (low byte of `a`) | — | pool start | `len` = arity, pool = `refs: Vec<DecodedRef>` |
| `Case` | scrutinee ordinal | fallback ordinal (or `NO_BRANCH`) | pool start | `len`, `min_tag` in `flags`; pool = `branches: Vec<CodeRef>` (`NO_BRANCH` sentinel retained, `opcode.rs:81`) |
| `Seq` | scrutinee ordinal | body ordinal | — | — |
| `Ann` | — | — | — | **eliminated**, see §3 |
| `FusedPrimop` | left ordinal | right ordinal | — | `primop_id` in low byte of `flags` |
| `BlackHole` | — | — | — | — |
| `App` | callable `DecodedRef` packed | — | pool start | `len`, pool = `offsets: Vec<CodeRef>` (atom-node ordinals, mirrors today's `arg_offs`) |
| `Let` / `LetRec` | body ordinal | — | pool start | `len`, pool = `headers: Vec<FormHeader>` (unchanged 12-byte shape, `machine.rs:174-179`) |
| `DirectApp` | callable `DecodedRef` packed | — | pool start | `len`, pool = `offsets`; smid via `smid_idx` (was inline, `machine.rs:1896`) |
| `Bif` | intrinsic index (low byte of `a`) | — | pool start | `len`, pool = `refs` |
| `Meta` | meta-ref `DecodedRef` packed | body-ref `DecodedRef` packed | — | — |
| `DeMeta` | scrutinee ordinal | handler ordinal | fallback ordinal | — |
| `LookupLit` | key `DecodedRef` packed | obj `DecodedRef` packed | default ordinal | smid via `smid_idx` |

A `DecodedRef` (`machine.rs:74-81`: `Local(u32) \| Global(u32) \| Value(u32)`)
packs into one `u32` field: the tag needs 2 bits (fits in the 4 spare bits of
`flags` for a single-ref opcode, or — for `Meta`/`LookupLit`, which need two
packed refs — reuses 2+2 bits of `flags`). This is the same information
`read_ref` decodes today (`machine.rs:85-97`, tag byte + `u32` payload); packing
loses nothing, since `Local`/`Global`/`Value` indices never need the full 32
bits distinguishing themselves from each other (that's what the tag is for).

**Three side pools**, each a plain `Vec<T>` built once, append-only, indexed by
`(start: u32, len: u16)` from the owning `Instr`:

| Pool | Element | Element size | Used by |
|---|---|---|---|
| `refs: Vec<DecodedRef>` | tag + payload | 8 B | `Cons`, `Bif` arg lists |
| `offsets: Vec<CodeRef>` | ordinal | 4 B | `App`, `DirectApp` arg-atom lists |
| `headers: Vec<FormHeader>` | kind/arity/smid/body | 12 B | `Let`, `LetRec` bindings |
| `branches: Vec<CodeRef>` | ordinal or `NO_BRANCH` | 4 B | `Case` densified branch table |

**This directly fixes the spike's identified residual.** The spike's
`Instr::Case` still held `branch_table: PdBranches` *inside* the enum and
`handle_op_predecoded` rebuilt a fresh GC `Array` from it on every evaluation
(`table[off] → Array::with_capacity + push` loop, the spike's `Instr::Case` arm)
— the exact `Array<Option<u64>>::push` cost the drop_cons profile still showed
at 2.8% (§4.2 of the spike report). Under this layout the branch table lives
**once** in the `branches` pool at decode time; the `Branch` continuation
(`cont.rs:25`) needs to reference a *slice* of that pool rather than own an
`Array`, so `BcContinuation::Branch` changes from owning an `Array<Option<u64>>`
to holding `(pool_start: u32, pool_len: u16)` (or, if the continuation's
lifetime must survive a pool reallocation — it cannot, since the pool is
append-only and never resized after decode — a raw `&'static [CodeRef]`-shaped
index is safe). No per-Case allocation, no per-Case rebuild: this is the
"materialise once, not every time" fix review B (§1, "CASE") diagnosed HeapSyn
as already doing via `branch_table.clone()` (a refcount-style `Array` clone,
`vm.rs:470`) — the side-pool slice is bytecode's off-heap equivalent.

### 1.3 Memory footprint estimate

`size_of::<Instr>() = 16` bytes (vs. the spike's ~80–120 bytes per variant).
Over the prelude's ~7,061 STG arena nodes (a reasonable proxy for instruction
count, since STG-compile emits roughly one opcode instruction per arena node):

- Dense `Vec<Instr>`: 7,061 × 16 B ≈ **113 KB**.
- Side pools: proportional to the variable-length operand bytes already present
  in the 265,438-byte stream (arg-offset lists, branch tables, form headers) —
  these are a *minority* of the stream (most bytes are fixed-shape headers:
  opcode byte, tag bytes, `u32` targets); a conservative estimate, assuming
  ~40% of the stream is variable-length payload re-expressed at 1.3–2× density
  (word-aligned `u32`/8-byte entries vs. tightly packed bytes), is **~140–210
  KB**.
- **Total projected resident cost for the prelude: ~250–320 KB**, i.e.
  **roughly parity with the 265 KB packed byte stream**, not the 3–5× blow-up a
  naive per-variant enum would cost. This is the number that matters for the
  "does BV5's startup/footprint win survive" question in §2.

This is architectural arithmetic, not a measurement — **only implementation
confirms the real multiplier** (STG-node-count is a proxy for instruction
count, not exact; alignment/padding in the actual Rust layout may differ). Flag
this explicitly for the implementer: measure resident heap (RSS delta,
`heap_stats()`/`HeapStats`) with the production layout on the full canonical
suite (once eu-2sa6.6 lands), not just the four spike benches — see §8 on why
small benches can hide a footprint regression that only shows on AoC-scale
programs.

### 1.4 Interaction with the block index (eu-4zhi, deferred, disjoint)

`LookupLit`'s `default_off` and the `bc_lookup_in_block` fast path
(`machine.rs:894-932`) currently re-walk `decode_cons` byte-by-byte on every
lookup. Under this layout `decode_cons` becomes an O(1) `Instr` lookup by
ordinal (§5). The block-index work (O(n)→O(1) `LookupOr`/`SafeLookup`) is
**orthogonal** — the 2026-07-12 allocation-gap spike found zero block-lookup
samples in every alloc-bound case profiled — but the `LookupLit` record's
`c` field (default ordinal) and the `refs`/`headers` pool layout should be
treated as stable once eu-4zhi is scheduled: that work indexes *into* a forced
block value at runtime, not into the decoded-instruction pools, so no
record-layout churn is expected, but the implementer should re-read eu-4zhi's
eventual design before assuming this.

---

## 2. Decode-once-at-load path

### 2.1 Where it hooks

The natural hook is `driver/eval.rs:490`, `BytecodeMachine::new(prog, root,
&gforms, ...)` — the point where a **complete, final** `BytecodeProgram` exists,
regardless of which of the two upstream paths produced it:

- **Blob path** (`driver/eval.rs:471-482`): `encode_overrides_and_root`
  (`encode.rs:663-703`) clones the embedded prelude's `code`/`constants`
  (`base.code.clone()`, `encode.rs:670`) and appends the per-run
  `__args`/`__io`-override globals and the user program root via
  `enc.encode_node` (`encode.rs:675,689`) — a small delta on top of the
  265,438-byte prelude.
- **Source path** (`driver/eval.rs:483-486`, no blob or a pre-BV5 blob):
  `encode(&syn, &globals)` (`encode.rs:596`) encodes the whole program —
  prelude included — from STG in one pass.

Either way, `BytecodeMachine::new` receives one finished `BytecodeProgram`
whose `code: Vec<u8>` never grows again — this is exactly the right moment to
decode. **eu-2sa6.5 (blob-vs-source fusion-parity bug: fib 88.85 M ticks blob
vs. 98.28 M ticks source, same code) must be fixed first** (already a Tier-0
gate, plan §7 item 5): the decode pass faithfully reproduces whatever bytecode
it is handed, so if the two paths still diverge, pre-decoding bakes the
divergence into two different `Vec<Instr>` shapes rather than fixing it — the
tick-parity assertion belongs *before* this lands, not as a side effect of it.

### 2.2 Eager (prelude) vs. lazy (user code) — a genuine design choice, resolved asymmetrically

The spike decoded **lazily, per-offset, on first execution**
(`handle_op_predecoded`, `if table[off].is_none() { table[off] =
Some(decode_instr(...)) }`) into a `Vec<Option<Instr>>` sized to `code.len()`
— i.e. one slot **per byte**, mostly `None`. That is correctness-proven and
low-risk but wasteful in two ways production should not inherit: (a) the
per-byte sparse array costs `code.len() × size_of::<Option<Instr>>()`
regardless of how much code actually executes; (b) every dispatch pays an
`is_none()` branch-and-maybe-decode check, which is exactly the kind of
per-tick overhead this lever exists to remove.

Recommendation: **treat the prelude and user code differently**, because they
have different amortisation profiles:

- **Prelude (BV5-embedded, reused unmodified every run):** decode **eagerly
  and fully** at blob-load time (or lazily on first blob use, memoised for the
  process — the blob is loaded once per process regardless), via a
  **reachability walk** from every static entry point: `global_entries`
  (548 slots), `templates` (indexed by `DataConstructor` tag), `pap` (up to
  `PAP_MAX_ARITY²` = 256 trampoline entries), `blackhole`, `meta_template`,
  `apply1_template`, `apply2_template`, `producer_tail_template`
  (`program.rs:33-79`, the full `BytecodeProgram` field list) — plus every
  offset any decoded `Instr`'s operand fields transitively name. At ~7,061
  nodes and a handful of `read_u8`/`u16`/`u32` calls each (sub-100 ns per
  node, generously), full eager decode is projected at **≈350 µs–1 ms** —
  negligible against BV5's measured ~10 ms startup floor (ROADMAP §7:490-493).
  **This must be measured, not assumed**: BV5's entire value proposition is a
  low, deterministic startup floor, and "the added step is obviously cheap" is
  precisely the kind of unvalidated intuition the flat-closures revert (§8,
  ROADMAP §10.2) punished — a component that looks negligible in isolation
  cost 2–10% wall time in aggregate there.
- **User program code (compiled fresh per run, coverage varies widely — a
  one-shot script may execute a tiny fraction of its own compiled forms, let
  alone the prelude's):** keep the spike's **lazy, first-touch** strategy —
  proven correct, and it avoids paying full-decode cost for code paths a
  particular invocation never reaches (e.g. an `eu -e '1+1'` run touches a
  handful of prelude globals, not all 548).

This asymmetry requires the ordinal space (§2.3) to be assignable
incrementally — the prelude's ordinals are fixed at blob-load time; the
user-code delta (small, appended after `base.code.clone()`, §2.1) gets its own
ordinals lazily as `encode_overrides_and_root`'s output executes. The two
`Vec<Instr>`/pool sets are logically one array (concatenated ordinal space)
but populated by two different schedules.

> **Phase 1 status & deferred optimisation (2026-07-13, eu-2sa6.13).** Phase 1
> ships a single **eager full-reachability decode** from every root (§2.2's eager
> path applied to user code too), not the eager-prelude + lazy-first-touch split
> above. The lazy schedule is a **startup optimisation, not a correctness
> requirement** — walking from the user program's own root already decodes only
> code reachable from it, and the prelude (the bulk) is decoded eagerly either
> way — so the split was dropped for Phase 1 to keep one decode path. **Deferred
> follow-up:** re-introduce lazy first-touch *decode-fill* for user code (eager
> ordinal discovery, lazy `Instr` population) if the startup measurement (§8
> risk 2) shows the eager user-code decode is a measurable share of the BV5
> startup floor. Owner-signed for Phase 1.

### 2.3 `CodeRef` changes meaning: byte offset → instruction ordinal

Today `CodeRef = u32` (`mod.rs:7`) is a **byte offset** into `prog.code`; every
closure (`BcClosure.info: InfoTagged<CodeRef>`, `closure.rs:36`) and every
program-level table (`global_entries`, `templates`, `pap`, …) carries one.
Under a dense `Vec<Instr>`, the natural (and cache-friendliest) representation
is to redefine `CodeRef` as an **instruction ordinal** — an index into
`Vec<Instr>` — not a byte offset, eliminating the "one slot per byte" waste the
spike's `Vec<Option<Instr>>` incurred.

This is sound because **every `CodeRef` a closure or table ever holds
originates from decoding** — there is no runtime code path that synthesises a
raw byte offset from scratch (closures are always built from a decoded
`Instr`'s `a`/`b`/`c`/pool-entry field, or from one of the `BytecodeProgram`'s
static table entries). The decode pass therefore does a **two-phase** walk per
reachable region:

1. **Discovery** (worklist/BFS from the roots enumerated in §2.2): assign each
   newly-discovered byte offset the next ordinal, in discovery order, building
   a transient `byte_off → ordinal: HashMap<u32, u32>` (or a sorted `Vec` +
   binary search, if profiling shows the hash map itself is a decode-time cost
   worth avoiding — decode-time cost, not hot-path cost, so this is a minor
   choice). LetRec/recursive bindings and forward Case-branch targets mean
   this must be a full worklist, not a single recursive descent — cycles and
   forward references are expected (mirrors why `Op::LetRec`'s two-pass
   frame-fill exists today, `machine.rs:1862-1891`).
2. **Decode + fixup**: decode each discovered offset's `Instr` (§1.2), writing
   every `CodeRef`-typed operand as a **byte offset** initially, then run one
   `O(instruction count)` fixup pass rewriting every such field through the
   completed map. (Or interleave: decode after the worklist reaches a fixed
   point, guaranteeing every child offset is already assigned an ordinal —
   avoids the separate fixup pass at the cost of two full walks instead of
   one-and-a-half; a minor implementation-time trade, not a design fork.)

The transient `byte_off → ordinal` map is **discarded after decode** — it is
not part of the runtime footprint in §1.3. Only the dense `Vec<Instr>` and the
three side pools persist.

### 2.4 What decode-once removes from the hot path

- The lazy-cache-miss check (`table[off].is_none()`) the spike needed —
  gone for the prelude (eager); still present, but on a shrinking miss-rate
  curve, for user code (lazy). No behavioural difference; a request to note
  for the implementer, not a design gap.
- The sparse `Vec<Option<Instr>>` sized to `code.len()` (one slot per byte,
  spike-only) — replaced by the dense, ordinal-indexed `Vec<Instr>` sized to
  instruction count.

---

## 3. BV2 fold-in (eu-2sa6.1) — smids as a side table, `Op::Ann` retired

**Decision: a side table (`smids: Vec<Smid>`, one entry per instruction
ordinal), not an inline field on every record.**

`Smid` is `Option<NonZeroU32>` (`sourcemap.rs:17`), 4 bytes via niche
optimisation, and — per the existing memory note — "Copy, no GC scanning
needed." Embedding a full `Smid` in *every* `Instr` costs 4 bytes × ~7,061
prelude instructions ≈ 28 KB, cheap in isolation, but most instructions carry
**no** annotation in the current encoding (only `Op::Ann`-wrapped bodies and
the two opcodes that already inline a smid — `DirectApp`, `machine.rs:1896`,
whose comment reads "Inline smid replaces a wrapping Ann"; `LookupLit`,
`machine.rs:2006` — carry one at all). A side table keyed by ordinal,
populated only where a real annotation exists (unpopulated slots read as
`Smid::default()`, the existing "no annotation" sentinel, `sourcemap.rs:19-24`),
is both smaller and — more importantly — is the mechanism that lets `Op::Ann`
disappear from dispatch entirely rather than merely become a typed record:

- **Today:** `Op::Ann` is its own dispatch arm (`machine.rs:1807-1812`): read
  `smid` + `body_off`, set `state.annotation = smid`, jump to `body_off`. It
  costs one full dispatch cycle (`handle_op` re-entry) purely to relabel the
  annotation before falling through to the wrapped node.
  `Op::DirectApp`/`Op::LookupLit` already avoid this by inlining their smid
  (skipping the wrapper), which is the pattern to generalise.
- **Production:** at decode time, when the discovery walk (§2.3) encounters an
  `Op::Ann` byte-stream node, it does **not** allocate an `Instr` slot for it.
  Instead it (a) decodes the wrapped body at the `Ann` node's `body_off` as
  usual, giving it the *next* ordinal, (b) records `smids[body_ordinal] =
  smid` in the side table, and (c) rewrites every *other* instruction's
  operand field that named the `Ann` node's own offset (its parent's `CodeRef`
  to it) to point directly at the body's ordinal instead. The wrapper offset
  itself never becomes an `Instr` and is never visited by the dispatch loop.
  `DirectApp`/`LookupLit`'s already-inline smids become ordinary `smid_idx`
  entries in the same table, unifying the two existing mechanisms (wrap-node
  vs. inline-field) into one.
- **Dispatch change:** `handle_op_predecoded`'s equivalent reads
  `state.annotation = smids[ordinal]` (a table lookup, O(1), no branch on
  presence needed if the sentinel is cheap to test) as a small step folded into
  every opcode's dispatch prologue — exactly mirroring how `handle_op` already
  does `if closure.annotation().is_valid() { state.annotation = ... }` from the
  *closure's* static info (`machine.rs:1726-1728`) before the `match op`; BV2
  moves the second annotation source (the byte-stream `Ann` wrapper) onto the
  same uniform mechanism, and the `match op` itself loses a whole arm.

**Correctness-critical note:** annotation propagation feeds error source
locations (the `ExecutionError`/diagnostic trace machinery,
`src/eval/error.rs`, and the `env_trace`/`stack_trace` search the memory notes
flag as delicate). BV2's fold-in must be validated against the existing error-
location harness tests, not just tick/output identity — a regression here is
silent (wrong-but-plausible source location in an error message) rather than a
crash, so it needs its own explicit gate in §6, not just "harness green."

---

## 4. GC invariants

The spike already proved the *cache-side* half of this: `BcClosure` scans only
its `env: RefPtr<BcEnvFrame>` (`closure.rs:179-191`, `GcScannable for
BcClosure::scan`) — the `code: CodeRef` field is documented as "an inert `u32`
offset" that "never scans" (`closure.rs:16-18`, `mod.rs:4-6`). Redefining
`CodeRef` from byte offset to ordinal (§2.3) does not change this: it is still
a `u32` index into an off-GC-heap structure, still `Copy`, still invisible to
`scan`/`scan_and_update`.

For the **new** structures (`Vec<Instr>`, the three side pools, `Vec<Smid>`),
the same argument extends field-by-field:

| Field | Type | GC surface |
|---|---|---|
| `Instr::op`/`flags`/`len` | scalar | none |
| `Instr::a`/`b`/`c` | `u32` — either an ordinal (inert, §2.3) or a packed `DecodedRef` payload | none directly; `DecodedRef::Value(k)` *indexes* into `constants: Vec<Ref>` (`program.rs`), which is separately GC-rooted (`BcMachineState::constants`, `machine.rs:259`) exactly as today — the index is inert, the pointer it eventually resolves to is scanned where it always was |
| `Instr::smid_idx` | `u32` index into `smids: Vec<Smid>` | none — `Smid` is `Copy`, no scan (existing project convention) |
| `refs: Vec<DecodedRef>` pool | tag + `u32` payload | none, same reasoning as `Instr::a`/`b`/`c` |
| `offsets: Vec<CodeRef>` pool | ordinal | none |
| `headers: Vec<FormHeader>` pool | `kind: u8, arity: u8, smid: Smid, body: CodeRef` | none — all four fields are `Copy` scalars/ordinals (`FormHeader` is already `#[derive(Clone, Copy)]`, `machine.rs:173`) |
| `branches: Vec<CodeRef>` pool | ordinal or `NO_BRANCH` sentinel | none |

**Zero GC pointers anywhere in the pre-decoded structures** — this matches
ROADMAP §7 decision 2 exactly ("Code lives in the non-GC arena; the **values
it references** … must stay GC-/pool-managed. Decide the constants-pool
boundary so no GC scan ever touches code and no arena offset is ever moved by
evacuation.") The constants pool (`BytecodeProgram::prepare_constants`,
`program.rs:104-123`, producing `Ref::V(Native)` entries rooted via
`BcMachineState::constants`) is the boundary today and remains the boundary
under this design — pre-decoding does not move it, only replaces how the
*indices into it* are read (once, at decode time, instead of once per tick).

The `BcContinuation::Branch` change in §1.2 (owning a pool slice instead of an
`Array<Option<u64>>`) needs one more check: today `Branch`'s `branch_table:
Array<Option<u64>>` lives on the **GC heap** (`Array::with_capacity(&view,
len)`, `machine.rs:1777`) and is scanned as part of the continuation stack. If
it becomes a pool-slice reference instead, it moves **off** the GC heap (the
pool is a plain `Vec` outside the mutator's `view`) — this is a net
**reduction** in what the collector scans per `Case`, but the implementer must
confirm `BcContinuation`'s `GcScannable` impl (`cont.rs`, not read in detail
here — flagged for implementation-time verification) is updated to stop
scanning that field, and that nothing else assumes `Branch`'s branch table is
heap-allocated (e.g. any code relying on its address stability across a GC —
the pool, being append-only and never resized after decode for a given
program, has this property trivially, unlike a `Vec` that could reallocate,
but *only* if the pool truly never grows post-decode; user-code lazy decode
(§2.2) means the pool **does** grow during a run, so a `push` that
reallocates the backing `Vec` would invalidate any raw pointer/slice held
across it — continuations must therefore hold `(start, len)` **indices**, not
raw slices, exactly as specified in §1.2, precisely to survive pool growth).

**`EU_GC_VERIFY=2`/`EU_GC_POISON=1`/`EU_GC_STRESS=1` gate (§6):** since nothing
in the new structures is scanned, these flags exercise unchanged code paths
for the pre-decoded structures themselves — the gate is really validating that
*nothing new leaked into the scan set*, which is a "stays green" bar, not a
"new behaviour to verify" bar. The spike already passed this for its (smaller)
structures; production must re-confirm on the larger, pool-based layout.

---

## 5. What gets deleted, what stays

**Retired from the hot dispatch loop** (still exist as the decode-time
byte-stream reader, invoked once per instruction at decode, not once per
tick):

- `Op::from_u8` (`opcode.rs:36-56`) — called once per instruction at decode
  time instead of once per dispatch. The `match b { 0x01 => ..., }` body is
  unchanged; only its call frequency drops from O(ticks) to O(instructions).
- `read_u8`/`read_u16`/`read_u32` (`program.rs:130-150`) — same: still exist
  (the decoder uses them), no longer on the per-tick path.
- `read_ref` (`machine.rs:85-97`), `read_arg_offsets` (`machine.rs:167-170`),
  `read_form_header` (`machine.rs:182-193`), `read_branch_table`
  (`machine.rs:198-212`), `arg_ref` (`machine.rs:1585-1588`) — all decode-time
  only, called from the discovery/decode walk (§2.3), never from dispatch.
- The **byte-path arms of `handle_op`** (`machine.rs:1716-2090`, ~375 lines)
  — once the flag/soak period (§6) ends and the byte path is deleted, this
  entire function is replaced by the typed `handle_op_predecoded` equivalent.
  Line count is **not** dramatically smaller (the typed version still has one
  arm per opcode with comparable logic — the win is removing the `read_*`
  calls *inside* each arm, not removing arms), but every arm's body shrinks
  (no more `read_u32`/`read_u8` sequences, just field reads) and — the
  qualitative change that matters — **the whole function's cost moves from
  "paid every tick" to "written once, executed as plain struct-field
  dispatch"**.
- `decode_cons` (`machine.rs:820-829`), `value_symbol` (`machine.rs:834-869`),
  `pair_key_symbol` (`machine.rs:872-888`), `bc_lookup_in_block`
  (`machine.rs:894-932`) — these currently byte-peek a closure's code to check
  its shape (e.g. "is this a `Cons(Block)` node") on every lookup. Under the
  ordinal-indexed `Vec<Instr>` these become `table[c.code() as usize]` matches
  on the typed record — O(1) field reads instead of a partial byte-decode,
  same simplification pattern as `handle_op`.

**Stays unchanged:**

- The byte stream itself, **as the BV5 blob wire format** (`program.rs:33-79`
  `BytecodeProgram.code: Vec<u8>`, the postcard-serialised
  `PreludeBytecodeImage`) — review B §3's reframing ("keep the stream for the
  blob/cache/PP wire … decode once into the flat IR for execution") is the
  basis of this whole design: nothing about serialisation, the blob's
  determinism/CI-guard, or PP's future IPC wire format (ROADMAP §7 decision 5)
  changes. The decoder becomes purely a **load-time** step, run once per
  process (prelude) or once per compiled unit (user code, lazily).
- The continuation stack model (`cont.rs`, six kinds), the environment-frame
  model (`env.rs`, `EnvironmentFrame<BcValue>`), the deferred-BIF boundary
  (`BifFrame`, `machine.rs:224-240`, `materialize_bif_args`/`recycle_bif_args`,
  `machine.rs:463-489`) — all untouched by this lever, per ROADMAP §7 decision
  3 ("Continuations & intrinsics — unchanged in BV1 … Revisit … only post-BV1"),
  which this design does not revisit either.
- `BcClosure`/`BcValue` (`closure.rs`) — unchanged except that `CodeRef` now
  means ordinal, a type-level rename with no structural change (`BcClosure`
  still holds `InfoTagged<CodeRef>` + `env`).

**Net `machine.rs` effect:** modest LOC reduction (the decode helpers move but
mostly persist as decode-time code; the dispatch arms shrink per-arm rather
than disappearing), but a real reduction in the *file's conceptual surface* —
one dispatch path instead of two (once the flag is retired, §6), and the
byte-decode helpers become clearly load-time-only rather than interleaved with
hot-path logic. Do not oversell this as a large deletion; the meaningful win is
architectural (per-tick cost, §7), not textual.

---

## 6. Migration & gates

**Not an atomic switch.** Land flag-gated, exactly as the spike validated the
mechanism, then soak, then delete the byte path — mirroring how BV1 itself
shipped (default engine behind `EU_HEAPSYN=1` opt-out, HeapSyn retained as
oracle/fallback, not deleted on day one). "Phase 4 resolves the residual" is
already rejected as a criterion by the ratified review (§5 item 4-adjacent);
the same discipline applies here: earn confidence before deleting the
fallback.

**Step A — infrastructure, flag-gated, coexisting with the byte path.**
Land the side-pool decode (§1.2), the ordinal-indexed dense array + lazy
user-code array (§2), and `handle_op_predecoded` as a new path selected by (a
real, documented, not throwaway) flag. Gates, all required before merge:
- `cargo test` fully green with the flag **off** — byte-identical (the byte
  path is untouched).
- Full harness green with the flag **on**
  (`EU_<FLAG>=1 cargo test --test harness_test`).
- **Tick-identical** flag-on vs. flag-off on the full canonical suite (once
  eu-2sa6.6 lands; the spike validated this on 4 benches, production must
  validate on the full suite before this is trusted generally) — this is a
  representation change, not a semantic one, and must be provably so.
- `EU_GC_VERIFY=2` + `EU_GC_POISON=1` + `EU_GC_STRESS=1`, flag on, full suite.
- `differential.rs` (`src/eval/bytecode/differential.rs`, the existing
  bytecode-vs-HeapSyn differential harness) extended to run **three** modes —
  byte-path bytecode, pre-decoded bytecode, HeapSyn — asserting all three agree,
  not just bytecode vs. HeapSyn as today.

**Step B — BV2 fold-in (§3), same PR or immediately following.** Additional
gate: the error-location/source-annotation test suite (wherever the project's
`Smid`-propagation/diagnostic-location tests live — the memory notes flag this
area as historically fragile, e.g. the `annotated_lambda`/`Ann`-node source
propagation work referenced in project memory) must stay green, checked
explicitly, not just swept into "harness green."

**Step C — eu-2sa6.5 must land first (already a Tier-0 gate).** The blob-vs-
source fusion-parity fix, plus its own tick-parity assertion, is a
**precondition**, not a parallel-track nice-to-have: this design's decode pass
faithfully reproduces whatever `BytecodeProgram` it is handed (§2.1), so if
the two producing paths still diverge, pre-decode ships two silently different
`Vec<Instr>` shapes for "the same" program depending on build configuration.

**Step D — soak, then delete.** After a release (or the project's chosen soak
period, mirroring the Phase-4 gate's own "one release of soak" requirement,
review spec §6 item 2) with the flag on by default and no regressions
surfaced, delete the byte-path dispatch arms (§5) and the flag itself. This is
the point at which the LOC reduction in §5 is fully realised (one path, not
two) — do not delete the byte path at Step A; keeping it "one release
reintroducible" is the same risk posture the Phase-4 gate already requires for
HeapSyn itself.

---

## 7. Projected outcome per workload class

Grounded in the spike's measured **18–22% uniform per-tick reduction** from
operand-only pre-decode, plus the reasoned (unmeasured) effect of the
branch-table-pool fix (§1.2) this design adds on top. Phase-4 gate thresholds
(review spec §6): dispatch/env-walk ≤ 1.00; startup/config render ≤ 1.00;
decode-bound compute ≤ 1.05; alloc-bound compute ≤ 1.15; GC-pressure remains a
win; aggregate geomean ≤ 1.00.

| Class | Spike evidence | Projection with this design | Confidence |
|---|---|---|---|
| **Alloc-bound** (drop_cons-class: LET-per-cons, dense `Cons`/`Case`) | per-tick 1.60× → 1.25× (operand-only) | Branch-table pooling removes the residual `Array<Option<u64>>::push` rebuild (2.8% of samples, spike profile §4.2) and the Let/LetRec header-array build is already lean (§1.2 pool). Plausibly **1.10–1.15×** — at or just inside the ≤1.15× gate. | **Low-medium** — this is the one class the spike did not measure with the fix applied; genuinely needs implementation + measurement, not just the operand-pre-decode number extrapolated. |
| **Decode-bound compute** (day03-p2/day09-p1-class: arithmetic+call dense, `FusedPrimop`-heavy) | per-tick 1.47× → 1.14× (day03-p2, operand-only) | Little further headroom from *this* lever specifically (its residual is dispatch/env-walk, shared with HeapSyn, not decode) — **≈1.10–1.14×ish**, likely short of the ≤1.05× gate on its own. | **Medium** — BV3 or further `FusedPrimop` broadening is probably needed to close the last stretch, consistent with ROADMAP's own sequencing (BV3 after lever a). |
| **Dispatch/env-walk** (day11-p1-class, call-dense recursion, few allocations) | Not directly isolated by the spike's four benches (fib mixes App+FusedPrimop; none is a pure dispatch/env-walk probe) | The uniform 18–22% per-tick reduction, if it generalises (reasoned, not measured on this exact class), should be sufficient to clear ≤1.00× — this class already sits near parity per the 0.12 BV1 realised note (ROADMAP §7: day11-p1 bc/hs ≈ 0.93–0.97 pre-fusion). | **Medium** — needs eu-2sa6.6's canonical suite (which explicitly targets this gap per its own scope) to confirm. |
| **Startup/config render** (BV5's domain) | Not measured by the spike (spike never isolated load time) | §2.2's ~350 µs–1 ms projection against BV5's ~10 ms floor suggests negligible risk, but this is the class most vulnerable to an unvalidated "obviously cheap" assumption (§8) — must be the *first* thing measured once implemented. | **Low** — explicitly flagged as needing direct measurement, not inference from the four compute benches. |
| **GC-pressure** (day08-class, bytecode already wins hugely, zero GC frames) | Not applicable — this lever doesn't touch allocation counts or the GC scan set (§4) | Should hold or mildly improve (less dispatch overhead per tick, same allocation behaviour) — low priority to re-validate given the existing enormous margin, but included in the full-suite regression run for completeness. | **High** (low risk either way) |

**What only implementation can confirm**, stated plainly: the alloc-bound
projection (the class this design's one substantive addition over the spike —
branch-table pooling — targets), the startup-cost measurement, and whether the
per-tick reduction generalises to dispatch-heavy code the spike's four benches
did not probe. None of these should be asserted as "done" until measured on
the Tier-0 canonical suite (eu-2sa6.6) under the ratified protocol.

---

## 8. Risks

**1. Resident memory growth — the flat-closures lesson, restated for this
lever.** ROADMAP §10.2: a uniformly wider representation (flat closures)
improved its targeted self-time metric (env `get`, −27%) but **regressed net
wall time 2–10%** because most captures were small and the wider record cost
more broadly than it saved narrowly. The structural parallel here: `Instr`
sized to its largest-variant enum form (the spike's shortcut, §1.1) would
repeat this exactly — a real win on `Case`/`Let` traffic, paid for by every
`Atom`/`Ann`/`Seq` instruction (the majority by *count*, even if not by
hot-path *time*) carrying dead weight. §1.2's fixed-16-byte-record-plus-pools
design is the mitigation, but it is a **design choice that must be enforced at
implementation time** — if an implementer takes the "it already works, why
change it" path from the proven spike code, the regression risk returns.
**Concrete gate:** measure net wall time (not just the decode-bucket
self-time) on the *full* canonical suite, including AoC-scale programs whose
code footprint is orders of magnitude larger than the four spike benches — a
footprint regression proportional to program size could be invisible on
`drop_cons`/`fib`/`short_lived`/`day03-p2` (all small, short-running) and only
show up on `day08`/`day09`-class inputs.

**2. Startup regression.** BV5's entire value proposition is a low,
deterministic startup floor (~10 ms measured, ROADMAP §7:490-493). Eager
prelude pre-decode (§2.2) is projected negligible but is exactly the kind of
"obviously cheap, actually wasn't" trap the flat-closures revert warns about.
**Gate:** measure `eu -e true` wall time before/after, on the release/CI-blob
path specifically (where BV5's win is realised today), as the *first*
benchmark run once this lands, not an afterthought.

**3. Interaction with BV3 (register frames) — sequencing matters, not just
scheduling.** ROADMAP §6.4 item 5 / §7 decision 4: BV3 should land **after**
lever (a), so "the frame-model change lands on the cheap dispatch loop, not
the expensive one" (review B §3, "BV3 … after pre-decode"). This design does
not touch `EnvironmentFrame`/`env.rs` at all — confirmed compatible — but the
implementer should resist the temptation to bundle BV3 into the same landing
window "since we're already touching dispatch"; the two are independently
gated and independently risky (BV3 is rated **high risk** in the ROADMAP,
shares CG's escape analysis, and the flat-closures cautionary tale applies to
*it* even more directly than to this lever).

**4. The `Op::Ann`-elimination (§3) is a genuine dispatch-loop deletion, not
just a retype — verify nothing depends on `Ann` being independently
steppable.** If any existing code (a debugger hook, a step-count metric, a
test asserting a specific tick count through an `Ann`-heavy path) counts
`Op::Ann` as its own tick, folding it away changes tick counts for those
specific programs (not a correctness bug, but a *visible* behaviour change
unlike everywhere else in this design, which is deliberately tick-identical).
**Gate:** grep the test suite and any tick-count-sensitive benchmarks for
programs whose current tick count depends on `Ann`-wrapper traversal before
landing Step B; if any exist, they need their expected counts updated with an
explicit, reviewed rationale (not silently).

**5. Side-pool growth during lazy user-code decode invalidates naive raw
pointers/slices held across a `push` (§4, last paragraph).** Any continuation
or intermediate structure that borrows a pool slice by reference rather than
by `(start, len)` index will dangle the moment the *same* pool grows from a
later lazy-decode `push` (Rust's `Vec` reallocates on growth). This is a
correctness risk, not a performance one, but it is easy to get wrong precisely
because the *prelude's* pools never grow post-load (making a raw-reference
implementation look correct in prelude-only testing) while *user-code* pools
do. **Gate:** any implementation review must specifically check that
continuations/frames hold indices, never slices, into the `offsets`/`refs`/
`headers`/`branches` pools.

---

## Appendix — file:line index (this document's citations, spike branch vs. master)

All line numbers below are from `master` @ `7a82ad2f` (this design note's base)
unless marked *(spike)*, which refers to `spike/predecode-ir` @ `37045ba7`
(draft PR #988) — the spike branch adds ~630 lines to `machine.rs` and 10 to
`mod.rs` on top of the same master baseline, so pre-spike line numbers below
are stable across both.

- `src/eval/bytecode/mod.rs:7` — `CodeRef = u32` (byte offset today).
- `src/eval/bytecode/opcode.rs:11-33` — `Op` enum (15 variants).
- `src/eval/bytecode/opcode.rs:36-56` — `Op::from_u8`.
- `src/eval/bytecode/opcode.rs:66-70` — `REF_L`/`REF_G`/`REF_V`.
- `src/eval/bytecode/opcode.rs:81` — `NO_BRANCH` sentinel.
- `src/eval/bytecode/opcode.rs:86-90` — `FORM_LAMBDA`/`FORM_THUNK`/`FORM_VALUE`.
- `src/eval/bytecode/program.rs:33-79` — `BytecodeProgram` fields.
- `src/eval/bytecode/program.rs:104-123` — `prepare_constants`.
- `src/eval/bytecode/program.rs:130-150` — `read_u8`/`read_u16`/`read_u32`.
- `src/eval/bytecode/closure.rs:16-18,179-198` — `CodeRef` inertness,
  `GcScannable for BcClosure`.
- `src/eval/bytecode/machine.rs:74-97` — `DecodedRef`, `read_ref`.
- `src/eval/bytecode/machine.rs:167-212` — `read_arg_offsets`,
  `read_form_header`, `read_branch_table`.
- `src/eval/bytecode/machine.rs:173-179` — `FormHeader`.
- `src/eval/bytecode/machine.rs:224-240` — `BifFrame`.
- `src/eval/bytecode/machine.rs:259` — `BcMachineState::constants`.
- `src/eval/bytecode/machine.rs:463-489` — `materialize_bif_args`/
  `recycle_bif_args`.
- `src/eval/bytecode/machine.rs:820-932` — `decode_cons`, `value_symbol`,
  `pair_key_symbol`, `bc_lookup_in_block`.
- `src/eval/bytecode/machine.rs:1585-1631` — `arg_ref`, `make_arg_array`,
  `bc_info`.
- `src/eval/bytecode/machine.rs:1716-2090` — `handle_op` (all opcode arms;
  per-arm line numbers in §1.2's table).
- `src/eval/bytecode/machine.rs:1896-1899` — `DirectApp`'s inline-smid comment
  ("Inline smid replaces a wrapping Ann").
- `src/eval/bytecode/machine.rs:2097-2117` — `step`.
- `src/eval/bytecode/machine.rs:2219-2253` — `BytecodeMachine::run`.
- `src/eval/bytecode/machine.rs:2284-2390` — `dispatch` (the `Bif` deferred
  dispatch tail).
- `src/common/sourcemap.rs:17` — `Smid(Option<NonZeroU32>)`.
- `src/driver/eval.rs:447-503` — bytecode engine selection, blob-vs-source
  program assembly, `BytecodeMachine::new` call site (the decode hook, §2.1).
- `src/eval/bytecode/encode.rs:596,663-703` — `encode`,
  `encode_overrides_and_root`.
- `ROADMAP.md:366-414` (§6.4) — BV0-BV4 phase sequencing.
- `ROADMAP.md:437-498` (§7, Pillar BV) — decisions 1-5, BV0 gate, realised
  0.12 notes.
- `ROADMAP.md:1024-1034` (§10.2) — flat-closures revert, the cautionary tale
  cited throughout §8.
- `docs/superpowers/specs/2026-07-12-bytecode-transition-review.md:187-209`
  (§6) — Phase-4 falsifiable gate thresholds.
- `docs/superpowers/specs/2026-07-12-bytecode-transition-review.md:214-238`
  (§7) — Tier-0/Tier-1 course, this design note is the output of Tier-1 item 7.
- `docs/superpowers/reports/2026-07-12-transition-review-B-architecture.md` —
  the architecture review this design implements (§1 per-op cost anatomy, §2a
  lever-a specification, §3 end-state).
- `docs/superpowers/reports/2026-07-13-predecode-spike.md` — the falsification
  spike this design extends; all "spike measured X" claims in this document
  cite that report's own numbers.
