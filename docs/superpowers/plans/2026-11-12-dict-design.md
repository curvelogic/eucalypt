# dict — design note

- **Status:** Draft for owner agreement (not yet agreed)
- **Date:** 2026-11-12
- **Bead:** eu-oaz72.21 (eu-oaz72.22, priority queue, folded in)
- **Plan:** `docs/superpowers/plans/2026-11-12-aoc-2026-preparation.md`
  (PR #1132) — §2 principles, §4 constraints, §6 "Dict"
- **Decision record:** ADR-002 in `docs/development/architectural-decisions.md`
  (PR #1132): every heap value's storage lives in the GC heap
- **Depends on:** eu-oaz72.5 (native storage moved into the GC heap) —
  implementation starts after it merges

This note fixes the design of `dict`, a persistent ordered dictionary whose
nodes are traced GC heap objects, and of sets rebuilt on the same nodes. It
covers what the bead's acceptance asks the note to cover — node layout, GC
tracing and evacuation, key equality and order, sorted iteration, rendering
and typing — plus the API and an implementation breakdown.

Choices the plan and the bead leave open are marked **OPEN** with a
recommendation; §11 lists them together for the owner.

## 1. Fixed by the plan and the owner decisions

- Name `dict`; constructors `dict.of(pairs)` with alias `dict.from-list`.
- A GC-native persistent **ordered tree**; no `im`/`rpds`/`imbl`, no `Rc`,
  no finalisers, no Rust-heap storage inside heap objects (ADR-002).
- Opt-in value type, independent of blocks; blocks stay cons-lists.
- Keys: numbers, strings, symbols (booleans and null once eu-oaz72.30
  lands) and lists of those, deep-forced. Total order across types:
  numbers < strings < symbols < lists (lexicographic).
- Values arbitrary and **lazy** (stored unforced), so self-referential memo
  tables work.
- Iteration sorted by key; `min`, `max`, `pop-min`, `pop-max`,
  `range(lo, hi)` make the dict the priority queue (key `[priority, seq]`).
- Typed `dict(k, v)` in the checker from the start.
- Renders as a mapping in key order with keys stringified; a clear error for
  list keys where a format cannot represent them.
- Sets rebuilt on the same nodes (a set is a dict with unit values), making
  `set.add`/`set.remove` O(log n), with `set.to-list` order unchanged.
- GC area: `EU_GC_VERIFY=2`, `EU_GC_POISON=1`, `EU_GC_STRESS=1`; two
  independent reviews; new intrinsics merge under the plan's §4 rule.

## 2. Tree structure

**OPEN 1 — which balanced tree.** The owner decision allows "B-tree or
balanced binary tree". **Recommendation: a weight-balanced binary tree**
(Adams' trees, the structure behind Haskell's `Data.Map`), for three
reasons:

1. **Allocation per update.** A persistent update copies the search path.
   For n = 1e5 a binary path is ~17 small nodes (~1.7 KB); a B-tree of
   order 16 copies ~4–5 nodes of ~0.9 KB each (~4 KB). The engine's cost
   is dominated by allocation and collection (plan §5), so the smaller
   copy wins on updates, which AoC workloads (counting, Dijkstra) are full
   of. Lookups favour the B-tree's locality, but both are O(log n).
2. **Split and join.** Weight-balanced trees have simple, well-known
   `split`/`join` algorithms, which give `range(lo, hi)` as an O(log n)
   persistent sub-dict, `pop-min`/`pop-max` in O(log n), and union,
   intersection, difference and `merge-with` in O(m log(n/m + 1)) — what
   the set operations need — without a separate bulk-rebuild path.
3. **Size in every node.** The subtree size is the balance invariant, so
   `size` is O(1) and rank queries (`nth`, used by `set.sample`) are
   O(log n) for free.

The B-tree alternative remains viable; if chosen, the node layout in §3
becomes a fixed-capacity entry array per node and §5's algorithms change,
but the GC, key, value, rendering and typing design is unchanged.

Balance parameters follow Straka's corrected values (Δ = 3, Γ = 2), which
are proven correct for insert, delete, split and join.

## 3. Node layout

One new heap object type, allocated with the ordinary allocator (nodes are
well under the large-object threshold):

```rust
/// A weight-balanced tree node. The empty tree is a null child pointer;
/// an empty dict is a `Native::Dict` holding `None`.
#[repr(C)]
pub struct DictNode {
    size: u64,                       // subtree entry count (balance + size)
    left: Option<RefPtr<DictNode>>,
    right: Option<RefPtr<DictNode>>,
    key: DictKey,
    value: BcValue,                  // lazy value; unit for sets (§6)
}

pub enum DictKey {
    Int(i64),
    UInt(u64),                       // only above i64::MAX
    Float(OrderedFloat<f64>),
    Str(RefPtr<HeapString>),
    Sym(SymbolId),
    List(RefPtr<DictKeyList>),       // deep-forced key list (§4)
    // Bool(bool) and Null once eu-oaz72.30 lands
}

/// Immutable list key: a length plus an inline array of `DictKey`
/// (variable-size object, one allocation, no separate backing buffer).
pub struct DictKeyList { len: u32, /* [DictKey; len] follows */ }
```

- `Native` gains `Dict(Option<RefPtr<DictNode>>)` (or a `RefPtr` to a small
  header object if a root-level field is wanted later). `Native::Set`
  becomes the same shape (§6).
- Nodes are immutable after construction: every operation builds new nodes
  and shares untouched subtrees. Nothing mutates a published node, so there
  is no write barrier question.
- `DictKeyList` is a single variable-size allocation (header + inline
  elements), so it needs no `OpaqueHeapBytes` backing-buffer handling.

## 4. Keys: equality, order and hashing

**Forcing.** Keys are deep-forced before they reach an intrinsic: the
prelude wrappers force them (the key is small and primitive by
construction), and the intrinsic rejects anything that is not a number,
string, symbol or list of those with a clear `ExecutionError` naming the
offending type. Converting a forced eucalypt value to a `DictKey` allocates
heap strings/key lists but never forces, so an intrinsic never re-enters
the machine mid-operation (§5).

**Numbers.** `Native::Num` is a `serde_json::Number` (i64, u64 or f64).
Keys keep integers exact (fixing for dicts the 2^53 collision eu-oaz72.10
reports for sets). Comparison:

- int vs int: exact (via i128 for i64/u64);
- float vs float: `OrderedFloat` total order — `-0.0` equals `0.0`, all
  NaNs are one key, greater than every other number;
- int vs float: exact numeric comparison (an integral float within range
  compares as an integer, otherwise by value; NaN greatest).

So `1` and `1.0` are the same key, matching `1 = 1.0` (`num_eq` in
`src/eval/stg/eq.rs`). On `assoc` of an equal key the stored key is kept
and only the value replaced (**OPEN 6**, minor: affects only whether `1.0`
or `1` renders).

**Strings** compare by UTF-8 bytes (= code point order), as sets do today.

**Symbols — OPEN 2.** Sets today order symbols by `SymbolId`, i.e.
interning order, not name. **Recommendation: order symbol keys by name**
(resolved through the symbol pool, which intrinsics already hold), so
iteration and rendering order are independent of what was interned first.
This changes `set.to-list` order for sets of several symbols, against the
acceptance's "to-list order unchanged"; it needs the owner's decision. The
alternative keeps interning order for both.

**Lists** compare lexicographically element by element (shorter prefix
first), with elements compared by the same total order, so `[2, 10]` <
`[10, 2]` and `[1]` < `[1, 0]`.

**Cross-type order:** numbers < strings < symbols < lists (owner decision);
**OPEN 3:** booleans and null are placed with eu-oaz72.30 —
recommendation: null < booleans (false < true) < numbers, so a missing
value sorts first.

**Hashing** is not needed: the ordered tree uses comparison only and
equality is `compare == Equal`. If a CHAMP follows (plan §6), its hash must
agree with this equality (hash an integral float as its integer, all NaNs
alike, `-0.0` as `0.0`).

## 5. Operations and GC safety

**Construction.** `dict.of(pairs)` collects forced keys, sorts (stable),
resolves duplicates last-wins (the same rule as block merge), and builds a
perfectly balanced tree bottom-up in O(n) after the O(n log n) sort — one
pass, no repeated `assoc` (the fix eu-oaz72.32 applies to `set.of`).

**Updates** (`assoc`, `dissoc`, `update`) path-copy in O(log n) with the
standard single/double rotations. `merge-with(f)` and `update(k, f, d)`
build the combined value as a lazy thunk with the existing
`apply2_thunk`/`apply1_thunk` ABI (as block `MERGEWITH` does), so no
intrinsic forces user code.

**GC safety across allocation.** Collection runs only at the machine's
safepoints, never inside an allocation call, and these intrinsics never
force; so a tree operation may hold raw node pointers across its own
allocations, as `SET.ADD` does today. Any future operation that must force
(e.g. a strict variant implemented natively) roots its live pointers with
`gc_root_push` before forcing — the existing pattern.

**Lazy values and memoisation.** A value passed to `assoc` arrives as an
env-slot reference. Storing a thunk closure by value in the node would lose
memoisation: each `get` would re-run it, which makes memo tables
exponential. So:

- a value already in WHNF (a native, a constructor, a lambda) is stored
  directly as a `BcValue`;
- a thunk is wrapped in a **value cell**: a one-slot env frame holding the
  thunk, plus a fixed "enter slot 0" code template (allocated once, like
  the `apply1_thunk` templates). Entering the cell goes through the normal
  black-hole + `Update` path on slot 0, so the first `get` memoises it, all
  later `get`s — from any version of the dict sharing the cell — see the
  value, and a cyclic self-reference is reported as a black hole, not a
  hang.

**OPEN 7 — strict variants.** Lazy values let counting loops build long
`+` thunk chains (`update(k, (+ n))` repeated across a step). The
acceptance's 75-blink stone-counting benchmark is the test of this.
**Recommendation:** keep values lazy as decided, and add
`dict.update-strict` and `dict.merge-with-strict` (WHNF-force the combined
value; prelude `seq`-based where possible), named like `foldl-strict`
(plan §6). Decide after the benchmark PR measures the lazy versions.

## 6. Sets on the same nodes

`Native::Set` holds an `Option<RefPtr<DictNode>>` whose values are unit
(a shared static `BcValue`, so no per-element value allocation). All set
intrinsics map onto tree operations: `add`/`remove` O(log n),
`contains?` O(log n), `size` O(1), `union`/`intersect`/`diff` by
split/join, `to-list` an in-order walk, `sample` by rank. `set.of`
(eu-oaz72.32) uses the bulk build. Set elements gain list elements for
free (a deliberate widening; **OPEN 8** — recommendation: allow, it is the
same key type and AoC wants `[x, y]` sets).

eu-oaz72.5 first moves set storage into a sorted heap array; this work
replaces that array with tree nodes. The conversion helpers between
`Primitive` and keys become `DictKey` helpers shared by both types.

## 7. API

Collection argument last, for pipelines (`d dict.get(k)`), matching `set.*`.
Names from the owner decision; `range`/`min`/`max`/`pop-*` from the plan.

| Function | Result | Cost |
|---|---|---|
| `dict.of(pairs)`, `dict.from-list` | dict from `[k, v]` pairs, last wins | O(n log n) |
| `dict.empty` | the empty dict | O(1) |
| `dict.to-list(d)` | sorted `[k, v]` pairs | O(n) |
| `dict.keys(d)`, `dict.values(d)` | in key order | O(n) |
| `dict.get(k, d)` | value; error naming the key if absent | O(log n) |
| `dict.get-or(k, default, d)` | value or `default` | O(log n) |
| `dict.assoc(k, v, d)`, `dict.dissoc(k, d)` | new dict | O(log n) |
| `dict.update(k, f, default, d)` | `assoc(k, f(get-or(k, default)))` | O(log n) |
| `dict.contains?(k, d)`, `dict.size(d)`, `dict.empty?(d)` | | O(log n), O(1) |
| `dict.merge-with(f, a, b)` | union; `f(va, vb)` on collision | split/join |
| `dict.min(d)`, `dict.max(d)` | `[k, v]` | O(log n) |
| `dict.pop-min(d)`, `dict.pop-max(d)` | `[k, v, rest]` | O(log n) |
| `dict.range(lo, hi, d)` | sub-dict with `lo ≤ k < hi` | O(log n) |
| `dict.from-block(b)`, `dict.to-block(d)` | conversion | O(n log n), O(n) |

**OPEN 4 — edge behaviour.** Recommendations:

- `range` is half-open `[lo, hi)`, like `range(b, e)` for lists; a
  `range-incl` is not added unless asked.
- `min`/`max`/`pop-min`/`pop-max` on an empty dict raise a clear error;
  callers test `empty?` (Dijkstra loops do anyway). No `-or` variants yet.
- `dict.from-block` keys are the block's symbols; `dict.to-block` accepts
  string and symbol keys, stringifies number keys as they render, and
  errors on list keys.
- `update` takes an explicit default (the owner decision lists
  `update(k, f, d)`; the default is needed for absent keys and makes
  counting one call: `dict.update(k, (+ 1), 0)`).

**Priority queue (eu-oaz72.22).** No `pq.*` names in the prelude
(plan §2, principle 3: AoC helpers live in the external helper library). The cookbook
documents the pattern: key `[priority, node]` (unique for Dijkstra, so no
sequence number is needed; `[priority, seq, x]` otherwise), `pop-min` to
take the next item, `assoc` to push, `dissoc` of the old key for
decrease-key. eu-oaz72.22 closes with a 141×141 Dijkstra example.

**Out of scope here:** structural `=` on dicts (stays reference identity,
as for sets today — documented), insertion-order helpers (owner decision 4:
only if needed), and a parametric `set(k)` type.

## 8. Rendering

The render wrapper (`src/eval/stg/render.rs`) dispatches on constructor
tags and sends natives to `EmitNative`, which emits sets and vecs strictly.
A dict's values are lazy and arbitrary, so a dict renders through the block
path instead: the native branch recognises `Native::Dict` and converts it
(one intrinsic) into a block of `[stringified-key, value]` pairs in key
order, which the existing block renderer emits as a mapping, forcing and
rendering each value as for any block. Order is therefore deterministic in
every format. Dicts are runtime values only; nothing reaches the prelude blob.

- Number keys stringify as they render as values (`1`, `2.5`); string keys
  as-is; symbol keys as their name.
- **OPEN 5 — list keys and collisions.** Recommendation: a list key is an
  error in every format in this work (the emitter API takes scalar keys;
  YAML complex keys can follow on request), naming the key; two keys that
  stringify to the same text (`1` and `"1"`) are also an error, since JSON,
  TOML and YAML all reject duplicate keys.
- `printf`, `assert` messages and `describe_native` print
  `<dict:N>` (size), like `<set>` today.
- Parallel serialisation (`par-*`): a dict is "opaque value — not
  serialisable", as sets are today; a follow-up can serialise it.

## 9. Typing

**OPEN 9 — name clash with `Dict(T)`.** The checker already has `Dict(T)`,
a homogeneous *block* (`src/core/typecheck/types.rs`, documented in
`docs/guide/type-checking.md`). A lowercase `dict(k, v)` beside it invites
confusion. **Recommendation:** add `dict(k, v)` as specified, and rename the
block type to `Block(T)` with `Dict(T)` kept as a deprecated alias for one
release (a separate small PR in the docs/typecheck area). The alternative
keeps both names and documents the difference.

Design of `dict(k, v)`:

- A new constructor of kind `* → * → *`, written `dict(k, v)`; bare `dict`
  means `dict(any, any)`. `IntrinsicType::Dict` for runtime type errors.
- Covariant in both parameters (persistent, so no mutation hazard), like
  `[a]`.
- Prelude signatures, e.g. `get: k → dict(k, v) → v`,
  `assoc: k → v → dict(k, v) → dict(k, v)`,
  `to-list: dict(k, v) → [(k, v)]`, `pop-min: dict(k, v) → [any]`
  (the checker has no type for a 3-element heterogeneous result beyond a
  prefix-list; `[k, v, dict(k, v)…]` is not expressible, so `[any]`).
- `set` stays the nullary `set` type.

## 10. Implementation breakdown

Each PR is GC-area (two independent reviews, the three GC verification
modes); intrinsic-adding PRs merge under the plan's §4 rule. Lane: dict.

1. **Keys and nodes** (memory only): `DictKey`, `DictKeyList`, `DictNode`
   with `GcScannable` (scan pushes children, key strings/lists and the
   value; `scan_and_update` rewrites each forwarded pointer), key compare,
   weight-balanced insert/delete/split/join/bulk-build, Rust unit tests
   including evacuation under `EU_GC_VERIFY=2`. No user-visible change.
2. **`Native::Dict` and the core intrinsics:** `of`, `to-list`, `get`,
   `get-or`, `assoc`, `dissoc`, `contains?`, `size`, value cells, the tag
   checks, `describe_native`/printf/assert/serialise arms, prelude `dict`
   namespace, harness tests (fault-injection verified), 1e5 insert+lookup
   measurement.
3. **Ordered operations:** `min`, `max`, `pop-min`, `pop-max`, `range`,
   `keys`, `values`, `update`, `merge-with`, `from-block`/`to-block`; the
   Dijkstra example (closes eu-oaz72.22) and the 75-blink counting
   benchmark; strict variants if OPEN 7 says so.
4. **Rendering and typing:** dict render via the block path in all formats,
   list-key and collision errors (error corpus), `dict(k, v)` in the
   checker and prelude signatures, docs (prelude reference regenerated,
   guide section, cookbook). Plus the `Dict(T)` rename PR if OPEN 9 agrees.
5. **Sets on dict nodes:** `Native::Set` onto `DictNode`, all set
   intrinsics rewritten, existing set tests passing with order unchanged
   (subject to OPEN 2), 1e5 `set.add` measurement, GC-churn benches
   002, 005–009 not regressed under the protocol.

The editor name lists are not touched (plan §4); the grammar lanes batch
the new `dict.*` names.

## 11. Open choices for the owner

| # | Choice | Recommendation |
|---|---|---|
| 1 | Tree: weight-balanced binary vs B-tree | weight-balanced binary (less copying, split/join, O(1) size) |
| 2 | Symbol key order | by name; changes `set.to-list` order for multi-symbol sets |
| 3 | Bool/null placement | null < false < true < numbers < … |
| 4 | Edge behaviour | half-open `range`; error on empty `min`/`pop-*`; `update` takes a default; block conversion rules as §7 |
| 5 | List keys / key collisions on render | error in all formats for now |
| 6 | Key kept on re-`assoc` of an equal key (`1` vs `1.0`) | keep the existing key |
| 7 | Strict variants | add `update-strict`, `merge-with-strict` after the benchmark |
| 8 | List elements in sets | allow |
| 9 | Clash with block type `Dict(T)` | rename block type to `Block(T)`, deprecate `Dict(T)` |
