# dict — design note

- **Status:** Agreed 2026-11-12 (owner accepted every §11 recommendation)
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

Balance parameters are (Δ, Γ) = (3, 2), the only integer pair proved to
keep the balance invariant under insert and delete (Hirai and Yamamoto,
"Balancing weight-balanced trees", JFP 2011; Straka, "Adams' trees
revisited", TFP 2011) and the pair `Data.Map` uses. Split, join and the
join-based set operations follow Blelloch, Ferizovic and Sun, "Just join
for parallel ordered sets" (SPAA 2016), which proves join correct for
weight-balanced trees.

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
  elements), so it needs no `OpaqueHeapBytes` backing-buffer handling. It
  is the first variable-size *typed* heap object (strings and `Array`
  backings are raw bytes), so PR 1 adds an allocation constructor that
  sizes the header for `len` inline elements.

### GC tracing and evacuation

Sets, vecs and arrays are leaves today: `scan_native` in
`src/eval/bytecode/closure.rs` and the `Ref::V` arms in
`src/eval/bytecode/machine.rs` (around lines 490–530) only *mark* them and
never push them for scanning. A dict arm copied from the set arm would
leave every node below the root unscanned and stale after evacuation. So:

- **Entry points.** `Native::Dict` (and `Native::Set` once on nodes) is
  handled like `Native::Str`: mark the root and, if newly marked, push it
  as `ScanPtr::from_non_null` so its `scan` runs; `update_native` and the
  `Ref::V` update arm rewrite the root pointer from `forwarded_to`. Both
  sites (closure env values and machine `Ref::V` constants) get the arm.
- **`DictNode::scan`** marks and pushes `left`, `right`, the key's heap
  pointer (`Str` string, which then marks its byte backing as
  `HeapString::scan` does; `List` key list) and the value (`BcValue`:
  closure env or native, via the existing `scan_native`/closure path).
- **`DictNode::scan_and_update`** rewrites each of those pointers from
  `forwarded_to`.
- **`DictKeyList::scan` / `scan_and_update`** do the same for each inline
  element's `Str`/`List` pointer.
- Under `EU_GC_STRESS=1` every collection evacuates, so PR 1's unit tests
  build trees, force a stress collection and walk the result under
  `EU_GC_VERIFY=2` and `EU_GC_POISON=1`.

## 4. Keys: equality, order and hashing

**Forcing.** Keys are deep-forced before they reach an intrinsic: the
prelude wrappers force them (the key is small and primitive by
construction), and the intrinsic rejects anything that is not a number,
string, symbol or list of those with a clear `ExecutionError` naming the
offending type. Converting a forced eucalypt value to a `DictKey` allocates
heap strings/key lists but never forces, so an intrinsic never re-enters
the machine mid-operation (§5).

**OPEN 10 — keys hold heap references.** Owner decision 2 says forced keys
"hold no heap references"; the bead's constraint 5 says strings are held as
GC heap strings, not Rust `String`s (ADR-002 rules the latter out). Both
cannot hold literally. This note reads decision 2 as "no references to
lazy or user structure": a key holds only immutable heap strings and key
lists, which `DictNode::scan` traces (§3). **Recommendation:** accept that
reading. The alternative — interning every string key as a symbol-like id
— avoids the references but grows a never-collected pool.

**Numbers.** `Native::Num` is a `serde_json::Number` (i64, u64 or f64).
Keys keep integers exact (fixing for dicts the 2^53 collision eu-oaz72.10
reports for sets). Comparison:

- int vs int: exact (via i128 for i64/u64);
- float vs float: `OrderedFloat` total order — `-0.0` equals `0.0`, all
  NaNs are one key, greater than every other number (a `serde_json::Number`
  cannot hold NaN or infinity today, so the NaN rule only fixes the order
  should a non-finite number ever reach a key);
- int vs float: exact numeric comparison (an integral float within range
  compares as an integer, otherwise by value; NaN greatest).

So `1` and `1.0` are the same key, matching `1 = 1.0` (`num_eq` in
`src/eval/stg/eq.rs`).

**OPEN 11 — key equality is not exactly `=`.** It differs from `num_eq` in
two places: `num_eq` compares an integer with a float through f64, so
`9007199254740993 = 9007199254740992.0` is true but they are different
keys; and `NaN = NaN` is false but NaN is one key (a key must equal itself
for lookup to work). **Recommendation:** accept both differences and
document them; the exact-integer rule is what eu-oaz72.10 asks for.

On `assoc` of an equal key the stored key is kept
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

**Hashing** is not needed (owner decision 2 says keys are "hashed
structurally", written before the ordered-tree decision; under OPEN 1
either tree needs only comparison): the ordered tree uses comparison only and
equality is `compare == Equal`. If a CHAMP follows (plan §6), its hash must
agree with this equality (hash an integral float as its integer, all NaNs
alike, `-0.0` as `0.0`).

## 5. Operations and GC safety

**Construction.** `dict.of(pairs)` forces the list spine, each pair and
each key — never the values — then sorts (stable),
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
exponential. Memoisation in the engine happens only in `enter_local` /
`enter_global`, which black-hole the slot and push `Update` **only for a
closure whose `update()` flag is set**. The closures `apply1_thunk` and
`apply2_thunk` build (`BcClosure::new`) are *not* updatable — their callers
store them in a cons cell or block slot whose own entry memoises — so
storing them, or a cell around them, unchanged would re-run `f` on every
`get`. The rule is therefore:

- **Which values get a cell:** every stored value that is not already in
  WHNF — any `BcValue::Closure` of arity 0 that is not a data constructor,
  updatable or not, which covers user thunks passed to `assoc`/`of` and the
  `f(old)` / `f(a, b)` applications `update` and `merge-with` build. A
  value already in WHNF (a native, a constructor, a partial application or
  lambda) is stored directly as a `BcValue`, with no cell.
- **The cell:** a one-slot env frame holding the value, and in the node a
  closure over that frame of a fixed "enter local 0" template (pre-encoded
  once, like the `apply1_thunk` template). The *slot-0 value* is made
  updatable: the thunk is stored with the update flag set (for the
  `apply*_thunk` results, a dict-specific variant of those builders sets
  it). Entering the node's closure does `enter_local(cell, 0)`, which
  black-holes slot 0 and pushes `Update { cell, 0 }`; the first `get`
  memoises, later `get`s — from any dict version sharing the cell — read
  the value, and a cyclic self-reference is reported as a black hole.
- PR 2 tests this directly: a value whose evaluation increments an
  observable counter (via a debug/trace intrinsic) is fetched twice, and a
  memoised Fibonacci over a self-referential `dict.of` runs in linear time.

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

eu-oaz72.5 first moves set storage into the GC heap (its fix design picks
the representation); this work replaces whatever it lands with tree nodes.
The conversion helpers between `Primitive` and keys become `DictKey`
helpers shared by both types.

**Visible change to set contents.** Sets store numbers as f64 today
(`Primitive::from_number`), so `set.to-list` returns `9007199254740992`
for an added `9007199254740993`. On `DictKey`
numbers stay exact. This fixes eu-oaz72.10 for sets but changes what
`set.to-list` returns for such elements — and, since every set number is
an f64 today, possibly how ordinary integers come back (PR 5 checks this
against the existing set tests), beyond the order question of
OPEN 2; the owner should confirm it (recommendation: accept, and close
eu-oaz72.10's set half with this work).

## 7. API

Collection argument last, for pipelines (`d dict.get(k)`), matching `set.*`.
Names from the owner decision; `range`/`min`/`max`/`pop-*` from the plan.
Rows marked † go beyond the owner's list or fix a result shape the plan
leaves open (OPEN 4).

| Function | Result | Cost |
|---|---|---|
| `dict.of(pairs)`, `dict.from-list` | dict from `[k, v]` pairs, last wins | O(n log n) |
| `dict.empty` † | the empty dict | O(1) |
| `dict.to-list(d)` | sorted `[k, v]` pairs | O(n) |
| `dict.keys(d)`, `dict.values(d)` | in key order | O(n) |
| `dict.get(k, d)` | value; † error naming the key if absent | O(log n) |
| `dict.get-or(k, default, d)` | value or `default` | O(log n) |
| `dict.assoc(k, v, d)`, `dict.dissoc(k, d)` | new dict | O(log n) |
| `dict.update(k, f, default, d)` † | `assoc(k, f(get-or(k, default)))` | O(log n) |
| `dict.contains?(k, d)`, `dict.size(d)`, `dict.empty?(d)` † | | O(log n), O(1) |
| `dict.merge-with(f, a, b)` | union; † `f(va, vb)` on collision (`a`'s value first, as block `merge-with`) | split/join |
| `dict.min(d)`, `dict.max(d)` | † `[k, v]` | O(log n) |
| `dict.pop-min(d)`, `dict.pop-max(d)` | † `[k, v, rest]` | O(log n) |
| `dict.range(lo, hi, d)` | sub-dict with `lo ≤ k < hi` | O(log n) |
| `dict.from-block(b)`, `dict.to-block(d)` | conversion | O(n log n), O(n) |

**OPEN 4 — API additions and edge behaviour.** Recommendations:

- Add `dict.empty` and `dict.empty?` (sets have `∅` and `set.empty?`; a
  search loop needs both).
- `get` on a missing key is an error naming the key (as block lookup of a
  missing key is); `get-or` is the total form.
- `min`/`max` return `[k, v]` and `pop-min`/`pop-max` return
  `[k, v, rest]`: flat lists destructure in one pattern, matching the shape
  eu-oaz72.22's acceptance proposes for `pq.pop`.

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

**OPEN 12 — `=` on dicts.** `=` on two sets today is pointer identity (the
`(l, r) => l == r` fallback in `eq.rs`), so two equal sets built separately
are not `=`. **Recommendation:** dicts behave the same in this work,
documented, since structural equality would have to force and compare lazy
values; sets on nodes could gain cheap structural `=` (keys only) as a
follow-up decision.

**Out of scope here:** insertion-order helpers (owner decision 4: only if
needed) and a parametric `set(k)` type.

## 8. Rendering

The render wrapper (`src/eval/stg/render.rs`) dispatches on constructor
tags and sends natives to `EmitNative`, which emits sets and vecs strictly.
A dict's values are lazy and arbitrary, so `EmitNative` cannot emit them
(it would have to force user code). A dict renders through the block path
instead. Today the wrapper's final fallback is `EmitNative(x)`
(`render.rs`, the native arm). That fallback becomes a case on a new
intrinsic `DICT.RENDER_VIEW(x)`, which returns a `Block` of
`[stringified-key, value]` pairs in key order when `x` is a dict (values
copied as stored, unforced) and `x` unchanged otherwise; the `Block` arm
re-enters the existing block render branch, which emits a mapping, forcing
and rendering each value as for any block, and every other result goes to
`EmitNative` as before. Non-dict values pay one intrinsic call on the
native path only; PR 4 measures that against the render-heavy benches. Order is therefore deterministic in
every format. Dicts are runtime values only; nothing reaches the prelude blob.

- Number keys stringify as they render as values (`1`, `2.5`); string keys
  as-is; symbol keys as their name.
- **OPEN 5 — list keys and collisions.** Recommendation: a list key is an
  error in every format in this work (the emitter API takes scalar keys;
  YAML complex keys can follow on request), naming the key; two keys that
  stringify to the same text (`1` and `"1"`) are also an error, since JSON,
  TOML and YAML all reject duplicate keys.
- The `Display` impl for `Native` and `assert` messages print `<dict:N>`
  (size), like `<set>` today; `printf` errors "cannot format dict" as it
  does for sets; `describe_native` (no set arm today) gets none either.
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
  `to-list: dict(k, v) → [(k, v)]`, `min: dict(k, v) → (k, v)`,
  `pop-min: dict(k, v) → (k, v, dict(k, v))` — fixed tuples, which the
  checker already parses (`Type::Tuple`).
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
   `get-or`, `assoc`, `dissoc`, `contains?`, `size`, value cells, the
   GC entry points (`scan_native`/`update_native` and the machine's
   `Ref::V` arms, §3), the tag checks, `Display`/printf/assert/serialise arms, prelude `dict`
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
| 10 | Keys hold heap strings/key lists vs "no heap references" | accept: no references to lazy or user structure |
| 11 | Key equality differs from `=` above 2^53 (and NaN, should it ever occur) | accept and document |
| 12 | `=` on dicts | reference identity, as sets today |
| — | Exact numbers in sets change `set.to-list` for >2^53 (§6) | accept; closes eu-oaz72.10's set half |
| — | Additions beyond the owner's list: `empty`, `empty?`, `update` default, result shapes (OPEN 4) | as §7 |

## 12. Implementation notes (PR 2)

Recorded as built; none changes an agreed choice.

- **Value cells.** The cell template is a new fixed fixture,
  `BytecodeProgram::cell_template` (`Atom(L0)`), beside the apply and
  producer templates; the blob wire format moves to v9.
  `IntrinsicMachine::memo_cell` prepares every stored value.
- **Arguments arrive as aliases.** An intrinsic's lazy argument is an
  `OP_ATOM` alias closure over the caller's frame. Storing it would keep
  that frame alive, and in a loop that frame holds the previous version of
  the dict being updated, so every version would be retained (measured:
  5,414 peak heap blocks for 50,000 chained updates, against 1,113 with the
  fix; measured-single). `memo_cell` therefore follows aliases: an
  evaluated target is stored directly (a boxed scalar is rebuilt over a
  fresh frame, since the box also closes over its binding frame); an
  unevaluated one is stored as a cell around the original alias, so it
  memoises in the slot it names and the cell drops the alias once evaluated.
- **Key forcing** happens in the wrappers: `seqKey` deep-forces a key and
  `seqDictPairs` prepares `dict.of`'s list, rebuilding each well-formed pair
  as `BlockPair(forced key, value)`. List keys may nest at most 32 deep
  (`BadDictKey` beyond).
- **Rendering** is PR 4; until then a dict reaching output raises
  `DictNotRenderable`, pointing at `dict.to-list`. Dict arguments are typed
  `any` in prelude signatures until PR 4 adds `dict(k, v)`.
