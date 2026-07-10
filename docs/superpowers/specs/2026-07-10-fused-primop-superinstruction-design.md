# Fused-primop superinstruction — design note (eu-9mvh, lever a)

- **Status:** Draft — for owner sign-off before any implementation.
- **Date:** 2026-07-10
- **Author:** Furnace
- **Gate:** design-note-first, per owner decision after lever (c) (PR #978, closed
  unmerged — sound but inert). No VM/encoder/compiler code changes accompany
  this note.
- **Depends on:** W0 spike (`docs/superpowers/reports/2026-07-09-bytecode-decode-cost-spike.md`),
  the eu-9mvh lever-(c) postmortem in `docs/superpowers/specs/` history / PR
  #978's description, and the code paths cited by `path:line` throughout.

---

## 1. Problem recap

`eu dump stg tests/harness/bench/001_naive_fib.eu` shows that every strict
binary primop call (`<=`, `-`, `+`, `*`, `/`, `÷`, `%`, `>`, `>=`, `<`, …)
compiles to a fully-expanded, uniform-branch `Case`-of-`Case` tree whose only
purpose is to force both operands to WHNF and unbox them to `Native` values
before a single, internally-polymorphic intrinsic call
(`src/eval/stg/arith.rs:452` `binary_wrapper`). Concretely, for a two-argument
intrinsic at global index `idx`, `binary_wrapper(idx)` builds (annotated with
the exact nesting the W0 report's fib dump shows):

```
case x of                       # outer: is x boxed or native?
  BoxedNumber → force(unboxed-x, ...)
  BoxedString → force(unboxed-x, ...)
  BoxedSymbol → force(unboxed-x, ...)
  BoxedZdt    → force(unboxed-x, ...)
  _(native x)  → case y of      # x already native — check y
                   BoxedNumber → force(unboxed-y, BIF(x, unboxed-y))
                   ... (3 more) ...
                   _(native y)  → BIF(x, y)          # fast path
  # each boxed-x branch continues, after unboxing x, with:
  case y of
    BoxedNumber → force(unboxed-y, BIF(unboxed-x, unboxed-y))
    ... (3 more) ...
    _(native y) → force(unboxed-x-again?, BIF(unboxed-x, y))
```

(`binary_wrapper`, `src/eval/stg/arith.rs:452-501`, precisely matches the W0
report's §3.1 dump — the outer 5-way dispatch on `x`'s tag, an inner 5-way
dispatch on `y`'s tag for the boxed-`x` path, and a cheaper native/native fast
path when both are already unboxed.) This is why `LTE(` occurs **25 times**
and `SUB(`/`ADD(` occur **50/25 times** respectively in a single `eu dump stg`
of `001_naive_fib.eu` (§3.1 of the W0 report) — every one of the 25 leaves
calls the textually identical intrinsic; only the path taken to force+unbox
the two operands differs.

The W0 report's per-symbol CPU profile puts the combined
`handle_op`+`dispatch`+decode (`read_ref`/`read_arg_offsets`/`read_form_header`/
`read_let`/`step`)+`enter_local` bucket — the cost of *walking the bytecode
instruction stream itself* — at **45.88–64.63% of active CPU across all four
profiled programs** (fib/day03-p2/day09-p1/day08-p1), the single largest
bucket in every case, dwarfing both system malloc (1.9–6.2%) and the managed
heap allocator (5.85–11.34%, itself substantially the `Op::Case` branch-table
`Array` build). Every primop call currently walks through up to **four**
nested `Op::Case` evaluations (two levels of tag-dispatch for each of two
operands) plus a **fifth**, separate `Op::Bif` deferred-dispatch tail
(`src/eval/bytecode/machine.rs:1694-1706`, `2017-2083`) — i.e. roughly five
opcode decode-and-dispatch cycles, each with its own branch-table decode
and (for `Op::Case`) a heap-allocated `Array<Option<CodeRef>>`, to execute
one `+`.

**Why lever (c) (`Op::Seq` for uniform-branch `Case`) cannot capture this
pattern**, and why that finding is directly load-bearing for lever (a)'s
design: `Op::Case`'s fallback/matched-branch paths **bind** the forced or
extracted value into a *fresh* environment slot (`env_from_data_args`,
`src/eval/bytecode/machine.rs:669-713`; `from_value`, used by both
`return_data`'s and `return_native`'s fallback arms). `Op::Seq` deliberately
does **not** — it forces the scrutinee to WHNF purely for its side effect
(thunk memoisation) and continues in the *same*, unextended environment
(`src/eval/bytecode/machine.rs:1548-1557`, `987-1003`). Every leaf of
`binary_wrapper`'s tree calls `BIF(x, y)` — it reads the forced/unboxed
*value*, which is exactly the binding `Op::Seq` omits. A sound version of the
lever-(c) rule must therefore always decline on this pattern (verified
empirically: zero `Op::Seq` downgrades fire anywhere in `binary_wrapper`'s
output — see PR #978). Reaching this hot path requires an opcode that
*does* bind the forced value — which is what this design proposes, fused
directly into a single decode-and-dispatch cycle instead of stacking two.

---

## 2. Proposed fused opcode(s)

### 2.1 Shape: one generic `Op::FusedPrimop`, not one opcode per primop

**Recommendation: a single new opcode, `Op::FusedPrimop`, parameterised by a
`primop_id: u8` operand that indexes the *existing* intrinsic table**
(`src/eval/intrinsics.rs:1044-1058`, `INTRINSICS: [Intrinsic; 195]`), rather
than a dedicated opcode per primop (`Op::FusedAdd`, `Op::FusedLte`, …).

**Encoding** (mirrors `Op::Seq`'s and `Op::Case`'s scrutinee convention —
operands are pre-emitted `Op::Atom` nodes referenced by offset, per the
file's own "every argument operand... is pre-emitted as its own `OP_ATOM`
node" convention, `src/eval/bytecode/encode.rs:7-16`):

```
[Op::FusedPrimop][primop_id: u8][arg1_off: u32][arg2_off: u32]
```

10 bytes total (1 opcode + 1 id + 2×4 offsets) — cheaper to decode than even
a *single* `Op::Case` (`min_tag`+`len`+`len`×`u32` branch table,
`src/eval/bytecode/machine.rs:1519-1546`), and this replaces up to **five**
existing decode-and-dispatch cycles.

**Trade-off — why generic-with-id wins over per-primop opcodes:**

| | Generic `FusedPrimop(id, a, b)` | One opcode per primop |
|---|---|---|
| Opcode space | +1 (`0x0F`) | +~10 (arith: ADD/SUB/MUL/DIV/MOD; cmp: GT/GTE/LT/LTE, + any later additions) |
| `encode.rs` changes | One global-form interception point (§4) keyed by intrinsic index — data-driven | One `match` arm per primop at the interception point |
| `machine.rs` dispatch | One opcode handler; a *data-driven* call into `self.intrinsics[primop_id]` — identical to how `Op::Bif` already dispatches (`machine.rs:2032` `let bif = self.intrinsics[idx as usize];`) | One handler per primop, OR one handler with an internal `match primop_id` — no real saving over the generic form, just opcode-table churn |
| Extending to more primops later | Add the index to a whitelist constant; zero opcode/VM changes | New opcode, new `Op::from_u8` arm, new encoder case, new dispatch arm — per primop |
| Type-checking/error-path reuse | Trivial — see §5, delegates to the existing `StgIntrinsic::execute` for the *actual* arithmetic/comparison and all its error paths | Same delegation still needed underneath, so this buys nothing |

The per-primop-opcode's only theoretical edge — devirtualising the intrinsic
call to skip the `dyn StgIntrinsic` vtable indirection — is not worth the
opcode-space churn and duplicated encoder logic: the vtable call is already
one indirect call, no different in kind from what `Op::Bif`'s existing
deferred-dispatch tail does today, and is not where the profiled cost sits
(the W0 report's dominant bucket is decode/dispatch and branch-table
allocation, not the intrinsic call itself, whose `execute` functions show up
individually at ≤2–3% each — see §2.3 of the W0 report). **Recommendation:
generic `Op::FusedPrimop(primop_id, arg1, arg2)`.**

### 2.2 What the opcode actually does, at a glance

Force `arg1` to WHNF; if it (or, if boxed, its extracted native field) is a
`Native`, capture it — otherwise capture whatever it resolved to as-is (§5.5).
Do the same for `arg2`. Bind both resolved values into a small scratch
environment and **defer to the existing, completely unmodified `Op::Bif`
dispatch tail** (`machine.rs:2017-2083`) via `state.pending_bif`/
`pending_bif_args` — exactly the mechanism `Op::Bif` itself already uses,
not a new finalisation path (§5 explains why: the intrinsic table and
`execute()`'s full context are only reachable from `dispatch()`, not from
the operand-resolution code in `return_data`/`return_native`). **The fused
opcode's entire job is to replace `binary_wrapper`'s expensive
force-dispatch tree with a cheap, VM-native force-and-bind — it does not
reimplement, or even touch, any arithmetic, comparison, type-checking,
error-raising, or intrinsic-dispatch logic at all.** This is the single
most important risk-reduction decision in this design (§6, §9).

---

## 3. Primop scope

### 3.1 First cut — the `binary_wrapper` family

All eight intrinsics that use `arithmetic_wrapper`/`comparison_wrapper` (both
thin aliases for `binary_wrapper`, `src/eval/stg/arith.rs:503-520`) and are
confirmed, by direct code reference, to share the *exact* nested-`Case` shape
analysed in §1:

| Op | Intrinsic | `src/eval/stg/arith.rs` | Domain |
|---|---|---|---|
| `+` | `Add` | `:77-141` | numeric (+ `NdArray`, see §3.3) |
| `-` | `Sub` | `:144-208` | numeric (+ `NdArray`) |
| `*` | `Mul` | `:211-` | numeric (+ `NdArray`) |
| `/` | `Div` | `:278-350` | numeric, **floor** division, `DivisionByZero` on zero divisor |
| `>` | `Gt` | `:577-600` | `Num`/`Str`/`Sym`/`Zdt` (ordered, not numeric-only) |
| `>=` | `Gte` | `:603-626` | ″ |
| `<` | `Lt` | `:629-` | ″ |
| `<=` | `Lte` | `:655-` | ″ |

`÷` (`PreciseDiv`, `:826-873`) is the **exact** counterpart to `/`'s floor
division — confirm before implementation whether it also uses
`binary_wrapper` (grep shows no `fn wrapper` override in the earlier survey
for `Pow`/`PreciseDiv`/`Quot`/`Rem`, meaning they fall through to the
*generic* `wrap()` — see §3.2); if so it is trivially in scope alongside
`/`, sharing the same fused-op machinery and differing only in which
`execute()` runs.

### 3.2 Deferred from the first cut

- **`%` (`Mod`, `:353-411`) and `÷`/`POW`/`QUOT`/`REM`/bitwise ops.** These do
  **not** override `wrapper()` at all (confirmed: no `fn wrapper` in `Mod`'s
  `impl StgIntrinsic` block, `arith.rs:355-411`) — they fall back to the
  generic `wrap()` (`src/eval/stg/wrap.rs:27`), which is a **different**,
  more general STG shape: a linear chain of `force`/`unbox_num` pairs driven
  by the intrinsic's demand signature (`strict_indices_for`), not
  `binary_wrapper`'s hand-tuned 5×5 dispatch tree with a native/native fast
  path. It is very likely fusible with the *same* opcode and dispatch
  algorithm (§5) — both shapes reduce to "force + unbox two Number operands,
  then call `execute`" — but the encoder's interception point (§4) must be
  verified against `wrap()`'s actual output before including these in the
  whitelist. **Recommendation: land the `binary_wrapper` family first
  (smallest, most-profiled, and best-understood surface), then extend the
  whitelist to the `wrap()`-shaped numeric primops (`%`, `÷`, `POW`, `QUOT`,
  `REM`) as a fast follow, each addition being a one-line whitelist change
  plus a targeted differential-harness case if the shape genuinely matches.**
- **`=` (`Eq`, `src/eval/stg/eq.rs`).** Confirmed **out of scope**: `Eq`'s own
  doc comment states it "recurses through data structures lazily and calls
  the intrinsic only for natives" (`eq.rs:32-33`) — its STG shape
  (`nullary_branch`/`unary_branch`, `eq.rs:36-60`) is a genuinely different,
  recursive-structural-equality dispatch (matching blocks/lists element-wise,
  not just numbers), not the flat two-operand force pattern. This is exactly
  the W0 report's own observation that `EQ(` occurs 11 times in day03-p2, not
  a multiple of 25 (§3.1 of the W0 report, "mixed-type equality, not purely
  numeric"). Fusing `=` would require a materially different design (handling
  arbitrary recursion, not a flat two-operand force) and is explicitly
  deferred, not attempted here.
- **Bitwise ops (`BITAND`/`BITOR`/`BITXOR`/`BITNOT`/`SHL`/`SHR`) and unary
  numeric ops (`CEILING`/`FLOOR`/`POPCOUNT`/`CTZ`/`CLZ`).** Not part of the
  W0 spike's evidenced hot pattern (fib/day03/day09/day08 are dominated by
  `+`/`-`/`*`/comparisons); no profiling evidence justifies fusing them yet.
  `CEILING`/`FLOOR`/etc. are also unary, not binary, so would need a
  different (single-operand) fused shape if ever pursued.

### 3.3 Domain notes carried over verbatim into the fused path

- `/` is **floor** division (`floor_div_i64`, `arith.rs:39-49`); `÷` is
  **exact/precise** division (`PreciseDiv`). These remain textually and
  behaviourally distinct intrinsics — fusion changes *how operands reach*
  `execute()`, never *what* `execute()` computes.
- `Add`/`Sub`/`Mul`/`Div` special-case `Native::NdArray` operands
  (`arith.rs:95-102` et al.), dispatching to `array_binop` instead of scalar
  arithmetic. The fused path must preserve this: it resolves operands to
  `Native` (or an env-bound non-native) exactly as today, then hands off to
  the *unmodified* `execute()`, which still performs this check itself
  (§5.3) — no special-casing needed in the fused dispatch code at all.
- Comparisons (`Gt`/`Gte`/`Lt`/`Lte`) are **not** numeric-only — `ordered_cmp`
  (`arith.rs:544-574`) compares `Num`/`Str`/`Sym`/`Zdt` alike, raising
  `ComparisonTypeMismatch` only for genuinely incomparable pairs (e.g.
  `Num`/`Str`). The fused path must resolve operands to *any* `Native`, not
  assume `Native::Num`.

---

## 4. Encoding — how `encode.rs` finds the shape

**Recommendation: intercept by *intrinsic identity* at the global-form level,
not by pattern-matching the compiled `StgSyn` tree.**

This is the central risk-reduction decision, directly motivated by the
lever-(c) postmortem (§9). Lever (c) tried to *recognise* a semantically
special `Case` shape anywhere in an arbitrary STG tree and proved, twice,
subtly wrong to get right (an aliasing rule that was locally sound but broke
under composition with independent, later re-encoding of a nested `Case`).
Lever (a) does not need that generality: the fusible shape is not spread
arbitrarily through user code — it is *exactly and only* the compiler-emitted
body of a **known, small, fixed set of intrinsic global forms**
(`ADD`/`SUB`/`MUL`/`DIV`/`GT`/`GTE`/`LT`/`LTE`, §3.1), each identified by its
own intrinsic index, which the encoder already has in hand.

`src/eval/bytecode/encode.rs:437-476` (`emit_fixtures_and_globals`) builds
the global-slot table from `globals: &[LambdaForm]`, one entry per global
form, by index — and per the project's own global-slot convention
("`G(n)` is a global slot index (intrinsics `0..INTRINSIC_COUNT`, then
prelude)"), `globals[i]` for `i < 195` (`INTRINSICS.len()`,
`intrinsics.rs`) is *exactly* the compiled wrapper `LambdaForm` for
intrinsic `i`. Concretely:

```rust
// FUSIBLE_PRIMOPS: intrinsic indices resolved once via intrinsics::index(),
// never hardcoded magic numbers (mirrors the existing convention, e.g.
// `intrinsics::index("PRODUCER_NEXT")` at encode.rs:459-460).
static FUSIBLE_PRIMOPS: LazyLock<[usize; 8]> = LazyLock::new(|| {
    ["ADD", "SUB", "MUL", "DIV", "GT", "GTE", "LT", "LTE"]
        .map(|n| intrinsics::index(n).expect("fusible primop registered"))
});

// In emit_fixtures_and_globals's `globals.iter().map(...)`:
let entry = if FUSIBLE_PRIMOPS.contains(&i) {
    // Skip encoding lf.body() (the Case-of-Case tree) entirely — emit the
    // fused op directly. binary_wrapper always builds `lambda(2, body)`
    // (arith.rs:500), so the wrapper's own bound args are exactly
    // local(0)/local(1) — the two atoms the fused op forces.
    enc.emit_fused_primop(i as u8, /* arg1 */ 0, /* arg2 */ 1)
} else {
    enc.encode_node(lf.body())
};
```

No STG tree is inspected, walked, or rewritten — the interception is purely
**by intrinsic index**, verified once (structurally, `binary_wrapper` is
`arith.rs`'s only caller of that name, so this is a closed, auditable set)
rather than inferred per-occurrence. This has three concrete safety
consequences:

1. **`StgSyn` is completely unmodified — no new variant, no compiler change.**
   `binary_wrapper`'s output keeps existing exactly as-is; only the bytecode
   encoder's treatment of *this specific, whitelisted global form* changes.
   HeapSyn walks the untouched `StgSyn::Case` tree as it always has — it does
   not need to know a fused opcode exists at all. This preserves the
   byte-identical-both-engines invariant *by construction*, the same
   guarantee lever (c) achieved for the same reason.
2. **No shape-inference risk.** There is nothing analogous to lever (c)'s
   "detect a uniform-branch `Case` anywhere, prove the body doesn't need the
   binding" — the shape is known at the point of interception because the
   *intrinsic identity* is known, not inferred from tree structure.
3. **Extending the whitelist (§3.2) is a one-line, low-risk change** —
   confirm the new intrinsic's `wrapper()` output is `lambda(2, body)`-shaped
   with the two bound args as the operands, add its name to
   `FUSIBLE_PRIMOPS`, done.

**Every call site of a fusible primop is unaffected at the call side** — user
code still compiles `n <= 1` to `App(Lte.gref(), [n, 1])`
(`CallGlobal2`/`app_bif`-style call, unchanged); only `Lte`'s own *global
form body* (what that `App` ultimately enters) changes shape. This is exactly
analogous to how `Op::Bif`'s existing dispatch already routes through a
*global* intrinsic table by index (`machine.rs:2032`) — the fused opcode is,
conceptually, "the wrapper *and* the `Bif` call, pre-fused into the global
form's own entry point," not a new kind of user-facing call.

---

## 5. Dispatch — `machine.rs` handler design

**This section was revised after tracing the actual call graph** — the
intrinsic table (`intrinsics: &[&dyn StgIntrinsic]`) and everything
`execute()` needs (`symbol_pool`, `heap`, `program`, …) live on
`BcBifContext`/the outer `BytecodeMachine` (`machine.rs:2901-2914`), **not**
on `BcMachineState`, and are therefore **not reachable from `return_data`/
`return_native`** (`fn return_data(state: &mut BcMachineState, view, prog,
...)`, `machine.rs:887`; `return_native` is the same shape) — the functions
where operand resolution naturally happens. A first draft of this design
tried to call `execute()` directly from there and does not compile against
the real signatures. The fix follows directly from how `Op::Bif` itself
already solves the identical problem: **defer, and let `dispatch()`'s
existing tail (`machine.rs:2017-2083`) do the finalisation, completely
unmodified.**

### 5.1 Why deferral, not direct dispatch

`Op::Bif`'s own handler (`machine.rs:1694-1706`) cannot call `execute()`
either — it only sets `state.pending_bif`/`state.pending_bif_args` and
returns; `dispatch()` (the *outer* function, with full context) checks
`pending_bif` after every `step()` and does the actual
`ctx`-construction/`execute()` call (`machine.rs:2017-2083`). The fused
opcode should use **exactly this same deferral mechanism**, not a new one:
once both operands are resolved (inside `return_data`/`return_native`, which
only need `state`+`view` — matching every other continuation arm there),
set `state.pending_bif = Some(primop_id)` and
`state.pending_bif_args = smallvec![DecodedRef::Local(0), DecodedRef::Local(1)]`
pointing at a small scratch environment holding the two resolved operands,
and let `dispatch()`'s tail run entirely unmodified.

This is a stricter, more literal version of §2.2/§5's "no new arithmetic/
dispatch logic" claim than the first draft achieved: **zero new code touches
`execute()`, `BifFrame`, `ctx` construction, or error-trace attachment at
all** — every one of those stays exactly as `Op::Bif` already leaves them.

### 5.2 Why the operands need a scratch environment (not `Ref::V` directly)

`materialize_bif_args` (`machine.rs:433-442`) converts `DecodedRef::Local(i)
→ Ref::L(i)`, `Global(i) → Ref::G(i)`, `Value(k) → state.constants[k].clone()`
— the third form is a **compile-time constant pool** lookup; there is no
`DecodedRef` variant that carries an arbitrary *runtime-resolved* `Native`
value directly, and adding one (`DecodedRef::Resolved(Native)`) would put a
heap-pointer-carrying value into `state.pending_bif_args`, a field with no
existing GC-scan coverage today (because today's three `DecodedRef` variants
are all plain `u32` indices — trivially inert). Rather than open that new,
unaudited GC-root surface for a micro-optimisation, **bind both resolved
operands into a small (2-slot) scratch `BcEnvFrame`** — a heap structure
that is *already*, unconditionally, GC-scanned (it is exactly what every
ordinary environment frame is) — and reference them via
`DecodedRef::Local(0)`/`DecodedRef::Local(1)`, reusing `materialize_bif_args`
completely unmodified. This costs one small `Array<BcValue>`+`BcEnvFrame`
allocation per fused call that the zero-allocation ideal would avoid, but it
is still strictly cheaper than what it replaces: today's path already pays a
`materialize_bif_args` `Vec` allocation for the eventual `Bif` *and* up to
two `Op::Case` branch-table `Array` allocations (§1) that fusion removes
entirely. The `Resolved(Native)` micro-optimisation is noted in §9.4 as a
possible, separately-justified follow-up — not part of the first cut.

Critically, `execute()`'s `Ref::L(i)` resolution does **not** go through
`state.current`'s own closure env — it goes through `self.bif_env()`
(`machine.rs:2921-2927`), which reads `state.bif_frames.last().env`, a
*rooted* copy `dispatch()`'s tail captured from
`state.current.as_closure().map(|c| c.env())` at the moment it saw
`pending_bif` (`machine.rs:2025-2031`). So the scratch frame must be reachable
as `state.current`'s closure env at exactly that moment — see §5.4.

### 5.3 New continuation variants

```rust
/// Waiting for the first fused-primop operand to reach WHNF.
FusedPrimopLeft {
    primop_id: u8,
    /// Pre-emitted Op::Atom offset for the second operand (mirrors
    /// Op::Seq's `body`, cont.rs:59-68).
    right: CodeRef,
    environment: RefPtr<BcEnvFrame>,
    annotation: Smid,
},
/// First operand resolved to a BcValue (Native, or — rarely — a Closure
/// over a non-boxed-native data value, see §5.4); waiting for the second.
FusedPrimopRight {
    primop_id: u8,
    left: BcValue,
    environment: RefPtr<BcEnvFrame>,
    annotation: Smid,
},
```

`BcValue` (not a bespoke enum) is deliberately reused as the "resolved
operand" representation: `BcContinuation::ApplyTo`'s existing `args:
Array<BcValue>` is already scanned element-by-element
(`cont.rs:155-167`, `for arg in args.iter() { out.push(ScanPtr::new(scope,
arg)); }`), confirming `BcValue` already has a working, generic
`GcScannable`/`ScanPtr` story — holding a bare `left: BcValue` field needs
the same one-line treatment, not new scanning logic (§6.4).

### 5.4 `Op::FusedPrimop` decode (in `handle_op`)

```rust
Op::FusedPrimop => {
    let primop_id = read_u8(code, &mut pc);
    let left_off = read_u32(code, &mut pc);
    let right_off = read_u32(code, &mut pc);
    state.stack.push(BcContinuation::FusedPrimopLeft {
        primop_id,
        right: right_off,
        environment: env,
        annotation: state.annotation,
    });
    state.current = BcValue::Closure(BcClosure::new(left_off, env));
}
```

Byte-for-byte the same shape as `Op::Seq`'s handler
(`machine.rs:1548-1557`). The already-WHNF fast path needs no special-casing
here: `enter_local`/`enter_global` (`machine.rs:1387-1439`) set
`state.current` directly for an already-`BcValue::Native` slot, and the
outer trampoline takes it from there exactly as it does for any other
already-resolved value — no new fast-path code, inherited for free.

### 5.5 Resolving operands (new arms in `return_native`/`return_data`)

**`return_native`** — the common case once operands are actual numbers:

```rust
BcContinuation::FusedPrimopLeft { primop_id, right, environment, annotation } => {
    state.stack.push(BcContinuation::FusedPrimopRight {
        primop_id,
        left: BcValue::Native(value),
        environment,
        annotation,
    });
    state.current = BcValue::Closure(BcClosure::new(right, environment));
}
BcContinuation::FusedPrimopRight { primop_id, left, environment, .. } => {
    finish_fused_primop(state, view, primop_id, left, BcValue::Native(value), environment)?;
}
```

**`return_data`** — extract the single field if the tag is one of the four
boxed-native constructors (`DataConstructor::BoxedNumber`/`BoxedSymbol`/
`BoxedString`/`BoxedZdt`, `src/eval/stg/tags.rs:13-33`), mirroring exactly
what `unbox_any` does at the STG level today (`arith.rs:419-430`); for any
other tag, bind the *whole* data value as-is. Field extraction reuses
`env_from_data_args`'s existing single-arg resolution
(`machine.rs:669-713` — for a 1-field constructor this is already a cheap,
non-allocating path sharing the constructor's own backing array) to obtain
a `BcValue` for the field — which is itself either `Native` (the expected,
common case — `machine_return_num`/`machine_return_str`/etc. all return via
`machine.return_native`, `support.rs:325-330`, and boxing happens
separately when a value is bound into a slot; in practice this field is
always eagerly a `Native` by construction) or, defensively, still a
`Closure` (not asserted anywhere the fused path can safely rely on) — in
which case it is passed through as `BcValue::Closure(..)` unchanged, and
`execute()`'s own `resolve_native` (§5.2 — via `native_from_value`,
`machine.rs:2931-2967`, which *does* follow one more `Op::Atom` indirection)
will resolve or correctly error on it exactly as it does today, with no
extra forcing machinery needed on the fused path's side:

```rust
BcContinuation::FusedPrimopLeft { primop_id, right, environment, annotation } => {
    let resolved = resolve_fused_operand(state, view, tag, args, environment)?;
    state.stack.push(BcContinuation::FusedPrimopRight {
        primop_id, left: resolved, environment, annotation,
    });
    state.current = BcValue::Closure(BcClosure::new(right, environment));
}
BcContinuation::FusedPrimopRight { primop_id, left, environment, .. } => {
    let resolved = resolve_fused_operand(state, view, tag, args, environment)?;
    finish_fused_primop(state, view, primop_id, left, resolved, environment)?;
}

/// tag/args are return_data's own parameters — the just-produced data value.
fn resolve_fused_operand(
    state: &BcMachineState, view: MutatorHeapView<'_>,
    tag: Tag, args: &[DecodedRef], environment: RefPtr<BcEnvFrame>,
) -> Result<BcValue, ExecutionError> {
    match DataConstructor::try_from(tag) {
        Ok(DataConstructor::BoxedNumber | DataConstructor::BoxedString
         | DataConstructor::BoxedSymbol | DataConstructor::BoxedZdt) => {
            // args has exactly one entry for these constructors (arity 1,
            // tags.rs:73-92) — resolve it via the same machinery
            // env_from_data_args already uses.
            resolve_ref(view, &state.constants, /* constructor's own env */, state.globals, args[0])
        }
        _ => {
            // Not a boxed native — bind the whole (tag, args) value as a
            // closure over its own constructor env, unchanged; execute()'s
            // resolve_native/num_arg/ordered_cmp will classify and error on
            // it exactly as they do for the unfused path today.
            Ok(/* the whole Cons value, as return_data already has it in
                   `state.current` at this point */ state.current.clone())
        }
    }
}
```

### 5.6 Finalisation — deferring to the unmodified `Op::Bif` tail

```rust
fn finish_fused_primop(
    state: &mut BcMachineState, view: MutatorHeapView<'_>,
    primop_id: u8, left: BcValue, right: BcValue,
    environment: RefPtr<BcEnvFrame>,
) -> Result<(), ExecutionError> {
    // A 2-slot scratch frame holding the resolved operands — GC-scanned
    // like any other env frame, no new root-scanning surface (§5.2).
    let mut backing = Array::with_capacity(&view, 2);
    backing.push(&view, left);
    backing.push(&view, right);
    let scratch_env = view
        .alloc(BcEnvFrame::new(backing, state.annotation, Some(environment)))?
        .as_ptr();

    state.pending_bif = Some(primop_id);
    state.pending_bif_args = smallvec![DecodedRef::Local(0), DecodedRef::Local(1)];
    // `dispatch()`'s tail reads `state.current.as_closure().map(|c| c.env())`
    // for the BifFrame's rooted env (machine.rs:2025-2031) — this closure is
    // never entered (pending_bif short-circuits normal execution), so any
    // harmless code offset works; state.blackhole is the existing sentinel
    // used for exactly this "placeholder closure, only its env matters" role
    // elsewhere (enter_local's black-holing, machine.rs:1397-1406).
    state.current = BcValue::Closure(BcClosure::new(state.blackhole, scratch_env));
    Ok(())
}
```

Control then returns from `return_data`/`return_native` → `step()` →
`dispatch()`, whose **existing, completely unmodified** tail
(`machine.rs:2017-2083`) sees `pending_bif = Some(primop_id)`, calls
`materialize_bif_args` (turning `Local(0)`/`Local(1)` into `Ref::L(0)`/
`Ref::L(1)`), pushes the rooted `BifFrame { env: scratch_env, args }`,
constructs `ctx`, and calls `bif.execute(&mut ctx, view, emitter, &args)` —
`self.intrinsics[primop_id as usize]` (`ctx.intrinsics`,
`machine.rs:2910`/`2032`) is exactly the same table `Op::Bif` indexes.
**This is the concrete form of the design's central risk-reduction claim
from §2.2: the fused path's only new code is operand-gathering (§5.3-§5.5);
finalisation is the pre-existing `Op::Bif` tail, touched by zero new lines.**

---

## 6. Correctness

### 6.1 The reuse argument (why this is lower-risk than lever (c))

Every type check, every `ExecutionError` variant, every NdArray dispatch,
every floor-vs-precise-division distinction, every "blocks cannot be used in
arithmetic" contextual note (`num_arg`, `src/eval/stg/support.rs:92-116`) is
**unchanged Rust code, called exactly as it is today**, because the fused
path's only new responsibility is producing the `&[Ref]` `execute()` already
expects — via `Ref::V(native)` in the fast path or `Ref::L(scratch_index)` in
the fallback path, both of which `execute()`'s existing helpers
(`resolve_native`/`num_arg`/`str_arg`/`ordered_cmp`) already handle without
modification (`resolve_native`'s own contract, `bytecode/machine.rs:3232`
`fn resolve_native`, already resolves both `Ref::V` and `Ref::L` uniformly).
**A type error on a non-numeric operand raises the textually identical
`ExecutionError` it does today**, because it is raised by the identical code
path, just reached one opcode-dispatch cycle sooner.

### 6.2 Operand forcing order and laziness

`binary_wrapper` forces `x` (the *first* App argument in source order, per
`CallGlobal2`'s argument convention) before `y` in every path (§1's dump —
the outer `case x of` always precedes any inspection of `y`). The fused
opcode preserves this: `FusedPrimopLeft`/`FusedPrimopRight` force `arg1`
(source-order-first) strictly before `arg2` is even entered — §5.2's decode
pushes the *left* continuation and enters *left* first, unconditionally.
Only the two operands named in the `App` call are ever forced — no other
part of the environment is touched, matching today's behaviour exactly
(the wrapper only ever references `local(0)`/`local(1)`, its own two bound
args).

### 6.3 Division/overflow behaviour

Entirely `execute()`'s responsibility, unchanged: `DivisionByZero` for a zero
divisor (`Div::execute`, `arith.rs:308-313`), `NumericRangeError` for
checked-arithmetic overflow (e.g. `i64::checked_add` failure,
`Add::execute`, `arith.rs:107-113`), `NumericDomainError` for a non-finite
float result. None of this logic is touched, duplicated, or reachable from
the fused path except via the unmodified `execute()` call.

### 6.4 GC safety — the concrete new hazard this design introduces

This is the one genuinely new correctness surface (nothing in §6.1–6.3 is —
they are pure reuse), and it has two distinct parts, both mechanical but
both real:

1. **The two new continuation variants.** `FusedPrimopRight` holds
   `left: BcValue` — for `BcValue::Native(Native::Str/Set/NdArray/Vec(..))`
   (`src/eval/memory/syntax.rs:37-58`) this embeds a `RefPtr<...>` **heap
   pointer** that must be marked and forwarded across a GC cycle triggered
   *while the second operand is still being forced* (e.g. the second operand
   is itself an expensive, allocating thunk). §5.3 already identifies the
   reuse path: `BcContinuation::ApplyTo`'s existing `args: Array<BcValue>` is
   scanned element-by-element via `ScanPtr::new(scope, arg)`
   (`cont.rs:155-167`) — the same one-line `ScanPtr::new(scope, &self.left)`
   treatment covers `FusedPrimopRight.left`, and both continuations'
   `environment: RefPtr<BcEnvFrame>` fields need the same
   mark-and-forward-pointer handling every other variant's `environment`
   field already gets (`cont.rs:121-232`, e.g. `Branch`'s/`SeqBind`'s
   `environment` arms).
2. **The scratch `BcEnvFrame` built in `finish_fused_primop` (§5.6).** This is
   an *ordinary* heap-allocated environment frame — the same `BcEnvFrame`/
   `Array<BcValue>` type every `Let`/`LetRec`/`Branch`/`from_value` binding
   already allocates and that the collector already scans unconditionally as
   part of scanning any reachable environment. It needs **no new scanning
   code** — its safety rests entirely on it being reachable in the standard
   way: `state.current`'s closure holds it (§5.6), and `state.current` is
   already a GC root. The one thing to verify at implementation time is
   ordering — the frame must be fully populated and `state.current` updated
   to reference it *before* any subsequent allocation (e.g. inside
   `dispatch()`'s own `BifFrame` push, or inside `execute()` itself) could
   trigger a collection; §5.6's ordering (build frame → set `pending_bif`/
   `pending_bif_args` → set `state.current` → return) already satisfies this,
   but it is exactly the kind of ordering assumption worth a targeted
   `EU_GC_STRESS=1` test (allocate aggressively between resolving the second
   operand and reaching `dispatch()`'s tail) rather than trusting the
   argument alone.

**This must be implemented and covered by the existing `EU_GC_VERIFY=2`/
`EU_GC_POISON=1`/`EU_GC_STRESS=1` harness gates before merge — it is
mechanical, not novel, but it is exactly the kind of thing a lever-(c)-style
oversight would miss**, so it is called out explicitly here rather than left
implicit.

### 6.5 Differential testing — how this is actually validated

Unchanged from lever (c)'s validation strategy and for the identical
structural reason: `StgSyn` is untouched (§4), so **HeapSyn requires zero
awareness of the fused opcode** — it continues walking `binary_wrapper`'s
original `Case`-of-`Case` tree exactly as it always has.
`cargo test`/`EU_HEAPSYN=1 cargo test` full-suite byte-identical output
across both engines is therefore a genuine, structural differential-testing
oracle for this change (not merely "the tests happened to pass") — any
divergence in a fusible primop's result or error message between the two
engines can only come from a bug in the fused path, since the HeapSyn
reference path is provably unmodified.

---

## 7. Blob + source-prelude parity

Because the interception (§4) happens at `emit_fixtures_and_globals`, which
runs identically whether building the live-source pipeline (`encode`) or the
pre-compiled prelude image (`encode_prelude`) — both call the same function
with the same `globals` — **the fused opcode is emitted consistently in
both paths with no special-casing required.** The prelude blob **must** be
regenerated (`cargo xtask prelude-compile`) after this change, since the
eight fusible primops' global-form bodies change shape (shrinking, per the
byte-count argument in §1); the standard parity check applies unchanged:
`--source-prelude` and blob-mode must produce byte-identical evaluation
results (not necessarily byte-identical *bytecode*, since the blob format
should already tolerate this — the same tolerance lever (c) exercised when
the blob shrank by 60 bytes with zero behavioural change). No `Op` table
version bump is needed beyond the new `Op::FusedPrimop = 0x0F` variant itself
(`Op::from_u8`, `opcode.rs:29-46`, gets one more arm) — the blob format
already carries the full `Op` enum's encoding, so a stale blob built before
this lands will simply not contain the new opcode (harmless — it only
appears in newly-encoded intrinsic wrapper bodies, and blob staleness is
already handled by regeneration, not forward/backward wire compatibility).

---

## 8. Estimated upside

**Grounded in, but explicitly not over-claiming beyond, the W0 profile.**
The W0 report's own §3.3 declines to claim an exact percentage attributable
to the primop-forcing pattern specifically, "out of scope for the spike" —
this design note inherits that same discipline and does not fabricate a
number lever (c) could not have validated either.

What **is** directly supported by the W0 evidence:

- The "core dispatch/decode loop" bucket (`handle_op`+`dispatch`+decode+
  `enter_local`) is **45.88% (fib) to 64.63% (day09-p1)** of active CPU —
  the single largest bucket in every profiled program.
- `grep -c "LTE(\|SUB(\|ADD("` on `eu dump stg 001_naive_fib.eu` confirms the
  *entire* arithmetic/comparison surface of `fib`'s body (4 binops per
  logical call: one `<=`, two `-`, one `+`) is expressed through exactly this
  pattern — there is no other `Case` traffic of comparable density in this
  program (`fib`'s recursive calls themselves are plain `App`s, not `Case`s).
  This program is therefore close to a **best case** for this lever: fusion
  directly attacks the dominant cost bucket's dominant contributor.
- Each fused primop call removes: one `Op::Case` branch-table decode +
  `Array<Option<CodeRef>>` allocation (up to 2, for the boxed-`x` path) that
  the W0 report attributes `Array<Option<Native>>::push` at **4.43% of fib's
  total active CPU alone** (a single-attribute-visible number, not the whole
  cost — the branch-table `min_tag`/`len`/entries *decode* itself is folded
  into the larger `handle_op` bucket); one `Op::Bif` deferred-dispatch tail's
  `materialize_bif_args` `Vec` allocation (part of the 14.84% "arg/env build"
  bucket); and 3–4 fewer `handle_op`/`dispatch` round trips per primop call
  in the already-native fast path (§5.2).

**Candid uncertainty:** the exact post-fusion ratio cannot be predicted
without implementing and re-profiling — precisely the same caveat the W0
report itself carries for lever (c), which is why that lever was measured
before being trusted. What can be said with reasonable confidence:

- **fib is the best-case target for this specific lever** (arithmetic-dense,
  no other major cost driver per the W0 profile) — if fusion does not move
  fib's ratio meaningfully, no other lever within this design's scope will
  either, and that would be a strong, early, cheap signal to re-plan rather
  than iterate further on this specific mechanism.
- **day09-p1 and day03-p2** are more heterogeneous (list/data-structure
  traffic alongside arithmetic — `enter_local`'s own share grows with call
  density independent of `Case` traffic, 2.64% fib → 10.09% day09-p1 per the
  W0 report) — fusion should measurably help but is less likely alone to
  close the full gap on these; the plan's own risk mitigation (§9 of the W0
  report: "if the top lever falls short, stack a second lever… before
  loosening the bar") anticipates this.
- **A residual gap likely remains even after full fusion**, because
  decode/dispatch cost is structural to *any* bytecode VM relative to
  HeapSyn's zero-decode materialised-graph walk (delivery spec §2's own
  "Rationale" section already concedes this) — denser operand encoding
  (lever b, deprioritised by the W0 report pending (c)+(a)'s combined
  outcome) remains the documented fallback if (a) alone does not close the
  gap to the 1.25×/1.3× bar.

**Recommendation for the implementation phase:** implement, re-baseline
per the same fresh-clean-build protocol lever (c) used, and report the
measured fib/005/007/day03-p2/day09-p1 ratios honestly before claiming any
bar is met — do not extrapolate from this section's qualitative argument as
if it were a measurement.

---

## 9. Risks & interactions

### 9.1 W2 (ExecutionError boxing, PR #977, held pending this landing first)

PR #977 boxes the heavy `ExecutionError` variant payloads
(`Traced`/`LookupFailure`/`TypeMismatch`/… behind individual `Box`es,
shrinking `size_of::<ExecutionError>()` 128→40 bytes) and touches
`src/eval/bytecode/machine.rs` construction/match sites extensively. This
design's new code (`finish_fused_primop`, the two new continuation arms in
`return_native`/`return_data`) also lives in `machine.rs` and constructs/
propagates `ExecutionError` values on the failure path. Per the existing
sequencing decision (bead eu-adnu notes: "merge W1 (eu-9mvh) FIRST, then
rebase #977 onto the new master — both touch bytecode/machine.rs"), this
design's implementation should **also land before #977 is rebased**, so #977
is rebased once against the final machine.rs shape rather than twice. No
correctness interaction beyond the rebase-ordering mechanics — this design
does not depend on `ExecutionError`'s internal layout at all (it only
constructs/propagates existing variants via existing constructors).

### 9.2 W3 (block index, eu-4zhi, sequenced after this lever)

No code overlap: W3 targets `LookupOr`/`SafeLookup`/`bc_lookup_in_block`
(block-lookup `Case`-on-`Block`/`BlockPair`/`BlockKvList` tags), a
completely disjoint `Case` family from the `Native`-boxed-tag dispatch this
design touches. The only shared surface is that both eventually touch
`BcContinuation`/GC-scan code in the same files — sequencing this design
before W3 (already the plan's existing order) means W3's GC-safety work is
designed against the final `BcContinuation` shape, not a moving target.

### 9.3 Correctness pitfalls analogous to the lever-(c) aliasing bug

The lever-(c) bug arose from an encoder-time AST rewrite whose index
arithmetic silently assumed a nested `Case` node would *later* be encoded
with one set of runtime semantics, while a *separate*, independently-applied
encoding decision for that same nested node actually gave it different
semantics — the two decisions were not made consistently. **This design's
structure avoids that specific failure mode by construction**: there is no
AST rewrite and no nested, independently-re-decided sub-shape — the
interception (§4) is a single, flat, index-keyed substitution at the global
form boundary, and the dispatch (§5) reuses `execute()` unmodified rather
than re-deriving arithmetic/error semantics from a rewritten tree. The
closest analogous risk here is **not** index arithmetic but the GC-safety
surface in §6.4 (a genuinely new kind of continuation, holding heap-pointer
data, that must be scanned correctly) — which is why §6.4 is called out
explicitly, with a concrete implementation-time gate
(`EU_GC_VERIFY=2`/`EU_GC_POISON=1` must be clean) rather than left as an
assumed detail.

### 9.4 Whitelist drift

If a future refactor of `arith.rs` changes `binary_wrapper`'s shape (e.g.
alters which local index holds which operand, or the arity), the
`FUSIBLE_PRIMOPS` interception (§4) would silently emit a fused op assuming
the *old* shape (`lambda(2, body)` with operands at `local(0)`/`local(1)`).
**Mitigation:** add a debug-only assertion at encode time — when a global
form's index is in `FUSIBLE_PRIMOPS`, assert its `LambdaForm` is exactly
`arity() == 2` and (structurally) matches `binary_wrapper`'s known shape
before substituting the fused body; panic loudly in debug builds if not,
rather than silently emitting a wrong-shaped fused op. This is cheap (one
`debug_assert!`) and turns a silent-corruption risk into a build-time-loud
one.

---

## 10. Recommendation

**Implement `Op::FusedPrimop(primop_id: u8, arg1: CodeRef, arg2: CodeRef)`**,
a single generic opcode dispatched through the existing intrinsic table by
index, intercepted at the `emit_fixtures_and_globals` global-form boundary
for a small, explicit whitelist of intrinsic names (`ADD`/`SUB`/`MUL`/`DIV`/
`GT`/`GTE`/`LT`/`LTE` first; `%`/`÷`/`POW`/`QUOT`/`REM` as a verified fast
follow once their `wrap()`-shaped wrapper is confirmed compatible; `=`
explicitly excluded, permanently, unless a future design specifically
addresses its recursive-structural shape). `StgSyn` and HeapSyn are
untouched; the fused path forces both operands via the existing
continuation/trampoline machinery (mirroring `Op::Seq`) and delegates 100%
of arithmetic/comparison/type-checking/error-raising to the *unmodified*
`StgIntrinsic::execute()` already used by `Op::Bif` — not by calling it from
a second call site, but by binding the resolved operands into a scratch
environment and setting the exact same `pending_bif`/`pending_bif_args`
fields `Op::Bif` itself sets (§5.6), so `dispatch()`'s existing tail
(`machine.rs:2017-2083`) is the *only* code path that ever invokes an
intrinsic — genuinely one path, touched by zero new lines, not two paths
kept in sync by convention.

### Phased implementation outline

1. **Land W2 first if it's ready** (PR #977) — or explicitly re-confirm the
   "W1 lands first, W2 rebases" sequencing still holds; either order is fine
   correctness-wise, but only one rebase should be needed.
2. **`Op::FusedPrimop` + decode + the two continuation variants + GC scan/
   scan_and_update** (§5, §6.4) — implement and unit-test in isolation
   first (a handful of targeted `#[cfg(test)]` cases directly exercising
   `finish_fused_primop`/`resolve_fused_operand`, mirroring the existing
   `cont.rs`/`machine.rs` test style), before touching the encoder at all.
3. **`emit_fixtures_and_globals` interception** (§4) for the `binary_wrapper`
   whitelist, with the debug-assert shape guard (§9.4).
4. **`cargo xtask prelude-compile`**, then the full gate: `eu dump stg`
   unchanged (StgSyn genuinely untouched — this should be a *trivial* pass,
   unlike lever (c) which had to prove a *rewrite* was equivalent);
   `cargo fmt --all`; `cargo clippy --all-targets -- -D warnings`; `cargo
   clippy --target wasm32-unknown-unknown --lib -- -D warnings`; `cargo test`
   + `EU_HEAPSYN=1 cargo test` (byte-identical); `EU_GC_VERIFY=2`/
   `EU_GC_POISON=1 cargo test`; blob/`--source-prelude` parity.
5. **Fresh clean-build benchmark** (identical protocol to lever (c)'s
   PR #978) — report real, honest before/after numbers for the full
   regression set + protected wins before claiming any bar is met, exactly
   as instructed for lever (c). If fib alone does not move meaningfully,
   stop and report rather than expand scope further within this design.
6. **Fast-follow whitelist extension** (`%`/`÷`/`POW`/`QUOT`/`REM`) only
   after step 5's numbers justify continuing.

This design is scoped to be implementable as roughly the same size of change
as lever (c) turned out to be (one new opcode, two continuation variants, one
encoder interception point), while being **structurally lower-risk** than
lever (c) because it does not require proving an AST rewrite sound under
composition — the interception is index-keyed and flat, and all
domain-specific correctness is delegated, unmodified, to code that already
exists and is already exercised by `Op::Bif` today.
