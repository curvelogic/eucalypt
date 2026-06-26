# CG1: Known-Call Direct Dispatch

- **Bead:** eu-dc72
- **Pillar:** CG — Demand- and type-directed compilation (type-free tier)
- **Release:** 0.11
- **Date:** 2026-06-25

---

## 1. Problem

Every function application in the STG VM goes through the same generic
dispatch path regardless of whether the callee is statically known:

1. Create argument array from refs
2. Push an `ApplyTo` continuation onto the stack
3. Resolve the callable to a `SynClosure` (global or local lookup)
4. Enter the closure — hit the lambda body
5. Pop `ApplyTo` in `return_fun`
6. Compare `args.len()` against `closure.arity()` (runtime arity check)
7. Branch: exact → saturate, under → partial-apply, over → split + re-push

Steps 2, 5, 6, and 7 are unnecessary when the compiler already knows the
callee is a lambda with a specific arity and the call site provides exactly
that many arguments. On AoC-2025 profiles, VM dispatch
(`handle_instruction` + `Machine::run`) accounts for 60–81% of CPU time.

## 2. Design

### 2.1 Detection (compile time)

In `ProtoAppGroup::take_syntax()` (`src/eval/stg/compiler.rs`), after the
callee and arguments are resolved, the compiler checks:

1. **The callee has a name** — either a `Var::Bound(bv)` with
   `bv.name.is_some()` or a `Var::Free(name)` / `Expr::Intrinsic`.
2. **A demand signature exists for that name** — looked up from
   `intrinsic_demand_sigs`, `user_demand_sigs`, or `prelude_demand_sigs`
   (this chain already exists at lines 1075–1087).
3. **Arity > 0 and equals the argument count exactly.**
4. **The callee is not a thunk** — for `Ref::L` bindings, the compiler
   must confirm the binding was compiled as a `Value` lambda form, not a
   `Thunk`. For `Ref::G` globals (intrinsics + blob prelude), this is
   always true for arity > 0. For `Ref::L` locals, the compiler knows
   the lambda-form decision from `take_lambda_form`. The implementation
   must thread this information (e.g. via the `Context` or a side-table
   populated during binding compilation) so it is available at the call
   site in `ProtoAppGroup`.

When all four conditions hold, emit `StgSyn::DirectApp` instead of
`StgSyn::App`. Skip the `Ann` wrapper — the `Smid` is carried inline in
the node.

All other cases (unknown callee, arity mismatch, thunk callee, anonymous
callee expression) fall through to `StgSyn::App` as today.

### 2.2 Coverage

This fires uniformly across all prelude modes:

- **Blob prelude:** prelude functions are `Ref::G` globals with
  `prelude_demand_sigs` → `DirectApp`.
- **Source prelude / substitute prelude:** prelude functions are `Ref::L`
  locals with `user_demand_sigs` from `analyse_demands()` → `DirectApp`.
- **Intrinsics:** always `Ref::G` with `intrinsic_demand_sigs` →
  `DirectApp` (for non-inlined intrinsics; inlined ones already bypass
  `App` entirely).
- **User-defined named functions:** `Ref::L` locals with
  `user_demand_sigs` → `DirectApp`.

### 2.3 IR changes

New variant added to three representations:

**`StgSyn`** (`src/eval/stg/syntax.rs`):
```rust
DirectApp { smid: Smid, callable: Ref, args: Vec<Ref> }
```

**`HeapSyn`** (`src/eval/memory/syntax.rs`):
```rust
DirectApp { smid: Smid, callable: Ref, args: Array<Ref> }
```

**`ArenaStgSyn`** (`src/eval/stg/arena.rs`):
```rust
DirectApp { smid: Smid, callable: Ref, args: Vec<Ref> }
```

The loader (`src/eval/memory/loader.rs`) translates
`StgSyn::DirectApp` → `HeapSyn::DirectApp` following the same pattern as
`App`.

`GcScannable` for `HeapSyn` scans `DirectApp`'s args array identically
to `App`.

Blob serialisation round-trips through `ArenaStgSyn`.

The optimiser (`src/eval/stg/optimiser.rs`) treats `DirectApp` as
opaque — no transformations needed.

### 2.4 VM fast path

In `handle_instruction` (`src/eval/machine/vm.rs`), `HeapSyn::DirectApp`
gets a dedicated match arm:

1. Set `self.annotation = smid` (replaces the `Ann` dispatch step).
2. Create arg array from refs (same mechanism as `App`).
3. Resolve `callable` to a `SynClosure` via `resolve_callable`.
4. Call `saturate_with_array` directly — bind args to the lambda's params
   and enter the body.

**No `ApplyTo` continuation is pushed. No runtime arity check. No
partial-application or over-application branches.**

The hot path drops from ~6 steps (push ApplyTo → resolve → enter lambda →
pop ApplyTo → arity check → saturate) to ~3 steps (resolve → saturate →
enter body).

**Error handling:** if `resolve_callable` fails (shouldn't happen for a
valid reference, but defensive), the existing `ExecutionError` machinery
handles it using the `smid` already set on `self.annotation`. A
`debug_assert!` that the resolved closure's arity matches the arg count
is included for development builds.

### 2.5 Relationship to `Bif`

`Bif` (saturated intrinsic with deferred execution) is left unchanged.
`DirectApp` handles the non-intrinsic case and the intrinsic case where
inlining did not fire. The two nodes coexist; unification is deferred to
the bytecode work (BV), which will reshape the dispatch mechanism.

## 3. Scope

### In scope

- Exact-arity calls to named callees (prelude globals, intrinsics, user
  functions, source-prelude locals) where arity is known from demand
  signatures and the binding is a lambda form (not a thunk).
- `Smid` folded into the `DirectApp` node (no wrapping `Ann`).
- All three IR representations (`StgSyn`, `HeapSyn`, `ArenaStgSyn`) and
  the loader, GC scan, and blob serialisation.

### Out of scope

- Partial application of known callees (CG4 / BV3).
- Unifying `DirectApp` with `Bif` (deferred to BV).
- Anonymous callee expressions (complex expression in function position).
- Thunk-wrapped callees (fall back to `App`).

## 4. Success Criteria

1. **Correctness:** full harness green (`cargo test --test harness_test`).
2. **GC safety:** `EU_GC_VERIFY=2` + `EU_GC_POISON=1` passes on the
   full harness.
3. **Output identity:** rendered output byte-identical across the
   conformance corpus.
4. **Diagnostics:** error messages preserve source locations — no lost
   `Smid`s. Existing error harness tests pass.
5. **Observability:** `eu dump stg` on a prelude-heavy program shows
   `DirectApp` nodes where `App` to known callees used to appear.
6. **Performance (blob prelude):** measurable tick reduction on
   `day11-p1` via `eu --statistics` under default (blob) prelude.
7. **Performance (source prelude):** measurable tick reduction on
   `day11-p1` via `eu --statistics --source-prelude`, confirming
   `DirectApp` fires for `Ref::L` locals.

## 5. Testing

- Existing harness covers correctness (byte-identical output).
- Add a targeted harness test exercising known-global calls in various
  patterns: pipeline (`xs map(f)`), direct (`map(f, xs)`), nested
  (`map(filter(p), xs)`), and user-defined named functions.
- `EU_GC_VERIFY=2` + `EU_GC_POISON=1` on the full harness run.
- Before/after tick and allocation comparison on `day11-p1` under both
  blob and `--source-prelude` modes.
- Verify `eu dump stg` output shows `DirectApp` nodes.

## 6. Risks

- **Low:** the change is additive — `App` remains for all cases
  `DirectApp` does not cover. A bug in detection logic means falling
  back to the existing path, not incorrect execution.
- **Medium:** the `debug_assert!` on arity match is the safety net. If
  the demand signature arity disagrees with the actual lambda form's
  arity at runtime, the assert catches it in development. In release, a
  mismatched saturate would produce incorrect binding — but this can only
  happen if demand analysis is wrong, which would already be a bug.
- **Blob serialisation:** `ArenaStgSyn::DirectApp` must be handled in
  the postcard codec. Existing blob consumers (prelude blob, unit cache)
  must deserialise it correctly. The blob version should be bumped.
