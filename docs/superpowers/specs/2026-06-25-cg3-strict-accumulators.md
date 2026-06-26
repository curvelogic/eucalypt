# CG3+CG4: Strict Arguments at Recursive Call Sites

- **Bead:** eu-r7vr (subsumes eu-s9a5)
- **Pillar:** CG — Demand- and type-directed compilation (type-free tier)
- **Release:** 0.11
- **Date:** 2026-06-25

---

## 1. Problem

Two apparently distinct performance pathologies in recursive
functions share a single root cause.

### 1.1 The accumulator chain (CG3's original scope)

A lazy left fold builds a chain of unevaluated thunks — each
iteration creates a new accumulator binding as a `Thunk` with an
`Update` frame, but the thunk is not forced until the next iteration
(or the end). On `day01`, this produces a stack depth of 28,626
Update frames.

### 1.2 The higher-order fold O(n²) (CG4's original scope)

A higher-order fold `foldl(op, acc, list)` where `op` is a local
function is O(n²) in ticks, while the identical first-order
recursion using a global `+` directly is O(n):

| | N=5000 | N=10000 | N=20000 | Scaling |
|---|---|---|---|---|
| First-order (+ in body) | 1.1M | 2.2M | 4.4M | **O(n)** |
| Higher-order (op as arg) | 13.6M | 52.3M | 204.6M | **O(n²)** |

### 1.3 The shared root cause

Both are caused by the same mechanism in `create_arg_array`
(`src/eval/machine/env_builder.rs:256–270`). When the VM compiles
the arguments for a function call (`App`), each argument is wrapped
as:

```rust
SynClosure::new(self.atom(syn.clone())?.as_ptr(), environment)
```

This creates a lazy `Atom{Ref::L(idx)}` closure that captures the
**current environment**. At a recursive call site like
`foldl(op, op(acc, head), tail)`:

- `op` becomes `SynClosure(Atom{Ref::L(op_idx)}, env_iteration_N)`
- `acc'` becomes `SynClosure(Atom{Ref::L(acc_idx)}, env_iteration_N)`
- `tail` becomes `SynClosure(Atom{Ref::L(tail_idx)}, env_iteration_N)`

After `saturate` creates the new frame extending my-fold's original
env, the next iteration accesses `op` (parameter 0) and gets the
closure from the previous iteration. Entering that closure resolves
`Ref::L(op_idx)` from `env_iteration_N` — the **previous
iteration's** env. That resolution walks the env chain to find the
previous iteration's `op`, which is itself a lazy closure pointing
to `env_iteration_N-1`, and so on.

**The result is a chain-of-chains:** each iteration's `op` is a
lazy indirection through the previous iteration's environment. After
N iterations, resolving `op` walks through all N previous
environments. N resolutions × O(N) walk = **O(n²)**.

The same mechanism causes the accumulator chain: `acc'` is a lazy
thunk whose Update frame sits on the stack until the next iteration
forces it — but the forcing happens *after* the recursive call has
already pushed new frames.

**The fix is the same for both:** force strict arguments to WHNF
*before* the recursive call. This:

- Resolves `op` to its actual closure value (the `(_ + _)` lambda),
  breaking the chain-of-lazy-indirections. Next iteration gets the
  resolved value directly.
- Forces `acc'` to its computed value, retiring its Update frame
  within the current iteration.

## 2. Design

### 2.1 Transformation

At STG compile time, when compiling a recursive binding's body and
encountering a self-recursive call, the compiler:

1. Looks up the function's own demand signature (from
   `user_demand_sigs`, `prelude_demand_sigs`, or
   `intrinsic_demand_sigs`).
2. Identifies arguments at the recursive call site that are:
   - **Strict** in the demand signature, AND
   - **Complex expressions** (compiled as Thunks, not already
     atoms/WHNF values like `Ref::V` or `Ref::L` pointing to a
     known Value).
3. Wraps those arguments in `Seq` forms so they are forced to WHNF
   *before* the recursive call is entered.

This is the same `Seq` mechanism proven in 0.10.0, applied at
recursive call sites rather than at definition sites.

### 2.2 Detection

The compiler needs to know "I am currently compiling binding X, and
this call targets X." Two pieces of information are required:

1. **The current binding name** — the compiler must track which
   named binding it is currently compiling. This context must be
   threaded through `compile_body` / `compile_application`.

2. **The callee is the same binding** — at a call site, if the
   callee is a `Var::Bound` that resolves to the current binding
   (same de Bruijn index as the enclosing `LetRec` binding), or a
   `Var::Free` / `Ref::G` that matches the current binding's name,
   it is a self-recursive call.

The demand analysis already detects recursive bindings within
`LetRec` scopes (`src/core/analyse_demand.rs:465–497`). The
compiler can use the binding name to look up the demand signature
and apply per-argument seq wrapping at the recursive call site.

### 2.3 What this produces

For `foldl(op, op(acc, head), tail)` where foldl's signature marks
all three arguments as strict:

**Before:**
```
App { callable: foldl, args: [op, <thunk: op(acc, head)>, <thunk: tail>] }
```

Each arg is wrapped as SynClosure(Atom{Ref::L}, current_env) by
create_arg_array. The `op` arg is a lazy indirection through the
current env; `acc'` is an unevaluated thunk.

**After:**
```
Seq { scrutinee: <thunk: op>,
  Seq { scrutinee: <thunk: op(acc, head)>,
    Seq { scrutinee: <thunk: tail>,
      App { callable: foldl, args: [forced_op, forced_acc, forced_tail] }
    }
  }
}
```

All strict arguments are forced before entering the recursive call:

- `op` is resolved to its actual closure value (the lambda). When
  create_arg_array wraps it, the `Atom{Ref::L}` closure points to
  the *already-resolved* value in the current env — entering it is
  O(1), not a walk through previous iterations.
- `acc'` is computed to WHNF, retiring its Update frame.
- `tail` is forced, collapsing the list spine.

### 2.4 Why forcing `op` breaks the O(n²)

The key insight: `Seq` on `op` forces it to WHNF. The Update
continuation overwrites `op`'s env slot with the resolved value.
When `create_arg_array` then wraps `op` as
`SynClosure(Atom{Ref::L(op_idx)}, current_env)`, the
`Ref::L(op_idx)` slot now contains the **actual lambda closure**,
not a lazy indirection. The next iteration enters `op` and gets
the lambda directly — O(1), no chain.

Without the Seq, the env slot contains the original lazy
`Atom{Ref::L}` closure pointing to the *previous* iteration's env.
Each iteration adds one more link to the chain.

### 2.5 Interaction with existing Seq wrapping

The existing definition-site Seq wrapping (compiler.rs:756–792)
handles `Let` bindings. This change adds call-site Seq wrapping for
recursive calls. They are complementary:

- Definition-site: forces strict bindings when they are *created*.
- Call-site: forces strict arguments when they are *passed to a
  recursive call*.

An argument that is already a simple `Ref::L` to a binding that
was already forced at its definition site does not need seq-ing
again — it is already WHNF. The check for "complex expression"
(step 2 above) avoids redundant forcing.

**Important subtlety:** even `Ref::L` references to *passed-through
parameters* (like `op`) benefit from seq-ing. Although `op` was
forced when originally bound in the lambda frame, it is re-wrapped
as a lazy `Atom{Ref::L}` closure by `create_arg_array` at each
recursive call. The Seq forces that indirection, collapsing it to
the resolved value. The "complex expression" check must therefore
include `Ref::L` references to parameters that are passed through
unchanged — these are not "already WHNF" in the relevant sense.

### 2.6 Soundness

This transformation is sound because:

- It does not skip Update frames or change cardinality — it only
  reorders *when* a strict argument is forced (before the recursive
  call rather than after).
- The demand signature guarantees the argument will be forced anyway
  — forcing it earlier changes timing but not semantics.
- The §10.3 concerns (render traversal entering values twice,
  dynamic lookups, higher-order use) do not apply — we are not
  eliding updates, we are forcing them earlier.

## 3. Scope

### In scope

- Self-recursive calls within `LetRec` bindings where the
  function's demand signature is known.
- Both prelude functions (via `prelude_demand_sigs`) and
  user-defined functions (via `user_demand_sigs`).
- Both blob and source-prelude modes.
- Seq wrapping of **all** strict arguments at the recursive call
  site, including passed-through parameters (addresses both the
  accumulator chain and the env-walk chain).
- Subsumes CG4 (eu-s9a5, selective lambda-lifting / pre-projection).

### Out of scope

- Mutual recursion (A calls B calls A) — the detection is
  self-recursive calls only. Mutual recursion can be added later.
- Full worker/wrapper splitting — this uses the simpler
  force-before-recurse approach.
- Update elision / AtMostOnce cardinality changes — this does not
  change how Update frames work, only when they fire.
- BV3 register frames — those will further improve local resolution
  to O(1) array indexing, but this fix is independent and effective
  on the current cactus-env VM.

## 4. Success Criteria

1. **Correctness:** full harness green
   (`cargo test --test harness_test`).
2. **GC safety:** `EU_GC_VERIFY=2` + `EU_GC_POISON=1` passes on
   the full harness.
3. **Output identity:** rendered output byte-identical across the
   conformance corpus.
4. **Stack depth:** `EU_STACK_DIAG=1` on a left-fold program (e.g.
   `range(0, 10000) foldl((_+_), 0)`) shows substantially reduced
   max stack depth compared to baseline.
5. **Higher-order fold linearity:** a higher-order fold
   `my-fold(op, acc, list)` with a local `op` scales linearly
   (O(n)) instead of quadratically (O(n²)). Baseline measurements:

   | N | Before (ticks) | Target |
   |---|---|---|
   | 5,000 | 13.6M | ~1-2M (O(n)) |
   | 10,000 | 52.3M | ~2-4M (O(n)) |
   | 20,000 | 204.6M | ~4-8M (O(n)) |

6. **Performance:** measurable tick reduction on fold-heavy programs
   under both blob and `--source-prelude` modes.

## 5. Testing

- Existing harness covers correctness (byte-identical output).
- `EU_GC_VERIFY=2` + `EU_GC_POISON=1` on the full harness run.
- Before/after stack depth comparison using `EU_STACK_DIAG=1` on
  fold-heavy programs.
- Before/after tick comparison on both first-order and higher-order
  fold programs, confirming both scale as O(n).
- A targeted harness test with a large higher-order fold confirming
  linear scaling.

## 6. Risks

- **Low:** the transformation is conservative — it only forces
  arguments that the demand signature already says are strict. If
  the signature is wrong (overly strict), the existing code would
  already have the same forced-too-early behaviour at the
  definition site.
- **Medium:** detecting self-recursive calls requires threading the
  current binding name through the compiler. This is new context
  but straightforward plumbing.
- **Medium:** the subtlety around seq-ing passed-through `Ref::L`
  parameters (§2.5) means the "complex expression" filter must be
  carefully designed — it cannot simply skip all `Ref::L` args.
  The criterion is whether the argument's env slot has already been
  forced to WHNF in the current frame, which may require tracking
  which bindings have been seq'd.
- **Low:** redundant forcing (seq-ing an already-WHNF value) is
  harmless — `Seq` on a value in WHNF is a no-op at runtime.
