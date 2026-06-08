# eu-ynhu: String interpolation index bug in nested block-dot

## Status: INVESTIGATING

## Reproduction

Fails:
```eu
to-input-block(component-elements): {
  one-component-block(pair): {
    i: pair first
    l: (pair second) first
    s: (pair second) second
    one-component-key: "component_{i:%02d}"
    one-weight-key: "weight_{i:%02d}"
    ret: [[one-component-key sym, l str.of], [one-weight-key sym, s]] block
  }.ret
  default: { component_01: "None", weight_01: 0 }
  input-block: component-elements zip(iota(1)) map(one-component-block) take(4) foldl(merge, {})
}.(default input-block)

test: [["c1", 0.0], ["c2", 0.0]] to-input-block
```

Error: "type mismatch: received a number where a structured value
(block or list) was expected" at the `"component_{i:%02d}"` string.

## What works / what doesn't

| Scenario | Result |
|----------|--------|
| Function with inner block at top level | WORKS |
| Function with inner block via `map` at top level | WORKS |
| Function with inner block, 2 bindings (i + result), inside outer block-dot | WORKS |
| Function with inner block, 3+ bindings (i + x + result), inside outer block-dot | FAILS |
| Same function but NOT inside outer block-dot | WORKS |

The two required conditions for failure:
1. The function is defined inside an outer block that uses `.(...)` (block-dot)
2. The inner block has 3+ bindings

## Core expression (from `eu dump desugared`)

```
let to-input-block = λ(component-elements).
    let f = λ(pair).
        let i = (pair first);
            x = 42;
            result = JOIN(["component_", STR(FMT(i, "%02d"))], "")
          in { i: i, x: x, result: result }.result;
        out = (component-elements map(f))
      in { f: f, out: out }.out;
```

The `FMT(i, "%02d")` references `i` as `Bound(scope:0, binder:0)`.
This is correct at the core expression level.

## STG output (from `eu dump stg`)

### Bad case (3 bindings, nested in outer block-dot)

Inner letrec:
```
[0] thunk (pair first)        — i's value
[1] Num(42)                   — x's value
[2] thunk (JOIN/FMT code)     — result's value (FMT does case ✳6)
[3] Pair(:result, ✳2)         — block structure
[4] Cons(✳3, ⊗82)
[5] Pair(:x, ✳1)
[6] Cons(✳5, ✳4)              ← ✳6 lands HERE (WRONG!)
[7] Pair(:i, ✳0)
[8] Cons(✳7, ✳6)
```

The FMT code at `[2]` does `case ✳6 of Num → FMT(...)`. But `✳6` is
a Cons cell, not `i`'s value at `[0]`.

### Good case (2 bindings, same nesting)

```
[0] thunk (pair first)        — i's value
[1] thunk (JOIN/FMT code)     — result's value (FMT does case ✳6)
[2] Pair(:result, ✳1)
[3] Cons(✳2, ⊗82)
[4] Pair(:i, ✳0)
[5] Cons(✳4, ✳3)
```

`✳6` in a letrec of size 6 (0-5) goes past the letrec to the
enclosing lambda argument. This happens to work.

## Key observation

**The reference `✳6` is the same in both cases** — it's not being
adjusted when the letrec grows due to extra Pair/Cons entries from
`compile_block`.

## Relevant compiler code

### `compile_block` (`compiler.rs:1418`)

```rust
pub fn compile_block(&self, binder: &mut LetBinder, smid: Smid, block_map: ...) {
    binder.ensure_recursive();
    let mut index = KEmptyList.gref();
    for (k, v) in block_map.iter().rev() {
        let v_index = self.compile_binding(binder, v.clone(), smid, false)?;
        let kv_index = binder.add(dsl::pair(k, v_index))?;
        index = binder.add(dsl::cons(kv_index, index))?;
    }
    Ok(Holder::new(dsl::block(index)))
}
```

This adds Pair/Cons entries to the **same** LetBinder that holds the
Let binding values. Each `binder.add()` grows the letrec.

### `ProtoLet::take_syntax` (`compiler.rs:753`)

```rust
fn take_syntax(&mut self, compiler, context) {
    let mut binder = LetBinder::for_scope(self.expr.clone(), context);
    for (_, value) in scope.pattern.iter() {
        let index = compiler.compile_binding(&mut binder, value.clone(), ...)?;
        binder.add_var_index(index);
    }
    let body = compiler.compile_body(&mut binder, scope.body.clone())?;
    binder.set_body(body)?;
    binder.freeze();
    binder.into_stg(compiler)
}
```

The body (`{ i: i, x: x, result: result }.result`) is compiled via
`compile_body` which hits `compile_block`. This adds Pair/Cons entries
to `binder` AFTER the binding values were compiled as deferred protos.

### `Context::lookup` (`compiler.rs:452`)

```rust
fn lookup(&self, bound_var: &BoundVar) -> Result<Ref, CompileError> {
    if self.is_synthetic() {
        self.next()?.lookup(bound_var).map(|r| r.bump(self.size))
    } else if *scope == 0 {
        Ok(self.var_refs[*binder as usize].clone())
    } else {
        // decrement scope, bump by size
    }
}
```

For scope 0, `var_refs[binder]` gives the local ref. This is set at
compile time but the letrec grows afterwards due to compile_block.

### `var_refs` population

`add_var_index` is called in `ProtoLet::take_syntax` after each
`compile_binding`. So `var_refs[0]` = ref to compiled `i` value.
This ref is `Ref::L(n)` where `n` is the local index at time of
addition. But compile_block later adds more entries, and deferred
protos are resolved with the final binder size including those entries.

## Hypotheses

### H1: The deferred proto resolution uses wrong context size

When the FMT code (inside result's value, a deferred ProtoAppGroup)
is resolved via `into_stg`, the context includes the block structure
entries. The `bump` in synthetic binder lookup adds a delta that
includes the Pair/Cons entries, making the reference point too far.

### H2: var_refs holds stale indices

`var_refs[0]` = `Ref::L(0)` at time of creation. After compile_block
adds 6 entries, the letrec has grown but `var_refs[0]` still says
`Ref::L(0)`. Since letrec indices are absolute within the letrec,
`Ref::L(0)` should still mean "binding 0" regardless of size. So
this hypothesis may be wrong.

### H3: The issue is in how `into_stg` orders bindings

The deferred protos are resolved in `into_stg`. The order of bindings
in the final letrec may not match the order assumed by `var_refs`.
If compile_block's entries are interleaved or prepended, the indices
shift.

## Findings from Sonnet investigation (agent 3)

### Key finding 1: format specifier is the trigger

Plain `"{i}"` interpolation works. Adding any format spec like
`"{i:%02d}"` causes the error. This suggests the FMT intrinsic
wrapper inlining (ProtoInline) is where the index goes wrong.

### Key finding 2: `zip(iota(1))` vs manual pairs

The bug also manifests with `zip(iota(1))` even with fewer bindings.
Manual `[[1, elem], ...]` works but `zip(iota(1))` fails in the same
position. This suggests the issue may be related to lazy evaluation
of zip pairs, not purely a compile-time index problem.

### Key finding 3: NoBranchForNative is the error path

`return_native` at vm.rs:619 fires when a native value (Num/Str/Sym)
arrives at a `Branch` continuation WITHOUT a fallback. The FMT wrapper
body uses `case local(0) of Num → ...` — when the wrong value arrives
(a structured value like a Cons cell), it evaluates to a native number
from inside the cell, which then hits a branch expecting structured
data.

### Key finding 4: ProtoInline and FMT wrapper

The FMT intrinsic is inlined via ProtoInline. The wrapper body
contains `lambda(2, let_([...], ...))` where L(0)=value, L(1)=format
string. When inlined, these positions may not match what the let
binder actually places there, especially when nested inside a
block-dot's letrec.

### Key finding 5: env_from_data_args and zip

`env_from_data_args` creates a new env frame for data constructor
arguments. When `zip` creates pairs, the pair's fields are accessed
via this frame. The chain from the branch body back to the enclosing
lambda's `pair` argument may be disrupted.

## Approach for fix

The investigation suggests the issue is in ProtoInline's index
mapping when the FMT wrapper is inlined inside a letrec that has
been extended by compile_block. The format-specifier path triggers
ProtoInline; the no-format path uses a simpler STR call that
doesn't inline.

Investigate by:
1. Compare STG dump for `"{i}"` (works) vs `"{i:%02d}"` (fails) in
   the nested case — the difference is ProtoInline for FMT
2. Check how ProtoInline::take_syntax maps its arg indices when
   the context includes block structure entries
3. Check if the issue is compile-time (wrong STG) or runtime (correct
   STG, wrong env frame chain)

The fix likely involves one of:
- Fix ProtoInline's index mapping for the FMT wrapper
- Compile the block body in a nested binder (not the same one as Let)
- Fix env_from_data_args for lazy zip pairs
