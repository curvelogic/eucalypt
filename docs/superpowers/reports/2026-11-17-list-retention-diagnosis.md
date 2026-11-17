# eu-oaz72.175: list traversal retention — interim diagnosis

Status: root cause of the `range(0, n) count` / `foldl` case NOT yet isolated.
Figures are measured-single (one release build on one machine, master cb1cc36,
source prelude, no protocol interleaving). Nothing here is a performance claim.

## Established
- Not a regression from #1247 / #1242: a build of 5aa27b7 (before both) gives
  the same ticks and peak blocks. A pre-#1251 build was not completed.
- `range(0,200000) count`: 22.6M ticks (~113/cell), peak 10,985 blocks,
  Blocks Recycled 0, sweep time ~0, 7 collections. Everything allocated stays live.
- The retained set is the consumed list. A global-held `up(0) __take(200000)`
  walked by a hand loop costs more blocks (16,443) than the loop shape that leaks
  (11,776). A hand loop that takes the list head/tail without calling a
  function parameter runs in constant space (peak 1,026, 20 collections).
- Trigger (minimal): `fs(op, l): if(l nil?, 0, seq(op(0), fs(op, l tail)))`
  with `op` a parameter retains; calling a global instead, not calling `op`, or
  using a counter instead of a list does not.
- ~113 ticks/cell is the expected shape (hand `take(up)` loop ~97).

## Refuted as the sole cause (each tried and reverted)
1. `make_arg_array*` wrapping evaluated cons/native slots in Atom alias
   closures (200k wraps; settling them removes the wraps, peak unchanged).
2. Global/Value arg atoms capturing the caller env (use `state.globals`).
3. Memoising Atom-alias callee slots in `enter_local` (fires once).

## Tracer (diff in `2026-11-17-list-retention-tracer.patch`, not for merge)
`dbg_trace` / `dbg_path` in `machine.rs`: at op N under `EU_DBG=1`, walks
env frames from each stack continuation and from `state.current`, counting new
frames per root, then BFS from `current` env to the deepest frame and prints the
edge path and the slots of three chain frames. Findings on `d1`
(`fs(op, n, l)` counter+list loop):
- Stack roots reach 1-2 frames each; `state.current`'s env reaches 166k frames,
  so the pin is a frame chain from the running frame, not a stack Update.
- Chain edge: frame.slot2 (the `l` parameter) = non-update arity-0 `Case`
  closure whose env is the PREVIOUS iteration's frame.

## Caveat: d1 is probably a poor proxy
For d1 the demand sig is `fs: [(Lazy,Multi),(Strict,Multi),(Lazy,AtMostOnce)]`:
`l` is only demanded inside the lazy second argument of `seq`, so the `l tail`
argument is a non-updating Value-form closure and the chain is the documented
lazy-accumulator shape (`TAIL(TAIL(...))`, see eu-oaz72.147), not necessarily
what `count`/`foldl-strict` do. The tracer was NOT run on `a1`/`b` (the real
repro, where `l` IS forced each iteration). Next lane should do that first.

## Suggested next steps
1. Run the tracer on `x: range(0, 200000) count` (move the trigger count so it
   fires mid-loop) and print the slot kinds of the chain frames.
2. If the chain is again an update-elided (AtMostOnce) argument closure: the
   likely fix is in the STG compiler, not to elide the update for an argument
   binding that is both `seq`-forced and passed on (the seq's result is
   discarded and the callee re-evaluates a closure that references the caller
   frame). See `take_lambda_form` / `strict_thunk_indices` in
   `src/eval/stg/compiler.rs` (strict marking only applies to `Thunk` forms).
3. Gating test: harness test with a heap limit (`--heap-limit-mib`) or a stats
   assertion that a 400k-cell fold keeps peak blocks bounded; fault-inject by
   reverting the fix.

## Update (second lane): root cause found (measured-single)

Tracer run on `range(0, 200000) count` (release build, master + tracer):
the pending `seq` continuation's env reaches 322k frames. Path: loop frame
slot0 (`op` lambda, arity 2) -> count's let frame (the lambda's env) -> count's
frame, whose param `l` was evaluated to the list head -> the whole consumed list.

So the pin is **frame capture**: a wrapper frame holding the list head stays
live because a closure allocated in it (`op`) is threaded through every
iteration. Peak blocks 10,985 vs 1,026 for the direct call.

Pure user-code repros (peak blocks, 200k cells):
- `x: range(0,200000) fs(+,0)` (fs = foldl-strict clone): 1,026 (no leak)
- `x: range(0,200000) fs({n:•el:•}.(n inc),0)`: 1,026 (no leak)
- `c3(l): fs(+,0,l)`; `x: range(0,200000) c3`: 10,346 (leak, global `+`)
- `c3(l): fs(+,0,l) + 0` and `fs(+,0,l tail)`: ~10,340 (leak)
- `count` / `foldl(+,0)` via prelude: ~11,000 (leak)

The earlier "non-update Case closure chain" is the list's own structure seen
through that root; the AtMostOnce-update-elision suspect is not the cause.
Side observation: `count`'s signature is `(Strict, AtMostOnce)` for `l`, but the
inlined body uses `l` three times (stale cardinality; not investigated).

Proposed fix (awaiting coordinator approval): last-use dead-slot clearing in
the STG compiler for params consumed only by a tail call and not free in any
closure allocated in the frame. Alternative: avoid wrapper frames in prelude
hot paths (needs lib/prelude.eu, currently held by another lane).
