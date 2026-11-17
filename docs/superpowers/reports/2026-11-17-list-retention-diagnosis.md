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

## Update 2: pinning path in detail, and why the approved fix is non-trivial

Coordinator approved last-use dead-slot clearing (conservative: slot not free in
any closure/thunk/lambda allocated in the frame body at any depth; only after
the last read). Not yet implemented; findings for the next lane (measured-single):

- Bytecode opcode numbers: `Op::Cons` is 3, `Op::Case` is 2, `Op::Atom` is 1.
  The "non-update Case closures" in the earlier tracer notes are really **list
  cells (Cons closures)**: the chain is the list itself, linked via tail slots.
- Tracer on `c3(l): fs(+,0,l)`; `x: range(0,200000) c3` (also leaks, 10,346
  blocks). Roots: the pending `SeqBind` (stack index 3) env reaches ~312k
  frames. Path: loop frame slot0 (`op`, an arity-2 lambda) -> its env, a letrec
  frame holding the `+` lambda -> `next`: the wrapper's frame whose slot0 `l`
  is an **Atom alias closure** (op 1) -> the caller frame whose slot holds the
  list thunk, updated to the head Cons -> the whole list.
- Mechanism: `make_arg_array`/`make_arg_array_pd` pass an unsettled local as a
  non-updating alias closure `Atom ✳i` over the *current frame*
  (machine.rs `make_arg_array`), so callee params keep the caller frame
  alive; and `op` lambdas allocated in a frame capture the whole frame. Any
  frame on that chain holding the evaluated list head pins it. Memoising the
  alias slot (previous lane) does not help, since the head then sits in the
  wrapper frame directly.
- Design difficulties for slot clearing: (1) Refs are flat de Bruijn indices
  across a cactus chain, so a "clear" needs the frame depth resolved like
  `resolve_ref`; (2) the alias closure defers the read (it enters the caller's
  slot later, possibly several times, e.g. count's inlined body reads `l`
  three times), so "last read" is not at the call site; (3) the encoder,
  predecoder, machine (both dispatch paths), blob wire format and
  `Reference` users (~60 `Ref::L` sites) all need the new form/flag
  (`FLAG_EAGER`-style bit on App/DirectApp is the least invasive carrier).
- Separate follow-up (not fixed here): `count`'s signature records `l` as
  `(Strict, AtMostOnce)`, but the inlined body uses `l` three times (stale
  cardinality after inlining).
- Cheap validation experiment worth doing first: an unsound prototype that
  clears every local read by a tail-call argument, to confirm peak blocks drop
  to ~1,000 for `count` and the `c3` shape before building the sound analysis.

## Update 3: prototypes and design choice (third lane, measured-single)

Release build, byte-dispatch and predecoded, 200k cells. Baselines reproduce:
`count` 10,985 peak blocks / 22.6M ticks; `c3` wrapper 10,878 / 23.0M; direct
(no wrapper) 1,026 / 23.0M. The direct figure is the ceiling any fix can reach.

**(a) Blanket "clear every local read passed by value in an App".** Throwaway,
reverted. Not usable even as an upper-bound probe: replacing a read slot with a
tombstone makes `c3` evaluate to `null` (the prelude and the unrolled body read
the slot again), and peak blocks rose (13.8k). It confirms the clear must be
proved, not approximated; it confirms nothing about the ceiling.

**(b) Settling/overwriting the alias, or settling a tail-call argument first.**
Refuted without a new build. The wrapper frame still holds the evaluated head
after settling (the previous lane measured peak unchanged), and the wrapper frame
is the thing `op` pins. Whatever form `l` takes, the slot must be dropped or the
frame must not be captured.

**Why the plan's "last-use" analysis is harder than a single-use rule.** The
post-inlining STG for `c3` (`eu dump stg`) unrolls `fs` into the wrapper: `l`
is read by a `nil?` thunk, `head` and `tail` thunks (each `seq`-forced before the
tail call) and the final call argument. Static single occurrence never holds;
the clear is only sound if reads inside thunks that are certainly forced (and so
updated) before the call are treated as already finished. Further constraints
found in `src/eval/machine/env.rs`: case-branch frames share backing storage with
the constructor frame (a clear through a branch frame would clobber a live
constructor field); branch frame sizes are not known statically (only a lower
bound of 0 is sound); the flag needs a new App/DirectApp bit, so the blob wire
format changes and both dispatch paths change.

**Candidate designs, smallest first.**
1. Closed-frame trimming: a `Let`/`LetRec` whose bindings and body have no
   local ref reaching past the frame gets `next = None` (flag on the Let opcode).
   Sound with a lower-bound frame-size analysis (unknown branch sizes count as 0).
   Fixes closed combinators such as `+`, `count`'s `op`, `foldl(+, 0)`; does not
   fix a wrapper whose `op` genuinely captures a live local. Wire format changes.
2. Dead-slot clearing with seq-forced-thunk reasoning (the approved direction):
   general, but needs the happens-before analysis above plus per-arg move flags.
3. Both: 1 first (smaller), 2 as a follow-up bead.

## Update 4: closed-binding prototype (third lane, measured-single)

Prototype diff (throwaway, NOT for merge, unsound across programs because it
keys a process-global side table by byte offset):
`2026-11-17-list-retention-closed-binding.patch`. Predecoded path only.

**What pins the head in `c3(l): fs(+, 0, l)`.** `+` is inlined, so there *is* an
op lambda allocated in c3's frame: a `LetRec` binding `λ{2}` (the `+` body)
whose closure env is that LetRec frame, whose `next` is c3's frame, whose slot 0
(`l`) holds the list head. The loop threads `op` through every iteration.

**Prototype.** A `Let`/`LetRec` binding whose code has no local ref escaping
its own frame (no sibling or outer slot, lower-bound branch frame sizes: tag
3-5/11/17 = 1 field, 7 = 2, other tags 0, fallback = 1) is closed over
`state.globals` instead of the let frame. This is a special case of flat
closures (a closure capturing nothing needs no parent frame), semantics
preserving; the owner should see it as such (Decision 33 declined general flat
closures).

| 200k cells | base peak blocks | proto peak blocks | ticks |
|---|---|---|---|
| `range count` | 10,985 | 1,026 | 22.6M both |
| `c3` wrapper | 10,878 | 1,026 | 23.0M both |
| `foldl(+,0)` / `foldl((_+_),0)` | 11,001 | 11,001 (NOT fixed) | 21.8M both |

Per-frame trimming (frame `next = None` for the bindings' frame) gave the same
count result but is weaker than per-binding. Benches 015..022: ticks identical
to baseline in every file; peak blocks within +-1% except 016 (2,171 -> 2,422,
unexplained, one run); wall times single-run and noisy (e.g. 020 0.33s -> 0.43s)
so no timing claim is made. 022 (`range foldl(_+_,0)`) is not fixed.

**Open: `foldl(+, 0)` still leaks.** There the inlined `foldl` is a `Let` of
{fs lambda (self-referencing, not closed), `+` lambda (closed), `Num 0`} in a
thunk; the pin path was not traced. Next step: run the tracer on `fold.eu`
(`range(0,200000) foldl(+,0)`) with the closed-binding patch applied.

**Wire format.** The real change needs a per-binding "closed" bit in the form
header (`kind` high bit is the least invasive carrier) read by encoder,
predecoder, both dispatch paths and the blob: a blob wire-format change.
