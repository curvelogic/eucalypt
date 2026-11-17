# eu-oaz72.175: why blob mode still retains the list under closed bindings

Status: diagnosis plus a validated throwaway fix. Figures are measured-single
(release build, `-S` peak blocks, base = PR #1268 head a8c8029, 200k cells unless
stated). Peak blocks are deterministic; tick counts are exact.

## Finding

In blob mode the prelude's bodies are compiled **without the demand analysis
running on them**. `xtask prelude-compile` derives top-level *signatures*
(`derive_top_level_signatures`, step 5b) but then compiles each peeled
binding body with `compile_named` on the raw core: the inner `let`/`letrec`
bindings carry `Demand::default()` and `user_demand_sigs` is empty. Source
mode (and user code in blob mode, `driver/eval.rs`) runs
`analyse_demands_with_prelude` + `reflatten` before STG compile.

Consequence for `range`: its local `up(n): seq(n, cons(n, up(n + 1)))`.

- Source mode: the recursive call is `in seq ✳0 in →✳2(✳0)` (argument
  forced, then passed as a value).
- Blob global `range` (`eu dump runtime`): `in ✳2(✳0)`, no `seq`: the lazy
  `n + 1` thunk is passed on unevaluated, as an `Atom ✳0` alias over the
  current `let` frame.

Each `up` frame's `n` is then an alias whose env is the previous frame, whose
`n` is an alias of the frame before: an O(n) chain that is never collapsed.
Tracer (`EU_DBG=1`, op 6M, `range(0,200000) foldl-strict(+,0)`): the pending
`SeqBind`'s env reaches 108k frames; the BFS path alternates `next` and
`slot0 Atom alias (op 1)` for the whole depth, with a Cons cell in slot 1 at
the head. That is a different pin from #1268's (closed lambdas): closed
bindings are applied in the blob, but the producer's counter chain remains.
About half the unfixed slope is removed by #1268 (peak 11k -> 6k), which is
why it looks like a "second pin".

## Controls (peak blocks, 200k)

| program | source | blob |
|---|---|---|
| `range(0,200000) count` (global `range`) | 1,026 | 6,004 |
| wrapper fixture (`fs`/`c3`, global `range`) | 1,026 | 5,922 |
| `range(0,200000) foldl-strict(+,0)` | 1,026 | 5,924 |
| user `mk` (user `up`) `count` (global `count`) | 1,026 | 1,031 |
| `up(0) __take(200000) count` (user producer, global `count`/`__take`) | 1,026 | 1,031 |
| user `up(n inc)` / `up(nxt(n))` | 1,026 | 1,031 |
| `repeat(1) take(200000)` + wrapper | 12,470* | 1,031 |

(*source-mode wrapper over `repeat`/`take` is a separate lazy case, not this
bug.) So: consumers (`count`, `foldl-strict`, `__take`) in blob are fine; the
global `range` is the pin. User code compiles `up` with the `seq` because it
goes through the analysis.

## Proposed fix (validated as a throwaway)

In `xtask/src/main.rs` step 6, per binding body, before `compile_named`:

```rust
let (annotated, _s, named_sigs) = analyse_demands_with_prelude(
    body, Some(PreludeSignatures { name_to_slot: &name_to_slot, sigs: &demand_signatures }));
let annotated = reflatten(&annotated);
let mut body_settings = stg_settings.clone();
body_settings.user_demand_sigs = named_sigs;
// compile_named(&body_settings, annotated, ...)
```

i.e. exactly what `driver/eval.rs` does for user code, with the prelude
signatures already in hand. Diff: ~10 lines in xtask. It mirrors source mode
(which analyses prelude and program together), so it is a parity fix, not a new
analysis.

Results, blob mode, release (measured-single):

| | before | after |
|---|---|---|
| `range count` 200k peak blocks | 6,004 | 1,031 |
| wrapper 200k | 5,922 | 1,031 |
| `range foldl-strict` 200k | 5,924 | 1,031 |
| `range count` 800k | 25,597 | 1,031 (constant) |
| ticks `range count` 200k | 25.2M | 26.8M (+6%, see below) |
| benches 015..022 ticks | 2.018/5.584/5.538/2.402/1.051/7.896/716/49.20M | +1.5%/+0.8%/+0.8%/0/+1.7%/0/0/0 |

`scripts/blob-mode-tests.sh` with the fixed blob: every target ok (harness 711
passed; `diagnostics_snapshots`, `gate_liveness` pass; the two `FAIL` lines in
the output are `gate_liveness` fixtures that assert a FAIL). No timing claims
made (ticks only); the tick increase on `count` is the now-strict counter
(explicit `seq` before the recursive call) and should be checked under the
engine A/B protocol before it is called acceptable.

## Size, risk, wire format

- Size: ~10 lines in `xtask`, plus (required) adding `xtask/src/main.rs` to
  `DEMAND_ANALYSIS_PATHS` / `BUILD_RERUN_PATHS` in `wire_format.rs`, because the
  blob's code now depends on the xtask pipeline, which is not a freshness input
  today (an xtask-only change would otherwise leave a stale blob accepted).
- No layout change to the wire format: only the encoded code differs. Changing
  the hash inputs already makes old blobs classify Stale; a version bump is
  optional. So no further format change beyond the input list.
- Risk: medium. It changes the code of every prelude global in blob mode
  (strictness, update elision, SCC reflatten) to what source mode already
  runs, so it should be validated by: full harness in blob mode (done), the
  `tests/harness/errors/` corpus and `diagnostics_snapshots` (done, pass),
  `EU_GC_VERIFY=2 EU_GC_STRESS=1` and `EU_GC_POISON=1` in blob mode (not run),
  and the engine A/B protocol for performance (not run).
- Gate: extend `tests/list_retention_test.rs` to bound blob mode at about
  1,100 blocks (and assert n vs 2n equality); the current 8,192 bound would
  not catch this regression.

## Not done

GC-verify/stress runs in blob mode; interleaved timing; checking whether other
prelude globals have inner `let` shapes that were silently lazy (the fix
covers them all, but their effect was not surveyed).
