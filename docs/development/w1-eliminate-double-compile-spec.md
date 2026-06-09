# W1: Eliminate the double STG compile of plain documents

## Status: Spec — ready for implementation
## Release: 0.7.1
## Agent: Furnace (GC/heap lifecycle)
## Bead: eu-u2vn.1

## Problem

For a plain (non-IO) document — the common case — `try_execute`
compiles the prelude-plus-unit evaluand to STG **twice**: once headless
to classify IO vs document, then again under `RenderType::RenderDoc` on
a fresh machine to render.

The second compile (~90 ms) exists as a workaround: rendering the
headless result in place via `render_headless_result` crashes the GC on
stale string pointers left by the first run. The IO paths already
render in place safely; only the pure-document path recompiles.

## Code locations

- `src/driver/eval.rs:163-302` — `try_execute`, the IO-vs-document
  classification and the second compile at lines 278-295
- `src/driver/io_run.rs:1602` — `render_headless_result`, the
  in-place render that currently crashes
- The stale string hazard: after the first run, the heap may contain
  string pointers in continuation frames that become dangling when the
  emitter is reclaimed

## Design

Make `render_headless_result` safe so the pure-document path renders
in place — one compile, one machine — like the IO paths already do.

After the headless run detects a plain document (arity=0 result):
1. Build the `RENDER_DOC(value)` wrapper on the existing heap
2. Push it as a continuation or new closure on the existing machine
3. Resume execution for rendering
4. The heap, machine, and emitter must all stay alive — no drop/recreate

The stale string hazard must be identified and fixed. The GC must be
safe across the render step — the first run's live data must remain
reachable during rendering.

## Validation

- Validate under `EU_GC_VERIFY=2` + `EU_GC_STRESS=1` across the full
  harness suite
- Verify single compile via `stg-compile` timing (should appear once)
- Run AoC examples to confirm no regression

## Acceptance criteria

1. A plain-document run performs exactly one STG compile
2. The headless result renders in place without GC crash
3. No regression across the full harness under GC verification
4. Regression test exercises string values in plain document render
5. Measurable wall-clock improvement on small plain documents
