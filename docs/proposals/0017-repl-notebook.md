# 0017 — An interactive surface: REPL / notebook for data exploration

- **Status:** Draft proposal for review
- **Track:** D — tooling
- **Classification:** Whitespace
- **Suggested horizon:** 1.0
- **Related:** H-none; ADR-001 (`docs/development/architectural-decisions.md`);
  sibling proposals [0004 — compiled-unit & prelude caching](0004-compiled-unit-caching.md),
  [0014 — incremental, query-based core](0014-incremental-query-core.md),
  [0016 — `eu doc`](0016-eu-doc.md)

---

## Summary

Eucalypt is used to *explore and transform* structured data, yet every
iteration demands a full CLI invocation: edit a file, re-run `eu`, read
output, repeat. A ~500–700 ms compile-pipeline overhead ([0004]) makes
this loop painful for exploratory work on small queries. This proposal
argues that eucalypt's purity, laziness, and catenation-pipeline style
make it unusually well-suited for an interactive surface, and recommends
a sequenced programme: (1) a watch-file mode as an immediate, low-cost
improvement — Unison's scratch-file UX transplanted to a config-language
context; (2) a thin `eu repl` command over the existing pipeline as a
second step; (3) a reactive browser notebook built on the existing WASM
API and CodeMirror playground as the ambitious end-state, contingent on
the incremental re-evaluation infrastructure proposed in [0014]. The GC
and session-state constraints are real and must be designed around
honestly rather than deferred.

---

## Motivation

### The iteration gap

The `eu` CLI has no interactive mode (`src/driver/options.rs:86–104`
enumerates every subcommand: `run`, `test`, `dump`, `version`, `explain`,
`list-targets`, `fmt`, `lsp`, `check` — no `repl`). Every query is a
fresh process: parse prelude (~120 ms) + cook (~190 ms) + eliminate
(~110 ms) + STG compile (~90 ms) + VM execution — ~500–700 ms before
any real work (`docs/development/deep-find-performance-baseline.md`).
For a 1 MB input where `data.total` runs in 1 ms, that is a 500×
overhead on every keystroke iteration.

Eucalypt's primary use pattern is exploratory: a user receives a YAML or
JSON blob, wants to slice, reshape, or derive values from it, and builds
up a transformation incrementally. The current workflow is:

```sh
eu data.yaml -e 'items filter(active?)'    # see what we have
eu data.yaml -e 'items filter(active?) count'  # count them
eu data.yaml -e 'items filter(active?) map(lookup(:name))'  # extract names
```

Each invocation pays the full compile tax. The accumulated friction
discourages exploratory use — particularly for users coming from `jq`,
`yq`, or Python/Pandas, where interactive evaluation is the default.

### Why eucalypt is unusually well-suited

Three properties make eucalypt a *good* candidate for an interactive
surface, not merely a plausible one:

1. **Purity: re-evaluation is always safe.** The pure core performs no
   effects (`src/driver/eval.rs:163–165`; IO is an explicit monad
   interpreted by the driver loop). Re-running any expression on any
   sub-expression of the input is safe: there is no mutable state to
   corrupt, no sequence of effects to replay, no transaction to undo.
   This is the property that makes Haskell REPLs and Unison scratch
   files tractable — and eucalypt has it fully.

2. **Catenation pipeline style: natural incrementality.** Eucalypt's
   central idiom is catenation — applying one more stage to a pipeline
   (`x f g h`). Each new stage is a small delta on the previous
   expression. A REPL or notebook cell is *exactly* one more stage: the
   user types `h` and the notebook re-evaluates from the previous
   result. This maps cleanly onto a cell-per-stage notebook model.

3. **Lisp lineage.** Eucalypt is Lisp-inspired (uniform syntax, block
   = environment, metadata = annotation channel). Lisp has the oldest
   and most developed REPL tradition in programming; the `read-eval-print
   loop` is natural for any language with a uniform expression language
   and a clear distinction between definition and evaluation.

---

## Prior art & landscape

### Unison's scratch-file UX (recommended first step)

Unison's UCM watches `.u` scratch files for changes and evaluates any
line beginning with `>` as a watch expression, printing results to the
console in real time (https://www.unison-lang.org/docs/tour/_scratch-files/).
UCM caches results keyed by the content hash of each expression, so
unchanged watch expressions are not re-evaluated on save — only changed
or newly dependent expressions run. The developer never leaves their
editor; the terminal running UCM acts as a live output pane.

This UX transfers directly to eucalypt with minimal new code:

- `eu watch file.eu` monitors the file for saves (via `inotify` /
  `FSEvents` / `ReadDirectoryChangesW`).
- On each save, re-run the full pipeline and reprint the result.
- Optionally, identify and evaluate lines ending with `#!watch` as
  expressions to evaluate and show inline — analogous to UCM's `>`.

The watch-file mode requires no persistent state, no session management,
no GC changes. It is a shell loop around the existing `run` subcommand,
plus a filesystem-watcher dependency. Cost: low. Value: immediate and
high for the exploratory pattern. This is the recommended Phase 1.

### The classic Lisp REPL

The REPL tradition — from McCarthy's original LISP `eval` to modern
`ghci`, Clojure's `nREPL`, and Racket's DrRacket — is the direct
ancestor of all interactive programming. The core insight: if the
language has a clean expression evaluation function, the REPL is just
that function called in a loop with accumulated bindings. For eucalypt,
`wasm_pipeline::evaluate_unit` (`src/wasm_pipeline.rs:92`) is already
that function — it evaluates a eucalypt unit from source and returns
rendered output. A native `eu repl` command would iterate over this,
accumulating declarations across prompts.

The complication unique to eucalypt is that the machine is ephemeral:
each call to `evaluate_unit` builds a fresh heap, interns symbols into a
per-machine `SymbolPool` (`src/eval/memory/symbol.rs:3`), and drops
everything on completion. There is no persistent heap across sessions.
This constraint is addressed in the design below.

### Observable reactive notebooks

Observable (https://observablehq.com/) is a JavaScript notebook where
cells are named reactive variables in a dataflow graph: change one cell
and the runtime re-executes all downstream cells automatically. The
runtime (`github.com/observablehq/runtime`) topologically sorts cells by
dependency and re-runs only the invalidated suffix.

This model maps excellently onto eucalypt's block semantics: each
declaration in a block is a named binding that can be a notebook cell;
the dependency graph is implicit in the expressions. A eucalypt notebook
where each cell is a named binding, and changing one cell causes all
dependent cells to re-render, would be a natural fit — and is precisely
the evaluation model that [0014 — incremental, query-based core] proposes
for the LSP. The reactive browser notebook is therefore contingent on
[0014] landing. Without [0014]'s Salsa-style dependency tracking, the
browser notebook must re-evaluate the whole document on every cell
change, which is correct but loses the "reactive spreadsheet" quality
that makes Observable compelling.

### jqplay / jq explorers

jqplay (https://jqplay.org/) and play.jqlang.org are the closest
*config-tool* analogues: a browser split-pane where the user pastes JSON
on the left, types a jq filter in the middle, and sees the result on the
right, with immediate re-evaluation on every keystroke. The entire jq
binary is compiled to WASM and runs in the browser.

Eucalypt already has this, partially: the CodeMirror playground (separate
repo `eucalypt-playground`) wraps `src/wasm.rs:evaluate` and
`evaluate_expr`, which wrap `wasm_pipeline::evaluate_unit`. The
playground is the jqplay equivalent. The gap is that it evaluates a
single full document, not a progressive session of staged transformations.
The browser notebook proposal extends the playground with cell-based
editing and reactive re-evaluation.

### Jupyter notebooks

Jupyter's cell model (code cell → execute → output cell, with shared
kernel state across cells) is familiar to the data-engineering audience
eucalypt wants to reach. However, Jupyter's design assumes a *stateful*
kernel that accumulates bindings across cells — which conflicts with
eucalypt's fresh-machine model. The appropriate analogue for eucalypt is
not Jupyter but Observable: cells are *declarations*, not *statements*,
and order does not matter.

---

## Proposed design

### Phase 1: `eu watch` — Unison-style file watching

A `eu watch [file.eu]` subcommand that:

1. Evaluates the file immediately and prints output.
2. Watches for filesystem changes (using the `notify` crate, already a
   transitive dependency in the Rust ecosystem).
3. On each change, re-evaluates and reprints. Prepend a `---` separator
   and a timestamp so the terminal history is readable.
4. Optionally: lines ending with `# => watch` (or a dedicated comment
   convention) are evaluated independently and their results printed
   inline, Unison-style.

No session state. No GC changes. The implementation is `loop { watch_for_change(); run(opt, loader)?; }` around the existing `run` path in `src/driver/eval.rs`. The main new code is a small `src/driver/watch.rs` module and a new `Commands::Watch` variant in `src/driver/options.rs`.

Latency concern: with [0004]'s prelude cache, each re-evaluation drops to
≤ 200 ms on a cache hit — acceptable for a watch loop triggered by file
save (not keystroke). Without [0004], the 500–700 ms latency is still
acceptable for watch-on-save (a human save is slow; 700 ms feels
instant). Phase 1 is useful today and better with [0004].

### Phase 2: `eu repl` — thin read-eval-print loop

A `eu repl` command that:

1. Presents a `>> ` prompt and reads a line of eucalypt declarations or
   an expression prefixed with `:eval`.
2. Accumulates declarations in an in-memory source string.
3. On each `:eval expr` or bare expression, evaluates the accumulated
   source + the new expression by calling the existing pipeline, starting
   a fresh machine each time.
4. Prints the result and prompts again.
5. Named data files loaded at startup (`eu repl data.yaml other.eu`)
   are re-loaded from disk on each evaluation — fresh but cheap for
   small data.

The critical design choice: **do not attempt to persist heap state across
prompts.** Each prompt runs a fresh machine (`src/driver/eval.rs:287`)
over the accumulated source text. This avoids the stale-string GC hazards
(`src/driver/eval.rs:274`) entirely, at the cost of re-paying the compile
overhead on every prompt — until [0004]'s prelude cache lands, at which
point the marginal cost per prompt drops to ~10 ms (loader) + VM.

Session state (`history`, loaded bindings as source text) is stored as a
`String`, not on the heap. The REPL is purely a thin loop over the
existing `run` pipeline; it adds no new runtime semantics.

```
$ eu repl data.yaml
eucalypt 0.7.0 — :help for help, :quit to exit
data loaded: data.yaml (1.2 MB, 4123 items)

>> items filter(active?)
---
- name: Alice
  score: 92
[...]

>> :let top: items filter(active?) filter(_ .score > 90)

>> top count
---
17

>> top map(lookup(:name))
---
- Alice
- Carol
[...]
```

`:let` adds a named binding to the accumulated source without evaluating.
Bare expressions evaluate in the context of all prior `:let` bindings plus
the loaded data. `:load file.eu` merges an additional source file.
`:reset` clears accumulated bindings.

The output shows intermediate WHNF where evaluation is partial (e.g. a
partially-applied function). This aids exploration: the user can see that
`items filter(active?)` is a list, `count` an integer, and so on, without
needing to know the type system.

### Phase 3: reactive browser notebook

The existing WASM API (`src/wasm.rs`) exposes `evaluate(source, format)`
and `evaluate_expr(source, format)`, which the CodeMirror playground
already uses. The browser notebook extends this:

1. The notebook is a sequence of named eucalypt declaration cells,
   displayed as a CodeMirror editor pane.
2. Editing any cell triggers a full re-evaluation of all cells (initially)
   or only dependent cells (once [0014]'s incremental core is available).
3. Each cell's output is rendered below the cell — YAML, JSON, or an
   embedded table/chart depending on cell metadata.
4. Cells may reference prior cells by name (since they are all bindings
   in one merged block).
5. Data import: a file-drop zone or URL field loads YAML/JSON/CSV into a
   named binding (using the existing import pipeline).

The WASM evaluation model already re-runs a fresh pipeline per call
(`src/wasm_pipeline.rs:108`), so Phase 3 does not introduce new GC
constraints — it merely calls `evaluate_unit` on the concatenated cell
source on each change.

The reactive per-cell model (only re-run changed cells and dependents)
requires the dependency graph from [0014]. Without [0014], Phase 3 is
still useful as a "evaluate on change" notebook — not reactive, but correct
and interactive. [0014] makes it feel like Observable; without [0014] it
feels like a fast Jupyter.

---

## Interaction with the existing roadmap

**[0004 — compiled-unit & prelude caching]**: the most important
enabler. Without the prelude cache, each REPL prompt and each watch-cycle
pays ~500 ms of compile overhead. With it, the marginal cost drops to
~10 ms (loader) + VM execution. Phase 1 and Phase 2 are useful without
[0004] but significantly more pleasant with it. Implement [0004] in 0.8;
Phase 1 can ship alongside it.

**[0014 — incremental, query-based core]**: enables Phase 3's reactive
cell model. Without [0014], the browser notebook re-evaluates the whole
document on every cell edit (correct, but O(n) in document size).
[0014]'s Salsa-style dependency graph makes it O(changed cells +
transitive dependents). Phase 3 can ship without [0014] as a "fast
batch" notebook and upgrade to reactive when [0014] lands.

**[0016 — `eu doc`]**: orthogonal but complementary. A REPL with `:doc
name` that calls the `eu doc` pipeline and prints the extracted
documentation would be a natural convenience. Implement as a thin REPL
command post-[0016].

**LSP**: the LSP server (`src/driver/lsp/`) already holds pipeline state
across incremental document edits — it is the closest existing analogue
to a persistent session. The REPL's fresh-machine approach is simpler
than the LSP's incremental sync model, but the two share the lesson that
the source text (not the heap) is the persistent session state.

---

## Implementation sketch

### Phase 1: watch mode

| File | Change |
|------|--------|
| `src/driver/watch.rs` *(new)* | filesystem watcher loop: `notify` crate, re-run `run()` on change |
| `src/driver/options.rs` | Add `Commands::Watch(WatchArgs)` |
| `src/driver/mod.rs` | Dispatch `Commands::Watch` to `watch::run()` |
| `Cargo.toml` | Add `notify = "6"` dependency |

Risk: low. No new semantics. New dependency (`notify`) is well-maintained.
Estimate: 2–3 days including tests.

### Phase 2: REPL

| File | Change |
|------|--------|
| `src/driver/repl.rs` *(new)* | prompt loop, source accumulation, `:let` / `:load` / `:reset` / `:eval` / `:quit` |
| `src/driver/options.rs` | Add `Commands::Repl(ReplArgs)` |
| `Cargo.toml` | Add `rustyline` or `reedline` for line editing and history |

Risk: medium. The fresh-machine-per-prompt approach is safe but slow
without [0004]. Readline integration is straightforward. The session
accumulation (source string) is trivial. The risk is UX quality (error
messages from accumulated source must point to the right prompt, not to
line 1 of a synthetic concatenated source).

Estimate: 1–2 weeks to a working prototype; 2–4 weeks to production
quality (error attribution, history persistence, `:help`, tab completion
via the LSP completion engine).

### Phase 3: browser notebook

The browser notebook lives in the `eucalypt-playground` repository, not
in this one. Changes to `eucalypt` itself are limited to:

- Extend `src/wasm.rs` with a `evaluate_cells(cells_json, format)` entry
  point that accepts a JSON array of `{name, source}` objects, concatenates
  them, evaluates, and returns per-cell outputs.
- Optionally expose partial evaluation (evaluate up to a named binding)
  for the per-cell output model.

The playground-side changes (CodeMirror cell editing, reactive wiring,
output rendering) are out of scope for this proposal.

Risk: medium-high. The per-cell output model requires evaluating the
document up to each named binding, which is not a current pipeline
capability. The simplest approach — evaluate the whole document and
extract per-binding values from the output — may suffice for Phase 3a.
A proper per-cell WHNF inspector is a larger change.

Estimate: 3–6 weeks for Phase 3a (whole-document evaluate + per-binding
output extraction); 6–12 weeks for a full reactive notebook contingent
on [0014].

---

## Alternatives considered

**Full persistent-heap REPL**: retaining the heap across prompts would
eliminate the re-parse overhead but requires solving the stale-string GC
hazard (`src/driver/eval.rs:274`), the per-machine `SymbolPool`
lifecycle (`src/eval/memory/symbol.rs:39`), and the stop-the-world
`UnsafeCell` heap soundness constraint (`src/eval/memory/heap.rs:8–11`).
None of these is impossible, but together they constitute a significant
GC and runtime engineering project. The honest assessment: a persistent
heap REPL would be the right end-state but is gated on either resolving
ADR-001's GC-finalisation wall (see [0020]) or accepting that the REPL
is the only long-lived heap consumer (a viable but constrained
architecture). Deferred to post-1.0.

**Daemon/server mode**: [0004] sketches a daemon that keeps the compiled
prelude in memory across invocations, serving the CLI as a thin client.
A daemon would eliminate the loader cost per prompt (currently ~10 ms
after [0004] caches the prelude). However, a daemon introduces process
lifecycle complexity and is gated on [0004] landing. It is the right
Phase 2b after the basic REPL proves its value. This proposal does not
specify the daemon; [0004] already sketches it.

**Embedding a general-purpose scripting REPL**: some configuration tools
offer a Python or Lua shell for interactive exploration. Eucalypt's
value is in its own language — embedding another language's REPL would
undermine the tool-first thesis.

**IEx (Elixir) / `ghci` (GHC) style persistent-module REPL**: these work
because Erlang/BEAM and GHC's RTS support code loading and heap
coexistence across sessions. Eucalypt's STG machine and Immix GC have no
equivalent dynamic-loading machinery. Transplanting this model is post-1.0
scope.

---

## Risks & what would kill this

1. **Compile latency without [0004]**: Phase 2's fresh-machine-per-prompt
   approach takes ~500–700 ms per prompt without the prelude cache. This
   is not interactive-quality. If [0004] stalls or is deprioritised, the
   REPL is usable only as a convenience wrapper around `eu run`, not as
   a true interactive environment. Mitigation: ship Phase 1 (watch-file)
   first, independent of latency; gate Phase 2 on [0004].

2. **Error attribution in accumulated source**: when the REPL evaluates
   prompt 7 and the error originates in a binding set at prompt 3, the
   source-map spans a synthetic concatenated string. Error messages will
   be confusing without careful source-map bookkeeping per accumulated
   chunk. This is a UX risk, not a correctness risk.

3. **Heap lifecycle for Phase 3 per-cell extraction**: extracting
   intermediate binding values (each cell's WHNF) from a single
   evaluation requires either multiple evaluations (one per cell) or a
   way to snapshot the heap at each named binding. The former is O(n²)
   in document size; the latter requires heap introspection not currently
   supported. Mitigation: Phase 3a uses full-document evaluate and
   renders only the final output, leaving per-cell intermediate results
   as a post-[0014] enhancement.

4. **Playground repository boundary**: Phase 3 changes live in
   `eucalypt-playground` (a separate repository). This proposal cannot
   specify those changes in detail; it only specifies the `src/wasm.rs`
   contract. Coordination between the two repositories is a process risk.

5. **Discoverability**: a REPL and notebook are valuable only if users
   know they exist and reach for them. If the primary adoption path is
   CI pipelines and scripting, interactive tools may see low usage. This
   is not a reason to avoid them — interactive exploration is a distinct,
   valuable use-case — but success metrics should be measured separately
   from the core CLI usage.

---

## Success criteria

- **Phase 1 (watch mode)**: `eu watch file.eu` re-evaluates on save
  within 800 ms (no prelude cache) or 250 ms (with [0004]'s cache),
  measured on a 1 MB YAML input. Works on Linux (inotify), macOS
  (FSEvents), and Windows (ReadDirectoryChangesW).

- **Phase 2 (REPL)**: a session of 20 sequential `:let` and `:eval`
  prompts on a 1 MB input completes without error; accumulated source
  does not grow without bound (`:reset` works); history persists across
  sessions via readline history file. Error messages from accumulated
  bindings identify the prompt number where the binding was entered.

- **Phase 3 (browser notebook)**: the playground can evaluate a 10-cell
  notebook (each cell a named binding referencing the previous) in under
  1 second in a modern browser on a 100 KB YAML input. Editing one cell
  re-evaluates and re-renders in under 500 ms.

- **Regression**: all existing harness tests (`cargo test --test
  harness_test`) pass unmodified; the REPL and watch commands introduce
  no new clippy warnings; `cargo fmt --all` is clean.

---

## References

- `src/driver/options.rs:86–104` — `Commands` enum: current subcommands,
  confirming no `repl` or `watch` exists.
- `src/driver/eval.rs:163–295` — headless→RENDER_DOC double-compile,
  fresh-machine pattern, stale-string GC hazard comment at line 274.
- `src/eval/memory/heap.rs:1–11` — `UnsafeCell` heap, single-threaded
  stop-the-world GC constraint.
- `src/eval/memory/symbol.rs:3,39` — per-machine `SymbolPool` with
  machine-same lifetime.
- `src/wasm.rs` — WASM API: `evaluate(source, format)` and
  `evaluate_expr(source, format)`.
- `src/wasm_pipeline.rs:92–108` — `evaluate_unit`: fresh pipeline per
  call, confirms stateless evaluation model.
- `docs/development/deep-find-performance-baseline.md` — ~500–700 ms
  pipeline timing table.
- `docs/reference/cli.md` — full CLI reference; no interactive subcommand.
- [0004 — compiled-unit & prelude caching](0004-compiled-unit-caching.md)
  — prelude cache, daemon sketch, double-compile elimination.
- [0014 — incremental, query-based core](0014-incremental-query-core.md)
  — Salsa-style dependency tracking; enabler for reactive notebook.
- [0016 — `eu doc`](0016-eu-doc.md) — documentation extraction;
  `:doc` REPL command dependency.
- Unison scratch files and watch expressions:
  https://www.unison-lang.org/docs/tour/_scratch-files/
- Unison UCM result caching (hash-keyed):
  https://www.unison-lang.org/docs/tour/
- Observable reactive dataflow runtime:
  https://github.com/observablehq/runtime
- Observable notebook model:
  https://observablehq.com/@observablehq/reactive-dataflow
- jqplay — interactive jq explorer in the browser:
  https://jqplay.org/
- ijq — terminal interactive jq wrapper:
  https://sr.ht/~gpanders/ijq/
