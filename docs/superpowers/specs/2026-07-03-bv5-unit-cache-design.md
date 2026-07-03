# BV5 Whole-Program Unit Cache — Design

> Status: **DRAFT for owner review** · Date: 2026-07-03 · Bead: **eu-lb0r**
> Parent: BV5 (eu-xfxc) · Depends on: eu-amp9 (dual-form blob, merged #952)
> Related memory: `bv5-serialisation-format-three-consumers`

## 1. Context & framing

Per ROADMAP §7 Pillar BV **decision 5 ("one serialisation format, three consumers")**, BV5's
postcard/arena serialisation machinery serves three consumers:

1. **Embedded prelude bytecode** — shipped in eu-amp9 (`PreludeBlob` now carries a
   `BytecodeProgram` alongside `ArenaStgSyn`).
2. **Content-hash unit cache** — *this document*.
3. **PP IPC wire payload** (Pillar PP, ~0.14) — serialised eucalypt **values**.

Settled scope fork (owner, 2026-07-03): build the serialisation **machinery + versioning
discipline** payload-agnostic, but implement the **code payload only** in 0.12 (prelude +
unit cache). The **value payload** (PP wire) is a documented extension point deferred to 0.14.
This spec covers consumer #2, code payload only.

The unit cache is, concretely, **the prelude-blob mechanism generalised to arbitrary merged
user programs**: the blob already caches one well-known "unit" (the whole prelude) as
serialised `ArenaStgSyn`, swapped in before STG-compile. This cache does the same for the
merged user program.

## 2. Goals / non-goals

**Goals**
- Skip recompilation on a repeat run whose inputs are unchanged: on a hit, load a serialised
  compiled artifact instead of running parse → desugar → merge → cook → … → STG-compile.
- **Default-on** (owner decision), so it must be **correct by construction**: a hit is
  byte-identical to a fresh compile, or it is not a hit.
- Serves both engines (bytecode default, HeapSyn via `EU_HEAPSYN=1`). The engine is part of
  the key (§6), so each engine caches the artifact it actually consumes; whether the two can
  share one entry is an implementation determination (§4).
- Reuse the existing postcard/arena serialisation (`src/eval/stg/blob.rs`,
  `src/eval/stg/arena/`).

**Non-goals (0.12)**
- **Per-unit / incremental caching** ("edit one file → fast recompile"). The pipeline merges
  units *early* (§4), so this needs the compiler restructured to defer merge — deferred to the
  roadmap (bead filed: "separate unit compilation (defer merge)").
- **Value serialisation / PP wire.** Machinery built payload-agnostic; value payload lands in
  0.14.
- Caching data-file parsing independently of the program.

## 3. Scope decision — whole-program

**The pipeline merges units early.** From the pipeline map (`src/driver/prepare.rs`):
per-input desugar (`prepare.rs:154-166`) → **merge into one `TranslationUnit`**
(`prepare.rs:169-188`) → then cook / split-letrecs / hoist / DCE / inline / fuse / verify /
demand-analysis / STG-compile all run **once on the single merged program**
(`prepare.rs:234` onward, `eval.rs:361-414`). Desugar itself carries cross-unit state forward
via `UnitInterface` (monad/operator/type registries, `source.rs:564-570`).

Consequence: a per-unit cache is **not natural** — the expensive work is inherently
whole-program, and per-unit desugar has sequential cross-unit dependence. So 0.12 caches the
**whole merged program**. Its reach: a hit occurs only when the **entire input set** is
byte-identical across runs (same `.eu` files, same data-loaded-as-units, same
compile-affecting options, same compiler). This covers CI re-runs, repeated identical
invocations, and run-on-save-without-edits; it does **not** cover changing input data between
runs or incremental multi-file edits (that is the deferred per-unit work).

## 4. What is cached — the artifact

Cache the **compiled artifact the active engine consumes**, at the post-STG-compile seam the
prelude blob uses. The **engine is part of the key** (§6), so correctness never depends on the
two engines producing identical bytes. HeapSyn consumes the `ArenaStgSyn` form directly; the
bytecode engine consumes a `BytecodeProgram` (the bytecode path recompiles STG with
`render_type: RenderDoc` before encoding — the reason eu-amp9's blob is **dual-form**).
**Implementation determination:** confirm whether the post-STG `ArenaStgSyn` is genuinely
identical across engines — if so, one shared entry suffices with the bytecode engine
re-encoding in <15 µs on load (measured); if the `RenderDoc` recompile makes them differ,
cache each engine's artifact under its engine-keyed `K`. Default to the safe, engine-keyed
form; treat the single-shared-entry version as an optimisation to prove, not assume.

Rationale for the stage: startup/compile cost is dominated by the front end (parse 7.8 /
desugar 7.0 / cook 5.6 / merge 2.9 / hoist 2.2 ms; STG-compile 33 µs; bytecode encode <15 µs
— measured on the prelude). Caching the post-STG artifact captures essentially the entire
compilation cost, and it matches exactly how the prelude blob already works.

**Insertion point** (from the map): the artifact produced by `stg::compile` (`eval.rs:391`,
via `src/eval/stg/mod.rs:333`). On a hit, skip everything from parse through STG-compile and
inject the cached `ArenaStgSyn` at the same seam the blob uses; on a miss, compile normally
and write the artifact. Cross-reference note: a compiled user program resolves prelude
free-vars to `Ref::G` global slots via the blob's `name_to_slot`, so the artifact references
prelude global slots — hence the prelude `source_hash` is part of the cache key (§6).

## 5. Cache lifecycle

```
run start
  → resolve inputs + options  (unchanged; parse phase still discovers the import graph)
  → compute cache key K        (§6)
  → look up  <cache-dir>/<K>.euc
       hit  → validate header (§7); load ArenaStgSyn; skip to demand-analysis/eval
       miss → compile normally; write <K>.euc atomically (§9)
```

Note: computing K requires reading the input file contents and resolving the import graph,
which the driver already does in the parse phase (`prepare.rs:57-109`) *before* the expensive
passes. So the key is available at the right point with no extra I/O beyond hashing bytes we
already read.

## 6. The cache key (correctness core)

`K = SHA-256( domain-sep ‖ inputs ‖ options ‖ compiler-identity )`, where a hit **guarantees**
the artifact equals a fresh compile. Components:

1. **Inputs.** For the ordered list of resolved input `Input`s (the full transitive set,
   including data-loaded-as-units and the `__args`/`__io`/`__build` pseudo-inputs that affect
   the merged program): each contributes `(locator-identity, content-bytes)`. Locator identity
   distinguishes `a.eu` from `lib/a.eu`. Order matters (merge order affects the program).
2. **Compile-affecting options.** Derived **programmatically from the canonical compile
   configuration**, not a hand-maintained list — see §8. The known set (from the map):
   `--no-dce`, `--source-prelude`/`EU_SOURCE_PRELUDE`, `--no-prelude`, `-t/--target`,
   `-e/--eval` (evaluand text), `-c/--collect-as` + `-N/--name-inputs`,
   `--suppress-demand-analysis`, the engine selector (`EU_BYTECODE`/`EU_HEAPSYN`), and the
   effective `StgSettings` compile flags (`prelude_globals`, `suppress_inlining`,
   `suppress_updates`, `suppress_optimiser`, `suppress_demand_analysis`). Rendering/output
   options (`-x`, `-j`, `-o`, `--error-format`, `-S`, `-d`, `--allow-io`, `--seed`*) are
   **excluded**. (*`--seed` feeds `__io` content, which is captured via the `__io`
   pseudo-input in component 1, not as an option.)
3. **Compiler identity.** §7.

Domain-separation prefix + a `format-version` constant guard against cross-version /
cross-consumer collisions.

## 7. Compiler identity — default-on strength

Default-on means any identity gap silently corrupts every user's output, so the identity must
change on **any** compiler change. The reliable ground-truth signal is the compiler **binary
itself** (a `.rs` edit re-links it; `build.rs`-embedded constants do **not** reliably change,
because the existing `build.rs` emits `rerun-if-changed` directives that disable the
full-package rescan). A full binary hash every run (~10–25 ms for a 20–50 MB binary) would
eat the startup win, so:

**Memoised binary self-hash.** Identity = SHA-256 of the compiler binary, **memoised** via a
sidecar `<cache-dir>/self-hash.json` keyed by `(exe-path, mtime, size)`:
- First run after a rebuild: `stat` misses the sidecar key → hash the binary once (~15 ms,
  one-time) → store `{path,mtime,size} → sha`.
- Subsequent runs: `stat` matches → reuse the stored sha (near-zero cost).

Combine with `build-meta.yaml` `version` + `commit` and the prelude `source_hash`
(`blob.rs:53`). The only theoretical hole — a byte-different binary with identical `mtime`
*and* `size` — cannot arise from a normal `cargo build` (it always re-links, bumping mtime).

`std::env::current_exe()` provides the path; fall back to "no cache" (compile normally,
don't write) if the exe path can't be resolved or hashed, so identity uncertainty never
produces a hit.

## 8. Completeness, made structural

A forgotten compile-affecting option is the other corruption path. Mitigations:
- The key's option-component is computed by a single function over the **resolved
  compile-configuration struct(s)** (`EucalyptOptions` compile-affecting fields +
  `StgSettings`), so a newly-added compile flag is included by construction rather than by
  remembering to update a list.
- A **guard test** enumerates the compile-configuration fields and asserts each is either in
  the key or on an explicit `#[cache_irrelevant]`-style allowlist — failing CI when a new
  field is added without a decision. (Mechanism: a derive/macro or a reflection-style test
  over the struct; exact form an implementation detail.)

## 9. Safety envelope (fail-closed)

Every cache file `<K>.euc` is:
```
Header { format_version: u32, compiler_identity: [u8;32], key: [u8;32] }
Payload: postcard(ArenaStgSyn-bundle)
```
On load: if `format_version` ≠ current, or `compiler_identity` ≠ current, or `key` ≠ the K we
computed, or postcard deserialisation fails, or any I/O error → **treat as a miss and compile
normally.** The cache is therefore *faster-or-equal, never wrong* given a complete key. This
is the prelude blob's `source_hash` reject-not-misdecode discipline, generalised.

## 10. Rollout & escape hatches

- **Default-on** from 0.12 (owner decision).
- `--no-cache` / `EU_NO_CACHE=1` — bypass entirely (no read, no write).
- `eu cache clear` — wipe the cache dir. `eu cache path` / `eu cache info` — introspection.
- On a `compiler_identity` change, stale entries are simply never hit (fail-closed); eviction
  (§11) reclaims them lazily.

## 11. Operational

- **Location.** `$XDG_CACHE_HOME/eucalypt/units/` → `~/.cache/eucalypt/units/` (respect
  `XDG_CACHE_HOME`; platform-appropriate on macOS/Windows via a small dirs helper). One file
  per key: `<K>.euc`. The `self-hash.json` sidecar lives at `~/.cache/eucalypt/self-hash.json`.
- **Eviction.** Size cap (default e.g. 512 MiB or N entries, configurable) with **LRU by file
  mtime** (bump mtime on read-hit). Prune opportunistically: cheap probabilistic check on
  write, plus `eu cache clear`/gc. No background daemon.
- **Concurrency.** Writes are **write-to-temp + atomic rename** into place, so a reader never
  sees a partial file; concurrent writers of the same K race harmlessly (last rename wins;
  content is identical by construction). Readers tolerate missing/half-written (rename makes
  the latter impossible) and always fall back to compile-on-any-error.

## 12. Correctness test plan (non-negotiable for default-on)

- **Differential matrix:** for a representative set of programs × compile-affecting options,
  assert **cache-hit output is byte-identical to a fresh (`--no-cache`) compile** — on both
  engines (default bytecode and `EU_HEAPSYN=1`).
- **Key-sensitivity:** assert K **changes** when any hashed input changes — flip each
  compile-affecting option, edit each input file, and vary the compiler identity; assert a
  miss (and no stale hit) in each case.
- **Envelope:** corrupt/short/wrong-version/wrong-identity files → miss, never a panic or
  wrong output.
- **Completeness guard test** (§8).
- **Concurrency:** parallel processes over the same K produce identical results and a valid
  cache file.

## 13. Risks & residual concerns

- **Identity `mtime`/`size` collision** — negligible in practice (see §7); fail-closed envelope
  bounds blast radius to "unexpected miss," never "wrong hit," *unless* the collision produces
  a matching self-hash sidecar entry for a different binary. Accepted.
- **Option-completeness drift** — mitigated structurally (§8) but is the highest-consequence
  failure; the guard test is the safety net.
- **Reach is narrow** (whole-program identical-inputs only). Documented; the per-unit story is
  the deferred roadmap item.
- **Cache-dir portability** across OSes — handled by a dirs helper; verify Windows path
  behaviour.

## 14. Future (out of scope here)

- **Per-unit incremental cache** — blocked on "separate unit compilation (defer merge)"
  (roadmap bead). Once units compile independently, cache each unit's compiled form keyed on
  its transitive-source hash + replayed `UnitInterface` state; this cache's format/envelope
  carries over.
- **Value payload / PP IPC wire** (0.14) — the same postcard/arena machinery + versioning
  envelope, carrying serialised eucalypt values instead of compiled code.

## 15. Open questions for the owner

1. Default eviction cap (512 MiB? entry count?) and whether it's user-configurable in 0.12.
2. `eu cache` subcommand surface — `clear` + `path`/`info` in 0.12, or just `clear`?
3. Is the `--no-cache`/`EU_NO_CACHE` naming right, or prefer `--cache=off` style?
4. Should a run with `--source-prelude` (no blob) still use the unit cache? (It can — the key
   already encodes `--source-prelude`; confirm we want it.)
