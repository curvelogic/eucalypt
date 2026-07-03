# BV5 Whole-Program Unit Cache — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking. This is a backend feature in an existing codebase you know — where a task says "hook in at `eval.rs:NNN`", read that seam and match its conventions. Every task ends `cargo fmt --all` + `cargo clippy --all-targets -- -D warnings` clean and its tests green; wrap every `eu` run in `timeout 90 --heap-limit-mib 12288`.

**Goal:** A default-on, content-hash, whole-program cache of the compiled merged program, so a repeat run with unchanged inputs skips parse→…→STG-compile and loads a serialised artifact instead — correct by construction (a hit is byte-identical to a fresh compile, or it is not a hit).

**Architecture:** Generalises the prelude-blob mechanism (`src/eval/stg/blob.rs`) to arbitrary merged user programs. At the post-STG seam (`src/driver/eval.rs:391`), compute a cache key `K` over (all input contents + compile-affecting options + a memoised binary-self-hash compiler identity); on hit, load the engine-keyed compiled artifact from `~/.cache/eucalypt/units/<K>.euc` and skip compilation; on miss, compile and write it atomically. A fail-closed envelope (`format_version` + identity + key re-checked on load) makes the cache faster-or-equal, never wrong.

**Tech Stack:** Rust, `postcard` (already a dep, used by the blob), `sha2` (already a build-dep), `serde`. New code lives under `src/driver/unit_cache/`. Reuses `ArenaStgSyn`/`BytecodeProgram` serialisation from eu-amp9 (`blob.rs`, `program.rs`).

## Global Constraints

- **Spec:** `docs/superpowers/specs/2026-07-03-bv5-unit-cache-design.md` — the authority; this plan implements it. Re-read §6–§12 before Task 5.
- **Default-on**, so correctness is non-negotiable: never a stale hit. Every "is this in the key?" decision fails safe (include it, or recompile).
- **Fail-closed:** any header/key/identity mismatch, decode failure, or I/O error → treat as a miss and compile normally. The cache path must never panic on a bad/short/foreign file.
- **Engine-keyed:** the engine selector is part of `K`. Do NOT assume the bytecode and HeapSyn artifacts are byte-identical (the bytecode path recompiles STG with `RenderDoc`) — see Task 5; default to caching each engine's own artifact.
- **Never change HeapSyn or compiled output.** The cache is a pure short-circuit: `--no-cache` output must be byte-identical to cached-hit output on both engines. This is the differential test net (Task 10).
- Base branch: `docs/bv5-unit-cache-spec` (has the spec) off post-#952 `integration/0.12.0`. Work on a feature branch; PR to `integration/0.12.0`.
- Blob stays gitignored; this feature adds no committed binary artifacts.
- UK English in comments/docs; no `#[allow]`s.

---

## File Structure

**New — `src/driver/unit_cache/` (one responsibility each):**
- `mod.rs` — public surface: `UnitCache` façade (`lookup`/`store`), `CacheOutcome`, wiring types. Re-exports.
- `identity.rs` — `compiler_identity() -> [u8;32]`: memoised binary self-hash (`(exe-path,mtime,size)`-keyed sidecar) combined with build-meta version/commit + prelude `source_hash`.
- `key.rs` — `CacheKey` = `[u8;32]`; `compute_key(inputs, compile_config, identity) -> CacheKey`.
- `config.rs` — `compile_affecting_fingerprint(&EucalyptOptions, &StgSettings) -> Vec<u8>` (structural extraction, Task 3) + the completeness guard.
- `envelope.rs` — `Header { format_version:u32, identity:[u8;32], key:[u8;32] }`; `write_entry`/`read_entry` (fail-closed).
- `store.rs` — path resolution (`cache_dir()`), `read(K)`, `write_atomic(K, bytes)`, `evict_if_needed()`.
- `artifact.rs` — the serialisable cached-artifact bundle (Task 5) wrapping the engine's compiled form + what the loader needs to inject it.

**Modified:**
- `src/driver/options.rs` — add `--no-cache` flag + `EU_NO_CACHE` env; add the `eu cache` subcommand (Task 11); expose the compile-affecting fields the fingerprint reads.
- `src/driver/eval.rs` — the seam: lookup before `stg::compile` (`:391`); store after; skip-to-eval on hit (Task 9).
- `Cargo.toml` — add `sha2` to `[dependencies]` if only a build-dep today; add a `dirs`-style cache-dir helper (or hand-roll XDG — see Task 7, prefer no new dep if `std` + `EU_*`/`XDG_CACHE_HOME`/`HOME` suffice).
- Docs: `CLAUDE.md` debug-env table (`EU_NO_CACHE`), `ROADMAP.md` (mark BV5 unit-cache landed), `eu --help`.

**Tests:** unit tests inline per module; one integration test `tests/unit_cache_differential_test.rs` (Task 10).

---

### Task 1: Cache directory + config plumbing (`--no-cache` / `EU_NO_CACHE`)

**Files:**
- Create: `src/driver/unit_cache/mod.rs`, `src/driver/unit_cache/store.rs`
- Modify: `src/driver/options.rs` (flag + env), `src/driver/mod.rs` (add `pub mod unit_cache;`)

**Interfaces:**
- Produces: `unit_cache::cache_enabled(&EucalyptOptions) -> bool` (false if `--no-cache` or `EU_NO_CACHE=1`); `unit_cache::store::cache_dir() -> Option<PathBuf>` (respects `EU_CACHE_DIR` → `XDG_CACHE_HOME/eucalypt` → `HOME/.cache/eucalypt`; `None` if none resolvable → caching disabled, fail-safe); the sub-dir for entries is `<cache_dir>/units/`.

- [ ] **Step 1: Failing test** — in `store.rs`, `#[cfg(test)]`: `cache_dir_honours_env` sets `EU_CACHE_DIR` to a temp path and asserts `cache_dir()` returns `<temp>` and `units_dir()` returns `<temp>/units`. Assert `None` when `EU_CACHE_DIR`/`XDG_CACHE_HOME`/`HOME` are all unset.
- [ ] **Step 2: Run — expect FAIL** (`cargo test --lib unit_cache::store::` — undefined fn).
- [ ] **Step 3: Implement** `cache_dir()`/`units_dir()` with the precedence above (std env only; no new dep). Add `--no-cache` to `options.rs` (mirror an existing bool flag like `--no-dce` at `options.rs:220`) and read `EU_NO_CACHE` (mirror the `EU_SOURCE_PRELUDE` env read at `options.rs:439`). Implement `cache_enabled`.
- [ ] **Step 4: Run — expect PASS.**
- [ ] **Step 5: Commit** `feat(unit-cache): cache dir resolution + --no-cache/EU_NO_CACHE`.

---

### Task 2: Compiler identity — memoised binary self-hash

**Files:**
- Create: `src/driver/unit_cache/identity.rs`
- Modify: `Cargo.toml` (ensure `sha2` is a normal dependency)

**Interfaces:**
- Produces: `identity::compiler_identity() -> [u8;32]` — deterministic within a build, changes on any recompile. Internally: `sha256( self_hash ‖ build_meta_version ‖ build_meta_commit ‖ prelude_source_hash )`.
- Consumes: `Resources::get("build-meta")` (build-meta.yaml string, `resources.rs:51`); the prelude `source_hash` — via `PreludeBlob::source_hash` when a blob is present (`blob.rs:53`), else a fixed sentinel (source-prelude runs have no blob hash but the `--source-prelude` option is already in the key, Task 3).

**Design (spec §7):** `self_hash` = SHA-256 of the compiler binary, **memoised** in `<cache_dir>/self-hash.json` keyed by `(exe_path, mtime, size)`. First run after a rebuild hashes once (~15 ms); subsequent runs `stat` + reuse. `std::env::current_exe()` for the path; on any failure (no exe, unreadable) return a random-free sentinel that **disables caching for the run** (caller treats identity-uncertain as no-cache — see Task 9), never a fixed value that could alias two different binaries.

- [ ] **Step 1: Failing test** `self_hash_memo_reuses_on_stat_match`: write a temp "binary" file, compute+store its self-hash via the memo, mutate its bytes but reset mtime+size to the stored pair (simulate the impossible-in-practice case) → memo returns the OLD hash (documents the (mtime,size) trust boundary); then change mtime → memo re-hashes and returns the NEW hash. Also `compiler_identity_changes_with_build_meta`: two different build-meta strings ⇒ different identity.
- [ ] **Step 2: Run — expect FAIL.**
- [ ] **Step 3: Implement** the memo (serde_json or postcard sidecar; postcard preferred, no new dep) + `compiler_identity()`.
- [ ] **Step 4: Run — expect PASS.**
- [ ] **Step 5: Commit** `feat(unit-cache): memoised binary-self-hash compiler identity`.

---

### Task 3: Compile-affecting option fingerprint + completeness guard

**Files:**
- Create: `src/driver/unit_cache/config.rs`
- Modify: `src/driver/options.rs` / `src/eval/stg/mod.rs` only if a compile-affecting field lacks an accessor.

**Interfaces:**
- Produces: `config::compile_affecting_fingerprint(opts: &EucalyptOptions, stg: &StgSettings) -> Vec<u8>` — a stable byte-encoding of exactly the compile-affecting inputs (spec §6.2): `no_dce`, `source_prelude`, `no_prelude`, `target`, `evaluand text`, `collect_as`+`name_inputs`, `suppress_demand_analysis`, engine selector (`EU_BYTECODE`/`EU_HEAPSYN` resolved to a single `Engine` enum), and the `StgSettings` compile flags (`prelude_globals` presence, `suppress_inlining`, `suppress_updates`, `suppress_optimiser`, `suppress_demand_analysis`). Rendering/output/IO options are excluded.

**Structural completeness (spec §8):** build the fingerprint by matching on the *canonical* config structs field-by-field so a new field forces a compile error / conscious decision. Add a guard test that will fail when a field is added without classification.

- [ ] **Step 1: Failing test** `fingerprint_sensitive_to_each_compile_option`: construct a baseline `(opts, stg)`; for each compile-affecting field, flip it and assert the fingerprint bytes change; for two rendering-only options (`json`, `output`), flip and assert the fingerprint is **unchanged**.
- [ ] **Step 2: Failing test** `compile_config_fields_all_classified`: a test that enumerates the compile-config fields (via an explicit `const CLASSIFIED: &[&str]` list the implementer maintains next to the match, or a small proc/derive) and asserts it equals the current struct's field set — failing CI when a field is added un-classified. (Choose the lightest mechanism; a hand-list + `debug_assert`-style equality against `stringify!`-enumerated fields is acceptable and zero-dep.)
- [ ] **Step 3: Run — expect FAIL.**
- [ ] **Step 4: Implement** `compile_affecting_fingerprint` with the field-by-field match + the classification list.
- [ ] **Step 5: Run — expect PASS.**
- [ ] **Step 6: Commit** `feat(unit-cache): structural compile-affecting-option fingerprint + guard`.

---

### Task 4: Cache key

**Files:** Create `src/driver/unit_cache/key.rs`.

**Interfaces:**
- Produces: `key::CacheKey([u8;32])`; `key::compute_key(inputs: &[ResolvedInput], fingerprint: &[u8], identity: &[u8;32]) -> CacheKey`, where `ResolvedInput { locator_id: String, content: Vec<u8> }` is the ordered list of all merged inputs (Task 9 gathers them). Encoding: a domain-separation constant + `format_version`, then length-prefixed `(locator_id, sha256(content))` per input in order, then the fingerprint, then the identity → one SHA-256.
- Consumes: nothing runtime; `ResolvedInput` is defined here.

- [ ] **Step 1: Failing test** `key_order_and_content_sensitive`: same inputs in a different order ⇒ different key; changing one input's content ⇒ different key; changing a locator id ⇒ different key; identical everything ⇒ identical key; changing fingerprint or identity ⇒ different key.
- [ ] **Step 2: Run — expect FAIL.**
- [ ] **Step 3: Implement** `compute_key` (hash content per input so large data files aren't re-hashed into one giant buffer; length-prefix everything to avoid concatenation ambiguity).
- [ ] **Step 4: Run — expect PASS.**
- [ ] **Step 5: Commit** `feat(unit-cache): content-hash cache key`.

---

### Task 5: Cached artifact bundle (engine-keyed) + serialisation

**Files:** Create `src/driver/unit_cache/artifact.rs`.

**Interfaces:**
- Produces: `artifact::CachedArtifact` (postcard `Serialize/Deserialize`) carrying exactly what `eval.rs` needs to resume post-STG for the active engine, plus `to_bytes`/`from_bytes` (mirror `PreludeBlob::to_bytes`/`from_bytes`, `blob.rs:113-120`).

**Determination (spec §4 — do this first):** empirically check whether the post-STG artifact differs between engines. In a scratch test, compile a small program through `stg::compile` on the HeapSyn config and on the bytecode config (the `RenderDoc` recompile at `eval.rs:412`) and compare. **Record the finding in a comment.**
  - If identical → cache one engine-agnostic `ArenaStgSyn` bundle; the bytecode engine re-encodes on load (<15 µs) exactly as eu-amp9's loader does.
  - If different → `CachedArtifact` holds the **engine's own** compiled form (reuse `ArenaStgSyn` serialisation for HeapSyn; reuse `BytecodeProgram` serialisation from eu-amp9/`program.rs` for bytecode). The engine is already in `K`, so a given `<K>.euc` is unambiguous.

Reuse the arena serialisation the blob already round-trips (`src/eval/stg/arena/`, `blob.rs`). Do NOT invent a new format.

- [ ] **Step 1: Determination test** `post_stg_artifact_engine_parity` — asserts + documents which branch holds (this is a real test that pins the assumption; if it ever flips, this fails loudly).
- [ ] **Step 2: Failing test** `cached_artifact_round_trips`: build a `CachedArtifact` from a compiled small program, `to_bytes` → `from_bytes`, assert structural equality (and that reconstructing + evaluating yields the same render as compiling fresh — can be deferred to Task 10 if heavy here).
- [ ] **Step 3: Run — expect FAIL.**
- [ ] **Step 4: Implement** `CachedArtifact` + serde, per the determination.
- [ ] **Step 5: Run — expect PASS.**
- [ ] **Step 6: Commit** `feat(unit-cache): engine-keyed cached-artifact bundle + serialisation`.

---

### Task 6: Fail-closed envelope

**Files:** Create `src/driver/unit_cache/envelope.rs`.

**Interfaces:**
- Produces: `envelope::FORMAT_VERSION: u32`; `envelope::write_entry(w, identity:&[u8;32], key:&CacheKey, artifact:&CachedArtifact) -> io::Result<()>`; `envelope::read_entry(bytes:&[u8], expected_identity:&[u8;32], expected_key:&CacheKey) -> Option<CachedArtifact>` returning `None` on ANY mismatch/short/decode-error (never `Err`, never panic).

- [ ] **Step 1: Failing tests** in `envelope.rs`: `roundtrip_ok` (write then read with matching identity+key ⇒ `Some`); `rejects_wrong_version` (hand-craft a header with a bumped version ⇒ `None`); `rejects_wrong_identity`; `rejects_wrong_key`; `rejects_truncated` (feed 3 bytes ⇒ `None`, no panic); `rejects_garbage` (random bytes ⇒ `None`).
- [ ] **Step 2: Run — expect FAIL.**
- [ ] **Step 3: Implement** header write/read with length checks before every slice; postcard decode wrapped so errors map to `None`.
- [ ] **Step 4: Run — expect PASS.**
- [ ] **Step 5: Commit** `feat(unit-cache): fail-closed cache envelope`.

---

### Task 7: On-disk store — atomic write + read

**Files:** Modify `src/driver/unit_cache/store.rs`.

**Interfaces:**
- Produces: `store::read(dir:&Path, key:&CacheKey) -> Option<Vec<u8>>` (bump the file's mtime on hit, for LRU); `store::write_atomic(dir:&Path, key:&CacheKey, bytes:&[u8]) -> io::Result<()>` (write to `<dir>/units/.tmp-<rand-free-unique>` then `rename` into `<dir>/units/<hex(K)>.euc`; create dirs as needed). Uniqueness for the temp name without `rand`: use `std::process::id()` + an atomic counter (no `Math.random`/time deps).

- [ ] **Step 1: Failing test** `write_then_read_roundtrips` (temp dir): `write_atomic` then `read` returns the bytes; `read` of an absent key ⇒ `None`; a partial temp file left behind is ignored by `read` (only `.euc` files are read).
- [ ] **Step 2: Failing test** `concurrent_writers_leave_valid_file`: spawn N threads writing the same key with identical bytes; after join, exactly one `.euc` exists and reads back intact (atomic rename guarantees no torn file).
- [ ] **Step 3: Run — expect FAIL.**
- [ ] **Step 4: Implement** write-to-temp + atomic rename + read (with mtime bump).
- [ ] **Step 5: Run — expect PASS.**
- [ ] **Step 6: Commit** `feat(unit-cache): atomic-rename store with LRU mtime bump`.

---

### Task 8: Eviction — LRU by mtime + size cap

**Files:** Modify `src/driver/unit_cache/store.rs`; cap config in `config.rs`.

**Interfaces:**
- Produces: `store::evict_if_needed(dir:&Path, cap_bytes:u64)` — if `units/` total size > cap, delete oldest-mtime `.euc` files until under cap. Default cap `512 MiB` (`config::DEFAULT_CACHE_CAP_BYTES`), overridable via `EU_CACHE_CAP_MIB`.

**When to run:** cheaply and probabilistically on write — e.g. run eviction only when `hex(K)` starts with a fixed nibble (≈1/16 of writes), so it's amortised and needs no timestamp/RNG. Also runnable via `eu cache gc` (Task 11).

- [ ] **Step 1: Failing test** `evicts_oldest_until_under_cap`: create entries with ascending mtimes totalling > a tiny cap; run `evict_if_needed(cap)`; assert the newest survive, oldest are gone, total ≤ cap.
- [ ] **Step 2: Run — expect FAIL.**
- [ ] **Step 3: Implement** eviction (list `.euc`, sort by mtime asc, delete until under cap; ignore I/O errors on individual deletes — best-effort).
- [ ] **Step 4: Run — expect PASS.**
- [ ] **Step 5: Commit** `feat(unit-cache): LRU size-cap eviction`.

---

### Task 9: Driver integration at the seam (the load-bearing task)

**Files:** Modify `src/driver/eval.rs` (around `:361-414`), `src/driver/unit_cache/mod.rs`.

**Interfaces:**
- Produces: `UnitCache::lookup(...) -> Option<CachedArtifact>` and `UnitCache::store(key, artifact)`; and a `CacheOutcome` the eval path branches on.
- Consumes: everything above; the resolved input list + `EucalyptOptions` + `StgSettings` available at the seam; the `ArenaStgSyn`/`BytecodeProgram` produced at `stg::compile` (`eval.rs:391`) / bytecode encode (`eval.rs:412`).

**Behaviour (spec §5):**
1. Early in the compile path (after inputs are resolved — the parse phase already knows the full input set, `prepare.rs:57-109`), gather `Vec<ResolvedInput>` from the merged input list (each input's locator identity + content bytes; include the `__args`/`__io`/`__build` pseudo-inputs since they affect the merged program).
2. If `cache_enabled` and identity is certain: `identity = compiler_identity()`; `fp = compile_affecting_fingerprint(opts, stg)`; `K = compute_key(inputs, &fp, &identity)`; `read(dir,K)` → `read_entry(bytes,&identity,&K)`.
   - **Hit:** reconstruct the engine's runnable form from `CachedArtifact` and jump straight to eval — skipping parse→…→STG-compile. Match exactly how the blob path injects a precompiled artifact (`Executor::try_execute`, `eval.rs:275-324`, `set_prelude_bindings`) — the user program is the analogue of the prelude here.
   - **Miss:** compile normally; on success build `CachedArtifact` from the just-produced compiled form and `store` it (`write_atomic` + probabilistic `evict_if_needed`).
3. Identity-uncertain (no exe / unreadable) ⇒ behave as `--no-cache` (compile, do not read or write).

**Guard:** the hit path must produce byte-identical output to the miss path (Task 10 proves it). If reconstructing from the artifact is non-trivial for the bytecode+blob interplay (the artifact references prelude `Ref::G` slots — valid because the prelude `source_hash` is in the identity), verify the prelude image loaded for this run matches the one the artifact was compiled against; the identity already enforces this, but assert it.

- [ ] **Step 1: Failing integration test** (put in `tests/unit_cache_differential_test.rs`, expand in Task 10) `second_run_hits_cache`: run a small program twice with a shared `EU_CACHE_DIR` temp; assert run 2 produces identical stdout AND that a `.euc` file exists after run 1. (Use the `eu` binary via `assert_cmd`-style or the existing harness/tester plumbing — match how `tests/` invokes `eu`.)
- [ ] **Step 2: Run — expect FAIL** (no caching yet / no hit).
- [ ] **Step 3: Implement** the seam wiring in `eval.rs` per the behaviour above.
- [ ] **Step 4: Run — expect PASS** (run 2 hits; output identical; entry written).
- [ ] **Step 5: Gate** — full harness both engines unchanged with cache **on** (default): `cargo test --test harness_test` and `EU_HEAPSYN=1 cargo test --test harness_test` both green; `cargo test --lib` green.
- [ ] **Step 6: Commit** `feat(unit-cache): wire whole-program cache into the compile seam (default-on)`.

---

### Task 10: Differential correctness net (non-negotiable)

**Files:** Modify `tests/unit_cache_differential_test.rs`.

**Behaviour (spec §12):** the safety net that justifies default-on.

- [ ] **Step 1: `hit_equals_no_cache_matrix`** — for a representative set of programs (arithmetic, block/list, `map`/`filter` recursion, a multi-file `import`, a data-file-as-unit, a `-t target`, a `-e` evaluand) × a few compile-affecting options, run each **twice with cache on** (second = hit) and once with `--no-cache`; assert all three stdouts are byte-identical. Run the whole matrix on the default engine AND under `EU_HEAPSYN=1`.
- [ ] **Step 2: `key_sensitivity`** — for each program, flipping any compile-affecting option or editing any input file yields a **miss** (fresh compile), never a stale hit: assert output tracks the change (e.g. edit an imported constant → output changes on the very next run despite a warm cache).
- [ ] **Step 3: `envelope_robustness_e2e`** — corrupt a `.euc` file on disk (truncate / flip the version byte / random bytes) then run; assert the program still produces correct output (miss + recompile) and no panic/hang.
- [ ] **Step 4: `identity_change_invalidates`** — simulate a compiler-identity change (point `EU_CACHE_DIR` at a warm cache but force a different identity via the test seam, or stale the self-hash sidecar) → miss, correct output.
- [ ] **Step 5: Run — all PASS.** Also run under `EU_GC_VERIFY=2 EU_GC_STRESS=1` for the map/filter cases (the hit path reconstructs heap artifacts — GC-sensitive).
- [ ] **Step 6: Commit** `test(unit-cache): differential + key-sensitivity + envelope correctness net`.

---

### Task 11: `eu cache` subcommand

**Files:** Modify `src/driver/options.rs` (subcommand parse) + a small handler (near where other non-eval subcommands like `eu test`/`eu dump` dispatch).

**Interfaces:**
- Produces: `eu cache clear` (delete `units/` + `self-hash.json`), `eu cache path` (print `cache_dir()`), `eu cache info` (entry count + total size + cap), `eu cache gc` (run `evict_if_needed`).

- [ ] **Step 1: Failing test** `cache_clear_empties_dir`: warm the cache (run a program), `eu cache info` shows ≥1 entry, `eu cache clear`, `eu cache info` shows 0.
- [ ] **Step 2: Run — expect FAIL.**
- [ ] **Step 3: Implement** the subcommand + handlers (match the existing subcommand dispatch style — find how `eu test`/`eu dump` are routed in `options.rs`/`driver`).
- [ ] **Step 4: Run — expect PASS.**
- [ ] **Step 5: Commit** `feat(unit-cache): eu cache clear/path/info/gc`.

---

### Task 12: Docs

**Files:** `CLAUDE.md` (debug-env table: add `EU_NO_CACHE`, `EU_CACHE_DIR`, `EU_CACHE_CAP_MIB`), `ROADMAP.md` (§6.4/§9 mark BV5 unit-cache landed; note per-unit incremental remains the deferred "separate unit compilation" route), `eu --help` text.

- [ ] **Step 1:** Update the three docs (real content, not placeholders).
- [ ] **Step 2:** `eu build.eu -t build-meta` NOT needed; just confirm `eu --help` renders the new flag.
- [ ] **Step 3: Commit** `docs(unit-cache): env vars, roadmap, help`.

---

## PR & acceptance

- PR to `integration/0.12.0`. Title: `feat(bytecode): BV5 — whole-program content-hash unit cache, default-on (eu-lb0r)`.
- Acceptance: full harness 477/477 on **both** engines with cache default-on; the Task 10 differential net green; `cargo test --lib` green; `EU_GC_VERIFY=2 EU_GC_STRESS=1` clean on the hit path; clippy `--all-targets` + fmt clean; CI green (incl the new `test-bytecode-blob` job once #954 lands). Gatekeeper (wicket) reviews — the key scrutiny is Task 3 (option completeness), Task 6/9 (fail-closed, never a stale hit), and Task 10 (hit == no-cache on both engines).
- Do NOT merge your own PR; do NOT close eu-lb0r — report back.

## Self-review notes (author)

- Spec coverage: §3 whole-program→Task 9; §4 artifact→Task 5; §6 key→Tasks 3+4; §7 identity→Task 2; §8 completeness→Task 3; §9 envelope→Task 6; §10 rollout/escape→Tasks 1+11; §11 location/eviction/concurrency→Tasks 1+7+8; §12 tests→Task 10. All sections mapped.
- Sequence: Tasks 1–8 are independent, testable units; Task 9 integrates them; Task 10 proves correctness; 11–12 finish the surface. Task 5's determination gates whether the artifact is shared or engine-keyed — do it first within Task 5.
- Open determinations flagged inline (Task 5 engine-parity; Task 3 completeness-guard mechanism) — resolve against the code, don't guess.
