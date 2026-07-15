# Change log

All notable changes to eucalypt are documented here.

## [Unreleased]

## [0.13.0] - Unreleased

### Added

- **Prefix-list type `[A, B, C…]`** — a new type-DSL form sitting between the fixed tuple `(A, B)` and the homogeneous list `[T]`: a fixed-shape prefix followed by a homogeneous variable tail (an `A`, then a `B`, then zero or more `C`). This types the canonical hiccup/markup element shape `[tag, attrs, …content]` = `[Symbol, Block, (String | Element)…]` that `lib/markup.eu` is built on. Both `…` (U+2026, canonical) and ASCII `...` are accepted as the tail marker; `[C…]` normalises to the plain list `[C]`. Subtyping is covariant — a literal element (which synthesises to a tuple) checks against a prefix-list annotation via `Tuple <: PrefixList`, prefix widening and tail covariance hold, and a prefix-list widens to a homogeneous list; an unstructured `[T]` is deliberately **not** a subtype of a non-empty-prefix prefix-list. `head`/`second`/`tail` project the fixed prefix precisely (including through a recursive `Mu` alias), and a named projection past the fixed prefix returns `T?`, honest about the tail possibly being empty. The type flows through the full SV surface — `to-data`/`from-data` (`:t-prefix-list`), `as-spec`/`match?` runtime validation, and JSON-Schema export (the 2020-12 `prefixItems` + open `items` idiom). Per the owner-signed design `docs/superpowers/specs/2026-07-13-sv-prefix-list-design.md` (eu-2sa6.3)

### Changed

- **Recursive higher-order combinators (`foldl`, `map`, `foldr`, `scanl`, `scanr`, `map2`, `drop-while`, `iterate`) are now copy-specialised at every call site, on both the blob (default) and source-prelude paths — unconditional, default-on codegen, no flag** — `tag_combinators` (the shared inline-pass criterion) admits a self-recursive lambda as inlinable when it applies one of its own parameters, recurses on its own name, and stays within a 48-node size cap (design doc `docs/superpowers/specs/2026-07-13-recursive-combinator-copy-specialisation-design.md`, owner-signed). The inline pass then distributes the combinator into each call site as a local, self-recursive specialised copy with the concrete operator baked in, and demand analysis specialises that copy's now-concrete operator strict — collapsing what was previously a quadratic count of shallow environment lookups (not growing walk depth — measured directly via `EU_ENV_DEPTH_HISTOGRAM=1`) to linear. Landed in two stages: **blob path** (PR #1010, eu-dp0k) shipped this as default-on behaviour with no CHANGELOG entry at the time — corrected here; **source-prelude path** (`EU_SOURCE_PRELUDE=1`, PR #1016, eu-rb5n) closes the divergence PR #1010 knowingly left open, where three independent `Meta`-wrapper blind spots (in the inline pass's distribution step, in demand analysis's per-binding signature recording, and in the STG compiler's self-recursion detection — every source-path prelude binding is `Meta(Lam, {doc, type})`, invisible to code that only matched a bare `Lam`) meant the same criterion was tagging the copies correctly but never activating the optimisation at compile time. `distribute`'s new compile-time pre-expansion fixed point (`pre_expand_inlines`) only ever fires on bindings collected into the source-path's `inlines` list; the blob's own `inlinable_bindings` are baked pre-peeled and matched by the pre-existing `inlinable()` check already, so they were never subject to the Meta-blindness bug and this fix leaves blob-path codegen untouched. Canonical bench `tests/harness/bench/022_hof_fold.eu` (`range(0,10000) foldl((_+_), 0)`), ticks (deterministic — **measured-verified** per `docs/superpowers/engine-ab/PROTOCOL.md` §5, no wall-clock re-run needed; re-measured from a `cargo clean` build on both engines, corrects an earlier version of this table that mixed bytecode- and HeapSyn-engine figures under a single "bytecode engine" label, and a subsequent incorrect revision that reported the blob row as improved — that revision was itself an artefact of a stale, non-clean incremental build, not a real effect; a clean rebuild shows the blob path genuinely unchanged):

  | path | engine | before | after | Δ |
  |---|---|--:|--:|--:|
  | blob (default) | bytecode | 2,085,398 | 2,085,398 | unchanged |
  | blob (default) | HeapSyn | 2,185,410 | 2,185,410 | unchanged |
  | source-prelude (`EU_SOURCE_PRELUDE=1`) | bytecode | 51,885,361 | 1,500,319 | **−97.11%** |
  | source-prelude (`EU_SOURCE_PRELUDE=1`) | HeapSyn | 51,985,369 | 1,640,321 | **−96.84%** |

  The blob path is untouched by this fix, as expected (it never exercised the Meta-blindness bug). Source-prelude not only closes the quadratic-vs-linear divergence, it now runs *fewer* ticks than blob on both engines — 28.06% fewer on bytecode, 24.94% fewer on HeapSyn — because `pre_expand_inlines` (source-path-only, per above) gives the source path an extra depth of pre-expansion the blob path's simpler bake-once `inlinable_bindings` snapshot doesn't have. Type-annotation safety unaffected on either path — the pre-inline type check (eu-rb5n Q1) runs, and captures any warnings, on the un-transformed core before either the tagging or distribution passes touch it; harness test 104 is the standing tripwire and passes identically before and after on both paths. First-order self-recursion (`fib`, `take`, `iota`, …) is deliberately excluded by the criterion (no parameter application) and unaffected by either stage — that remains eu-npp9's separate domain (tracking bead eu-2sa6.22 records this combined entry's provenance)

### Fixed

- **`tests/harness/183_widen_type_def_literals.eu`'s `result-def` case was silently red, masked by a hardcoded `RESULT: :PASS`** — found by Wicket's pre-release silent-assertion audit (eu-ntwg.2, PR #1017 review). The test's second half put `` `{result-def: true}` `` on `make-tag(x): { tag: :default, value: x }` with no `type:` annotation; a plain lambda with no block-destructured parameter synthesises to `any` (`synthesise_lam`'s documented fast path), so the checker had no function type to extract a return type from — it correctly warned (`` `result-def` on a non-function binding has no return type to capture``) and `s"Tag" as-spec` correctly degraded to `any?`, silently accepting the `t-result-reject` case the test meant to exercise. A second, independent mistake compounded it: `result-def: true` registers the alias under the binding's own name (`make-tag`), not `Tag`, so even a working synthesis would not have populated the alias the test queried. Triaged as a test-authoring bug, not an implementation gap — every `result-def` unit test in `check.rs`, and the already-correct `parse-nums` example in `tests/harness/182_typedata_alias_resolution.eu`, use an explicit `type:` annotation; there is no code path that was ever meant to synthesise a concrete return type for an unannotated function. Fixed by annotating `make-tag` (`` ` { result-def: "Tag", type: s"any → { tag: symbol, value: any }" } ``, naming the alias explicitly) and converting the whole file's `RESULT` from a hardcoded `:PASS` to a computed aggregate (`[...] all-true? then(:PASS, :FAIL)`, matching 182's idiom), so a regression here fails the build instead of being masked again. Fault-injection verified: reintroducing the unannotated metadata against the new aggregate `RESULT` correctly fails (eu-2sa6.23)
- **`as-spec` on a union type (`A | B`) always produced an always-false predicate** — `union-spec(ts, v): ts map(to-spec-td) any(v match?)` wrote `v match?` as a bare catenation with no explicit spec argument, which partially applies `match?(pat, target)` with `v` bound as the **pattern** (`pat`), not the value being matched — the opposite of the intended `match?(sub-spec, v)`. Every sub-spec test was therefore nonsensical and `union-spec` always returned `false`, regardless of which branch of the union the value actually matched. `union-spec` now uses a named helper (`matches-v(s): v match?(s)`, mirroring `partial-spec`'s existing correct catenation) so each sub-spec is tested as the pattern against the fixed value `v`. Regression-tested via `tests/harness/189_r9oy_union_as_spec.eu` (accept/reject across primitive, list-nested, and record-nested unions) and extended AC4 coverage in `tests/harness/175_sv2_to_spec.eu` (eu-r9oy)
- **Type diagnostics are now computed pre-inline, so inlining can no longer hide a type warning** — the `eu` evaluate path previously ran the warning-emitting type check on the *post-inline* core, so any inlining that eliminated the application a mismatch hangs on (e.g. inlining `wrap("hello")` to `"hello"` when `wrap` is `` `{type:"number->number"} ``) silently dropped the warning. Warnings are now captured on the pruned, pre-inline core inside `prepare()` and emitted by the binary, matching `eu check` (which already checked pre-inline) — inlining is an optimisation and must not change diagnostics. **`--strict` implication:** a programme that inlined away a genuine type mismatch and previously passed `--strict` will now correctly fail it; run `eu check` to see the warning. A full-corpus sweep (harness + `lib/`) found **zero** programmes whose warnings change from this alignment alone. Prerequisite for source-prelude combinator fusion (eu-rb5n)
- **Eval-path type check now sees the prelude's actual definitions, not just its declared type schemes, without paying the prelude-source compile** — the blob path's eval-time check silently missed record-membership constraints (e.g. `lookup(:naem, person)` against a closed record type) because verifying them needs `lookup`'s actual body, not just its declared signature. The journey: an initial pre-inline capture missed this case (false negative on the record-key-typo harness test); a post-inline capture introduced three new false positives; falling back to the fully merged `run_type_checker` (the same pipeline `eu check` runs) was correct but added ~22ms of prelude-source-compile startup cost on every invocation; seeding the checker from only the blob's prelude type *summary* was fast but fundamentally cannot see record-membership constraints, since those need the prelude's bindings, not their types (a source-built summary fails the same case, ruling out a blob-generation bug). The blob now additionally bakes each prelude-side unit's post-translate core (`__build`, `__io`, `__args`, and the prelude itself — postcard-encoded, ~132 KiB, decoded lazily only when a warning-emitting check actually runs) so `run_type_checker_from_blob_core` can reproduce `run_type_checker`'s merge → cook → eliminate → type-check pipeline exactly while skipping only the prelude source load + translate. Verified byte-equal `eu check --strict` output between the blob-core and source-prelude (`EU_SOURCE_PRELUDE=1`) paths across the full harness and `lib/` corpus (515 unique files — 509 recursively under `tests/harness/`, including the 111 in `tests/harness/typecheck/`, plus 6 in `lib/` — zero diffs); against the true prior fallback (blob active for eval, `run_type_checker` only for the check itself — not the strictly harder `EU_SOURCE_PRELUDE=1`, which also disables the blob for eval), the eval-path check overhead drops from ~23–30ms to ~10–17ms, a ~1.8–2.4x reduction — short of an initial ~2ms aspiration (the remaining cost is the merge/cook/eliminate/type-check pipeline itself, not the postcard decode), but it never approaches the refused ~22ms-adding compromise. A blob without a baked core (stale wire format, or generated before this field existed) falls back explicitly and correctly to `run_type_checker` for that invocation (eu-rb5n)
- **Source-prelude path now fuses recursive higher-order combinators (`foldl`, `map`, …) exactly like the blob path, closing a source-vs-blob divergence that left `EU_SOURCE_PRELUDE=1` quadratic on the same workload the blob path made linear** — three independent meta-blindness bugs, each in a different pipeline stage, combined to silently disable the eu-dp0k copy-specialisation criterion on the source path (the blob path was unaffected because `xtask` peels `Meta` wrappers before baking `inlinable_bindings`, so the blob path never exercised these code paths at all): (1) `reduce::distribute`'s inline-collection step only recognised a bare `Lam(_, true, _)`/`Intrinsic`, so a documented/annotated combinator (`Meta(Lam, {doc, type})` — the shape of every prelude binding on the source path) was never collected for distribution to call sites at all; (2) even once distributed, `analyse_demand`'s named-signature recording had the same bare-`Lam` blind spot, so the STG compiler never learned the distributed copy's own per-argument demand signature; (3) `compiler::compile_binding`'s own `Expr::Meta` case explicitly discarded the `self_recurse` context (passed `None`) when descending into the wrapped value, so even a canonical, correctly-`demand.recursive`-flagged combinator's own recursive self-call was never recognised as self-recursive at compile time — silently losing the eager-argument evaluation that prevents an O(n) thunk-chain from building up over the recursion, which is what actually produces the O(n²) forcing cost. Fixed all three (peeling `Meta` in `distribute`'s inline collection and `analyse_demand`'s signature check; threading `self_recurse` through `compile_binding`'s `Meta` case), plus added a compile-time pre-expansion fixed point in `distribute` (mirroring the blob path's xtask-baked pre-expansion) so a self-recursive copy's non-recursive helper calls resolve to their own bodies before distribution, matching blob's pre-expanded shape. Verified via `EU_SOURCE_PRELUDE=1`: the canonical `range(0,10000) foldl((_+_),0)` benchmark drops from ~52.0M ticks (effectively unfused; the criterion was landing but never activating) to 1,500,319 ticks on the bytecode engine (1,640,321 on `EU_HEAPSYN=1`) — linear scaling confirmed on the bytecode engine across N=5,000/10,000/20,000 (750,319/1,500,319/3,000,319, ratios 1.9996×/1.9998×), beating the blob-path figures of 2,085,398 (bytecode) / 2,185,410 (HeapSyn), which this fix leaves unchanged — see the combined table above under "Changed" for the full before/after/engine breakdown. `--suppress-type-warnings`/type-annotation safety unaffected (type checking already runs pre-inline, per eu-rb5n Q1, before any of these passes execute; harness 104 unchanged on both paths) — a naive `Meta`-strip would have been type-unsafe, but peeling only within the inline/fusion machinery (never before the type check has already run and captured its diagnostics) avoids that hazard entirely. Compile-time impact measured negligible (within noise, ±3%) on both configs. First-order self-recursion (`fib`, `take`, `iota`, …) is unaffected by design (the copy-specialisation criterion only admits *higher-order* self-recursion) — the separate eu-npp9 fib-based tick-parity tripwire is untouched by this change, and its own ratio (currently ~1.000002, effectively at parity already) is reported for visibility but not claimed as fixed here (eu-rb5n)
- **Documented and tripwired the source-prelude fallback's performance handicap** — root-caused the +10.6% tick regression on arithmetic-dense, strictly-recursive code (`fib(30)`: 88,853,885 blob-path ticks vs 98,277,770 on `--source-prelude`/`EU_SOURCE_PRELUDE=1`) to demand-analysis strictness divergence, *not* a fused-primop gap (`emit_fixtures_and_globals` already emits byte-identical fused encodings on both paths). The blob path keeps prelude references `Var::Free` until `inject_prelude_inline_cores` exposes their strict shape to demand analysis before the general inline pass; the source-prelude path merges the prelude directly, resolving every reference to `Var::Bound` before `cook()` runs, so no free-variable name is left for an equivalent injection to catch (confirmed via `eu dump cooked --debug-format`: zero `Var::Free` nodes on that path). A full unification of the two prelude-loading pipelines is deferred to a follow-on bead so it doesn't run ahead of the 0.13 lever-(a) predecoded-IR work. This is **not a fix** — it adds `tests/tick_parity_test.rs`, a tripwire asserting the source-prelude handicap on a small fused/strict-recursion fixture stays within its documented bound (capped at 12%), and `docs/development/prelude-blob.md`, documenting the handicap and its cause. Released binaries are unaffected — they always embed the pre-compiled blob (eu-2sa6.5)
- **Windows absolute paths misparsed as URLs, failing every fully-qualified `eu C:\path\file.eu` invocation** — `Locator::from(&str)` attempted `Url::parse` before falling back to a filesystem path; a Windows drive letter (`C:\...`) is syntactically a valid single-letter URL scheme under RFC 3986, so every absolute Windows path became `Locator::Url` instead of `Locator::Fs`. No code path actually reads a `Locator::Url` back other than `resource:`/`file:` schemes (`SourceLoader::load_source`'s catch-all rejects anything else as `FileCouldNotBeRead("unsupported locator: ...")`), so this silently broke every real Windows user passing a fully-qualified path — it went undetected because no existing test fed a raw absolute Windows path through the CLI-string parsing route. `Locator::from` now recognises a drive-letter prefix (single ASCII letter, `:`, then `\` or `/`) and routes it straight to `Locator::Fs` before attempting URL parsing; none of eucalypt's actual supported schemes are a single letter, so this cannot misclassify a genuine URL. Regression-tested via `Locator::from("C:\\foo\\bar.eu")`/lowercase/forward-slash variants (deliberately not `#[cfg(windows)]` — pure string parsing, must be caught on any CI platform), alongside a sanity check that real URLs still parse as `Locator::Url`. Found via a Windows CI failure surfaced by the eu-rb5n Smid regression test (eu-2sa6.19)
- **Debug test-suite stack overflow from broader-than-scoped `Meta`-aware self-recursion detection (Wicket review of PR #1016)** — extending eager-argument resolution to `Meta`-wrapped self-recursive combinators (the fix above) legitimately applies to nearly every prelude binding, since almost all of them carry a `doc`/`type` annotation; the debug build's larger per-frame footprint then needed more native stack depth than `cargo test`'s in-process worker threads provide by default. Investigated per Wicket's explicit direction: a targeted repro (a `cond`-gated self-recursive local function, structurally identical to `lib/reflect.eu`'s `to-str[tag:rest]`, with a branch that references neither of two locally-bound recursive calls) confirmed laziness is preserved — the unreferenced branch's recursive call, whose argument is not list-shaped and would fail destructuring if forced, is never forced. The actual cause is that `tester::test()` (what harness tests call in-process) never goes through the production `eu` binary's `main()`, which already spawns a dedicated 64 MiB thread (`src/bin/eu.rs`) for exactly this class of native-stack-depth workload — `cargo test`'s own worker threads have no equivalent provision, a pre-existing gap `test_bench_021_io_loop` already documents and works around for a single test. Rather than whack-a-mole per-test stack overrides, `.cargo/config.toml` now sets `RUST_MIN_STACK=67108864` (64 MiB, matching `eu`'s own `STACK_SIZE`) so every `cargo test` worker thread gets the same headroom the production binary already has; this only affects test-thread sizing (the production binary already explicitly overrides its own stack size, unaffected by this env var) and does not mask genuinely unbounded recursion, since 64 MiB is the same fixed, already-accepted budget `eu` ships with, not an escalating one. Verified: all 499 harness tests pass under `cargo test` (debug), on all three engines (bytecode, `EU_SOURCE_PRELUDE=1`, `EU_HEAPSYN=1`), and under `EU_GC_VERIFY=2 EU_GC_STRESS=1`. Regression-tested via `tests/harness/189_branch_gated_self_recursion.eu`, which panics loudly if the unreferenced branch is ever forced (eu-rb5n, PR #1016)

  **Known limitation**: compiling a scope containing many mutually-referencing, documented (`Meta`-wrapped), self-recursive higher-order combinators now needs meaningfully more native stack than before — quantified by Wicket via `RUST_MIN_STACK` binary search on a direct (non-`cargo test`) `eu` invocation: debug needs 4–8 MiB versus master's <512 KiB (8–16×), release needs ~800 KiB–1 MiB versus master's ~48 KiB (17–21×). The mechanism is `pre_expand_inlines`'s O(N²) pre-expansion over N Meta-wrapped self-recursive bindings in one scope (N=143 for the whole prelude, matching xtask's own "inlinable bindings total (fixed-point): 143" figure) — a largely fixed, one-time cost tied to *scope size*, not to input data size (`range(0,1000000) foldr((_+_),0)` completes without incident on this branch, confirming it is not a growing-with-input-size risk). Production's `eu` binary has substantial absolute headroom against this (its 64 MiB thread against a worst-case low-single-digit-MiB need), so this is not expected to affect real programmes, but a user writing an unusually large set of mutually-referencing documented recursive functions in one scope of their own code could see a real, if narrow, native-stack-depth increase
- **`lib/markup.eu` accessors (`tag`, `attrs`, `content`) silently undefined** — the three declarations used `=` instead of `:` (e.g. `tag = head`), which Rowan's lenient parser accepted as declaration metadata rather than raising a parse error, so the names were never actually bound. In combination with a further malformed doc string on the `content` declaration, the whole file in fact failed to load at all (`empty expression where a value was expected`), masking the underlying typo. All three declarations are now proper `:` bindings and `lib/markup.eu` loads and evaluates correctly; `markup.eu` is now also available as a baked-in resource import (matching `lens.eu`/`state.eu`/`reflect.eu`), consistent with its documented status as a shipped library. Found by the parser-equivalence audit (PR #995). Regression-tested via `tests/harness/186_markup_accessors.eu` (eu-2sa6.16)
- **`deconstruct_head` dropped repeated `.class` fragments** — the hiccup-style tag shorthand parser in `lib/markup.eu` (e.g. `:'a.red.big'`) folded per-fragment qualifier blocks with a shallow `merge`, so a later `{class: "big"}` fragment silently overwrote an earlier `{class: "red"}` instead of the two combining. Multiple `.class` fragments are now space-joined (`"red big"`), matching hiccup convention; a single `#id` is still taken. The bug was latent and undetectable until the `=`/`:` typo above was fixed, since the file failed to parse at all beforehand. Regression-tested via `markup.eu`'s own embedded `test-deconstruct-head` target, now auto-discovered and run by the harness (eu-8t2j)

### Changed

- **Removed the dead `PreludeBlob::type_summary` field (internal, pre-release)** — Wicket-verified during PR #1012 re-review that `type_summary` (a baked `PreludeSummary`: type schemes, aliases, branch shapes) was write-only, with `xtask/src/main.rs` its sole writer and zero readers anywhere in the runtime; its only consumer (the interim summary-seeded eval-path check) was deleted in PR #1012 in favour of the `desugared_unit_cores`-based merged check. Removed the field, its bake logic in `xtask` (the now-pointless `PreludeSummary` extraction, and the `operator_overloads` plumbing that only existed to populate it — `xtask` now runs a plain `type_check` as a build-time sanity pass instead), and the corresponding row in `blob.rs`'s module-doc anatomy table. `UnitInterface::type_summary` (`driver/unit_interface.rs`) and the `PreludeSummary` type itself are unrelated and untouched — that's a live, separate consumer (`driver/check.rs`'s `build_prelude_interface_for`, which derives its own summary by re-running the type checker over freshly-loaded prelude source, independent of the blob). `BYTECODE_WIRE_FORMAT_VERSION` bumped 3 → 4 (postcard is positional; removing a field shifts every subsequent field's byte offset) — following the same precedent as the v1→v2 bump for the `u16`→`u32` binding-count width change. `lib/prelude.blob` regenerated (598,398 → 596,625 bytes, −1,773 bytes / −0.30%; two independent regenerations byte-identical). Free to do pre-release: wire v3 has never shipped in a release, so there is no on-disk v3 blob to be backwards-compatible with (eu-2sa6.20)

## [0.12.1] - Unreleased

### Fixed

- **Bytecode panic on very large imported literals** — importing a document that splices into a single recursive binding group of more than 65,535 bindings (e.g. a TOML array-of-tables above ~2,500 full-schema records) truncated the `LetRec` binding count (`u16`), desynchronising the byte stream and crashing the VM with `bytecode: invalid opcode`. The binding count is now 32-bit (bytecode wire format v2; the embedded prelude blob auto-regenerates via a format-version hash), and the one residual hard limit — a compiled code section exceeding the 32-bit offset space — is now reported as a proper compile error instead of a panic. Regression-tested at the previously failing size on both engines (eu-2sa6.11)

### Changed

- **Bytecode-vs-HeapSyn decode-bound gap substantially narrowed (not closed)** — primop fusion (`Op::FusedPrimop`, inlined at strict-binary-primop call sites) cuts VM ticks materially on arithmetic-heavy code (naive `fib`: −23% ticks); per-op transient system-heap `malloc`/`free` was removed from the `LET`/`BIF` hot paths; `ExecutionError`'s hot-path payload was boxed (128 → 40 bytes). A residual per-tick decode/dispatch envelope on allocation- and call-dense workloads remains — it is architectural (re-reading the byte stream on every step) rather than fusion-addressable, and is characterised, not eliminated, by this work. Its removal (decode-once into a pre-decoded, off-heap execution IR) is the ratified plan of record for 0.13; see `docs/superpowers/specs/2026-07-12-bytecode-transition-review.md`. (The W3 block index was evaluated and deferred — eu-4zhi, PR #985 — pending real static-large-block evidence; it is not part of this release.)
- **Per-op system-heap `malloc`/`free` removed from the `LET`/`BIF` hot paths (eu-cj1h.3)** — completes the transient-allocation reduction begun in eu-w2oy. `Op::Let` now decodes its form headers straight into the managed env-frame `Array` (mirroring the shipped `Op::Case` branch-table read) instead of buffering a transient `Vec<FormHeader>` on the system heap on **every** thunk/closure allocation; `Op::LetRec`, which must fill its array in a second pass over the frame it closes over, buffers the headers in an inline `SmallVec` (decoded once, off the system heap for the common small-arity case). BIF dispatch recycles its resolved-args `Vec<Ref>` through a per-machine free-list rather than `malloc`ing a fresh buffer per call — the buffer stays a heap-stable `Vec` (not an inline `SmallVec`) so the GC-rooted `BifFrame` args slice the dispatch tail aliases across `execute` cannot dangle when a nested dispatch reallocates the frame stack. Layout/allocation-strategy only: the whole harness is byte-identical on both engines, and GC behaviour is unchanged. On the `drop_cons` alloc-bound probe the system `malloc`/`free`/`memset` profile share falls from ~4.6% to ~0.2% of mutator self-time (~8% mutator wall). A **partial** win on allocation-bound workloads (the residual bytecode↔HeapSyn gap is dominated by fusion-immune byte-stream *decode*, which is architectural and out of scope here). See `docs/superpowers/reports/2026-07-12-allocation-gap-spike.md`.
- **`Op::FusedPrimop` superinstruction (eu-9mvh, lever a)** — a single generic bytecode opcode that force-and-binds both operands of a strict binary primop VM-natively and defers all arithmetic/comparison/type-check/error handling to the existing, unmodified `Op::Bif` dispatch tail. Emitted by the encoder in place of the `binary_wrapper` force-dispatch `Case`-of-`Case` tree for a whitelisted set of intrinsic *global forms* (`ADD`/`SUB`/`MUL`/`DIV`/`GT`/`GTE`/`LT`/`LTE`), intercepted by intrinsic identity — `StgSyn`/HeapSyn are untouched, so the full harness stays byte-identical on both engines. Modestly faster (~7%) on first-class-operator-heavy, allocation-light workloads that route through the intrinsic global form (e.g. `foldl(+, …)`, `qsort(<)`); neutral elsewhere with no regressions. See `docs/superpowers/specs/2026-07-10-fused-primop-superinstruction-design.md`.
- **Direct-arithmetic fusion — inline `Op::FusedPrimop` at the call site (eu-9mvh, Option C)** — direct strict-binary-primop call sites (`n <= 1`, `a + b`, …) are compiled by inlining the `binary_wrapper` body at the call site, so they previously bypassed the fused global form and kept the 25-leaf `Case`-of-`Case` decode. The STG compiler now wraps the inlined body of a whitelisted primop in a new `StgSyn::FusedPrimop` marker carrying the intrinsic identity and the two operand refs; the bytecode encoder collapses the marker to a single inline `Op::FusedPrimop` reading the operands straight from the enclosing environment — no App+enter and no operand env frame. HeapSyn and every other `StgSyn` consumer transparently execute the wrapped wrapper body, so both engines stay byte-identical (whole harness green under `EU_HEAPSYN=1` too). The whitelist and force-dispatch shape guard are shared with the encoder's global-form path. Naive `fib(30)`: **−23% VM ticks (115.8M → 88.9M)** and **−34% bytecode wall** versus master, with **no regression** on the previously inline-cheap cases (`drop_cons`, AoC day07-p2) — reusing #980's already-GC-verified opcode, continuations, and dispatch tail.
- **Bytecode hot-path error payload boxed** — `ExecutionError`'s heavy variants (`Traced`, `LookupFailure`, `TypeMismatch`, `BadDateTimeComponents`, `BadRegex`, `ParseError`, `VersionRequirementFailed`, `AssertionFailed`, `FormatError`) now carry their string/vector/tuple payloads behind a single `Box`, shrinking `size_of::<ExecutionError>()` from 128 to 40 bytes (68.75%). This retires the oversized `Result<_, ExecutionError>` the bytecode VM's per-instruction dispatch loop was moving and dropping on every step. Layout-only change: error messages, diagnostics, and harness output are byte-identical on both engines (eu-adnu).

## [0.12.0] - 2026-07-04

### Changed

- **The bytecode VM is now the default execution engine (BV1)** — evaluation runs on the new bytecode interpreter: code lives in an off-heap arena as a flat opcode stream and is no longer allocated into, scanned by, or evacuated through the garbage collector. The full test harness is byte-identical on both engines. The legacy HeapSyn tree-walk engine remains fully supported via `EU_HEAPSYN=1` and is retained as the performance baseline and differential-testing reference (`EU_BYTECODE=1` is now a redundant no-op). Known trade-off, to be closed in 0.12.1: allocation- and call-dense programs currently run ≈1.2–2.1× slower than HeapSyn (instruction-stream decode cost; the extant worst measured ratio was fib at 2.14×), pure-dispatch and higher-order/env-walk workloads are at parity or faster. See `docs/superpowers/reports/2026-07-03-bytecode-vs-heapsyn-ab.md` for the full engine comparison
- **XML attribute values are now whitespace-normalised (BEHAVIOUR CHANGE)** — following the `quick-xml` 0.25→0.41 security bump, XML import now applies XML 1.0 §3.3.3 attribute-value normalisation, as required of a conforming processor. Each **literal** whitespace character (tab, carriage return, line feed) inside an attribute value is replaced by a single space. Raw newlines/tabs in attribute values therefore no longer survive import verbatim as they did in every previous release (which was non-conformant). To carry literal whitespace in an attribute value, use character references: `&#10;` for a newline, `&#9;` for a tab — these are resolved before normalisation and survive intact. **Element text content is unaffected.** (eu-cgys)

## [0.11.1] - Unreleased

### Changed

- **`eu.build` metadata sanitised** — the `github-env` block that captured all `GITHUB_*` environment variables from the CI runner (including filesystem paths, actor IDs, and workflow internals) has been replaced with a curated set of build metadata: version, commit, build number, URL, timestamp, target triple, Rust version, build profile, and prelude blob status (#917)
- **`to-data` / `from-data` moved to `lib/reflect.eu`** — moved out of the prelude into the optional `reflect` library (`{ import: "reflect.eu" }`); the prelude re-exports both for backward compatibility. `to-spec` removed — `as-spec` is now the single canonical name (#920)

### Fixed

- **Type alias resolution in `--source-prelude` mode** — dead-code elimination pruned `type-def:`/`result-def:` metadata (and the aliases it defines) before the type checker ran, because s-strings reference types by opaque name and DCE couldn't see the dependency. Type aliases are now collected in a full type-check pass before target pruning. Also fixes blob-mode detection of `type-def: true` metadata (#918)
- **Literal types widened in synthesised `type-def:`/`result-def:` aliases** — a synthesised (non-annotated) type alias no longer pins to the literal value it was inferred from: `:circle` widens to `symbol`, `3.4` to `number`, `"hello"` to `string`. Explicit `type:` annotations are unaffected and remain precise (#919)

## [0.11.0] - 2026-06-27

### Added

- **Type checking always-on (TY)** — the gradual type checker now runs on every `eu` invocation without any flag. Previously it required `--type-check`. Type warnings are emitted to stderr before evaluation and never affect stdout or exit code (unless `--strict`) (#906)
- **`--suppress-type-warnings`** — new flag to silence type-warning output. The checker still runs; warnings are computed but not printed (#906)
- **`s"…"` type-data value semantics (SV1)** — s-strings are now validated at compile time via the type-DSL parser. Invalid type syntax (e.g. `s"42"`) produces a source-located compile error. `to-data` projects type-data to `t-*` tagged-list form; `from-data` rebuilds it. `TYPE_TO_DATA` / `TYPE_FROM_STRING` intrinsics support the projection (#907, #914)
- **`to-spec` / `as-spec` (SV2)** — prelude functions that convert `s"…"` type-data into `match?`-compatible patterns for runtime validation. Covers all type forms: primitives, records, lists, unions, functions, optionals, tuples, forall (#908)
- **`datetime?` predicate** — new `__ISZDT` intrinsic (index 194) for runtime datetime type checking (#908)
- **Optional record fields** — type-DSL syntax `name?: T` for fields that may be absent, distinct from `name: T?` (present but nullable). Presence subtyping, checker handling, `to-data` projection with `[:t-field :optional T]`, `to-spec`/`match?` integration (#909)
- **Known-call direct dispatch (CG1)** — new `StgSyn::DirectApp` node for exact-arity calls to known callees (both `Ref::G` globals and `Ref::L` locals). Bypasses `ApplyTo` continuation push and runtime arity check. Source-prelude and blob-prelude parity (#905, #913)
- **Literal-key resolution (CG2)** — new `StgSyn::LookupLit` node for `.key` lookups with compile-time-known keys. Single VM step with WHNF fast path and `LookupOr` fallback for thunked blocks (#910)
- **Eager argument resolution at recursive calls (CG3)** — at self-recursive call sites, `Ref::L` arguments are resolved eagerly via `env.get(i)` instead of being wrapped as lazy indirections. Eliminates the O(n²) indirection chain in higher-order folds — `foldl` with a local `op` now scales linearly (19× speedup at N=10k). Unused arguments are not forced (soundness verified) (#911, #915)

### Changed

- **`--type-check` is now a silent no-op** — the flag is accepted for backward compatibility but type checking is always on (#906)

### Fixed

- **Type checker false positives eliminated** — six checker improvements eliminate all spurious warnings across the full harness and AoC corpus (#906):
  - Literal types (`"apple"`, `:active`) widen to their base type (`string`, `symbol`) when binding a type variable
  - Row-variable records treated as effectively open in subtype and lookup checks
  - Gradual consistency (`~`) used in the overloaded-function fallback path
  - Row variable absorption preserves source openness only when there are actual extra fields
  - `synthesise_lookup` handles row variables
  - `io.exec` and `io.exec-with` prelude type annotations corrected from `string` to `[string]`
- **Prelude doc extraction leakage** — `extract_children` no longer walks into function bodies, preventing internal helpers (e.g. `from-data.prim-names`, `monad.step`) from leaking as public prelude entries (#907)

## [0.10.1] - 2026-06-25

### Changed

- **Legacy parser removed** — the pre-Rowan hand-written parser (`ast.rs`, `kind.rs`, `lex.rs`, `make.rs`, `parse.rs`, `string_lex.rs`, `validate.rs`) has been deleted. The Rowan-based parser is now the sole parser (#902)

### Fixed

- **Idiot-bracket expressions as juxtaposed call targets** — an idiot-bracket expression immediately followed by a call bracket now parses as a call rather than as catenation with a literal. `⌊map(_ * 2)⌋[1, 2, 3]` calls the bracket expression with `[1, 2, 3]`, and `⌊lookup(:b)⌋{a: 1, b: 99}` with the block (#899, #901, #902)

## [0.10.0] - 2026-06-22

### Added

- **Strict eager evaluation (W11)** — demand analysis marks strict bindings and the STG compiler wraps them with `Seq` forms to force evaluation at definition time, avoiding thunk allocation. -38% allocations, -34% ticks on 010_prelude; -76% on 008_folds
- **Prelude demand signatures** — ~60 common prelude functions have conservative strictness signatures enabling strict eager eval at call sites
- **SCC splitting + reflatten** — LetRec scopes decomposed by dependency analysis for more precise demand, then merged back to preserve flat env frames
- **User function demand signature wiring** — named signature table threaded from demand analysis into STG compiler for user function call sites
- **`eu dump split`** — dump intermediate representation after SCC splitting
- **`eu dump reflatten`** — dump after re-flattening
- **`eu dump runtime` in blob mode** — now shows complete runtime including named prelude global slots
- **Git imports restored (W12)** — `{ import: "git://..." }` with bare clone cache and commit pinning
- **Namespace lambda hoisting** — namespace members (str, cal, vec) hoisted to top level for inlining, including in blob pipeline
- **Dev version clarity** — dev builds now report `v0.0.0.dev` instead of a stale version number
- **Prelude blob gitignored** — the blob is a build optimisation, not source; build falls back to source-prelude mode when absent
- **`io.args` / `io.env` integration tests** — 10 binary-level tests verifying runtime pseudo-inputs work correctly with the prelude blob

### Fixed

- **Blob prelude stale `io.args` / `io.RANDOM_SEED`** — the pre-compiled prelude blob baked `__args` and `__io` pseudo-inputs with empty/default values at blob compilation time, so `io.args`, `io.RANDOM_SEED`, `io.epoch-time`, and `io.env` returned stale data at runtime. The executor now compiles fresh override `LambdaForm`s for these slots from the actual command-line arguments and seed (#897)
- **Assertion failure `<string>` display** — `//=` failures on interpolated strings showed `<string>` instead of the actual value. `render_debug_repr_forced` now evaluates inner thunks via `evaluate_to_whnf` (#898)
- **Assertion failure source location** — `//=` failure diagnostics pointed at the prelude's `//=` definition rather than user code. When no user-file location is available, the prelude label is now suppressed (#898)
- **Type checker µ-type cycle** — `is_consistent` entered infinite recursion on mutually recursive µ-types. Added coinductive cycle detection with a pending-pair set (#896)
- **LSP stack overflow** — pipeline threads could overflow on deeply nested expressions; now use 64 MiB stacks
- **Prelude demand signatures** — `cons` was incorrectly marked strict, forcing list spines eagerly and causing stack overflows on large lists (P0)
- **HeapNavigator hot-path overhead** — `ok_or(ExecutionError)` replaced with `match` to avoid constructing/dropping error values on the success path. -20% mutator CPU
- **HeapNavigator inlining** — `#[inline(always)]` on `get`/`global` eliminates call overhead + register spills. ~3-7% wall time improvement
- **Array::push overhead** — redundant bounds check and Option unwrap eliminated in pre-allocated push path. -4.5-6% wall time
- **Array allocation drop glue** — `RawArray` allocation helpers changed from `Result<_, ExecutionError>` to direct panic, eliminating non-trivial drop glue on success path
- **YAML timestamp regex** — `is_timestamp()` compiled regex on every call; moved to `lazy_static!`. -5% startup overhead
- **AoC head-retention** — day01, day04, day09 refactored from map-then-aggregate to foldl patterns. day01-p2: -95% wall time (9.7s → 0.46s)
- **YAML complex-key panic** — converted to proper error diagnostic

## [0.9.2] - 2026-06-20

### Added

- **Statistics on interrupt** — pressing Ctrl-C during a long-running program now prints partial statistics to stderr before exiting with code 130, instead of silently terminating

### Fixed

- **CI release versioning** — the prelude blob is now rebuilt with the freshly regenerated `build-meta.yaml` during CI release builds, so `eu version` reports the correct version instead of a stale development string
- **`eu version` reports correct build number** — the xtask prelude compiler now reads `build-meta.yaml` from disk rather than the compiled-in resource, so the CI-regenerated version flows through to the final binary

## [0.9.1] - 2026-06-19

### Added

- **Enhanced statistics output** — sectioned layout (Machine, Heap, GC, Pipeline, IO, VM) with inline bar charts, thousands separators, section subtotals, and a top-level percentage summary bar
- **`--heap-limit-mib` in test mode** — shared flags (`-d`, `-S`, `-L`, `-Q`, `--type-check`, `--heap-limit-mib`) now available in `eu test` via `CommonArgs` extraction
- **`--heap-limit-mib` at top level** — the flag works regardless of whether the `run` subcommand is explicit
- **JSON Schema from type parser** — `eu doc --format json` now uses the real type parser instead of naive string matching, producing correct schemas for record types, tuples, DateTime, literal types, and union types
- **LSP test hardening** — `col_of` helper computes UTF-16 column offsets from source text, eliminating 16 magic column numbers; `assert_goto_def_line` helper for go-to-definition assertions; 3 vacuous `is_some()` assertions strengthened
- **CLI parsing tests** — 26 characterisation tests for command-line argument parsing

### Changed

- **CLI options refactored** — duplicated fields between `EucalyptCli` and `RunArgs` extracted into `CommonArgs` struct; `From<EucalyptCli>` simplified by eliminating the `cmd_foo.unwrap_or(cli.foo)` reconciliation pattern
- **Dead `strict` field removed** — `EucalyptOptions.strict` was always `false` and never read; the actual `--strict` functionality uses `check_strict`

### Fixed

- **GC performance** — end-of-run garbage collection was unconditional, running a full mark/sweep after every `machine.run()` call (including each IO step). Now guarded by `policy_requires_collection()` so it only runs when the heap is under memory pressure. For IO-heavy programs this eliminates the dominant source of GC overhead

## [0.9.0] - 2026-06-16

### Added

- **Pre-compiled prelude blob (W6)** — the prelude is pre-compiled at build time into a binary blob, eliminating parse/desugar/cook on every invocation. ~40% faster startup for short programs
- **Prelude inline cores** — combinator prelude functions (arithmetic, comparisons, `if`, `head`, `tail`, etc.) are pre-expanded and injected as inlinable Let bindings, restoring direct BIF call performance on the blob path
- **Incremental query-based LSP pipeline (W7)** — per-stage caching via `QueryStore` replaces the monolithic `CachedPipeline`. Editing file A no longer re-parses file B; cross-file invalidation triggers re-checking of importers
- **Demand annotations on core bindings (W9)** — `CoreBinding` carries cardinality and strictness metadata through the pipeline for future update-elision optimisations
- **`eu doc --output-dir`** — multi-file categorised documentation output with namespace docstrings, unit-level documentation, and supplement merging
- **Prelude docs freshness CI check** — CI verifies that `docs/reference/prelude/` matches `eu doc --prelude --output-dir` output
- **Declaration-level parser error recovery (W4)** — syntax errors in one declaration no longer prevent parsing of subsequent declarations; errors are wrapped in ERROR nodes with real token spans
- **RcExpr property tests (W5)** — render-parse round-trip property tests for core expressions using `proptest` arbitrary generators
- **Fuzz targets** — `cargo-fuzz` targets for parser and loader with seed corpora and regression tests
- **`--statistics` timing for render and IO phases** — `stg-eval`, `stg-render`, and `io-run` timings now correctly reflect all execution phases (previously only the initial headless eval was timed)

### Changed

- **Test harness uses prelude blob** — the test runner now exercises the same blob code path as the default `eu` binary, ensuring blob-specific regressions are caught. `EU_SOURCE_PRELUDE=1` continues to test the source prelude path
- **`test`/`plan_only` flag split** — the overloaded `test` flag is split into `test` (invocation context) and `plan_only` (skip cook in prepare), preventing the test runner from accidentally short-circuiting the pipeline
- **Deep combinator tagging** — the inline pass's combinator detection now accepts nested intrinsic application trees (not just flat ones), capturing functions like `abs`, `max`, `min`
- **Fixed-point inline core collection** — the blob generation iteratively discovers inlinable prelude bindings and pre-expands all free variable references at build time

### Fixed

- **Monad specs in prelude blob** — `:for` and `:random` monadic blocks now work correctly with the blob path. The blob carries monad namespace specifications and seeds them into the desugarer
- **Assignment-style false positive on prelude constants** — `null = null`, `true = x` etc. no longer trigger the assignment-style declaration diagnostic
- **Demand analysis performance regression** — the prune pass no longer emits `AtMostOnce` for core bindings (which caused catastrophic re-evaluation in recursive functions like naive fib: 6s → 60s+)
- **`eu doc` UTF-8 panic** — multi-byte em-dash/en-dash characters in doc comments no longer cause a slice panic
- **`UnitInterface.demands` not populated in eval path** — `extract_demands()` is now called after the final eliminate pass in the main evaluation pipeline

## [0.8.1] - 2026-06-12

### Fixed

- **Type checker: docstrings on type aliases no longer break the alias** — adding a docstring to a `types:` alias entry previously caused the alias to silently resolve to `any`, disabling all type-checking through it (#810)
- **Type checker: interpolated strings no longer vanish from record types** — a block field with an interpolated value was dropped from the synthesised record type, producing spurious missing-field warnings (#811)

## [0.8.0] - 2026-06-12

### Added

- **Declaration trace metadata** — annotate declarations with `` ` :trace ``, `{ trace: :strict }`, `{ trace: :exit }`, or `{ trace: :strict-exit }` to trace entry arguments and/or exit values to stderr. Lazy mode peeks without forcing; strict mode forces arguments to WHNF before rendering
- **Deprecation metadata** — mark declarations deprecated with `` ` :deprecated ``, `{ deprecated: "message" }`, and optionally `{ replaced-by: "new-fn" }`. Emits compile-time warnings visible in `eu check` and LSP; `--strict` mode treats them as errors
- **Deprecation LSP quick-fix** — when a deprecated function has a `replaced-by` field, the LSP offers a code action to replace the old name with the new one
- **Prelude selection mechanism** — units can specify an alternative prelude via `{ prelude: "path/to/alt.eu" }` metadata. The prelude type cache is keyed per selection
- **`requires` unit metadata** — units can declare a minimum version via `{ requires: ">=0.8" }` in their metadata block, checked at load time before evaluation. Replaces the `_ : eu.requires(...)` binding pattern. Shipped libraries updated to use the metadata form
- **Stability policy** — published `docs/development/stability-policy.md` defining three tiers (Stable, Experimental, Not covered) and semver field meanings
- **Conformance corpus** — `tests/conform/` directory with 20 conformance files and `.golden` sidecars pinning rendered output bytes across all export formats. Separate `conform_test.rs` runner with `BLESS=1` regeneration and unified diff on mismatch
- **GC-verified CI job** — full harness under `EU_GC_VERIFY=2` + `EU_GC_STRESS=1` + `EU_GC_POISON=1` on x86-64
- **Static self-assignment diagnostic** — `eu check` now flags always-divergent self-referential bindings (`{ x: x }`, `{ f: f(...) }`) as errors. Argument-position self-reference (`ones: cons(1, ones)`) is not flagged
- **`select` and `dissoc` prelude functions** — `block select[:a, :c]` returns a sub-block containing only the listed keys; `block dissoc[:b]` returns a sub-block with the listed keys removed
- **`type-def: true` shorthand** — `type-def: true` in declaration metadata uses the binding name as the type alias (instead of requiring an explicit string). Bare `:type-def` symbol expands to `{ type-def: true }`. New `result-def` metadata key registers the function's return type as a type alias

### Changed

- **Semver-compliant build versioning** — release tags now use base semver (`0.8.0`) instead of four-dot build numbers (`0.8.0.1685`). The CI build number appears as `+build` metadata in `eu version` output only. Bug fix releases bump the patch version (`0.8.1`)
- **Resilient parser pipeline** — the all-or-nothing parse shim is removed; a file with syntax errors now produces a partial tree that reaches the desugarer, type-checker, and `eu dump` commands. Valid declarations in a file with errors still get diagnostics
- **`eu dump ast` shows Rowan syntax tree** — default output is now the indented Rowan node tree (kinds, ranges, token text) instead of re-rendered source. `--embed` gives the old behaviour

### Fixed

- **Black-holing in function position** — `{ f: f(1) }` now correctly raises "infinite loop detected" instead of hanging until heap exhaustion. The `App` handler mirrors the `Atom` handler's black-hole logic for local-ref callables

## [0.7.1] - 2026-06-09

### Added

- **`export: :internal` declaration visibility** — mark declarations as unit-private with `export: :internal` metadata (or the `` ` :internal `` shorthand). Internal bindings remain accessible within their own unit but are invisible to importers, mergers, and rendered output. Two units with same-named internal helpers no longer conflict

### Changed

- **Cross-import bracket pairs** — bracket pairs defined in an imported file now work correctly in the importing file. Previously, a block-mode bracket pair (e.g. `⟦{}⟧:`) defined in a library was silently mis-parsed when used in a file that imported it
- **Empty block-mode brackets are now an error** — a block-mode bracket pair used with no declarations produces a clear error instead of silently misbehaving

### Performance

- **Plain-document evaluation** — plain (non-IO) documents now evaluate ~15–20% faster by eliminating a redundant compilation pass

### Fixed

- **Format specifiers on arithmetic results** — `"{n:%02d}"` now works correctly when `n` is the result of arithmetic (e.g. from `iota`)
- **Several internal panics converted to proper errors** — malformed expressions that previously crashed the process now produce diagnostic error messages with source locations

## [0.7.0] - 2026-06-04

### Added

- **Higher-kinded type variables** — type-constructor variables (`m :: * -> *`) with `Type::Con`/`Type::App`, explicit `Kind` system (`*`, `* -> *`), `forall` quantification, and kind annotations in the type DSL
- **Higher-order pattern unification** — `Type::Lam` (type-level lambda) and Miller-pattern unification: when a `* -> *` variable is applied to a type variable and unified against a concrete type, the unifier abstracts the variable out to construct a type-level function. This makes `monad()` work for any monad — including those with non-constructor action types like `stream -> {value: a, rest: stream}` — via pure unification, with no special-casing
- **Monad namespace typing via HKT** — `monad()` carries a `forall (m :: * -> *)` type; calling it with `{bind: ..., return: ...}` infers `m` automatically and derives all nine combinator types (`map`, `then`, `join`, `sequence`, `map-m`, `filter-m`, etc.)
- **Dependent record indexed access** — `lookup(:k, b)` with a literal symbol returns the exact field type; absent key produces a static warning; non-literal key falls back to `any`. `ProjectionShape` classifier for `head`/`tail` on tuples
- **Prelude type-summary cache** — the prelude is type-checked once per process and cached; user files are seeded with the cached types, improving LSP responsiveness
- **Partial(T) / T? opaque type** — `ExecutionError` type and `T?` postfix sugar for fallible functions; warning when partial results flow into total positions; `nth`, `lookup`, `lookup-in`, `parse-as` annotated
- **Full row-variable inference** — unannotated parameters used as blocks infer row-polymorphic types instead of `any`
- **Structural operator constraints** — `<(a, a) => a -> a -> a` syntax constraining type variables by operator requirements; gradual when arguments are `any`
- **Tree-sitter t-string support** — `t"..."` ZDT literal rule in grammar, with Emacs and VS Code highlighting
- **VS Code type annotation snippets** — comprehensive snippet set for type annotations

### Changed

- **Monad namespaces no longer carry explicit type annotations** — `io`, `for`, `random`, `let`, `state` inherit their types from `monad()` via HKT unification. The `monad:` metadata is retained only for LSP element-type hints
- **Type representation** — six dedicated type variants (`List`, `IO`, `Dict`, `NonEmpty`, `Lens`, `Traversal`) replaced by `Con`/`App` constructor application

### Performance

- **HeapSyn clone avoidance** — raw pointer reference replaces 56-byte clone per VM tick (~9% faster on compute-heavy workloads)
- **Uninitialised array backing** — skip redundant zero-fill in `RawArray` allocation (~10% faster on allocation micro-benchmarks)
- **Partial application Vec elimination** — direct `Array` push replaces temporary `Vec` (~7.5% faster on curried calls)

### Fixed

- **Misleading string function hints** — `str.replace`, `str.starts-with?`, `str.ends-with?`, `str.contains?` all exist; error hints now point to them correctly
- **NotCallable / NotValue source locations** — errors now include the source span instead of a synthetic location
- **NumericDomainError source location** — NaN comparison errors now include the source span
- **BadTimeZone source location** — timezone parse errors now include the source span
- **Set type errors** — `Panic` replaced with proper `TypeMismatch` for set element and argument errors
- **Broken bench tests** — `fib(500)` overflow and `deep-find-perf` symbol/string key mismatch corrected

## [0.6.2] - 2026-06-02

### Added

- **Gradual type system — Dict types** — `Dict(a)` type for homogeneous key-value blocks with covariant subtyping; closed records widen to `Dict(T)` when all values share a type
- **Gradual type system — equirecursive types** — `type-def: { Json: "number | string | bool | null | [Json] | Dict(Json)" }` with `Mu` binder and coinductive subtyping for recursive aliases
- **Gradual type system — literal string types** — string literal expressions synthesise as `"hello"` (a singleton subtype of `string`); union smart-constructor deduplicates and absorbs
- **Gradual type system — flow-sensitive narrowing** — `x nil? then(a, b)` and `if(x nil?, a, b)` narrow `x` to `List(never)` in the then-branch and `NonEmpty` in the else-branch; works with `nil?`, `number?`, `string?`, `block?`, and user-defined branchers
- **Gradual type system — NonEmpty refinement** — `NonEmpty([a])` type for provably non-empty lists; `cons` and `‖` return `NonEmpty`; branch narrowing refines `[a]` to `NonEmpty` in else-branch of `nil?`
- **Gradual type system — first-class alias references** — type aliases are resolved in annotations; LSP provides hover (shows resolved type), go-to-definition, and rename for alias references
- **Gradual type system — monadic element-type hints** — `for.bind(m, f)` and similar monadic combinators extract the element type from the monad spec, enabling better inlay hints
- **`cond` multi-way conditional** — new `cond[c1 => r1, c2 => r2, default]` syntax using `=>` clause operator and `__COND`/`__CLAUSE` intrinsics
- **`nil?` intrinsic** — `nil?` is now a `NILP` intrinsic (direct tag check) instead of `= []`; semantically equivalent but faster
- **Cross-type equality** — comparing values of different types (e.g. `1 = [1]`) now returns `false` instead of erroring

### Changed

- **`cond` API (BREAKING)** — `cond(list_of_pairs, default)` is replaced by `cond[clause_list]`.  The old two-argument form `cond([[c1, v1], [c2, v2]], default)` must be rewritten as `cond[c1 => v1, c2 => v2, default]`.  The `=>` operator (precedence 15) builds clause pairs.  Internal callers (`max-of`, `min-of`, `parse-args`) have been updated
- **Prelude type annotations** — `lookup`, `keys`, `values`, `map-values`, `group-by`, `merge`, `cons`, `||` now have precise `Dict`/`NonEmpty`/row-polymorphic type annotations
- **Arithmetic native returns** — arithmetic operations return native atoms directly, avoiding one allocation per result (PR #720)

### Fixed

- **Source location for `BitwiseIntegerRequired` errors** — now includes the source span
- **`render-as` unknown format** — produces `UnknownRenderFormat` error instead of a panic
- **`cal.zdt` year overflow** — produces a proper error instead of a panic

## [0.6.1] - 2026-05-16

### Added

- **Named row variables** — `{k: T, ..r}` syntax in type annotations.  Row variables express "a record with at least these fields, plus unknown extra fields that are preserved through a transformation"
- **Record type propagation** — blocks applied to blocks (catenation) now propagate field types through the merge.  The type checker understands that blocks are callable
- **Monad namespace annotations** — `for`, `io`, `random`, and `state` have explicit record type annotations declaring all combinator types (bind, return, map, etc.)
- **Monadic bound variable type hinting** — LSP inlay hints show the unwrapped element type (e.g. `x: number`) instead of the raw wrapper type (`x: [a]`) for monadic block bindings
- **Symbol metadata shortcut for targets** — bare symbols in declaration metadata become targets: `` ` :test `` is equivalent to `` ` { target: :test } ``.  `:target` uses the declaration's own name
- **LSP pipeline backend** — the LSP server uses the SourceLoader pipeline as its primary semantic backend with debounced background type checking, replacing AST-level scraping
- **LSP incremental text sync** — the LSP uses incremental text document synchronisation with proper UTF-16 offset handling
- **LSP import resolution** — `{ import: "file.eu" }` declarations are resolved by the pipeline; go-to-definition, hover, and completion work for imported names
- **LSP bracket pair support** — semantic tokens, hover, go-to-definition, document highlight (matching), and inlay hints for idiot bracket pairs
- **LSP code actions** — structural editing: wrap as namespace (single or multi-declaration with reference prefixing), promote/demote metadata shortcuts, add metadata fields, let-block toggle
- **LSP prelude go-to-definition** — prelude functions resolve to a temp file with the prelude source, enabling go-to-definition navigation
- **LSP pipeline error diagnostics** — pipeline errors (e.g. invalid monad spec) surface as visible editor diagnostics with source locations
- **Prelude operator annotations** — ~30 remaining unannotated operators now have `type:` metadata
- **`eu` with no inputs shows help** — running `eu` with no files, no `-e`, and terminal stdin shows the clap help text instead of crashing

### Changed

- **LSP architecture** — per-document caching with background pipeline, green node change detection, `didClose` cleanup.  Type checking works on unsaved buffers

### Removed

- **Eufile support** — the Eufile project file mechanism and `Ergonomic`/`Batch` `CommandLineMode` distinction are removed.  `eu` now always behaves as former batch mode.  The `-B`/`--batch` flag is removed.  Scripts using `-B` should remove the flag

### Fixed

- **Bracket pair inline monad definitions** — `⟦{}⟧: { :monad bind(m, f): f(m) return(a): a }` now works; falls back to namespace monad when inline definitions can't be extracted as names
- **Row variable closedness** — row variable absorption uses `row.is_some()` for openness instead of inheriting anonymous openness from synthesised blocks
- **Parser bare colon recovery** — `{ : }` (typing a monad tag) no longer crashes the parser; the LSP can offer completion

## [0.6.0] - 2026-04-25

### Added

- **Gradual type system** — optional, structural type checking for eucalypt. Types are never required; all existing code continues to work unchanged.
  - `eu check file.eu` — type-check a file, report warnings
  - `eu check --strict file.eu` — treat warnings as errors
  - `eu --type-check file.eu` — type-check then evaluate (warnings to stderr)
  - `eu --type-check --strict file.eu` — type-check (abort on warnings) then evaluate
- **Type annotation syntax** — `type:` metadata on declarations: `` ` { type: "number → number" } ``
  - Primitives: `number`, `string`, `symbol`, `bool`, `null`, `datetime`
  - Composites: `[T]` lists, `(A, B)` tuples, `{k: T, ..}` open records, `A → B` functions, `A | B` unions
  - Special types: `any` (gradual), `top`, `never`, `set`, `vec`, `array`
  - Type constructors: `IO(T)`, `Lens(A, B)`, `Traversal(A, B)`
  - Type variables: `a`, `b`, etc. for polymorphic functions
  - `→` (U+2192) as alternative to `->` in type strings
  - `block` keyword as shorthand for `{..}` (open empty record)
  - Asserted annotations: `type: "!T"` prefix trusts the type without verifying the body
- **Typed monad metadata** — `monad:` metadata field on namespace declarations optionally declares the monadic wrapper type (e.g. `monad: "[a]"` instead of `monad: true`). The desugarer injects type hints on binding values, enabling the type checker to catch wrong-type bindings in monadic blocks (e.g. `{ :for x: 42 }` warns "expected [a], found number")
  - `for` typed as `[a]`, `io` as `IO(a)`, `random` and `state` also typed
  - `let` remains `monad: true` (untyped) for backward compatibility
- **LSP monad support** — completion for monad tags after `{ :`, inlay hints showing expected binding types, hover on monad tags showing description and binding type
- **Literal symbol types** — `:name` in type annotations as singleton types matching specific symbols. `:active | :inactive` for discriminated unions
- **Type aliases** — `type-def:` metadata derives an alias from a declaration's value; `types:` in unit metadata for standalone aliases
- **Prelude type annotations** — ~200 functions annotated with `type:` metadata covering arithmetic, comparison, string, list, block, IO, lens, set, vec, arr, random, and monad operations
- **Bidirectional type checker** — synthesis for literals, lists, blocks, variables, applications; checking mode for annotated functions; catenation/pipeline type flow; polymorphic instantiation via unification; union overload resolution with subtype fallback
- **LSP type integration** — hover shows type annotations and inferred types; completion from record field types; inlay hints for inferred types; type warnings as `DiagnosticSeverity::WARNING`
- **Warning diagnostic infrastructure** — `Diagnostic::warning()` in CLI (codespan-reporting) and `DiagnosticSeverity::WARNING` in LSP; warnings don't cause non-zero exit; `--strict` promotes to errors
- **Desugarer type hints** — lens bracket expressions `‹ :items 0 :meta ›` carry `Lens(any, any)` type hints via `Expr::Meta`
- **`arr.unit-arrays(shape)`** — list of arrays with a single 1 at each coordinate position (standard basis vectors for 1D, elementary matrices for 2D)
- **Type check message tests** — integration tests for `eu check --strict` validating exit codes and stderr message content

### Changed

- **`arr.slice` and `arr.neighbours` parameter order** — reordered via prelude wrappers for pipeline style: `a arr.slice(axis, idx)`, `a arr.neighbours(coords, offsets)`

### Fixed

- **Dedicated `head`/`tail` of empty list errors** — `head` and `tail` on empty lists now produce clear "empty list" errors instead of generic panics
- **Dedicated `head`/`tail` of non-list errors** — `head` and `tail` on non-list values produce specific "expected a list" errors with suggestions
- **User `panic()` vs internal errors** — `panic("message")` in user code now shows "panic: message" without the "internal error" framing used for VM-level failures
- **`NotValue` diagnostic improvements** — type-specific messages for bool, list, and block contexts; helpful notes for null values
- **List index out of bounds** — `nth` raises `ListIndexOutOfBounds` with the index and list length instead of a generic panic
- **User-code source location as primary error site** — runtime errors now show the user's source location as the primary site, not the prelude internals

## [0.5.4] - 2026-04-19

### Added

- **`EU_GC_VERIFY=2`** — full multi-checkpoint structural verification of the GC heap: header validity, pointer validity, line consistency, forwarding pointer lifecycle, and block list integrity across four checkpoints during collection. Level 1 unchanged
- **Ceiling/floor bracket notation** — `⌈n⌉` for ceiling and `⌊n⌋` for floor, using idiot bracket pairs. `⌈3.2⌉` → `4`, `⌊3.8⌋` → `3`
- **Debugging tools guide** — `docs/development/debugging.md` documenting all debug environment variables, dump commands, and recommended debugging workflows
- **Editor Unicode coverage** — `⌈⌉⌊⌋` (ceiling/floor) and `‹›` (lens brackets) added to Emacs quail/transient and VS Code quick-pick. Bracket pairs group in Emacs transient menu with auto-pairing

### Changed

- **Unified stream producers** — `StreamProducer`/`StreamTable`/`STREAM_NEXT` replaced by `LazyProducer` trait, `ProducerTable`, and `PRODUCER_NEXT` BIF. All handle-based lazy producers (file imports, future IO streams) use one registration mechanism. `Native::Stream(u64)` renamed to `Native::Prng(u64)`, `Native::Producer(u32)` added for all producer handles

### Fixed

- **Lens brackets `‹›` in tree-sitter grammar** — added to `BRACKET_OPEN_RE`/`BRACKET_CLOSE_RE` so lens path expressions parse and highlight correctly
- **`▶` and `⊝` in tree-sitter** — added to `OPER_CHARS` for correct operator highlighting
- **`•` in VS Code TextMate grammar** — added to unicode-operator character class

## [0.5.3] - 2026-04-17

### Added

- **State monad library** — optional import (`{ import: "state.eu" }`) providing `{ :state ... }` monadic blocks for threading block-valued state. Includes `state.get`, `state.put`, `state.lift`, `state.query`, `state.modify`, `state.run`/`eval`/`exec`, and lens-based operators `=!` and `%!`
- **Eucalypt export format** — `eu -x eu` and `render-as(:eu)` preserving type distinctions: symbols as `:name`, strings quoted, numbers bare. Uses `pretty` crate for layout
- **Import guards** — include-once deduplication for diamond imports. Same file imported multiple times is desugared once; different-name imports of the same file emit alias bindings
- **Deep nested destructuring** — list and block destructuring now supports arbitrary depth: `f([[a, b], [c, d]])`, `g({x: {y: inner}})`
- **IntrinsicMachine thunk-forcing** — `Machine::evaluate_to_whnf` enables Rust intrinsics to force lazy thunks. `MachineCore`/`MachineBifContext` split eliminates aliased `&mut` UB. `evaluate_to_whnf_for_io` refactored to delegate
- **`deep-transform(rule, data)`** — recursive structural rewrite: apply `rule` at each node, return non-null to replace or null to recurse into children
- **`map-elements(f, block)`** — apply `f([key, value])` to each element of a block, returning a new block
- **`unzip(pairs)`** — list of pairs to pair of lists, inverse of `zip`
- **`interleave(a, b)`** — alternate elements from two lists, appending remainder when one is exhausted
- **`window-all` / `partition-all`** — like `window`/`partition` but include trailing incomplete chunks
- **`random.run` / `random.eval` / `random.exec`** — convenience methods matching the state monad API
- **Set membership operators** — `v ∈ s` and `v ∉ s` at precedence 40; sections work as predicates: `when(∈ ks, f)`
- **Editor Unicode support** — `∈`, `∉`, `⊝`, `▶` added to Emacs quail/transient and VS Code quick-pick
- **List monad (`:for` blocks)** — `{ :for x: xs y: ys [x, y] }` for list comprehensions with monadic bind over lists
- **`parts-of(traversal)`** — lens combinator turning a traversal into a lens on the list of all foci. `view` collects; `over` applies list transforms and distributes back
- **`each-element` / `filtered-elements(p?)`** — block traversals for the lens library, analogous to `each`/`filtered` for lists. Traverse all or matching kv pairs with `to-list-of`, `over`, and composition with `_key`/`_value`
- **`EU_IO_TRACE=1`** — debug environment variable tracing all `io.shell` and `io.exec` commands to stderr, showing command strings, stdin piping, and exit codes

### Changed

- **Unified assertion/expectation operators** — `//=` and `//!` now use a single `__EXPECT` BIF with stderr diagnostics on failure, replacing separate `__ASSERT` and `__CHECK` implementations
- **`dbg` and `▶` rendering** — switched from `render-as(:json)` to `render-as(:eu)`, preserving symbol syntax in debug output
- **Simple lookup semantics** — `.name` is now consistently key lookup restricted to block bindings, never extending to outer scope. Previously, `.name` on a static block literal resolved through the block scope and fell through to outer scope
- **Monadic blocks in generalised lookup** — `A.{ :monad ... }` now evaluates the monadic block in `A`'s scope with implicit return. Previously the monadic block was desugared independently, losing the lookup scope
- **Monadic return left-associativity** — `{ :monad ... }.{ block }.(expr)` now parses as `(({ :monad }).{ block }).(expr)`. The return expression is a single element; subsequent `.` operations are separate lookups on the result
- **State monad representation** — state actions return `{ value: v, state: s }` blocks (matching random monad's `{ value: v, rest: stream }` pattern) instead of `[v, s]` lists
- **Quoted identifier handling** — `NormalIdentifier::value()` strips single quotes from quoted names like `'x/z'` in all desugarer paths

### Fixed

- **Compiler panic on consecutive metadata blocks** — `{ a: 1 } { b: 2 } c: 3` no longer panics. Root cause: `mark_body` in prune pass now marks metadata as reachable. Belt-and-braces `InternalCompilerError` diagnostic replaces the `expect()` panic
- **Deep merge panic with list-valued keys** — `{a: 1, b: []} << {a: 2}` no longer panics. `deconstruct` uses `HeapNavigator::resolve_in_closure` which handles `Ref::G` (global constants like `[]`) and `Ref::V` (inline natives)
- **Merge with dynamically-created symbol keys** — `[[:b str.of sym, 0]] block` now merges correctly. `ExtractKey` handles raw symbol atoms; `BlockPair` normalises keys via `ExtractKey`
- **`set.contains?` with non-primitives** — returns `false` instead of panicking when given lists, blocks, or other non-primitive arguments
- **`head`/`tail` error messages** — now show the actual argument: `"head requires a list argument, got 42"` instead of a generic message
- **TypeMismatch error values** — error messages include the actual runtime value with char-safe truncation
- **Parser stray colon recovery** — `f(2, 2:)` produces a parse error instead of an assertion panic
- **`str.of` on quoted symbols** — symbols with special characters (e.g. `'x/z'`) no longer include quotes in string conversion

### Performance

- **Env-var lookup caching** — `EU_STACK_DIAG` and `EU_GC_STRESS` checked once at startup instead of every VM step / GC cycle
- **Exact-arity fast path** — `saturate_with_array` avoids an `Array` copy for exact-arity function application
- **Native arithmetic fast path** — binary operators skip unbox/force chains when both arguments are already unboxed natives

## [0.5.1] - 2026-03-24

### Added

- **Vec type** — `vec.of`, `vec.len`, `vec.nth`, `vec.slice`, `vec.sample`, `vec.shuffle`, `vec.to-list` for O(1) indexed access on large primitive collections
- **Set sampling** — `set.sample(k, s, stream)` for random monad sampling from sets
- **Unified test expectations** — `//=` and `//!` now emit stderr diagnostics on failure via `__EXPECT` BIF; `__DBG_REPR` renders values for diagnostic output
- **Debug tracing** — `dbg(opts, v)` function and `▶` prefix operator for stderr debug output
- **Structured argument parsing** — `parse-args(defaults, args)` with short flags, type coercion, combined options, and `--help` generation
- **Monadic blocks** — `monad: true` metadata registers namespaces; implicit return from non-underscore bindings; identity monad (`:let` blocks)
- **Bracket registry** — proper content-type registry replaces parser heuristic for bracket pairs
- **Idiot brackets collect as list** — `⟦ a b c ⟧` now collects items as `[a, b, c]`; bracket parameter definitions support destructuring patterns
- **Multi-label diagnostics** — secondary source labels from env trace; stack trace reversed to read top-down with name-first formatting
- **Error source locations** — all `ExecutionError` variants now carry `Smid` for source location
- **WASM API** — `evaluate(source, format)` via wasm-bindgen for browser/Node.js use
- **Browser playground** — CodeMirror-based eucalypt playground (separate repo)
- **Markdown docstrings** — tree-sitter grammar identifies docstrings; Emacs mode highlights inline markdown
- **VS Code extension** — feature parity with Emacs mode: improved highlighting, Unicode input, snippets, render command
- **Windows support** — crash handler gated with `cfg(unix)`, PowerShell shell dispatch, Windows CI and release binary
- **Deep merge metadata** — merge and deep merge now preserve block metadata (RHS wins)
- **`coalesce(xs)`** — return first non-null element from a list
- **`update-nth(n, f, l)`** — apply function to element at index n in a list
- **`update-first(p?, f, l)`** — apply function to first matching element in a list
- **`eu.os` / `eu.arch`** — platform constants for cross-platform test portability
- **Nested list destructuring** — `f([a, [b: c]])` now works in function parameters
- **AddressSanitizer CI** — ASAN job (continue-on-error) for catching memory safety issues

### Changed

- **Moniker dependency removed** — replaced with custom binding module; simplified type signatures throughout core pipeline (22 files)
- **`:suppress` documentation** — clarified as data-only; not needed on functions
- **Type predicates** — `number?`, `string?`, `symbol?`, `bool?` intrinsics added
- **`list-update` removed** — replaced by `update-nth` with pipeline-friendly arg order

### Fixed

- **Array growth bug** — `default_array_growth(1)` returned 1 (no growth) due to integer division truncation; caused heap-buffer-overflow on Linux (detected by ASAN)
- **Monadic implicit return self-reference** — synthesised `{a: a}` used `Expr::Block` (letrec) causing self-reference; fixed with manual `Let + Block` scope
- **`-e` monad registry** — `monad: true` registrations from prelude now persist across translation units so `-e` expressions see them
- **`split-after` no-match crash** — crashed when predicate never matched; separated nil check from predicate check
- **`head`/`tail` on empty list** — now panics with "head of empty list" / "tail of empty list" instead of cryptic type mismatch (fixed `Panic.global` to use `BoxedString`)
- **Stack traces show function names** — `new_smid` inherits declaration name from desugarer stack; `intrinsic_display_name` catch-all no longer masks user function names
- **Graceful cwd error** — warns instead of panicking when current directory is inaccessible
- **String interpolation** — pipelines inside interpolation braces were silently producing wrong results; fixed in prelude
- **Windows c-string test** — newline comparison uses c-string instead of literal to avoid `\r\n` conversion

## [0.5.0] - 2026-03-13

### Added

- **IO Monad** — Execute shell commands and system processes from eucalypt
  - `{ :io r: io.shell("cmd") }.(r.stdout)` — monadic block syntax with bind/return desugaring
  - `io.shell(c)`, `io.shell-with(opts, c)` — run shell commands via `sh -c`
  - `io.exec([cmd : args])`, `io.exec-with(opts, [cmd : args])` — exec processes directly
  - `io.check(r)`, `io.checked`, `io.bind`, `io.return`, `io.map`, `io.and-then`, `io.fail`
  - `--allow-io` / `-I` CLI flag required to enable IO operations
  - Results as `{stdout, stderr, exit-code}` blocks
  - Spawn failures return result blocks (exit-code 127) rather than hard errors

- **`render-as(fmt, value)`** — Serialise any eucalypt value to a string at runtime
  - Supports `:json`, `:yaml`, `:toml`, `:text`, `:edn`, `:html`

- **`parse-as(fmt, str)`** — Pure inverse of `render-as`; converts strings to eucalypt data structures
  - Supports `:json`, `:yaml`, `:toml`, `:csv`, `:xml`, `:edn`, `:jsonl`
  - Data-only mode: untrusted input (e.g. shell output) never executes embedded code

- **`monad(m)`** — Derive standard monad combinators from a block with `bind` and `return` fields
  - Returns a block with `map`, `and-then`, `then`, `join`, `sequence`, `map-m`, `filter-m`
  - `and-then(f, action)` — bind with flipped args for pipeline use
  - Compose with `{ ... }` to build monadic namespaces: `monad(m) { extra-field: ... }`

- **Monadic `random:` namespace** — State-monad interface to the PRNG
  - `random.stream(seed)` — create initial stream; each action is a function `stream → {value, rest}`
  - `random.float`, `random.int(n)`, `random.choice(xs)`, `random.shuffle(xs)`, `random.sample(n, xs)`
  - `random.map`, `random.sequence`, `random.map-m`, `random.bind`, `random.return`
  - Legacy `random-stream`, `random-int`, `random-choice`, `random-shuffle`, `random-sample` retained

### Fixed

- **`render-as` argument order** — Changed from `render-as(value, fmt)` to
  `render-as(fmt, value)` for pipeline-friendly partial application
  (e.g. `data render-as(:json)`)
- **`render` / `render-as` nested block null** — Nested block values inside
  `render` or `render-as` were serialised as null; now correctly traverses
  unevaluated Let/LetRec thunks in the heap walk
- **GC correctness** — Multiple garbage collector fixes addressing crashes on aarch64-linux and macOS ARM:
  - 16-byte alignment for evacuation allocations
  - Per-heap mark state (global `MARK_STATE` moved into `Heap` struct), fixing parallel test crashes
  - Backing arrays of `Cons`/`App`/`Bif`/`Case` nodes now evacuated correctly
  - Line marking extended to evacuation target blocks, preventing lazy sweep from recycling live data
  - Full allocation span (header + object) now marked when straddling Immix line boundaries
  - Heap string corruption in release builds prevented
  - Cross-line array backing mark coverage corrected

- **Error diagnostics** — Comprehensive error message improvements:
  - Source locations for operator errors, function calls, lookup failures, dot-on-non-block, intrinsic type mismatches, comparison errors, datetime/timezone errors, regex errors, and base64 decode errors
  - Available keys shown on lookup failure when no close match exists
  - Contextual hints: `map(.field)` for dot on list of blocks, `count` for list-used-as-block, block/list in string interpolation
  - Panics replaced with structured errors: `ComparisonTypeMismatch`, `BitshiftRangeError`, `BitwiseIntegerRequired`, `AssertionFailed`, `VersionRequirementFailed`, dotted metadata keys
  - Improved messages for partial application, datetime components, numeric format types, symbol-vs-string mismatches

- **Block-dot metadata** — `AllocationPruner` letrec strip index adjustment corrected, fixing incorrect evaluation when accessing fields of metadata-tagged blocks

- **Emacs mode** — Backtick auto-pairing, closing brace indentation, docstring indentation, smartparens compatibility, new Unicode chars in Quail input method and transient menu

## [0.4.0] - Destructuring, Monadic Blocks, Arrays, Error Messages

### Added

- **`deep-merge-at`** — New prelude function to merge a value into a nested path

- **`✓` Postfix Non-nil Predicate Operator**
  - Also useful for expanding scope of expression anaphora eg. filter(\_0✓ && fn(\_0))

- **Destructuring Parameters** - Pattern matching in function parameters
  - Block destructuring: `f({x y}): x + y`
  - Fixed-length list destructuring: `f([a, b]): a + b`
  - Head/tail cons destructuring: `f([h : t]): h` with `‖` operator
  - Juxtaposed call syntax: `f{x: 1 y: 2}` and `f[1, 2]`
  - Juxtaposed definition syntax: `f{x y}: x + y` and `f[a, b]: a + b`
  - Destructure fusion pass to elide intermediate allocations

- **N-dimensional Arrays** - `arr.*` namespace for tensor operations
  - `arr.from-flat`, `arr.reshape`, `arr.map`, `arr.fold`, `arr.neighbours`
  - Polymorphic arithmetic: `+`, `-`, `*`, `/` work element-wise on arrays
  - Heap-backed `HeapNdArray` with GC integration

- **Relative Imports** - Resolve imports relative to source file directory

- **Power Operator** - `^` for exponentiation with `pow` intrinsic

- **Division Semantics** - `/` is floor division, `÷` is exact division

- **Unicode Operator Aliases** - `≤`, `≥`, `≠` for comparison operators

- **`product`** - Multiply elements in a list (complement to `sum`)

- **Contextual Error Messages** - Comprehensive error improvement programme
  - Did-you-mean suggestions for key lookup failures
  - Expected vs actual type in type mismatch errors
  - Structured error output with `--error-format=json`
  - Multiple parse/translate errors reported in one pass
  - Hints for common mistakes: `==` → `=`, `->` is const not lambda,
    `import`/`use`/`require` as unresolved variables, camelCase function
    names, missing parity functions, string method patterns
  - Source locations in parse errors and BlackHole (infinite loop) errors
  - Contextual help for operator precedence issues

- **(Experimental) Idiot Brackets** - User-defined handling for Unicode bracket pairs
  - Unicode Ps/Pe category detection for bracket support
  - Custom evaluation semantics via bracket metadata
  - Allows "customising catenation"

- **(Experimental) Monadic Blocks** - Blocks as a "do notation" for monads
  - Several syntactic forms all desugar to `bind` / `return` expressions
  - Inline monad definition `{ { :monad bind: ... return: ...} declarations... }.expr`
  - Referencing a namespace with `bind` and `return`: `{ { :monad namespace: ns } declarations... }.expr`
  - Via monad key in block metadata: `{ { monad: ns } ... }.expr`
  - Short form using only symbol metadata for namespace reference: `{ :ns ... }.expr`
  - User-defined bracket pairs with allowing `⟦ a: ma  b: mb ⟧.expr` 

- **Documentation**
  - Eucalypt Guide (15 tutorial chapters)
  - Eucalypt by Example (15 worked examples)
  - FAQ (20 common questions)
  - Agent reference page for AI coding agents
  - Syntax cheat sheet
  - `llms.txt` and `llms-full.txt` for LLM context
  - mdBook migration with syntax highlighting
  - Documentation example testing (`scripts/test-doc-examples.py`)

- **Editor Support**
  - Unified Emacs mode with tree-sitter and traditional variants
  - Tree-sitter grammar updates for 0.4.0 syntax
  - VS Code extension improvements

- **Platform**
  - aarch64-linux release binary
  - Curl-installable install script
  - WASM compilation gate in CI

### Changed

- **`assert` Refactoring** — `assert` now accepts a predicate; moved adjacent to
  assertion operators in the prelude
- **Default heap limit** reduced from 64 GiB to 32 GiB managed heap
- **Assertion operators** reorganised; falsy variants deprecated
- **Stack traces** - Lazy iterators, pre-allocated buffers, auto-filtered
  intrinsic frames, suppressed empty traces

### Performance

- **NdArray arithmetic dispatch** (eu-76sv) — Array type dispatch moved from
  interpreted prelude `is-array?` checks to Rust intrinsics, giving approximately
  10× performance improvement for array operations
- **VM execution hot loop** optimisation
- **Continuations** stored inline in Vec, off the eucalypt heap
- **Boolean returns** use pre-allocated global closures
- **`return_native`** reuses existing Atom closures
- **Thunk memoisation** preserved through shared constructor env backing
- **Update accumulation** prevented in IF branches via `suppress_update`
- **`str_arg_ref()`** zero-copy string borrowing for intrinsics

### Fixed

- **Depth-aware `beta_reduce`** (eu-5pe9) — Substitution in `beta_reduce` now
  tracks binder depth correctly for destructuring lambdas, fixing incorrect
  variable capture in certain patterns
- **`deep-find` symbol keys** (eu-9vzc) — `deep-find`, `deep-find-first`, and
  `deep-find-paths` now accept symbol keys only (previously accepted strings,
  which was inconsistent with the block key model)
- **Deep-query prelude** — Fixed nested conditional in `match-sym`; refactored
  deep-fold abstraction to unify deep-query functions
- **Emacs mode** — Corrected indentation of backtick metadata at top-level
  declarations
- Expression anaphor scoping for `_`, `_0`, `_1` in arg positions
- Cons pattern mangling in EU formatter
- `HeapNdArray` GC evacuation correctness
- `u8` overflow for large frames in constructor env
- `set.add` for computed values
- Deep merge handling of boxed symbols in dynamic blocks
- Unclosed string interpolation converted from panic to proper error
- `::` converted from panic to proper error
- `#` inside string literals no longer parsed as comment (tree-sitter, Emacs)
- Rainbow-delimiters restored with string-aware syntax-propertize
- Stray debug `println` corrupting CI version output
- Block-level DCE handling of dynamise fallbacks

## [0.3.0] - Runtime v2, GC, Random Numbers, Streaming Imports

### Added

- **Random Number Facilities** - Pure functional SplitMix64 PRNG
  - `__PRNG_NEXT` / `__PRNG_FLOAT` built-in functions
  - Seed from environment (`RANDOM_SEED`) or default
  - No mutable state - deterministic and reproducible

- **Streaming File Imports** - Eager drain via `__STREAM_NEXT` intrinsic
  - Thread-local `StreamTable` for import stream management
  - Process large files without loading entirely into memory

- **Sets Data Type** - New `Native::Set` with heap-backed storage
  - Set literal syntax and set operations in prelude
  - Deterministic output via sorted rendering

- **Symbol Interning** - Runtime `SymbolPool` for efficient symbol handling
  - `Native::Sym` changed from `RefPtr<HeapString>` to `SymbolId`

- **Deep-Query DSL** - Pattern-based data querying
  - `deep-find` functions and type predicates (`ISBLOCK`, `ISLIST`)
  - Path wildcard support for nested data access

- **Block Indexing** - O(1) key lookup for blocks
  - `Native::Index` variant for block index storage
  - `LOOKUPOR` modified for lazy block indexing
  - Block-level dead code elimination

- **RAWMETA Intrinsic** - Non-recursive metadata access
  - Doc metadata preserved at runtime for doc generation library

- **GC Evacuation** - Full Immix evacuating collector
  - `scan_and_update` for pointer fixup during evacuation
  - Lazy sweeping optimisation
  - O(1) block lookup for line marking
  - GC stress benchmarks and Criterion benchmarks
  - GC benchmarking script (`scripts/gc-bench.sh`)

- **Command-Line Args** - `__args` pseudo-block and `io.args` in prelude

- **C-String Literals** - `c"..."` syntax with escape sequences

- **ZDT Literal Syntax** - `t"..."` for zoned datetime values

- **Polymorphic Comparison** - Equality and ordering for strings, symbols, datetimes

- **Cartesian Product** - `cross` function in prelude

- **SHA-256 Hash Intrinsic** and **Base64 Encode/Decode**

- **Version Assertion Intrinsic**

- **Self-Referential Thunk Detection**

- **Tight-Binding Head Operator** - `↑` for list head access

- **JSONL Import** - JSON Lines format support

- **Nullary Operators**

- **YAML Import Enhancements** - Anchor/alias resolution, merge keys, timestamp-to-ZDT

- **Source Code Formatter** (`eu fmt`)
  - Conservative and full reformat modes
  - Configurable line width and indent size
  - Check mode (`--check`) for CI, in-place modification (`--write`)

- **LSP Server** - Language Server Protocol implementation
  - Diagnostics, completion, hover, go-to-definition
  - Find references, semantic tokens, formatting
  - Code actions, inlay hints, rename, document symbols

- **Editor Support**
  - Emacs major mode (`eucalypt-mode.el`) with tree-sitter variant
  - VS Code extension with TextMate grammar
  - Tree-sitter grammar for Eucalypt

- **CLI Rationalisation** - clap v4 subcommand architecture
  - `eu run`, `eu test`, `eu dump`, `eu fmt`, `eu version`, `eu explain`, `eu list-targets`

- **CI/CD Improvements**
  - Consolidated lint job, `cargo audit` security audit
  - SHA-based concurrency groups for builds
  - Error test `.expect` sidecar validation

- **Testing Expansion** - 106 harness tests (up from ~50)
  - GC stress tests, error tests, benchmark validation

- **Prelude Extensions**
  - List sorting (`sort`, `sort-by`), string comparison
  - Streaming string intrinsic returns
  - Extended block and list functions

### Changed

- **Parser Rewrite** - Complete replacement of LALRPOP parser with Rowan-based implementation
  - Better error recovery and diagnostics
  - Foundation for IDE tooling support

- **Runtime v2** - STG machine rewrite with new BIF system
  - Case-of-known-constructor folding
  - O(1) tag dispatch for case branches
  - Redundant branch elimination
  - Performance: lazy stack trace iterators, pre-allocated trace buffers, reduced Vec cloning

- **Dependencies** - Security updates, replaced unmaintained crates, clap v4 migration

### Fixed

- Large object allocation sizing for improved memory efficiency
- Object alignment for double-word boundaries
- Array bounds checking guards
- Deprecated chrono API usage
- YAML tag output, merge key spans, scalar conversion error handling
- Unicode support in TextMate grammar and VS Code
- UTF-16 offset handling in LSP
- Formatter unary operator spacing and tab-to-space alignment
- `-e` evaluand blocking on stdin in non-TTY contexts

## [0.2.0] - Rust Implementation

### Added

- Complete rewrite in Rust (from Haskell)
- Immix-style garbage collector with:
  - Block hierarchy (32KB blocks, 128B lines)
  - Size class allocation
  - Fragmentation detection and collection strategy selection
  - Comprehensive performance metrics and telemetry
  - Graceful OOM handling
  - Per-heap mark state architecture

- New prelude functions:
  - `group-by(k, xs)` - Group elements by key function
  - `qsort(lt, xs)` - Quicksort with custom comparison
  - `discriminate(pred, xs)` - Split list by predicate
  - Modulus operator `%`

- EDN (Extensible Data Notation) import and export support
- Heap statistics output with `-S` flag

### Fixed

- `head-or` function behavior
- Missing `__UPPER` and `__LOWER` string intrinsics
- YAML tag output formatting
- Large object allocation

## [0.1.x] - Haskell Implementation

Initial implementation in Haskell. See git history for detailed changes.
