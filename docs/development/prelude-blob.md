# The prelude blob and the source-prelude fallback

Every `eu` invocation loads the standard library (`lib/prelude.eu`) before
running user code. There are two ways this happens:

1. **Blob path (default, and what release binaries embed).** A
   pre-compiled `lib/prelude.blob` — built by `cargo xtask
   prelude-compile` — is embedded into the binary at compile time
   (`src/driver/resources.rs`, gated on `cfg(prelude_blob_ok)`, set by
   `build.rs` when the blob is present and its embedded source hash
   matches `lib/prelude.eu`). At runtime the prelude's operators, type
   summary, and compiled STG globals are loaded straight from the blob;
   the prelude source is never parsed, cooked, or merged.
2. **Source-prelude fallback.** Used when no blob is embedded (a plain
   checkout with `lib/prelude.blob` absent — it is git-ignored — or a
   stale blob whose hash doesn't match), or when explicitly requested via
   `--source-prelude` / `EU_SOURCE_PRELUDE=1`. `lib/prelude.eu` is loaded,
   translated, and merged into the user's core expression like any other
   input, then compiled from scratch on every invocation.

## The two paths are correctness-equivalent but not performance-equivalent

Both paths produce identical results for identical eucalypt programs — the
fallback exists precisely so a plain checkout without a generated blob
still works correctly. But they are **not tick-equivalent** on
arithmetic-dense, strictly-recursive code. On the naive-fibonacci benchmark
(`tests/harness/bench/001_naive_fib.eu`, `fib(30)`):

| Configuration | VM ticks |
|---|---|
| Blob embedded | 88,853,885 |
| `--source-prelude` fallback | 98,277,770 (+10.6%) |

### Why

Root-caused in bead `eu-2sa6.5` (2026-07-13). It is **not** a gap in
fused-primop codegen — `emit_fixtures_and_globals`
(`src/eval/bytecode/encode.rs`) emits byte-identical fused `Op::FusedPrimop`
encodings for the intrinsic global forms on both paths. The actual cause is
that demand analysis infers different strictness for user-defined
recursive functions on the two paths:

- **Blob path**: the prelude is never merged into user code, so
  references like `<=`, `+`, `-` remain `Var::Free` in the user's core
  expression. `Loader::inject_prelude_inlinable_bindings`
  (`src/driver/source.rs`) injects the blob's pre-computed
  `inlinable_bindings` set — a small, already-resolved subset of prelude
  combinators and intrinsic aliases — as a flat `Let` immediately before
  the general inline pass, matched by free-variable name. This exposes the
  strict, `Case`-on-first-argument shape of the arithmetic/comparison
  intrinsics to demand analysis, which can then infer that (e.g.) a naive
  `fib`'s recursive argument is used strictly — no thunk is allocated for
  `n - 1` before the recursive call.
- **Source-prelude path**: the prelude *is* merged into user code
  (`merge_units`), so by the time `cook()` runs every prelude reference
  has already been resolved to `Var::Bound` inside one large merged
  `letrec` — confirmed directly via `eu dump cooked --debug-format`, which
  shows zero `Var::Free` nodes and thousands of `Var::Bound` ones on this
  path. There is no free-variable name left for a
  `inject_prelude_inlinable_bindings`-style injection to intercept, and the
  general inline pass (`tag_combinators` + `reduce::inline_pass`) does not
  perform the same fold — a sweep of 2 through 12 inline iterations left
  ticks unchanged at 98,277,770, ruling out "just iterate further" as a
  fix. `fib`'s recursive argument is inferred `Lazy`, so each call
  allocates and later forces a thunk.

Unifying the two paths — making the source-prelude fallback build the same
in-memory global-slot / `Ref::G` structure the blob embeds, rather than
merging prelude source directly — is a real fix, but a substantial one. It
is tracked as a follow-on bead and deliberately deferred so it doesn't run
ahead of the 0.13 lever-(a) predecoded-IR work.

## What this means in practice

- **Released binaries are unaffected.** `cargo xtask prelude-compile` runs
  before every release build (`.github/workflows/build-rust.yaml`,
  `test-bytecode-blob` / release jobs), so shipped binaries always embed
  the blob and never take the source-prelude tick handicap.
- **A plain `cargo build`/`cargo test` on a fresh checkout** — where
  `lib/prelude.blob` has not been generated — silently uses the
  source-prelude fallback. This is correct but ~10% slower on
  arithmetic-heavy workloads. Run `cargo xtask prelude-compile` locally if
  you are taking performance measurements.
- **`tests/tick_parity_test.rs`** is a tripwire, not a fix: it asserts the
  source-prelude tick handicap on a small fused/strict-recursion fixture
  stays within its documented bound (capped at 12%, with headroom over the
  measured ~10.6%). It only has teeth when the `eu` binary it spawns
  embeds a blob — see the `tick-parity` CI job, which generates one first.
  If you are benchmarking anything performance-sensitive, always confirm
  which path is active (`cargo xtask prelude-compile` beforehand, or check
  for the `precompiled prelude not found` build warning).
