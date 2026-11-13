# Derived prelude signatures: follow-ups (eu-oaz72.92)

**Bead:** eu-oaz72.92 (AoC 2026 preparation epic eu-oaz72), follow-ups from
PR #1173 (eu-oaz72.77, `2026-11-13-derived-prelude-signatures.md`).
**Branch:** `eu-oaz72.92/sig-follow-ups` from master `47e4709`.

## Items

| # | Item | Outcome |
|---|---|---|
| 1 | Signatures through aliases and partial applications | Fixed. A non-lambda binding takes the signature of the function it denotes: an intrinsic's, a bound binding's, or `f`'s less the `k` arguments a partial application `f(a1…ak)` supplies. Resolved each fixpoint round from that round's lambda signatures, so a same-group alias never reads a stale one. Blob slots with a signature: 239/373 → 344/373. |
| 2 | Pipeline steps | Fixed. `analyse_app` reads an application spine `App(App(g, a), b)` as one call of `g` with `a ++ b`; the expression keeps its shape. `head-or` and `tail-or` are now strict in their list. |
| 3 | `errors/110` extra boundary frame in blob mode | Won't fix. `lookup-or`'s default is lazy (`__LOOKUPOR`'s fallback), so the `panic` default runs inside `lookup-or`, and the curated trace keeps that `:boundary` combinator as context. Source mode shows no frame only because inlining dissolves the call into `__LOOKUPOR`. Removing it would mean inlining intrinsic wrappers into intra-prelude bodies at blob build, which changes many traces for no correctness gain. The snapshot is unchanged by this branch. |
| 4 | Signature table cloned per run | Fixed. `Executor::set_prelude_blob` moves the blob's signatures into an `Arc` once; each run clones the `Arc`. |
| 5 | Bench baseline | Re-measured below against the commit before #1173 (`fce2781`, post-#1169). |
| 6 | Branch join through an alias | Fixed, generalised: each `Let` frame records, per binding, the condition and branch positions of a conditional — `__IF`, an alias of one, or a lambda whose body is a conditional over its own parameters (`then(t, f, c): if(c, t, f)` is `[2, 0, 1]`). A saturated call through any of them joins its branches. |
| 7 | Compare every blob signature with the source analysis | `every_blob_signature_matches_the_source_analysis` (blob mode) re-derives the signatures from the prelude source and compares all 373 slots. |
| 8 | Alias test without a fresh blob | Unit tests on hand-built core: `signatures_follow_aliases_partial_applications_and_pipelines`, `free_callees_take_prelude_signatures`. |
| 9 | Blob-mode user analysis lacked prelude signatures | Fixed. The analysis takes `PreludeSignatures` (the blob's `name_to_slot` and signatures, by reference) and gives a saturated call of a free variable naming a prelude global that global's signature. CHANGELOG entry added. |

`eu dump demands` still analyses without prelude signatures; it is a debug
dump of the source pipeline.

## Verification

- Harness: target `test-signature-propagation` in
  `tests/harness/165_demand_annotation.eu` binds, for each mechanism, a value
  the call does not need (`panic`), passing in both prelude modes.
- Fault injection (each failed, then passed once restored): partial
  application offset taken from the wrong end (harness target and unit
  test); branch join taking either branch (harness target, unit test);
  every signature strict at a spine (harness target); spine flattening off,
  `__IF` shape off, lambda shapes off, partial applications off, prelude
  lookup off (unit tests); xtask dropping `zip`'s signature from the blob
  (both blob tests, naming `zip`).
- `cargo test` with and without a blob; fmt; clippy.

## Performance (measured-single)

Ticks and max stack are deterministic. One run each, release builds, each
with its own freshly generated blob: `fce2781` (before #1173), `c6e2036`
(#1173 merged) and this branch. Current master `47e4709` gives the same
figures as `c6e2036` on every row.

| Bench | fce2781 ticks / stack | c6e2036 ticks / stack | Δ ticks | Branch ticks / stack |
|---|---|---|---|---|
| 015 block_merge | 2,011,911 / 3,034 | 1,945,868 / 6,017 | −3.3% | 1,945,868 / 6,017 |
| 016 yaml | 5,563,734 / 25 | 5,520,820 / 2,175 | −0.8% | 5,520,820 / 2,175 |
| 017 toml | 5,517,485 / 25 | 5,474,571 / 2,175 | −0.8% | 5,474,571 / 2,175 |
| 018 string_scale | 2,408,803 / 14,017 | 2,345,806 / 14,017 | −2.6% | 2,345,806 / 14,017 |
| 019 list_scale | 1,061,231 / 19 | 989,233 / 12,009 | −6.8% | 989,233 / 12,009 |
| 020 lookup_curve | 7,894,167 / 47 | 7,887,949 / 304 | −0.1% | 7,887,949 / 304 |
| 021 io_loop | 1,213 / 27 | 1,213 / 27 | 0 | 1,213 / 27 |
| 022 hof_fold | 50,500,708 / 800,006 | 46,900,711 / 800,006 | −7.1% | 46,900,711 / 800,006 |
| `range(0, 1000000) map(_ + 1) sum` | 176,750,243 / 18 | 164,750,245 / 2,000,008 | −6.8% | 164,750,245 / 2,000,008 |

So #1173's gains against its true parent are smaller than its report's
(which measured against `b8bfb43`, before #1169). This branch changes no
bench: the signatures it adds are for functions the benches do not call in
a strict position with a complex argument.
