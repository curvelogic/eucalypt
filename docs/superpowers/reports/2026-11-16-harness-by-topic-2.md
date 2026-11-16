# Harness by topic, part 2: `errors/` and `typecheck/` (eu-oaz72.62)

Follows part 1 (`2026-11-16-harness-by-topic.md`).

## Renames

Each `.eu`, its `.eu.expect`, and its `tests/diagnostics/snapshots/errors/*.snap` moved together;
the Rust test function names, fixture paths in the snapshots, the diagnostics corpus metadata,
`docs/guide/contracts.md`, `docs/llms-full.txt` and code comments follow.

| Directory | Change |
|---|---|
| `errors/` | `191`-`195` (drop `m93j`, `1tkk_7_*`, `8a49h`, `og3u6`), `201`/`202` (`sv3`), `206`-`209` and their two `testdata/` imports (`mqlkv`), `229`, `231`, `232` (`1tkk`, `oaz72`), `233_oaz72_43_nth_negative` to `233_nth_negative`, `241_oaz72_32_set_of_non_primitive` to `241_set_of_non_primitive` |
| `errors/` (new numbers) | `233_oaz72_43_update_nth_negative` to `234_update_nth_negative`; `241_oaz72_32_set_of_datetime` to `236_`; `241_oaz72_32_set_of_function` to `237_` (these numbers were free) |
| `typecheck/` | `070`-`083`: the `b2`/`b5`/`b7`/`b9` plan tags dropped, and the same tags in header comments |

Not renamed, by instruction, because the diagnostics divergence inventory
(`tests/diagnostics/DIVERGENCE.md`, generated and held by open work) lists them:
`errors/235_oaz72_49_window_{all_step_zero,size_zero,step_negative,step_zero}` and
`errors/226_pp_concat_non_serialisable`. `225_pp_*` and `227_pp_*` stay with `226` so the series is
renamed in one go, and a CI job selects them with a `pp` filter. Duplicate numbers that remain are
topic series, not bead-named files: `088`, `250`, `275`-`278`, `307`, `308`, `235`. They need an
owner decision (renumbering about 50 fixtures and snapshots; `250_*` is held).

## Counts, before and after

| Measure | Before | After |
|---|---|---|
| `errors/*.eu` / `*.expect` / snapshots | 314 / 309 / 314 | 314 / 309 / 314 |
| `typecheck/*.eu` / `*.expect` | 126 / 125 | 126 / 125 |
| `test_error_*` / `test_typecheck_*` functions | 303 / 125 | 303 / 125 |

Snapshots were re-blessed for renamed fixtures only. The one content change is
`202_contract_bad_spec`: `validate({ ... })` became the juxtaposed `validate{ ... }`, which moves
the primary label's span. The `if` forms in `typecheck/070` and `072` are kept: those fixtures test
how the checker recognises `if` as a brancher; `081`-`083` use `cond` and `then`. `073` also became a juxtaposed call.

## Verification

`cargo test --test harness_test` (691 passed), `--test diagnostics_snapshots` and
`--test diagnostics_trace_anchor_test` pass; fmt and clippy clean.

Fault injection on moved fixtures (each broken, run, restored):

| Fixture | Break | Result |
|---|---|---|
| `errors/233_nth_negative.eu.expect` | expected message changed | FAIL |
| `errors/193_curated_trace.eu.expect` | expected exit code changed | FAIL |
| `typecheck/076_partial_warns_in_total_position.eu.expect` | expected exit code changed | FAIL |
| `snapshots/errors/201_contract_violation.snap` | `primary:` line changed | FAIL (`source_prelude_snapshots_match`) |
