# Many-declaration snapshot fixtures

Fixtures for the golden diagnostic snapshot harness
(`tests/diagnostics/SNAPSHOTS.md`) whose only distinguishing feature is
**size**.

## Why they exist

Two diagnostics defects found in one week were invisible to every existing test
because every test input was small:

* **eu-og3u6** — the primary label and the curated trace anchor name the wrong
  declaration once other bindings render first. In a two-line file the anchor is
  correct; in a 900-line file it names whichever binding happened to be rendered
  first.
* **eu-r4647** — a safety argument in `src/core/typecheck/check.rs` rests on
  using "a Smid far outside any range this test's tiny SourceMap will ever
  register". That is precisely the reasoning eu-7x0r disproved: a baked Smid
  fails to resolve in a small program and resolves against an *unrelated user
  declaration* once the SourceMap grows past it.

A snapshot corpus made only of small files would have inherited that blind spot
and detected neither. These fixtures exist to make the corpus size-sensitive on
purpose.

They earned their keep immediately: `many_decls_late_nth` shows the blob-prelude
trace naming **three unrelated `padNNNN` declarations** for a failure in
`result`, and `many_decls_early_nth` shows that the "correct in a small file"
property does not survive into blob mode at all.

## The fixtures

| fixture | shape | what it discriminates |
|---|---|---|
| `many_decls_late_nth.eu` | 900 pads, then the failing call | the eu-og3u6 repro exactly |
| `many_decls_early_nth.eu` | the failing call, then 900 pads | control: if this and the above blame differently, *position* is driving the diagnostic, not the mistake |
| `many_decls_middle_nth.eu` | 450 pads, the failing call, 450 pads | distinguishes "blames the first declaration" from "blames a fixed low source position" |
| `huge_decls_type_mismatch.eu` | ~2000 pads, then `"a" + 1` | grows the runtime SourceMap well past the low Smid range baked prelude/xtask Smids occupy (eu-r4647) |
| `huge_decls_type_warning.eu` | 1500 pads, then `str.letters(42)` | the same range, but on the **type-checker** path, which is where eu-r4647 is actually open |
| `many_decls_nested_user_fn.eu` | 600 pads, a user helper, a late call | a correct curated trace names *two* user frames, not one |
| `many_decls_lookup_miss.eu` | 600 pads, then a key typo | the "did you mean?" path alongside the blame question |

The padding declarations are `padNNNN: NNNN + 0` — deliberately trivial, so that
any declaration a diagnostic names other than `result` is unambiguously wrong.
They are zero-padded to four digits so the names sort stably and a misblamed
declaration is identifiable at a glance.

## Regenerating

These are mechanical, so they are generated rather than hand-edited. To
reproduce a fixture, emit the header comment followed by `padNNNN: NNNN + 0`
lines and the failing declaration in the order the table above describes, then
run `cargo xtask diag-snapshot --bless`.

## Scope

These fixtures are used **only** by the snapshot harness, not by
`tests/diagnostics_invariants.rs`. That is deliberate: several of them currently
render a *wrong* diagnostic (that is why they exist), and a snapshot records
what the output is without asserting it is correct. Once eu-og3u6 and eu-r4647
land, the snapshot diff is the evidence they were fixed — and promoting the
fixtures into the invariant corpus with `.meta.toml` sidecars is then the way to
lock the gain permanently.
