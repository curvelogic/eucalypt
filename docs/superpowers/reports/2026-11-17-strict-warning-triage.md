# `eu check --strict` triage across the harness (eu-oaz72.168)

Fresh release build from master; `eu check --strict` over 325 files
(`tests/harness/*.eu`, `tests/harness/typecheck/*.eu`; `examples/` holds only
`aoc25`, no warnings). 63 files exit non-zero: 14 in `tests/harness/`, 49 in
`typecheck/`.

| Category | Files | Outcome |
|---|---|---|
| (a) wrong prelude signature | 010 (`interleave`) | Fixed: `interleave :: [a] → [b] → [a \| b]`. Gated by `typecheck/127`. |
| decision | 110, 111, 113 (`parse-as` returns documented partial `any?`, so `head`/`count` on it warns by design) | Owner decision eu-s2ato; signature unchanged |
| (b) checker false positive | 015, 047, 119, 120, 129, 141, 144, 163 | Beads filed (below) |
| (c) deliberately ill-typed | 142 (uses deprecated `state.exec`), 217 (`kv-block` with non-symbol keys), `typecheck/*` (49 fixtures, each an intended warning/error) | Left |

## False positives filed

- eu-oaz72.169: `lookup(:k)` over a list of blocks with differing fields (015).
- eu-oaz72.170: derived monad `map` on identity-like monads and `random.map` (119, 120).
- eu-oaz72.171: `:let`/`:id` monadic blocks typed as their result (129, 144).
- eu-oaz72.172: list-with-block destructuring parameter (141).
- eu-oaz72.173: `eu check` ignores `prelude:` metadata (163).
- eu-oaz72.174: `drop` over heterogeneous imported XML (047).

## AoC impact

Under `--strict`, ordinary code can still fail on: `lookup`/`map` over lists of
blocks with differing keys, user-defined or `random` monad `map`, and monadic
(`:let`) blocks used with block operations.

## Allow-list

No allow-list file is needed: the deliberate cases (142, 217 and `typecheck/*`)
are not run under `--strict` by anything except the `typecheck/` harness, whose
`.expect` sidecars already state the expected warnings.

## Also fixed

eu-oaz72.127: `bind_var` widens literal symbols inside tuples (fixture 128).
