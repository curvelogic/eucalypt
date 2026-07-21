# Diagnostics provocation corpus

Each entry is a **valid eucalypt program with exactly one injected mistake**, plus a
`.meta.toml` sidecar describing the mistake and where the diagnostic should point.

The corpus feeds `tests/diagnostics_invariants.rs`, which runs every fixture through
`eu --error-format json` and asserts the five objective invariants (see that file's
header). It is the ratchet: adding a fixture, or removing an `xfail`, tightens the gate.

## Adding a fixture

1. Write `<name>.eu` — a short program with one clear mistake. Read
   `docs/reference/agent-reference.md` and `docs/appendices/syntax-gotchas.md` first so
   the mistake is the one you intend.
2. Run `timeout 60 ./target/release/eu --error-format json --heap-limit-mib 2048 <name>.eu`
   and read the JSON. Decide the region the primary span *should* fall in.
3. Write `<name>.meta.toml` (schema in the header of `tests/diagnostics_invariants.rs`).
4. If the diagnostic is already correct, leave `xfail` out — the gate now enforces it.
   If it is currently wrong (a bug this epic will fix), set `xfail = true` with a
   `xfail_reason` naming the bead. When that bead lands, delete the marker and the gate
   turns the fixture into a permanent guard.
