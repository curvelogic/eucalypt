# Assessment: BV2 — annotation side tables (0.15 scope decision)

> Owner-accepted 2026-08-11. Produced for the ROADMAP §9 re-dating decision.

Date: 2026-08-11. Assessor: Fable assessment agent (read-only).
Repo: /Users/greg/dev/curvelogic/eucalypt (local HEAD a561bb71 predates PR #1121;
merged state confirmed via `gh pr view 1121` — MERGED 2026-08-06).

## 1. What BV2 was for

ROADMAP.md §6.4 item 4 (line 404): "Move `Smid`s out of the instruction stream
(retire the `Ann` dispatch step, `vm.rs:536`) into offset-keyed side tables the
error machinery reads. Additive, low risk." Three-part scope:

1. Smids out of the instruction stream;
2. retire `Op::Ann` as a dispatch step (each `Ann` cost a full dispatch
   iteration before its body ran);
3. the error machinery reads the side table instead.

§9 (line 1054) carries the fold-in note: "folded into lever (a) pre-decoded
execution IR if the spike confirms — transition review §7 item 7". The §8
critical path (line 1029) says the same: "lever (a) pre-decoded execution IR
with BV2 side tables folded in (eu-2sa6.1)".

## 2. What shipped instead

The fold-in happened, verbatim, in the predecode IR work (eu-2sa6.13, PRs
#1002/#1004/#1009; default engine since PR #1031, 0.13.1). All three scope
parts are in the shipped code:

1. **Smids out of the instruction stream** —
   `src/eval/bytecode/predecode.rs:81`: `Instr` is compile-time-asserted 16
   bytes with no inline smid field. `predecode.rs:186-188`:
   `DecodedProgram::smids: Vec<Smid>` — "Source annotation per ordinal
   (`Smid::default()` where none). Keyed by ordinal, mirroring instrs" — this
   *is* the offset-keyed side table BV2 called for (ordinal-keyed, which is the
   pre-decoded analogue of offset-keyed).
2. **`Ann` dispatch step retired** — `predecode.rs:302-326` (`ordinal_for`)
   peels `Ann` at decode: the wrapped body takes the ordinal, the smid lands in
   the side table, chained `Ann`s collapse innermost-smid-wins. The dispatch
   loop never steps an `Ann`; `predecode.rs:415-419` and `machine.rs`
   (predecoded match arm) both `unreachable!` on `Op::Ann`.
3. **Error machinery reads the side table** — the predecoded dispatch prologue
   does `let smid = decoded.smids[ord]; if smid.is_valid() { state.annotation
   = smid }` (machine.rs, per the eu-2sa6.1 spike evidence at 2308-2321),
   feeding the same `state.annotation` register all existing diagnostics
   (`ExecutionError::Panic`, trace construction, `set_annotation` /
   `last_annotation` at `machine.rs:263-295, 380-397`) already consume. No new
   wiring was needed.

**Process record:** bead **eu-2sa6.1** ("BV2: Side tables for annotations") is
CLOSED, close reason: "NO-GO spike verdict, coordinator-verified 2026-07-17:
BV2 was already delivered as the 'smid side-table fold-in' inside eu-2sa6.13's
predecode IR work". The spike (owner-ruled spike-first, 2026-07-15; run by
Furnace) verified all three scope parts directly in code before any prototype
was built, and cited the errors harness green (500/500) on the predecoded
default engine as the source-location-correctness gate. The
2026-07-13-predecode-ticks-first report had already scoped BV2's win honestly
("removes steps, but only where `Op::Ann` nodes sit"); the transition-review
reports (B-architecture line 288-291) called BV2 and lever (a) "the same
refactor done once".

**Engine landscape since:** HeapSyn deleted entirely (PR #1121, merged
2026-08-06 — single bytecode engine). The byte-dispatch fallback
(`EU_PREDECODE=0`) is the sole remaining code that still steps `Op::Ann` as a
dispatch arm; its deletion is already scheduled and gated (eu-1hcw, OPEN, 0.14:
release soak + one measured-verified engine-ab session).

## 3. What residue remains (with citations)

Three candidates examined; none is worth scheduling:

**(a) Byte-dispatch path still steps `Op::Ann`** (machine.rs ~1891/1895 area).
This is the pre-BV2 reference implementation retained only for the soak
comparison. eu-1hcw deletes the whole path; a BV2 fold-in there would be
sunk-cost work on code scheduled for removal (the eu-2sa6.1 spike said exactly
this). Residue evaporates with eu-1hcw — no action.

**(b) The wire format still carries `Ann` inline.** `encode.rs:287-295` emits
`Op::Ann` + u32 smid + u32 body offset (9 bytes per annotation) into
`BytecodeProgram::code: Vec<u8>` (program.rs:33-35), which is what the prelude
blob serialises (postcard, BV5). Post-predecode this is a **load-time-only**
cost: `ordinal_for` peels every `Ann` once at startup; zero runtime dispatch
cost. Moving annotations to a serialised side table in the wire format would
be a blob-format change (recorded-review surface, version churn) whose entire
upside is bounded by a fraction of the ~10 ms measured startup floor (ROADMAP
§6.4 item 3) and a modest shrink of a 594 KiB blob. If the blob ever moves to
serialising `DecodedProgram` directly (a BV5-cache/startup question, not a
BV2 one), the side table travels with it for free. Not worth standalone work.

**(c) Side-table memory footprint.** `smids: Vec<Smid>` is 4 bytes/ordinal
mirroring 16-byte `Instr`s — a fixed 25% overhead on the instruction array,
mostly `Smid::default()` entries. A sparse/sorted representation would shrink
it but the dispatch prologue reads `smids[ord]` on *every* instruction — dense
is the correct hot-loop choice, and the total is small (the whole decoded
prelude derives from a 594 KiB blob). Not a problem to solve.

**Semantics check:** chained-`Ann` collapse is innermost-smid-wins, explicitly
matched to the byte path's behaviour (predecode.rs:296-301), and the
source-location-sensitive errors harness is green on the default engine — no
diagnostics-fidelity residue.

## 4. Value & cost of the residue

| Residue | Value | Cost | Verdict |
|---|---|---|---|
| (a) byte-path `Ann` arm | zero (path deleted by eu-1hcw) | — | already scheduled elsewhere |
| (b) wire-format `Ann` inline | < startup floor fraction (~10 ms total, measured, ROADMAP); blob shrink on 594 KiB | blob format rev + recorded review | not worth it; folds into any future blob rev for free |
| (c) 4 B/ordinal side table | negligible memory | hot-path regression risk if sparsified | correct as-is |

No new measurement was needed: the only quantifiable residue (b) is bounded
above by an already-measured figure (the ~10 ms startup floor); everything
else is structural. No performance claim beyond that bound is made
(protocol-compliant: no new engine numbers quoted).

## 5. Recommendation

**ABSORBED — mark superseded.** BV2 was delivered in full as the predecode
fold-in exactly as ROADMAP §9's own conditional note anticipated; the spike
that §9 conditioned on ran (eu-2sa6.1, 2026-07-17) and returned NO-GO on any
further work, coordinator-verified against the code. The single remaining
`Op::Ann` dispatch arm dies with eu-1hcw (already scheduled, 0.14, gated).

Suggested ROADMAP edit (for the owner/docs agent — not made here, read-only):
- §9: change the BV2 row to "*(shipped 0.13 — folded into lever (a)
  pre-decoded IR, eu-2sa6.13; spike eu-2sa6.1 confirmed full absorption,
  NO-GO on residue 2026-07-17)*", matching the BV4/BV5 shipped-row style.
- §10: add a short entry (e.g. "10.6 BV2 standalone side-table work —
  superseded") recording: the fold-in citations above, the eu-2sa6.1 NO-GO,
  and that the wire-format `Ann` residue is deliberately left inline (decode
  peel is load-time-only; revisit only if the blob format is revved for other
  reasons).
- §6.4 item 4: append a "shipped via eu-2sa6.13" note (also fix the stale
  `vm.rs:536` citation — that file was HeapSyn's and vm.rs now serves the
  post-#1121 layout).

No spike needed: the spike already happened and is on the record.
