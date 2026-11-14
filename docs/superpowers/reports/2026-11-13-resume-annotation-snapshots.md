# Case continuations restore their push-time annotation: snapshot changes

Bead **eu-oaz72.63** (epic eu-oaz72, owner decision 21, 2026-11-13).

## The change

The bytecode machine's `annotation` register is meant to be scope-accurate:
`SeqBind` and `ApplyTo` restore the annotation live where they were pushed.
Case continuations (`Branch`, `BranchPredecoded`, `FusedPrimopLeft`,
`FusedPrimopRight`) captured it at push but never restored it on resume. Any
code that ran after forcing a scrutinee — an intrinsic's inlined wrapper, an
arithmetic operator, a branch body — therefore saw the location of whatever
annotated code had computed the scrutinee last, and an error raised there
was reported at that location.

The fix (`src/eval/bytecode/machine.rs`, `return_data` / `return_native` /
`return_fun`) calls `set_annotation(annotation)` on every successful resume
of those four continuations: into a branch body, a fallback, the next fused
operand, or the fused intrinsic. As with `SeqBind`, an invalid annotation is
restored too; `last_annotation` remains the fallback for errors raised in
unannotated code. Error paths (`NoBranchForDataTag`, `NoBranchForNative`,
`CannotReturnFunToCase`) are unchanged.

The effect is general: an error raised after a case resumes now carries the
location of the code doing the scrutinising, not of the code that produced
the value. The old location was acting as a crude "where the bad value came
from" pointer, and several snapshots relied on it.

## Changed snapshots

`tests/diagnostics/DIVERGENCE.md` goes from 61 of 276 fixtures diverging (master at the last merge)
between blob and source prelude to 59 of 281. "Old" and "new" are the
primary location (`line:column`), the same in both prelude modes unless
marked.

| fixture | old | new | why | for the user |
|---|---|---|---|---|
| errors/152_set_element_type_mismatch | 1:29 | 1:11 | `set.add` reported at the `set.from-list` its argument ran through; now at `set.add` | better (the bead's target) |
| errors/241_oaz72_32_set_of_function | 2:9 | 2:19 | the list literal's annotation leaked; now the `set.from-list` call | better |
| errors/241_oaz72_32_set_of_datetime | 2:10 | 2:35 | the list literal's annotation leaked; now the `set.of` call | better |
| errors/250_dict_bad_pair_computed | 2:30 | 2:9 | was `dict.size`; now `dict.of`, which rejects the pair | better |
| errors/250_dict_get_missing | 2:9 | 2:31 | was the `dict.of` construction; now the failing `dict.get` | better |
| errors/250_dict_key_depth | 5:9 (source 5:48) | 5:20 | was the `dict.empty` receiver (source prelude: inside the argument); now `dict.assoc` in both modes; one trace frame fewer | better |
| errors/080_array_oob | 2:4 | 2:40 | was `arr.from-flat`; now the failing `arr.get` | better |
| errors/082_array_reshape_mismatch | 2:4 | 2:40 | was `arr.from-flat`; now `arr.reshape` | better |
| errors/276_array_shape_reshape_overflow | 2:4 | 2:17 | was `arr.zeros`; now `arr.reshape` | better |
| errors/277_array_get_non_integer | 2:4 | 2:40 | was `arr.from-flat`; now `arr.get`; spurious trace frame gone | better |
| errors/277_array_slice_negative | 2:4 | 2:40 | was `arr.from-flat`; now the failing `arr.slice` | better |
| errors/277_array_slice_non_integer | 2:4 | 2:40 | was `arr.from-flat`; now the failing `arr.slice` | better |
| errors/277_vec_nth_{negative,non_integer,out_of_range} | 2:19 | 2:19 | primary unchanged; the "called from here" label on the `vec.of[…]` receiver is gone | neutral |
| errors/277_vec_slice_{negative,non_integer} | 2:23 | 2:23 | as above | neutral |
| errors/108_secondary_labels | 4:1 | 4:31 | was the start of `transform`'s declaration; now the failing `+` in `_ + 1`; trace frame folded into labels | better |
| errors/078_anon_anaphor_scope | 5:23 | 5:23 | a trace frame naming the file's header comment (1:1) is gone | better |
| errors/149_not_value_source_loc | 4:9 | 4:9 | as 078 | better |
| errors/263_catenation_before_sum | 5:11 | 5:11 | trace frame was `f`'s declaration (1:1); now `r` at 5:11 | better |
| errors/271_namespace_call_beside_merge | 4:17 | 4:17 | trace frame moves from 4:7 (`str.of`) to 4:17 (`x {a: 1}`, the failing merge) | better |
| errors/250_dict_value_cycle | 3:19 | 3:19 | a duplicated `d` trace frame is gone (both modes) | neutral |
| errors/011_assert_pred | 1:14 | 1:8 | was the predicate's `>`; now the `//=?` assertion; the source-mode secondary label is gone | arguable — see below |
| errors/192_1tkk_7_9_function_not_block | 2:9 | 2:16 | was the under-applied `add(1)`; now the `+` it is passed to | **worse** — see below |
| errors/229_1tkk_7_38_underapplied_call_span | 10:7 (11 cols) | 11:19 (1 col) | as 192, for `foldl(+, 0) + 1` (fixture gained a comment line) | **worse** — see below |
| provocations/catenation_arith | 2:7 | 2:19 | same program as 229 | **worse** — see below |
| provocations/metadata_span | 3:1 | 2:27 | was the `doubled` call passing `"2"`; now `* factor` in `scale`'s body; trace frame gone | **worse** — see below |

`tests/diagnostics/DIVERGENCE.md` is regenerated to match. Five new goldens
(`errors/278_intrinsic_call_site_*`, `errors/088_numeric_range_literal_operands`)
record the fixed behaviour. The last is a case raised by eu-oaz72.12: an
overflow between two computed operands, `(2^62) + (2^62)`, used to caret the
second `^` (the operand evaluated last) rather than the `+`.

The errors/229 fixture no longer pins the eleven-column under-applied-call
span from eu-1tkk.7.38; nothing else does either, so that span is unguarded
until the operand's location is carried with the error.

## Locations that are worse for a user

Re-baselining was accepted by owner decision; these are recorded rather than
papered over.

- **Under-applied operand (192, 229, catenation_arith).** `xs foldl(+, 0) + 1`
  hands the partial application `foldl(+, 0)` to `+`. The old primary
  covered `foldl(+, 0)` — the expression the user must fix — only because
  the operand's annotation leaked. Now it is the `+`. The note "a function
  value cannot be used directly here" still says what went wrong, but the
  pointer to the culprit is lost. Recovering it properly needs the operand's
  own Smid carried with the error (blame the value, not the register), which
  is a separate change.
- **metadata_span.** The primary leaves `doubled(xs): scale("2", xs)` (the
  line the user edits) for the `*` in `scale`'s body, which is correct code.
  This undoes the eu-1tkk.7.34 narrowing; its pinned region is widened back
  from 3..3 to 2..3 (`tests/diagnostics/corpus/metadata_span.meta.toml`),
  the one loosening of the ratchet, made by owner decision.
- **011_assert_pred.** The primary moves from the predicate's `>` to the
  `//=?` assertion. That names the assertion that failed, which is
  defensible, but no longer the comparison inside it.

## Verification

- Four new error fixtures, `tests/harness/errors/278_intrinsic_call_site_*`
  (`set.add`, `set.remove`, `set.from-list`, `vec.of`, each with an element
  computed by an annotated user function), assert the call-site location.
  `tests/harness/errors/088_numeric_range_literal_operands` asserts that
  `(2^62) + (2^62)` carets the overflowing `+` (2:11), not the second `^`.
  Each `.expect` anchors on the primary's `errors/<fixture>.eu:line:col`
  (the trace line names the bare file, so it cannot satisfy the regex).
  Fault injection: with `machine.rs` reverted to master all five fail in
  both prelude modes (the 278 fixtures report `pair`'s body, 2:16, or for
  `vec_of` the list literal, 5:9); restored, all five pass in both modes.
- `cargo test` (all gates), `scripts/blob-mode-tests.sh`, and the harness
  under `EU_GC_VERIFY=2 EU_GC_STRESS=1`.
