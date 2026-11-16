# Harness by topic, part 1: top-level `tests/harness/*.eu` (eu-oaz72.62)

Bead-named top-level harness files are renamed by topic or folded into topic files. Error
fixtures and typecheck fixtures follow in a second PR.

## Mapping

| Before | After |
|---|---|
| `174_sv1_typedata`, `175_sv2_to_spec`, `189_r9oy_union_as_spec`, `196`-`200_sv3_*` | `174_spec_validation.eu`, one `test-*` target each (8 targets) |
| `179_cg3_strict_recurse`, `181_cg3_bomb`, `188_v8n8_cluster_laziness`, `189_branch_gated_self_recursion`, `231_oaz72_1_demand_untaken_branch`, `238_oaz72_40_block_body_demand` | `179_demand_laziness.eu` (6 targets) |
| `232_oaz72_35_range_take_termination`, `233_oaz72_43_negative_counts` | `232_negative_counts.eu` (2 targets) |
| `230_uhd7d_close_brace_after_interpolation`, `234_oaz72_3_interpolation_whitespace` | `230_string_interpolation.eu` (2 targets) |
| `234_oaz72_4_gc_volume_trigger`, `234_oaz72_5_native_storage_gc`, `238_oaz72_55_large_object_churn` | `gc/gc_012_volume_trigger`, `gc/gc_013_native_storage`, `gc/gc_014_large_object_churn` |
| `190_odkp_xml_entity_refs` (+ testdata), `203_ef1_do_monad`, `204_3aa6s_meta_symbol_metadata`, `205_frf2o_inline_annotated_block`, `210_gua64_inline_arg_sharing`, `221_wpswc_settled_slot_passthrough`, `222_pp_par_equivalence`, `223_pazes_max_of_linear`, `224_pp_stream_import_boundary`, `228_bc34x_meta_body_memoisation`, `233_oaz72_34_constant_stack_loops`, `235_oaz72_47_prelude_loops_inline_if`, `235_oaz72_6_fmt_leading_docstring` (now `242_`), `236`/`237`/`239`/`240` sharing files, `241_oaz72_32_set_vec_of` | same files without the bead or phase code (renamed in place) |

Files kept as single files rather than folded all carry a Rust-side gate that names the file or
its targets: a wall-clock deadline (`210`, `228`), a tick budget (`236`, `237`, `239`, `240`),
`max-stack` (`233`), a GC subprocess test (`gc_013`, `gc_014`, `228`) or a formatter fixture
(`242`). Folding them would change what those tests run.

Left alone: `176`-`178`, `187` (`sv_` is a topic abbreviation, and the frozen oracle-corpus
golden files are keyed by these names).

## Counts, before and after (debug build, `eu test`, `-I`)

Over the 39 pre-existing files in scope (excluding the four untouched files above) and the 25
files that replace them:

| Measure | Before | After |
|---|---|---|
| Targets run | 53 | 53 |
| Validations (PASS) | 62 (62) | 62 (62) |
| Assertion operators (`//=`, `//=>`, `//!`, `//=?`, `//=?>`) | 60 | 58 |
| Check items (RESULT list entries plus `checks:` block entries) | 317 | 317 |

The two missing operators are the two mentions of `//=>` in the prose comment of the old
`189_r9oy_union_as_spec.eu`, not assertions: all 57 real assertions in the spec files, and the
one in `190`, are unchanged.

`cargo test --test harness_test`: 684 passed. The gc, large-object and native-container
integration tests and the LSP formatting tests also pass.

## Fault injection

Each break was made in the moved fixture, the matching `cargo test` was run, and the fixture was
restored:

| Fixture | Break | Result |
|---|---|---|
| `174_spec_validation.eu` | `//=> true` flipped to `false` (`test-as-spec`) | FAIL |
| `174_spec_validation.eu` | `t19 ... count = 1` changed to `2` (`test-validate`) | FAIL |
| `179_demand_laziness.eu` | `ho-sum` expectation off by one | FAIL |
| `179_demand_laziness.eu` | `pass-through` made to return its lazy argument (forces the bomb) | FAIL |
| `232_negative_counts.eu` | `drop-negative-finite` expectation changed | FAIL |
| `230_string_interpolation.eu` | `lone-after` expectation changed | FAIL |
| `gc/gc_014_large_object_churn.eu` | `t1` expectation changed | FAIL |
| `233_constant_stack_loops.eu` | `constant-stack` validator bound `< 1000` changed to `< 1` | FAIL |

## Style

Every moved file: header comment became unit `doc` metadata, `RESULT: if(..., :PASS, :FAIL)`
became `... then(:PASS, :FAIL)`, simple two-way `if` became `then`, and single-list-argument calls
use the juxtaposed form. Per-declaration `#` comments were left in place: converting them to
backtick docs attaches metadata to the binding, which changes what the metadata tests
(`204`, `205`), the metadata-memoisation test (`228`) and the stream-import test (`224`) evaluate.
`233` and `gc_012`/`gc_013` keep `if`: they test the `if` form, or nest it.
