//! Use tester and harness tests
use eucalypt::driver::{options::EucalyptOptions, tester};
use eucalypt::syntax::input::Input;
use std::{path::PathBuf, str::FromStr};

/// Common options for all tests
pub fn opts(filename: &str) -> EucalyptOptions {
    let lib_path = vec![PathBuf::from("harness/test")];
    let path = format!("harness/test/{filename}");

    EucalyptOptions::default()
        .with_explicit_inputs(vec![Input::from_str(&path).unwrap()])
        .with_lib_path(lib_path)
        .build()
}

/// Common options for error tests
pub fn error_opts(filename: &str) -> EucalyptOptions {
    let lib_path = vec![PathBuf::from("harness/test/errors")];
    let path = format!("harness/test/errors/{filename}");

    EucalyptOptions::default()
        .with_explicit_inputs(vec![Input::from_str(&path).unwrap()])
        .with_lib_path(lib_path)
        .build()
}

/// Parse and desugar the test files and analyse for expectations,
/// then run and assert success.
fn run_test(opt: &EucalyptOptions) {
    let exit_code = tester::test(opt).unwrap();
    assert_eq!(exit_code, 0);
}

/// Run an error test — validates against `.expect` sidecar if present,
/// otherwise passes as unvalidated.
fn run_error_test(opt: &EucalyptOptions) {
    let exit_code = tester::error_test(opt).unwrap();
    assert_eq!(exit_code, 0);
}

#[test]
pub fn test_harness_001() {
    run_test(&opts("001_ski.eu").without_prelude());
}

#[test]
pub fn test_harness_002() {
    run_test(&opts("002_null.eu").without_prelude());
}

#[test]
pub fn test_harness_003() {
    run_test(&opts("003_unicode.eu"));
}

#[test]
pub fn test_harness_004() {
    run_test(&opts("004_bools.eu"));
}

#[test]
pub fn test_harness_005() {
    run_test(&opts("005_if.eu"));
}

#[test]
pub fn test_harness_006() {
    run_test(&opts("006_lists.eu"));
}

#[test]
pub fn test_harness_007() {
    run_test(&opts("007_recursion.eu"));
}

#[test]
pub fn test_harness_008() {
    run_test(&opts("008_folds.eu"));
}

#[test]
pub fn test_harness_009() {
    run_test(&opts("009_lazy.eu"));
}

#[test]
pub fn test_harness_010() {
    run_test(&opts("010_prelude.eu"));
}

#[test]
pub fn test_harness_011() {
    run_test(&opts("011_yaml.yaml"));
}

#[test]
pub fn test_harness_012() {
    run_test(&opts("012_arith.eu"));
}

#[test]
pub fn test_harness_014() {
    run_test(&opts("014_numeric_combinators.eu"));
}

#[test]
pub fn test_harness_015() {
    run_test(&opts("015_block_fns.eu"));
}

#[test]
pub fn test_harness_016() {
    run_test(&opts("016_string_fns.eu"));
}

#[test]
pub fn test_harness_017() {
    run_test(&opts("017_namespacing.eu"));
}

#[test]
pub fn test_harness_018() {
    run_test(&opts("018_metadata.eu"));
}

#[test]
pub fn test_harness_019() {
    run_test(&opts("019_env.eu"));
}

#[test]
pub fn test_harness_020() {
    run_test(&opts("020_op_precedence.eu"));
}

#[test]
pub fn test_harness_021() {
    run_test(&opts("021_calls_and_lookups.eu"));
}

#[test]
pub fn test_harness_022() {
    run_test(&opts("022_sections.eu"));
}

#[test]
pub fn test_harness_023() {
    run_test(&opts("023_yaml_embedding.yaml"));
}

#[test]
pub fn test_harness_024() {
    run_test(&opts("024_interpolation.eu"));
}

#[test]
pub fn test_harness_025() {
    run_test(&opts("025_updates.eu"));
}

#[test]
pub fn test_harness_026() {
    run_test(&opts("026_imports.eu"));
}

#[test]
pub fn test_harness_027() {
    run_test(&opts("027_unit_import.eu"));
}

#[test]
pub fn test_harness_029() {
    run_test(&opts("029_gen_lookup_static.eu"));
}

#[test]
pub fn test_harness_030() {
    run_test(&opts("030_text_import.eu"));
}

#[test]
pub fn test_harness_031() {
    run_test(&opts("031_block_anaphora.eu"));
}

#[test]
pub fn test_harness_032() {
    run_test(&opts("032_number_parse.eu"));
}

#[test]
pub fn test_harness_033() {
    run_test(&opts("033_scans.eu"));
}

#[test]
pub fn test_harness_034() {
    run_test(&opts("034_letters.eu"));
}

#[test]
pub fn test_harness_036() {
    run_test(&opts("036_takes_and_drops.eu"));
}

#[test]
pub fn test_harness_038() {
    run_test(&opts("038_partitions.eu"));
}

#[test]
pub fn test_harness_039() {
    run_test(&opts("039_tags.eu"));
}

#[test]
pub fn test_harness_040() {
    run_test(&opts("040_preserve_tags.yaml"));
}

#[test]
pub fn test_harness_041() {
    run_test(&opts("041_numeric_formats.eu"));
}

#[test]
pub fn test_harness_042() {
    run_test(&opts("042_dot_lookup_precedence.eu"));
}

#[test]
pub fn test_harness_043() {
    run_test(&opts("043_gen_lookup_dynamic.eu"));
}

#[test]
pub fn test_harness_044() {
    run_test(&opts("044_time.eu"));
}

#[test]
pub fn test_harness_046() {
    run_test(&opts("046_commas.eu"));
}

#[test]
pub fn test_harness_047() {
    run_test(&opts("047_xml_import.eu"));
}

#[test]
pub fn test_harness_048() {
    run_test(&opts("048_parse_embed_core.eu"));
}

#[test]
pub fn test_harness_049() {
    run_test(&opts("049_tester.eu"));
}

#[test]
pub fn test_harness_050() {
    run_test(&opts("050_edn.edn"));
}

#[test]
pub fn test_harness_051() {
    run_test(&opts("051_head_or.eu"));
}

#[test]
pub fn test_harness_052() {
    run_test(&opts("052_group_by.eu"));
}

#[test]
pub fn test_harness_053() {
    run_test(&opts("053_discriminate.eu"));
}

#[test]
pub fn test_harness_054() {
    run_test(&opts("054_qsort.eu"));
}

#[test]
pub fn test_harness_055() {
    run_test(&opts("055_jsonl_import.eu"));
}

#[test]
pub fn test_harness_059() {
    run_test(&opts("059_nullary_operators.eu"));
}

#[test]
pub fn test_harness_060() {
    run_test(&opts("060_cstring.eu"));
}

#[test]
pub fn test_harness_061() {
    run_test(&opts("061_yaml_anchors.yaml"));
}

#[test]
pub fn test_harness_062() {
    run_test(&opts("062_yaml_merge.yaml"));
}

#[test]
pub fn test_harness_063() {
    run_test(&opts("063_yaml_timestamps.yaml"));
}

#[test]
pub fn test_harness_064() {
    run_test(&opts("064_polymorphic_cmp.eu"));
}

#[test]
pub fn test_harness_065() {
    run_test(&opts("065_version_assertions.eu"));
}

#[test]
pub fn test_harness_066() {
    run_test(&opts("066_base64.eu"));
}

#[test]
pub fn test_harness_067() {
    run_test(&opts("067_sha256.eu"));
}

#[test]
pub fn test_harness_068() {
    run_test(&opts("068_zdt_literals.eu"));
}

#[test]
pub fn test_harness_069() {
    run_test(&opts("069_sort_keys.eu"));
}

#[test]
pub fn test_harness_070() {
    run_test(&opts("070_bench_validation.eu"));
}

#[test]
pub fn test_harness_071() {
    run_test(&opts("071_sorting_lists.eu"));
}

#[test]
pub fn test_harness_072() {
    run_test(&opts("072_deep_find.eu"));
}

#[test]
pub fn test_harness_073() {
    run_test(&opts("073_block_indexing.eu"));
}

#[test]
pub fn test_harness_074() {
    run_test(&opts("074_sets.eu"));
}

#[test]
pub fn test_harness_075() {
    run_test(&opts("075_deep_query.eu"));
}

#[test]
pub fn test_harness_076() {
    run_test(&opts("076_doc_gen.eu"));
}

#[test]
pub fn test_harness_077() {
    run_test(&opts("077_block_dce.eu"));
}

#[test]
pub fn test_harness_078() {
    run_test(&opts("078_random.eu"));
}

#[test]
pub fn test_harness_079() {
    run_test(&opts("079_streams.eu"));
}

#[test]
pub fn test_gc_001() {
    run_test(&opts("gc/gc_001_basic_collection.eu"));
}

#[test]
pub fn test_gc_002() {
    run_test(&opts("gc/gc_002_stress_allocation.eu"));
}

#[test]
pub fn test_gc_003() {
    run_test(&opts("gc/gc_003_memory_pressure.eu"));
}

#[test]
pub fn test_gc_004() {
    run_test(&opts("gc/gc_004_fragmentation.eu"));
}

#[test]
pub fn test_gc_005() {
    run_test(&opts("gc/gc_005_collection_cycles.eu"));
}

#[test]
pub fn test_gc_006() {
    run_test(&opts("gc/gc_006_object_lifecycle.eu"));
}

#[test]
pub fn test_gc_007() {
    run_test(&opts("gc/gc_007_concurrent_allocation.eu"));
}

#[test]
pub fn test_gc_008() {
    run_test(&opts("gc/gc_008_edge_cases.eu"));
}

#[test]
pub fn test_gc_009() {
    run_test(&opts("gc/gc_009_performance_regression.eu"));
}

#[test]
pub fn test_gc_010() {
    run_test(&opts("gc/gc_010_comprehensive_stress.eu"));
}

// Error tests — validate against `.expect` sidecars when present

#[test]
pub fn test_error_001() {
    run_error_test(&error_opts("001_dot_in_metadata_key.eu"));
}

#[test]
pub fn test_error_002() {
    run_error_test(&error_opts("002_lists.eu"));
}

#[test]
pub fn test_error_003() {
    run_error_test(&error_opts("003_free_var_arg.eu"));
}

#[test]
pub fn test_error_004() {
    run_error_test(&error_opts("004_circular.eu"));
}

#[test]
pub fn test_error_005() {
    run_error_test(&error_opts("005_free_var.eu"));
}

#[test]
pub fn test_error_006() {
    run_error_test(&error_opts("006_div_by_zero.eu"));
}

#[test]
pub fn test_error_008() {
    run_error_test(&error_opts("008_op_spacing.eu"));
}

#[test]
pub fn test_error_009() {
    run_error_test(&error_opts("009_panic.eu"));
}

#[test]
pub fn test_error_010() {
    run_error_test(&error_opts("010_assert.eu"));
}

#[test]
pub fn test_error_011() {
    run_error_test(&error_opts("011_assert_pred.eu"));
}

#[test]
pub fn test_error_012() {
    run_error_test(&error_opts("012_bad_value.eu"));
}

#[test]
pub fn test_error_013() {
    run_error_test(&error_opts("013_bad_nested_value.eu"));
}

#[test]
pub fn test_error_014() {
    run_error_test(&error_opts("014_unterm_strlit.eu"));
}

#[test]
pub fn test_error_015() {
    run_error_test(&error_opts("015_missing_argtuple_close.eu"));
}

#[test]
pub fn test_error_016() {
    run_error_test(&error_opts("016_empty_brackets.eu"));
}

#[test]
pub fn test_error_017() {
    run_error_test(&error_opts("017_too_many_args.eu"));
}

#[test]
pub fn test_error_019() {
    run_error_test(&error_opts("019_no_such_key.eu"));
}

#[test]
pub fn test_error_020() {
    run_error_test(&error_opts("020_no_such_key_fn.eu"));
}

#[test]
pub fn test_error_021() {
    run_error_test(&error_opts("021_bad_num_parse.eu"));
}

#[test]
pub fn test_error_023() {
    run_error_test(&error_opts("023_arg_types.eu"));
}

#[test]
pub fn test_error_024() {
    run_error_test(&error_opts("024_invalid_zdt_literal.eu"));
}

#[test]
pub fn test_error_025() {
    run_error_test(&error_opts("025_malformed_zdt_literal.eu"));
}

#[test]
pub fn test_error_026() {
    run_error_test(&error_opts("026_empty_zdt_literal.eu"));
}

#[test]
pub fn test_error_027() {
    run_error_test(&error_opts("027_self_ref.eu"));
}

#[test]
pub fn test_error_028() {
    run_error_test(&error_opts("028_mutual_cycle.eu"));
}

#[test]
pub fn test_error_029() {
    run_error_test(&error_opts("029_type_mismatch_num.eu"));
}

#[test]
pub fn test_error_030() {
    run_error_test(&error_opts("030_type_mismatch_str.eu"));
}

#[test]
pub fn test_error_031() {
    run_error_test(&error_opts("031_type_mismatch_block_as_num.eu"));
}

#[test]
pub fn test_error_032() {
    run_error_test(&error_opts("032_type_mismatch_num_as_str.eu"));
}

#[test]
pub fn test_error_033() {
    run_error_test(&error_opts("033_type_mismatch_str_as_num.eu"));
}

#[test]
pub fn test_error_034() {
    run_error_test(&error_opts("034_did_you_mean.eu"));
}

#[test]
pub fn test_error_035() {
    run_error_test(&error_opts("035_nested_fn_trace.eu"));
}
