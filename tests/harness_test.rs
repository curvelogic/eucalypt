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

/// Parse and desugar the test files and analyse for expectations,
/// then run and assert success.
fn run_test(opt: &EucalyptOptions) {
    let exit_code = tester::test(opt).unwrap();
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
