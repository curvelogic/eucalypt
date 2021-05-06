//! Use tester and harness tests
use eucalypt::{
    core::analyse::testplan::TestPlan,
    driver::{
        options::EucalyptOptions, source::SourceLoader, statistics::Timings,
        tester::InProcessTester,
    },
};
use eucalypt::{
    driver::{
        prepare,
        tester::{resolve_input, Tester},
    },
    syntax::input::Input,
};
use std::{path::PathBuf, str::FromStr};

/// Common options for all tests
pub fn opts(filename: &str) -> EucalyptOptions {
    let lib_path = vec![PathBuf::from("eucalypt-hs/harness")];
    let path = format!("eucalypt-hs/harness/test/{}", filename);

    EucalyptOptions::default()
        .with_inputs(vec![Input::from_str(&path).unwrap()])
        .with_lib_path(lib_path)
        .build()
}

/// Parse and desugar the test files and analyse for expectations,
/// then run and assert success.
fn run_test(opt: &EucalyptOptions) {
    let input = opt.inputs().last().unwrap();
    let filename = resolve_input(opt, &input).unwrap();

    let test_plan = {
        let mut loader = SourceLoader::new(opt.lib_path().to_vec());

        // desugar to parse out targets and docstrings
        prepare::prepare(opt, &mut loader, &mut Timings::default()).unwrap();

        // analyse into test plan
        TestPlan::analyse(&filename, loader.core()).unwrap()
    };

    test_plan.prepare_directory().unwrap();

    let tester = InProcessTester {};

    let results = tester.run(&test_plan, opt).unwrap();

    assert!(!results.is_empty());

    for result in results {
        println!("{}", result);
        if let Some(n) = result.exit_code {
            assert_eq!(n, 0);
        } else {
            panic!("test failed to complete");
        }
    }
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
