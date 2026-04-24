//! Use tester and harness tests
use eucalypt::driver::{options::EucalyptOptions, tester};
use eucalypt::syntax::input::Input;
use std::{path::Path, path::PathBuf, str::FromStr};

/// Common options for all tests
pub fn opts(filename: &str) -> EucalyptOptions {
    let lib_path = vec![PathBuf::from("tests/harness")];
    let path = format!("tests/harness/{filename}");

    EucalyptOptions::default()
        .with_explicit_inputs(vec![Input::from_str(&path).unwrap()])
        .with_lib_path(lib_path)
        .build()
}

/// Common options for error tests
pub fn error_opts(filename: &str) -> EucalyptOptions {
    let lib_path = vec![PathBuf::from("tests/harness/errors")];
    let path = format!("tests/harness/errors/{filename}");

    EucalyptOptions::default()
        .with_explicit_inputs(vec![Input::from_str(&path).unwrap()])
        .with_lib_path(lib_path)
        .build()
}

/// Options for IO error tests — enables shell execution via --allow-io
pub fn io_error_opts(filename: &str) -> EucalyptOptions {
    let lib_path = vec![PathBuf::from("tests/harness/errors")];
    let path = format!("tests/harness/errors/{filename}");

    EucalyptOptions::default()
        .with_explicit_inputs(vec![Input::from_str(&path).unwrap()])
        .with_lib_path(lib_path)
        .with_allow_io()
        .build()
}

/// Options for IO monad tests — enables shell execution via --allow-io
pub fn io_opts(filename: &str) -> EucalyptOptions {
    let lib_path = vec![PathBuf::from("tests/harness")];
    let path = format!("tests/harness/{filename}");

    EucalyptOptions::default()
        .with_explicit_inputs(vec![Input::from_str(&path).unwrap()])
        .with_lib_path(lib_path)
        .with_allow_io()
        .build()
}

/// Parse and desugar the test files and analyse for expectations,
/// then run and assert success.
fn run_test(opt: &EucalyptOptions) {
    let exit_code = tester::test(opt).unwrap();
    assert_eq!(exit_code, 0);
}

/// Path to the eu binary built by cargo for this test run.
fn eu_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_eu"))
}

/// Run an error test — validates against `.expect` sidecar if present,
/// otherwise passes as unvalidated.
fn run_error_test(opt: &EucalyptOptions) {
    let exit_code = tester::error_test_with_binary(eu_binary(), opt).unwrap();
    assert_eq!(exit_code, 0);
}

/// Run a type check test via `eu check --strict`.
///
/// Validates exit code and stderr content against the `.expect` sidecar.
fn run_typecheck_test(filename: &str) {
    let path = format!("tests/harness/typecheck/{filename}");
    let expect_path = format!("{path}.expect");

    let output = std::process::Command::new(eu_binary())
        .args(["check", "--strict", &path])
        .output()
        .expect("failed to run eu check");

    let exit_code = output.status.code().unwrap_or(-1);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Parse the .expect sidecar
    let expect_content =
        std::fs::read_to_string(&expect_path).unwrap_or_else(|_| panic!("missing {expect_path}"));

    let mut expected_exit: Option<i32> = None;
    let mut expected_stderr: Option<String> = None;

    for line in expect_content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if let Some(code) = line.strip_prefix("exit:") {
            expected_exit = Some(code.trim().parse().expect("invalid exit code in .expect"));
        }
        if let Some(pattern) = line.strip_prefix("stderr:") {
            let pattern = pattern.trim().trim_matches('"');
            expected_stderr = Some(pattern.to_string());
        }
    }

    if let Some(expected) = expected_exit {
        assert_eq!(
            exit_code, expected,
            "exit code mismatch for {filename}: expected {expected}, got {exit_code}\nstderr: {stderr}"
        );
    }

    if let Some(pattern) = &expected_stderr {
        assert!(
            stderr.contains(pattern.as_str()),
            "stderr for {filename} does not contain \"{pattern}\"\nactual stderr:\n{stderr}"
        );
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
pub fn test_harness_080() {
    run_test(&opts("080_pow.eu"));
}

#[test]
pub fn test_harness_081() {
    run_test(&opts("081_division.eu"));
}

#[test]
pub fn test_harness_082() {
    run_test(&opts("082_graph.eu"));
}

#[test]
pub fn test_harness_083() {
    run_test(&opts("083_running.eu"));
}

#[test]
pub fn test_harness_084() {
    run_test(&opts("084_bitwise.eu"));
}

#[test]
pub fn test_harness_086() {
    run_test(&opts("086_expr_anaphora_parens.eu"));
}

#[test]
pub fn test_harness_087() {
    run_test(&opts("087_arrays.eu"));
}

#[test]
pub fn test_harness_089() {
    run_test(&opts("089_sharing.eu"));
}

#[test]
pub fn test_harness_090() {
    run_test(&opts("090_relative_imports.eu"));
}

#[test]
pub fn test_harness_091() {
    run_test(&opts("091_destructure_block.eu"));
}

#[test]
pub fn test_harness_092() {
    run_test(&opts("092_destructure_list.eu"));
}

#[test]
pub fn test_harness_093() {
    run_test(&opts("093_cons_operator.eu"));
}

#[test]
pub fn test_harness_094() {
    run_test(&opts("094_juxtaposed_call.eu"));
}

#[test]
pub fn test_harness_095() {
    run_test(&opts("095_destructuring_integration.eu"));
}

#[test]
pub fn test_harness_096() {
    run_test(&opts("096_monadic_blocks.eu"));
}

#[test]
pub fn test_harness_097() {
    run_test(&opts("097_idiot_brackets.eu"));
}

#[test]
pub fn test_harness_098() {
    run_test(&opts("098_juxtaposed_definitions.eu"));
}

#[test]
pub fn test_harness_099() {
    run_test(&opts("099_expression_anaphora.eu"));
}

#[test]
pub fn test_harness_100() {
    run_test(&opts("100_if_tail_recursion.eu"));
}

#[test]
pub fn test_harness_101() {
    run_test(&opts("101_non_nil_postfix.eu"));
}

#[test]
pub fn test_harness_102() {
    run_test(&opts("102_destructure_list_in_block.eu"));
}

#[test]
pub fn test_harness_103() {
    run_test(&opts("103_io_render.eu"));
}

#[test]
pub fn test_harness_104() {
    run_test(&io_opts("104_io_basic.eu"));
}

#[test]
pub fn test_harness_105() {
    run_test(&io_opts("105_io_chain.eu"));
}

#[test]
pub fn test_harness_106() {
    run_test(&io_opts("106_io_block_chain.eu"));
}

#[test]
pub fn test_harness_107() {
    run_test(&opts("107_parse_as_json.eu"));
}

#[test]
pub fn test_harness_108() {
    run_test(&opts("108_parse_as_yaml.eu"));
}

#[test]
pub fn test_harness_109() {
    run_test(&opts("109_parse_as_toml.eu"));
}

#[test]
pub fn test_harness_110() {
    run_test(&opts("110_parse_as_csv.eu"));
}

#[test]
pub fn test_harness_111() {
    run_test(&opts("111_parse_as_xml.eu"));
}

#[test]
pub fn test_harness_112() {
    run_test(&opts("112_parse_as_edn.eu"));
}

#[test]
pub fn test_harness_113() {
    run_test(&opts("113_parse_as_jsonl.eu"));
}

#[test]
pub fn test_harness_114() {
    run_test(&opts("114_parse_as_roundtrip.eu"));
}

#[test]
pub fn test_harness_115() {
    run_test(&opts("115_parse_as_data_only.eu"));
}

#[test]
pub fn test_harness_116() {
    run_test(&io_opts("116_io_shell_with.eu"));
}

#[test]
pub fn test_harness_117() {
    run_test(&io_opts("117_io_exec.eu"));
}

#[test]
pub fn test_harness_118() {
    run_test(&io_opts("118_io_exec_with.eu"));
}

#[test]
pub fn test_harness_119() {
    run_test(&io_opts("119_monad_utility.eu"));
}

#[test]
pub fn test_harness_120() {
    run_test(&opts("120_random_monad.eu"));
}

#[test]
pub fn test_harness_121() {
    run_test(&io_opts("121_io_exec_not_found.eu"));
}

#[test]
pub fn test_harness_122() {
    run_test(&opts("122_str_replace.eu"));
}

#[test]
pub fn test_harness_123() {
    run_test(&opts("123_render_to_string.eu"));
}

#[test]
pub fn test_harness_124() {
    run_test(&opts("124_cstring_escapes.eu"));
}

#[test]
pub fn test_harness_125() {
    run_test(&opts("125_expectations.eu"));
}

#[test]
pub fn test_harness_126() {
    run_test(&opts("126_type_predicates.eu"));
}

#[test]
pub fn test_harness_127() {
    run_test(&opts("127_merge_metadata.eu"));
}

#[test]
pub fn test_harness_128() {
    run_test(&opts("128_parse_args.eu"));
}

#[test]
pub fn test_harness_129() {
    run_test(&opts("129_monadic_implicit_return.eu"));
}

#[test]
pub fn test_harness_130() {
    run_test(&opts("130_vec.eu"));
}

#[test]
pub fn test_harness_131() {
    run_test(&opts("131_debug.eu"));
}

#[test]
pub fn test_harness_132() {
    run_test(&opts("132_safe_navigation.eu"));
}

#[test]
pub fn test_harness_133() {
    run_test(&opts("133_lens_import.eu"));
}

#[test]
pub fn test_lib_lens() {
    let opt = EucalyptOptions::default()
        .with_explicit_inputs(vec![Input::from_str("lens.eu").unwrap()])
        .with_lib_path(vec![PathBuf::from("lib")]);
    run_test(&opt);
}

#[test]
pub fn test_harness_134() {
    run_test(&opts("134_match_predicate.eu"));
}

#[test]
pub fn test_harness_135() {
    run_test(&opts("135_dynamic_key_merge.eu"));
}

#[test]
pub fn test_harness_136() {
    run_test(&opts("136_eu_format.eu"));
}

#[test]
pub fn test_harness_137() {
    run_test(&opts("137_diamond_import.eu"));
}

#[test]
pub fn test_harness_138() {
    run_test(&opts("138_partition_window_all.eu"));
}

#[test]
pub fn test_harness_139() {
    run_test(&opts("139_import_alias_rule3.eu"));
}

#[test]
pub fn test_harness_140() {
    run_test(&opts("140_state_monad.eu"));
}

#[test]
pub fn test_harness_141() {
    run_test(&opts("141_deep_destructuring.eu"));
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

#[test]
pub fn test_gc_011() {
    run_test(&opts("gc/gc_011_ndarray_evacuation.eu"));
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

#[test]
pub fn test_error_036() {
    run_error_test(&error_opts("036_arrow_as_lambda.eu"));
}

#[test]
pub fn test_error_038() {
    run_error_test(&error_opts("038_double_equals.eu"));
}

#[test]
pub fn test_error_040() {
    run_error_test(&error_opts("040_json_error_format.eu"));
}

#[test]
pub fn test_error_041() {
    run_error_test(&error_opts("041_modulo_by_zero.eu"));
}

#[test]
pub fn test_error_042() {
    run_error_test(&error_opts("042_map_non_list.eu"));
}

#[test]
pub fn test_error_043() {
    run_error_test(&error_opts("043_assignment_syntax.eu"));
}

#[test]
pub fn test_error_044() {
    run_error_test(&error_opts("044_double_colon_type_annot.eu"));
}

#[test]
pub fn test_error_045() {
    run_error_test(&error_opts("045_list_plus_concat.eu"));
}

#[test]
pub fn test_error_046() {
    run_error_test(&error_opts("046_import_keyword.eu"));
}

#[test]
pub fn test_error_047() {
    run_error_test(&error_opts("047_str_plus_plus.eu"));
}

#[test]
pub fn test_error_048() {
    run_error_test(&error_opts("048_unclosed_interpolation.eu"));
}

#[test]
pub fn test_error_049() {
    run_error_test(&error_opts("049_dot_on_number.eu"));
}

#[test]
pub fn test_error_050() {
    run_error_test(&error_opts("050_dot_on_string.eu"));
}

#[test]
pub fn test_error_051() {
    run_error_test(&error_opts("051_missing_flatten.eu"));
}

#[test]
pub fn test_error_052() {
    run_error_test(&error_opts("052_missing_length.eu"));
}

#[test]
pub fn test_error_053() {
    run_error_test(&error_opts("053_block_in_arithmetic.eu"));
}

#[test]
pub fn test_error_054() {
    run_error_test(&error_opts("054_num_merge_hint.eu"));
}

#[test]
pub fn test_error_055() {
    run_error_test(&error_opts("055_list_not_callable.eu"));
}

#[test]
pub fn test_error_056() {
    run_error_test(&error_opts("056_fold_hint.eu"));
}

#[test]
pub fn test_error_057() {
    run_error_test(&error_opts("057_head_empty_list.eu"));
}

#[test]
pub fn test_error_058() {
    run_error_test(&error_opts("058_python_true.eu"));
}

#[test]
pub fn test_error_059() {
    run_error_test(&error_opts("059_else_keyword.eu"));
}

#[test]
pub fn test_error_060() {
    run_error_test(&error_opts("060_return_keyword.eu"));
}

#[test]
pub fn test_error_061() {
    run_error_test(&error_opts("061_lambda_keyword.eu"));
}

#[test]
pub fn test_error_064() {
    run_error_test(&error_opts("064_has_string_not_sym.eu"));
}

#[test]
pub fn test_error_065() {
    run_error_test(&error_opts("065_even_predicate_hint.eu"));
}

#[test]
pub fn test_error_066() {
    run_error_test(&error_opts("066_odd_predicate_hint.eu"));
}

#[test]
pub fn test_error_067() {
    run_error_test(&error_opts("067_str_upper_hint.eu"));
}

#[test]
pub fn test_error_068() {
    run_error_test(&error_opts("068_str_lower_hint.eu"));
}

#[test]
pub fn test_error_069() {
    run_error_test(&error_opts("069_parse_num_hint.eu"));
}

#[test]
pub fn test_error_070() {
    run_error_test(&error_opts("070_str_upper_method.eu"));
}

#[test]
pub fn test_error_071() {
    run_error_test(&error_opts("071_def_keyword.eu"));
}

#[test]
pub fn test_error_072() {
    run_error_test(&io_error_opts("072_io_fail.eu"));
}

#[test]
pub fn test_error_073() {
    run_error_test(&error_opts("073_sprintf_hint.eu"));
}

#[test]
pub fn test_error_074() {
    run_error_test(&error_opts("074_where_keyword.eu"));
}

#[test]
pub fn test_error_075() {
    run_error_test(&error_opts("075_let_keyword.eu"));
}

#[test]
pub fn test_error_076() {
    run_error_test(&error_opts("076_fn_keyword.eu"));
}

#[test]
pub fn test_error_077() {
    run_error_test(&error_opts("077_double_anaphora.eu"));
}

#[test]
pub fn test_error_078() {
    run_error_test(&error_opts("078_anon_anaphor_scope.eu"));
}

#[test]
pub fn test_error_079() {
    run_error_test(&error_opts("079_camelcase_take_while.eu"));
}

#[test]
pub fn test_error_080() {
    run_error_test(&error_opts("080_array_oob.eu"));
}

#[test]
pub fn test_error_081() {
    run_error_test(&error_opts("081_array_shape_mismatch.eu"));
}

#[test]
pub fn test_error_082() {
    run_error_test(&error_opts("082_array_reshape_mismatch.eu"));
}

#[test]
pub fn test_error_083() {
    run_error_test(&error_opts("083_array_arith_mismatch.eu"));
}

#[test]
pub fn test_error_084() {
    run_error_test(&error_opts("084_destructure_missing_field.eu"));
}

#[test]
pub fn test_error_085() {
    run_error_test(&error_opts("085_destructure_short_list.eu"));
}

#[test]
pub fn test_error_086() {
    run_error_test(&error_opts("086_destructure_empty_cons.eu"));
}

#[test]
pub fn test_error_087() {
    run_error_test(&error_opts("087_bad_regex.eu"));
}

#[test]
pub fn test_error_088() {
    run_error_test(&error_opts("088_numeric_range.eu"));
}

#[test]
pub fn test_error_089() {
    run_error_test(&error_opts("089_numeric_domain.eu"));
}

#[test]
pub fn test_error_090() {
    run_error_test(&error_opts("090_bad_format_string.eu"));
}

#[test]
pub fn test_error_091() {
    // UnknownFormat: verify the error message text directly from the error type.
    // The full pipeline test would require running with -x xml, which the standard
    // harness cannot express, so we test the error variant message directly.
    let err = eucalypt::eval::error::ExecutionError::UnknownFormat("xml".to_string());
    let msg = format!("{err}");
    assert!(
        msg.contains("unknown export format"),
        "expected 'unknown export format' in error, got: {msg}"
    );
    assert!(
        msg.contains("xml"),
        "expected format name 'xml' in error, got: {msg}"
    );
}

#[test]
pub fn test_error_092() {
    // TargetNotFound: verify the error message text directly from the error type.
    // The full pipeline test would require -t nonexistent, which the standard
    // harness cannot express, so we test the error variant message directly.
    let err = eucalypt::core::error::CoreError::TargetNotFound("nonexistent".to_string());
    let msg = format!("{err}");
    assert!(
        msg.contains("not found"),
        "expected 'not found' in error, got: {msg}"
    );
    assert!(
        msg.contains("list-targets"),
        "expected 'list-targets' hint in error, got: {msg}"
    );
}

#[test]
pub fn test_error_093() {
    run_error_test(&error_opts("093_monad_missing_marker.eu"));
}

#[test]
pub fn test_error_094() {
    // Run with the :result target so the evaluand is the IO PAP, which
    // triggers "IO operations require --allow-io" when no flag is given.
    use eucalypt::driver::options::EucalyptOptions;
    let lib_path = vec![
        std::path::PathBuf::from("tests/harness/errors"),
        std::path::PathBuf::from("tests/harness"),
    ];
    let path = "tests/harness/errors/094_io_no_flag.eu".to_string();
    let opt = EucalyptOptions::default()
        .with_explicit_inputs(vec![Input::from_str(&path).unwrap()])
        .with_lib_path(lib_path)
        .with_target(Some("result".to_string()))
        .build();
    run_error_test(&opt);
}

#[test]
pub fn test_error_095() {
    run_error_test(&error_opts("095_parse_as_bad_format.eu"));
}

#[test]
pub fn test_error_096() {
    run_error_test(&error_opts("096_parse_as_bad_input.eu"));
}

#[test]
pub fn test_error_097() {
    run_error_test(&error_opts("097_div_by_zero_source_loc.eu"));
}

#[test]
pub fn test_error_098() {
    run_error_test(&error_opts("098_type_mismatch_operator_loc.eu"));
}

#[test]
pub fn test_error_099() {
    run_error_test(&error_opts("099_string_minus_loc.eu"));
}

#[test]
pub fn test_error_100() {
    run_error_test(&error_opts("100_fn_call_source_loc.eu"));
}

#[test]
pub fn test_error_101() {
    run_error_test(&error_opts("101_named_fn_source_loc.eu"));
}

#[test]
pub fn test_error_102() {
    run_error_test(&error_opts("102_dot_on_list_source_loc.eu"));
}

#[test]
pub fn test_error_103() {
    run_error_test(&error_opts("103_comparison_type_mismatch.eu"));
}

#[test]
pub fn test_error_104() {
    run_error_test(&error_opts("104_comparison_type_mismatch_lte.eu"));
}

#[test]
pub fn test_error_105() {
    run_error_test(&error_opts("105_base64_decode_invalid.eu"));
}

#[test]
pub fn test_error_106() {
    use eucalypt::driver::options::EucalyptOptions;
    let lib_path = vec![
        std::path::PathBuf::from("tests/harness/errors"),
        std::path::PathBuf::from("tests/harness"),
    ];
    let path = "tests/harness/errors/106_io_check_fail.eu".to_string();
    let opt = EucalyptOptions::default()
        .with_explicit_inputs(vec![Input::from_str(&path).unwrap()])
        .with_lib_path(lib_path)
        .with_allow_io()
        .build();
    run_error_test(&opt);
}

#[test]
pub fn test_error_107() {
    run_error_test(&error_opts("107_source_location_in_error.eu"));
}

#[test]
pub fn test_error_108() {
    run_error_test(&error_opts("108_secondary_labels.eu"));
}

#[test]
pub fn test_error_109() {
    run_error_test(&error_opts("109_unknown_arg.eu"));
}

#[test]
pub fn test_error_110() {
    run_error_test(&error_opts("110_unknown_short_arg.eu"));
}

#[test]
pub fn test_error_111() {
    run_error_test(&error_opts("111_missing_option_value.eu"));
}

#[test]
pub fn test_error_112() {
    run_error_test(&error_opts("112_head_empty_list.eu"));
}

#[test]
pub fn test_error_113() {
    run_error_test(&error_opts("113_tail_empty_list.eu"));
}

#[test]
pub fn test_error_115() {
    run_error_test(&error_opts("115_render_as_invalid_format.eu"));
}

#[test]
pub fn test_error_116() {
    run_error_test(&error_opts("116_dbg_scalar.eu"));
}

#[test]
pub fn test_error_117() {
    run_error_test(&error_opts("117_dbg_list.eu"));
}

#[test]
pub fn test_error_118() {
    run_error_test(&error_opts("118_dbg_pipeline_tap.eu"));
}

#[test]
pub fn test_error_119() {
    run_error_test(&error_opts("119_dbg_function_wrap.eu"));
}

#[test]
pub fn test_error_120() {
    run_error_test(&error_opts("120_dbg_pipeline_function.eu"));
}

#[test]
pub fn test_error_121() {
    run_error_test(&error_opts("121_dbg_labelled.eu"));
}

#[test]
pub fn test_error_122() {
    run_error_test(&error_opts("122_dbg_multiarg_function.eu"));
}

#[test]
pub fn test_error_123() {
    run_error_test(&error_opts("123_dbg_block.eu"));
}

#[test]
#[cfg(not(target_os = "windows"))]
pub fn test_error_124() {
    run_error_test(
        &io_error_opts("124_dbg_io_map.eu")
            .with_target(Some("result".to_string()))
            .build(),
    );
}

#[test]
#[cfg(not(target_os = "windows"))]
pub fn test_error_125() {
    run_error_test(
        &io_error_opts("125_dbg_io_function_wrap.eu")
            .with_target(Some("result".to_string()))
            .build(),
    );
}

#[test]
pub fn test_error_126() {
    run_error_test(&error_opts("126_stray_colon_in_call_args.eu"));
}

#[test]
pub fn test_error_127() {
    run_error_test(&error_opts("127_type_mismatch_value_str.eu"));
}

#[test]
pub fn test_error_128() {
    run_error_test(&error_opts("128_type_mismatch_value_sym.eu"));
}

#[test]
pub fn test_error_129() {
    run_error_test(&error_opts("129_head_tail_not_list.eu"));
}

#[test]
pub fn test_error_130() {
    run_error_test(&error_opts("130_simple_lookup_key_not_found.eu"));
}

#[test]
pub fn test_error_131() {
    run_error_test(&error_opts("131_map_user_location.eu"));
}

#[test]
pub fn test_error_132() {
    run_error_test(&error_opts("132_foldl_user_location.eu"));
}

#[test]
pub fn test_error_133() {
    run_error_test(&error_opts("133_stack_trace_cycle_dedup.eu"));
}

#[test]
pub fn test_error_134() {
    run_error_test(&error_opts("134_bool_where_value.eu"));
}

#[test]
pub fn test_error_139() {
    run_error_test(&error_opts("139_list_index_oob.eu"));
}

#[test]
pub fn test_error_135() {
    run_error_test(&error_opts("135_head_empty_list.eu"));
}

#[test]
pub fn test_error_136() {
    run_error_test(&error_opts("136_tail_empty_list.eu"));
}

#[test]
pub fn test_error_137() {
    run_error_test(&error_opts("137_user_panic.eu"));
}

#[test]
pub fn test_error_138() {
    run_error_test(&error_opts("138_internal_no_panic_prefix.eu"));
}

#[test]
pub fn test_harness_142() {
    run_test(&opts("142_consecutive_metadata_blocks.eu"));
}

#[test]
pub fn test_harness_143() {
    run_test(&opts("143_deep_transform.eu"));
}

#[test]
pub fn test_harness_144() {
    run_test(&opts("144_lookup_monad_semantics.eu"));
}

#[test]
pub fn test_harness_145() {
    run_test(&opts("145_list_monad.eu"));
}

#[test]
pub fn test_harness_146() {
    run_test(&opts("146_ceil_floor_brackets.eu"));
}

#[test]
pub fn test_harness_147() {
    run_test(&opts("147_type_annotations.eu"));
}

// ── Type check message tests ──────────────────────────────────────────────────

#[test]
pub fn test_typecheck_001_arg_mismatch() {
    run_typecheck_test("001_arg_mismatch.eu");
}

#[test]
pub fn test_typecheck_002_multi_arg() {
    run_typecheck_test("002_multi_arg.eu");
}

#[test]
pub fn test_typecheck_003_annotation_mismatch() {
    run_typecheck_test("003_annotation_mismatch.eu");
}

#[test]
pub fn test_typecheck_004_invalid_annotation() {
    run_typecheck_test("004_invalid_annotation.eu");
}

#[test]
pub fn test_typecheck_005_no_warnings() {
    run_typecheck_test("005_no_warnings.eu");
}

#[test]
pub fn test_typecheck_006_polymorphic() {
    run_typecheck_test("006_polymorphic.eu");
}

#[test]
pub fn test_typecheck_007_type_unchecked() {
    run_typecheck_test("007_type_unchecked.eu");
}

#[test]
pub fn test_typecheck_008_literal_symbol() {
    run_typecheck_test("008_literal_symbol.eu");
}

#[test]
pub fn test_typecheck_009_function_name_in_warning() {
    run_typecheck_test("009_function_name_in_warning.eu");
}

#[test]
pub fn test_typecheck_010_for_number_binding() {
    run_typecheck_test("010_for_number_binding.eu");
}

#[test]
pub fn test_typecheck_011_for_string_binding() {
    run_typecheck_test("011_for_string_binding.eu");
}

#[test]
pub fn test_typecheck_012_for_correct_binding() {
    run_typecheck_test("012_for_correct_binding.eu");
}

#[test]
pub fn test_typecheck_013_io_number_binding() {
    run_typecheck_test("013_io_number_binding.eu");
}

#[test]
pub fn test_typecheck_014_let_any_binding() {
    run_typecheck_test("014_let_any_binding.eu");
}
