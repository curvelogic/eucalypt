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

/// Run `eu doc` on a fixture file and assert that stdout contains all the given patterns.
fn run_doc_test(path: &str, extra_args: &[&str], expected_patterns: &[&str]) {
    let output = std::process::Command::new(eu_binary())
        .args(["doc"])
        .args(extra_args)
        .arg(path)
        .output()
        .expect("failed to run eu doc");

    let exit_code = output.status.code().unwrap_or(-1);
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert_eq!(
        exit_code, 0,
        "eu doc exited with {exit_code} for {path}\nstdout: {stdout}"
    );

    for pattern in expected_patterns {
        assert!(
            stdout.contains(pattern),
            "eu doc stdout for {path} does not contain {pattern:?}\nactual stdout:\n{stdout}"
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

// ── Engine A/B canonical suite (eu-2sa6.6) ──────────────────────────────────
//
// The seven new class-coverage benches plus the frozen env-walk fold. Each
// carries a PASS/FAIL RESULT gated by an explicit `verify: ["default-expectation"]`
// (a bare `bench-*` target would otherwise auto-pass). These run under `cargo
// test` on the default (bytecode) engine and under `EU_HEAPSYN=1 cargo test` on
// HeapSyn, so both engines are exercised. See docs/superpowers/engine-ab/.
//
// They are >1s each by design (to escape the startup-noise floor), so the whole
// group adds ~15-20s to the suite. The full interleaved timing run lives in
// `cargo xtask engine-ab`, not here.

#[test]
pub fn test_bench_015_block_merge() {
    run_test(&opts("bench/015_block_merge.eu"));
}

#[test]
pub fn test_bench_016_import_export_yaml() {
    run_test(&opts("bench/016_import_export_yaml.eu"));
}

#[test]
pub fn test_bench_017_import_export_toml() {
    run_test(&opts("bench/017_import_export_toml.eu"));
}

#[test]
pub fn test_bench_018_string_scale() {
    run_test(&opts("bench/018_string_scale.eu"));
}

#[test]
pub fn test_bench_019_list_scale() {
    run_test(&opts("bench/019_list_scale.eu"));
}

#[test]
pub fn test_bench_020_lookup_curve() {
    run_test(&opts("bench/020_lookup_curve.eu"));
}

#[test]
pub fn test_bench_021_io_loop() {
    // `io.map-m` over 1000 actions builds its bind chain via the prelude's
    // recursive `sequence` (non-tail: `seq-step` recurses into `sequence`
    // before constructing the bind), which recurses natively in Rust once
    // per IO step. In a release build this is cheap enough per frame to fit
    // comfortably in the default thread stack; in the unoptimised debug
    // build `cargo test` uses in CI, the much larger per-frame footprint
    // overflows the default test-thread stack (observed on ubuntu-latest,
    // windows-latest, and reproduced locally on macOS debug builds — this
    // is a debug-build stack-depth issue, not a platform/io.shell
    // portability issue). Run on a dedicated thread with a generous stack
    // rather than shrinking the bench's iteration count.
    std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(|| run_test(&io_opts("bench/021_io_loop.eu")))
        .expect("spawn test_bench_021_io_loop thread")
        .join()
        .expect("test_bench_021_io_loop thread panicked");
}

#[test]
pub fn test_bench_022_hof_fold() {
    run_test(&opts("bench/022_hof_fold.eu"));
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

/// Runs `lib/markup.eu` as the top-level file so its own `test-*` target
/// (`test-deconstruct-head`) is auto-discovered and executed (eu-8t2j).
/// Mirrors `test_lib_lens` above, which does the same for `lib/lens.eu`'s
/// eight `test-*` targets — every baked-in library resource that ships an
/// embedded test target must have a harness test exercising it this way.
#[test]
pub fn test_lib_markup() {
    let opt = EucalyptOptions::default()
        .with_explicit_inputs(vec![Input::from_str("markup.eu").unwrap()])
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
pub fn test_error_140() {
    run_error_test(&error_opts("140_invalid_string_interpolation.eu"));
}

#[test]
pub fn test_error_141() {
    run_error_test(&error_opts("141_calzdt_year_overflow.eu"));
}

#[test]
pub fn test_error_142() {
    run_error_test(&error_opts("142_render_unknown_format.eu"));
}

#[test]
pub fn test_error_143() {
    run_error_test(&error_opts("143_bitwise_float.eu"));
}

#[test]
pub fn test_error_144() {
    run_error_test(&error_opts("144_str_replace_hint.eu"));
}

#[test]
pub fn test_error_145() {
    run_error_test(&error_opts("145_str_starts_with_hint.eu"));
}

#[test]
pub fn test_error_146() {
    run_error_test(&error_opts("146_str_ends_with_hint.eu"));
}

#[test]
pub fn test_error_147() {
    run_error_test(&error_opts("147_str_contains_hint.eu"));
}

#[test]
pub fn test_error_148() {
    run_error_test(&error_opts("148_not_callable_source_loc.eu"));
}

#[test]
pub fn test_error_149() {
    run_error_test(&error_opts("149_not_value_source_loc.eu"));
}

#[test]
pub fn test_error_150() {
    run_error_test(&error_opts("150_bad_timezone_source_loc.eu"));
}

#[test]
pub fn test_error_151() {
    run_error_test(&error_opts("151_set_add_non_set.eu"));
}

#[test]
pub fn test_error_152() {
    run_error_test(&error_opts("152_set_element_type_mismatch.eu"));
}

#[test]
pub fn test_error_153() {
    run_error_test(&error_opts("153_unexpected_expr_stg_compiler.eu"));
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

#[test]
pub fn test_harness_148() {
    run_test(&opts("148_symbol_target_shortcut.eu"));
}

#[test]
pub fn test_harness_149() {
    run_test(&opts("149_bracket_inline_monad.eu"));
}

#[test]
pub fn test_harness_150() {
    run_test(&opts("150_named_diamond_import.eu"));
}

#[test]
pub fn test_harness_151() {
    run_test(&opts("151_nested_named_diamond.eu"));
}

#[test]
pub fn test_harness_152() {
    run_test(&opts("152_at_operator.eu"));
}

#[test]
pub fn test_harness_153() {
    run_test(&opts("153_cross_type_eq.eu"));
}

#[test]
pub fn test_harness_154() {
    run_test(&opts("154_fmt_lazy_values.eu"));
}

#[test]
pub fn test_harness_155() {
    run_test(&opts("155_cross_import_bracket.eu"));
}

#[test]
pub fn test_harness_156() {
    run_test(&opts("156_plain_doc_string_render.eu"));
}

#[test]
pub fn test_harness_157() {
    run_test(&opts("157_static_lookup_branches.eu"));
}

#[test]
pub fn test_harness_158() {
    run_test(&opts("158_export_internal.eu"));
}

/// Run a trace test: execute via the `eu` binary, check exit code 0 and verify
/// that every pattern in `expected_stderr` appears somewhere in stderr output.
fn run_trace_test(filename: &str, expected_stderr: &[&str]) {
    let path = format!("tests/harness/{filename}");
    let output = std::process::Command::new(eu_binary())
        .arg(&path)
        .output()
        .unwrap_or_else(|e| panic!("failed to run eu for {filename}: {e}"));

    let exit_code = output.status.code().unwrap_or(-1);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        exit_code, 0,
        "trace test {filename} exited with code {exit_code}\nstderr:\n{stderr}"
    );

    for pattern in expected_stderr {
        assert!(
            stderr.contains(pattern),
            "stderr for {filename} does not contain {pattern:?}\nactual stderr:\n{stderr}"
        );
    }
}

/// Trace entry only (lazy): args shown as `<thunk>`.
#[test]
pub fn test_harness_159() {
    run_trace_test("159_trace_lazy.eu", &["→ add(x: <thunk>, y: <thunk>)"]);
}

/// Trace entry only (strict): args forced before logging.
#[test]
pub fn test_harness_160() {
    run_trace_test("160_trace_strict.eu", &["→ add(x: 1, y: 2)"]);
}

/// Trace entry and exit (lazy): result shown as `<thunk>`.
#[test]
pub fn test_harness_161() {
    run_trace_test(
        "161_trace_exit.eu",
        &["→ add(x: <thunk>, y: <thunk>)", "← add: <thunk>"],
    );
}

/// Trace entry and exit (strict): args and result forced before logging.
#[test]
pub fn test_harness_162() {
    run_trace_test(
        "162_trace_strict_exit.eu",
        &["→ add(x: 1, y: 2)", "← add: 3"],
    );
}

#[test]
pub fn test_harness_163() {
    run_test(&opts("163_prelude_selection.eu"));
}

#[test]
pub fn test_harness_164() {
    run_test(&opts("164_s_string.eu"));
}

#[test]
pub fn test_harness_165() {
    run_test(&opts("165_demand_annotation.eu"));
}

#[test]
pub fn test_error_154() {
    run_error_test(&error_opts("154_internal_import_error.eu"));
}

#[test]
pub fn test_error_155() {
    run_error_test(&error_opts("155_block_content_in_soup_bracket.eu"));
}

#[test]
pub fn test_error_156() {
    run_error_test(&error_opts("156_soup_content_in_monad_bracket.eu"));
}

#[test]
pub fn test_error_157() {
    run_error_test(&error_opts("157_self_ref_function_call.eu"));
}

#[test]
pub fn test_target_symbol_shortcut_alpha() {
    let output = std::process::Command::new(eu_binary())
        .args(["-t", "alpha", "tests/harness/148_symbol_target_shortcut.eu"])
        .output()
        .expect("failed to run eu");
    assert_eq!(output.status.code(), Some(0), "eu -t alpha should succeed");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains('1'),
        "alpha target should render 1, got: {stdout}"
    );
}

#[test]
pub fn test_target_symbol_shortcut_self_named() {
    let output = std::process::Command::new(eu_binary())
        .args([
            "-t",
            "self-named",
            "tests/harness/148_symbol_target_shortcut.eu",
        ])
        .output()
        .expect("failed to run eu");
    assert_eq!(
        output.status.code(),
        Some(0),
        "eu -t self-named should succeed"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains('3'),
        "self-named target should render 3, got: {stdout}"
    );
}

#[test]
pub fn test_target_symbol_suppress_still_works() {
    let output = std::process::Command::new(eu_binary())
        .args(["tests/harness/148_symbol_target_shortcut.eu"])
        .output()
        .expect("failed to run eu");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("hidden"),
        ":suppress should suppress 'hidden' from output, got: {stdout}"
    );
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

// ── Prelude type annotation regression tests ──────────────────────────────────

/// Run `eu check <path>` and assert zero warnings (exit 0, empty stderr).
fn run_prelude_check(path: &str) {
    let output = std::process::Command::new(eu_binary())
        .args(["check", path])
        .output()
        .unwrap_or_else(|e| panic!("failed to run eu check on {path}: {e}"));

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        output.status.code(),
        Some(0),
        "eu check {path} exited with non-zero status; stderr:\n{stderr}"
    );
    assert!(
        stderr.is_empty(),
        "eu check {path} produced unexpected warnings:\n{stderr}"
    );
}

#[test]
pub fn test_prelude_check_zero_warnings() {
    run_prelude_check("lib/prelude.eu");
}

#[test]
pub fn test_state_check_zero_warnings() {
    run_prelude_check("lib/state.eu");
}

#[test]
pub fn test_typecheck_015_row_var_no_warning() {
    run_typecheck_test("015_row_var_no_warning.eu");
}

#[test]
pub fn test_typecheck_016_row_var_missing_field() {
    run_typecheck_test("016_row_var_missing_field.eu");
}

#[test]
pub fn test_typecheck_017_row_var_wrong_field_type() {
    run_typecheck_test("017_row_var_wrong_field_type.eu");
}

#[test]
pub fn test_typecheck_018_block_app_merge_no_warn() {
    run_typecheck_test("018_block_app_merge_no_warn.eu");
}

#[test]
pub fn test_typecheck_019_block_app_rhs_overrides() {
    run_typecheck_test("019_block_app_rhs_overrides.eu");
}

#[test]
pub fn test_typecheck_020_block_app_gradual_boundary() {
    run_typecheck_test("020_block_app_gradual_boundary.eu");
}

#[test]
pub fn test_typecheck_021_row_var_block_app_preserves_field() {
    run_typecheck_test("021_row_var_block_app_preserves_field.eu");
}

// DEFERRED: `synthesise_app` does not yet propagate row variable bindings
// from function arguments into the return type.  When `extend({x:1})` is
// checked, unification binds `r → {}` but that substitution is not applied
// to the declared return type, so the result is treated as open and `result.w`
// produces no warning.  This test documents the desired end state.
#[test]
pub fn test_typecheck_022_row_var_block_app_absent_field_warns() {
    run_typecheck_test("022_row_var_block_app_absent_field_warns.eu");
}

// ── Monad namespace field type tests ─────────────────────────────────────────
//
// Verify that the explicit record type annotations on monad namespaces do not
// produce spurious warnings for well-typed direct field access calls.
//
// DEFERRED: type-mismatch warnings for direct-call monad field access
//   e.g. `for.bind(42, identity)` should warn "expected [a], found number".
//   The checker currently does not synthesise types through direct field access
//   on namespace bindings defined with function-definition syntax (f(x): ...).
//   Colon-block syntax (tests 010–014) is the supported path today.

#[test]
pub fn test_typecheck_023_for_map_no_warning() {
    run_typecheck_test("023_for_map_no_warning.eu");
}

#[test]
pub fn test_typecheck_024_for_bind_no_warning() {
    run_typecheck_test("024_for_bind_no_warning.eu");
}

// ── Row polymorphism prelude annotations ────────────────────────────────────

#[test]
pub fn test_typecheck_025_merge_row_poly_no_warning() {
    run_typecheck_test("025_merge_row_poly_no_warning.eu");
}

#[test]
pub fn test_typecheck_029_merge_all_row_poly_no_warning() {
    run_typecheck_test("029_merge_all_row_poly_no_warning.eu");
}

// ── Dict type prelude annotations ──────────────────────────────────────────

#[test]
pub fn test_typecheck_026_map_values_dict_no_warning() {
    run_typecheck_test("026_map_values_dict_no_warning.eu");
}

#[test]
pub fn test_typecheck_027_values_keys_dict_no_warning() {
    run_typecheck_test("027_values_keys_dict_no_warning.eu");
}

#[test]
pub fn test_typecheck_028_group_by_dict_no_warning() {
    run_typecheck_test("028_group_by_dict_no_warning.eu");
}

// ── Recursive types (equirecursive Mu) ─────────────────────────────────────

#[test]
pub fn test_typecheck_030_recursive_alias_no_hang() {
    run_typecheck_test("030_recursive_alias_no_hang.eu");
}

#[test]
pub fn test_typecheck_031_json_function_no_warning() {
    run_typecheck_test("031_json_function_no_warning.eu");
}

#[test]
pub fn test_typecheck_032_nonrecursive_alias_flat() {
    run_typecheck_test("032_nonrecursive_alias_flat.eu");
}

#[test]
pub fn test_typecheck_033_json_dict_no_warning() {
    run_typecheck_test("033_json_dict_no_warning.eu");
}

#[test]
pub fn test_typecheck_034_literal_string_synthesis() {
    run_typecheck_test("034_literal_string_synthesis.eu");
}

#[test]
pub fn test_typecheck_035_literal_string_dsl() {
    run_typecheck_test("035_literal_string_dsl.eu");
}

#[test]
pub fn test_typecheck_036_literal_string_mismatch() {
    run_typecheck_test("036_literal_string_mismatch.eu");
}

#[test]
pub fn test_typecheck_037_union_absorbs_literal_string() {
    run_typecheck_test("037_union_absorbs_literal_string.eu");
}

#[test]
pub fn test_typecheck_038_literal_string_annotation_widens() {
    run_typecheck_test("038_literal_string_annotation_widens.eu");
}

#[test]
pub fn test_typecheck_039_narrowing_true_branch_no_warn() {
    run_typecheck_test("039_narrowing_true_branch_no_warn.eu");
}

#[test]
pub fn test_typecheck_040_narrowing_subtraction_no_warn() {
    run_typecheck_test("040_narrowing_subtraction_no_warn.eu");
}

#[test]
pub fn test_typecheck_041_narrowing_cond_no_warn() {
    run_typecheck_test("041_narrowing_cond_no_warn.eu");
}

#[test]
pub fn test_typecheck_042_narrowing_rebind_no_false_positive() {
    run_typecheck_test("042_narrowing_rebind_no_false_positive.eu");
}

#[test]
pub fn test_typecheck_043_narrowing_nil_check() {
    run_typecheck_test("043_narrowing_nil_check.eu");
}

#[test]
pub fn test_typecheck_044_clause_operator() {
    run_typecheck_test("044_clause_operator.eu");
}

#[test]
pub fn test_typecheck_045_narrowing_union_subtraction() {
    run_typecheck_test("045_narrowing_union_subtraction.eu");
}

#[test]
pub fn test_typecheck_046_narrowing_then_no_warn() {
    run_typecheck_test("046_narrowing_then_no_warn.eu");
}

#[test]
pub fn test_typecheck_047_narrowing_user_brancher() {
    run_typecheck_test("047_narrowing_user_brancher.eu");
}

#[test]
pub fn test_typecheck_048_nonempty_list_literal_tuple() {
    run_typecheck_test("048_nonempty_list_literal_tuple.eu");
}

#[test]
pub fn test_typecheck_049_nonempty_nil_narrowing() {
    run_typecheck_test("049_nonempty_nil_narrowing.eu");
}

#[test]
pub fn test_typecheck_050_nonempty_head_tuple_precise() {
    run_typecheck_test("050_nonempty_head_tuple_precise.eu");
}

#[test]
pub fn test_typecheck_051_nonempty_empty_head_warns() {
    run_typecheck_test("051_nonempty_empty_head_warns.eu");
}

#[test]
pub fn test_typecheck_052_for_elem_type_correct_use() {
    run_typecheck_test("052_for_elem_type_correct_use.eu");
}

#[test]
pub fn test_typecheck_053_for_string_elem_correct() {
    run_typecheck_test("053_for_string_elem_correct.eu");
}

#[test]
pub fn test_typecheck_054_io_elem_type_correct() {
    run_typecheck_test("054_io_elem_type_correct.eu");
}

#[test]
pub fn test_typecheck_055_user_monad_elem_type() {
    run_typecheck_test("055_user_monad_elem_type.eu");
}

#[test]
pub fn test_typecheck_056_hkt_list_app_no_warning() {
    run_typecheck_test("056_hkt_list_app_no_warning.eu");
}

#[test]
pub fn test_typecheck_057_hkt_forall_annotation_parse() {
    run_typecheck_test("057_hkt_forall_annotation_parse.eu");
}

#[test]
pub fn test_typecheck_058_hkt_kind_annotation_parse() {
    run_typecheck_test("058_hkt_kind_annotation_parse.eu");
}

#[test]
pub fn test_typecheck_059_user_monad_hkt_map_type() {
    run_typecheck_test("059_user_monad_hkt_map_type.eu");
}

#[test]
pub fn test_typecheck_060_let_correct_binding() {
    run_typecheck_test("060_let_correct_binding.eu");
}

#[test]
pub fn test_typecheck_061_let_wrong_type() {
    run_typecheck_test("061_let_wrong_type.eu");
}

#[test]
pub fn test_typecheck_062_for_correct_binding() {
    run_typecheck_test("062_for_correct_binding.eu");
}

#[test]
pub fn test_typecheck_063_for_wrong_binding() {
    run_typecheck_test("063_for_wrong_binding.eu");
}

// ── B6: Dependent record indexed access ────────────────────────────────────

#[test]
pub fn test_typecheck_064_lookup_literal_key_correct() {
    run_typecheck_test("064_lookup_literal_key_correct.eu");
}

#[test]
pub fn test_typecheck_065_lookup_key_typo_warns() {
    run_typecheck_test("065_lookup_key_typo_warns.eu");
}

#[test]
pub fn test_typecheck_066_lookup_open_record_no_warning() {
    run_typecheck_test("066_lookup_open_record_no_warning.eu");
}

#[test]
pub fn test_typecheck_067_lookup_dict_value_type() {
    run_typecheck_test("067_lookup_dict_value_type.eu");
}

#[test]
pub fn test_typecheck_068_projection_second_correct() {
    run_typecheck_test("068_projection_second_correct.eu");
}

#[test]
pub fn test_typecheck_069_projection_value_type_mismatch() {
    run_typecheck_test("069_projection_value_type_mismatch.eu");
}

// ── B7: Prelude type-summary cache — behaviour preservation ──────────────────

#[test]
pub fn test_typecheck_070_b7_predicate_narrowing() {
    run_typecheck_test("070_b7_predicate_narrowing.eu");
}

#[test]
pub fn test_typecheck_071_b7_then_brancher() {
    run_typecheck_test("071_b7_then_brancher.eu");
}

#[test]
pub fn test_typecheck_072_b7_user_brancher_wrapping_free() {
    run_typecheck_test("072_b7_user_brancher_wrapping_free.eu");
}

#[test]
pub fn test_typecheck_073_b7_head_tail_free() {
    run_typecheck_test("073_b7_head_tail_free.eu");
}

#[test]
pub fn test_typecheck_074_b5_partial_type_parses() {
    run_typecheck_test("074_b5_partial_type_parses.eu");
}

#[test]
pub fn test_typecheck_075_b5_execution_error_type() {
    run_typecheck_test("075_b5_execution_error_type.eu");
}

#[test]
pub fn test_typecheck_076_b5_partial_warns_in_total_position() {
    run_typecheck_test("076_b5_partial_warns_in_total_position.eu");
}

#[test]
pub fn test_typecheck_077_b5_partial_silent_in_any() {
    run_typecheck_test("077_b5_partial_silent_in_any.eu");
}

#[test]
pub fn test_typecheck_078_b9_merge_row_inference_no_warn() {
    run_typecheck_test("078_b9_merge_row_inference_no_warn.eu");
}

#[test]
pub fn test_typecheck_079_b9_merge_row_field_type_mismatch() {
    run_typecheck_test("079_b9_merge_row_field_type_mismatch.eu");
}

#[test]
pub fn test_typecheck_080_b9_non_block_param_no_row_var() {
    run_typecheck_test("080_b9_non_block_param_no_row_var.eu");
}

#[test]
pub fn test_typecheck_081_b2_constraint_satisfied() {
    run_typecheck_test("081_b2_constraint_satisfied.eu");
}

#[test]
pub fn test_typecheck_082_b2_constraint_violated() {
    run_typecheck_test("082_b2_constraint_violated.eu");
}

#[test]
pub fn test_typecheck_083_b2_constraint_gradual_any() {
    run_typecheck_test("083_b2_constraint_gradual_any.eu");
}

#[test]
pub fn test_typecheck_084_random_correct_binding() {
    run_typecheck_test("084_random_correct_binding.eu");
}

#[test]
pub fn test_typecheck_085_random_wrong_binding() {
    run_typecheck_test("085_random_wrong_binding.eu");
}

#[test]
pub fn test_typecheck_086_user_monad_with_monad_meta() {
    run_typecheck_test("086_user_monad_with_monad_meta.eu");
}

#[test]
pub fn test_typecheck_087_for_wrong_element_type() {
    run_typecheck_test("087_for_wrong_element_type.eu");
}

#[test]
pub fn test_typecheck_088_deprecated_bare() {
    run_typecheck_test("088_deprecated_bare.eu");
}

#[test]
pub fn test_typecheck_089_deprecated_with_message() {
    run_typecheck_test("089_deprecated_with_message.eu");
}

#[test]
pub fn test_typecheck_090_deprecated_with_replacement() {
    run_typecheck_test("090_deprecated_with_replacement.eu");
}

#[test]
pub fn test_typecheck_091_deprecated_not_referenced() {
    run_typecheck_test("091_deprecated_not_referenced.eu");
}

#[test]
pub fn test_error_158() {
    run_error_test(&error_opts("158_self_assign_direct.eu"));
}

#[test]
pub fn test_error_159() {
    run_error_test(&error_opts("159_self_assign_function_pos.eu"));
}

#[test]
pub fn test_error_160() {
    run_error_test(&error_opts("160_unclosed_block.eu"));
}

#[test]
pub fn test_error_161() {
    run_error_test(&error_opts("161_unclosed_list.eu"));
}

#[test]
pub fn test_error_162() {
    run_error_test(&error_opts("162_unclosed_paren.eu"));
}

#[test]
pub fn test_error_163() {
    run_error_test(&error_opts("163_malformed_decl_head.eu"));
}

#[test]
/// W4p2: a bare colon (no head) at block level is wrapped in ERROR;
/// surrounding valid declarations still parse.
pub fn test_error_164() {
    run_error_test(&error_opts("164_error_recovery_bare_colon.eu"));
}

#[test]
/// W4p2: a malformed declaration inside a nested block is wrapped in ERROR;
/// the outer block and sibling declarations still parse.
pub fn test_error_165() {
    run_error_test(&error_opts("165_error_recovery_nested_block.eu"));
}

#[test]
/// W4p2: a malformed declaration (bare colon) alongside valid declarations
/// reports an error with a span pointing at the bare colon, not the start
/// of the file.
pub fn test_error_166() {
    run_error_test(&error_opts("error_166.eu"));
}

#[test]
/// eu-9tah.3: a git import block missing the mandatory `commit` field
/// produces a clear error message referencing the commit SHA requirement.
pub fn test_error_167() {
    run_error_test(&error_opts("error_167.eu"));
}

#[test]
/// io.shell without a cmd field should produce a proper IoFail error, not a panic.
pub fn test_error_168() {
    run_error_test(&io_error_opts("168_io_shell_missing_cmd.eu"));
}

#[test]
/// Importing a malformed JSONL stream should produce a clear error, not a panic.
pub fn test_error_169() {
    run_error_test(&error_opts("169_malformed_jsonl_stream.eu"));
}

#[test]
/// graph.topo-sort with non-numeric edge values used to produce a locationless
/// Panic from collect_num_list; after the fix it emits a NotValue error with
/// source location.
pub fn test_error_173() {
    run_error_test(&error_opts("error_173.eu"));
}

#[test]
/// str.join-on on a list of numbers used to emit TypeMismatch with
/// Smid::default(); after the fix StrListIterator carries the call-site Smid.
pub fn test_error_174() {
    run_error_test(&error_opts("error_174.eu"));
}

#[test]
/// sort-nums on a string used to produce a locationless type-mismatch error
/// from DataIterator; after the fix the error carries the call-site Smid.
pub fn test_error_175() {
    run_error_test(&error_opts("error_175.eu"));
}

#[test]
/// Assertion failure on an interpolated string used to show `<string>` instead
/// of the actual string value in the EXPECT FAILED message (eu-kw67).
/// After the fix, `render_debug_repr_forced` evaluates the inner thunk and
/// shows the real string.
pub fn test_error_176() {
    run_error_test(&error_opts("error_176.eu"));
}

#[test]
/// Assertion failure diagnostic used to show the prelude `//=` definition
/// as the primary source location (eu-4qy4).  After the fix, when no
/// user-file source location is available, the prelude label is suppressed.
pub fn test_error_177() {
    run_error_test(&error_opts("error_177.eu"));
}

#[test]
/// Syntactically invalid type-DSL in an s-string produces a compile error.
pub fn test_error_178() {
    run_error_test(&error_opts("error_178.eu"));
}

#[test]
/// Exporting a JSON u64 value exceeding i64::MAX to YAML should produce an
/// informative panic message including the value, not a bare "unrenderable number".
pub fn test_error_171() {
    run_error_test(&error_opts("171_yaml_large_uint.eu"));
}

#[test]
/// YAML mappings with complex (non-scalar) keys should produce a clear error,
/// not a panic. Covers the bad-key-type path in the YAML parser.
pub fn test_error_172() {
    run_error_test(&error_opts("172_yaml_complex_key.eu"));
}

#[test]
/// A failing numeric assertion must render actual/expected through the
/// engine-neutral ABI and raise `AssertionFailed`, not panic in the
/// HeapSyn-only navigator on the default (bytecode) engine (eu-mr5e).
pub fn test_error_179() {
    run_error_test(&error_opts("179_assert_fail_scalar.eu"));
}

#[test]
/// A key-not-found lookup on a large block walks the whole kv-pair spine to
/// build the "did you mean?" hint; the walk must stay GC-safe (eu-f3ss).
pub fn test_error_180() {
    run_error_test(&error_opts("180_lookup_fail_large_block.eu"));
}

#[test]
/// W4p2 integration: valid declarations structurally equivalent to those
/// that would survive error recovery evaluate correctly end-to-end.
/// Paired with test_error_164/165 to prove the full recovery story:
/// malformed declarations are isolated, valid ones evaluate to correct values.
pub fn test_166_error_recovery_eval() {
    run_test(&opts("166_error_recovery_eval.eu"));
}

#[test]
/// Namespace lambda hoisting pass (eu-398r): verifies that inlinable
/// namespace members (str.to-upper, str.to-lower, str.of) are correctly
/// hoisted to top-level OtherLet bindings and their call sites rewritten,
/// while the namespace block itself remains usable as a value.
pub fn test_167_namespace_hoisting() {
    run_test(&opts("167_namespace_hoisting.eu"));
}

#[test]
/// Blob namespace hoisting (eu-9tah.7): verifies that namespace member
/// calls (str.to-upper, str.trim, str.split-on, str.join-on) are correctly
/// resolved via the blob's hoisted globals, and that namespace blocks remain
/// accessible as values when the blob pipeline is active.
pub fn test_168_blob_namespace_hoisting() {
    run_test(&opts("168_blob_namespace_hoisting.eu"));
}

#[test]
/// LetRec SCC splitting: independent bindings become non-recursive Let
/// scopes, mutually recursive groups stay as LetRec, blocks are untouched.
pub fn test_169_letrec_scc_splitting() {
    run_test(&opts("169_letrec_scc_splitting.eu"));
}

#[test]
/// Strict eager evaluation: Seq forces strict thunks at definition time
pub fn test_170_strict_eager_eval() {
    run_test(&opts("170_strict_eager_eval.eu"));
}

#[test]
/// Strict prelude args: Seq forces strict args at global call sites
pub fn test_171_strict_prelude_args() {
    run_test(&opts("171_strict_prelude_args.eu"));
}

#[test]
/// Idiot-bracket expressions accepted as juxtaposed call targets: ⌈expr⌉[args]
/// and ⌈expr⌉{block} are parsed as calls, not catenation with a list literal.
pub fn test_172_idiot_bracket_juxtaposed_call() {
    run_test(&opts("172_idiot_bracket_juxtaposed_call.eu"));
}

#[test]
/// Verify CG1 direct-dispatch (DirectApp) correctness across pipeline, direct,
/// nested, and user-function call patterns.
pub fn test_173_direct_app_dispatch() {
    run_test(&opts("173_direct_app_dispatch.eu"));
}

#[test]
/// SV1 type-data semantics: to-data projection, from-data round-trip, passthru.
pub fn test_174_sv1_typedata() {
    run_test(&opts("174_sv1_typedata.eu"));
}

#[test]
/// SV2 to-spec / as-spec: convert type-data to match?-compatible patterns.
pub fn test_175_sv2_to_spec() {
    run_test(&opts("175_sv2_to_spec.eu"));
}

#[test]
pub fn test_176_sv_optional_fields() {
    run_test(&opts("176_sv_optional_fields.eu"));
}

#[test]
pub fn test_177_sv_optional_fields_to_data() {
    run_test(&opts("177_sv_optional_fields_to_data.eu"));
}

#[test]
pub fn test_178_sv_optional_fields_to_spec() {
    run_test(&opts("178_sv_optional_fields_to_spec.eu"));
}

#[test]
/// CG3: higher-order recursive folds with strict arg forcing at self-recursive
/// call sites produce correct results and complete in linear time.
pub fn test_179_cg3_strict_recurse() {
    run_test(&opts("179_cg3_strict_recurse.eu"));
}

#[test]
/// CG1 DirectApp for Ref::L local callees: user-defined letrec functions
/// called at exact arity should use the DirectApp fast path.
pub fn test_180_direct_app_local() {
    run_test(&opts("180_direct_app_local.eu"));
}

#[test]
pub fn test_181_cg3_bomb() {
    run_test(&opts("181_cg3_bomb.eu"));
}

#[test]
pub fn test_182_typedata_alias_resolution() {
    run_test(&opts("182_typedata_alias_resolution.eu"));
}

#[test]
pub fn test_183_widen_type_def_literals() {
    run_test(&opts("183_widen_type_def_literals.eu"));
}

#[test]
pub fn test_184_fused_primop_gc() {
    run_test(&opts("184_fused_primop_gc.eu"));
}

#[test]
pub fn test_185_bytecode_large_literal() {
    run_test(&opts("185_bytecode_large_literal.eu"));
}

#[test]
/// Regression for eu-2sa6.16: `lib/markup.eu` declared `tag`/`attrs`/
/// `content` with `=` instead of `:`, which Rowan lenient-parsed as
/// declaration metadata rather than raising a parse error, leaving the
/// accessors silently undefined.
pub fn test_186_markup_accessors() {
    run_test(&opts("186_markup_accessors.eu"));
}

#[test]
pub fn test_187_sv_prefix_list() {
    run_test(&opts("187_sv_prefix_list.eu"));
}

#[test]
pub fn test_188_v8n8_cluster_laziness() {
    run_test(&opts("188_v8n8_cluster_laziness.eu"));
}

#[test]
pub fn test_189_r9oy_union_as_spec() {
    run_test(&opts("189_r9oy_union_as_spec.eu"));
}

#[test]
pub fn test_typecheck_092_self_assign_arg_pos_ok() {
    run_typecheck_test("092_self_assign_arg_pos_ok.eu");
}

#[test]
pub fn test_typecheck_093_type_def_bare_symbol_shorthand() {
    run_typecheck_test("093_type_def_bare_symbol_shorthand.eu");
}

#[test]
pub fn test_typecheck_094_type_def_true_structured_metadata() {
    run_typecheck_test("094_type_def_true_structured_metadata.eu");
}

#[test]
pub fn test_typecheck_095_type_def_explicit_string_regression() {
    run_typecheck_test("095_type_def_explicit_string_regression.eu");
}

#[test]
pub fn test_typecheck_096_result_def_explicit_name() {
    run_typecheck_test("096_result_def_explicit_name.eu");
}

#[test]
pub fn test_typecheck_097_result_def_true_binding_name() {
    run_typecheck_test("097_result_def_true_binding_name.eu");
}

#[test]
pub fn test_typecheck_098_result_def_on_non_function_warns() {
    run_typecheck_test("098_result_def_on_non_function_warns.eu");
}

#[test]
pub fn test_typecheck_099_both_type_def_and_result_def() {
    run_typecheck_test("099_both_type_def_and_result_def.eu");
}

#[test]
pub fn test_typecheck_100_target_bare_symbol_regression() {
    run_typecheck_test("100_target_bare_symbol_regression.eu");
}

#[test]
pub fn test_typecheck_101_unrecognised_bare_symbol_target_shortcut() {
    run_typecheck_test("101_unrecognised_bare_symbol_target_shortcut.eu");
}

#[test]
pub fn test_typecheck_102_types_alias_with_docstring() {
    run_typecheck_test("102_types_alias_with_docstring.eu");
}

#[test]
pub fn test_typecheck_103_interpolation_string_type() {
    run_typecheck_test("103_interpolation_string_type.eu");
}

#[test]
pub fn test_typecheck_105_optional_field_no_warning() {
    run_typecheck_test("105_optional_field_no_warning.eu");
}

#[test]
pub fn test_typecheck_106_optional_field_with_value_no_warning() {
    run_typecheck_test("106_optional_field_with_value_no_warning.eu");
}

#[test]
pub fn test_typecheck_107_optional_field_missing_required_warns() {
    run_typecheck_test("107_optional_field_missing_required_warns.eu");
}

#[test]
pub fn test_typecheck_108_optional_field_wrong_type_warns() {
    run_typecheck_test("108_optional_field_wrong_type_warns.eu");
}

#[test]
pub fn test_typecheck_109_prefix_list_no_warning() {
    run_typecheck_test("109_prefix_list_no_warning.eu");
}

#[test]
pub fn test_typecheck_110_prefix_list_wrong_shape_warns() {
    run_typecheck_test("110_prefix_list_wrong_shape_warns.eu");
}

#[test]
pub fn test_typecheck_111_prefix_list_out_of_prefix_partial() {
    run_typecheck_test("111_prefix_list_out_of_prefix_partial.eu");
}

// ── eu doc tests ──────────────────────────────────────────────────────────────

/// `eu doc` on a documented fixture produces Markdown with the expected content.
#[test]
pub fn test_doc_001_markdown_basic() {
    run_doc_test(
        "tests/harness/doc/001_documented.eu",
        &[],
        &[
            "# 001_documented",
            "## Combinators",
            "`identity(x)`",
            "Identity function",
            "`k(x, y)`",
            "Constant function",
            "## Data",
            "`version`",
            "A version string.",
        ],
    );
}

/// `eu doc` extracts type annotations and renders them in a code block.
#[test]
pub fn test_doc_002_type_annotations() {
    run_doc_test(
        "tests/harness/doc/001_documented.eu",
        &[],
        &["type: a → a", "type: string"],
    );
}

/// `eu doc` suppresses `:internal` bindings from the output.
#[test]
pub fn test_doc_003_internal_suppressed() {
    let output = std::process::Command::new(eu_binary())
        .args(["doc", "tests/harness/doc/001_documented.eu"])
        .output()
        .expect("failed to run eu doc");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("_internal-helper"),
        "eu doc should suppress :internal binding, but stdout contains _internal-helper:\n{stdout}"
    );
}

/// `eu doc --format json` produces valid JSON with a `$schema` key.
#[test]
pub fn test_doc_004_json_format() {
    run_doc_test(
        "tests/harness/doc/001_documented.eu",
        &["--format", "json"],
        &["\"$schema\":", "\"title\":"],
    );
}

/// `eu doc --check` produces a coverage report.
#[test]
pub fn test_doc_005_coverage_check() {
    run_doc_test(
        "tests/harness/doc/001_documented.eu",
        &["--check"],
        &["Documentation coverage", "documented"],
    );
}

/// `eu doc` renders all items from a `see-also: [...]` list.
#[test]
pub fn test_doc_007_see_also() {
    run_doc_test(
        "tests/harness/doc/002_see_also.eu",
        &[],
        &["**See also:** `k`, `identity`"],
    );
}

/// `eu doc --prelude` runs without error and produces the prelude reference.
#[test]
pub fn test_doc_006_prelude() {
    let output = std::process::Command::new(eu_binary())
        .args(["doc", "--prelude"])
        .output()
        .expect("failed to run eu doc --prelude");
    let exit_code = output.status.code().unwrap_or(-1);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        exit_code, 0,
        "eu doc --prelude exited with {exit_code}\nstdout: {stdout}"
    );
    assert!(
        stdout.contains("# Prelude Reference"),
        "eu doc --prelude should produce '# Prelude Reference'\nstdout: {stdout}"
    );
}

// ── Phase 2: type checking default-on ────────────────────────────────────────

/// Type checker runs unconditionally — no flag needed to see warnings.
#[test]
pub fn test_type_warnings_run_unconditionally() {
    // 001_arg_mismatch.eu calls `double("hello")` where double: number -> number.
    // Without any flag, the type checker must run and emit a warning.
    let output = std::process::Command::new(eu_binary())
        .arg("tests/harness/typecheck/001_arg_mismatch.eu")
        .output()
        .expect("failed to run eu");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("type mismatch"),
        "eu should emit type warnings unconditionally, but stderr was:\n{stderr}"
    );
}

/// `--suppress-type-warnings` silences type warnings (checker still runs, no stderr output).
///
/// Uses `104_suppress_type_warnings_ok.eu` which has an annotation mismatch but a
/// runtime behaviour that succeeds (identity function called with a string), so the
/// exit code is 0 both with and without suppression.
#[test]
pub fn test_suppress_type_warnings_silences_output() {
    // Without suppression: warning appears in stderr.
    let without = std::process::Command::new(eu_binary())
        .arg("tests/harness/typecheck/104_suppress_type_warnings_ok.eu")
        .output()
        .expect("failed to run eu");
    let stderr_without = String::from_utf8_lossy(&without.stderr);
    assert!(
        stderr_without.contains("type mismatch"),
        "without --suppress-type-warnings, warning should appear:\n{stderr_without}"
    );

    // With suppression: no warning in stderr, exit still 0.
    let with = std::process::Command::new(eu_binary())
        .args([
            "--suppress-type-warnings",
            "tests/harness/typecheck/104_suppress_type_warnings_ok.eu",
        ])
        .output()
        .expect("failed to run eu --suppress-type-warnings");
    let exit_code = with.status.code().unwrap_or(-1);
    let stderr_with = String::from_utf8_lossy(&with.stderr);
    assert_eq!(
        exit_code, 0,
        "--suppress-type-warnings should not affect exit code:\n{stderr_with}"
    );
    assert!(
        !stderr_with.contains("type mismatch"),
        "--suppress-type-warnings should silence type warnings, but stderr was:\n{stderr_with}"
    );
}

/// `--type-check` is a silent no-op — behaviour is identical to omitting it.
#[test]
pub fn test_type_check_flag_is_noop() {
    let without = std::process::Command::new(eu_binary())
        .arg("tests/harness/typecheck/001_arg_mismatch.eu")
        .output()
        .expect("failed to run eu");
    let with = std::process::Command::new(eu_binary())
        .args([
            "--type-check",
            "tests/harness/typecheck/001_arg_mismatch.eu",
        ])
        .output()
        .expect("failed to run eu --type-check");
    assert_eq!(
        without.status.code(),
        with.status.code(),
        "--type-check flag should not change exit code"
    );
    // Both should emit the same type warning.
    let stderr_with = String::from_utf8_lossy(&with.stderr);
    assert!(
        stderr_with.contains("type mismatch"),
        "--type-check should not suppress warnings:\n{stderr_with}"
    );
}

/// `--strict` causes the run to abort with exit 1 when there are type warnings.
#[test]
pub fn test_strict_aborts_on_type_warnings() {
    let output = std::process::Command::new(eu_binary())
        .args(["--strict", "tests/harness/typecheck/001_arg_mismatch.eu"])
        .output()
        .expect("failed to run eu --strict");
    let exit_code = output.status.code().unwrap_or(-1);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        exit_code, 1,
        "eu --strict should exit 1 when there are type warnings:\n{stderr}"
    );
    assert!(
        stderr.contains("type mismatch"),
        "eu --strict should still print type warnings:\n{stderr}"
    );
}

/// The eval path (`eu <file>`, no subcommand) must produce the same type
/// warning as `eu check --strict` for a record-membership constraint that
/// needs the prelude's actual *definitions* (not just declared type
/// schemes) to check — `lookup(:naem, person)` against a closed record type
/// (eu-rb5n). This is the regression the blob-core merged check
/// (`run_type_checker_from_blob_core`) exists to fix: seeding the checker
/// with only the blob's prelude type *summary* cannot see that `lookup`'s
/// body indexes into the record, so it silently missed this warning.
#[test]
pub fn test_eval_path_warns_on_record_key_typo() {
    let output = std::process::Command::new(eu_binary())
        .arg("tests/harness/typecheck/065_lookup_key_typo_warns.eu")
        .output()
        .expect("failed to run eu");
    let exit_code = output.status.code().unwrap_or(-1);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        exit_code, 1,
        "eval path should exit 1 (execution error after the warning):\n{stderr}"
    );
    assert!(
        stderr.contains("unknown record key :naem"),
        "eval path should warn on the record key typo, matching `eu check`:\n{stderr}"
    );
}

/// The blob-core merged check (default config) and the source-prelude merged
/// check (`EU_SOURCE_PRELUDE=1`) must produce byte-identical `eu check
/// --strict` output for the same file (eu-rb5n condition 2/4: the blob path
/// is only a startup optimisation, never a different answer). Covers both
/// config branches for a representative spread: a warning case, a clean
/// case, and a case needing the deeper prefix-list inference.
#[test]
pub fn test_config_matrix_blob_vs_source_prelude_byte_equal() {
    let files = [
        "tests/harness/typecheck/065_lookup_key_typo_warns.eu",
        "tests/harness/typecheck/104_suppress_type_warnings_ok.eu",
        "tests/harness/typecheck/111_prefix_list_out_of_prefix_partial.eu",
    ];
    for file in files {
        let blob = std::process::Command::new(eu_binary())
            .args(["check", "--strict", file])
            .output()
            .expect("failed to run eu check (blob-core)");
        let source = std::process::Command::new(eu_binary())
            .env("EU_SOURCE_PRELUDE", "1")
            .args(["check", "--strict", file])
            .output()
            .expect("failed to run eu check (source prelude)");

        assert_eq!(
            blob.status.code(),
            source.status.code(),
            "exit code differs between blob-core and source-prelude checks for {file}"
        );
        assert_eq!(
            String::from_utf8_lossy(&blob.stderr),
            String::from_utf8_lossy(&source.stderr),
            "stderr differs between blob-core and source-prelude checks for {file}"
        );
    }
}
