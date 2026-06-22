//! Integration tests for `io.args`, `io.RANDOM_SEED`, `io.epoch-time`, and
//! `io.env` when using the pre-compiled prelude blob.
//!
//! The blob bakes `__args` and `__io` with empty/default values at compile
//! time.  These tests verify that the executor overrides them at runtime so
//! the actual argument list and environment are visible to eucalypt programs.

use std::path::Path;

/// Path to the `eu` binary built by cargo for this test run.
fn eu_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_eu"))
}

/// Run `eu -e <expr> [extra_args]` and return the trimmed stdout.
///
/// Panics if the process exits with a non-zero status.
fn eu_eval(expr: &str, extra_args: &[&str]) -> String {
    let output = std::process::Command::new(eu_binary())
        .arg("-e")
        .arg(expr)
        .args(extra_args)
        .output()
        .expect("failed to run eu binary");

    assert!(
        output.status.success(),
        "eu exited with status {:?}\nstdout: {}\nstderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

/// `io.args` should return the arguments passed after `--`.
#[test]
fn test_io_args_returns_runtime_args() {
    let out = eu_eval("io.args", &["--", "hello", "world"]);
    // YAML list output
    assert!(
        out.contains("hello"),
        "expected 'hello' in io.args output, got: {out}"
    );
    assert!(
        out.contains("world"),
        "expected 'world' in io.args output, got: {out}"
    );
}

/// `io.args` should return an empty list when no args are passed.
#[test]
fn test_io_args_empty_when_no_args() {
    let out = eu_eval("io.args", &[]);
    // YAML renders an empty list as `[]`, which may appear on its own line.
    assert!(
        out.contains("[]"),
        "expected empty list for io.args, got: {out}"
    );
}

/// `io.args` count should match the number of arguments provided.
#[test]
fn test_io_args_count() {
    let out = eu_eval("io.args count", &["--", "a", "b", "c"]);
    assert!(out.contains('3'), "expected 3 args, got: {out}");
}

/// `io.RANDOM_SEED` should reflect `--seed` when provided.
#[test]
fn test_io_random_seed_with_explicit_seed() {
    let out = eu_eval("io.RANDOM_SEED", &["--seed", "12345"]);
    assert!(
        out.contains("12345"),
        "expected seed 12345 in output, got: {out}"
    );
}

/// `io.RANDOM_SEED` with a different seed value.
#[test]
fn test_io_random_seed_different_value() {
    let out = eu_eval("io.RANDOM_SEED", &["--seed", "99"]);
    assert!(out.contains("99"), "expected seed 99 in output, got: {out}");
}

/// `io.env` should be a non-empty block (environment variables are always set).
#[test]
fn test_io_env_is_nonempty() {
    let out = eu_eval("io.env elements non-nil?", &[]);
    assert!(
        out.contains("true"),
        "expected io.env to have environment variables, got: {out}"
    );
}

/// `io.epoch-time` should be a positive integer.
#[test]
fn test_io_epoch_time_is_positive() {
    // The prelude exposes this as `io.epoch-time`.
    let out = eu_eval("io.epoch-time", &[]);
    // Strip the YAML document marker and parse.
    let stripped = out
        .lines()
        .find(|l| !l.trim_start().starts_with("---"))
        .unwrap_or("")
        .trim();
    let epoch: i64 = stripped
        .parse()
        .unwrap_or_else(|_| panic!("expected integer epoch time, got: {out}"));
    assert!(epoch > 0, "expected positive epoch time, got: {epoch}");
}

/// `__args` direct access should also reflect runtime args (regression test).
#[test]
fn test_direct_args_access() {
    let out = eu_eval("__args", &["--", "foo", "bar"]);
    assert!(
        out.contains("foo"),
        "expected 'foo' in __args output, got: {out}"
    );
    assert!(
        out.contains("bar"),
        "expected 'bar' in __args output, got: {out}"
    );
}

/// `io.args` first element should be accessible.
#[test]
fn test_io_args_first_element() {
    let out = eu_eval("io.args head", &["--", "first", "second"]);
    assert!(
        out.contains("first"),
        "expected first arg 'first', got: {out}"
    );
}

/// Verify two seeds produce different `io.RANDOM_SEED` values.
#[test]
fn test_io_random_seed_values_differ_per_seed() {
    let out42 = eu_eval("io.RANDOM_SEED", &["--seed", "42"]);
    let out99 = eu_eval("io.RANDOM_SEED", &["--seed", "99"]);
    assert_ne!(
        out42, out99,
        "different --seed flags must produce different io.RANDOM_SEED values"
    );
}
