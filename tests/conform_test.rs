//! Conformance tests: run each `.eu` file in `tests/conform/` with
//! `eu -t conform` and compare stdout byte-for-byte against the
//! corresponding `.golden` sidecar.
//!
//! Set `BLESS=1` to regenerate all golden files from current output.
//! Missing golden files are a test failure (not a skip).

use std::path::{Path, PathBuf};

/// Path to the `eu` binary built for this test run.
fn eu_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_eu"))
}

/// Run a single conformance file and return `Ok(actual_output)` or
/// `Err(exit_status_message)`.
fn run_eu_conform(eu_file: &Path) -> Result<String, String> {
    let output = std::process::Command::new(eu_binary())
        .args([eu_file.to_str().expect("non-UTF8 path"), "-t", "conform"])
        .output()
        .unwrap_or_else(|e| panic!("failed to spawn eu for {}: {e}", eu_file.display()));

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).into_owned())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(format!("eu exited with status {}: {stderr}", output.status))
    }
}

/// Run one conformance assertion.  Returns `Ok(())` on pass, or
/// `Err(description)` on failure.
fn assert_conform(eu_file: &Path) -> Result<(), String> {
    let golden_path = PathBuf::from(format!("{}.golden", eu_file.display()));
    let bless = std::env::var("BLESS").as_deref() == Ok("1");

    let actual = run_eu_conform(eu_file)
        .map_err(|e| format!("{}: eu invocation failed — {e}", eu_file.display()))?;

    if bless {
        std::fs::write(&golden_path, &actual)
            .unwrap_or_else(|e| panic!("BLESS: failed to write {}: {e}", golden_path.display()));
        return Ok(());
    }

    if !golden_path.exists() {
        return Err(format!(
            "{}: missing golden file '{}' — run BLESS=1 cargo test --test conform_test to generate it",
            eu_file.display(),
            golden_path.display()
        ));
    }

    let golden = std::fs::read_to_string(&golden_path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", golden_path.display()));

    if actual == golden {
        Ok(())
    } else {
        let diff = similar::TextDiff::from_lines(&golden, &actual);
        let report = diff
            .unified_diff()
            .header("expected (golden)", "actual")
            .to_string();
        Err(format!("{}: output mismatch\n{report}", eu_file.display()))
    }
}

#[test]
fn test_all_conformance() {
    let conform_dir = Path::new("tests/conform");

    let mut files: Vec<PathBuf> = std::fs::read_dir(conform_dir)
        .expect("tests/conform directory not found")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|p| p.extension().is_some_and(|ext| ext == "eu"))
        .collect();

    assert!(
        !files.is_empty(),
        "no .eu files found in tests/conform — did you forget to create them?"
    );

    files.sort();

    let mut failures: Vec<String> = Vec::new();

    for file in &files {
        match assert_conform(file) {
            Ok(()) => {}
            Err(msg) => failures.push(msg),
        }
    }

    if !failures.is_empty() {
        let count = failures.len();
        let details = failures.join("\n\n");
        panic!("{count} conformance failure(s):\n\n{details}");
    }
}
