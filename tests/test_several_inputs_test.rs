//! `eu test a.eu b.eu` must test every file it is given (eu-oaz72.45).
//!
//! It used to test only the last input and silently ignore the rest, so a
//! failing file named first went unreported and the run exited 0. The tests
//! drive the real binary, because the defect was in how the `test`
//! subcommand handles its command-line inputs, which no `.eu` program can
//! reach.

use std::path::{Path, PathBuf};
use std::process::Command;

const PASSING: &str = "` { target: :test-ok }\nok: { RESULT: :PASS }\n";
const FAILING: &str = "` { target: :test-bad }\nbad: { RESULT: :FAIL }\n";

fn fixture(dir: &Path, name: &str, body: &str) -> PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, body).expect("write fixture");
    path
}

/// Run `eu test` over `files` and return `(exit code, stdout)`.
fn eu_test(dir: &Path, files: &[&Path]) -> (Option<i32>, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_eu"))
        .current_dir(dir)
        .arg("test")
        .arg("-L")
        .arg(concat!(env!("CARGO_MANIFEST_DIR"), "/lib"))
        .arg("-o")
        .arg(dir.join("report"))
        .args(files)
        .output()
        .expect("run eu test");
    (
        output.status.code(),
        String::from_utf8_lossy(&output.stdout).to_string(),
    )
}

/// Whether the run reported a verdict for the test titled `title`.
fn ran(stdout: &str, title: &str) -> bool {
    stdout
        .lines()
        .any(|l| l.starts_with(&format!("{title}...")))
}

#[test]
fn a_failing_file_named_first_fails_the_run() {
    let dir = tempfile::tempdir().expect("temp dir");
    let bad = fixture(dir.path(), "bad.eu", FAILING);
    let ok = fixture(dir.path(), "ok.eu", PASSING);

    let (code, stdout) = eu_test(dir.path(), &[&bad, &ok]);
    assert_eq!(code, Some(1), "stdout:\n{stdout}");
    assert!(ran(&stdout, "bad"), "bad.eu not run:\n{stdout}");
    assert!(ran(&stdout, "ok"), "ok.eu not run:\n{stdout}");
}

#[test]
fn a_failing_file_named_last_fails_the_run() {
    let dir = tempfile::tempdir().expect("temp dir");
    let ok = fixture(dir.path(), "ok.eu", PASSING);
    let bad = fixture(dir.path(), "bad.eu", FAILING);

    let (code, stdout) = eu_test(dir.path(), &[&ok, &bad]);
    assert_eq!(code, Some(1), "stdout:\n{stdout}");
    assert!(ran(&stdout, "ok"), "ok.eu not run:\n{stdout}");
    assert!(ran(&stdout, "bad"), "bad.eu not run:\n{stdout}");
}

#[test]
fn every_file_passing_passes_the_run() {
    let dir = tempfile::tempdir().expect("temp dir");
    let one = fixture(dir.path(), "one.eu", PASSING);
    let two = fixture(dir.path(), "two.eu", PASSING);

    let (code, stdout) = eu_test(dir.path(), &[&one, &two]);
    assert_eq!(code, Some(0), "stdout:\n{stdout}");
    assert!(ran(&stdout, "one"), "one.eu not run:\n{stdout}");
    assert!(ran(&stdout, "two"), "two.eu not run:\n{stdout}");
}
