//! `eu test a.eu b.eu` must test every file it is given (eu-oaz72.45).
//!
//! It used to test only the last input and silently ignore the rest, so a
//! failing file named first went unreported and the run exited 0. The tests
//! drive the real binary, because the defect was in how the `test`
//! subcommand handles its command-line inputs, which no `.eu` program can
//! reach.
//!
//! The same tests cover an empty suite, which used to panic, and the
//! follow-ups from that change (eu-oaz72.126): a file reached twice runs
//! once, `x.eu` and `x.yaml` keep separate result files, `-o` places the
//! report, a suite across several directories reports in the current
//! directory, `-L` directories are searched past one that lacks the file,
//! and the error-test runner runs every input.

use eucalypt::driver::{options::EucalyptOptions, tester};
use eucalypt::syntax::input::Input;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

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

/// A directory with no test files used to panic in report generation
/// ("report requires at least one test plan"). It is now an error.
#[test]
fn no_test_files_is_an_error_not_a_panic() {
    let dir = tempfile::tempdir().expect("temp dir");
    let output = Command::new(env!("CARGO_BIN_EXE_eu"))
        .current_dir(dir.path())
        .arg("test")
        .arg("-L")
        .arg(concat!(env!("CARGO_MANIFEST_DIR"), "/lib"))
        .arg(dir.path())
        .output()
        .expect("run eu test");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(output.status.code(), Some(2), "stderr:\n{stderr}");
    assert!(!stderr.contains("panicked"), "panicked:\n{stderr}");
    assert!(
        stderr.contains("no test files found"),
        "no error reported:\n{stderr}"
    );
}

/// Run `eu test` with `args` (no `-o`) from `cwd`; `(exit code, stdout)`.
fn eu_test_args(cwd: &Path, args: &[&std::ffi::OsStr]) -> (Option<i32>, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_eu"))
        .current_dir(cwd)
        .arg("test")
        .arg("-L")
        .arg(concat!(env!("CARGO_MANIFEST_DIR"), "/lib"))
        .args(args)
        .output()
        .expect("run eu test");
    (
        output.status.code(),
        String::from_utf8_lossy(&output.stdout).to_string(),
    )
}

/// The single run directory under `dir/.result/`.
fn run_dir(dir: &Path) -> PathBuf {
    let runs: Vec<_> = std::fs::read_dir(dir.join(".result"))
        .expect("result directory")
        .map(|e| e.expect("entry").path())
        .collect();
    assert_eq!(runs.len(), 1, "expected one run in {}", dir.display());
    runs.into_iter().next().unwrap()
}

/// The number of verdicts reported for the test titled `title`.
fn verdicts(stdout: &str, title: &str) -> usize {
    stdout
        .lines()
        .filter(|l| l.starts_with(&format!("{title}...")))
        .count()
}

#[test]
fn a_file_reached_twice_runs_once() {
    let dir = tempfile::tempdir().expect("temp dir");
    let once = fixture(dir.path(), "once.eu", PASSING);

    let (code, stdout) = eu_test(dir.path(), &[&once, dir.path(), &once]);
    assert_eq!(code, Some(0), "stdout:\n{stdout}");
    assert_eq!(verdicts(&stdout, "once"), 1, "stdout:\n{stdout}");
}

#[test]
fn same_stem_files_keep_separate_results() {
    let dir = tempfile::tempdir().expect("temp dir");
    fixture(dir.path(), "x.eu", PASSING);
    fixture(dir.path(), "x.yaml", "RESULT: FAIL\n");

    let (code, stdout) = eu_test_args(dir.path(), &[dir.path().as_os_str()]);
    assert_eq!(code, Some(1), "stdout:\n{stdout}");
    let run = run_dir(dir.path());
    let results = std::fs::read_to_string(run.join("results.yaml")).expect("results.yaml");
    assert_eq!(
        results.matches("overall: PASS").count(),
        1,
        "x.eu's result lost:\n{results}"
    );
    assert_eq!(
        results.matches("overall: FAIL").count(),
        1,
        "x.yaml's result lost:\n{results}"
    );
    // Each is reported under a title of its own, not both as `x`.
    assert_eq!(verdicts(&stdout, "x.eu"), 1, "stdout:\n{stdout}");
    assert_eq!(verdicts(&stdout, "x.yaml"), 1, "stdout:\n{stdout}");
}

/// A symbolic link to a test file, met in a directory that also holds the
/// file, is the same test: it runs once (eu-oaz72.143 a).
#[cfg(unix)]
#[test]
fn a_file_reached_through_a_symlink_runs_once() {
    let dir = tempfile::tempdir().expect("temp dir");
    fixture(dir.path(), "real.eu", PASSING);
    std::os::unix::fs::symlink(dir.path().join("real.eu"), dir.path().join("link.eu"))
        .expect("symlink");

    let (code, stdout) = eu_test_args(dir.path(), &[dir.path().as_os_str()]);
    assert_eq!(code, Some(0), "stdout:\n{stdout}");
    assert_eq!(verdicts(&stdout, "real"), 1, "stdout:\n{stdout}");
    assert_eq!(verdicts(&stdout, "link"), 0, "stdout:\n{stdout}");
}

/// One file named twice in different formats is two tests, each with its
/// own title and result (eu-oaz72.143 b, g).
#[test]
fn a_file_read_in_two_formats_runs_twice() {
    let dir = tempfile::tempdir().expect("temp dir");
    // Valid both as YAML and as JSON; it fails only if read as either.
    fixture(dir.path(), "x.txt", "{\"RESULT\": \"FAIL\"}\n");

    let (code, stdout) = eu_test_args(dir.path(), &["yaml@x.txt".as_ref(), "json@x.txt".as_ref()]);
    assert_eq!(code, Some(1), "stdout:\n{stdout}");
    assert_eq!(verdicts(&stdout, "yaml@x.txt"), 1, "stdout:\n{stdout}");
    assert_eq!(verdicts(&stdout, "json@x.txt"), 1, "stdout:\n{stdout}");
    let results =
        std::fs::read_to_string(run_dir(dir.path()).join("results.yaml")).expect("results.yaml");
    assert_eq!(
        results.matches("overall: FAIL").count(),
        2,
        "a result was lost:\n{results}"
    );
}

/// A format prefix holds through validation as well as the run: a YAML
/// file with another extension is not re-read as eucalypt
/// (eu-oaz72.143 b).
#[test]
fn a_format_prefix_holds_through_validation() {
    let dir = tempfile::tempdir().expect("temp dir");
    fixture(dir.path(), "cfg.conf", "RESULT: PASS\n");

    let (code, stdout) = eu_test_args(dir.path(), &["yaml@cfg.conf".as_ref()]);
    assert_eq!(code, Some(0), "stdout:\n{stdout}");
    assert_eq!(verdicts(&stdout, "cfg"), 1, "stdout:\n{stdout}");
}

/// Same-name files in different directories get their paths as titles,
/// and a title the file sets is kept even when it equals the stem.
#[test]
fn same_name_files_in_two_directories_keep_titles_of_their_own() {
    let dir = tempfile::tempdir().expect("temp dir");
    for sub in ["one", "two"] {
        std::fs::create_dir(dir.path().join(sub)).expect("mkdir");
        fixture(&dir.path().join(sub), "x.eu", PASSING);
    }
    let titled = "{ title: \"t\" }\n` { target: :test-ok }\nok: { RESULT: :PASS }\n";
    std::fs::create_dir(dir.path().join("three")).expect("mkdir");
    fixture(&dir.path().join("three"), "t.eu", titled);
    std::fs::create_dir(dir.path().join("four")).expect("mkdir");
    fixture(&dir.path().join("four"), "t.eu", titled);

    let (code, stdout) = eu_test_args(
        dir.path(),
        &[
            "one".as_ref(),
            "two".as_ref(),
            "three".as_ref(),
            "four".as_ref(),
        ],
    );
    assert_eq!(code, Some(0), "stdout:\n{stdout}");
    let x_titles: Vec<&str> = stdout.lines().filter(|l| l.contains("x.eu...")).collect();
    assert_eq!(x_titles.len(), 2, "stdout:\n{stdout}");
    assert_ne!(x_titles[0], x_titles[1], "stdout:\n{stdout}");
    assert_eq!(
        verdicts(&stdout, "t"),
        2,
        "metadata title rewritten:\n{stdout}"
    );
}

/// A relative `-o` is reported as the absolute path of the report
/// (eu-oaz72.143 c).
#[test]
fn a_relative_output_path_is_reported_absolute() {
    let dir = tempfile::tempdir().expect("temp dir");
    let ok = fixture(dir.path(), "ok.eu", PASSING);

    let (code, stdout) = eu_test_args(
        dir.path(),
        &["-o".as_ref(), "rel.html".as_ref(), ok.as_os_str()],
    );
    assert_eq!(code, Some(0), "stdout:\n{stdout}");
    let report = dir.path().join("rel.html");
    assert!(report.exists(), "no report at {}", report.display());
    // The reported path may spell the directory differently (a Windows
    // short name, a symlink), so compare the files it and -o name.
    let reported = stdout
        .lines()
        .find_map(|l| l.strip_prefix("Report generated at "))
        .map(PathBuf::from)
        .unwrap_or_else(|| panic!("no report line:\n{stdout}"));
    assert!(reported.is_absolute(), "reported as given:\n{stdout}");
    assert_eq!(
        reported.canonicalize().expect("reported report exists"),
        report.canonicalize().expect("canonical report"),
        "stdout:\n{stdout}"
    );
}

#[test]
fn output_option_places_the_report() {
    let dir = tempfile::tempdir().expect("temp dir");
    let ok = fixture(dir.path(), "ok.eu", PASSING);
    let report = dir.path().join("my-report.html");

    let (code, stdout) = eu_test_args(
        dir.path(),
        &["-o".as_ref(), report.as_os_str(), ok.as_os_str()],
    );
    assert_eq!(code, Some(0), "stdout:\n{stdout}");
    let html = std::fs::read_to_string(&report).expect("report written at -o path");
    assert!(html.contains("<html"), "not a report:\n{html}");
    assert!(
        stdout.contains(&format!("Report generated at {}", report.display())),
        "stdout:\n{stdout}"
    );
}

#[test]
fn a_suite_across_directories_reports_in_the_current_directory() {
    let dir = tempfile::tempdir().expect("temp dir");
    let one = dir.path().join("one");
    let two = dir.path().join("two");
    std::fs::create_dir(&one).expect("mkdir");
    std::fs::create_dir(&two).expect("mkdir");
    fixture(&one, "a.eu", PASSING);
    fixture(&two, "b.eu", PASSING);

    let (code, stdout) = eu_test_args(dir.path(), &["one".as_ref(), "two".as_ref()]);
    assert_eq!(code, Some(0), "stdout:\n{stdout}");
    let report = run_dir(dir.path()).join("report.html");
    assert!(report.exists(), "no report in cwd:\n{stdout}");
    assert!(
        !run_dir(&one).join("report.html").exists(),
        "report under the first directory:\n{stdout}"
    );
}

#[test]
fn a_file_found_only_on_the_library_path_is_tested() {
    let dir = tempfile::tempdir().expect("temp dir");
    let lib = dir.path().join("lib");
    std::fs::create_dir(&lib).expect("mkdir");
    fixture(&lib, "found.eu", PASSING);

    let (code, stdout) = eu_test_args(
        dir.path(),
        &["-L".as_ref(), lib.as_os_str(), "found.eu".as_ref()],
    );
    assert_eq!(code, Some(0), "stdout:\n{stdout}");
    assert_eq!(verdicts(&stdout, "found"), 1, "stdout:\n{stdout}");
}

/// `error_test_with_binary` used to run only its last input, so a
/// failing error test named first was not reported.
#[test]
fn the_error_test_runner_runs_every_input() {
    let dir = tempfile::tempdir().expect("temp dir");
    // Expects an error that never comes: fails.
    let bad = fixture(dir.path(), "bad.eu", "a: 1\n");
    fixture(
        dir.path(),
        "bad.eu.expect",
        "exit: 1\nno-location: no error is raised\n",
    );
    // Expects success: passes.
    let ok = fixture(dir.path(), "ok.eu", "a: 1\n");
    fixture(
        dir.path(),
        "ok.eu.expect",
        "exit: 0\nno-location: not an error\n",
    );

    let input = |p: &Path| Input::from_str(&p.to_string_lossy()).expect("input");
    let opts = |files: &[&Path]| {
        EucalyptOptions::default()
            .with_explicit_inputs(files.iter().map(|p| input(p)).collect())
            .with_lib_path(vec![dir.path().to_path_buf()])
            .build()
    };
    let eu = Path::new(env!("CARGO_BIN_EXE_eu"));

    assert_eq!(
        tester::error_test_with_binary(eu, &opts(&[&ok])).expect("run"),
        0
    );
    assert_eq!(
        tester::error_test_with_binary(eu, &opts(&[&bad, &ok])).expect("run"),
        1,
        "a failing first input was not reported"
    );
    // An input that cannot be run fails, without ending the run
    // (eu-oaz72.143 f).
    let missing = dir.path().join("missing.eu");
    assert_eq!(
        tester::error_test_with_binary(eu, &opts(&[&missing, &ok]))
            .expect("a missing input is a failed test, not an error"),
        1
    );
}

/// A target with checks that never runs fails the file in the results
/// and the HTML report, not only on the console (eu-oaz72.69).
#[test]
fn an_unrun_checked_target_fails_the_report() {
    let dir = tempfile::tempdir().expect("temp dir");
    let file = fixture(
        dir.path(),
        "unrun.eu",
        "` { target: :test-runs }\nruns: { RESULT: :PASS }\n\n` { target: :semantics }\nsemantics: { RESULT: :PASS }\n",
    );

    let (code, stdout) = eu_test_args(dir.path(), &[file.as_os_str()]);
    assert_eq!(code, Some(1), "stdout:\n{stdout}");
    assert!(stdout.contains("unrun...FAIL"), "stdout:\n{stdout}");
    let run = run_dir(dir.path());
    let results = std::fs::read_to_string(run.join("results.yaml")).expect("results.yaml");
    assert!(
        results.contains("overall: FAIL"),
        "results record a pass:\n{results}"
    );
    let html = std::fs::read_to_string(run.join("report.html")).expect("report.html");
    assert!(
        html.contains("has checks but never runs"),
        "report does not say why:\n{html}"
    );
}
