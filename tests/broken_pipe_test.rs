//! `eu … | head` must not panic (eu-1tkk.7.25).
//!
//! Piping output into a consumer that stops reading is everyday CLI use, and
//! it used to abort the process with a Rust backtrace and exit 101 in every
//! format that writes a document. A closed pipe is not a failure — the
//! reader has what it wanted — so the run must end quietly: exit 0, nothing
//! on stderr, and above all no panic.
//!
//! The tests drive the real binary, because the bug was in the interaction
//! between the emitter, the driver's stream lifecycle and process exit; a
//! unit test over an emitter cannot reach that. The classification of a
//! broken pipe against a genuine write failure is unit-tested separately in
//! `src/export/mod.rs::write_failure_tests`.

use std::io::Read;
use std::process::{Command, Stdio};

/// Every format whose emitter writes a document to the output stream.
///
/// `html` is excluded only because it needs a hiccup document; its write
/// path is covered by the unit tests.
const FORMATS: [&str; 6] = ["yaml", "json", "toml", "edn", "text", "eu"];

/// A program producing far more output than the reader will consume, so the
/// write is guaranteed still to be in progress when the pipe closes.
///
/// Every format renders this to at least 200 KB, comfortably beyond a pipe
/// buffer, so the child cannot finish writing into the buffer and exit
/// before we close our end.
const BIG_DOCUMENT: &str = "main: range(1, 40000)\n";

/// How much of the output to consume before closing the pipe.
///
/// Deliberately a byte count and not a line: EDN renders any document as a
/// *single* line, and TOML renders this one as two, so a `read_line` would
/// swallow the whole document and the child would finish successfully —
/// leaving those formats untested. Verified by injection: with a line-based
/// read, restoring the pre-fix `.expect` in `toml.rs` or `edn.rs` left this
/// test passing (eu-1tkk.7.25).
const BYTES_TO_CONSUME: usize = 64;

fn write_fixture(dir: &std::path::Path) -> std::path::PathBuf {
    let path = dir.join("big.eu");
    std::fs::write(&path, BIG_DOCUMENT).expect("write fixture");
    path
}

/// Run `eu -x <format> big.eu`, consume a short prefix, then drop the read
/// end — the equivalent of `| head -c 64` — and report `(exit code, stderr)`.
fn run_with_early_reader_exit(format: &str) -> (Option<i32>, String) {
    let dir = tempfile::tempdir().expect("temp dir");
    let fixture = write_fixture(dir.path());

    let mut child = Command::new(env!("CARGO_BIN_EXE_eu"))
        .arg("run")
        .arg("-L")
        .arg(concat!(env!("CARGO_MANIFEST_DIR"), "/lib"))
        .arg("-x")
        .arg(format)
        .arg("--heap-limit-mib")
        .arg("4096")
        .arg(&fixture)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn eu");

    {
        let mut stdout = child.stdout.take().expect("stdout piped");
        let mut buf = [0u8; BYTES_TO_CONSUME];
        // A short prefix is enough; dropping the handle closes our end of
        // the pipe, so eu's next write gets EPIPE.
        let _ = stdout.read(&mut buf);
    }

    let output = child.wait_with_output().expect("wait for eu");
    (
        output.status.code(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

/// The regression proper: no panic, in any format.
///
/// Asserted separately from the exit code because a panic is the specific
/// failure this bead is about, and it is worth failing with that word in the
/// message rather than a bare exit-code mismatch.
#[test]
fn piping_into_a_short_reader_never_panics() {
    for format in FORMATS {
        let (code, stderr) = run_with_early_reader_exit(format);
        assert!(
            !stderr.contains("panicked"),
            "{format}: eu panicked on a closed pipe (exit {code:?}):\n{stderr}"
        );
        assert_ne!(
            code,
            Some(101),
            "{format}: eu exited 101 (panic) on a closed pipe:\n{stderr}"
        );
    }
}

/// A closed pipe is a normal end, so the run reports success and says
/// nothing.
///
/// Exit 0 rather than 141: eu catches the condition rather than dying of
/// SIGPIPE, and 141 would fail any pipeline running under `set -o pipefail`
/// for what is not an error.
#[test]
fn piping_into_a_short_reader_exits_quietly() {
    for format in FORMATS {
        let (code, stderr) = run_with_early_reader_exit(format);
        assert_eq!(
            code,
            Some(0),
            "{format}: expected exit 0, stderr:\n{stderr}"
        );
        assert!(
            stderr.is_empty(),
            "{format}: expected empty stderr on a closed pipe, got:\n{stderr}"
        );
    }
}

// ---------------------------------------------------------------------------
// The CLI's own stdout writes (eu-oaz72.39)
//
// The tests above cover rendered documents. `eu test`, the `eu dump` phases,
// `eu list-targets`, `eu doc`, `eu fmt` and `eu error` print to stdout
// themselves, and used `print!`/`println!`, which panic on EPIPE:
// `eu test FILE | head -1` aborted with a backtrace and exit 101. Each must
// now end as rendering does on a closed pipe: exit 0, nothing on stderr.
// ---------------------------------------------------------------------------

/// A tiny passing test file, so `eu test` prints a result line and the
/// report path.
const PASSING_TEST: &str = "` { target: :test }\nt: { RESULT: :PASS }\n";

/// Run `eu <args>` with stdout a pipe whose read end is closed before eu
/// writes a byte, so its first stdout write gets EPIPE. Returns
/// `(exit code, stderr)`.
///
/// The read end is dropped immediately after spawning: eu has not finished
/// starting up, let alone printed anything, by then.
fn run_with_reader_gone(args: &[&str], cwd: &std::path::Path) -> (Option<i32>, String) {
    let child = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(args)
        .current_dir(cwd)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn eu");

    let mut child = child;
    drop(child.stdout.take().expect("stdout piped"));

    let output = child.wait_with_output().expect("wait for eu");
    (
        output.status.code(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

#[test]
fn cli_stdout_writes_end_quietly_on_a_closed_pipe() {
    let dir = tempfile::tempdir().expect("temp dir");
    let test_file = dir.path().join("pipe_test.eu");
    std::fs::write(&test_file, PASSING_TEST).expect("write test fixture");
    let big = write_fixture(dir.path());
    let prelude = concat!(env!("CARGO_MANIFEST_DIR"), "/lib/prelude.eu");
    let test_path = test_file.to_str().unwrap();
    let big_path = big.to_str().unwrap();

    let cases: Vec<Vec<&str>> = vec![
        vec!["test", test_path],
        vec!["dump", "ast", big_path],
        vec!["dump", "cooked", big_path],
        vec!["dump", "demands", big_path],
        vec!["dump", "reflatten", big_path],
        vec!["dump", "stg", big_path],
        vec!["dump", "runtime", big_path],
        vec!["list-targets", big_path],
        vec!["doc", prelude],
        vec!["fmt", prelude],
        vec!["error", "EU-EVAL-TYPE"],
    ];

    for args in cases {
        let (code, stderr) = run_with_reader_gone(&args, dir.path());
        assert!(
            !stderr.contains("panicked"),
            "eu {args:?} panicked on a closed pipe (exit {code:?}):\n{stderr}"
        );
        assert_eq!(
            code,
            Some(0),
            "eu {args:?}: expected exit 0 on a closed pipe, stderr:\n{stderr}"
        );
        assert!(
            stderr.is_empty(),
            "eu {args:?}: expected empty stderr on a closed pipe, got:\n{stderr}"
        );
    }
}

/// A closed pipe must not hide a failing suite.
///
/// Silencing EPIPE is right for the progress lines, but `eu test` must
/// still run every test and exit with the suite's real verdict: under
/// `set -o pipefail`, `eu test failing.eu | head` has to fail. Reading the
/// closed pipe as success here would report a failing suite as passing.
#[test]
fn eu_test_with_a_closed_pipe_still_reports_failure() {
    let dir = tempfile::tempdir().expect("temp dir");
    let suite = dir.path().join("suite");
    std::fs::create_dir(&suite).expect("create suite dir");
    let failing = suite.join("a_failing.eu");
    std::fs::write(&failing, "` { target: :test }\nt: { RESULT: :FAIL }\n")
        .expect("write failing fixture");
    std::fs::write(suite.join("b_passing.eu"), PASSING_TEST).expect("write passing fixture");

    // Sanity: with a reader, the failing suite exits 1.
    let with_reader = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(["test", failing.to_str().unwrap()])
        .current_dir(dir.path())
        .output()
        .expect("run eu test");
    assert_eq!(
        with_reader.status.code(),
        Some(1),
        "failing suite should exit 1"
    );

    // A directory suite: the failing test runs first and a passing one
    // after, so the verdict depends on the run reaching the end and keeping
    // the earlier failure.
    let (code, stderr) = run_with_reader_gone(&["test", suite.to_str().unwrap()], dir.path());
    assert!(
        !stderr.contains("panicked"),
        "eu test panicked on a closed pipe (exit {code:?}):\n{stderr}"
    );
    assert_eq!(
        code,
        Some(1),
        "a failing suite must exit 1 even with its reader gone, stderr:\n{stderr}"
    );
}
