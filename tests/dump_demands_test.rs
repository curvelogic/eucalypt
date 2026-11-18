//! `eu dump demands` shows the demands the executor compiles, in both
//! prelude modes (eu-oaz72.105).
//!
//! In blob mode the executor's demand analysis takes the signatures derived
//! for each prelude global when the blob was built (eu-oaz72.92). The dump
//! ran the analysis without them, so a user function whose strictness comes
//! only from a prelude call (`f(x): inc(x)`) was dumped lazy in blob mode
//! and strict in source mode, while blob mode compiled it strict.
//!
//! A dump is a CLI path no `.eu` program can reach, so this drives the
//! binary rather than adding a harness test. Without a blob both runs use
//! the source prelude; `scripts/blob-mode-tests.sh` runs this target with
//! one, where the first run is blob mode.

use std::process::Command;

/// A user function strict in its argument only through a prelude call.
const PROGRAM: &str = "f(x): inc(x)\n\nout: f(4)\n";

/// The line `eu dump demands` prints for `f`: its demand, then its
/// signature (eu-oaz72.66).
const EXPECTED: &str = "f: (Strict, Multi) [(Strict,AtMostOnce)]";

/// Run `eu dump demands` on `PROGRAM`, with the source prelude forced when
/// `source_prelude` is set, and return the line for `f` in its demand
/// annotations.
fn dumped_signature_of_f(source_prelude: bool) -> Option<String> {
    let dir = tempfile::tempdir().expect("temp dir");
    let path = dir.path().join("prog.eu");
    std::fs::write(&path, PROGRAM).expect("write fixture");

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_eu"));
    cmd.arg("dump").arg("demands").arg(&path);
    if source_prelude {
        cmd.env("EU_SOURCE_PRELUDE", "1");
    } else {
        cmd.env_remove("EU_SOURCE_PRELUDE");
    }
    let output = cmd.output().expect("run eu dump demands");
    assert!(
        output.status.success(),
        "eu dump demands failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    stdout
        .split("-- demand annotations --")
        .nth(1)?
        .lines()
        // The program's own top-level `f`: nested bindings, such as a local
        // `f` in a source-mode prelude function, are indented.
        .find(|line| line.starts_with("f:"))
        .map(str::to_string)
}

/// Blob mode (when the binary embeds a blob) and source mode both dump the
/// signature the executor compiles.
#[test]
fn dump_demands_agrees_across_prelude_modes() {
    let default_mode = dumped_signature_of_f(false);
    let source_mode = dumped_signature_of_f(true);
    assert_eq!(default_mode.as_deref(), Some(EXPECTED));
    assert_eq!(source_mode.as_deref(), Some(EXPECTED));
}
