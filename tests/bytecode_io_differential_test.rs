//! Regression tests for the bytecode IO driver (eu-lka7).
//!
//! Runs IO programs through the full driver and asserts on the rendered
//! output, exercising the io-run / world-injection loop end to end
//! (io.return, io.shell, io.exec, io.bind sequencing, and the parameterised
//! shell-with spec block).
//!
//! These used to run each program twice — once on the legacy HeapSyn engine
//! (`EU_HEAPSYN=1`) and once on the default bytecode engine — and assert the
//! two agreed. HeapSyn was deleted by the Phase 4 collapse (eu-oufc);
//! bytecode is the sole remaining engine, so a same-engine comparison would
//! be vacuous. Converted to direct assertions on the bytecode driver's
//! output instead, which is a *stronger* check (it pins the actual expected
//! content, not just "whatever the two engines happen to agree on") and
//! keeps the regression coverage the differential tests protected —
//! including eu-xqab's tag-vs-field dispatch bug below.
//!
//! One fixture (`io_bind_chain_agrees`) used a `where` clause that is not
//! valid eucalypt syntax (there is no `where` construct — see
//! `docs/appendices/syntax-gotchas.md`); both engines silently agreed on the
//! same parse error, so the test was vacuous even before HeapSyn's deletion.
//! Rewritten below using the block-scoped function pattern from
//! `tests/harness/105_io_chain.eu`.

use std::path::Path;
use std::process::Command;

/// Path to the `eu` binary built by cargo for this test run.
fn eu_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_eu"))
}

/// Run `eu -I -e <expr>`, returning `(stdout, exit_code)`.
fn run(expr: &str) -> (String, Option<i32>) {
    let output = Command::new(eu_binary())
        .arg("-I")
        .arg("--heap-limit-mib")
        .arg("2048")
        .arg("-e")
        .arg(expr)
        .output()
        .expect("failed to run eu binary");
    (
        String::from_utf8_lossy(&output.stdout).into_owned(),
        output.status.code(),
    )
}

/// Run `expr` and assert its stdout contains every one of `expected`, and
/// that it exits successfully.
fn assert_output_contains(expr: &str, expected: &[&str]) {
    let (out, code) = run(expr);
    assert_eq!(
        code,
        Some(0),
        "eu did not exit successfully for {expr:?}\n{out}"
    );
    for e in expected {
        assert!(
            out.contains(e),
            "expected {e:?} in output for {expr:?}, got:\n{out}"
        );
    }
}

#[test]
fn io_return_produces_the_returned_block() {
    assert_output_contains(
        "io.return({ result: :ok, n: 42 })",
        &["result: ok", "n: 42"],
    );
}

#[test]
fn io_shell_runs_the_command_and_captures_stdout() {
    assert_output_contains("io.shell(\"echo hello\")", &["hello", "exit-code: 0"]);
}

#[test]
fn io_exec_runs_the_binary_directly() {
    assert_output_contains(
        "io.exec([\"echo\", \"a\", \"b\"])",
        &["a b", "exit-code: 0"],
    );
}

#[test]
fn io_bind_chain_threads_the_result_into_the_continuation() {
    // io.bind sequences two shell actions, threading the first result into
    // the continuation that decides the final document. `check` is a
    // block-scoped function (the pattern used by
    // `tests/harness/105_io_chain.eu`) rather than a `where` clause, which
    // is not valid eucalypt syntax.
    assert_output_contains(
        "{check(r): io.return({ matched: r.stdout str.matches?(\"hello.*\") })}\
         .(io.bind(io.shell(\"echo hello\"), check))",
        &["matched: true"],
    );
}

#[test]
fn io_shell_with_stdin_pipes_the_option_block_field() {
    // Parameterised (App-thunk) spec block with an options block (stdin).
    //
    // The command must actually consume stdin, and the two platforms'
    // `io.shell` shells (`sh -c` on Unix, `pwsh -NoProfile -Command` on
    // Windows — see `execute_shell` in `src/driver/io_common.rs`) have no
    // single command in common that does that: on Windows, `cat` is a
    // PowerShell alias for `Get-Content`, which demands a `-Path` and
    // refuses piped stdin outright ("missing mandatory parameter: Path"),
    // so it cannot stand in for Unix's `cat`. Rather than gate the
    // assertion away on Windows (which would silently resurrect the exact
    // vacuous-differential defect this PR eliminates — the old bc-vs-hs
    // form "passed" there too, because both engines drove the identical
    // failing `pwsh` invocation and "agreed"), pick a platform-appropriate
    // stdin-consuming command so the property under test — the `stdin`
    // option-block field is genuinely piped into the shelled-out process —
    // is exercised for real on both platforms.
    let cmd = if cfg!(windows) {
        // Reads the raw redirected stdin stream directly (no dependence on
        // PowerShell's `$input` pipeline-object semantics).
        "[Console]::In.ReadToEnd()"
    } else {
        "cat"
    };
    assert_output_contains(
        &format!("io.shell-with({{ stdin: \"piped\\n\" }}, \"{cmd}\")"),
        &["piped", "exit-code: 0"],
    );
}

#[test]
fn io_tag_field_mismatch_dispatches_on_the_tag() {
    // eu-xqab: a spec whose FIELD set disagrees with its meta TAG. The block
    // is tagged `:io-shell` but also carries an `args` field. The driver must
    // dispatch on the meta tag (→ shell, so the pipe is interpreted by the
    // shell and the case-conversion actually runs), not infer the action
    // from the field set (`args` ⇒ exec, which would run the whole command
    // string as a single, nonexistent binary name and fail).
    assert_output_contains(
        "__IO_ACTION({:io-shell cmd: \"echo hi | tr a-z A-Z\", args: []}) io.map(_.stdout)",
        &["HI"],
    );
}

#[test]
fn io_exec_tag_with_shellish_cmd_runs_as_exec() {
    // The mirror case: an `:io-exec`-tagged spec must run as exec (direct
    // binary, no shell interpretation of the argument) even though `cmd`
    // looks shell-ish.
    assert_output_contains(
        "__IO_ACTION({:io-exec cmd: \"echo\", args: [\"a\", \"b\"]}) io.map(_.stdout)",
        &["a b"],
    );
}
