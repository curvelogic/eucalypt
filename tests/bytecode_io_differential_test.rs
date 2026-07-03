//! Differential tests for the bytecode IO driver (eu-lka7).
//!
//! Runs IO programs through the full driver twice — once on the default
//! HeapSyn engine and once on the parallel bytecode engine (`EU_BYTECODE=1`) —
//! and asserts the rendered output agrees. This exercises the ported io-run /
//! world-injection loop end to end (io.return, io.shell, io.exec, io.bind
//! sequencing, and the parameterised shell-with spec block).

use std::path::Path;
use std::process::Command;

/// Path to the `eu` binary built by cargo for this test run.
fn eu_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_eu"))
}

/// Run `eu -I -e <expr>` on the given engine, returning `(stdout, exit_code)`.
/// `bytecode` selects the parallel engine via `EU_BYTECODE=1`.
fn run(expr: &str, bytecode: bool) -> (String, Option<i32>) {
    let mut cmd = Command::new(eu_binary());
    cmd.arg("-I")
        .arg("--heap-limit-mib")
        .arg("2048")
        .arg("-e")
        .arg(expr);
    if bytecode {
        cmd.env("EU_BYTECODE", "1");
    } else {
        cmd.env_remove("EU_BYTECODE");
    }
    let output = cmd.output().expect("failed to run eu binary");
    (
        String::from_utf8_lossy(&output.stdout).into_owned(),
        output.status.code(),
    )
}

/// Assert the two engines render byte-identical stdout and share an exit code.
fn assert_engines_agree(expr: &str) {
    let (heap_out, heap_code) = run(expr, false);
    let (bc_out, bc_code) = run(expr, true);
    assert_eq!(
        heap_out, bc_out,
        "engines disagree on stdout for {expr:?}\n HeapSyn: {heap_out:?}\n bytecode: {bc_out:?}"
    );
    assert_eq!(
        heap_code, bc_code,
        "engines disagree on exit code for {expr:?}"
    );
}

#[test]
fn io_return_agrees() {
    assert_engines_agree("io.return({ result: :ok, n: 42 })");
}

#[test]
fn io_shell_agrees() {
    assert_engines_agree("io.shell(\"echo hello\")");
}

#[test]
fn io_exec_agrees() {
    assert_engines_agree("io.exec([\"echo\", \"a\", \"b\"])");
}

#[test]
fn io_bind_chain_agrees() {
    // io.bind sequences two shell actions, threading the first result into the
    // continuation that decides the final document.
    assert_engines_agree(
        "io.bind(io.shell(\"echo hello\"), \
         check) where check(r): io.return({ matched: r.stdout str.matches?(\"hello.*\") })",
    );
}

#[test]
fn io_shell_with_stdin_agrees() {
    // Parameterised (App-thunk) spec block with an options block (stdin).
    assert_engines_agree("io.shell-with({ stdin: \"piped\\n\" }, \"cat\")");
}
