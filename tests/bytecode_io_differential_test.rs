//! Differential tests for the bytecode IO driver (eu-lka7).
//!
//! Runs IO programs through the full driver twice — once on the legacy
//! HeapSyn engine (`EU_HEAPSYN=1`) and once on the default bytecode engine —
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
/// `bytecode` selects the default bytecode engine; otherwise the legacy
/// HeapSyn engine is selected via `EU_HEAPSYN=1`.
fn run(expr: &str, bytecode: bool) -> (String, Option<i32>) {
    let mut cmd = Command::new(eu_binary());
    cmd.arg("-I")
        .arg("--heap-limit-mib")
        .arg("2048")
        .arg("-e")
        .arg(expr);
    // Select engines explicitly and defensively: clear the other engine's
    // selector so an inherited env var cannot bleed across the two runs.
    cmd.env_remove("EU_BYTECODE");
    if bytecode {
        cmd.env_remove("EU_HEAPSYN");
    } else {
        cmd.env("EU_HEAPSYN", "1");
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

#[test]
fn io_tag_field_mismatch_dispatches_on_tag_agrees() {
    // eu-xqab: a spec whose FIELD set disagrees with its meta TAG. The block is
    // tagged `:io-shell` but also carries an `args` field. The HeapSyn driver
    // dispatches on the meta tag (→ shell, so the pipe is interpreted by the
    // shell); the bytecode driver historically inferred the action from the
    // field set (`args` ⇒ exec), running the whole command string as a single
    // binary name — a divergent result. Both engines must now dispatch on the
    // tag and agree.
    assert_engines_agree(
        "__IO_ACTION({:io-shell cmd: \"echo hi | tr a-z A-Z\", args: []}) io.map(_.stdout)",
    );
}

#[test]
fn io_exec_tag_with_shellish_cmd_agrees() {
    // The mirror case: an `:io-exec`-tagged spec must run as exec on both
    // engines (direct binary, no shell interpretation of the argument).
    assert_engines_agree(
        "__IO_ACTION({:io-exec cmd: \"echo\", args: [\"a\", \"b\"]}) io.map(_.stdout)",
    );
}
