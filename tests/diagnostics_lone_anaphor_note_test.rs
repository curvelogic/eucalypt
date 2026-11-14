//! The lone-anaphor note (eu-oaz72.132).
//!
//! An expression anaphor's scope ends at the parentheses or comma round
//! it, so an argument that is nothing but an anaphor, `_` in `nth(_)`, is
//! the identity function. When the call then fails because a function
//! arrived where a value was wanted, the error states that as fact.
//!
//! Each case runs `eu --error-format json` on a small program and checks
//! the diagnostic's notes. The note must appear exactly where cook made a
//! lone anaphor an argument of the labelled call, and nowhere else.

use std::path::Path;
use std::process::Command;

const PRELUDE: &str = "\
f(a, b): a + b
g(a): a + 0
xs: [1, 2, 3]
";

/// Run `x: <expr>` and return (exit code, stdout, the diagnostic's notes).
fn run(expr: &str) -> (i32, String, Vec<String>) {
    let file = std::env::temp_dir().join(format!(
        "eu-lone-anaphor-note-{}-{:x}.eu",
        std::process::id(),
        fxhash(expr)
    ));
    std::fs::write(&file, format!("{PRELUDE}x: {expr}\n")).unwrap();
    let out = Command::new(Path::new(env!("CARGO_BIN_EXE_eu")))
        // The whole file is rendered, not `-e x`: an error under `-e`
        // currently has no user frame to label.
        .args(["--heap-limit-mib", "1024", "--error-format", "json"])
        .arg(&file)
        .output()
        .expect("failed to run eu");
    let _ = std::fs::remove_file(&file);
    let code = out.status.code().unwrap_or(-1);
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let stderr = String::from_utf8_lossy(&out.stderr);
    let notes = stderr
        .lines()
        .filter_map(|l| serde_json::from_str::<serde_json::Value>(l).ok())
        .filter_map(|v| v.get("notes").and_then(|n| n.as_array()).cloned())
        .flatten()
        .filter_map(|n| n.as_str().map(str::to_string))
        .collect();
    (code, stdout, notes)
}

fn fxhash(s: &str) -> u64 {
    s.bytes().fold(0xcbf29ce484222325u64, |h, b| {
        (h ^ b as u64).wrapping_mul(0x100000001b3)
    })
}

/// `expr` fails and its notes include exactly `note`.
fn assert_note(expr: &str, note: &str) {
    let (code, _, notes) = run(expr);
    assert_ne!(code, 0, "`{expr}` should fail");
    assert!(
        notes.iter().any(|n| n == note),
        "`{expr}`: expected the note\n  {note}\ngot notes: {notes:#?}"
    );
}

/// `expr` fails and no note speaks of the identity function.
fn assert_no_note(expr: &str) {
    let (code, _, notes) = run(expr);
    assert_ne!(code, 0, "`{expr}` should fail");
    assert!(
        !notes.iter().any(|n| n.contains("identity function")),
        "`{expr}`: expected no lone-anaphor note, got notes: {notes:#?}"
    );
}

fn only_argument(call: &str, anaphor: &str) -> String {
    format!(
        "note: in '{call}', '{anaphor}' is the whole argument, so the argument is the identity \
         function: an expression anaphor's scope ends at the parentheses round it"
    )
}

/// A single-argument call whose argument is a lone anaphor.
#[test]
fn only_argument_is_lone_anaphor() {
    for (expr, call, anaphor) in [
        ("xs nth(_)", "nth(_)", "_"),
        ("xs nth(_0)", "nth(_0)", "_0"),
        // parentheses round the anaphor are its scope too
        ("xs nth((_))", "nth((_))", "_"),
        // inside a block and inside another call's argument
        ("{ a: xs nth(_) }", "nth(_)", "_"),
        ("xs map(inc(_))", "inc(_)", "_"),
        // a later pipeline step that is not the failing one
        ("[1, 2] map(inc(_)) map(_ * 2)", "inc(_)", "_"),
    ] {
        assert_note(expr, &only_argument(call, anaphor));
    }
}

/// Calls of several arguments name the argument.
#[test]
fn one_of_several_arguments() {
    assert_note(
        "f(_, 1)",
        "note: in 'f(_, 1)', '_' is the whole of argument 1, so that argument is the identity \
         function: an expression anaphor's scope ends at the commas and parentheses round it",
    );
    assert_note(
        "f(1, _)",
        "note: in 'f(1, _)', '_' is the whole of argument 2, so that argument is the identity \
         function: an expression anaphor's scope ends at the commas and parentheses round it",
    );
    assert_note(
        "f(_, _) + 1",
        "note: in 'f(_, _)', '_' and '_' are the whole of arguments 1 and 2, so each of those \
         arguments is the identity function: an expression anaphor's scope ends at the commas \
         and parentheses round it",
    );
}

/// The note is shown only for a lone anaphor and a function received.
#[test]
fn not_shown_without_a_lone_anaphor() {
    for expr in [
        // sections: the anaphor shares its scope with an operator
        "xs nth(_ + 1)",
        "f(_ + 1, 1)",
        "xs map(_ + 1) nth(_ * 2)",
        // subsumed: the inner `_` joins the enclosing scope
        "xs map(_ + inc(_)) sum",
        // `_1` alone pads binder 0: a two-argument function, not the
        // identity
        "xs nth(_1)",
        "xs nth(_2)",
        // a function argument that is not an anaphor
        "xs nth(inc)",
        "g(inc)",
        // a lone anaphor in a call that fails for another reason: `0` is
        // not a list, nothing here received a function
        "nth(_, 0)",
        // a lone anaphor elsewhere in the program than the failing call
        "{ a: [3] map(_) }.a nth(inc)",
        // ordinary errors with no anaphor
        "\"a\" + 1",
        "true(1)",
    ] {
        assert_no_note(expr);
    }
}

/// A lone anaphor where the identity function is wanted is not an error.
#[test]
fn legitimate_identity() {
    let (code, stdout, notes) = run("[3, 1, 2] map(_) sum");
    assert_eq!(code, 0, "notes: {notes:#?}");
    assert!(stdout.contains('6'), "stdout: {stdout}");
}
