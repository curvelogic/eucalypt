//! The pipeline-precedence note (eu-oaz72.29).
//!
//! An infix operator binds tighter than catenation, so `c nth(i) = 2` is
//! read as `c (nth(i) = 2)`: the whole operator expression becomes the
//! pipeline step. When that parse makes the program fail — the operator's
//! result is called (a1), or the operator receives the step's partial
//! application as an operand (a2) — the error states the parse as fact.
//!
//! Each case runs `eu --error-format json` on a small program and checks
//! the diagnostic's notes. The note must appear exactly where cook made an
//! unparenthesised operator expression the function operand of a
//! catenation, and nowhere else.

use std::path::Path;
use std::process::Command;

const PRELUDE: &str = "\
(l <+> r): l + r
f(a): a + 0
xs: [1, 2, 3]
xss: [[1], [2]]
";

/// Run `x: <expr>` and return (exit code, stdout, the diagnostic's notes).
fn run(expr: &str) -> (i32, String, Vec<String>) {
    let file = std::env::temp_dir().join(format!(
        "eu-catenation-note-{}-{:x}.eu",
        std::process::id(),
        fxhash(expr)
    ));
    std::fs::write(&file, format!("{PRELUDE}x: {expr}\n")).unwrap();
    let out = Command::new(Path::new(env!("CARGO_BIN_EXE_eu")))
        .args([
            "--heap-limit-mib",
            "1024",
            "--error-format",
            "json",
            "-e",
            "x",
        ])
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

/// `expr` fails and no note states how a pipeline was read.
fn assert_no_note(expr: &str) {
    let (code, _, notes) = run(expr);
    assert_ne!(code, 0, "`{expr}` should fail");
    assert!(
        !notes.iter().any(|n| n.contains("was read as")),
        "`{expr}`: expected no parse note, got notes: {notes:#?}"
    );
}

fn parse_note(whole: &str, read: &str, fix: &str) -> String {
    format!("note: '{whole}' was read as '{read}'; parenthesise the pipeline: '{fix}'")
}

/// (a1) the operator's result is called, across operator bands.
#[test]
fn result_of_operator_called() {
    for (expr, read, fix) in [
        ("xs nth(0) = 1", "xs (nth(0) = 1)", "(xs nth(0)) = 1"),
        (
            "xs map(inc) != []",
            "xs (map(inc) != [])",
            "(xs map(inc)) != []",
        ),
        ("xs tail ++ [1]", "xs (tail ++ [1])", "(xs tail) ++ [1]"),
        ("xs tail <$> inc", "xs (tail <$> inc)", "(xs tail) <$> inc"),
        ("xs nth(0) < 9", "xs (nth(0) < 9)", "(xs nth(0)) < 9"),
    ] {
        assert_note(expr, &parse_note(expr, read, fix));
    }
}

/// (a2) the operator receives the step's partial application.
#[test]
fn operator_receives_step_function() {
    for (expr, read, fix) in [
        ("xs nth(0) + 1", "xs (nth(0) + 1)", "(xs nth(0)) + 1"),
        ("xs nth(0) * 2", "xs (nth(0) * 2)", "(xs nth(0)) * 2"),
        (
            "xs nth(0) && true",
            "xs (nth(0) && true)",
            "(xs nth(0)) && true",
        ),
        (
            "xs head || false",
            "xs (head || false)",
            "(xs head) || false",
        ),
        ("xs nth(0) <= 1", "xs (nth(0) <= 1)", "(xs nth(0)) <= 1"),
        // a user operator at the default precedence
        ("xs nth(0) <+> 1", "xs (nth(0) <+> 1)", "(xs nth(0)) <+> 1"),
    ] {
        assert_note(expr, &parse_note(expr, read, fix));
    }
}

/// Longer pipelines, chained operators, and steps in blocks, arguments
/// and parentheses written round the pipeline's own atoms.
#[test]
fn nested_positions() {
    assert_note(
        "xs tail nth(0) = 2 && true",
        &parse_note(
            "xs tail nth(0) = 2 && true",
            "xs tail (nth(0) = 2 && true)",
            "(xs tail nth(0)) = 2 && true",
        ),
    );
    assert_note(
        "{ a: xs nth(1) = 2 }.a",
        &parse_note("xs nth(1) = 2", "xs (nth(1) = 2)", "(xs nth(1)) = 2"),
    );
    assert_note(
        "if(xs nth(0) = 1, 1, 2)",
        &parse_note("xs nth(0) = 1", "xs (nth(0) = 1)", "(xs nth(0)) = 1"),
    );
    assert_note(
        "f(xs nth(0) + 1)",
        &parse_note("xs nth(0) + 1", "xs (nth(0) + 1)", "(xs nth(0)) + 1"),
    );
    assert_note(
        "(xs) nth(0) = (1)",
        &parse_note(
            "(xs) nth(0) = (1)",
            "(xs) (nth(0) = (1))",
            "((xs) nth(0)) = (1)",
        ),
    );
    // An operator inside the step's leftmost operand receives the step's
    // function: the error is raised at the inner `+`, not the `=`.
    assert_note(
        "xs nth(0) + 1 = 2",
        &parse_note(
            "xs nth(0) + 1 = 2",
            "xs (nth(0) + 1 = 2)",
            "(xs nth(0)) + 1 = 2",
        ),
    );
    assert_note(
        "xs nth(0) * 2 + 1 = 3",
        &parse_note(
            "xs nth(0) * 2 + 1 = 3",
            "xs (nth(0) * 2 + 1 = 3)",
            "(xs nth(0)) * 2 + 1 = 3",
        ),
    );
    // A step operator expression ending before a lower-precedence operator.
    assert_note(
        "xs nth(0) = 1 //=> true",
        &parse_note("xs nth(0) = 1", "xs (nth(0) = 1)", "(xs nth(0)) = 1"),
    );
}

/// Inside an anaphoric lambda the parse is stated but no parenthesisation
/// is suggested: `(_ nth(0)) = 1` would scope the anaphor to the
/// parentheses. A block anaphor is not scoped by parentheses.
#[test]
fn anaphoric_lambdas() {
    assert_note(
        "xss map(_ nth(0) = 1)",
        "note: '_ nth(0) = 1' was read as '_ (nth(0) = 1)'",
    );
    // An anaphor in the step's head would be scoped by the parentheses too.
    assert_note(
        "xss map(nth(0) _ = 1)",
        "note: 'nth(0) _ = 1' was read as 'nth(0) (_ = 1)'",
    );
    assert_note(
        "xss map(• nth(0) = 1)",
        &parse_note("• nth(0) = 1", "• (nth(0) = 1)", "(• nth(0)) = 1"),
    );
}

/// The note is shown only for that parse and those failures.
#[test]
fn not_shown_without_the_parse() {
    for expr in [
        // the step written in parentheses: what was written was parsed
        "xs (nth(0) = 1)",
        "xs (nth(0) + 1)",
        // the operator on the left of the catenation: `(1 + xs) nth(0)`
        "1 + xs nth(0)",
        // a pipeline that is itself an operator expression:
        // `(1 + xs) (nth(0) = 2)`, not a step on `xs`
        "1 + xs nth(0) = 2",
        // an operator off the step's leftmost spine
        "xs 1 = nth(0) + 1",
        // a section, not a catenation
        "xss map(nth(0) = 1)",
        // a correct pipeline before a lower-precedence operator
        "xs head //=> 2",
        // ordinary not-callable and type-mismatch errors
        "true(1)",
        "\"a\" + 1",
        // a step operator that fails for another reason than a function
        "xs 1 + \"a\"",
        "xs head ∘ inc",
    ] {
        assert_no_note(expr);
    }
}

/// A legitimate operator step, `c f ∘ g`, is not an error.
#[test]
fn legitimate_operator_step() {
    let (code, stdout, notes) = run("2 inc ∘ inc");
    assert_eq!(code, 0, "notes: {notes:#?}");
    assert!(stdout.contains('4'), "stdout: {stdout}");
}
