//! LSP integration tests using the in-process test harness.
//!
//! Each test creates an `LspTestSession`, simulates editing actions,
//! and asserts on LSP responses.  No JSON-RPC transport is involved.

use eucalypt::driver::lsp::testing::LspTestSession;

// ── Stability: partial input must not panic ────────────────────────────────

#[test]
fn bare_colon_in_block_does_not_crash() {
    let mut s = LspTestSession::new();
    s.open("{ : }");
    // These should not panic
    let _ = s.complete(0, 3);
    let _ = s.hover(0, 3);
    let _ = s.inlay_hints();
}

#[test]
fn empty_document_does_not_crash() {
    let mut s = LspTestSession::new();
    s.open("");
    let _ = s.complete(0, 0);
    let _ = s.hover(0, 0);
    let _ = s.inlay_hints();
}

#[test]
fn unclosed_block_does_not_crash() {
    let mut s = LspTestSession::new();
    s.open("{ x: 1");
    let _ = s.complete(0, 6);
    let _ = s.hover(0, 2);
    let _ = s.inlay_hints();
}

#[test]
fn unclosed_string_does_not_crash() {
    let mut s = LspTestSession::new();
    s.open("x: \"hello");
    let _ = s.complete(0, 9);
    let _ = s.hover(0, 0);
    let _ = s.inlay_hints();
}

#[test]
fn incomplete_declaration_does_not_crash() {
    let mut s = LspTestSession::new();
    s.open("x:");
    let _ = s.complete(0, 2);
    let _ = s.hover(0, 0);
    let _ = s.inlay_hints();
}

#[test]
fn rapid_edit_sequence_does_not_crash() {
    let mut s = LspTestSession::new();
    let edits = [
        "{", "{ ", "{ x", "{ x:", "{ x: ", "{ x: 1", "{ x: 1 ", "{ x: 1 }",
    ];
    for edit in edits {
        s.change(edit);
        let _ = s.complete(0, edit.len() as u32);
    }
}

// ── Monad tag completion ───────────────────────────────────────────────────

#[test]
fn monad_tag_completion_after_colon() {
    let mut s = LspTestSession::new();
    s.open("main: { :for x: [1,2,3] }.(x)");
    // Completion at the `:` position inside a block with a known monad tag
    // should include monad namespaces (requires prelude loaded)
    s.change("main: { : }");
    let labels = s.complete_labels(0, 8);
    // The prelude defines for, io, let, random, state as monads
    assert!(
        labels.iter().any(|l| l.contains("for")),
        "should offer :for in completion, got: {labels:?}"
    );
}

// ── Hover ──────────────────────────────────────────────────────────────────

#[test]
fn hover_on_prelude_function() {
    let mut s = LspTestSession::new();
    s.open("main: map(negate, [1,2,3])");
    let text = s.hover_text(0, 6);
    assert!(text.is_some(), "hover on 'map' should return something");
}

// ── Inlay hints ────────────────────────────────────────────────────────────

#[test]
fn inlay_hints_on_monadic_binding() {
    let mut s = LspTestSession::new();
    s.open("main: { :for x: [1,2,3] }.(x)");
    let hints = s.inlay_hints();
    let type_hints: Vec<_> = hints
        .iter()
        .filter(|h| matches!(&h.label, lsp_types::InlayHintLabel::String(s) if s.contains("[a]")))
        .collect();
    assert!(
        !type_hints.is_empty(),
        "should show [a] type hint on monadic binding, got: {hints:?}"
    );
}

// ── Regression: typing monad tag character by character ─────────────────────

#[test]
fn typing_monad_tag_keystroke_by_keystroke() {
    let mut s = LspTestSession::new();
    let keystrokes = [
        "main: {",
        "main: { ",
        "main: { :",
        "main: { :f",
        "main: { :fo",
        "main: { :for",
        "main: { :for ",
        "main: { :for x",
        "main: { :for x:",
        "main: { :for x: ",
        "main: { :for x: [",
        "main: { :for x: [1",
        "main: { :for x: [1,",
        "main: { :for x: [1,2",
        "main: { :for x: [1,2]",
        "main: { :for x: [1,2] ",
        "main: { :for x: [1,2] }",
        "main: { :for x: [1,2] }.",
        "main: { :for x: [1,2] }.(x)",
    ];
    for keystroke in keystrokes {
        s.change(keystroke);
        // All of these must not panic
        let _ = s.complete(0, keystroke.len() as u32);
        let _ = s.hover(0, keystroke.len().saturating_sub(1) as u32);
    }
}
