//! LSP integration tests using the in-process test harness.
//!
//! Each test creates an `LspTestSession`, simulates editing actions,
//! and asserts on LSP responses.  No JSON-RPC transport is involved.

use eucalypt::driver::lsp::testing::LspTestSession;

// ── Stability: single characters and tokens ──────────────────────────────────

#[test]
fn single_open_brace() {
    let mut s = LspTestSession::new();
    s.open("{");
    s.exercise_all();
}

#[test]
fn single_close_brace() {
    let mut s = LspTestSession::new();
    s.open("}");
    s.exercise_all();
}

#[test]
fn single_open_paren() {
    let mut s = LspTestSession::new();
    s.open("(");
    s.exercise_all();
}

#[test]
fn single_open_square() {
    let mut s = LspTestSession::new();
    s.open("[");
    s.exercise_all();
}

#[test]
fn single_colon() {
    let mut s = LspTestSession::new();
    s.open(":");
    s.exercise_all();
}

#[test]
fn single_backtick() {
    let mut s = LspTestSession::new();
    s.open("`");
    s.exercise_all();
}

#[test]
fn single_dot() {
    let mut s = LspTestSession::new();
    s.open(".");
    s.exercise_all();
}

#[test]
fn single_hash() {
    let mut s = LspTestSession::new();
    s.open("#");
    s.exercise_all();
}

#[test]
fn single_quote() {
    let mut s = LspTestSession::new();
    s.open("\"");
    s.exercise_all();
}

// ── Stability: partial constructs ────────────────────────────────────────────

#[test]
fn empty_document() {
    let mut s = LspTestSession::new();
    s.open("");
    s.exercise_all();
}

#[test]
fn bare_colon_in_block() {
    let mut s = LspTestSession::new();
    s.open("{ : }");
    s.exercise_all();
}

#[test]
fn colon_at_block_end() {
    let mut s = LspTestSession::new();
    s.open("{:}");
    s.exercise_all();
}

#[test]
fn unclosed_block() {
    let mut s = LspTestSession::new();
    s.open("{ x: 1");
    s.exercise_all();
}

#[test]
fn unclosed_string() {
    let mut s = LspTestSession::new();
    s.open("x: \"hello");
    s.exercise_all();
}

#[test]
fn incomplete_declaration() {
    let mut s = LspTestSession::new();
    s.open("x:");
    s.exercise_all();
}

#[test]
fn bare_operator() {
    let mut s = LspTestSession::new();
    s.open("+");
    s.exercise_all();
}

#[test]
fn operator_without_rhs() {
    let mut s = LspTestSession::new();
    s.open("x: 1 +");
    s.exercise_all();
}

#[test]
fn metadata_without_declaration() {
    let mut s = LspTestSession::new();
    s.open("` \"orphan doc\"");
    s.exercise_all();
}

#[test]
fn metadata_block_without_declaration() {
    let mut s = LspTestSession::new();
    s.open("` { doc: \"orphan\" }");
    s.exercise_all();
}

#[test]
fn unclosed_paren() {
    let mut s = LspTestSession::new();
    s.open("f(x");
    s.exercise_all();
}

#[test]
fn unclosed_list() {
    let mut s = LspTestSession::new();
    s.open("[1, 2,");
    s.exercise_all();
}

#[test]
fn incomplete_import() {
    let mut s = LspTestSession::new();
    s.open("{ import: ");
    s.exercise_all();
}

#[test]
fn double_colon() {
    let mut s = LspTestSession::new();
    s.open("{ x :: 1 }");
    s.exercise_all();
}

#[test]
fn nested_unclosed_blocks() {
    let mut s = LspTestSession::new();
    s.open("{ a: { b: { c:");
    s.exercise_all();
}

#[test]
fn declaration_with_no_value_before_close() {
    let mut s = LspTestSession::new();
    s.open("{ x: }");
    s.exercise_all();
}

#[test]
fn only_whitespace() {
    let mut s = LspTestSession::new();
    s.open("   \n  \n  ");
    s.exercise_all();
}

#[test]
fn only_comment() {
    let mut s = LspTestSession::new();
    s.open("# just a comment\n");
    s.exercise_all();
}

// ── Stability: rapid edit sequences ──────────────────────────────────────────

#[test]
fn rapid_edit_block_declaration() {
    let mut s = LspTestSession::new();
    for edit in [
        "{", "{ ", "{ x", "{ x:", "{ x: ", "{ x: 1", "{ x: 1 ", "{ x: 1 }",
    ] {
        s.change(edit);
        let _ = s.complete(0, edit.len() as u32);
    }
}

#[test]
fn rapid_edit_function_call() {
    let mut s = LspTestSession::new();
    for edit in [
        "m",
        "ma",
        "map",
        "map(",
        "map(n",
        "map(ne",
        "map(neg",
        "map(nega",
        "map(negat",
        "map(negate",
        "map(negate,",
        "map(negate, ",
        "map(negate, [",
        "map(negate, [1",
        "map(negate, [1]",
        "map(negate, [1])",
    ] {
        s.change(edit);
        let _ = s.complete(0, edit.len() as u32);
        let _ = s.hover(0, 0);
    }
}

#[test]
fn rapid_edit_monad_tag() {
    let mut s = LspTestSession::new();
    for edit in [
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
    ] {
        s.change(edit);
        let _ = s.complete(0, edit.len() as u32);
        let _ = s.hover(0, edit.len().saturating_sub(1) as u32);
    }
}

#[test]
fn rapid_edit_type_annotation() {
    let mut s = LspTestSession::new();
    for edit in [
        "`",
        "` ",
        "` {",
        "` { ",
        "` { t",
        "` { ty",
        "` { typ",
        "` { type",
        "` { type:",
        "` { type: ",
        "` { type: \"",
        "` { type: \"n",
        "` { type: \"number",
        "` { type: \"number\"",
        "` { type: \"number\" ",
        "` { type: \"number\" }",
        "` { type: \"number\" }\n",
        "` { type: \"number\" }\nf(x): x",
    ] {
        s.change(edit);
        s.exercise_all();
    }
}

#[test]
fn rapid_edit_string_interpolation() {
    let mut s = LspTestSession::new();
    for edit in [
        "x: \"",
        "x: \"h",
        "x: \"he",
        "x: \"hel",
        "x: \"hell",
        "x: \"hello",
        "x: \"hello {",
        "x: \"hello {n",
        "x: \"hello {na",
        "x: \"hello {name",
        "x: \"hello {name}",
        "x: \"hello {name}\"",
    ] {
        s.change(edit);
        s.exercise_all();
    }
}

// ── Stability: pathological input ────────────────────────────────────────────

#[test]
fn deeply_nested_blocks() {
    let mut s = LspTestSession::new();
    let depth = 50;
    let mut input = String::new();
    for i in 0..depth {
        input.push_str(&format!("{{a{i}: "));
    }
    input.push('1');
    for _ in 0..depth {
        input.push('}');
    }
    s.open(&input);
    let _ = s.complete(0, 10);
    let _ = s.hover(0, 10);
    let _ = s.inlay_hints();
}

#[test]
fn very_long_line() {
    let mut s = LspTestSession::new();
    let input = format!("x: \"{}\"", "a".repeat(10_000));
    s.open(&input);
    let _ = s.complete(0, 100);
    let _ = s.hover(0, 50);
    let _ = s.inlay_hints();
}

#[test]
fn many_declarations() {
    let mut s = LspTestSession::new();
    let mut input = String::new();
    for i in 0..500 {
        input.push_str(&format!("x{i}: {i}\n"));
    }
    s.open(&input);
    let _ = s.complete(250, 0);
    let _ = s.hover(250, 0);
    let _ = s.inlay_hints();
}

#[test]
fn many_list_elements() {
    let mut s = LspTestSession::new();
    let elems: Vec<String> = (0..500).map(|i| i.to_string()).collect();
    let input = format!("x: [{}]", elems.join(", "));
    s.open(&input);
    let _ = s.complete(0, 10);
    let _ = s.hover(0, 0);
    let _ = s.inlay_hints();
}

// ── Stability: Unicode edge cases ────────────────────────────────────────────

#[test]
fn unicode_identifiers() {
    let mut s = LspTestSession::new();
    s.open("résultat: 42\nπ: 3.14159");
    s.exercise_all();
}

#[test]
fn unicode_operators() {
    let mut s = LspTestSession::new();
    s.open("main: 1 ∧ true ∨ false");
    s.exercise_all();
}

#[test]
fn emoji_in_string() {
    let mut s = LspTestSession::new();
    s.open("x: \"hello 🌍 world\"");
    s.exercise_all();
}

#[test]
fn bracket_pair_characters() {
    let mut s = LspTestSession::new();
    s.open("⟦{}⟧: id\nmain: ⟦ x: 1 ⟧");
    s.exercise_all();
}

#[test]
fn lens_bracket_characters() {
    let mut s = LspTestSession::new();
    s.open("main: ‹ :name ›");
    s.exercise_all();
}

// ── Stability: document lifecycle ────────────────────────────────────────────

#[test]
fn open_and_immediate_close() {
    let mut s = LspTestSession::new();
    s.open("x: 1");
    // Immediately change to empty — simulates close-like behaviour
    s.change("");
    s.exercise_all();
}

#[test]
fn repeated_opens() {
    let mut s = LspTestSession::new();
    s.open("x: 1");
    s.exercise_all();
    s.open("y: 2");
    s.exercise_all();
    s.open("z: 3");
    s.exercise_all();
}

#[test]
fn edit_to_valid_then_invalid_then_valid() {
    let mut s = LspTestSession::new();
    s.open("x: 1");
    s.exercise_all();
    s.change("x:");
    s.exercise_all();
    s.change("x: 2");
    s.exercise_all();
}

// ── Stability: multiline documents ───────────────────────────────────────────

#[test]
fn multiline_block() {
    let mut s = LspTestSession::new();
    s.open("ns: {\n  x: 1\n  y: 2\n  z: x + y\n}");
    for line in 0..5 {
        let _ = s.complete(line, 0);
        let _ = s.hover(line, 2);
    }
    let _ = s.inlay_hints();
}

#[test]
fn multiline_with_metadata() {
    let mut s = LspTestSession::new();
    s.open("` { doc: \"A function\"\n    type: \"number → number\" }\nf(x): x + 1");
    for line in 0..3 {
        let _ = s.complete(line, 0);
        let _ = s.hover(line, 5);
    }
    let _ = s.inlay_hints();
}

// ── Feature: monad tag completion ────────────────────────────────────────────

#[test]
fn monad_tag_completion_after_colon() {
    let mut s = LspTestSession::new();
    s.open("main: { : }");
    let labels = s.complete_labels(0, 8);
    assert!(
        labels.iter().any(|l| l.contains("for")),
        "should offer :for in completion, got: {labels:?}"
    );
}

// ── Feature: hover ───────────────────────────────────────────────────────────

#[test]
fn hover_on_prelude_function() {
    let mut s = LspTestSession::new();
    s.open("main: map(negate, [1,2,3])");
    let text = s.hover_text(0, 6);
    assert!(text.is_some(), "hover on 'map' should return something");
}

// ── Feature: inlay hints ─────────────────────────────────────────────────────

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

// ── Feature: import resolution ───────────────────────────────────────────────

#[test]
fn hover_on_imported_function() {
    let mut s = LspTestSession::new();
    // Point the URI at a real file location so import resolution works
    let test_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/lsp");
    let test_uri =
        lsp_types::Url::from_file_path(test_dir.join("test_import.eu")).expect("valid file URI");
    s.set_uri(test_uri.as_str());
    // Use run_pipeline to wait for the background pipeline to complete
    // before checking hover — import symbols come from the pipeline now.
    s.run_pipeline("{ import: \"import_lib.eu\" }\nmain: double(21)");
    let text = s.hover_text(1, 6);
    assert!(
        text.is_some(),
        "hover on 'double' (from import) should return something"
    );
    let content = text.unwrap();
    assert!(
        content.contains("double") || content.contains("helper"),
        "hover should mention the imported function, got: {content}"
    );
}

#[test]
fn completion_includes_imported_names() {
    let mut s = LspTestSession::new();
    let test_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/lsp");
    let test_uri =
        lsp_types::Url::from_file_path(test_dir.join("test_import.eu")).expect("valid file URI");
    s.set_uri(test_uri.as_str());
    // Wait for the pipeline to complete so import symbols are available.
    s.run_pipeline("{ import: \"import_lib.eu\" }\nmain: d");
    let labels = s.complete_labels(1, 7);
    assert!(
        labels.iter().any(|l| l == "double"),
        "completion should include 'double' from import, got: {labels:?}"
    );
}

#[test]
fn missing_import_does_not_crash() {
    let mut s = LspTestSession::new();
    let test_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/lsp");
    let test_uri =
        lsp_types::Url::from_file_path(test_dir.join("test_import.eu")).expect("valid file URI");
    s.set_uri(test_uri.as_str());
    s.open("{ import: \"nonexistent.eu\" }\nmain: 42");
    // Must not panic — pipeline will fail but that's fine.
    s.exercise_all();
}

#[test]
fn multiple_imports() {
    let mut s = LspTestSession::new();
    let test_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/lsp");
    let test_uri =
        lsp_types::Url::from_file_path(test_dir.join("test_import.eu")).expect("valid file URI");
    s.set_uri(test_uri.as_str());
    // Wait for the pipeline to complete so import symbols are available.
    s.run_pipeline("{ import: [\"import_lib.eu\"] }\nmain: lib-version");
    let text = s.hover_text(1, 6);
    assert!(
        text.is_some(),
        "hover on 'lib-version' (from import) should return something"
    );
}
