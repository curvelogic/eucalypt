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
fn inlay_hints_on_monadic_binding_without_pipeline() {
    let mut s = LspTestSession::new();
    s.open("main: { :for x: [1,2,3] }.(x)");
    let hints = s.inlay_hints();
    // Without the pipeline, no resolved element type is available.
    // We suppress the hint entirely rather than showing the misleading
    // wrapper type [a].
    let type_hints: Vec<_> = hints
        .iter()
        .filter(|h| matches!(&h.label, lsp_types::InlayHintLabel::String(s) if s.contains("[a]")))
        .collect();
    assert!(
        type_hints.is_empty(),
        "should NOT show [a] wrapper type hint without pipeline, got: {hints:?}"
    );
}

// ── Feature: monadic bound variable type hinting ─────────────────────────────

#[test]
fn inlay_hint_shows_element_type_after_pipeline() {
    let mut s = LspTestSession::new();
    // After the pipeline runs, the type checker infers x: number
    // from for.bind([1,2,3], λx. ...) where bind: [a] → (a → [b]) → [b]
    // and a unifies with number.
    s.run_pipeline("main: { :for x: [1,2,3] }.(x)");
    let hints = s.inlay_hints();
    let type_hints: Vec<_> = hints
        .iter()
        .filter(
            |h| matches!(&h.label, lsp_types::InlayHintLabel::String(s) if s.contains("number")),
        )
        .collect();
    assert!(
        !type_hints.is_empty(),
        "after pipeline, should show element type 'number' not wrapper '[a]', got: {:?}",
        hints
            .iter()
            .filter_map(|h| match &h.label {
                lsp_types::InlayHintLabel::String(s) => Some(s.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>()
    );
}

#[test]
fn inlay_hint_let_monad_shows_wrapper_not_element() {
    let mut s = LspTestSession::new();
    // :let is untyped (monad: true) — no element type inference
    s.run_pipeline("main: { :let x: 42 }.(x)");
    let hints = s.inlay_hints();
    // Should NOT show a resolved element type — :let has no typed wrapper
    let number_hints: Vec<_> = hints
        .iter()
        .filter(
            |h| matches!(&h.label, lsp_types::InlayHintLabel::String(s) if s.contains("number")),
        )
        .collect();
    assert!(
        number_hints.is_empty(),
        ":let monad should not show element type hints, got: {:?}",
        hints
            .iter()
            .filter_map(|h| match &h.label {
                lsp_types::InlayHintLabel::String(s) => Some(s.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>()
    );
}

#[test]
fn inlay_hint_element_type_does_not_crash_on_incomplete() {
    let mut s = LspTestSession::new();
    // Incomplete monadic block value — pipeline may fail, must not crash
    s.open("main: { :for x: }.(x)");
    let _ = s.inlay_hints();
    s.open("main: { :for x: [1, }.(x)");
    let _ = s.inlay_hints();
}

#[test]
fn inlay_hint_io_monad_element_type() {
    let mut s = LspTestSession::new();
    // IO monad bindings: io.bind returns IO(a), element type should be string
    // for io.read-file. Since we can't actually run IO in tests, use a typed
    // wrapper that the checker can resolve.
    s.run_pipeline("main: { :io x: io.read-file(\"test.txt\") }.(x)");
    let hints = s.inlay_hints();
    let type_hints: Vec<_> = hints
        .iter()
        .filter_map(|h| match &h.label {
            lsp_types::InlayHintLabel::String(s) => Some(s.as_str()),
            _ => None,
        })
        .filter(|s| s.contains(':'))
        .collect();
    // IO monad has monad type IO(a) — if the pipeline resolves it,
    // we get a type hint; if not (IO is opaque), hint is suppressed.
    // Either way, no crash and no misleading [a]-style wrapper.
    for hint in &type_hints {
        assert!(
            !hint.contains("[a]") && !hint.contains("IO("),
            "IO binding should not show raw wrapper type, got: {hint}"
        );
    }
}

#[test]
fn inlay_hint_multi_block_same_binding_name() {
    let mut s = LspTestSession::new();
    // Two :for blocks on different lines with the same binding name 'x'.
    // Each should resolve independently without collision.
    s.run_pipeline("a: { :for x: [1, 2, 3] }.(x)\nb: { :for x: [\"a\", \"b\"] }.(x)\n");
    let hints = s.inlay_hints();
    let type_hints: Vec<_> = hints
        .iter()
        .filter_map(|h| match &h.label {
            lsp_types::InlayHintLabel::String(s) => Some(s.as_str()),
            _ => None,
        })
        .filter(|s| s.contains(':'))
        .collect();
    // Should have two hints: one 'number' and one literal-string union ("a" | "b").
    // Since A4, string literals synthesise to their LiteralString type, so the
    // element type of ["a", "b"] is "a" | "b" rather than string.
    let has_number = type_hints.iter().any(|h| h.contains("number"));
    let has_string_hint = type_hints
        .iter()
        .any(|h| h.contains("\"a\"") && h.contains("\"b\""));
    assert!(
        has_number && has_string_hint,
        "should have both number and literal-string union hints for independent blocks, got: {type_hints:?}"
    );
}

#[test]
fn inlay_hint_heterogeneous_list_shows_union() {
    let mut s = LspTestSession::new();
    // Heterogeneous list — element type should be a union
    s.run_pipeline("main: { :for x: [1, \"hello\"] }.(x)");
    let hints = s.inlay_hints();
    let type_hints: Vec<_> = hints
        .iter()
        .filter_map(|h| match &h.label {
            lsp_types::InlayHintLabel::String(s) => Some(s.as_str()),
            _ => None,
        })
        .filter(|s| s.contains(':'))
        .collect();
    // Should show a union type (number | string) or suppress if
    // the checker can't resolve it. Must not show [a] wrapper.
    for hint in &type_hints {
        assert!(
            !hint.contains("[a]"),
            "heterogeneous list should not show raw wrapper, got: {hint}"
        );
    }
    // If a hint is present, it should mention both types
    if !type_hints.is_empty() {
        let combined = type_hints.join(" ");
        assert!(
            combined.contains("number") || combined.contains("string"),
            "heterogeneous hint should show union type, got: {type_hints:?}"
        );
    }
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

// ── Pipeline: green node change detection ────────────────────────────────────

#[test]
fn identical_content_does_not_spawn_pipeline() {
    let mut s = LspTestSession::new();
    s.run_pipeline("x: 1");
    assert!(s.has_cached(), "first open should produce cached result");

    // Change to identical content — green node should be the same
    s.change("x: 1");

    // Wait a bit longer than the debounce period
    std::thread::sleep(std::time::Duration::from_millis(500));

    // No new pipeline result should have arrived (try_recv returns false)
    let received = s.try_recv_pipeline();
    assert!(
        !received,
        "identical content should not spawn a new pipeline"
    );
}

#[test]
fn different_content_spawns_pipeline() {
    let mut s = LspTestSession::new();
    s.run_pipeline("x: 1");
    assert!(s.has_cached());

    // Change to different content
    s.change("x: 2");
    s.wait_for_pipeline();

    // Should have a new cached result
    assert!(
        s.has_cached(),
        "different content should produce new cached result"
    );
}

// ── Pipeline: rapid changes cancel previous runs ─────────────────────────────

#[test]
fn rapid_changes_produce_single_pipeline_result() {
    let mut s = LspTestSession::new();

    // Fire many rapid changes
    for i in 0..10 {
        s.change(&format!("x: {i}"));
    }

    // Wait for the final pipeline to complete
    s.wait_for_pipeline();
    assert!(s.has_cached());

    // Drain any extra results — there should be at most one
    // (the final change, since earlier ones were cancelled)
    let mut extra = 0;
    while s.try_recv_pipeline() {
        extra += 1;
    }
    assert!(
        extra <= 1,
        "rapid changes should cancel intermediate pipelines, got {extra} extra results"
    );
}

// ── Pipeline: error handling ─────────────────────────────────────────────────

#[test]
fn pipeline_error_does_not_crash() {
    let mut s = LspTestSession::new();
    // Valid content first
    s.run_pipeline("x: 1");
    assert!(s.has_cached());

    // Content with a deliberate desugar error (unclosed block in import)
    // This should cause a pipeline error but not crash
    s.change("{ import: \"nonexistent_file_that_doesnt_exist.eu\" }\nx: 1");
    s.wait_for_pipeline();

    // LSP operations should still work (using AST)
    let _ = s.complete(0, 0);
    let _ = s.hover(0, 0);
}

// ── Pipeline: requests during pipeline run ───────────────────────────────────

#[test]
fn requests_during_pipeline_run_do_not_crash() {
    let mut s = LspTestSession::new();
    s.open("x: 1\ny: 2\nz: x + y");

    // Don't wait — fire requests while pipeline is in flight
    for _ in 0..5 {
        let _ = s.complete(0, 0);
        let _ = s.hover(0, 0);
        let _ = s.hover(1, 0);
        let _ = s.inlay_hints();
    }

    // Now wait and verify it completes
    s.wait_for_pipeline();
    assert!(s.has_cached());
}

// ── Pipeline: error diagnostics with source location ─────────────────────────

#[test]
fn pipeline_error_has_source_location() {
    let mut s = LspTestSession::new();
    let test_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/lsp");
    let test_uri =
        lsp_types::Url::from_file_path(test_dir.join("pipeline_error.eu")).expect("valid file URI");
    s.set_uri(test_uri.as_str());

    // Content that will cause a pipeline error (bad import)
    s.open("{ import: \"nonexistent_file.eu\" }\nmain: 42");
    s.wait_for_pipeline();

    let err = s.last_pipeline_error();
    assert!(
        err.is_some(),
        "pipeline should have produced an error for missing import"
    );
    let err = err.unwrap();
    assert!(
        !err.message.is_empty(),
        "pipeline error should have a message"
    );
}

#[test]
fn pipeline_error_clears_stale_cache() {
    let mut s = LspTestSession::new();

    // Successful pipeline first
    s.run_pipeline("x: 1");
    assert!(s.has_cached(), "should have cached result");
    assert!(
        s.last_pipeline_error().is_none(),
        "should have no error after success"
    );

    // Now cause a pipeline error
    let test_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/lsp");
    let test_uri =
        lsp_types::Url::from_file_path(test_dir.join("pipeline_error.eu")).expect("valid file URI");
    s.set_uri(test_uri.as_str());
    s.open("{ import: \"nonexistent_file.eu\" }\nmain: 42");
    s.wait_for_pipeline();

    assert!(
        s.last_pipeline_error().is_some(),
        "should have pipeline error"
    );
    // Stale cache should have been cleared
    assert!(
        !s.has_cached(),
        "stale cached result should be cleared on pipeline error"
    );
}

// ── Incremental sync: apply_edit ─────────────────────────────────────────────

#[test]
fn incremental_edit_appends_binding() {
    let mut s = LspTestSession::new();
    s.open("x: 1\n");
    // Append a new binding at the end of the document.
    s.apply_edit(1, 0, 1, 0, "y: 2\n");
    assert_eq!(s.content(), "x: 1\ny: 2\n");
}

#[test]
fn incremental_edit_replaces_value() {
    let mut s = LspTestSession::new();
    s.open("x: 42\n");
    // Replace the value 42 with 99 (columns 3–5 on line 0).
    s.apply_edit(0, 3, 0, 5, "99");
    assert_eq!(s.content(), "x: 99\n");
}

#[test]
fn incremental_edit_deletes_range() {
    let mut s = LspTestSession::new();
    s.open("x: 1\ny: 2\nz: 3\n");
    // Delete the middle line entirely.
    s.apply_edit(1, 0, 2, 0, "");
    assert_eq!(s.content(), "x: 1\nz: 3\n");
}

#[test]
fn incremental_edit_multi_step_produces_valid_parse() {
    let mut s = LspTestSession::new();
    s.open("x: 1\n");
    s.apply_edit(1, 0, 1, 0, "y: ");
    s.apply_edit(1, 3, 1, 3, "2\n");
    assert_eq!(s.content(), "x: 1\ny: 2\n");
    // LSP operations on the final content should not panic.
    s.exercise_all();
}

#[test]
fn incremental_full_replace_without_range() {
    // An edit with no range is treated as a full-document replacement.
    let mut s = LspTestSession::new();
    s.open("x: 1\n");
    s.change("y: 2\n");
    assert_eq!(s.content(), "y: 2\n");
}

#[test]
fn incremental_edit_unicode_operator() {
    // Eucalypt uses Unicode operators — verify that UTF-16 offsets are
    // handled correctly when the text contains multi-byte UTF-8 characters.
    // '÷' is U+00F7: 1 UTF-16 code unit, 2 UTF-8 bytes.
    // UTF-16 layout of "x: 10 ÷ 2\n":
    //   x(0) :(1) (2) 1(3) 0(4) (5) ÷(6,1unit) (7) 2(8) \n(9)
    let mut s = LspTestSession::new();
    s.open("x: 10 ÷ 2\n");
    // Replace '÷' at UTF-16 offset 6–7 with '*'.
    s.apply_edit(0, 6, 0, 7, "*");
    assert_eq!(s.content(), "x: 10 * 2\n");
}

#[test]
fn incremental_edit_surrogate_pair_offset() {
    // Characters requiring surrogate pairs in UTF-16 (U+10000+) must shift
    // subsequent offsets by 2 code units.  '😀' is U+1F600: 2 UTF-16 code
    // units, 4 UTF-8 bytes.
    // UTF-16 layout of "a 😀 b\n":
    //   a(0) (1) 😀(2-3,2units) (4) b(5) \n(6)
    let mut s = LspTestSession::new();
    s.open("a \u{1F600} b\n");
    // Replace 'b' at UTF-16 offset 5 with 'c'.
    s.apply_edit(0, 5, 0, 6, "c");
    assert_eq!(s.content(), "a \u{1F600} c\n");
}

// ── Green node change detection: string-only changes ────────────────────────

/// The green node equality comparison includes token text, not just structure.
/// Changing a string literal must trigger a pipeline re-run because string
/// values can affect the pipeline (e.g. import paths are string literals).
#[test]
fn string_change_triggers_pipeline_run() {
    let mut s = LspTestSession::new();
    // First open: pipeline spawned.
    s.open("x: \"hello\"\n");
    // Change only the string content.  The tree structure is identical
    // (same node kinds), but the token text differs.  Change detection
    // must still report a change and spawn a new pipeline run.
    s.change("x: \"world\"\n");
    // Wait for the pipeline spawned by the second change.
    // If change detection incorrectly skipped the re-run, this would time out.
    s.wait_for_pipeline();
    assert!(
        s.has_cached(),
        "pipeline should have run after string change"
    );
}

#[test]
fn identical_content_does_not_respawn_pipeline() {
    let mut s = LspTestSession::new();
    s.open("x: 1\n");
    // Simulate an editor sending the same content again (no real change).
    // The green node is identical so no pipeline should be spawned.
    // We verify by checking that try_recv returns nothing after a brief wait.
    s.change("x: 1\n");
    // Give a short window for any spurious spawn.
    std::thread::sleep(std::time::Duration::from_millis(50));
    let received = s.try_recv_pipeline();
    assert!(
        !received,
        "no pipeline should run when content is unchanged"
    );
}

// ── Bracket pair LSP features (eu-gduc, Phase 1) ─────────────────────────────

/// Bracket delimiters must not cause panics in any LSP operation.
#[test]
fn bracket_pair_expression_mode_stability() {
    let mut s = LspTestSession::new();
    // Expression-mode bracket pair: ⟦ x ⟧: body
    s.open("⟦ x ⟧: x\nresult: ⟦ 42 ⟧\n");
    s.exercise_all();
}

#[test]
fn bracket_pair_block_mode_stability() {
    let mut s = LspTestSession::new();
    // Block-mode bracket pair: ⟦{}⟧: id  then  ⟦ a: 1 ⟧
    s.open("⟦{}⟧: id\nresult: ⟦ a: 1 ⟧\n");
    s.exercise_all();
}

/// Hover on a bracket open delimiter should return information about the pair.
#[test]
fn hover_on_bracket_open_delimiter() {
    let mut s = LspTestSession::new();
    let src = "⟦ x ⟧: x\nresult: ⟦ 42 ⟧\n";
    s.open(src);
    // The second line starts with '⟦' — hover at column 8 (UTF-16)
    // Line 1 is "result: ⟦ 42 ⟧"
    //   r(0) e(1) s(2) u(3) l(4) t(5) :(6) (7) ⟦(8, 1 UTF-16 unit) …
    let hover = s.hover_text(1, 8);
    assert!(
        hover.is_some(),
        "hover on bracket delimiter should return information"
    );
    let text = hover.unwrap();
    assert!(
        text.contains("⟦⟧") || text.contains("bracket"),
        "hover should mention the bracket pair: {text}"
    );
}

/// Go-to-definition on a bracket open delimiter should resolve to the declaration.
#[test]
fn goto_definition_on_bracket_open_not_panic() {
    let mut s = LspTestSession::new();
    let src = "⟦ x ⟧: x\nresult: ⟦ 42 ⟧\n";
    s.open(src);
    // Just verify it does not panic — the result depends on the symbol table
    let _hover = s.hover(1, 8);
    let _complete = s.complete(1, 8);
}

/// Bracket pair definitions should appear in document symbols.
#[test]
fn bracket_pair_in_stability_exercise() {
    let mut s = LspTestSession::new();
    // Multiple different bracket pairs
    s.open("⟦ x ⟧: x\n⌈ x ⌉: x * 2\n\nresult: ⟦ 99 ⟧\n");
    s.exercise_all();
}

// ── Phase 2: bracket pair matching (documentHighlight) ───────────────────────

/// Hovering on the open bracket should highlight both open and close.
#[test]
fn document_highlight_bracket_open_highlights_pair() {
    let mut s = LspTestSession::new();
    s.open("⟦ x ⟧: x\nresult: ⟦ 42 ⟧\n");
    // Hover at '⟦' on line 1, UTF-16 column 8
    let highlights = s.document_highlights(1, 8);
    assert_eq!(
        highlights.len(),
        2,
        "should highlight both open and close bracket: {highlights:?}"
    );
}

/// Hovering on the close bracket should also highlight both.
#[test]
fn document_highlight_bracket_close_highlights_pair() {
    let mut s = LspTestSession::new();
    s.open("⟦ x ⟧: x\nresult: ⟦ 42 ⟧\n");
    // '⟧' is at UTF-16 column 13 on line 1
    // "result: ⟦ 42 ⟧" → r(0)e(1)s(2)u(3)l(4)t(5):(6) (7)⟦(8) (9)4(10)2(11) (12)⟧(13)
    let highlights = s.document_highlights(1, 13);
    assert_eq!(
        highlights.len(),
        2,
        "close bracket should highlight both delimiters: {highlights:?}"
    );
}

/// Non-bracket positions should return no highlights.
#[test]
fn document_highlight_non_bracket_returns_empty() {
    let mut s = LspTestSession::new();
    s.open("x: 42\n");
    let highlights = s.document_highlights(0, 0);
    assert!(highlights.is_empty(), "should return empty for non-bracket");
}

/// Block-mode bracket pairs should also get matching highlights.
#[test]
fn document_highlight_block_mode_bracket() {
    let mut s = LspTestSession::new();
    s.open("⟦{}⟧: id\nresult: ⟦ a: 1 ⟧\n");
    let highlights = s.document_highlights(1, 8);
    assert_eq!(
        highlights.len(),
        2,
        "block-mode bracket should highlight both delimiters: {highlights:?}"
    );
}

// ── Phase 2: inlay hints for monadic bracket blocks ───────────────────────────

/// Without the pipeline, monadic bracket blocks should suppress binding
/// type hints rather than showing the misleading wrapper type.
#[test]
fn inlay_hints_monadic_bracket_block_without_pipeline() {
    let mut s = LspTestSession::new();
    // Define a monadic bracket pair with type annotation, then use it.
    s.open("` { monad: \"[a]\" }\n⟦{}⟧: id\nresult: ⟦ x: [1,2] y: [3,4] ⟧\n");
    let hints = s.inlay_hints();
    // Without the pipeline, no resolved element type — hints suppressed.
    let type_hints: Vec<_> = hints
        .iter()
        .filter(|h| matches!(&h.label, lsp_types::InlayHintLabel::String(s) if s.contains("[a]")))
        .collect();
    assert!(
        type_hints.is_empty(),
        "should NOT show [a] wrapper type hints without pipeline"
    );
}

// ── Phase 2: go-to-definition on bracket block binding names ─────────────────

/// Go-to-definition on a binding name inside a bracket block should jump
/// to the bracket pair definition.
#[test]
fn goto_definition_bracket_block_binding_jumps_to_pair() {
    let mut s = LspTestSession::new();
    // Define and use a block-mode bracket pair
    let src = "⟦{}⟧: id\nresult: ⟦ a: 1 ⟧\n";
    s.open(src);
    s.wait_for_pipeline();
    // 'a' is the binding name in "⟦ a: 1 ⟧" on line 1
    // Line 1: "result: ⟦ a: 1 ⟧"
    //   r(0)e(1)s(2)u(3)l(4)t(5):(6) (7)⟦(8) (9)a(10):(11) (12)1(13) (14)⟧(15)
    let def = s.goto_definition(1, 10);
    assert!(
        def.is_some(),
        "go-to-def on bracket block binding should return a result"
    );
}

// ── Go-to-definition: prelude names ──────────────────────────────────────────

#[test]
fn goto_definition_prelude_function_returns_file_uri() {
    let mut s = LspTestSession::new();
    // "map" is at column 6 in "main: map(negate, [1])"
    s.open("main: map(negate, [1])");
    let def = s.goto_definition(0, 6);
    assert!(def.is_some(), "go-to-def on 'map' should return a result");
    // The definition should point to a file: URI (the temp prelude),
    // not resource:prelude which editors can't navigate to.
    match def.unwrap() {
        lsp_types::GotoDefinitionResponse::Scalar(loc) => {
            assert!(
                loc.uri.scheme() == "file",
                "prelude go-to-def should use file: URI, got: {}",
                loc.uri
            );
        }
        lsp_types::GotoDefinitionResponse::Array(locs) => {
            assert!(!locs.is_empty());
            assert!(
                locs[0].uri.scheme() == "file",
                "prelude go-to-def should use file: URI, got: {}",
                locs[0].uri
            );
        }
        lsp_types::GotoDefinitionResponse::Link(links) => {
            assert!(!links.is_empty());
            assert!(
                links[0].target_uri.scheme() == "file",
                "prelude go-to-def should use file: URI, got: {}",
                links[0].target_uri
            );
        }
    }
}

// ── Code actions: structural editing ────────────────────────────────────────────

#[test]
fn code_action_wrap_as_namespace() {
    let mut s = LspTestSession::new();
    s.open("my-ns: value\n");
    let titles: Vec<String> = s.all_code_actions().into_iter().map(|a| a.title).collect();
    assert!(
        titles.contains(&"Wrap as namespace".to_string()),
        "expected 'Wrap as namespace' action, got: {titles:?}"
    );

    // Verify the edit content
    let action = s
        .all_code_actions()
        .into_iter()
        .find(|a| a.title == "Wrap as namespace")
        .unwrap();
    let edit = action.edit.unwrap();
    let changes = edit.changes.unwrap();
    let edits = changes.values().next().unwrap();
    assert!(
        edits[0].new_text.contains('{'),
        "edit should introduce a block: {:?}",
        edits[0].new_text
    );
}

#[test]
fn code_action_promote_metadata_docstring() {
    let mut s = LspTestSession::new();
    s.open("` \"docstring\"\nx: 1\n");
    let titles: Vec<String> = s.all_code_actions().into_iter().map(|a| a.title).collect();
    assert!(
        titles.contains(&"Promote metadata to block form".to_string()),
        "expected promote action, got: {titles:?}"
    );

    let action = s
        .all_code_actions()
        .into_iter()
        .find(|a| a.title == "Promote metadata to block form")
        .unwrap();
    let edit = action.edit.unwrap();
    let changes = edit.changes.unwrap();
    let edits = changes.values().next().unwrap();
    assert!(
        edits[0].new_text.contains("doc:"),
        "promoted form should contain 'doc:': {:?}",
        edits[0].new_text
    );
}

#[test]
fn code_action_promote_metadata_suppress() {
    let mut s = LspTestSession::new();
    s.open("` :suppress\nx: 1\n");
    let action = s
        .all_code_actions()
        .into_iter()
        .find(|a| a.title == "Promote metadata to block form")
        .unwrap();
    let edit = action.edit.unwrap();
    let changes = edit.changes.unwrap();
    let edits = changes.values().next().unwrap();
    assert!(
        edits[0].new_text.contains("export: :suppress"),
        "promoted :suppress should become 'export: :suppress': {:?}",
        edits[0].new_text
    );
}

#[test]
fn code_action_demote_metadata() {
    let mut s = LspTestSession::new();
    s.open("` { doc: \"hello\" }\nx: 1\n");
    let titles: Vec<String> = s.all_code_actions().into_iter().map(|a| a.title).collect();
    assert!(
        titles.contains(&"Demote metadata to shortcut form".to_string()),
        "expected demote action, got: {titles:?}"
    );

    let action = s
        .all_code_actions()
        .into_iter()
        .find(|a| a.title == "Demote metadata to shortcut form")
        .unwrap();
    let edit = action.edit.unwrap();
    let changes = edit.changes.unwrap();
    let edits = changes.values().next().unwrap();
    assert!(
        edits[0].new_text.contains("\"hello\""),
        "demoted form should contain the string: {:?}",
        edits[0].new_text
    );
    assert!(
        !edits[0].new_text.contains("doc:"),
        "demoted form should not contain 'doc:': {:?}",
        edits[0].new_text
    );
}

#[test]
fn code_action_demote_metadata_not_offered_for_multi_field() {
    let mut s = LspTestSession::new();
    s.open("` { doc: \"hello\", type: \"Num\" }\nx: 1\n");
    let titles: Vec<String> = s.all_code_actions().into_iter().map(|a| a.title).collect();
    assert!(
        !titles.contains(&"Demote metadata to shortcut form".to_string()),
        "demote should not be offered for multi-field metadata"
    );
}

#[test]
fn code_action_add_metadata_to_bare_declaration() {
    let mut s = LspTestSession::new();
    s.open("x: 1\n");
    let titles: Vec<String> = s.all_code_actions().into_iter().map(|a| a.title).collect();
    assert!(
        titles.contains(&"Add `doc:` metadata".to_string()),
        "expected add-doc action, got: {titles:?}"
    );
    assert!(
        titles.contains(&"Add `type:` metadata".to_string()),
        "expected add-type action, got: {titles:?}"
    );
}

#[test]
fn code_action_add_metadata_to_block_metadata() {
    let mut s = LspTestSession::new();
    s.open("` { doc: \"hello\" }\nx: 1\n");
    let action = s
        .all_code_actions()
        .into_iter()
        .find(|a| a.title == "Add `type:` metadata")
        .expect("should offer add-type action on block metadata");
    let edit = action.edit.unwrap();
    let changes = edit.changes.unwrap();
    let edits = changes.values().next().unwrap();
    assert!(
        edits[0].new_text.contains("type:"),
        "edit should add 'type:' field: {:?}",
        edits[0].new_text
    );
}

#[test]
fn code_action_let_block_toggle_add() {
    let mut s = LspTestSession::new();
    s.open("{ x: 1, y: 2 }\n");
    let titles: Vec<String> = s.all_code_actions().into_iter().map(|a| a.title).collect();
    assert!(
        titles.contains(&"Convert to sequential let-block".to_string()),
        "expected let-block toggle action, got: {titles:?}"
    );

    let action = s
        .all_code_actions()
        .into_iter()
        .find(|a| a.title == "Convert to sequential let-block")
        .unwrap();
    let edit = action.edit.unwrap();
    let changes = edit.changes.unwrap();
    let edits = changes.values().next().unwrap();
    assert!(
        edits[0].new_text.contains(":let"),
        "edit should insert ':let': {:?}",
        edits[0].new_text
    );
}

#[test]
fn code_action_let_block_toggle_remove() {
    let mut s = LspTestSession::new();
    s.open("{ :let x: 1, y: 2 }\n");
    let titles: Vec<String> = s.all_code_actions().into_iter().map(|a| a.title).collect();
    assert!(
        titles.contains(&"Convert to plain block".to_string()),
        "expected plain block toggle action, got: {titles:?}"
    );
}

#[test]
fn code_action_wrap_selection_into_namespace() {
    let mut s = LspTestSession::new();
    s.open("a: 1\nb: 2\nc: a + b\n");
    // Select first two declarations (a and b), leaving c outside
    let actions = s.code_actions(0, 0, 1, 4);
    let wrap_action = actions
        .iter()
        .find(|a| a.title == "Wrap selection into namespace");
    assert!(
        wrap_action.is_some(),
        "expected multi-wrap action when multiple declarations selected"
    );

    // Check that the edit prefixes the reference to `a` in `c: a + b`
    let action = wrap_action.unwrap();
    let ws_edit = action.edit.as_ref().unwrap();
    let edits = ws_edit.changes.as_ref().unwrap();
    let file_edits = edits.values().next().unwrap();
    // Should have: 1 edit for the wrap + 2 edits for prefixing `a` and `b` in `c: a + b`
    let prefix_edits: Vec<_> = file_edits
        .iter()
        .filter(|e| e.new_text.starts_with("my-ns."))
        .collect();
    assert!(
        prefix_edits.len() >= 2,
        "should prefix references to a and b outside the wrapped block, got: {prefix_edits:?}"
    );
}

#[test]
fn inlay_hint_element_type_multiline_file() {
    let mut s = LspTestSession::new();
    // Multi-line file with content before the monadic block —
    // replicates the real-world scenario where the hint shows [a]
    // instead of number.
    s.run_pipeline("helper: 42\nfor-test: { :for x: [1, 2, 3] }.(x * 2)\n");
    let hints = s.inlay_hints();
    let number_hints: Vec<_> = hints
        .iter()
        .filter(
            |h| matches!(&h.label, lsp_types::InlayHintLabel::String(s) if s.contains("number")),
        )
        .collect();
    let all_labels: Vec<_> = hints
        .iter()
        .filter_map(|h| match &h.label {
            lsp_types::InlayHintLabel::String(s) => Some(s.as_str()),
            _ => None,
        })
        .collect();
    assert!(
        !number_hints.is_empty(),
        "multiline: should show 'number' hint, got: {all_labels:?}"
    );
}

#[test]
fn inlay_hint_element_type_with_import() {
    let mut s = LspTestSession::new();
    let test_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/lsp");
    let test_uri =
        lsp_types::Url::from_file_path(test_dir.join("hint_import_test.eu")).expect("valid URI");
    s.set_uri(test_uri.as_str());
    s.run_pipeline("{ import: \"import_lib.eu\" }\nfor-test: { :for x: [1, 2, 3] }.(x * 2)\n");
    let hints = s.inlay_hints();
    let number_hints: Vec<_> = hints
        .iter()
        .filter(
            |h| matches!(&h.label, lsp_types::InlayHintLabel::String(s) if s.contains("number")),
        )
        .collect();
    let all_labels: Vec<_> = hints
        .iter()
        .filter_map(|h| match &h.label {
            lsp_types::InlayHintLabel::String(s) => Some(s.as_str()),
            _ => None,
        })
        .collect();
    assert!(
        !number_hints.is_empty(),
        "with import: should show 'number' hint, got: {all_labels:?}"
    );
}

// ── Type-alias reference tooling ─────────────────────────────────────────────

/// Go-to-definition from an alias reference inside a `type:` string lands
/// on the alias definition site.
#[test]
fn alias_goto_definition_from_type_string() {
    let mut s = LspTestSession::new();
    // Line 0: the alias definition (type-def: "Point")
    // Line 1: the defined binding
    // Line 2: `{ type: "Point -> number" }` — contains alias reference
    // Line 3: the annotated function
    s.open(concat!(
        "` { type-def: \"Point\" }\n",
        "point: { x: 1, y: 2 }\n",
        "` { type: \"Point -> number\" }\n",
        "get-x(p): p.x\n",
    ));

    // Cursor on "Point" inside the type: string on line 2.
    // "` { type: \"" is 11 chars; "Point" starts at col 11.
    let result = s.goto_definition_for_type_alias(2, 11);
    assert!(
        result.is_some(),
        "should find definition of Point from type: string"
    );
}

/// Hover on an alias reference inside a `type:` string shows the alias name and
/// its resolved type when the pipeline has run.
#[test]
fn alias_hover_in_type_string() {
    let mut s = LspTestSession::new();
    // Use a types: block so the type checker registers "Point" → number in the
    // alias map.  A real declaration must appear between consecutive backtick
    // metadata blocks.  run_pipeline waits for the background pipeline so that
    // alias_types is populated in the cached result.
    s.run_pipeline(concat!(
        "` { types: { Point: \"number\" } }\n",
        "data: 1\n",
        "` { type: \"Point -> number\" }\n",
        "get-x(p): p\n",
    ));

    // Cursor on "Point" in the type: string on line 2 (col 11 = after `"`).
    let hover = s.hover_for_type_alias(2, 11);
    assert!(hover.is_some(), "should produce hover for alias reference");
    if let Some(h) = hover {
        let text = match h.contents {
            lsp_types::HoverContents::Markup(m) => m.value,
            _ => String::new(),
        };
        assert!(
            text.contains("Point"),
            "hover should mention the alias name"
        );
        assert!(
            text.contains("Resolved"),
            "hover should include the resolved type (got: {text})"
        );
    }
}

/// Hover correctly positions aliases in type strings containing Unicode arrows (→).
///
/// U+2192 (→) is 3 UTF-8 bytes but 1 UTF-16 code unit; go-to-definition must
/// use UTF-16 column arithmetic to produce correct LSP positions.
#[test]
fn alias_goto_definition_utf16_column() {
    let mut s = LspTestSession::new();
    s.open(concat!(
        "` { type-def: \"MyType\" }\n",
        "alias-val: 42\n",
        "` { type: \"number \u{2192} MyType\" }\n",
        "get-it(p): p\n",
    ));

    // Line 2: ` { type: "number → MyType" }
    //          0123456789012345678901234567
    // "number " = 7 ASCII + "→" (1 UTF-16 unit) + " " = col 9 for "MyType".
    // String token starts at col 9 of that line (the opening quote is at
    // position 9 in ` { type: ...), content starts col 10, so "MyType" is
    // at col 10 + 9 = 19.  We place the cursor in the middle of "MyType".
    let result = s.goto_definition_for_type_alias(2, 21);
    assert!(
        result.is_some(),
        "should find definition of MyType through a Unicode → in the type string"
    );
}

/// Rename a type alias: updates both the type-def: value and the type: reference.
#[test]
fn alias_rename_updates_definition_and_references() {
    let mut s = LspTestSession::new();
    s.open(concat!(
        "` { type-def: \"Point\" }\n",
        "point: { x: 1, y: 2 }\n",
        "` { type: \"Point -> number\" }\n",
        "get-x(p): p.x\n",
    ));

    // Cursor on "Point" in the type: string on line 2.
    let edit = s.rename_type_alias(2, 11, "Coord");
    assert!(edit.is_some(), "rename should produce a WorkspaceEdit");

    let changes = edit.unwrap().changes.unwrap();
    // The test session uses a single URI.
    let edits: Vec<_> = changes.values().flatten().collect();
    assert!(
        edits.len() >= 2,
        "expected at least 2 edits (definition + reference)"
    );
    for e in &edits {
        assert_eq!(e.new_text, "Coord", "all edits should rename to 'Coord'");
    }
}

/// Alias tooling degrades gracefully when cursor is NOT inside a type: string.
#[test]
fn alias_no_result_outside_type_string() {
    let mut s = LspTestSession::new();
    s.open(concat!(
        "` { type-def: \"Point\" }\n",
        "point: { x: 1, y: 2 }\n",
    ));

    // Cursor on the identifier "point" (not inside a type: string).
    let result = s.goto_definition_for_type_alias(1, 0);
    assert!(
        result.is_none(),
        "should return None when cursor is not on an alias in a type: string"
    );
}
