//! WASM API for eucalypt — browser-facing evaluation interface.
//!
//! This module exposes a minimal JS API via wasm-bindgen:
//!
//! - `evaluate(source, format)` — evaluate eucalypt source and return JSON
//! - `evaluate_expr(source, format)` — the same, parsing the source as an
//!   expression (like CLI `-e`)
//! - `evaluate_with_options(source, format, options)` and
//!   `evaluate_expr_with_options(source, format, options)` — the same, with
//!   options given as a JSON string; every key is optional and an unknown
//!   key is an error:
//!
//!   ```json
//!   { "files": { "helpers.eu": "double(x): x * 2", "input.txt": "1\n2\n" },
//!     "max_steps": 5000000 }
//!   ```
//!
//!   `files` are files that `import` resolves against, by path, before the
//!   embedded libraries (`lens.eu`, `state.eu`, ...). An `.eu` file's own
//!   imports resolve as written, then relative to its directory. Other
//!   formats are read as data by extension or by an explicit format
//!   (`text@input.txt`). `max_steps` is the step limit (default
//!   1,000,000): a program that runs longer fails with a "did not
//!   terminate" error instead of hanging the tab.
//! - `formats()` — return a JSON array of supported output format names
//!
//! The actual evaluation pipeline lives in [`crate::wasm_pipeline`] so it can
//! be tested on native targets.  This module is a thin wrapper that converts
//! the pipeline result into the JSON envelope that JS callers expect.

use wasm_bindgen::prelude::*;

use crate::wasm_pipeline;

/// Result of evaluating eucalypt source, serialised as JSON for JS callers.
///
/// On success:
/// ```json
/// { "success": true, "output": "---\nhello: world\n" }
/// ```
/// On failure:
/// ```json
/// { "success": false, "error": { "message": "...", "location": { ... } } }
/// ```
/// A location names its `file`: `<input>` for the source evaluated, else
/// a supplied file's path or an embedded library's name.
#[derive(serde::Serialize)]
struct EvalResult {
    success: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    output: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<ErrorInfo>,
}

#[derive(serde::Serialize)]
struct ErrorInfo {
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<WasmSourceLocation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    notes: Option<Vec<String>>,
}

#[derive(serde::Serialize)]
struct WasmSourceLocation {
    file: String,
    line: usize,
    column: usize,
    end_line: usize,
    end_column: usize,
}

/// Evaluate a eucalypt unit (declarations) and return the result as JSON.
///
/// Input is parsed as a unit — the same as a `.eu` file.  This is the
/// default mode for the playground.
///
/// # Arguments
/// * `source` — eucalypt source code (declarations, like a `.eu` file)
/// * `format` — output format: `"yaml"`, `"json"`, `"toml"`, `"text"`, `"edn"`, `"html"`
///
/// # Returns
/// A JSON string containing an `EvalResult`.
#[wasm_bindgen]
pub fn evaluate(source: &str, format: &str) -> String {
    wrap_result(wasm_pipeline::evaluate_unit(source, format))
}

/// Evaluate a eucalypt expression and return the result as JSON.
///
/// Input is parsed as a bare expression — the same as CLI `-e`.
#[wasm_bindgen]
pub fn evaluate_expr(source: &str, format: &str) -> String {
    wrap_result(wasm_pipeline::evaluate_expr(source, format))
}

/// Evaluate a eucalypt unit with options (a JSON string: `files` for
/// imports and `max_steps`; see the module documentation) and return the
/// result as JSON. Invalid options give a failure result.
#[wasm_bindgen]
pub fn evaluate_with_options(source: &str, format: &str, options: &str) -> String {
    wrap_result(
        wasm_pipeline::EvalOptions::from_json(options)
            .and_then(|o| wasm_pipeline::evaluate_unit_with(source, format, &o)),
    )
}

/// Evaluate a eucalypt expression with options, as
/// [`evaluate_with_options`] does a unit.
#[wasm_bindgen]
pub fn evaluate_expr_with_options(source: &str, format: &str, options: &str) -> String {
    wrap_result(
        wasm_pipeline::EvalOptions::from_json(options)
            .and_then(|o| wasm_pipeline::evaluate_expr_with(source, format, &o)),
    )
}

fn wrap_result(outcome: Result<String, wasm_pipeline::PipelineError>) -> String {
    let result = match outcome {
        Ok(output) => EvalResult {
            success: true,
            output: Some(output),
            error: None,
        },
        Err(e) => EvalResult {
            success: false,
            output: None,
            error: Some(ErrorInfo {
                message: e.message,
                location: e.location.map(|loc| WasmSourceLocation {
                    file: loc.file,
                    line: loc.line,
                    column: loc.column,
                    end_line: loc.end_line,
                    end_column: loc.end_column,
                }),
                notes: e.notes,
            }),
        },
    };

    serde_json::to_string(&result).unwrap_or_else(|e| {
        format!(r#"{{"success":false,"error":{{"message":"serialisation error: {e}"}}}}"#)
    })
}

/// Return the list of supported output formats as a JSON array string.
#[wasm_bindgen]
pub fn formats() -> String {
    r#"["yaml","json","toml","text","edn","html"]"#.to_string()
}

#[cfg(test)]
#[cfg(target_arch = "wasm32")]
mod wasm_tests {
    use super::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_evaluate_unit() {
        let result = evaluate("x: 1", "json");
        let parsed: serde_json::Value =
            serde_json::from_str(&result).expect("evaluate should return valid JSON");
        assert_eq!(parsed["success"], true);
        let output = parsed["output"].as_str().expect("should have output");
        let output_parsed: serde_json::Value =
            serde_json::from_str(output.trim()).expect("output should be valid JSON");
        assert_eq!(output_parsed["x"], 1);
    }

    #[wasm_bindgen_test]
    fn test_evaluate_expr() {
        let result = evaluate_expr("{x: 1}", "json");
        let parsed: serde_json::Value =
            serde_json::from_str(&result).expect("evaluate_expr should return valid JSON");
        assert_eq!(parsed["success"], true);
        let output = parsed["output"].as_str().expect("should have output");
        let output_parsed: serde_json::Value =
            serde_json::from_str(output.trim()).expect("output should be valid JSON");
        assert_eq!(output_parsed["x"], 1);
    }

    #[wasm_bindgen_test]
    fn test_evaluate_parse_error() {
        let result = evaluate("{{{{", "json");
        let parsed: serde_json::Value =
            serde_json::from_str(&result).expect("evaluate should return valid JSON even on error");
        // Under resilient parsing the partial tree may evaluate (success: true)
        // or fail at a later stage (success: false).  Both are acceptable.
        // When success is false, parse errors must appear in the error message
        // or notes so the caller has enough context to diagnose the problem.
        if parsed["success"].as_bool() == Some(false) {
            let message = parsed["error"]["message"].as_str().unwrap_or("");
            let notes_contain_parse = parsed["error"]["notes"]
                .as_array()
                .map(|ns| {
                    ns.iter()
                        .any(|n| n.as_str().unwrap_or("").to_lowercase().contains("parse"))
                })
                .unwrap_or(false);
            assert!(
                message.to_lowercase().contains("parse") || notes_contain_parse,
                "on failure, parse errors must appear in message or notes; got: {parsed}"
            );
        }
    }

    /// An array shape that fits the heap's largest object (u32::MAX bytes)
    /// but not wasm32's `isize::MAX` is reported as too large, not a
    /// "capacity overflow" panic when ndarray allocates it.
    #[wasm_bindgen_test]
    fn test_oversized_array_is_an_error_not_a_panic() {
        let result = evaluate("x: arr.zeros[300000000]", "json");
        let parsed: serde_json::Value =
            serde_json::from_str(&result).expect("evaluate should return valid JSON");
        assert_eq!(parsed["success"], false, "got: {parsed}");
        let message = parsed["error"]["message"].as_str().unwrap_or("");
        assert!(
            message.contains("too many elements") && message.contains("268435455"),
            "expected the array-too-large error; got: {parsed}"
        );
    }

    #[wasm_bindgen_test]
    fn test_evaluate_with_options_imports_a_supplied_file() {
        let options = r#"{"files": {"helpers.eu": "double(x): x * 2"}, "max_steps": 5000000}"#;
        let result =
            evaluate_with_options(r#"{ import: "helpers.eu" } x: double(21)"#, "json", options);
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed["success"], true, "got: {parsed}");
        let output: serde_json::Value =
            serde_json::from_str(parsed["output"].as_str().unwrap().trim()).unwrap();
        assert_eq!(output["x"], 42);
    }

    #[wasm_bindgen_test]
    fn test_evaluate_expr_with_options_step_limit() {
        let result = evaluate_expr_with_options("1 + 2", "json", r#"{"max_steps": 5}"#);
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed["success"], false, "got: {parsed}");
        let message = parsed["error"]["message"].as_str().unwrap_or("");
        assert!(message.contains("after 5 steps"), "got: {parsed}");
    }

    #[wasm_bindgen_test]
    fn test_evaluate_with_options_reports_bad_options() {
        let result = evaluate_with_options("x: 1", "json", r#"{"maxSteps": 5}"#);
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed["success"], false, "got: {parsed}");
        let message = parsed["error"]["message"].as_str().unwrap_or("");
        assert!(message.starts_with("invalid options"), "got: {parsed}");
    }

    #[wasm_bindgen_test]
    fn test_formats() {
        let result = formats();
        let parsed: serde_json::Value =
            serde_json::from_str(&result).expect("formats should return valid JSON");
        assert!(parsed.is_array());
        let arr = parsed.as_array().unwrap();
        assert!(arr.contains(&serde_json::json!("json")));
        assert!(arr.contains(&serde_json::json!("yaml")));
    }
}
