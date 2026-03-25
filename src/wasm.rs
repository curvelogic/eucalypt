//! WASM API for eucalypt — browser-facing evaluation interface.
//!
//! This module exposes a minimal JS API via wasm-bindgen:
//!
//! - `evaluate(source, format)` — evaluate eucalypt source and return JSON
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
        assert_eq!(parsed["success"], false);
        assert!(parsed["error"]["message"]
            .as_str()
            .unwrap()
            .contains("Parse error"));
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
