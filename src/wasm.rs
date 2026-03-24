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

/// Evaluate eucalypt source code and return the result as a JSON string.
///
/// # Arguments
/// * `source` — eucalypt source code
/// * `format` — output format: `"yaml"`, `"json"`, `"toml"`, `"text"`, `"edn"`, `"html"`
///
/// # Returns
/// A JSON string containing an `EvalResult`.
#[wasm_bindgen]
pub fn evaluate(source: &str, format: &str) -> String {
    let result = match wasm_pipeline::evaluate_pipeline(source, format) {
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
