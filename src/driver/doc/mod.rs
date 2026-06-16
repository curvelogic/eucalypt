//! `eu doc` — documentation and schema extraction from eucalypt source files.
//!
//! Runs the front-end pipeline (parse → desugar → cook → eliminate →
//! type-check) and extracts documentation, type annotations, deprecation
//! notices and visibility from backtick metadata.
//!
//! Outputs:
//! - Markdown documentation (`--format md`, default)
//! - JSON Schema (`--format json`)
//! - Coverage report (`--check`)

mod extract;
mod prelude;
mod render;

use std::fs;
use std::path::Path;

use crate::driver::error::EucalyptError;
use crate::driver::options::{DocFormat, EucalyptOptions};
use crate::driver::resources::Resources;
use crate::syntax::rowan::parse_unit;

// Re-export the public API so external callers (eu.rs, driver/mod.rs)
// can continue using `doc::DocEntry`, `doc::render_markdown`, etc.
pub use extract::{extract_doc_entries, DeprecationInfo, DocEntry, DocKind, DocVisibility};
pub use prelude::render_prelude_multifile;
pub use render::{compute_coverage, render_json_schema, render_markdown, Coverage};

// ── Entry point ───────────────────────────────────────────────────────────────

/// Run `eu doc` — extract and render documentation from eucalypt source.
pub fn doc(opt: &EucalyptOptions) -> Result<i32, EucalyptError> {
    if opt.doc_prelude() {
        return doc_prelude(opt);
    }

    let inputs = opt.explicit_inputs();
    if inputs.is_empty() {
        eprintln!("eu doc: no files specified (use --prelude for the prelude reference)");
        return Ok(1);
    }

    let mut exit = 0;
    for input in inputs {
        use crate::syntax::input::Locator;
        let path_str = match input.locator() {
            Locator::Fs(p) => p.to_string_lossy().into_owned(),
            other => {
                eprintln!("eu doc: unsupported input locator {other:?}");
                exit = 1;
                continue;
            }
        };
        let e = doc_file(Path::new(&path_str), opt)?;
        if e != 0 {
            exit = e;
        }
    }
    Ok(exit)
}

/// Process a single file.
fn doc_file(path: &Path, opt: &EucalyptOptions) -> Result<i32, EucalyptError> {
    let source = fs::read_to_string(path).map_err(|e| {
        EucalyptError::FileCouldNotBeRead(path.display().to_string(), Some(e.to_string()))
    })?;

    let (entries, unit_doc) = extract_from_source(&source);

    let title = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("Documentation");

    render_and_output(&entries, title, unit_doc.as_deref(), opt)
}

/// Process the embedded prelude.
fn doc_prelude(opt: &EucalyptOptions) -> Result<i32, EucalyptError> {
    let source = Resources::default()
        .get("prelude")
        .cloned()
        .ok_or_else(|| EucalyptError::UnknownResource("prelude".to_string()))?;

    let (entries, unit_doc) = extract_from_source(&source);

    // Multi-file output when --output-dir is given
    if let Some(output_dir) = opt.doc_output_dir() {
        // Locate supplements directory: <output-dir>/supplements/ if it exists,
        // otherwise a sibling supplements/ directory relative to output-dir.
        let supplements_dir = {
            let sibling = output_dir.join("supplements");
            if sibling.exists() {
                sibling
            } else if let Some(parent) = output_dir.parent() {
                parent.join("supplements")
            } else {
                output_dir.join("supplements")
            }
        };
        return render_prelude_multifile(
            &entries,
            output_dir,
            &supplements_dir,
            unit_doc.as_deref(),
        );
    }

    render_and_output(&entries, "Prelude Reference", unit_doc.as_deref(), opt)
}

/// Parse source and extract doc entries plus the unit-level doc string.
fn extract_from_source(source: &str) -> (Vec<DocEntry>, Option<String>) {
    let parse_result = parse_unit(source);
    let unit = parse_result.tree();
    let entries = extract_doc_entries(&unit);
    let unit_doc = extract::extract_unit_doc(&unit);
    (entries, unit_doc)
}

/// Render and output documentation in the requested format.
fn render_and_output(
    entries: &[DocEntry],
    title: &str,
    unit_doc: Option<&str>,
    opt: &EucalyptOptions,
) -> Result<i32, EucalyptError> {
    if opt.doc_coverage_check() {
        return render_coverage(entries, title);
    }

    let output = match opt.doc_format() {
        DocFormat::Markdown => render_markdown(entries, title, unit_doc),
        DocFormat::Json => render_json_schema(entries, title),
    };

    println!("{output}");
    Ok(0)
}

/// Print a coverage report and return exit code 0 (always, it's informational).
fn render_coverage(entries: &[DocEntry], title: &str) -> Result<i32, EucalyptError> {
    let cov = compute_coverage(entries);
    let pct = if cov.total == 0 {
        100.0
    } else {
        cov.documented as f64 / cov.total as f64 * 100.0
    };

    println!("# Documentation coverage: {title}");
    println!();
    println!(
        "{}/{} public bindings documented ({:.0}%)",
        cov.documented, cov.total, pct
    );

    if !cov.undocumented_names.is_empty() {
        println!();
        println!("Undocumented:");
        for name in &cov.undocumented_names {
            println!("  - {name}");
        }
    }

    Ok(0)
}
