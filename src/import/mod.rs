use codespan_reporting::files::SimpleFiles;

use crate::{common::sourcemap::SourceMap, core::expr::RcExpr};
use codespan_reporting::files::Files;

use self::error::SourceError;

pub mod csv;
pub mod edn;
pub mod error;
pub mod jsonl;
#[cfg(not(target_arch = "wasm32"))]
pub mod stream;
pub mod text;
pub mod toml;
pub mod xml;
pub mod yaml;

/// Read a supported source format into core representation
pub fn read_to_core<'smap>(
    format: &str,
    files: &'smap mut SimpleFiles<String, String>,
    source_map: &'smap mut SourceMap,
    file_id: usize,
) -> Result<RcExpr, SourceError> {
    read_to_core_impl(format, files, source_map, file_id, false)
}

/// Read a supported source format into core representation in data-only mode.
///
/// In data-only mode all code-execution paths are suppressed: YAML `!eu` tags
/// produce plain string literals and timestamps are returned as strings rather
/// than `ZDT.PARSE` applications. This must be used when parsing untrusted
/// input such as shell command output.
pub fn read_to_core_data_only<'smap>(
    format: &str,
    files: &'smap mut SimpleFiles<String, String>,
    source_map: &'smap mut SourceMap,
    file_id: usize,
) -> Result<RcExpr, SourceError> {
    read_to_core_impl(format, files, source_map, file_id, true)
}

fn read_to_core_impl<'smap>(
    format: &str,
    files: &'smap mut SimpleFiles<String, String>,
    source_map: &'smap mut SourceMap,
    file_id: usize,
    data_only: bool,
) -> Result<RcExpr, SourceError> {
    match format {
        "yaml" | "json" => {
            let text = files.source(file_id)?.to_string();
            yaml::read_yaml(files, source_map, file_id, &text, data_only)
        }
        "jsonl" => {
            let text = files.source(file_id)?.to_string();
            jsonl::read_jsonl(files, source_map, file_id, &text)
        }
        "toml" => toml::read_toml(source_map, file_id, files.source(file_id)?, data_only),
        "text" => text::read_text(source_map, file_id, files.source(file_id)?),
        "csv" => csv::read_csv(source_map, file_id, files.source(file_id)?),
        "xml" => xml::read_xml(source_map, file_id, files.source(file_id)?),
        "edn" => edn::read_edn(source_map, file_id, files.source(file_id)?, data_only),
        _ => Err(SourceError::UnknownSourceFormat(
            format.to_string(),
            file_id,
        )),
    }
}

/// Returns true if the format is a streaming format.
#[cfg(not(target_arch = "wasm32"))]
pub fn is_stream_format(format: &str) -> bool {
    matches!(format, "jsonl-stream" | "csv-stream" | "text-stream")
}

/// Streaming imports are not supported on WASM.
#[cfg(target_arch = "wasm32")]
pub fn is_stream_format(_format: &str) -> bool {
    false
}

/// Create a streaming import by registering a producer and returning
/// a core expression that calls `PRODUCER_NEXT(handle)`.
///
/// The `path` must be a resolved filesystem path (or "-" for stdin
/// with text-stream).
#[cfg(not(target_arch = "wasm32"))]
pub fn create_stream_import(format: &str, path: &str) -> Result<RcExpr, SourceError> {
    use crate::core::expr::acore;
    use crate::eval::stg::stream::register_producer;

    let handle = match format {
        "jsonl-stream" => {
            let producer = stream::JsonlProducer::open(path).map_err(|e| {
                SourceError::InvalidSource(format!("cannot open for streaming: {e}"), 0)
            })?;
            register_producer(Box::new(producer))
        }
        "csv-stream" => {
            let producer = stream::CsvProducer::open(path).map_err(|e| {
                SourceError::InvalidSource(format!("cannot open for streaming: {e}"), 0)
            })?;
            register_producer(Box::new(producer))
        }
        "text-stream" => {
            let producer = stream::TextProducer::open(path).map_err(|e| {
                SourceError::InvalidSource(format!("cannot open for streaming: {e}"), 0)
            })?;
            register_producer(Box::new(producer))
        }
        _ => return Err(SourceError::UnknownSourceFormat(format.to_string(), 0)),
    };

    // Return: PRODUCER_NEXT(handle)
    Ok(acore::app(
        acore::bif("PRODUCER_NEXT"),
        vec![acore::num(handle as i64)],
    ))
}
