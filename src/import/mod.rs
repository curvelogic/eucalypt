use codespan_reporting::files::SimpleFiles;

use crate::{common::sourcemap::SourceMap, core::expr::RcExpr};
use codespan_reporting::files::Files;

use self::error::SourceError;

pub mod csv;
pub mod edn;
pub mod error;
pub mod jsonl;
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
    match format {
        "yaml" | "json" => {
            let text = files.source(file_id)?.to_string();
            yaml::read_yaml(files, source_map, file_id, &text)
        }
        "jsonl" => {
            let text = files.source(file_id)?.to_string();
            jsonl::read_jsonl(files, source_map, file_id, &text)
        }
        "toml" => toml::read_toml(source_map, file_id, files.source(file_id)?),
        "text" => text::read_text(source_map, file_id, files.source(file_id)?),
        "csv" => csv::read_csv(source_map, file_id, files.source(file_id)?),
        "xml" => xml::read_xml(source_map, file_id, files.source(file_id)?),
        "edn" => edn::read_edn(source_map, file_id, files.source(file_id)?),
        _ => Err(SourceError::UnknownSourceFormat(
            format.to_string(),
            file_id,
        )),
    }
}

/// Returns true if the format is a streaming format.
pub fn is_stream_format(format: &str) -> bool {
    matches!(format, "jsonl-stream" | "csv-stream" | "text-stream")
}

/// Create a streaming import by registering a producer and returning
/// a core expression that calls `STREAM_NEXT(handle)`.
///
/// The `path` must be a resolved filesystem path (or "-" for stdin
/// with text-stream).
pub fn create_stream_import(format: &str, path: &str) -> Result<RcExpr, SourceError> {
    use crate::core::expr::acore;
    use crate::eval::stg::stream::register_stream;

    let handle = match format {
        "jsonl-stream" => {
            let producer = stream::JsonlProducer::open(path).map_err(|e| {
                SourceError::InvalidSource(format!("cannot open for streaming: {e}"), 0)
            })?;
            register_stream(Box::new(producer))
        }
        "csv-stream" => {
            let producer = stream::CsvProducer::open(path).map_err(|e| {
                SourceError::InvalidSource(format!("cannot open for streaming: {e}"), 0)
            })?;
            register_stream(Box::new(producer))
        }
        "text-stream" => {
            let producer = stream::TextProducer::open(path).map_err(|e| {
                SourceError::InvalidSource(format!("cannot open for streaming: {e}"), 0)
            })?;
            register_stream(Box::new(producer))
        }
        _ => return Err(SourceError::UnknownSourceFormat(format.to_string(), 0)),
    };

    // Return: STREAM_NEXT(handle)
    Ok(acore::app(
        acore::bif("STREAM_NEXT"),
        vec![acore::num(handle as i64)],
    ))
}
