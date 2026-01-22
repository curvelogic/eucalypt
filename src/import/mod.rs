use codespan_reporting::files::SimpleFiles;

use crate::{common::sourcemap::SourceMap, core::expr::RcExpr};
use codespan_reporting::files::Files;

use self::error::SourceError;

pub mod csv;
pub mod edn;
pub mod error;
pub mod jsonl;
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
