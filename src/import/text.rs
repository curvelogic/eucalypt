//! Import lines from text
use crate::common::sourcemap::SourceMap;
use crate::core::expr::*;
use crate::import::error::SourceError;

/// Read text into core syntax data as list of strings representing
/// the lines
pub fn read_text<'src>(
    _source_map: &'src mut SourceMap,
    _file_id: usize,
    text: &'src str,
) -> Result<RcExpr, SourceError> {
    Ok(acore::list(text.lines().map(acore::str).collect()))
}
