//! Import CSV lines as blocks from (assuming header row)
use crate::common::sourcemap::SourceMap;
use crate::core::expr::*;
use crate::import::error::SourceError;
use std::io::BufReader;

/// Read a CSV file into a list of lists
pub fn read_csv<'src>(
    _source_map: &'src mut SourceMap,
    file_id: usize,
    text: &'src str,
) -> Result<RcExpr, SourceError> {
    let text_reader = BufReader::new(text.as_bytes());
    let mut rdr = csv::ReaderBuilder::new()
        .has_headers(true)
        .from_reader(text_reader);

    let headers = rdr
        .headers()
        .map_err(|e| SourceError::InvalidSource(format!("{}", e), file_id))?
        .clone();

    let rows = rdr
        .records()
        .map(|r| match r {
            Err(e) => Err(SourceError::InvalidSource(format!("{}", e), file_id)),
            Ok(rec) => Ok(convert_string_record(&headers, rec)),
        })
        .collect::<Result<Vec<RcExpr>, SourceError>>()?;

    Ok(acore::list(rows.into_iter().collect()))
}

/// Convert a CSV line into a core list
fn convert_string_record(headers: &csv::StringRecord, record: csv::StringRecord) -> RcExpr {
    acore::block(
        headers
            .iter()
            .zip(record.iter())
            .map(|(h, r)| (h.to_string(), acore::str(r))),
    )
}
