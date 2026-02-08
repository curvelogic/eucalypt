//! Streaming import producers for JSONL, CSV, and text formats.
//!
//! Each producer wraps a file reader and yields one STG value per
//! call to `next()`, enabling lazy line-by-line processing of large
//! files.

use std::fs::File;
use std::io::{BufRead, BufReader};
use std::rc::Rc;

use crate::eval::stg::json_to_stg::json_to_stg;
use crate::eval::stg::stream::StreamProducer;
use crate::eval::stg::syntax::dsl;
use crate::eval::stg::syntax::StgSyn;

/// Streams JSON Lines (JSONL) files, yielding one parsed JSON value
/// per line.
pub struct JsonlProducer {
    reader: BufReader<File>,
    line_buf: String,
}

impl JsonlProducer {
    /// Create a new JSONL producer from a file path.
    pub fn open(path: &str) -> std::io::Result<Self> {
        let file = File::open(path)?;
        Ok(JsonlProducer {
            reader: BufReader::new(file),
            line_buf: String::new(),
        })
    }
}

impl StreamProducer for JsonlProducer {
    fn next(&mut self) -> Option<Rc<StgSyn>> {
        loop {
            self.line_buf.clear();
            match self.reader.read_line(&mut self.line_buf) {
                Ok(0) => return None, // EOF
                Ok(_) => {
                    let trimmed = self.line_buf.trim();
                    if trimmed.is_empty() {
                        continue; // skip blank lines
                    }
                    let value: serde_json::Value = serde_json::from_str(trimmed)
                        .unwrap_or_else(|e| panic!("malformed JSON on stream line: {e}"));
                    return Some(json_to_stg(&value));
                }
                Err(e) => panic!("IO error reading JSONL stream: {e}"),
            }
        }
    }
}

/// Streams CSV files, yielding one block per row with column-name keys.
pub struct CsvProducer {
    reader: csv::Reader<File>,
    headers: csv::StringRecord,
}

impl CsvProducer {
    /// Create a new CSV producer from a file path.
    pub fn open(path: &str) -> std::io::Result<Self> {
        let mut reader = csv::ReaderBuilder::new()
            .has_headers(true)
            .from_path(path)?;
        let headers = reader
            .headers()
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?
            .clone();
        Ok(CsvProducer { reader, headers })
    }
}

impl StreamProducer for CsvProducer {
    fn next(&mut self) -> Option<Rc<StgSyn>> {
        let record = match self.reader.records().next() {
            Some(Ok(rec)) => rec,
            Some(Err(e)) => panic!("CSV parse error on stream: {e}"),
            None => return None,
        };

        // Build a JSON object from headers + values, then convert to STG
        let mut map = serde_json::Map::new();
        for (header, value) in self.headers.iter().zip(record.iter()) {
            map.insert(
                header.to_string(),
                serde_json::Value::String(value.to_string()),
            );
        }
        Some(json_to_stg(&serde_json::Value::Object(map)))
    }
}

/// Streams text files line by line, yielding one string per line.
pub struct TextProducer {
    reader: BufReader<Box<dyn std::io::Read>>,
    line_buf: String,
}

impl TextProducer {
    /// Create a text producer from a file path, or stdin if path is "-".
    pub fn open(path: &str) -> std::io::Result<Self> {
        let reader: Box<dyn std::io::Read> = if path == "-" {
            Box::new(std::io::stdin())
        } else {
            Box::new(File::open(path)?)
        };
        Ok(TextProducer {
            reader: BufReader::new(reader),
            line_buf: String::new(),
        })
    }
}

impl StreamProducer for TextProducer {
    fn next(&mut self) -> Option<Rc<StgSyn>> {
        self.line_buf.clear();
        match self.reader.read_line(&mut self.line_buf) {
            Ok(0) => None, // EOF
            Ok(_) => {
                // Strip trailing newline
                let line = self.line_buf.trim_end_matches('\n').trim_end_matches('\r');
                Some(dsl::box_str(line))
            }
            Err(e) => panic!("IO error reading text stream: {e}"),
        }
    }
}
