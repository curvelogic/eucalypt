//! Read TOML into core

use codespan::Span;

use serde_json::Number;

use crate::{
    common::sourcemap::SourceMap,
    core::expr::{acore, RcExpr},
};

use super::error::SourceError;
use toml::Value;

/// Read the TOML into a core expression
///
/// Use `source_map` to create new SMIDs and `file_id` for error reporting.
///
/// When `data_only` is `true`, datetime values are returned as plain strings
/// rather than `ZDT.PARSE` applications. This must be set when parsing
/// untrusted input.
pub fn read_toml<'smap>(
    _source_map: &'smap mut SourceMap,
    file_id: usize,
    text: &'smap str,
    data_only: bool,
) -> Result<RcExpr, SourceError> {
    let value = text
        .parse::<Value>()
        .map_err(|e| SourceError::InvalidToml(e.to_string(), file_id))?;
    to_core(&value, file_id, data_only)
}

/// Convert a toml Value into a core expression
fn to_core(value: &Value, file_id: usize, data_only: bool) -> Result<RcExpr, SourceError> {
    match value {
        Value::String(s) => Ok(acore::str(s)),
        Value::Integer(n) => Ok(acore::num(*n)),
        Value::Float(n) => Number::from_f64(*n)
            .map(acore::num)
            .ok_or_else(|| SourceError::InvalidNumber(n.to_string(), file_id, Span::default())),
        Value::Boolean(b) => Ok(acore::bool_(*b)),
        Value::Datetime(d) => {
            if data_only {
                Ok(acore::str(d.to_string()))
            } else {
                Ok(acore::app(
                    acore::bif("ZDT.PARSE"),
                    vec![acore::str(d.to_string())],
                ))
            }
        }
        Value::Array(arr) => arr
            .iter()
            .map(|v| to_core(v, file_id, data_only))
            .collect::<Result<Vec<RcExpr>, SourceError>>()
            .map(acore::list),
        Value::Table(t) => t
            .iter()
            .map(|(k, v)| to_core(v, file_id, data_only).map(|expr| (k.clone(), expr)))
            .collect::<Result<Vec<(String, RcExpr)>, SourceError>>()
            .map(acore::block),
    }
}
