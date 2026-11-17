//! EDN import

use std::iter;

use super::error::SourceError;
use crate::{
    common::sourcemap::SourceMap,
    core::expr::{acore, RcExpr},
};
use codespan::Span;
use edn_format::{parse_str, Value};
use serde_json::Number;

/// Read EDN format data
///
/// Note that EDN files may have many top-level items, until we
/// support streaming, we need to read on and return as a list.
///
/// When `data_only` is `true`, `#inst` datetime values are returned as plain
/// strings rather than `ZDT.PARSE` applications. This must be set when parsing
/// untrusted input.
pub fn read_edn<'smap>(
    _source_map: &'smap mut SourceMap,
    file_id: usize,
    text: &'smap str,
    data_only: bool,
) -> Result<RcExpr, SourceError> {
    let edn = parse_str(text).map_err(|e| SourceError::InvalidEdn(Box::new(e), file_id))?;
    value_to_core(&edn, file_id, data_only)
}

fn value_to_key(edn: &Value, file_id: usize) -> Result<String, SourceError> {
    match edn {
        Value::Character(c) => Ok(c.to_string()),
        Value::String(s) => Ok(s.to_string()),
        Value::Symbol(sym) => Ok(sym.name().to_string()),
        Value::Keyword(kw) => Ok(kw.name().to_string()),
        v => Err(SourceError::InvalidBlockKey(
            v.to_string(),
            file_id,
            Span::default(),
        )),
    }
}

fn value_to_core(edn: &Value, file_id: usize, data_only: bool) -> Result<RcExpr, SourceError> {
    match edn {
        Value::Nil => Ok(acore::null()),
        Value::Boolean(b) => Ok(acore::bool_(*b)),
        Value::Character(c) => Ok(acore::str(c.to_string())),
        Value::String(s) => Ok(acore::str(s)),
        Value::Symbol(sym) => Ok(acore::sym(sym.name())),
        Value::Keyword(kw) => Ok(acore::sym(kw.name())),
        Value::Integer(i) => Ok(acore::num(*i)),
        Value::Float(f) => match Number::from_f64(f.into_inner()) {
            Some(n) => Ok(acore::num(n)),
            None => Err(SourceError::InvalidNumber(
                f.to_string(),
                file_id,
                Span::default(),
            )),
        },
        // The EDN exporter writes integers above i64::MAX as BigInt (`N`
        // suffix) to keep their exact digits, so read back every BigInt
        // that a eucalypt number can hold exactly. Anything wider is still
        // rejected: rounding it to a float would silently change the value.
        Value::BigInt(i) => {
            if let Ok(n) = i64::try_from(i) {
                Ok(acore::num(n))
            } else if let Ok(n) = u64::try_from(i) {
                Ok(acore::num(n))
            } else {
                Err(SourceError::InvalidNumber(
                    i.to_string(),
                    file_id,
                    Span::default(),
                ))
            }
        }
        Value::BigDec(d) => Err(SourceError::InvalidNumber(
            d.to_string(),
            file_id,
            Span::default(),
        )),
        Value::List(xs) => xs
            .iter()
            .map(|v| value_to_core(v, file_id, data_only))
            .collect::<Result<Vec<RcExpr>, SourceError>>()
            .map(acore::list),
        Value::Vector(xs) => xs
            .iter()
            .map(|v| value_to_core(v, file_id, data_only))
            .collect::<Result<Vec<RcExpr>, SourceError>>()
            .map(acore::list),
        Value::Map(m) => m
            .iter()
            .map(|(k, v)| {
                match (
                    value_to_key(k, file_id),
                    value_to_core(v, file_id, data_only),
                ) {
                    (Ok(k), Ok(v)) => Ok((k, v)),
                    (Err(e), _) => Err(e),
                    (_, Err(e)) => Err(e),
                }
            })
            .collect::<Result<Vec<(String, RcExpr)>, SourceError>>()
            .map(acore::block),
        Value::Set(xs) => xs
            .iter()
            .map(|v| value_to_core(v, file_id, data_only))
            .collect::<Result<Vec<RcExpr>, SourceError>>()
            .map(acore::list),
        Value::Inst(dt) => {
            if data_only {
                Ok(acore::str(dt.to_string()))
            } else {
                Ok(acore::app(
                    acore::bif("ZDT.PARSE"),
                    vec![acore::str(dt.to_string())],
                ))
            }
        }
        Value::Uuid(uuid) => Ok(acore::meta(
            acore::str(uuid.to_string()),
            acore::block(iter::once(("tag".to_string(), acore::str("uuid")))),
        )),
        Value::TaggedElement(t, e) => Ok(acore::meta(
            value_to_core(e, file_id, data_only)?,
            acore::block(iter::once(("tag".to_string(), acore::str(t.name())))),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::expr::tests::smid_strip;

    fn read(text: &str) -> Result<RcExpr, SourceError> {
        let mut sm = SourceMap::new();
        read_edn(&mut sm, 0, text, true).map(smid_strip)
    }

    #[test]
    fn bigint_above_i64_max_reads_back_exactly() {
        // The form the EDN exporter emits for an integer above i64::MAX.
        assert_eq!(
            read("9999999999999999999N").unwrap(),
            acore::num(9_999_999_999_999_999_999u64)
        );
        assert_eq!(read("18446744073709551615N").unwrap(), acore::num(u64::MAX));
    }

    #[test]
    fn bigint_within_i64_reads_as_ordinary_integer() {
        assert_eq!(read("5N").unwrap(), acore::num(5));
        assert_eq!(read("-5N").unwrap(), acore::num(-5));
    }

    #[test]
    fn bigint_beyond_number_range_is_rejected() {
        // No eucalypt number holds these exactly; rounding would corrupt.
        assert!(matches!(
            read("18446744073709551616N"),
            Err(SourceError::InvalidNumber(..))
        ));
        assert!(matches!(
            read("-9223372036854775809N"),
            Err(SourceError::InvalidNumber(..))
        ));
    }
}
