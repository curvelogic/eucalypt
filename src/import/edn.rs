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
pub fn read_edn<'smap>(
    _source_map: &'smap mut SourceMap,
    file_id: usize,
    text: &'smap str,
) -> Result<RcExpr, SourceError> {
    let edn = parse_str(text).map_err(|e| SourceError::InvalidEdn(e, file_id))?;
    value_to_core(&edn, file_id)
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

fn value_to_core(edn: &Value, file_id: usize) -> Result<RcExpr, SourceError> {
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
        Value::BigInt(i) => Err(SourceError::InvalidNumber(
            i.to_string(),
            file_id,
            Span::default(),
        )),
        Value::BigDec(d) => Err(SourceError::InvalidNumber(
            d.to_string(),
            file_id,
            Span::default(),
        )),
        Value::List(xs) => xs
            .iter()
            .map(|v| value_to_core(v, file_id))
            .collect::<Result<Vec<RcExpr>, SourceError>>()
            .map(acore::list),
        Value::Vector(xs) => xs
            .iter()
            .map(|v| value_to_core(v, file_id))
            .collect::<Result<Vec<RcExpr>, SourceError>>()
            .map(acore::list),
        Value::Map(m) => m
            .iter()
            .map(
                |(k, v)| match (value_to_key(k, file_id), value_to_core(v, file_id)) {
                    (Ok(k), Ok(v)) => Ok((k, v)),
                    (Err(e), _) => Err(e),
                    (_, Err(e)) => Err(e),
                },
            )
            .collect::<Result<Vec<(String, RcExpr)>, SourceError>>()
            .map(|entries| acore::block(entries.into_iter())),
        Value::Set(xs) => xs
            .iter()
            .map(|v| value_to_core(v, file_id))
            .collect::<Result<Vec<RcExpr>, SourceError>>()
            .map(acore::list),
        Value::Inst(dt) => Ok(acore::app(
            acore::bif("ZDT.PARSE"),
            vec![acore::str(dt.to_string())],
        )), // NB. no core primitive for datetime right now
        Value::Uuid(uuid) => Ok(acore::meta(
            acore::str(uuid.to_string()),
            acore::block(iter::once((
                "tag".to_string(),
                acore::str("uuid"),
            ))),
        )),
        Value::TaggedElement(t, e) => Ok(acore::meta(
            value_to_core(e, file_id)?,
            acore::block(iter::once(("tag".to_string(), acore::str(t.name())))),
        )),
    }
}
