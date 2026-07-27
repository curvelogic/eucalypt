//! TOML export

use crate::eval::emit::{Emitter, Event, Rejection, RenderMetadata};
use crate::eval::primitive::Primitive;
use std::io::Write;

use std::str::FromStr;
use toml::{value::Datetime, Value};

use super::error::RenderError;
use super::table::{AsKey, FromPairs, FromPrimitive, FromVec, TableAccumulator};
use super::INTEGER_RANGE_NOTES;

/// Describe why `n` cannot be carried as a TOML integer, or `None` if it can.
///
/// TOML integers are signed 64-bit by specification, so a JSON-sourced `u64`
/// above `i64::MAX` has no TOML integer representation. This is the single
/// predicate behind both `TomlEmitter::unrepresentable` (which rejects such a
/// value at the emit intrinsic, with a source location) and the fallback in
/// `from_primitive` below, so the two can never disagree.
fn toml_integer_overflow(n: &serde_json::Number) -> Option<Rejection> {
    match n.as_u64() {
        Some(u) if !n.is_i64() => Some(
            Rejection::new(format!(
                "the integer {u} is above {}, the largest integer a TOML \
                 integer can carry",
                i64::MAX
            ))
            .with_notes(INTEGER_RANGE_NOTES),
        ),
        _ => None,
    }
}

/// Describe why `primitive` cannot be carried in TOML, or `None` if it can.
///
/// TOML has no null: the exporter used to render one as an empty string, so
/// `{ a: null b: "" }` came out as two identical `""` values that no reader
/// could tell apart, and neither could be distinguished from a genuine
/// empty string on re-import. Reporting it is consistent with the treatment
/// of out-of-range integers (eu-1tkk.7.20) and with the project's position
/// on silent export data loss (eu-odkp): the caller decides what a null
/// should become, rather than the exporter guessing (eu-1tkk.7.28).
fn toml_unrepresentable(primitive: &Primitive) -> Option<Rejection> {
    match primitive {
        Primitive::Null => Some(
            Rejection::new("TOML has no null; every key must have a value").with_notes([
                "give the key a value the format can carry, or drop it from \
                 the block before rendering",
                "'yaml' and 'json' output both have a null, and 'edn' renders \
                 it as 'nil'",
            ]),
        ),
        Primitive::Num(n) => toml_integer_overflow(n),
        _ => None,
    }
}

impl AsKey<String> for Value {
    fn as_key(&self) -> String {
        self.as_str().unwrap().to_string()
    }
}

impl FromPrimitive for Value {
    fn from_primitive(_metadata: RenderMetadata, primitive: &Primitive) -> Self {
        match primitive {
            // Rejected by `unrepresentable` before reaching here. The empty
            // string is retained only as a can't-happen fallback; it is not
            // a representation of null (eu-1tkk.7.28).
            Primitive::Null => Value::String("".to_string()),
            Primitive::Bool(b) => Value::Boolean(*b),
            Primitive::Sym(s) => Value::String(s.clone()),
            Primitive::Str(s) => Value::String(s.clone()),
            Primitive::Num(n) => {
                if toml_integer_overflow(n).is_some() {
                    // Unreachable through the emit intrinsics, which reject
                    // this value via `unrepresentable` before it gets here.
                    // Should another route reach it, keep the exact digits
                    // rather than rounding to a float or aborting the whole
                    // process (eu-1tkk.7.20).
                    Value::String(n.to_string())
                } else if let Some(i) = n.as_i64() {
                    Value::Integer(i)
                } else if let Some(f) = n.as_f64() {
                    Value::Float(f)
                } else {
                    Value::String(n.to_string())
                }
            }
            Primitive::ZonedDateTime(dt) => {
                let rfc3339 = dt.to_rfc3339();
                // `to_rfc3339` always produces a form TOML's Datetime parses,
                // so the fallback is unreachable; carry the timestamp as a
                // string rather than aborting if that ever stops holding.
                match Datetime::from_str(rfc3339.as_str()) {
                    Ok(d) => Value::Datetime(d),
                    Err(_) => Value::String(rfc3339),
                }
            }
        }
    }
}

impl FromVec<Value> for Value {
    fn from_vec(_metadata: RenderMetadata, slice: Vec<Value>) -> Self {
        Value::Array(slice)
    }
}

impl FromPairs<String, Value> for Value {
    fn from_pairs(_metadata: RenderMetadata, pairs: Vec<(String, Value)>) -> Self {
        Value::Table(pairs.into_iter().collect())
    }
}

/// Currently basic TOML emitter (no tags yet) for bootstrapping
pub struct TomlEmitter<'a> {
    accum: TableAccumulator<String, Value>,
    out: &'a mut (dyn Write + 'a),
}

impl<'a> TomlEmitter<'a> {
    pub fn new(out: &'a mut (dyn Write + 'a)) -> Self {
        TomlEmitter {
            accum: Default::default(),
            out,
        }
    }
}

impl Emitter for TomlEmitter<'_> {
    fn format_name(&self) -> &'static str {
        "TOML"
    }

    fn unrepresentable(&self, primitive: &Primitive) -> Option<Rejection> {
        toml_unrepresentable(primitive)
    }

    fn emit(&mut self, event: Event) -> Result<(), RenderError> {
        self.accum.consume(event);
        if let Some(result) = self.accum.result() {
            writeln!(self.out, "{result}")?;
        }
        Ok(())
    }
}
