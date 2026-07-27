//! TOML export

use crate::eval::emit::{Emitter, Event, RenderMetadata};
use crate::eval::primitive::Primitive;
use std::io::Write;

use std::str::FromStr;
use toml::{value::Datetime, Value};

use super::table::{AsKey, FromPairs, FromPrimitive, FromVec, TableAccumulator};

/// Describe why `n` cannot be carried as a TOML integer, or `None` if it can.
///
/// TOML integers are signed 64-bit by specification, so a JSON-sourced `u64`
/// above `i64::MAX` has no TOML integer representation. This is the single
/// predicate behind both `TomlEmitter::unrepresentable` (which rejects such a
/// value at the emit intrinsic, with a source location) and the fallback in
/// `from_primitive` below, so the two can never disagree.
fn toml_integer_overflow(n: &serde_json::Number) -> Option<String> {
    match n.as_u64() {
        Some(u) if !n.is_i64() => Some(format!(
            "the integer {u} is above {}, the largest integer a TOML integer \
             can carry",
            i64::MAX
        )),
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
                Value::Datetime(Datetime::from_str(dt.to_rfc3339().as_str()).unwrap())
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

    fn unrepresentable(&self, primitive: &Primitive) -> Option<String> {
        match primitive {
            Primitive::Num(n) => toml_integer_overflow(n),
            _ => None,
        }
    }

    fn emit(&mut self, event: Event) {
        self.accum.consume(event);
        if let Some(result) = self.accum.result() {
            writeln!(self.out, "{result}").expect("failed to write TOML output");
        }
    }
}
