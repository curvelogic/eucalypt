//! TOML export

use crate::eval::emit::{Emitter, Event, RenderMetadata};
use crate::eval::primitive::Primitive;
use std::io::Write;

use std::str::FromStr;
use toml::{value::Datetime, Value};

use super::table::{AsKey, FromPairs, FromPrimitive, FromVec, TableAccumulator};

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
                if n.is_i64() {
                    Value::Integer(n.as_i64().unwrap())
                } else if n.is_f64() {
                    Value::Float(n.as_f64().unwrap())
                } else {
                    panic!("unrenderable number")
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
    fn emit(&mut self, event: Event) {
        self.accum.consume(event);
        if let Some(result) = self.accum.result() {
            writeln!(self.out, "{}", result).unwrap();
        }
    }
}
