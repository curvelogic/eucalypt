//! EDN export

use super::table::{AsKey, FromPairs, FromPrimitive, FromVec, TableAccumulator};
use crate::eval::emit::{Emitter, Event, RenderMetadata};
use crate::eval::primitive::Primitive;
use edn_format::{emit_str, Keyword, Value};
use ordered_float::OrderedFloat;
use std::io::Write;

impl AsKey<Value> for Value {
    fn as_key(&self) -> Value {
        self.clone()
    }
}

impl FromPrimitive for Value {
    fn from_primitive(_metadata: RenderMetadata, primitive: &Primitive) -> Self {
        match primitive {
            Primitive::Null => Value::Nil,
            Primitive::Bool(b) => Value::Boolean(*b),
            Primitive::Sym(s) => Value::Keyword(Keyword::from_name(s)),
            Primitive::Str(s) => Value::String(s.clone()),
            Primitive::Num(n) => {
                if let Some(i) = n.as_i64() {
                    Value::Integer(i)
                } else if let Some(f) = n.as_f64() {
                    Value::Float(OrderedFloat(f))
                } else {
                    panic!("unrepresentable number")
                }
            }
            Primitive::ZonedDateTime(dt) => Value::Inst(*dt),
        }
    }
}

impl FromVec<Value> for Value {
    fn from_vec(_metadata: RenderMetadata, slice: Vec<Value>) -> Self {
        Value::Vector(slice)
    }
}

impl FromPairs<Value, Value> for Value {
    fn from_pairs(_metadata: RenderMetadata, pairs: Vec<(Value, Value)>) -> Self {
        Value::Map(pairs.into_iter().collect())
    }
}

/// Emitter for EDN
pub struct EdnEmitter<'a> {
    accum: TableAccumulator<Value, Value>,
    out: &'a mut (dyn Write + 'a),
}

impl<'a> EdnEmitter<'a> {
    pub fn new(out: &'a mut (dyn Write + 'a)) -> Self {
        EdnEmitter {
            accum: Default::default(),
            out,
        }
    }
}

impl Emitter for EdnEmitter<'_> {
    fn emit(&mut self, event: Event) {
        self.accum.consume(event);
        if let Some(result) = self.accum.result() {
            writeln!(self.out, "{}", emit_str(result)).expect("failed to write EDN output");
        }
    }
}
