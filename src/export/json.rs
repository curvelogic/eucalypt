//! JSON export

use crate::eval::emit::{Emitter, Event, RenderMetadata};
use crate::eval::primitive::Primitive;
use std::io::Write;

use super::table::{AsKey, FromPairs, FromPrimitive, FromVec, TableAccumulator};

impl AsKey<String> for serde_json::Value {
    fn as_key(&self) -> String {
        self.as_str().unwrap().to_string()
    }
}

impl FromPrimitive for serde_json::Value {
    fn from_primitive(_metadata: RenderMetadata, primitive: &Primitive) -> Self {
        match primitive {
            Primitive::Null => serde_json::Value::Null,
            Primitive::Bool(b) => serde_json::Value::Bool(*b),
            Primitive::Sym(s) => serde_json::Value::String(s.clone()),
            Primitive::Str(s) => serde_json::Value::String(s.clone()),
            Primitive::Num(n) => serde_json::Value::Number(n.clone()),
            Primitive::ZonedDateTime(dt) => serde_json::Value::String(format!("{dt}")),
        }
    }
}

impl FromVec<serde_json::Value> for serde_json::Value {
    fn from_vec(_metadata: RenderMetadata, slice: Vec<serde_json::Value>) -> Self {
        serde_json::Value::Array(slice)
    }
}

impl FromPairs<String, serde_json::Value> for serde_json::Value {
    fn from_pairs(_metadata: RenderMetadata, pairs: Vec<(String, serde_json::Value)>) -> Self {
        serde_json::Value::Object(pairs.into_iter().collect())
    }
}

/// Currently basic JSON emitter (no tags yet) for bootstrapping
pub struct JsonEmitter<'a> {
    accum: TableAccumulator<String, serde_json::Value>,
    out: &'a mut (dyn Write + 'a),
}

impl<'a> JsonEmitter<'a> {
    pub fn new(out: &'a mut (dyn Write + 'a)) -> Self {
        JsonEmitter {
            accum: Default::default(),
            out,
        }
    }
}

impl Emitter for JsonEmitter<'_> {
    fn emit(&mut self, event: Event) {
        self.accum.consume(event);
        if let Some(result) = self.accum.result() {
            writeln!(
                self.out,
                "{}",
                serde_json::to_string_pretty(&result).expect("failed to serialise JSON")
            )
            .expect("failed to write JSON output");
        }
    }
}
