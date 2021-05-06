//! JSON export

use crate::eval::emit::{Emitter, Event};
use crate::eval::primitive::Primitive;
use std::io::Write;
use std::iter;

/// Currently basic JSON emitter (no tags yet) for bootstrapping
pub struct JsonEmitter<'a> {
    stack: Vec<Expectation>,
    out: &'a mut (dyn Write + 'a),
}

/// Represent core conversion state and what we're expecting next
#[derive(Debug)]
pub enum Expectation {
    Value(serde_json::Value),
    ListAccumulation(Vec<serde_json::Value>),
    EvenBlockAccumulation(Vec<(String, serde_json::Value)>),
    OddBlockAccumulation(Vec<(String, serde_json::Value)>, String),
}

impl Expectation {
    fn feed(&mut self, val: serde_json::Value) {
        match self {
            Expectation::Value(_) => *self = Expectation::Value(val),
            Expectation::ListAccumulation(ref mut items) => items.push(val),
            Expectation::EvenBlockAccumulation(items) => {
                *self = Expectation::OddBlockAccumulation(
                    items.to_vec(),
                    val.as_str().unwrap().to_string(),
                )
            }
            Expectation::OddBlockAccumulation(items, key) => {
                items.push((key.clone(), val));
                *self = Expectation::EvenBlockAccumulation(items.to_vec())
            }
        }
    }
}

impl<'a> JsonEmitter<'a> {
    pub fn new(out: &'a mut (dyn Write + 'a)) -> Self {
        JsonEmitter {
            stack: Vec::new(),
            out,
        }
    }

    fn feed(&mut self, val: serde_json::Value) {
        if let Some(mut top) = self.stack.pop() {
            top.feed(val);
            self.stack.push(top);
        } else {
            self.stack.push(Expectation::Value(val))
        }
    }
}

impl<'a> Emitter for JsonEmitter<'a> {
    /// Emit JSON events
    fn emit(&mut self, event: Event) {
        match event {
            Event::OutputScalar(_, prim) => {
                self.feed(as_json(&prim));
            }
            Event::OutputSequenceStart(_) => self.stack.push(Expectation::ListAccumulation(vec![])),
            Event::OutputSequenceEnd => {
                if let Some(Expectation::ListAccumulation(items)) = self.stack.pop() {
                    self.feed(serde_json::Value::Array(items))
                }
            }
            Event::OutputBlockStart(_) => {
                self.stack.push(Expectation::EvenBlockAccumulation(vec![]))
            }
            Event::OutputBlockEnd => {
                if let Some(Expectation::EvenBlockAccumulation(items)) = self.stack.pop() {
                    self.feed(serde_json::Value::Object(items.into_iter().collect()))
                }
            }
            Event::OutputDocumentStart => {
                self.stack
                    .push(Expectation::Value(serde_json::Value::Object(
                        iter::empty().collect(),
                    )));
            }
            Event::OutputDocumentEnd => {} // leave for now
            Event::OutputStreamStart => {}
            Event::OutputStreamEnd => {
                if let Some(Expectation::Value(val)) = self.stack.pop() {
                    writeln!(self.out, "{}", serde_json::to_string_pretty(&val).unwrap()).unwrap();
                }
            }
            _ => {}
        }
    }
}

fn as_json(prim: &Primitive) -> serde_json::Value {
    match prim {
        Primitive::Null => serde_json::Value::Null,
        Primitive::Bool(b) => serde_json::Value::Bool(*b),
        Primitive::Sym(s) => serde_json::Value::String(s.clone()),
        Primitive::Str(s) => serde_json::Value::String(s.clone()),
        Primitive::Num(n) => serde_json::Value::Number(n.clone()),
        Primitive::ZonedDateTime(dt) => serde_json::Value::String(format!("{}", dt)),
    }
}
