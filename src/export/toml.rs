//! TOML export

use crate::eval::emit::{Emitter, Event};
use crate::eval::primitive::Primitive;
use std::io::Write;
use std::iter;
use std::str::FromStr;
use toml::{value::Datetime, Value};

/// Currently basic TOML emitter (no tags yet) for bootstrapping
pub struct TomlEmitter<'a> {
    stack: Vec<Expectation>,
    out: &'a mut (dyn Write + 'a),
}

/// Represent core conversion state and what we're expecting next
#[derive(Debug)]
pub enum Expectation {
    Value(Value),
    ListAccumulation(Vec<Value>),
    EvenBlockAccumulation(Vec<(String, Value)>),
    OddBlockAccumulation(Vec<(String, Value)>, String),
}

impl Expectation {
    fn feed(&mut self, val: Value) {
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

impl<'a> TomlEmitter<'a> {
    pub fn new(out: &'a mut (dyn Write + 'a)) -> Self {
        TomlEmitter {
            stack: Vec::new(),
            out,
        }
    }

    fn feed(&mut self, val: Value) {
        if let Some(mut top) = self.stack.pop() {
            top.feed(val);
            self.stack.push(top);
        } else {
            self.stack.push(Expectation::Value(val))
        }
    }
}

impl<'a> Emitter for TomlEmitter<'a> {
    /// Emit TOML events
    fn emit(&mut self, event: Event) {
        match event {
            Event::OutputScalar(_, prim) => {
                self.feed(as_toml(&prim));
            }
            Event::OutputSequenceStart(_) => self.stack.push(Expectation::ListAccumulation(vec![])),
            Event::OutputSequenceEnd => {
                if let Some(Expectation::ListAccumulation(items)) = self.stack.pop() {
                    self.feed(Value::Array(items))
                }
            }
            Event::OutputBlockStart(_) => {
                self.stack.push(Expectation::EvenBlockAccumulation(vec![]))
            }
            Event::OutputBlockEnd => {
                if let Some(Expectation::EvenBlockAccumulation(items)) = self.stack.pop() {
                    self.feed(Value::Table(items.into_iter().collect()))
                }
            }
            Event::OutputDocumentStart => {
                self.stack
                    .push(Expectation::Value(Value::Table(iter::empty().collect())));
            }
            Event::OutputDocumentEnd => {} // leave for now
            Event::OutputStreamStart => {}
            Event::OutputStreamEnd => {
                if let Some(Expectation::Value(val)) = self.stack.pop() {
                    writeln!(self.out, "{}", val).unwrap();
                }
            }
            _ => {}
        }
    }
}

fn as_toml(prim: &Primitive) -> Value {
    match prim {
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
