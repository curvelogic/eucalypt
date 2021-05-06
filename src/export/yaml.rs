//! YAML export
use crate::eval::emit::{Emitter, Event};
use crate::eval::primitive::Primitive;
use std::iter;
use std::{convert::TryInto, io::Write};

/// Currently basic YAML emitter (no tags yet) for bootstrapping
pub struct YamlEmitter<'a> {
    stack: Vec<Expectation>,
    out: &'a mut (dyn Write + 'a),
}

/// Represent core conversion state and what we're expecting next
#[derive(Debug)]
pub enum Expectation {
    Value(yaml_rust::Yaml),
    ListAccumulation(Vec<yaml_rust::Yaml>),
    EvenBlockAccumulation(Vec<(yaml_rust::Yaml, yaml_rust::Yaml)>),
    OddBlockAccumulation(Vec<(yaml_rust::Yaml, yaml_rust::Yaml)>, yaml_rust::Yaml),
}

impl Expectation {
    fn feed(&mut self, val: yaml_rust::Yaml) {
        match self {
            Expectation::Value(_) => *self = Expectation::Value(val),
            Expectation::ListAccumulation(ref mut items) => items.push(val),
            Expectation::EvenBlockAccumulation(items) => {
                *self = Expectation::OddBlockAccumulation(items.to_vec(), val)
            }
            Expectation::OddBlockAccumulation(items, key) => {
                items.push((key.clone(), val));
                *self = Expectation::EvenBlockAccumulation(items.to_vec())
            }
        }
    }
}

impl<'a> YamlEmitter<'a> {
    pub fn new(out: &'a mut (dyn Write + 'a)) -> Self {
        YamlEmitter {
            stack: Vec::new(),
            out,
        }
    }

    fn feed(&mut self, val: yaml_rust::Yaml) {
        if let Some(mut top) = self.stack.pop() {
            top.feed(val);
            self.stack.push(top);
        } else {
            self.stack.push(Expectation::Value(val))
        }
    }
}

impl<'a> Emitter for YamlEmitter<'a> {
    /// Emit YAML events
    fn emit(&mut self, event: Event) {
        match event {
            Event::OutputScalar(_, prim) => {
                self.feed(as_yaml(&prim));
            }
            Event::OutputSequenceStart(_) => self.stack.push(Expectation::ListAccumulation(vec![])),
            Event::OutputSequenceEnd => {
                if let Some(Expectation::ListAccumulation(items)) = self.stack.pop() {
                    self.feed(yaml_rust::Yaml::Array(items))
                }
            }
            Event::OutputBlockStart(_) => {
                self.stack.push(Expectation::EvenBlockAccumulation(vec![]))
            }
            Event::OutputBlockEnd => {
                if let Some(Expectation::EvenBlockAccumulation(items)) = self.stack.pop() {
                    self.feed(yaml_rust::Yaml::Hash(items.into_iter().collect()))
                }
            }
            Event::OutputDocumentStart => {
                self.stack.push(Expectation::Value(yaml_rust::Yaml::Hash(
                    iter::empty().collect(),
                )));
            }
            Event::OutputDocumentEnd => {} // leave for now
            Event::OutputStreamStart => {}
            Event::OutputStreamEnd => {
                if let Some(Expectation::Value(val)) = self.stack.pop() {
                    let mut output = String::new();
                    yaml_rust::YamlEmitter::new(&mut output).dump(&val).unwrap();
                    writeln!(self.out, "{}", output).unwrap();
                }
            }
            _ => {}
        }
    }
}

fn as_yaml(prim: &Primitive) -> yaml_rust::Yaml {
    match prim {
        Primitive::Null => yaml_rust::Yaml::Null,
        Primitive::Bool(b) => yaml_rust::Yaml::Boolean(*b),
        Primitive::Sym(s) => yaml_rust::Yaml::String(s.clone()),
        Primitive::Str(s) => yaml_rust::Yaml::String(s.clone()),
        Primitive::Num(n) => {
            if n.is_f64() {
                yaml_rust::Yaml::Real(format!("{}", n.as_f64().unwrap()))
            } else if n.is_i64() {
                yaml_rust::Yaml::Integer(n.as_i64().unwrap())
            } else if n.is_u64() {
                yaml_rust::Yaml::Integer(
                    n.as_u64().unwrap().try_into().expect("unrenderable number"),
                )
            } else {
                panic!("unrenderable number")
            }
        }
        // TODO: tags...
        Primitive::ZonedDateTime(dt) => yaml_rust::Yaml::String(format!("{}", dt)),
    }
}
