//! YAML export
use crate::eval::emit::{Emitter, Event};
use crate::eval::primitive::Primitive;

use std::{convert::TryInto, io::Write};

use super::table::{AsKey, FromPairs, FromPrimitive, FromVec, TableAccumulator};

impl AsKey<yaml_rust::Yaml> for yaml_rust::Yaml {
    fn as_key(&self) -> yaml_rust::Yaml {
        self.clone()
    }
}

impl FromPrimitive for yaml_rust::Yaml {
    fn from_primitive(primitive: &Primitive) -> Self {
        match primitive {
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
}

impl FromVec<yaml_rust::Yaml> for yaml_rust::Yaml {
    fn from_vec(v: Vec<yaml_rust::Yaml>) -> Self {
        yaml_rust::Yaml::Array(v)
    }
}

impl FromPairs<yaml_rust::Yaml, yaml_rust::Yaml> for yaml_rust::Yaml {
    fn from_pairs(pairs: Vec<(yaml_rust::Yaml, yaml_rust::Yaml)>) -> Self {
        yaml_rust::Yaml::Hash(pairs.into_iter().collect())
    }
}

/// Currently basic YAML emitter (no tags yet) for bootstrapping
pub struct YamlEmitter<'a> {
    accum: TableAccumulator<yaml_rust::Yaml, yaml_rust::Yaml>,
    out: &'a mut (dyn Write + 'a),
}

impl<'a> YamlEmitter<'a> {
    pub fn new(out: &'a mut (dyn Write + 'a)) -> Self {
        YamlEmitter {
            accum: Default::default(),
            out,
        }
    }
}

impl<'a> Emitter for YamlEmitter<'a> {
    fn emit(&mut self, event: Event) {
        self.accum.consume(event);
        if let Some(result) = self.accum.result() {
            let mut output = String::new();
            yaml_rust::YamlEmitter::new(&mut output)
                .dump(result)
                .unwrap();
            writeln!(self.out, "{}", output).unwrap();
        }
    }
}
