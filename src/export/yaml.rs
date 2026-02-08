//! YAML export
use yaml_rust::yaml::Tag;

use crate::eval::emit::{Emitter, Event, RenderMetadata};
use crate::eval::primitive::Primitive;

use std::{convert::TryInto, io::Write};

use super::table::{AsKey, FromPairs, FromPrimitive, FromVec, TableAccumulator};

impl From<RenderMetadata> for Option<Tag> {
    fn from(metadata: RenderMetadata) -> Self {
        metadata.tag().as_ref().map(|t| {
            if let Some(suffix) = t.strip_prefix('!') {
                Tag("!".to_string(), suffix.to_string())
            } else {
                Tag("".to_string(), t.to_string())
            }
        })
    }
}

impl AsKey<yaml_rust::Yaml> for yaml_rust::Yaml {
    fn as_key(&self) -> yaml_rust::Yaml {
        self.clone()
    }
}

impl FromPrimitive for yaml_rust::Yaml {
    fn from_primitive(metadata: RenderMetadata, primitive: &Primitive) -> Self {
        match primitive {
            Primitive::Null => yaml_rust::Yaml::Null(metadata.into()),
            Primitive::Bool(b) => yaml_rust::Yaml::Boolean(metadata.into(), *b),
            Primitive::Sym(s) => yaml_rust::Yaml::String(metadata.into(), s.clone()),
            Primitive::Str(s) => yaml_rust::Yaml::String(metadata.into(), s.clone()),
            Primitive::Num(n) => {
                if n.is_f64() {
                    yaml_rust::Yaml::Real(metadata.into(), format!("{}", n.as_f64().unwrap()))
                } else if n.is_i64() {
                    yaml_rust::Yaml::Integer(metadata.into(), n.as_i64().unwrap())
                } else if n.is_u64() {
                    yaml_rust::Yaml::Integer(
                        metadata.into(),
                        n.as_u64().unwrap().try_into().expect("unrenderable number"),
                    )
                } else {
                    panic!("unrenderable number")
                }
            }
            Primitive::ZonedDateTime(dt) => {
                yaml_rust::Yaml::String(metadata.into(), format!("{dt}"))
            }
        }
    }
}

impl FromVec<yaml_rust::Yaml> for yaml_rust::Yaml {
    fn from_vec(metadata: RenderMetadata, v: Vec<yaml_rust::Yaml>) -> Self {
        yaml_rust::Yaml::Array(metadata.into(), v)
    }
}

impl FromPairs<yaml_rust::Yaml, yaml_rust::Yaml> for yaml_rust::Yaml {
    fn from_pairs(
        metadata: RenderMetadata,
        pairs: Vec<(yaml_rust::Yaml, yaml_rust::Yaml)>,
    ) -> Self {
        yaml_rust::Yaml::Hash(metadata.into(), pairs.into_iter().collect())
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

impl Emitter for YamlEmitter<'_> {
    fn emit(&mut self, event: Event) {
        self.accum.consume(event);
        if let Some(result) = self.accum.result() {
            let mut output = String::new();
            yaml_rust::YamlEmitter::new(&mut output)
                .dump(result)
                .expect("failed to emit YAML");
            writeln!(self.out, "{output}").expect("failed to write YAML output");
        }
    }
}
