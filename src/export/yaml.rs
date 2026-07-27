//! YAML export
use yaml_rust::yaml::Tag;

use crate::eval::emit::{Emitter, Event, Rejection, RenderMetadata};
use crate::eval::primitive::Primitive;

use std::io::Write;

use super::table::{AsKey, FromPairs, FromPrimitive, FromVec, TableAccumulator};
use super::INTEGER_RANGE_NOTES;

/// Describe why `n` cannot be carried as a YAML integer, or `None` if it can.
///
/// `yaml_rust::Yaml::Integer` is `i64`-typed, so a JSON-sourced `u64` above
/// `i64::MAX` has no YAML integer representation. This is the single predicate
/// behind both `YamlEmitter::unrepresentable` (which rejects such a value at
/// the emit intrinsic, with a source location) and the fallback in
/// `from_primitive` below, so the two can never disagree.
fn yaml_integer_overflow(n: &serde_json::Number) -> Option<Rejection> {
    match n.as_u64() {
        Some(u) if !n.is_i64() => Some(
            Rejection::new(format!(
                "the integer {u} is above {}, the largest integer a YAML \
                 integer scalar can carry",
                i64::MAX
            ))
            .with_notes(INTEGER_RANGE_NOTES),
        ),
        _ => None,
    }
}

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
                if yaml_integer_overflow(n).is_some() {
                    // Unreachable through the emit intrinsics, which reject
                    // this value via `unrepresentable` before it gets here.
                    // Should another route ever reach it, write the digits
                    // out verbatim as a plain scalar rather than aborting the
                    // whole process: `Yaml::Real` is emitted unquoted and
                    // unaltered, so no data is lost (eu-1tkk.7.20).
                    yaml_rust::Yaml::Real(metadata.into(), n.to_string())
                } else if let Some(i) = n.as_i64() {
                    yaml_rust::Yaml::Integer(metadata.into(), i)
                } else if let Some(f) = n.as_f64() {
                    yaml_rust::Yaml::Real(metadata.into(), format!("{f}"))
                } else {
                    yaml_rust::Yaml::Real(metadata.into(), n.to_string())
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
    fn format_name(&self) -> &'static str {
        "YAML"
    }

    fn unrepresentable(&self, primitive: &Primitive) -> Option<Rejection> {
        match primitive {
            Primitive::Num(n) => yaml_integer_overflow(n),
            _ => None,
        }
    }

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
