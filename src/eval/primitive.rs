//! Primitive types emittable by exporters
use chrono::{DateTime, FixedOffset};
use serde_json::Number;

/// Primitives that can be emitted in any of our output formats
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive {
    Sym(String),
    Str(String),
    Num(Number),
    Bool(bool),
    ZonedDateTime(DateTime<FixedOffset>),
    Null,
}
