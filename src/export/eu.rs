//! Eucalypt-syntax export
//!
//! Renders eucalypt values back to eucalypt source syntax.  The key
//! difference from JSON is that `Primitive::Sym` renders as `:name`
//! rather than `"name"`.  Output is pretty-printed with the `pretty`
//! crate.

use crate::eval::emit::{Emitter, Event, RenderMetadata};
use crate::eval::primitive::Primitive;
use pretty::RcDoc;
use std::io::Write;

use super::table::{AsKey, FromPairs, FromPrimitive, FromVec, TableAccumulator};

/// A eucalypt value tree, built up by `TableAccumulator` during export
#[derive(Clone)]
pub enum EuValue {
    Null,
    Bool(bool),
    Sym(String),
    Str(String),
    Num(serde_json::Number),
    ZonedDateTime(chrono::DateTime<chrono::FixedOffset>),
    Array(Vec<EuValue>),
    Block(Vec<(String, EuValue)>),
}

impl AsKey<String> for EuValue {
    fn as_key(&self) -> String {
        match self {
            EuValue::Sym(s) | EuValue::Str(s) => s.clone(),
            _ => panic!("non-string/symbol used as block key in eu export"),
        }
    }
}

impl FromPrimitive for EuValue {
    fn from_primitive(_metadata: RenderMetadata, primitive: &Primitive) -> Self {
        match primitive {
            Primitive::Null => EuValue::Null,
            Primitive::Bool(b) => EuValue::Bool(*b),
            Primitive::Sym(s) => EuValue::Sym(s.clone()),
            Primitive::Str(s) => EuValue::Str(s.clone()),
            Primitive::Num(n) => EuValue::Num(n.clone()),
            Primitive::ZonedDateTime(dt) => EuValue::ZonedDateTime(*dt),
        }
    }
}

impl FromVec<EuValue> for EuValue {
    fn from_vec(_metadata: RenderMetadata, slice: Vec<EuValue>) -> Self {
        EuValue::Array(slice)
    }
}

impl FromPairs<String, EuValue> for EuValue {
    fn from_pairs(_metadata: RenderMetadata, pairs: Vec<(String, EuValue)>) -> Self {
        EuValue::Block(pairs)
    }
}

/// Escape and quote a string as a eucalypt string literal.
///
/// Eucalypt uses JSON-compatible string syntax, so we delegate to
/// `serde_json` for correct escaping.
fn eu_quote_str(s: &str) -> String {
    serde_json::to_string(s).unwrap_or_else(|_| format!("{s:?}"))
}

/// Convert an `EuValue` to a `pretty` document for rendering.
fn eu_to_doc(val: &EuValue) -> RcDoc<'static> {
    match val {
        EuValue::Null => RcDoc::text("null"),
        EuValue::Bool(b) => RcDoc::text(if *b { "true" } else { "false" }),
        EuValue::Sym(s) => RcDoc::text(format!(":{s}")),
        EuValue::Str(s) => RcDoc::text(eu_quote_str(s)),
        EuValue::Num(n) => RcDoc::text(format!("{n}")),
        EuValue::ZonedDateTime(dt) => RcDoc::text(eu_quote_str(&format!("{dt}"))),
        EuValue::Array(items) => {
            if items.is_empty() {
                return RcDoc::text("[]");
            }
            let docs: Vec<RcDoc<'static>> = items.iter().map(eu_to_doc).collect();
            let body = RcDoc::intersperse(docs, RcDoc::text(",").append(RcDoc::line()));
            RcDoc::text("[")
                .append(RcDoc::line_().append(body).append(RcDoc::line_()).nest(2))
                .append(RcDoc::text("]"))
                .group()
        }
        EuValue::Block(pairs) => {
            if pairs.is_empty() {
                return RcDoc::text("{}");
            }
            let docs: Vec<RcDoc<'static>> = pairs
                .iter()
                .map(|(k, v)| {
                    RcDoc::text(format!("{k}:"))
                        .append(RcDoc::space())
                        .append(eu_to_doc(v))
                })
                .collect();
            let body = RcDoc::intersperse(docs, RcDoc::text(",").append(RcDoc::line()));
            // In flat mode: `{ k: v, ... }` (spaces inside braces).
            // In break mode: `{\n  k: v,\n  ...\n}` (indented entries).
            RcDoc::text("{")
                .append(RcDoc::line())
                .append(body)
                .nest(2)
                .append(RcDoc::line())
                .append(RcDoc::text("}"))
                .group()
        }
    }
}

/// Render an `EuValue` as a eucalypt-syntax string.
fn render_eu_value(val: &EuValue) -> String {
    let doc = eu_to_doc(val);
    let mut buf = Vec::new();
    doc.render(80, &mut buf).expect("failed to render eu value");
    String::from_utf8(buf).expect("eu output is not valid UTF-8")
}

/// Emitter for eucalypt-syntax output (`-x eu`)
pub struct EuEmitter<'a> {
    accum: TableAccumulator<String, EuValue>,
    out: &'a mut (dyn Write + 'a),
}

impl<'a> EuEmitter<'a> {
    pub fn new(out: &'a mut (dyn Write + 'a)) -> Self {
        EuEmitter {
            accum: Default::default(),
            out,
        }
    }
}

impl Emitter for EuEmitter<'_> {
    fn emit(&mut self, event: Event) {
        self.accum.consume(event);
        if let Some(result) = self.accum.result() {
            writeln!(self.out, "{}", render_eu_value(result)).expect("failed to write eu output");
        }
    }
}
