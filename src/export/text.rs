//! Export as plain text
use crate::eval::emit::{Emitter, Event};
use crate::eval::primitive::Primitive;
use std::io::Write;

/// A crude emitter that spits out all scalars as plain text
///
/// This is only really useful for evaluands that select single values.
pub struct TextEmitter<'a> {
    out: &'a mut (dyn Write + 'a),
}

impl<'a> TextEmitter<'a> {
    pub fn new(out: &'a mut (dyn Write + 'a)) -> Self {
        TextEmitter { out }
    }
}

impl Emitter for TextEmitter<'_> {
    /// Emit text scalars
    fn emit(&mut self, event: Event) {
        if let Event::OutputScalar(_, prim) = event {
            writeln!(self.out, "{}", as_text(&prim)).unwrap();
        }
    }
}

fn as_text(prim: &Primitive) -> String {
    match prim {
        Primitive::Null => "".to_string(),
        Primitive::Bool(b) => format!("{}", b),
        Primitive::Sym(s) => s.to_string(),
        Primitive::Str(s) => s.to_string(),
        Primitive::Num(n) => format!("{}", n),
        Primitive::ZonedDateTime(dt) => format!("{}", dt),
    }
}
