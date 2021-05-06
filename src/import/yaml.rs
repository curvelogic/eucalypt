//! Import YAML and transform into core syntax
use crate::core::{desugar::Desugarer, expr::*};
use crate::import::error::SourceError;
use crate::{
    common::sourcemap::{Smid, SourceMap},
    syntax::parser,
};
use codespan::{ByteIndex, ByteOffset, Span};
use codespan_reporting::files::SimpleFiles;
use moniker::FreeVar;
use regex::RegexSet;
use std::str::FromStr;
use std::{collections::HashMap, iter};
use yaml_rust::parser::{Event, MarkedEventReceiver, Parser};
use yaml_rust::scanner::{Marker, TScalarStyle, TokenType};

/// Read the YAML into a core expression
///
/// Use `source_map` to create new SMIDs and `file_id` for error reporting.
pub fn read_yaml<'smap>(
    files: &'smap mut SimpleFiles<String, String>,
    source_map: &'smap mut SourceMap,
    file_id: usize,
    text: &'smap str,
) -> Result<RcExpr, SourceError> {
    let mut receiver = Receiver::new(files, source_map, file_id);
    Parser::new(text.chars())
        .load(&mut receiver, false)
        .unwrap();
    Ok(receiver.core())
}

/// Parse event handler for accumulating core expression
struct Receiver<'smap> {
    stack: Vec<Expectation>,
    files: &'smap mut SimpleFiles<String, String>,
    file_id: usize,
    source_map: &'smap mut SourceMap,
}

impl<'smap> Receiver<'smap> {
    /// Create a blank parse event receiver for file with id `file_id`
    pub fn new(
        files: &'smap mut SimpleFiles<String, String>,
        source_map: &'smap mut SourceMap,
        file_id: usize,
    ) -> Self {
        Receiver {
            files,
            stack: Vec::new(),
            file_id,
            source_map,
        }
    }

    /// Retrieve the parsed core expression
    pub fn core(&self) -> RcExpr {
        if let Some(Expectation::Value(expr)) = self.stack.last() {
            expr.clone()
        } else {
            acore::block(iter::empty())
        }
    }

    /// Mint a new smid for the specified span
    pub fn new_smid(&mut self, span: Span) -> Smid {
        self.source_map.add(self.file_id, span)
    }

    /// Complete the currently accumulating list
    fn complete_list(&mut self, end: ByteIndex) {
        if let Some(Expectation::ListAccumulation(start, items)) = self.stack.pop() {
            let smid = self.new_smid(Span::new(start, end));
            self.feed(core::list(smid, items));
        } else {
            panic!("unexpected complete_list parse event")
        }
    }

    /// Complete the currently accumulating block
    fn complete_block(&mut self, end: ByteIndex) {
        if let Some(Expectation::EvenBlockAccumulation(start, entries)) = self.stack.pop() {
            let smid = self.new_smid(Span::new(start, end));

            // TODO: performance - repeated walks which are only
            // necessary if there is embedded eucalypt
            let fvs: HashMap<String, FreeVar<String>> =
                entries.iter().map(|(k, _)| (k.clone(), free(&k))).collect();
            let kvs = entries
                .iter()
                .map(|(k, v)| {
                    (
                        fvs.get(k).unwrap().clone(),
                        v.substs_free(&|n| {
                            fvs.get(n).map(|fv| core::var(Smid::default(), fv.clone()))
                        }),
                    )
                })
                .collect();
            self.feed(core::default_let(smid, kvs));
        } else {
            panic!("unexpected complete_block parse event")
        }
    }

    /// Feed a converted value into whatever the current expectation is
    pub fn feed(&mut self, expr: RcExpr) {
        if let Some(expectation) = self.stack.pop() {
            self.stack.push(expectation.feed(expr));
        } else {
            // done
            self.stack.push(Expectation::Value(expr));
        }
    }

    /// Convert a scalar from its text representation informed by tags
    /// if available
    pub fn convert_scalar(
        &mut self,
        byte_index: ByteIndex,
        text: String,
        style: TScalarStyle,
        _anchor_id: usize,
        token_type: Option<TokenType>,
    ) -> Result<RcExpr, SourceError> {
        let span = Span::new(byte_index, byte_index);
        let smid = self.new_smid(span);

        let tag = token_type
            .and_then(Tag::from_token_type)
            .unwrap_or_else(|| {
                if let TScalarStyle::Plain = style {
                    infer_tag_for_plain_scalar(&text)
                } else {
                    Tag::Str
                }
            });

        match tag {
            Tag::Null => Ok(core::null(smid)),
            Tag::Bool => Ok(core::bool_(smid, parse_bool(&text))),
            Tag::Str => Ok(core::str(smid, text)),
            Tag::Int | Tag::Float => serde_json::Number::from_str(&text)
                .map_err(|_e| SourceError::InvalidNumber(text, self.file_id, span))
                .map(|n| core::num(smid, n)),
            Tag::General(_, content) => match content.as_ref() {
                "eu" => self.parse_eu(text),
                "eu::fn" => self.parse_eu_fn(text),
                "eu::suppress" => Ok(core::meta(
                    smid,
                    core::str(smid, text),
                    acore::block(iter::once(("export".to_string(), acore::sym("suppress")))),
                )),
                t => Ok(core::meta(
                    smid,
                    core::str(smid, text),
                    acore::block(iter::once((
                        "tag".to_string(),
                        acore::str(format!("!{}", t)),
                    ))),
                )),
            },
        }
    }

    pub fn convert_key_scalar(
        &mut self,
        byte_index: ByteIndex,
        text: String,
    ) -> Result<RcExpr, SourceError> {
        let span = Span::new(byte_index, byte_index);
        let smid = self.new_smid(span);
        Ok(core::sym(smid, text))
    }

    /// Parse eucalypt expression embedded in YAML source
    pub fn parse_eu(&mut self, text: String) -> Result<RcExpr, SourceError> {
        let span = Span::new(ByteIndex(0), ByteIndex(0) + ByteOffset::from_str_len(&text));
        let file_id = self.files.add(format!("yaml:[{}]", text), text);
        let ast = parser::parse_expression(self.files, file_id)
            .map_err(|p| SourceError::EmbeddedParserError(p, file_id, span))?;

        let content = HashMap::new();
        let mut desugarer = Desugarer::new(&content, self.source_map);
        desugarer
            .translate_simple(file_id, &ast)
            .map_err(|c| SourceError::EmbeddedCoreError(c, file_id, span))
    }

    /// Parse eucalypt expression embedded in YAML source
    pub fn parse_eu_fn(&mut self, text: String) -> Result<RcExpr, SourceError> {
        let span = Span::new(ByteIndex(0), ByteIndex(0) + ByteOffset::from_str_len(&text));
        let file_id = self.files.add(format!("yaml:[{}]", text), text);
        let (head, body) = parser::parse_embedded_lambda(self.files, file_id)
            .map_err(|p| SourceError::EmbeddedParserError(p, file_id, span))?;

        let content = HashMap::new();
        let mut desugarer = Desugarer::new(&content, self.source_map);

        let lam_body = desugarer
            .translate_simple(file_id, &body)
            .map_err(|c| SourceError::EmbeddedCoreError(c, file_id, span))?;

        let fvs: HashMap<String, FreeVar<String>> = head
            .names()
            .iter()
            .map(|k| (k.name().to_string(), free(k.name())))
            .collect();

        let lam_body =
            lam_body.substs_free(&|n| fvs.get(n).map(|fv| core::var(Smid::default(), fv.clone())));

        let core = acore::lam(fvs.values().cloned().collect(), lam_body);
        Ok(core)
    }
}

/// Represent core conversion state and what we're expecting next
pub enum Expectation {
    Value(RcExpr),
    ListAccumulation(ByteIndex, Vec<RcExpr>),
    EvenBlockAccumulation(ByteIndex, Vec<(String, RcExpr)>),
    OddBlockAccumulation(ByteIndex, Vec<(String, RcExpr)>, String),
}

impl Expectation {
    pub fn new_list(i: ByteIndex) -> Self {
        Expectation::ListAccumulation(i, Vec::new())
    }

    pub fn new_block(i: ByteIndex) -> Self {
        Expectation::EvenBlockAccumulation(i, Vec::new())
    }

    pub fn feed(self, expr: RcExpr) -> Expectation {
        match self {
            Expectation::ListAccumulation(off, mut items) => {
                items.push(expr);
                Expectation::ListAccumulation(off, items)
            }
            Expectation::EvenBlockAccumulation(off, entries) => {
                if let Expr::Literal(_, prim) = &*expr.inner {
                    let key = match prim {
                        Primitive::Str(s) => s,
                        Primitive::Sym(s) => s,
                        _ => panic!("bad key type"),
                    };
                    Expectation::OddBlockAccumulation(off, entries, key.to_string())
                } else {
                    panic!("bad key type")
                }
            }
            Expectation::OddBlockAccumulation(off, mut entries, key) => {
                entries.push((key, expr));
                Expectation::EvenBlockAccumulation(off, entries)
            }
            _ => Expectation::Value(expr),
        }
    }
}

pub fn byte_index(m: Marker) -> ByteIndex {
    ByteIndex(m.index() as u32)
}

impl<'smap> MarkedEventReceiver for Receiver<'smap> {
    fn on_event(&mut self, event: Event, marker: Marker) {
        match event {
            Event::Nothing => {}
            Event::StreamStart => {}
            Event::StreamEnd => {}
            Event::DocumentStart => {}
            Event::DocumentEnd => {}
            Event::Alias(_i) => {}
            Event::Scalar(text, style, anchor_id, token_type) => {
                if matches!(
                    self.stack.last(),
                    Some(Expectation::EvenBlockAccumulation(..))
                ) {
                    let key = self.convert_key_scalar(byte_index(marker), text).unwrap();
                    self.feed(key);
                } else {
                    let value = self
                        .convert_scalar(byte_index(marker), text, style, anchor_id, token_type)
                        .unwrap();
                    self.feed(value);
                }
            }
            Event::SequenceStart(_anchor_id) => {
                self.stack.push(Expectation::new_list(byte_index(marker)));
            }
            Event::SequenceEnd => {
                self.complete_list(byte_index(marker));
            }
            Event::MappingStart(_anchor_id) => {
                self.stack.push(Expectation::new_block(byte_index(marker)));
            }
            Event::MappingEnd => {
                self.complete_block(byte_index(marker));
            }
        }
    }
}

/// YAML tags
#[derive(Debug, PartialEq)]
pub enum Tag {
    Null,
    Bool,
    Int,
    Float,
    Str,
    General(String, String),
}

impl Tag {
    /// When rust_yaml passes token type we'll use it
    fn from_token_type(token_type: TokenType) -> Option<Self> {
        if let TokenType::Tag(handle, content) = token_type {
            if handle == "!!" {
                match content.as_ref() {
                    "null" => Some(Tag::Null),
                    "bool" => Some(Tag::Bool),
                    "int" => Some(Tag::Int),
                    "float" => Some(Tag::Float),
                    "str" => Some(Tag::Str),
                    _ => None,
                }
            } else {
                Some(Tag::General(handle, content))
            }
        } else {
            None
        }
    }
}

/// If tags aren't explicit, infer from value
#[allow(clippy::trivial_regex)]
fn infer_tag_for_plain_scalar(text: &str) -> Tag {
    let set = RegexSet::new(&[
        r"^[nN]ull$",
        r"^NULL$",
        r"^~$",
        r"^$",
        r"^[tT]rue$",
        r"^[fF]alse$",
        r"^TRUE|FALSE$",
        r"^[-+]?[0-9]+$",
        r"^0o[0-7]+$",
        r"^0x[0-9a-fA-F]+$",
        r"^[-+]?(\.[0-9]+|[0-9]+(\.[0-9]*)?)([eE][-+]?[0-9]+)?$",
        r"^[-+]?(\.inf|\.Inf|\.INF)$",
        r"^\.nan|\.NaN|\.NAN$",
    ])
    .unwrap();

    if let Some(first_match) = set.matches(text).into_iter().min() {
        match first_match {
            0..=3 => Tag::Null,
            4..=6 => Tag::Bool,
            7..=9 => Tag::Int,
            10..=12 => Tag::Float,
            _ => Tag::Str,
        }
    } else {
        Tag::Str
    }
}

fn parse_bool(text: &str) -> bool {
    matches!(text, "TRUE" | "True" | "true")
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::core::expr::acore;
    use codespan_reporting::files::SimpleFiles;
    use moniker::assert_term_eq;
    use std::iter;

    fn parse(text: &str) -> RcExpr {
        let mut sm = SourceMap::new();
        let mut files = SimpleFiles::new();
        let file_id = files.add("blah".to_string(), "".to_string());
        let mut receiver = Receiver::new(&mut files, &mut sm, file_id);
        Parser::new(text.chars())
            .load(&mut receiver, false)
            .unwrap();
        receiver.core()
    }

    const SAMPLE1: &str = "
hello: !blah world
x: y
z: !!int 1234232353
";

    #[test]
    pub fn test_sample() {
        let parsed = parse(SAMPLE1);
        let expected = acore::default_let(vec![
            (
                free("hello"),
                acore::meta(
                    acore::str("world"),
                    acore::block(iter::once(("tag".to_string(), acore::str("!blah")))),
                ),
            ),
            (free("x"), acore::str("y")),
            (free("z"), acore::num(12342323535 as u64)),
        ]);

        assert_term_eq!(parsed, expected);
    }

    #[test]
    pub fn test_accepts_empty_input() {
        assert_term_eq!(parse(""), acore::block(iter::empty()));
    }

    #[test]
    pub fn test_parses_key_value() {
        assert_term_eq!(
            parse("a: !!int 1234"),
            acore::default_let(vec![(free("a"), acore::num(1234))])
        );
    }

    #[test]
    pub fn test_parses_json_data() {
        assert_term_eq!(
            parse(" { a: [1, 2, 3], b: {x: \"y\"} } "),
            acore::default_let(vec![
                (
                    free("a"),
                    acore::list(vec![acore::num(1), acore::num(2), acore::num(3)])
                ),
                (
                    free("b"),
                    acore::default_let(vec![(free("x"), acore::str("y"))])
                )
            ])
        );
    }

    #[test]
    pub fn test_parses_bools() {
        let sample = "a: true\nb: True\nc: TRUE\nd: false\ne: False\nf: FALSE ";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![
                (free("a"), acore::bool_(true)),
                (free("b"), acore::bool_(true)),
                (free("c"), acore::bool_(true)),
                (free("d"), acore::bool_(false)),
                (free("e"), acore::bool_(false)),
                (free("f"), acore::bool_(false))
            ])
        );
    }

    #[test]
    pub fn test_resolves_unknown_tags() {
        assert_eq!(infer_tag_for_plain_scalar("null"), Tag::Null);
        assert_eq!(infer_tag_for_plain_scalar("Null"), Tag::Null);
        assert_eq!(infer_tag_for_plain_scalar("NULL"), Tag::Null);
        assert_eq!(infer_tag_for_plain_scalar("nULL"), Tag::Str);
        assert_eq!(infer_tag_for_plain_scalar(""), Tag::Null);
        assert_eq!(infer_tag_for_plain_scalar("true"), Tag::Bool);
        assert_eq!(infer_tag_for_plain_scalar("True"), Tag::Bool);
        assert_eq!(infer_tag_for_plain_scalar("TRUE"), Tag::Bool);
        assert_eq!(infer_tag_for_plain_scalar("tRUE"), Tag::Str);
        assert_eq!(infer_tag_for_plain_scalar("false"), Tag::Bool);
        assert_eq!(infer_tag_for_plain_scalar("False"), Tag::Bool);
        assert_eq!(infer_tag_for_plain_scalar("FALSE"), Tag::Bool);
        assert_eq!(infer_tag_for_plain_scalar("fALSE"), Tag::Str);
        assert_eq!(infer_tag_for_plain_scalar("0"), Tag::Int);
        assert_eq!(infer_tag_for_plain_scalar("0o7"), Tag::Int);
        assert_eq!(infer_tag_for_plain_scalar("0x3A"), Tag::Int);
        assert_eq!(infer_tag_for_plain_scalar("-19"), Tag::Int);
        assert_eq!(infer_tag_for_plain_scalar("0."), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar("-0.0"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar(".5"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar("+12e03"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar("-2E+05"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar(".inf"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar("-.Inf"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar("+.INF"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar(".NAN"), Tag::Float);
    }
}
