//! Read XML into hiccup-style lists
use std::{collections::VecDeque, convert::TryInto};

use crate::common::sourcemap::SourceMap;
use crate::core::expr::*;
use crate::import::error::SourceError;
use codespan::{ByteIndex, Span};
use quick_xml::{
    events::{BytesStart, Event},
    Reader,
};

/// An incomplete element current being parsed
struct ProtoElement {
    pub name: RcExpr,
    pub attrs: RcExpr,
    pub content: Vec<RcExpr>,
}

impl ProtoElement {
    pub fn new(name: RcExpr, attrs: RcExpr) -> Self {
        ProtoElement {
            name,
            attrs,
            content: Vec::new(),
        }
    }

    pub fn add(&mut self, subelement: RcExpr) {
        self.content.push(subelement)
    }

    /// Complete a proto-element and turn it into a core list
    fn complete(self) -> RcExpr {
        let ProtoElement {
            name,
            attrs,
            mut content,
        } = self;
        content.insert(0, attrs);
        content.insert(0, name);
        acore::list(content)
    }
}

/// Read XML from text and convert to core syntax data
pub fn read_xml<'src>(
    _source_map: &'src mut SourceMap,
    file_id: usize,
    text: &'src str,
) -> Result<RcExpr, SourceError> {
    XmlImporter::new(file_id, text).parse()
}

struct XmlImporter<'src> {
    file_id: usize,
    reader: Reader<&'src [u8]>,
}

impl<'src> XmlImporter<'src> {
    pub fn new(file_id: usize, text: &'src str) -> Self {
        let mut reader = Reader::from_str(text);
        reader.trim_text(true);
        XmlImporter { file_id, reader }
    }

    pub fn parse(&mut self) -> Result<RcExpr, SourceError> {
        let mut buf = Vec::new();
        let mut stack: VecDeque<ProtoElement> = VecDeque::new();

        loop {
            match self.reader.read_event_into(&mut buf) {
                Ok(Event::Start(ref event)) => {
                    stack.push_back(self.to_proto_element(event)?);
                }
                Ok(Event::End(_)) => {
                    if let Some(top) = stack.pop_back() {
                        let n = top.complete();
                        if let Some(top) = stack.back_mut() {
                            top.add(n);
                        } else {
                            return Ok(n);
                        }
                    }
                }
                Ok(Event::Empty(ref event)) => {
                    let n = self.to_proto_element(event)?.complete();
                    if let Some(top) = stack.back_mut() {
                        top.add(n);
                    } else {
                        return Ok(n);
                    }
                }
                Ok(Event::Text(event)) => {
                    let text = event.unescape().map_err(|e| self.to_source_error(e))?;
                    if let Some(top) = stack.back_mut() {
                        top.add(acore::str(&text));
                    } else {
                        return Err(SourceError::InvalidXml(
                            "No top-level element".to_string(),
                            self.file_id,
                            self.span(),
                        ));
                    }
                }
                Err(e) => return Err(self.to_source_error(e)),
                _ => (),
            }
        }
    }

    /// A span indicating current reader position
    fn span(&self) -> Span {
        let pos = ByteIndex(self.reader.buffer_position().try_into().unwrap());
        Span::new(pos, pos)
    }

    /// Decode UTF-8 string, returning SourceError on failure
    fn to_str(&self, raw: &[u8]) -> Result<String, SourceError> {
        match std::str::from_utf8(raw) {
            Ok(s) => Ok(s.to_string()),
            Err(e) => {
                let (valid, _) = raw.split_at(e.valid_up_to());
                Err(SourceError::CharSetError(
                    std::str::from_utf8(valid).unwrap().to_string(),
                    self.file_id,
                ))
            }
        }
    }

    /// Create a proto element from an open / empty tag
    fn to_proto_element(&self, e: &BytesStart<'_>) -> Result<ProtoElement, SourceError> {
        let name = acore::sym(self.to_str(e.name().local_name().as_ref())?);
        let mut attrs = Vec::new();

        for a in e.attributes() {
            let attr = a.map_err(|e| self.to_source_error(quick_xml::Error::InvalidAttr(e)))?;
            let k = self.to_str(attr.key.local_name().as_ref())?;
            let v = acore::str(
                attr.decode_and_unescape_value(&self.reader)
                    .map_err(|e| self.to_source_error(e))?,
            );
            attrs.push((k, v));
        }

        Ok(ProtoElement::new(name, acore::block(attrs)))
    }

    /// Map quick_xml errors to SourceError
    fn to_source_error(&self, e: quick_xml::Error) -> SourceError {
        SourceError::InvalidXml(e.to_string(), self.file_id, self.span())
    }
}
