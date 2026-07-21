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
        // Deliberately NOT `trim_text(true)`. quick-xml 0.41 emits entity /
        // character references in text content as separate `Event::GeneralRef`
        // events, interspersed with `Event::Text` fragments for the literal
        // text either side. `trim_text(true)` trims each of those fragments
        // *individually*, which discards the interior whitespace between a
        // text fragment and a neighbouring reference (`"a "` + `"&amp;"` +
        // `" b"` would trim to `"a"` + `"&"` + `"b"`, losing both spaces).
        // Instead we accumulate raw text + resolved references into
        // `text_buf` across a whole run and trim only the coalesced whole in
        // `flush_text`, matching pre-0.12.0 behaviour exactly.
        reader.config_mut().trim_text(false);
        XmlImporter { file_id, reader }
    }

    pub fn parse(&mut self) -> Result<RcExpr, SourceError> {
        let mut buf = Vec::new();
        let mut stack: VecDeque<ProtoElement> = VecDeque::new();
        let mut text_buf = String::new();

        loop {
            match self.reader.read_event_into(&mut buf) {
                Ok(Event::Start(ref event)) => {
                    self.flush_text(&mut text_buf, &mut stack)?;
                    stack.push_back(self.to_proto_element(event)?);
                }
                Ok(Event::End(_)) => {
                    self.flush_text(&mut text_buf, &mut stack)?;
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
                    self.flush_text(&mut text_buf, &mut stack)?;
                    let n = self.to_proto_element(event)?.complete();
                    if let Some(top) = stack.back_mut() {
                        top.add(n);
                    } else {
                        return Ok(n);
                    }
                }
                Ok(Event::Text(event)) => {
                    // quick-xml 0.41: `BytesText::unescape` was removed. Text
                    // fragments no longer contain entity syntax at all (those
                    // arrive as separate `Event::GeneralRef` events below), so
                    // simply decode the character encoding and accumulate.
                    let decoded = event.decode().map_err(|e| self.to_source_error(e))?;
                    text_buf.push_str(&decoded);
                }
                Ok(Event::GeneralRef(bytes_ref)) => {
                    // Character reference (`&#NN;` / `&#xNN;`) or predefined
                    // entity reference (`&amp;`, `&lt;`, `&gt;`, `&quot;`,
                    // `&apos;`) in text content. Resolve and append to the
                    // same accumulating buffer as literal text so the whole
                    // run is coalesced into a single text node.
                    if let Some(ch) = bytes_ref
                        .resolve_char_ref()
                        .map_err(|e| self.to_source_error(e))?
                    {
                        text_buf.push(ch);
                    } else {
                        let name = bytes_ref.decode().map_err(|e| self.to_source_error(e))?;
                        match quick_xml::escape::resolve_predefined_entity(&name) {
                            Some(resolved) => text_buf.push_str(resolved),
                            None => {
                                return Err(SourceError::InvalidXml(
                                    format!("Unknown entity reference: &{name};"),
                                    self.file_id,
                                    self.span(),
                                ))
                            }
                        }
                    }
                }
                Err(e) => return Err(self.to_source_error(e)),
                _ => (),
            }
        }
    }

    /// Flush the accumulated text run (literal text plus resolved
    /// entity/character references) as a single text node, trimming the
    /// *whole* coalesced run rather than each contributing fragment. A
    /// whitespace-only run is dropped entirely, matching the previous
    /// `trim_text(true)` behaviour of eliding insignificant whitespace
    /// between elements.
    fn flush_text(
        &self,
        text_buf: &mut String,
        stack: &mut VecDeque<ProtoElement>,
    ) -> Result<(), SourceError> {
        if text_buf.is_empty() {
            return Ok(());
        }
        let trimmed = text_buf.trim();
        if !trimmed.is_empty() {
            if let Some(top) = stack.back_mut() {
                top.add(acore::str(trimmed));
            } else {
                return Err(SourceError::InvalidXml(
                    "No top-level element".to_string(),
                    self.file_id,
                    self.span(),
                ));
            }
        }
        text_buf.clear();
        Ok(())
    }

    /// A span indicating current reader position
    fn span(&self) -> Span {
        let pos = ByteIndex(
            self.reader
                .buffer_position()
                .try_into()
                .expect("XML reader position should fit in ByteIndex"),
        );
        Span::new(pos, pos)
    }

    /// Decode UTF-8 string, returning SourceError on failure
    fn to_str(&self, raw: &[u8]) -> Result<String, SourceError> {
        match std::str::from_utf8(raw) {
            Ok(s) => Ok(s.to_string()),
            Err(e) => {
                let (valid, _) = raw.split_at(e.valid_up_to());
                Err(SourceError::CharSetError(
                    std::str::from_utf8(valid)
                        .expect("valid portion of XML should be UTF-8")
                        .to_string(),
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
            let attr = a.map_err(|e| self.to_source_error(e))?;
            let k = self.to_str(attr.key.local_name().as_ref())?;
            // DELIBERATE (eu-cgys, post-review owner decision): apply XML 1.0
            // §3.3.3 attribute-value normalisation. Literal whitespace (tab, CR,
            // LF) in an attribute value becomes a space, as required of a
            // conforming processor; character references such as `&#10;` / `&#9;`
            // are resolved *before* normalisation and so still yield real
            // newlines/tabs. This is a behaviour change from pre-0.12 eucalypt
            // (which was non-conformant) — see CHANGELOG 0.12.0. Element *text*
            // content is unaffected (it is unescaped without normalisation).
            let v = acore::str(
                attr.decoded_and_normalized_value(
                    quick_xml::XmlVersion::Implicit1_0,
                    self.reader.decoder(),
                )
                .map_err(|e| self.to_source_error(e))?,
            );
            attrs.push((k, v));
        }

        Ok(ProtoElement::new(name, acore::block(attrs)))
    }

    /// Map any XML error type (reader, encoding, escape, attribute) to a
    /// `SourceError` by its display text.
    fn to_source_error<E: std::fmt::Display>(&self, e: E) -> SourceError {
        SourceError::InvalidXml(e.to_string(), self.file_id, self.span())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::sourcemap::SourceMap;

    /// Extract the `a` attribute string of the (single) root element from the
    /// hiccup import `[:root {a: "..."} ...]`.
    fn import_attr_a(xml: &str) -> String {
        let mut sm = SourceMap::new();
        let expr = read_xml(&mut sm, 0, xml).expect("xml import should succeed");
        let attrs = match &*expr.inner {
            Expr::List(_, xs) => xs.get(1).cloned().expect("attrs block present"),
            other => panic!("expected a hiccup list, got {other:?}"),
        };
        let value = match &*attrs.inner {
            Expr::Block(_, bm) => bm.get("a").cloned().expect("attribute `a` present"),
            other => panic!("expected an attrs block, got {other:?}"),
        };
        match &*value.inner {
            Expr::Literal(_, Primitive::Str(s)) => s.clone(),
            other => panic!("expected a string literal, got {other:?}"),
        }
    }

    /// eu-cgys: DELIBERATE spec-conformant behaviour. XML 1.0 §3.3.3 requires a
    /// conforming processor to normalise attribute values — each literal
    /// whitespace character (tab, CR, LF) in an attribute value is replaced by a
    /// single space. Post quick-xml 0.25→0.41 bump, eucalypt now conforms (it
    /// previously did not). This test pins that behaviour so a future change
    /// away from it is caught.
    #[test]
    fn literal_attribute_whitespace_is_normalised_to_space() {
        // Real newline + tab embedded in the `a` attribute value.
        assert_eq!(
            import_attr_a("<root a=\"x\nY\tZ\"/>"),
            "x Y Z",
            "literal newline/tab in an attribute value must normalise to a \
             single space each (XML 1.0 §3.3.3)"
        );
    }

    /// eu-cgys: the escape hatch. Per XML 1.0 §3.3.3, *character references* to
    /// whitespace are resolved before normalisation, so `&#10;` (LF) and `&#9;`
    /// (tab) yield real control characters that survive verbatim — this is how a
    /// document author requests literal whitespace in an attribute value.
    #[test]
    fn attribute_whitespace_character_references_survive() {
        assert_eq!(
            import_attr_a("<root a=\"x&#10;Y&#9;Z\"/>"),
            "x\nY\tZ",
            "character references to whitespace must survive normalisation \
             (XML 1.0 §3.3.3)"
        );
    }
}
