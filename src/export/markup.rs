//! Some exports (XML, HTML) depend on a data model that represents
//! tags, attributes and contents.
//!
//! This module defines the canonical form, prelude functions can
//! provide shorthands and abbreviations.

use std::cell::RefCell;

use crate::{
    eval::emit::{Emitter, Event, Rejection},
    eval::primitive::Primitive,
};

use super::error::RenderError;

/// Build a markup rejection with the remediation common to all of them.
fn markup_rejection(reason: &str) -> Rejection {
    Rejection::new(reason).with_notes([
        "html output renders hiccup markup: a list of tag, attribute block \
         and contents, e.g. '[:div, { id: \"top\" }, \"hello\"]'",
        "select the markup value with a target or '-e', e.g. \
         'eu -x html -e page.eu'; rendering a whole unit gives html the \
         unit's block, which is not markup",
        "to render arbitrary data instead, choose a format that accepts any \
         shape, such as 'yaml', 'json' or 'text'",
    ])
}

/// A markup element together with its content
#[derive(PartialEq, Eq, Debug)]
pub struct Element {
    tag: String,
    attrs: Vec<(String, String)>,
    content: Vec<Item>,
}

impl Element {
    pub fn new(tag: String, attrs: Vec<(String, String)>, content: Vec<Item>) -> Self {
        Element {
            tag,
            attrs,
            content,
        }
    }

    pub fn tag(&self) -> &str {
        &self.tag
    }

    pub fn attrs(&self) -> &Vec<(String, String)> {
        &self.attrs
    }

    pub fn items(&self) -> &Vec<Item> {
        &self.content
    }
}

/// An item in the model, either an element or text content
#[derive(PartialEq, Eq, Debug)]
pub enum Item {
    Element(Element),
    Content(String),
}

/// A specific serialisation (e.g. HTML5, XML, ...)
pub trait MarkupSerialiser {
    fn serialise(&mut self, root: Element) -> Result<(), RenderError>;
}

/// A partial element in construction, still expecting completion
#[derive(Debug)]
pub enum Expectation {
    /// Expecting a tag name
    EmptyElement,
    /// Expecting attributes
    TaggedElement(String),
    /// An open element receiving content or close
    OpenElement(Element),
    /// Accumulating attributes, expecting next key or close
    EvenAttrAccumulation(Vec<(String, String)>),
    /// Accumulating attributes, expecting a value
    OddAttrAccumulation(Vec<(String, String)>, String),
}

impl Expectation {
    /// Feed a string value to the current expectation
    pub fn feed_str(&mut self, value: String) {
        match self {
            Expectation::EmptyElement => *self = Expectation::TaggedElement(value),
            Expectation::OpenElement(e) => e.content.push(Item::Content(value)),
            Expectation::EvenAttrAccumulation(attrs) => {
                *self = Expectation::OddAttrAccumulation(attrs.clone(), value)
            }
            Expectation::OddAttrAccumulation(attrs, k) => {
                *self = {
                    attrs.push((k.clone(), value));
                    Expectation::EvenAttrAccumulation(attrs.to_vec())
                }
            }
            _ => {}
        }
    }

    /// Feed an attribute block to the current expectation
    pub fn feed_block(&mut self, block: Vec<(String, String)>) {
        if let Expectation::TaggedElement(tag) = self {
            *self = Expectation::OpenElement(Element::new(tag.clone(), block, vec![]))
        }
    }

    /// Feed a child element to the current expectation
    pub fn feed_element(&mut self, element: Element) {
        if let Expectation::OpenElement(e) = self {
            e.content.push(Item::Element(element))
        }
    }
}

/// An internal emitter requiring mutable state
///
/// Wrapped as interior of MarkupEmitter
pub struct MarkupEmitterInternal<S: MarkupSerialiser> {
    stack: Vec<Expectation>,
    result: Option<Element>,
    markup_serialiser: S,
}

impl<S> MarkupEmitterInternal<S>
where
    S: MarkupSerialiser,
{
    pub fn new(markup_serialiser: S) -> Self {
        MarkupEmitterInternal {
            stack: Vec::new(),
            result: None,
            markup_serialiser,
        }
    }

    pub fn serialiser(&self) -> &S {
        &self.markup_serialiser
    }

    /// Describe why `event` cannot be accepted in the current state, or
    /// `None` if it can.
    ///
    /// A markup document is a hiccup element — a sequence of tag, attribute
    /// block and contents — so at the document root only a sequence start
    /// makes sense. Anything else means the value being rendered is not
    /// markup at all, which used to abort the process partway through
    /// (a block root) or emit nothing at all (a scalar root); both now
    /// become a diagnostic naming the shape that was found (eu-1tkk.7.24).
    fn unacceptable(&self, event: &Event) -> Option<Rejection> {
        // At the document root nothing has been opened yet and no element
        // has been completed.
        let at_root = self.stack.is_empty() && self.result.is_none();
        match event {
            Event::OutputBlockStart(_) if at_root => Some(markup_rejection(
                "the value to render is a block, but markup output needs a \
                 hiccup element — a list whose first item is the tag",
            )),
            Event::OutputScalar(_, _) if at_root => Some(markup_rejection(
                "the value to render is a single scalar, but markup output \
                 needs a hiccup element — a list whose first item is the tag",
            )),
            // Guards for the two shapes that previously reached a panic!
            // inside `emit`. Both mean an attribute block was left open
            // where an element was expected.
            Event::OutputSequenceEnd
                if matches!(
                    self.stack.last(),
                    Some(Expectation::EvenAttrAccumulation(_))
                        | Some(Expectation::OddAttrAccumulation(_, _))
                ) =>
            {
                Some(markup_rejection(
                    "an attribute block is still open where markup output \
                     expected an element to close",
                ))
            }
            Event::OutputBlockEnd
                if self
                    .stack
                    .last()
                    .is_some_and(|e| !matches!(e, Expectation::EvenAttrAccumulation(_))) =>
            {
                Some(markup_rejection(
                    "a block was closed where markup output expected an \
                     attribute block; the value being rendered is not hiccup \
                     markup",
                ))
            }
            _ => None,
        }
    }

    fn emit(&mut self, event: Event) {
        match event {
            Event::OutputScalar(_, prim) => {
                if let Some(mut top) = self.stack.pop() {
                    top.feed_str(as_text(&prim));
                    self.stack.push(top);
                }
            }
            Event::OutputSequenceStart(_) => {
                self.stack.push(Expectation::EmptyElement);
            }
            Event::OutputSequenceEnd => {
                if let Some(top) = self.stack.pop() {
                    let element = match top {
                        Expectation::TaggedElement(tag) => Element::new(tag, vec![], vec![]),
                        Expectation::OpenElement(e) => Element::new(e.tag, e.attrs, e.content),
                        Expectation::EmptyElement => return, // discard
                        // Rejected by `unacceptable` before reaching here;
                        // drop the partial element rather than aborting the
                        // process should another route arrive (eu-1tkk.7.24).
                        _ => return,
                    };

                    if let Some(mut top) = self.stack.pop() {
                        top.feed_element(element);
                        self.stack.push(top);
                    } else {
                        // We're done
                        self.result = Some(element)
                    }
                }
            }
            Event::OutputBlockStart(_) => {
                self.stack.push(Expectation::EvenAttrAccumulation(vec![]))
            }
            Event::OutputBlockEnd => {
                if let Some(top) = self.stack.pop() {
                    let attrs = match top {
                        Expectation::EvenAttrAccumulation(attrs) => attrs,
                        // Rejected by `unacceptable` before reaching here;
                        // drop the event rather than aborting the process
                        // should another route arrive (eu-1tkk.7.24).
                        _ => return,
                    };

                    if let Some(mut top) = self.stack.pop() {
                        top.feed_block(attrs);
                        self.stack.push(top);
                    } else {
                        // attrs with no element!
                    }
                }
            }
            Event::OutputDocumentStart => {}
            Event::OutputDocumentEnd => {} // leave for now
            Event::OutputStreamStart => {}
            Event::OutputStreamEnd => {
                if let Some(root) = self.result.take() {
                    self.markup_serialiser
                        .serialise(root)
                        .expect("failed to serialise markup");
                }
            }
            _ => {}
        }
    }
}

pub struct MarkupEmitter<S>
where
    S: MarkupSerialiser,
{
    internal: RefCell<MarkupEmitterInternal<S>>,
}

impl<S> MarkupEmitter<S>
where
    S: MarkupSerialiser,
{
    pub fn new(markup_serialiser: S) -> Self {
        MarkupEmitter {
            internal: RefCell::new(MarkupEmitterInternal::new(markup_serialiser)),
        }
    }
}

impl<S> Emitter for MarkupEmitter<S>
where
    S: MarkupSerialiser,
{
    fn format_name(&self) -> &'static str {
        "html"
    }

    fn emit(&mut self, event: Event) {
        self.internal.borrow_mut().emit(event)
    }

    fn unacceptable(&self, event: &Event) -> Option<Rejection> {
        self.internal.borrow().unacceptable(event)
    }
}

/// Render value as text for tag, content or attribute key / value.
fn as_text(prim: &Primitive) -> String {
    match prim {
        Primitive::Null => "".to_string(),
        Primitive::Bool(b) => {
            if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        Primitive::Sym(s) => s.clone(),
        Primitive::Str(s) => s.clone(),
        Primitive::Num(n) => format!("{n}"),
        Primitive::ZonedDateTime(dt) => format!("{dt}"),
    }
}

#[cfg(test)]
pub mod tests {

    use crate::eval::emit::RenderMetadata;

    use super::*;

    #[derive(Default)]
    pub struct TestSerialiser {
        pub root: Option<Element>,
    }

    impl MarkupSerialiser for TestSerialiser {
        fn serialise(&mut self, root: Element) -> Result<(), RenderError> {
            self.root = Some(root);
            Ok(())
        }
    }

    #[test]
    pub fn test_single_empty_div() {
        let events = vec![
            Event::OutputStreamStart,
            Event::OutputDocumentStart,
            Event::OutputSequenceStart(RenderMetadata::empty()),
            Event::OutputScalar(RenderMetadata::empty(), Primitive::Sym("div".to_string())),
            Event::OutputBlockStart(RenderMetadata::empty()),
            Event::OutputBlockEnd,
            Event::OutputSequenceEnd,
            Event::OutputDocumentEnd,
            Event::OutputStreamEnd,
        ];

        let serialiser = TestSerialiser::default();
        let mut emitter = MarkupEmitterInternal::new(serialiser);

        for e in events {
            emitter.emit(e);
        }

        assert_eq!(
            emitter.serialiser().root,
            Some(Element::new("div".to_string(), vec![], vec![]))
        );
    }

    #[test]
    pub fn test_nested_elements() {
        let events = vec![
            Event::OutputStreamStart,
            Event::OutputDocumentStart,
            Event::OutputSequenceStart(RenderMetadata::empty()),
            Event::OutputScalar(RenderMetadata::empty(), Primitive::Sym("div".to_string())),
            Event::OutputBlockStart(RenderMetadata::empty()),
            Event::OutputBlockEnd,
            Event::OutputSequenceStart(RenderMetadata::empty()),
            Event::OutputScalar(RenderMetadata::empty(), Primitive::Sym("span".to_string())),
            Event::OutputBlockStart(RenderMetadata::empty()),
            Event::OutputBlockEnd,
            Event::OutputSequenceEnd,
            Event::OutputSequenceEnd,
            Event::OutputDocumentEnd,
            Event::OutputStreamEnd,
        ];

        let serialiser = TestSerialiser::default();
        let mut emitter = MarkupEmitterInternal::new(serialiser);

        for e in events {
            emitter.emit(e);
        }

        assert_eq!(
            emitter.serialiser().root,
            Some(Element::new(
                "div".to_string(),
                vec![],
                vec![Item::Element(Element::new(
                    "span".to_string(),
                    vec![],
                    vec![]
                ))]
            ))
        );
    }

    #[test]
    pub fn test_attrs() {
        let events = vec![
            Event::OutputStreamStart,
            Event::OutputDocumentStart,
            Event::OutputSequenceStart(RenderMetadata::empty()),
            Event::OutputScalar(RenderMetadata::empty(), Primitive::Sym("div".to_string())),
            Event::OutputBlockStart(RenderMetadata::empty()),
            Event::OutputScalar(RenderMetadata::empty(), Primitive::Sym("id".to_string())),
            Event::OutputScalar(RenderMetadata::empty(), Primitive::Str("foo".to_string())),
            Event::OutputBlockEnd,
            Event::OutputSequenceEnd,
            Event::OutputDocumentEnd,
            Event::OutputStreamEnd,
        ];

        let serialiser = TestSerialiser::default();
        let mut emitter = MarkupEmitterInternal::new(serialiser);

        for e in events {
            emitter.emit(e);
        }

        assert_eq!(
            emitter.serialiser().root,
            Some(Element::new(
                "div".to_string(),
                vec![("id".to_string(), "foo".to_string())],
                vec![],
            ))
        );
    }
}
