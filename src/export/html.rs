//! HTML5 exporter

use super::{
    error::RenderError,
    markup::{Element, Item, MarkupEmitter, MarkupSerialiser},
};
use html5ever::serialize::{HtmlSerializer, Serialize, SerializeOpts, Serializer, TraversalScope};
use html5ever::{namespace_url, ns, LocalName, QualName};

use std::{
    io::{Error, Write},
    iter,
};

/// Serialise a markup element and its descendants to HTML
impl Serialize for Element {
    fn serialize<S>(&self, serializer: &mut S, traversal_scope: TraversalScope) -> Result<(), Error>
    where
        S: Serializer,
    {
        let tag_name = QualName::new(None, ns!(), LocalName::from(self.tag()));
        let keys: Vec<_> = self
            .attrs()
            .iter()
            .map(|(k, _)| QualName::new(None, ns!(), LocalName::from(k.as_str())))
            .collect();

        let attrs = iter::zip(keys.iter(), self.attrs().iter().map(|(_, v)| v.as_str()));

        if traversal_scope == TraversalScope::IncludeNode {
            serializer.start_elem(tag_name.clone(), attrs)?;
        }

        for i in self.items() {
            match i {
                Item::Element(e) => e.serialize(serializer, TraversalScope::IncludeNode)?,
                Item::Content(t) => serializer.write_text(t)?,
            }
        }

        if traversal_scope == TraversalScope::IncludeNode {
            serializer.end_elem(tag_name)
        } else {
            Ok(())
        }
    }
}

pub struct HtmlMarkupSerialiser<'a> {
    out: Box<dyn Write + 'a>,
}

impl<'a> HtmlMarkupSerialiser<'a> {
    pub fn new(out: &'a mut (dyn Write + 'a)) -> Self {
        HtmlMarkupSerialiser { out: Box::new(out) }
    }
}

impl MarkupSerialiser for HtmlMarkupSerialiser<'_> {
    /// Serialise the markup Element as HTML
    fn serialise(&mut self, root: Element) -> Result<(), RenderError> {
        let mut serializer = HtmlSerializer::new(
            self.out.as_mut(),
            SerializeOpts {
                scripting_enabled: true,
                traversal_scope: TraversalScope::IncludeNode,
                create_missing_parent: true,
            },
        );

        serializer.write_doctype("html")?;
        root.serialize(&mut serializer, TraversalScope::IncludeNode)?;

        writeln!(self.out)?;
        self.out.flush()?;
        Ok(())
    }
}

pub type HtmlEmitter<'a> = MarkupEmitter<HtmlMarkupSerialiser<'a>>;
