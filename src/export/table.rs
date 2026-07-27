//! Table/object/block structured output use a common emitter state

use crate::eval::{
    emit::{Event, RenderMetadata},
    primitive::Primitive,
};

/// Convert an object / hash value into a key
pub trait AsKey<K> {
    fn as_key(&self) -> K;
}

/// Construct a value from a primitive
pub trait FromPrimitive {
    fn from_primitive(metadata: RenderMetadata, primitive: &Primitive) -> Self;
}

/// Construct a value from a list of values
pub trait FromVec<V> {
    fn from_vec(metadata: RenderMetadata, v: Vec<V>) -> Self;
}

/// Construct a value from a list of key / value pairs
pub trait FromPairs<K, V> {
    fn from_pairs(metadata: RenderMetadata, pairs: Vec<(K, V)>) -> Self;
}

/// Represent core conversion state and what we're expecting next
#[derive(Debug)]
pub enum Expectation<K, V>
where
    V: AsKey<K>,
    K: Clone,
    V: Clone,
{
    /// Holding a value a ready to feed it
    Value(V),
    /// Building a list and prepared to accept another value
    ListAccumulation(RenderMetadata, Vec<V>),
    /// At a potentially complete state building a table
    EvenBlockAccumulation(RenderMetadata, Vec<(K, V)>),
    /// Holding an 'unsatisfied' key that still needs a value
    OddBlockAccumulation(RenderMetadata, Vec<(K, V)>, K),
}

impl<K, V> Expectation<K, V>
where
    V: AsKey<K>,
    K: Clone,
    V: Clone,
{
    fn feed(&mut self, val: V) {
        match self {
            Expectation::Value(_) => *self = Expectation::Value(val),
            Expectation::ListAccumulation(_, ref mut items) => items.push(val),
            Expectation::EvenBlockAccumulation(metadata, items) => {
                *self = Expectation::OddBlockAccumulation(
                    metadata.clone(),
                    items.to_vec(),
                    val.as_key(),
                )
            }
            Expectation::OddBlockAccumulation(metadata, items, key) => {
                items.push((key.clone(), val));
                *self = Expectation::EvenBlockAccumulation(metadata.clone(), items.to_vec())
            }
        }
    }
}

/// Accumulates the document structure using a stack of Expectation
pub struct TableAccumulator<K, V>
where
    V: AsKey<K>,
    V: FromPrimitive,
    V: FromVec<V>,
    V: FromPairs<K, V>,
    K: Clone,
    V: Clone,
{
    stack: Vec<Expectation<K, V>>,
    result: Option<V>,
    /// Whether a document has been opened and not yet closed.
    ///
    /// The driver ends the output stream even when execution failed part
    /// way through rendering, so `OutputStreamEnd` can arrive with a
    /// half-built document on the stack. Flushing that would write a
    /// truncated (typically empty) document to the output alongside the
    /// error. Track completion so only a document that saw its matching
    /// `OutputDocumentEnd` becomes a result (eu-1tkk.7.20).
    document_open: bool,
}

impl<K, V> Default for TableAccumulator<K, V>
where
    V: AsKey<K>,
    V: FromPrimitive,
    V: FromVec<V>,
    V: FromPairs<K, V>,
    K: Clone,
    V: Clone,
{
    fn default() -> Self {
        Self {
            stack: vec![],
            result: None,
            document_open: false,
        }
    }
}

impl<K, V> TableAccumulator<K, V>
where
    V: AsKey<K>,
    V: FromPrimitive,
    V: FromVec<V>,
    V: FromPairs<K, V>,
    K: Clone,
    V: Clone,
{
    /// Feed a value to the top expectation on the stack, collapsing
    /// downard if it is complete
    fn feed(&mut self, val: V) {
        if let Some(mut top) = self.stack.pop() {
            top.feed(val);
            self.stack.push(top);
        } else {
            self.stack.push(Expectation::Value(val))
        }
    }

    /// Emit TOML events
    pub fn consume(&mut self, event: Event) {
        match event {
            Event::OutputScalar(metadata, prim) => {
                self.feed(V::from_primitive(metadata, &prim));
            }
            Event::OutputSequenceStart(metadata) => self
                .stack
                .push(Expectation::ListAccumulation(metadata, vec![])),
            Event::OutputSequenceEnd => {
                if let Some(Expectation::ListAccumulation(metadata, items)) = self.stack.pop() {
                    self.feed(V::from_vec(metadata, items))
                }
            }
            Event::OutputBlockStart(metadata) => self
                .stack
                .push(Expectation::EvenBlockAccumulation(metadata, vec![])),
            Event::OutputBlockEnd => {
                if let Some(Expectation::EvenBlockAccumulation(metadata, items)) = self.stack.pop()
                {
                    self.feed(V::from_pairs(metadata, items))
                }
            }
            Event::OutputDocumentStart => {
                self.document_open = true;
                self.stack.push(Expectation::Value(V::from_pairs(
                    RenderMetadata::empty(),
                    vec![],
                )));
            }
            Event::OutputDocumentEnd => self.document_open = false,
            Event::OutputStreamStart => {}
            Event::OutputStreamEnd => {
                // A document still open here was abandoned mid-render
                // (rendering failed); discard it rather than writing a
                // truncated document out.
                if self.document_open {
                    return;
                }
                if let Some(Expectation::Value(val)) = self.stack.pop() {
                    self.result = Some(val)
                }
            }
            _ => {}
        }
    }

    /// Return result (if complete)
    pub fn result(&self) -> Option<&V> {
        self.result.as_ref()
    }

    /// Whether the next scalar consumed will become a block key.
    ///
    /// Formats whose keys are restricted — JSON and TOML have string keys
    /// only, and eucalypt's own syntax needs a name — use this to reject a
    /// key their output cannot carry, rather than discovering it inside
    /// `AsKey::as_key` where there is no error channel (eu-1z503).
    pub fn expecting_key(&self) -> bool {
        matches!(
            self.stack.last(),
            Some(Expectation::EvenBlockAccumulation(_, _))
        )
    }
}
