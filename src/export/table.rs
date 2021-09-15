//! Table/object/block structured output use a common emitter state

use crate::eval::{emit::Event, primitive::Primitive};

/// Convert an object / hash value into a key
pub trait AsKey<K> {
    fn as_key(&self) -> K;
}

/// Construct a value from a primitive
pub trait FromPrimitive {
    fn from_primitive(primitive: &Primitive) -> Self;
}

/// Construct a value from a list of values
pub trait FromVec<V> {
    fn from_vec(v: Vec<V>) -> Self;
}

/// Construct a value from a list of key / value pairs
pub trait FromPairs<K, V> {
    fn from_pairs(pairs: Vec<(K, V)>) -> Self;
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
    ListAccumulation(Vec<V>),
    /// At a potentially complete state building a table
    EvenBlockAccumulation(Vec<(K, V)>),
    /// Holding an 'unsatisfied' key that still needs a value
    OddBlockAccumulation(Vec<(K, V)>, K),
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
            Expectation::ListAccumulation(ref mut items) => items.push(val),
            Expectation::EvenBlockAccumulation(items) => {
                *self = Expectation::OddBlockAccumulation(items.to_vec(), val.as_key())
            }
            Expectation::OddBlockAccumulation(items, key) => {
                items.push((key.clone(), val));
                *self = Expectation::EvenBlockAccumulation(items.to_vec())
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
            Event::OutputScalar(_, prim) => {
                self.feed(V::from_primitive(&prim));
            }
            Event::OutputSequenceStart(_) => self.stack.push(Expectation::ListAccumulation(vec![])),
            Event::OutputSequenceEnd => {
                if let Some(Expectation::ListAccumulation(items)) = self.stack.pop() {
                    self.feed(V::from_vec(items))
                }
            }
            Event::OutputBlockStart(_) => {
                self.stack.push(Expectation::EvenBlockAccumulation(vec![]))
            }
            Event::OutputBlockEnd => {
                if let Some(Expectation::EvenBlockAccumulation(items)) = self.stack.pop() {
                    self.feed(V::from_pairs(items))
                }
            }
            Event::OutputDocumentStart => {
                self.stack.push(Expectation::Value(V::from_pairs(vec![])));
            }
            Event::OutputDocumentEnd => {} // leave for now
            Event::OutputStreamStart => {}
            Event::OutputStreamEnd => {
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
}
