//! Emitting execution events

use crate::eval::primitive::Primitive;

/// Metadata controlling or contributing to output
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenderMetadata {
    tag: Option<String>,
}

impl RenderMetadata {
    pub fn new(tag: Option<String>) -> Self {
        RenderMetadata { tag }
    }

    pub fn empty() -> Self {
        RenderMetadata { tag: None }
    }

    pub fn tag(&self) -> &Option<String> {
        &self.tag
    }
}

/// Machine events for rendering output
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Event {
    /// Emitted before all output
    OutputStreamStart,
    /// Emitted after all output
    OutputStreamEnd,
    /// Emitted at start of individual document
    OutputDocumentStart,
    /// Emitted at end of individual document
    OutputDocumentEnd,
    /// Output a primitive value
    OutputScalar(RenderMetadata, Primitive),
    /// Output a sequence start
    OutputSequenceStart(RenderMetadata),
    /// Output a sequence end
    OutputSequenceEnd,
    /// Output a block start
    OutputBlockStart(RenderMetadata),
    /// Output a block end
    OutputBlockEnd,
    /// Reserved: TODO: YAML aliases...
    OutputAlias,
}

/// Trait via which machines emit events
pub trait Emitter {
    /// Emit an `Event`
    fn emit(&mut self, event: Event);

    /// Some implementations may keep a buffer or capture of events
    fn captures(&self) -> &[Event] {
        &[]
    }

    /// Output a stream start event
    fn stream_start(&mut self) {
        self.emit(Event::OutputStreamStart);
    }

    /// Output a stream end event
    fn stream_end(&mut self) {
        self.emit(Event::OutputStreamEnd);
    }

    /// Output doc start event
    fn doc_start(&mut self) {
        self.emit(Event::OutputDocumentStart);
    }

    /// Output doc end event
    fn doc_end(&mut self) {
        self.emit(Event::OutputDocumentEnd)
    }

    /// Output a scalar value
    fn scalar(&mut self, metadata: &RenderMetadata, primitive: &Primitive) {
        self.emit(Event::OutputScalar(metadata.clone(), primitive.clone()))
    }

    /// Output a sequence start
    fn sequence_start(&mut self, metadata: &RenderMetadata) {
        self.emit(Event::OutputSequenceStart(metadata.clone()))
    }

    /// Output a sequence end
    fn sequence_end(&mut self) {
        self.emit(Event::OutputSequenceEnd)
    }

    /// Output a block start
    fn block_start(&mut self, metadata: &RenderMetadata) {
        self.emit(Event::OutputBlockStart(metadata.clone()))
    }

    /// Output a block end
    fn block_end(&mut self) {
        self.emit(Event::OutputBlockEnd)
    }
}

/// A trivial emitter which does nothing
#[derive(Default)]
pub struct NullEmitter;

impl Emitter for NullEmitter {
    fn emit(&mut self, _event: Event) {}
}

/// A trivial emitter which simply printlns events to stdout
#[derive(Default)]
pub struct DebugEmitter();

impl Emitter for DebugEmitter {
    fn emit(&mut self, event: Event) {
        println!("{event:?}")
    }
}

/// An emitter for testing that captures events
#[derive(Default)]
pub struct CapturingEmitter {
    events: Vec<Event>,
}

impl Emitter for CapturingEmitter {
    fn emit(&mut self, event: Event) {
        self.events.push(event);
    }

    fn captures(&self) -> &[Event] {
        self.events.as_slice()
    }
}
