//! Emitting execution events

use crate::eval::primitive::Primitive;
use crate::export::error::RenderError;

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
    /// Reserved for future YAML alias support
    OutputAlias,
}

/// An emitter's refusal to render something, with remediation.
///
/// The emitter is the only thing that knows both why its format cannot take
/// the value or shape and what the caller should do instead, so it supplies
/// the diagnostic notes rather than leaving `error.rs` to infer them from
/// the wording of `reason` (eu-1tkk.7.28).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rejection {
    /// What the format cannot take, as a sentence fragment completing
    /// "cannot represent this value in YAML output: ...".
    pub reason: String,
    /// How to proceed, rendered as diagnostic notes.
    pub notes: Vec<String>,
}

impl Rejection {
    pub fn new(reason: impl Into<String>) -> Self {
        Rejection {
            reason: reason.into(),
            notes: Vec::new(),
        }
    }

    pub fn with_notes<I, S>(mut self, notes: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.notes = notes.into_iter().map(Into::into).collect();
        self
    }
}

/// Trait via which machines emit events
pub trait Emitter {
    /// Emit an `Event`.
    ///
    /// Fallible because the output stream can fail underneath us: the reader
    /// of a pipe can stop listening, a disk can fill. Returning the error
    /// rather than panicking lets the driver exit quietly on a broken pipe
    /// and report everything else as a diagnostic (eu-1tkk.7.25).
    fn emit(&mut self, event: Event) -> Result<(), RenderError>;

    /// Some implementations may keep a buffer or capture of events
    fn captures(&self) -> &[Event] {
        &[]
    }

    /// The name of the output format produced, as it should appear in
    /// diagnostics (e.g. "YAML").
    fn format_name(&self) -> &'static str {
        "output"
    }

    /// Describe why `primitive` cannot be faithfully represented in this
    /// emitter's output format, or `None` if it can.
    ///
    /// Not every eucalypt value fits every output format — a TOML table has
    /// no null, a YAML integer is bounded by `i64`. Emitters whose format
    /// has such a limit override this so the emit intrinsics can raise an
    /// `ExecutionError` carrying the source location of the offending value,
    /// instead of the serialiser failing part-way through a document with no
    /// context (eu-1tkk.7.20).
    fn unrepresentable(&self, _primitive: &Primitive) -> Option<Rejection> {
        None
    }

    /// Describe why `event` cannot be accepted here, given what this emitter
    /// has already received, or `None` if it can.
    ///
    /// Where `unrepresentable` is about a single value being out of range,
    /// this is about document *shape*: a format may demand a particular
    /// structure — html renders hiccup markup and nothing else — and only
    /// the emitter knows whether the events so far are building one. Asking
    /// before the event is consumed lets the emit intrinsics raise an
    /// `ExecutionError` with a source location rather than the emitter
    /// failing on an internal invariant (eu-1tkk.7.24).
    fn unacceptable(&self, _event: &Event) -> Option<Rejection> {
        None
    }

    /// Output a stream start event
    fn stream_start(&mut self) -> Result<(), RenderError> {
        self.emit(Event::OutputStreamStart)
    }

    /// Output a stream end event
    fn stream_end(&mut self) -> Result<(), RenderError> {
        self.emit(Event::OutputStreamEnd)
    }

    /// Output doc start event
    fn doc_start(&mut self) -> Result<(), RenderError> {
        self.emit(Event::OutputDocumentStart)
    }

    /// Output doc end event
    fn doc_end(&mut self) -> Result<(), RenderError> {
        self.emit(Event::OutputDocumentEnd)
    }

    /// Output a scalar value
    fn scalar(
        &mut self,
        metadata: &RenderMetadata,
        primitive: &Primitive,
    ) -> Result<(), RenderError> {
        self.emit(Event::OutputScalar(metadata.clone(), primitive.clone()))
    }

    /// Output a sequence start
    fn sequence_start(&mut self, metadata: &RenderMetadata) -> Result<(), RenderError> {
        self.emit(Event::OutputSequenceStart(metadata.clone()))
    }

    /// Output a sequence end
    fn sequence_end(&mut self) -> Result<(), RenderError> {
        self.emit(Event::OutputSequenceEnd)
    }

    /// Output a block start
    fn block_start(&mut self, metadata: &RenderMetadata) -> Result<(), RenderError> {
        self.emit(Event::OutputBlockStart(metadata.clone()))
    }

    /// Output a block end
    fn block_end(&mut self) -> Result<(), RenderError> {
        self.emit(Event::OutputBlockEnd)
    }
}

/// A trivial emitter which does nothing
#[derive(Default)]
pub struct NullEmitter;

impl Emitter for NullEmitter {
    fn emit(&mut self, _event: Event) -> Result<(), RenderError> {
        Ok(())
    }
}

/// A trivial emitter which simply printlns events to stdout
#[derive(Default)]
pub struct DebugEmitter();

impl Emitter for DebugEmitter {
    fn emit(&mut self, event: Event) -> Result<(), RenderError> {
        println!("{event:?}");
        Ok(())
    }
}

/// An emitter for testing that captures events
#[derive(Default)]
pub struct CapturingEmitter {
    events: Vec<Event>,
}

impl Emitter for CapturingEmitter {
    fn emit(&mut self, event: Event) -> Result<(), RenderError> {
        self.events.push(event);
        Ok(())
    }

    fn captures(&self) -> &[Event] {
        self.events.as_slice()
    }
}
