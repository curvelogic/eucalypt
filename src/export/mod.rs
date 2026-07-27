pub mod edn;
pub mod error;
pub mod eu;
pub mod html;
pub mod json;
pub mod markup;
pub mod table;
pub mod text;
pub mod toml;
pub mod yaml;

use self::html::HtmlMarkupSerialiser;

use super::export::toml::TomlEmitter;
use crate::eval::emit::Emitter;
use edn::EdnEmitter;
use eu::EuEmitter;
use html::HtmlEmitter;
use json::JsonEmitter;
use std::io::Write;
use text::TextEmitter;
use yaml::YamlEmitter;

/// Remediation shared by the formats whose integers are bounded by `i64`.
///
/// YAML and TOML both reject an integer above `i64::MAX`; the advice for
/// each is identical, so it lives here rather than being duplicated and
/// allowed to drift (eu-1tkk.7.20, eu-1tkk.7.23).
pub(crate) const INTEGER_RANGE_NOTES: [&str; 2] = [
    "render to a format that can carry the value — 'json', 'edn', 'text' and \
     'eu' output all keep integers of this magnitude",
    "to keep the exact digits in this format, convert the value to a string \
     first with 'str', e.g. 'n str'",
];

/// Reject a block key the target format cannot carry.
///
/// JSON object keys and TOML keys are strings; eucalypt's own syntax needs a
/// name. A block built with `kv-block(1, "a")` or `block([[1, "a"]])` has a
/// key that is none of those, and used to abort the process inside
/// `AsKey::as_key` (eu-1z503).
///
/// Reported rather than coerced to `"1"`, for the same reason as a TOML null
/// (eu-1tkk.7.28): a string key is not the key that was evaluated, and the
/// difference survives into anything that re-reads the output. The type
/// checker already warns on such a key, so this is confirming a mistake it
/// has flagged rather than refusing something idiomatic.
pub(crate) fn unrepresentable_key(
    format: &str,
    expecting_key: bool,
    event: &crate::eval::emit::Event,
) -> Option<crate::eval::emit::Rejection> {
    use crate::eval::emit::{Event, Rejection};
    use crate::eval::primitive::Primitive;

    if !expecting_key {
        return None;
    }
    let found = match event {
        Event::OutputScalar(_, Primitive::Str(_)) | Event::OutputScalar(_, Primitive::Sym(_)) => {
            return None
        }
        Event::OutputScalar(_, Primitive::Null) => "null".to_string(),
        Event::OutputScalar(_, Primitive::Bool(b)) => format!("the boolean {b}"),
        Event::OutputScalar(_, Primitive::Num(n)) => format!("the number {n}"),
        Event::OutputScalar(_, Primitive::ZonedDateTime(dt)) => format!("the timestamp {dt}"),
        // A list or block used as a key: rendered as a YAML complex key, but
        // json/toml/eu have no such thing.
        Event::OutputSequenceStart(_) => "a list".to_string(),
        Event::OutputBlockStart(_) => "a block".to_string(),
        _ => return None,
    };
    Some(
        Rejection::new(format!(
            "a block key is {found}, but {format} keys must be text"
        ))
        .with_notes([
            "give the block symbol keys, e.g. 'kv-block(:one, \"a\")', or \
             convert the key to text with 'str' before rendering"
                .to_string(),
            "'yaml' and 'edn' output both support keys that are not text, \
             and render this block unchanged"
                .to_string(),
        ]),
    )
}

/// Create an emitter for the format specified
///
/// Return None if the format is not recognised.
pub fn create_emitter<'a, S: AsRef<str>>(
    format: S,
    output: &'a mut (dyn Write + 'a),
) -> Option<Box<dyn Emitter + 'a>> {
    match format.as_ref() {
        "yaml" => Some(Box::new(YamlEmitter::new(output))),
        "toml" => Some(Box::new(TomlEmitter::new(output))),
        "json" => Some(Box::new(JsonEmitter::new(output))),
        "text" => Some(Box::new(TextEmitter::new(output))),
        "edn" => Some(Box::new(EdnEmitter::new(output))),
        "eu" => Some(Box::new(EuEmitter::new(output))),
        "html" => Some(Box::new(HtmlEmitter::new(HtmlMarkupSerialiser::new(
            output,
        )))),
        _ => None,
    }
}

#[cfg(test)]
mod write_failure_tests {
    //! Every emitter must surface a write failure rather than panicking, and
    //! must distinguish a closed pipe from a real error (eu-1tkk.7.25).
    //!
    //! Driven through a `Write` that fails with a chosen `ErrorKind`, so the
    //! ENOSPC case — which cannot be provoked from a shell without filling a
    //! filesystem — is covered as directly as the broken-pipe case.

    use super::*;
    use crate::eval::emit::{Event, RenderMetadata};
    use crate::eval::primitive::Primitive;
    use std::io::{self, ErrorKind};

    /// A sink whose every write fails with `kind`.
    struct FailingWriter {
        kind: ErrorKind,
    }

    impl Write for FailingWriter {
        fn write(&mut self, _buf: &[u8]) -> io::Result<usize> {
            Err(io::Error::new(self.kind, "injected write failure"))
        }
        fn flush(&mut self) -> io::Result<()> {
            Err(io::Error::new(self.kind, "injected flush failure"))
        }
    }

    /// Formats whose emitters write to the output stream. `html` is covered
    /// separately: it needs a hiccup document to produce any output at all.
    const DOCUMENT_FORMATS: [&str; 6] = ["yaml", "json", "toml", "edn", "text", "eu"];

    /// Drive a minimal single-scalar document through `format`, returning
    /// the first error any step produced.
    fn render_scalar_to(format: &str, kind: ErrorKind) -> Option<error::RenderError> {
        let mut writer = FailingWriter { kind };
        let mut emitter = create_emitter(format, &mut writer).expect("known format");
        let events = [
            Event::OutputStreamStart,
            Event::OutputDocumentStart,
            Event::OutputScalar(RenderMetadata::empty(), Primitive::Str("x".to_string())),
            Event::OutputDocumentEnd,
            Event::OutputStreamEnd,
        ];
        for event in events {
            if let Err(e) = emitter.emit(event) {
                return Some(e);
            }
        }
        None
    }

    /// A closed pipe must be reported as such, so the driver can exit 0
    /// silently. Every document format must agree — a format that swallowed
    /// it would print a partial document and claim success.
    #[test]
    fn every_format_reports_a_broken_pipe() {
        for format in DOCUMENT_FORMATS {
            let error = render_scalar_to(format, ErrorKind::BrokenPipe)
                .unwrap_or_else(|| panic!("{format}: expected a write error, got none"));
            assert!(
                error.is_broken_pipe(),
                "{format}: expected a broken pipe, got {error:?}"
            );
        }
    }

    /// The guard that matters: a real write failure must NOT be classified as
    /// a broken pipe. Were it to be, the driver would exit 0 on a full disk
    /// and the user would believe a truncated file was complete — strictly
    /// worse than the panic this replaced.
    #[test]
    fn a_real_write_failure_is_not_mistaken_for_a_broken_pipe() {
        for kind in [
            ErrorKind::OutOfMemory,
            ErrorKind::PermissionDenied,
            ErrorKind::WriteZero,
        ] {
            for format in DOCUMENT_FORMATS {
                let error = render_scalar_to(format, kind)
                    .unwrap_or_else(|| panic!("{format}/{kind:?}: expected a write error"));
                assert!(
                    !error.is_broken_pipe(),
                    "{format}: {kind:?} was misclassified as a broken pipe"
                );
            }
        }
    }

    /// The html path writes through a `MarkupSerialiser` rather than
    /// `writeln!`, so it needs its own coverage.
    #[test]
    fn html_reports_write_failures_too() {
        for (kind, expect_pipe) in [
            (ErrorKind::BrokenPipe, true),
            (ErrorKind::OutOfMemory, false),
        ] {
            let mut writer = FailingWriter { kind };
            let mut emitter = create_emitter("html", &mut writer).expect("html emitter");
            let events = [
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
            let mut seen = None;
            for event in events {
                if let Err(e) = emitter.emit(event) {
                    seen = Some(e);
                    break;
                }
            }
            let error = seen.unwrap_or_else(|| panic!("html/{kind:?}: expected a write error"));
            assert_eq!(
                error.is_broken_pipe(),
                expect_pipe,
                "html: {kind:?} classified wrongly ({error:?})"
            );
        }
    }
}
