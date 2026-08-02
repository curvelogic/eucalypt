//! Type diagnostic messages for the gradual type checker.
//!
//! `TypeWarning` is the primary diagnostic produced by the type checker.
//! Type issues are always warnings, never errors — they do not block evaluation.
use crate::common::sourcemap::{HasSmid, Smid, SourceMap};
use codespan_reporting::diagnostic::{Diagnostic, Label};

/// Identifies the call an argument-level type warning arose from.
///
/// `smid` on the warning itself points at the offending *argument*; this
/// records the *call* that argument was passed to, which is what a later
/// runtime error is normally blamed on. Two locations are kept because the
/// two are equally valid identifications of one call and different parts of
/// the diagnostic pipeline choose differently between them:
///
/// * `head` — the callee reference (`nth` in `nth(xs, 0)`)
/// * `app`  — the application node (the whole `nth(xs, 0)`)
///
/// Used by eu-1tkk.7.40 to match a warning to a runtime error across the
/// process's two independent `SourceMap`s.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CallSite {
    /// Location of the callee reference in the application spine.
    pub head: Smid,
    /// Location of the application node itself.
    pub app: Smid,
}

impl CallSite {
    /// Construct a call site from its head and application locations.
    pub fn new(head: Smid, app: Smid) -> Self {
        CallSite { head, app }
    }
}

/// A type-level warning emitted by the gradual type checker.
///
/// Type issues are reported as warnings so that they never prevent evaluation.
/// The `--strict` flag in `eu check` can promote these to errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeWarning {
    /// Human-readable description of the warning.
    pub message: String,
    /// Source location associated with the warning.
    pub smid: Smid,
    /// The expected type (as a display string), if applicable.
    pub expected: Option<String>,
    /// The found type (as a display string), if applicable.
    pub found: Option<String>,
    /// Additional notes shown below the primary diagnostic.
    pub notes: Vec<String>,
    /// The call this warning's argument was passed to, when the warning is an
    /// argument/parameter mismatch. `None` for every other warning kind, which
    /// keeps non-call warnings out of the runtime-error match entirely.
    pub call_site: Option<CallSite>,
}

impl HasSmid for TypeWarning {
    fn smid(&self) -> Smid {
        self.smid
    }
}

impl std::fmt::Display for TypeWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl TypeWarning {
    /// Construct a simple type warning with no location or type details.
    pub fn new(message: impl Into<String>) -> Self {
        TypeWarning {
            message: message.into(),
            smid: Smid::default(),
            expected: None,
            found: None,
            notes: vec![],
            call_site: None,
        }
    }

    /// Attach a source location (Smid) to this warning.
    pub fn at(mut self, smid: Smid) -> Self {
        self.smid = smid;
        self
    }

    /// Record the call this warning's argument was passed to (eu-1tkk.7.40).
    pub fn at_call(mut self, call_site: CallSite) -> Self {
        self.call_site = Some(call_site);
        self
    }

    /// Attach expected/found type information to this warning.
    pub fn with_types(mut self, expected: impl Into<String>, found: impl Into<String>) -> Self {
        self.expected = Some(expected.into());
        self.found = Some(found.into());
        self
    }

    /// Append a note to this warning.
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Convert this warning to a `codespan-reporting` `Diagnostic::warning()`.
    ///
    /// Uses the source map to resolve the Smid to a file and span if possible.
    pub fn to_diagnostic(&self, source_map: &SourceMap) -> Diagnostic<usize> {
        let mut diag = Diagnostic::warning().with_message(&self.message);

        if let Some(info) = source_map.source_info_for_smid(self.smid) {
            if let (Some(file), Some(span)) = (info.file, info.span) {
                let mut label = Label::primary(file, span);
                // Annotate the label with type information when available
                if let (Some(exp), Some(fnd)) = (&self.expected, &self.found) {
                    label = label.with_message(format!("expected {exp}, found {fnd}"));
                }
                diag = diag.with_labels(vec![label]);
            }
        }

        if !self.notes.is_empty() {
            diag = diag.with_notes(self.notes.clone());
        }

        diag
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::sourcemap::SourceMap;

    #[test]
    fn type_warning_renders_as_warning_diagnostic() {
        let source_map = SourceMap::new();
        let warning = TypeWarning::new("type mismatch");
        let diag = warning.to_diagnostic(&source_map);
        // The diagnostic should carry warning severity
        use codespan_reporting::diagnostic::Severity;
        assert_eq!(diag.severity, Severity::Warning);
        assert_eq!(diag.message, "type mismatch");
    }

    #[test]
    fn type_warning_with_types_and_note() {
        let source_map = SourceMap::new();
        let warning = TypeWarning::new("type mismatch")
            .with_types("number", "string")
            .with_note("double expects a number argument");
        let diag = warning.to_diagnostic(&source_map);
        use codespan_reporting::diagnostic::Severity;
        assert_eq!(diag.severity, Severity::Warning);
        assert_eq!(diag.notes, vec!["double expects a number argument"]);
        assert!(warning.expected.as_deref() == Some("number"));
        assert!(warning.found.as_deref() == Some("string"));
    }
}
