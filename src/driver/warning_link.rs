//! Carrying type-checker findings into the runtime errors they diagnose
//! (eu-1tkk.7.40).
//!
//! On an argument mistake the type-checker warning is often the best
//! diagnosis on screen — it names the offending argument and says what was
//! expected there — while the runtime error that follows describes the
//! failure in terms of the callee's internals. The two were previously
//! unconnected: the warnings were rendered and dropped before evaluation
//! began.
//!
//! # Why spans and not Smids
//!
//! The type checker on the evaluate path runs in a *separate*
//! [`SourceLoader`](crate::driver::source::SourceLoader) from the evaluator
//! (`run_type_checker` / `run_type_checker_from_blob_core` in
//! [`crate::driver::check`]), so it has its own `SourceMap` and its own file
//! database. A `Smid` from one is meaningless in the other, and so are file
//! ids. What *is* stable across the two is the source text itself, so a
//! warning is resolved to `(file name, byte span)` here, on the check side,
//! and matched against the resolved location of the runtime error later.
//!
//! # Matching is deliberately narrow
//!
//! Gradual typing makes warnings advisory, and citing the wrong one would be
//! worse than citing none. A link is made only when the runtime error's
//! primary label is *the very same call* the warning was raised against —
//! byte-identical span, same file, matched against either the callee
//! reference or the whole application. Nothing weaker (same line, same
//! declaration, same callee name) qualifies.
//!
//! # `--suppress-type-warnings`
//!
//! Under that flag the evaluate path does not run the checker at all (see
//! `src/bin/eu.rs`), so no links exist and errors render exactly as before.
//! Nothing here ever refers the reader to warning output: the argument span
//! and the expected/found text are carried into the error's own labels, so
//! the error stands on its own whether or not a warning was printed.

use codespan_reporting::files::{Files, SimpleFiles};

use crate::common::sourcemap::SourceMap;
use crate::core::typecheck::error::TypeWarning;

/// One type-checker finding about an argument, resolved to source coordinates.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArgumentFinding {
    /// Name of the file the call appears in, as registered in the file database.
    file: String,
    /// Byte span of the callee reference (`nth` in `nth(xs, 0)`).
    call_head: (usize, usize),
    /// Byte span of the application node (the whole `nth(xs, 0)`).
    call_app: Option<(usize, usize)>,
    /// Byte span of the offending argument.
    arg: (usize, usize),
    /// The finding, in the same words the warning used: `expected X, found Y`.
    label: String,
}

/// Type-checker findings retained for the duration of a run, indexed so that a
/// runtime error can recover the argument-level span for the call it blames.
#[derive(Debug, Clone, Default)]
pub struct TypeWarningLinks {
    findings: Vec<ArgumentFinding>,
}

impl TypeWarningLinks {
    /// Resolve argument-level type warnings against the check pass's own
    /// source map and file database.
    ///
    /// Warnings that are not argument mismatches, that carry no expected/found
    /// pair, or whose argument and call site cannot both be resolved to a span
    /// in the same user file are dropped — there is nothing to promote.
    pub fn build(
        warnings: &[TypeWarning],
        source_map: &SourceMap,
        files: &SimpleFiles<String, String>,
    ) -> Self {
        let mut findings = Vec::new();

        for warning in warnings {
            let Some(call_site) = warning.call_site else {
                continue;
            };
            let (Some(expected), Some(found)) = (&warning.expected, &warning.found) else {
                continue;
            };

            let resolve = |smid| {
                source_map
                    .source_info_for_smid(smid)
                    .and_then(|info| info.file.zip(info.span))
                    .map(|(file, span)| (file, (span.start().to_usize(), span.end().to_usize())))
            };

            let Some((arg_file, arg_span)) = resolve(warning.smid) else {
                continue;
            };
            let Some((head_file, head_span)) = resolve(call_site.head) else {
                continue;
            };

            // The argument and the call must be in one file, and that file
            // must be the user's — a warning about prelude internals is never
            // promoted into a user-facing error label.
            if arg_file != head_file || !source_map.is_user_file(arg_file) {
                continue;
            }

            let Ok(name) = files.name(arg_file) else {
                continue;
            };

            let app_span = resolve(call_site.app)
                .filter(|(file, _)| *file == arg_file)
                .map(|(_, span)| span);

            findings.push(ArgumentFinding {
                file: name.clone(),
                call_head: head_span,
                call_app: app_span,
                arg: arg_span,
                label: format!("expected {expected}, found {found}"),
            });
        }

        TypeWarningLinks { findings }
    }

    /// Whether any findings were retained.
    pub fn is_empty(&self) -> bool {
        self.findings.is_empty()
    }

    /// Argument spans and findings for the call located at `primary_span` in
    /// `file`, in source order and free of duplicates.
    ///
    /// A finding whose argument span *is* the primary span is dropped: the
    /// caret is already there, and a secondary label under it would only
    /// double up.
    pub fn findings_for_call(
        &self,
        file: &str,
        primary_span: (usize, usize),
    ) -> Vec<((usize, usize), String)> {
        let mut matches: Vec<((usize, usize), String)> = self
            .findings
            .iter()
            .filter(|f| {
                f.file == file
                    && (f.call_head == primary_span || f.call_app == Some(primary_span))
                    && f.arg != primary_span
            })
            .map(|f| (f.arg, f.label.clone()))
            .collect();

        matches.sort_by_key(|(span, _)| *span);
        matches.dedup();
        matches
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::sourcemap::{Smid, SourceMap};
    use crate::core::typecheck::error::CallSite;

    /// Build a source map with three spans in one user file, returning the
    /// Smids for (callee, application, argument).
    fn fixture() -> (SourceMap, SimpleFiles<String, String>, Smid, Smid, Smid) {
        let mut files = SimpleFiles::new();
        let file = files.add("swap.eu".to_string(), "result: nth(xs, 0)\n".to_string());
        let mut source_map = SourceMap::new();
        let head = source_map.add(file, codespan::Span::new(8, 11));
        let app = source_map.add(file, codespan::Span::new(8, 18));
        let arg = source_map.add(file, codespan::Span::new(12, 14));
        (source_map, files, head, app, arg)
    }

    fn warning(arg: Smid, head: Smid, app: Smid) -> TypeWarning {
        TypeWarning::new("type mismatch calling 'nth'")
            .at(arg)
            .at_call(CallSite::new(head, app))
            .with_types("number", "[a]")
    }

    #[test]
    fn argument_finding_is_recovered_from_the_callee_span() {
        let (source_map, files, head, app, arg) = fixture();
        let links = TypeWarningLinks::build(&[warning(arg, head, app)], &source_map, &files);

        assert_eq!(
            links.findings_for_call("swap.eu", (8, 11)),
            vec![((12, 14), "expected number, found [a]".to_string())]
        );
    }

    #[test]
    fn argument_finding_is_recovered_from_the_application_span() {
        let (source_map, files, head, app, arg) = fixture();
        let links = TypeWarningLinks::build(&[warning(arg, head, app)], &source_map, &files);

        assert_eq!(
            links.findings_for_call("swap.eu", (8, 18)),
            vec![((12, 14), "expected number, found [a]".to_string())]
        );
    }

    #[test]
    fn a_different_call_in_the_same_file_never_matches() {
        let (source_map, files, head, app, arg) = fixture();
        let links = TypeWarningLinks::build(&[warning(arg, head, app)], &source_map, &files);

        // Overlapping but not identical, and a wholly unrelated span.
        assert!(links.findings_for_call("swap.eu", (8, 12)).is_empty());
        assert!(links.findings_for_call("swap.eu", (0, 6)).is_empty());
    }

    #[test]
    fn the_same_span_in_a_different_file_never_matches() {
        let (source_map, files, head, app, arg) = fixture();
        let links = TypeWarningLinks::build(&[warning(arg, head, app)], &source_map, &files);

        assert!(links.findings_for_call("other.eu", (8, 11)).is_empty());
    }

    #[test]
    fn warnings_without_a_call_site_are_not_retained() {
        let (source_map, files, _head, _app, arg) = fixture();
        let plain = TypeWarning::new("head of empty list")
            .at(arg)
            .with_types("[a]", "[]");
        let links = TypeWarningLinks::build(&[plain], &source_map, &files);

        assert!(links.is_empty());
    }

    #[test]
    fn a_finding_on_the_primary_span_itself_is_dropped() {
        let (source_map, files, head, app, arg) = fixture();
        let links = TypeWarningLinks::build(&[warning(arg, head, app)], &source_map, &files);

        // Primary already sits on the argument — nothing to add.
        assert!(links.findings_for_call("swap.eu", (12, 14)).is_empty());
    }
}
