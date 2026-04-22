use codespan::Span;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::{Files, SimpleFiles},
};
use std::fmt::Display;
use std::num::NonZeroU32;
use std::{fmt, ops::Range};

/// A handle that points to a source location in a source map.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Smid(Option<NonZeroU32>);

impl Default for Smid {
    /// The default SMID is invalid.
    fn default() -> Self {
        Smid(None)
    }
}

impl From<u32> for Smid {
    fn from(n: u32) -> Self {
        if n == 0 {
            Smid(None)
        } else {
            Smid(Some(NonZeroU32::new(n).unwrap()))
        }
    }
}

impl From<Smid> for u32 {
    fn from(val: Smid) -> Self {
        match val.0 {
            None => 0,
            Some(i) => i.into(),
        }
    }
}

impl Display for Smid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self.0 {
            Some(n) => write!(f, "[{n}]"),
            None => write!(f, "[?]"),
        }
    }
}

impl Smid {
    fn new(index: usize) -> Smid {
        Smid(Some(NonZeroU32::new(index as u32 + 1).unwrap()))
    }

    pub fn is_valid(&self) -> bool {
        self.0.is_some()
    }

    pub fn get(self) -> usize {
        (self.0.expect("Invalid SMID").get() - 1) as usize
    }

    pub fn sym_name(&self) -> String {
        match self.0 {
            Some(n) => format!("__{n}"),
            None => "__<nosmid>".to_string(),
        }
    }
}

#[cfg(test)]
impl Smid {
    pub fn fake(index: usize) -> Smid {
        Smid::new(index)
    }
}

/// Anything that has a SMID identifying a source location.
pub trait HasSmid {
    fn smid(&self) -> Smid;
}

/// Source information to associate with a syntax element
///
/// As well as associating a file location with a SMID, we can
/// associate annotations which are useful in synthetic cases where
/// there is no source location.
pub struct SourceInfo {
    /// usize
    pub file: Option<usize>,
    /// Byte span
    pub span: Option<Span>,
    /// Text annotation (e.g. global name)
    pub annotation: Option<String>,
}

/// Store all source info...
#[derive(Default)]
pub struct SourceMap {
    source: Vec<SourceInfo>,
    /// File IDs that correspond to resources (e.g. prelude, stdlib) rather
    /// than user-authored files.  Used by `is_user_file` so that diagnostic
    /// code can prefer user locations over resource locations when building
    /// error labels.
    resource_file_ids: std::collections::HashSet<usize>,
}

impl SourceMap {
    /// Create a new, empty database of files.
    pub fn new() -> Self {
        SourceMap::default()
    }

    /// Add a new source info and get a SMID referencing it
    pub fn add(&mut self, file: usize, span: Span) -> Smid {
        let smid = Smid::new(self.source.len());
        self.source.push(SourceInfo {
            file: Some(file),
            span: Some(span),
            annotation: None,
        });
        smid
    }

    /// Add a new source info and get a SMID referencing it
    pub fn add_annotated<T: AsRef<str>>(&mut self, file: usize, span: Span, annotation: T) -> Smid {
        let smid = Smid::new(self.source.len());
        self.source.push(SourceInfo {
            file: Some(file),
            span: Some(span),
            annotation: Some(annotation.as_ref().to_string()),
        });
        smid
    }

    /// Add a notional location which has no concrete file co-ordinate
    pub fn add_synthetic<T: AsRef<str>>(&mut self, annotation: T) -> Smid {
        let smid = Smid::new(self.source.len());
        self.source.push(SourceInfo {
            file: None,
            span: None,
            annotation: Some(annotation.as_ref().to_string()),
        });
        smid
    }

    /// Create a new source location, identical save for a new or
    /// different annotation
    pub fn annotated(&mut self, smid: Smid, annotation: String) -> Smid {
        let new_smid = Smid::new(self.source.len());
        let new_info = if let Some(info) = self.source.get(smid.get()) {
            SourceInfo {
                annotation: Some(annotation),
                ..*info
            }
        } else {
            SourceInfo {
                file: None,
                span: None,
                annotation: Some(annotation),
            }
        };
        self.source.push(new_info);
        new_smid
    }

    /// Retrieve the SourceInfo for something that has a SMID
    pub fn source_info(&self, expr: &dyn HasSmid) -> Option<&SourceInfo> {
        if expr.smid().is_valid() {
            self.source.get(expr.smid().get())
        } else {
            None
        }
    }

    /// Retrieve the SourceInfo for a given Smid value
    pub fn source_info_for_smid(&self, smid: Smid) -> Option<&SourceInfo> {
        if smid.is_valid() {
            self.source.get(smid.get())
        } else {
            None
        }
    }

    /// Create a warning diagnostic for a value with a SMID.
    ///
    /// Identical in structure to [`diagnostic`], but uses
    /// `Diagnostic::warning()` rather than `Diagnostic::error()`.
    /// Used by the type checker to emit non-blocking diagnostics.
    pub fn warning_diagnostic<W>(&self, warning: &W) -> Diagnostic<usize>
    where
        W: HasSmid + Display,
    {
        let diag = Diagnostic::warning().with_message(format!("{warning}"));

        match self.source_info(warning) {
            Some(&SourceInfo {
                file: Some(file),
                span: Some(span),
                ..
            }) => diag.with_labels(vec![Label::primary(file, span)]),
            Some(SourceInfo {
                file: None,
                annotation: Some(ref ann),
                ..
            }) => {
                if let Some(display) = intrinsic_display_name(ann) {
                    diag.with_notes(vec![format!("in {display}")])
                } else {
                    diag
                }
            }
            _ => diag,
        }
    }

    /// Create a default diagnostic for an exception with a SMID
    pub fn diagnostic<E>(&self, error: &E) -> Diagnostic<usize>
    where
        E: HasSmid + Display,
    {
        let diag = Diagnostic::error().with_message(format!("{error}"));

        match self.source_info(error) {
            Some(&SourceInfo {
                file: Some(file),
                span: Some(span),
                ..
            }) => diag.with_labels(vec![Label::primary(file, span)]),
            Some(SourceInfo {
                file: None,
                annotation: Some(ref ann),
                ..
            }) => {
                // No source location, but we have an intrinsic name.
                // Show the user-facing name as context if available.
                if let Some(display) = intrinsic_display_name(ann) {
                    diag.with_notes(vec![format!("in {display}")])
                } else {
                    diag
                }
            }
            _ => diag,
        }
    }

    /// Mark a file ID as belonging to a resource (e.g. the prelude, a stdlib
    /// module).  Diagnostics can use `is_user_file` to avoid showing resource
    /// locations as the primary error site.
    pub fn mark_resource_file(&mut self, file_id: usize) {
        self.resource_file_ids.insert(file_id);
    }

    /// Returns `true` when `file_id` belongs to a user-authored file, i.e. it
    /// has *not* been marked as a resource file.
    pub fn is_user_file(&self, file_id: usize) -> bool {
        !self.resource_file_ids.contains(&file_id)
    }

    /// Find the first Smid in a trace slice that has a concrete file/span location.
    ///
    /// Used as a fallback when an error's own Smid is synthetic (e.g. an
    /// intrinsic label) and the diagnostic would otherwise show no source
    /// location.
    pub fn first_source_smid(&self, trace: &[Smid]) -> Option<Smid> {
        trace.iter().copied().find(|smid| {
            if let Some(info) = self.source_info_for_smid(*smid) {
                info.file.is_some() && info.span.is_some()
            } else {
                false
            }
        })
    }

    /// Find the first Smid in a trace slice that has a concrete location in a
    /// *user* file (i.e. not a prelude / resource file).
    ///
    /// Used to surface user-code call sites as the primary error location when
    /// the error itself originated inside a library function.
    pub fn first_user_source_smid(&self, trace: &[Smid]) -> Option<Smid> {
        trace.iter().copied().find(|smid| {
            if let Some(info) = self.source_info_for_smid(*smid) {
                if let Some(fid) = info.file {
                    info.span.is_some() && self.is_user_file(fid)
                } else {
                    false
                }
            } else {
                false
            }
        })
    }

    /// Format a stack / environment trace
    ///
    /// Produces source-level references where file locations are
    /// available, e.g. `example.eu:5:3 (+)` for an intrinsic call at
    /// line 5 column 3, or `example.eu:2:10 (str.letters(99))` for a
    /// source expression.
    pub fn format_trace(&self, trace: &[Smid], files: &SimpleFiles<String, String>) -> String {
        // Collect entries in trace order (innermost-first from the VM),
        // then reverse so the output reads outermost-first (conventional order).
        let mut elements: Vec<_> = trace
            .iter()
            .filter_map(|smid| {
                let info = self.source.get(smid.get())?;

                // Determine the display name: prefer intrinsic display name,
                // then annotation (function name), then source snippet
                let display_name = info
                    .annotation
                    .as_deref()
                    .and_then(|a| intrinsic_display_name(a).or(Some(a)));

                let source_snippet = || -> Option<String> {
                    let id = info.file?;
                    let source: &str = files.source(id).ok()?;
                    let span = info.span?;
                    let raw = source.get(Range::from(span))?;
                    // Truncate to first line as a safety net
                    let first_line = raw.lines().next().unwrap_or(raw);
                    if first_line.len() < raw.len() {
                        Some(format!("{first_line}…"))
                    } else {
                        Some(first_line.to_string())
                    }
                };

                // Build file:line:col location string if we have a source location
                let location = info.file.and_then(|id| {
                    let name = files.name(id).ok()?;
                    let span = info.span?;
                    let loc = files.location(id, span.start().to_usize()).ok()?;
                    // Strip directory prefix for readability
                    let short_name = std::path::Path::new(&name)
                        .file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or(&name);
                    Some(format!(
                        "{short_name}:{line}:{col}",
                        line = loc.line_number,
                        col = loc.column_number
                    ))
                });

                // Only include entries that have a user-visible name or source location.
                // Entries with neither are internal machinery and are silently dropped.
                let name = display_name
                    .map(|s| s.to_string())
                    .or_else(source_snippet)?;

                // Format: "name at file:line:col" or just "name" if no location
                let entry = match location {
                    Some(loc) => format!("- {name} at {loc}"),
                    None => format!("- {name}"),
                };

                Some(entry)
            })
            .collect();

        // Reverse to read outermost-first (matches conventional stack trace order)
        elements.reverse();

        // Compress repeated cycles (e.g. mutual recursion between foldl and +)
        let elements = compress_trace_cycles(elements);

        elements.as_slice().join("\n")
    }
}

/// Detect and compress repeating cycles in a formatted stack trace.
///
/// Scans the elements list for the smallest repeating prefix and, when the
/// same pattern of frames appears two or more times consecutively, emits the
/// pattern once followed by a `  ... N frames elided (M× repetition)` line.
/// The remaining non-repeating tail is appended unchanged.
///
/// Only patterns of length ≤ 8 are considered to keep the algorithm efficient.
fn compress_trace_cycles(elements: Vec<String>) -> Vec<String> {
    let n = elements.len();
    if n < 2 {
        return elements;
    }

    let mut result = Vec::with_capacity(n);
    let mut i = 0;

    while i < n {
        let remaining = n - i;
        let max_pat = (remaining / 2).min(8);
        let mut compressed = false;

        // Try shortest patterns first so we find the smallest repeating unit
        for pat_len in 1..=max_pat {
            let pattern = &elements[i..i + pat_len];
            let mut count = 1usize;
            let mut j = i + pat_len;

            while j + pat_len <= n && elements[j..j + pat_len] == *pattern {
                count += 1;
                j += pat_len;
            }

            if count >= 2 {
                // Emit the pattern once
                result.extend_from_slice(pattern);
                let elided = (count - 1) * pat_len;
                result.push(format!(
                    "  ... {} frame{} elided ({}× repetition)",
                    elided,
                    if elided == 1 { "" } else { "s" },
                    count
                ));
                i = j;
                compressed = true;
                break;
            }
        }

        if !compressed {
            result.push(elements[i].clone());
            i += 1;
        }
    }

    result
}

/// Map internal intrinsic names to user-facing display names.
///
/// Returns `None` for internal machinery that should be filtered out
/// of user-visible traces.
pub fn intrinsic_display_name(name: &str) -> Option<&str> {
    match name {
        // Arithmetic operators
        "ADD" => Some("+"),
        "SUB" => Some("-"),
        "MUL" => Some("*"),
        "DIV" => Some("/"),
        "PDIV" => Some("\u{00f7}"),
        "MOD" => Some("%"),
        "QUOT" => Some("quot"),
        "REM" => Some("rem"),
        "FLOOR" => Some("floor"),
        "CEILING" => Some("ceiling"),
        "POW" => Some("pow"),

        // Comparison operators
        "EQ" => Some("=="),
        "LT" => Some("<"),
        "GT" => Some(">"),
        "LTE" => Some("<="),
        "GTE" => Some(">="),

        // Boolean operators
        "NOT" => Some("not"),

        // String functions
        "LETTERS" => Some("str.letters"),
        "UPPER" => Some("str.upper"),
        "LOWER" => Some("str.lower"),
        "SPLIT" => Some("str.split"),
        "MATCH" => Some("str.match"),
        "MATCHES" => Some("str.matches"),
        "JOIN" => Some("str.join"),
        "FMT" => Some("fmt"),
        "STR" => Some("str"),
        "SYM" => Some("sym"),
        "NUMPARSE" => Some("num.parse"),

        // Collection functions
        "LOOKUP" => Some("lookup"),
        "LOOKUPOR" => Some("lookup-or"),
        "HEAD" => Some("head"),
        "TAIL" => Some("tail"),
        "CONS" => Some("cons"),
        "NIL" => Some("nil"),
        "REVERSE" => Some("reverse"),
        "MERGE" => Some("merge"),
        "MERGEWITH" => Some("merge-with"),
        "DEEPMERGE" => Some("deep-merge"),
        "ELEMENTS" => Some("elements"),
        "BLOCK" => Some("block"),
        "KV" => Some("kv"),
        "DEKV" => Some("de-kv"),

        // Metadata
        "META" => Some("meta"),
        "WITHMETA" => Some("with-meta"),
        "RAWMETA" => Some("raw-meta"),

        // Type checking
        "ISBLOCK" => Some("block?"),
        "ISLIST" => Some("list?"),
        "NULL" => Some("null"),
        "TAG" => Some("tag"),

        // Boolean constants
        "TRUE" => Some("true"),
        "FALSE" => Some("false"),

        // Control flow
        "IF" => Some("if"),
        "PANIC" => Some("panic"),
        "ASSERT_FAIL" => Some("assert.fail"),

        // Date/time
        "ZDT" => Some("zdt"),
        "ZDT.FROM_EPOCH" => Some("zdt.from-epoch"),
        "ZDT.FIELDS" => Some("zdt.fields"),
        "ZDT.PARSE" => Some("zdt.parse"),
        "ZDT.FORMAT" => Some("zdt.format"),

        // Encoding / hashing
        "BASE64_ENCODE" => Some("base64.encode"),
        "BASE64_DECODE" => Some("base64.decode"),
        "SHA256" => Some("sha256"),

        // Set operations
        "SET.EMPTY" => Some("set.empty"),
        "SET.FROM_LIST" => Some("set.from-list"),
        "SET.TO_LIST" => Some("set.to-list"),
        "SET.ADD" => Some("set.add"),
        "SET.REMOVE" => Some("set.remove"),
        "SET.CONTAINS" => Some("set.contains"),
        "SET.SIZE" => Some("set.size"),
        "SET.UNION" => Some("set.union"),
        "SET.INTERSECT" => Some("set.intersect"),
        "SET.DIFF" => Some("set.diff"),

        // Lookup error path
        "LOOKUP_FAIL" => Some("lookup"),

        // Random / streams
        "PRODUCER_NEXT" => Some("producer.next"),

        // Internal machinery — filter out of traces
        //
        // Emit and render pipeline
        "EMIT0" | "EMITx" | "EMITT" | "EMITF" | "EMIT[" | "EMIT]" | "EMIT{" | "EMIT}" | "EMIT<"
        | "EMIT>" | "EMITTAGx" | "EMITTAG[" | "EMITTAG{" | "NV.EMIT[*]" | "NV.EMIT{*}"
        | "Emit.RenderKV" | "RENDER" | "RENDER_ITEMS" | "RENDER_BLOCK_ITEMS" | "RENDER_KV"
        | "RENDER_DOC" => None,
        // Boolean / saturation / internal control
        "AND" | "OR" | "SATURATED" => None,
        // Internal block/list helpers
        "LOOKUPOR#" | "MATCHES_KEY" | "EXTRACT_VALUE" | "EXTRACT_KEY" | "PACK_PAIR"
        | "BLOCK_PAIR" | "seqStrList" | "NV.ALL[*]" => None,
        // Internal constants and data constructors
        "KNIL" | "K[]" | "K{}" | "DQ" | "IFIELDS" | "SUPPRESSES" | "REQUIRES" => None,

        // Unknown — not an intrinsic
        _ => None,
    }
}
