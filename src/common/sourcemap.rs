use crate::common::diagnostic_json::FrameKind;
use codespan::Span;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::{Files, SimpleFiles},
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Display;
use std::num::NonZeroU32;
use std::{fmt, ops::Range};

/// A handle that points to a source location in a source map.
///
/// Serialises as a `u32` (0 = synthetic/invalid, 1.. = source positions).
/// Pre-compiled blobs use `Smid::default()` (0) for the vast majority of
/// prelude locations (inner nodes within a combinator's compiled body) —
/// a raw Smid baked at `xtask` build time would index into a `SourceMap`
/// the loading process never populated, so it is elided at reconstruction.
/// The one exception is a blob global's own entry-point identity: see
/// [`Smid::global_slot`], which reserves a disjoint sub-range of this
/// same `u32` space for "which prelude global slot" rather than "which
/// source position" (eu-1tkk.7.11).
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Smid(Option<NonZeroU32>);

/// Tag bit distinguishing a [`Smid::global_slot`] identity from a real
/// `SourceMap` index.
///
/// Real Smids are minted sequentially from 1 by `SourceMap::add*` and never
/// approach this range for any realistic source file (billions of AST
/// nodes), so a tagged value can never collide with a genuine source
/// position — but it must never be used to index into a `SourceMap`.
const GLOBAL_SLOT_TAG: u32 = 0x8000_0000;

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

    /// Convert to a zero-based index into `SourceMap::source`, if valid.
    ///
    /// Returns `None` for an invalid (default/synthetic) Smid rather than
    /// panicking. Both trace producers (the HeapSyn VM and the bytecode
    /// machine) pre-filter invalid Smids before pushing them onto env/stack
    /// traces, so every current caller sees `Some` in practice — but this
    /// guard means a future unfiltered trace source cannot panic during
    /// error reporting (eu-1tkk.7.10).
    pub fn get(self) -> Option<usize> {
        self.0.map(|n| (n.get() - 1) as usize)
    }

    pub fn sym_name(&self) -> String {
        match self.0 {
            Some(n) => format!("__{n}"),
            None => "__<nosmid>".to_string(),
        }
    }

    /// Construct a Smid identifying a prelude-blob global slot, not a
    /// `SourceMap` index (eu-1tkk.7.11).
    ///
    /// Used by blob-mode STG-arena reconstruction (`StgArena::
    /// reconstruct_form_annotated`) to stamp a reconstructed lambda form's
    /// annotation with "which prelude global slot this came from" instead
    /// of the usual `Smid::default()` — restoring enough identity for
    /// Phase 2's blame classifier to name the combinator, without
    /// resurrecting a raw xtask-sourced Smid that would index into a
    /// `SourceMap` the loading process never populated (see the struct doc
    /// comment above).
    ///
    /// `slot` is the *prelude-relative* slot index (matching
    /// `PreludeBlob::name_to_slot` / `StandardRuntime`'s `prelude_names`),
    /// not the full `Ref::G` global index. Only the low 31 bits are
    /// retained (masked with `!GLOBAL_SLOT_TAG`) — the prelude has on the
    /// order of hundreds of bindings, nowhere near this limit.
    pub fn global_slot(slot: u32) -> Smid {
        Smid(NonZeroU32::new(GLOBAL_SLOT_TAG | (slot & !GLOBAL_SLOT_TAG)))
    }

    /// If this Smid was constructed by [`Smid::global_slot`], return the
    /// slot it identifies. Returns `None` for an ordinary source-map Smid,
    /// a synthetic Smid, or the invalid/default Smid.
    pub fn as_global_slot(&self) -> Option<u32> {
        self.0.and_then(|n| {
            let v = n.get();
            if v & GLOBAL_SLOT_TAG != 0 {
                Some(v & !GLOBAL_SLOT_TAG)
            } else {
                None
            }
        })
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
#[derive(Clone)]
pub struct SourceInfo {
    /// usize
    pub file: Option<usize>,
    /// Byte span
    pub span: Option<Span>,
    /// Text annotation (e.g. global name)
    pub annotation: Option<String>,
}

/// Store all source info...
#[derive(Default, Clone)]
pub struct SourceMap {
    source: Vec<SourceInfo>,
    /// File IDs that correspond to resources (e.g. prelude, stdlib) rather
    /// than user-authored files.  Used by `is_user_file` so that diagnostic
    /// code can prefer user locations over resource locations when building
    /// error labels.
    resource_file_ids: std::collections::HashSet<usize>,
    /// Declared blame classification by combinator name (eu-1tkk.7.12).
    ///
    /// Populated once at startup from whichever prelude path is active:
    /// the source-compiled path reconciles `TranslationUnit::blame`
    /// (`SourceLoader::blame_table`) into this map; the blob path merges in
    /// `PreludeBlob::blame` (already `HashMap<String, FrameKind>`). Both
    /// paths converge on the same shape so [`SourceMap::classify_frame`] is
    /// a single, uniform lookup regardless of which prelude is loaded.
    blame_by_name: HashMap<String, FrameKind>,
    /// Prelude-relative global slot → binding name (blob path only,
    /// eu-1tkk.7.12).
    ///
    /// Mirrors `PreludeBlob::name_to_slot` inverted, so a trace Smid that
    /// decodes as a [`Smid::as_global_slot`] identity can be resolved to a
    /// name without `classify_frame` needing a `PreludeBlob` reference of
    /// its own.
    slot_to_name: HashMap<u32, String>,
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
        let new_info = if let Some(info) = smid.get().and_then(|idx| self.source.get(idx)) {
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
        expr.smid().get().and_then(|idx| self.source.get(idx))
    }

    /// Retrieve the SourceInfo for a given Smid value
    pub fn source_info_for_smid(&self, smid: Smid) -> Option<&SourceInfo> {
        // A `Smid::global_slot` identity must never resolve to a `SourceInfo`
        // — it indexes a disjoint "which prelude global slot" space, not
        // this `SourceMap`. Without this explicit guard, rejection is only
        // incidental: `smid.get()` for a global-slot value is ~2.1 billion,
        // which happens to fall outside `self.source`'s bounds today purely
        // because no real source file is anywhere near that size. That
        // safety margin is not a contract — an explicit guard makes the
        // rejection structural, independent of `self.source`'s length
        // (eu-1tkk.7.11).
        if smid.as_global_slot().is_some() {
            return None;
        }
        if let Some(idx) = smid.get() {
            self.source.get(idx)
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

    /// Merge declared blame classifications into the name-keyed table
    /// [`SourceMap::classify_frame`] consults (eu-1tkk.7.12).
    ///
    /// Callable more than once — the source-compiled path and the blob path
    /// populate disjoint name spaces in practice (a user file's own
    /// declarations vs. the prelude's), so a plain merge (later entries win
    /// on a name collision) is sufficient; there is no ordering requirement
    /// between calls.
    pub fn extend_blame_table(&mut self, table: HashMap<String, FrameKind>) {
        self.blame_by_name.extend(table);
    }

    /// Record the blob's slot → name table so a blob-mode global-slot Smid
    /// (see [`Smid::as_global_slot`]) can be resolved to a binding name
    /// without `classify_frame` needing a `PreludeBlob` reference of its own
    /// (eu-1tkk.7.12).
    pub fn set_slot_names(&mut self, names: HashMap<u32, String>) {
        self.slot_to_name = names;
    }

    /// Classify a trace frame's Smid as `User`, `Boundary`, or `Transparent`
    /// (design spec §4.3, eu-1tkk.7.12).
    ///
    /// The single lookup both the source-compiled-prelude path and the
    /// blob (shipped-binary) path share:
    /// - A Smid resolving to a user-authored file is always `User`.
    /// - A Smid resolving to a prelude/resource `SourceMap` entry is
    ///   classified by its `annotation` (the declaring combinator's name,
    ///   set by `Desugarer::new_smid`) against `blame_by_name`.
    /// - A Smid carrying a [`Smid::as_global_slot`] identity (blob mode,
    ///   where prelude frames have no `SourceMap` entry at all) is resolved
    ///   via `slot_to_name` then `blame_by_name`.
    /// - Absent from both — an undeclared prelude combinator, an intrinsic,
    ///   or an unresolvable Smid — defaults to `Transparent`, never
    ///   silently `User` (a real user-file Smid is the only route to
    ///   `User`).
    pub fn classify_frame(&self, smid: Smid) -> FrameKind {
        if let Some(info) = self.source_info_for_smid(smid) {
            if let Some(fid) = info.file {
                if self.is_user_file(fid) {
                    return FrameKind::User;
                }
            }
            if let Some(ann) = &info.annotation {
                if let Some(kind) = self.blame_by_name.get(ann) {
                    return *kind;
                }
            }
            return FrameKind::Transparent;
        }
        if let Some(slot) = smid.as_global_slot() {
            if let Some(name) = self.slot_to_name.get(&slot) {
                if let Some(kind) = self.blame_by_name.get(name) {
                    return *kind;
                }
            }
        }
        FrameKind::Transparent
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

    /// Resolve a single trace Smid to `(display_name, location)`, shared by
    /// [`SourceMap::format_trace`] (raw) and
    /// [`SourceMap::format_curated_trace`] (Phase 2 curated, eu-1tkk.7.12)
    /// so the two formatters cannot silently diverge on name/location
    /// resolution. Returns `None` for an entry with neither a user-visible
    /// name nor a source location (internal machinery, silently dropped).
    fn resolve_trace_entry(
        &self,
        smid: Smid,
        files: &SimpleFiles<String, String>,
    ) -> Option<(String, Option<String>)> {
        let info = self.source.get(smid.get()?)?;

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

        Some((name, location))
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
            .filter_map(|&smid| {
                let (name, location) = self.resolve_trace_entry(smid, files)?;
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

    /// Format a curated trace (design spec §4.3, eu-1tkk.7.12): like
    /// [`SourceMap::format_trace`], but over frames already classified and
    /// cycle-collapsed by `eval::error::curate_trace` — a `Boundary` frame
    /// is labelled `in 'name'` rather than shown as a bare location, since
    /// it is the named combinator the user actually invoked, kept as
    /// context around the (already reassigned) primary location.
    pub fn format_curated_trace(
        &self,
        frames: &[(Smid, FrameKind)],
        files: &SimpleFiles<String, String>,
    ) -> String {
        let mut elements: Vec<_> = frames
            .iter()
            .filter_map(|&(smid, kind)| {
                let (name, location) = self.resolve_trace_entry(smid, files)?;
                let label = if kind == FrameKind::Boundary {
                    format!("in '{name}'")
                } else {
                    name
                };
                let entry = match location {
                    Some(loc) => format!("- {label} at {loc}"),
                    None => format!("- {label}"),
                };
                Some(entry)
            })
            .collect();

        // `curate_trace` yields innermost-first (matching the raw trace's
        // convention); reverse to the conventional outermost-first reading
        // order, same as `format_trace`. Cycle collapsing already happened
        // inside `curate_trace`, so no further compression here.
        elements.reverse();

        elements.join("\n")
    }
}

/// One run detected by [`compress_cycles`]: a pattern of elements, and how
/// many consecutive times it repeated (`1` for a passthrough, non-repeating
/// element).
pub(crate) struct CycleRun<T> {
    pub pattern: Vec<T>,
    pub count: usize,
}

/// Detect and compress repeating cycles in an element sequence.
///
/// Scans for the smallest repeating prefix and, when the same pattern of
/// elements appears two or more times consecutively, collapses it into one
/// [`CycleRun`] recording the pattern and its repetition count. Generalised
/// (eu-1tkk.7.12) from the original `String`-only cycle detector so the
/// Phase 2 curated-trace pipeline can collapse recursion on typed
/// `(Smid, FrameKind)` pairs *before* formatting, not just on
/// already-formatted strings — [`compress_trace_cycles`] below is now a
/// thin `String`-specialised wrapper over this for `format_trace`'s
/// existing callers.
///
/// Only patterns of length ≤ 8 are considered to keep the algorithm
/// efficient.
pub(crate) fn compress_cycles<T: Clone + PartialEq>(elements: &[T]) -> Vec<CycleRun<T>> {
    let n = elements.len();
    let mut result = Vec::new();
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
                result.push(CycleRun {
                    pattern: pattern.to_vec(),
                    count,
                });
                i = j;
                compressed = true;
                break;
            }
        }

        if !compressed {
            result.push(CycleRun {
                pattern: vec![elements[i].clone()],
                count: 1,
            });
            i += 1;
        }
    }

    result
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
    if elements.len() < 2 {
        return elements;
    }

    let mut result = Vec::with_capacity(elements.len());
    for run in compress_cycles(&elements) {
        let pat_len = run.pattern.len();
        result.extend(run.pattern);
        if run.count >= 2 {
            let elided = (run.count - 1) * pat_len;
            result.push(format!(
                "  ... {} frame{} elided ({}× repetition)",
                elided,
                if elided == 1 { "" } else { "s" },
                run.count
            ));
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

#[cfg(test)]
mod tests {
    use super::*;

    /// `Smid::get()` must never panic on an invalid (default/synthetic) Smid.
    ///
    /// Both trace producers (the HeapSyn VM and the bytecode machine) today
    /// pre-filter invalid Smids before pushing them onto env/stack traces, so
    /// this cannot currently happen in practice. This is a defensive
    /// regression test: a future trace source that fails to pre-filter must
    /// not be able to crash error reporting (eu-1tkk.7.10).
    #[test]
    fn get_returns_none_for_invalid_smid() {
        assert_eq!(Smid::default().get(), None);
    }

    #[test]
    fn get_returns_index_for_valid_smid() {
        let mut source_map = SourceMap::new();
        let smid = source_map.add(0, Span::new(0u32, 0u32));
        assert_eq!(smid.get(), Some(0));
    }

    /// `format_trace` must gracefully skip an invalid Smid rather than panic,
    /// even though today's trace producers never hand it one.
    #[test]
    fn format_trace_skips_invalid_smid_without_panicking() {
        let source_map = SourceMap::new();
        let files: SimpleFiles<String, String> = SimpleFiles::new();
        let trace = [Smid::default()];
        let out = source_map.format_trace(&trace, &files);
        assert_eq!(out, "");
    }

    /// A trace mixing a valid, resolvable Smid with an invalid one must
    /// render only the valid entry, not panic on the invalid one.
    #[test]
    fn format_trace_skips_invalid_smid_amongst_valid_ones() {
        let mut source_map = SourceMap::new();
        let mut files: SimpleFiles<String, String> = SimpleFiles::new();
        let file_id = files.add("x.eu".to_string(), "hello".to_string());
        let smid = source_map.add(file_id, Span::new(0u32, 5u32));
        let trace = [Smid::default(), smid, Smid::default()];
        let out = source_map.format_trace(&trace, &files);
        assert!(out.contains("x.eu:1:1"));
    }

    // ── Smid::global_slot / as_global_slot (eu-1tkk.7.11) ───────────────────

    #[test]
    fn global_slot_round_trips() {
        for slot in [0u32, 1, 42, 295, 65535] {
            let smid = Smid::global_slot(slot);
            assert_eq!(smid.as_global_slot(), Some(slot));
        }
    }

    #[test]
    fn global_slot_is_valid_and_not_a_source_index() {
        let smid = Smid::global_slot(7);
        assert!(smid.is_valid());
        // A global-slot Smid must never be mistaken for a real SourceMap
        // index by code that only checks `get()`/`is_valid()`.
        assert_ne!(smid.get(), None);
    }

    #[test]
    fn as_global_slot_is_none_for_default_smid() {
        assert_eq!(Smid::default().as_global_slot(), None);
    }

    #[test]
    fn as_global_slot_is_none_for_ordinary_source_smid() {
        let mut source_map = SourceMap::new();
        let smid = source_map.add(0, Span::new(0u32, 0u32));
        assert_eq!(smid.as_global_slot(), None);
    }

    /// `source_info_for_smid` must reject a global-slot identity explicitly
    /// (structurally, via `as_global_slot`), not merely by chance because
    /// its huge index falls out of `self.source`'s bounds today. Add a real
    /// entry to `source_map` first as a sanity check for the ordinary path.
    ///
    /// Note on fault-injection: deleting the guard clause outright is not
    /// observable by this (or any practically-sized) test, precisely
    /// because the guard is *defence in depth* — the fallback bounds check
    /// already rejects a ~2.1-billion index against any realistically
    /// small `self.source`. What this test does catch (verified live) is a
    /// broken/inverted guard condition: flipping `is_some()` to `is_none()`
    /// makes the ordinary-Smid sanity assertion above fail immediately,
    /// proving the guard is genuinely wired in and checked on every call,
    /// not dead code.
    #[test]
    fn source_info_for_smid_rejects_global_slot_identity() {
        let mut source_map = SourceMap::new();
        let real_smid = source_map.add(0, Span::new(0u32, 5u32));
        assert!(
            source_map.source_info_for_smid(real_smid).is_some(),
            "sanity: an ordinary Smid must still resolve"
        );

        for slot in [0u32, 1, 236, !GLOBAL_SLOT_TAG] {
            let smid = Smid::global_slot(slot);
            assert!(
                source_map.source_info_for_smid(smid).is_none(),
                "global_slot({slot}) must never resolve to a SourceInfo"
            );
        }
    }

    /// A real `SourceMap` Smid must never collide with a `global_slot`
    /// identity — the whole point of the tag bit. Adds a batch of ordinary
    /// Smids (as a real large source file would) and confirms none of them
    /// decode as a global slot.
    #[test]
    fn ordinary_smids_never_collide_with_global_slot_tag() {
        let mut source_map = SourceMap::new();
        for i in 0..10_000u32 {
            let smid = source_map.add(0, Span::new(i, i));
            assert_eq!(
                smid.as_global_slot(),
                None,
                "ordinary Smid #{i} misidentified as a global slot"
            );
        }
    }

    #[test]
    fn global_slot_masks_out_of_range_input_rather_than_colliding_with_tag() {
        // A slot value that already has the tag bit set (pathological input,
        // never produced by real callers) must still decode back to the
        // masked value, not silently misbehave.
        let smid = Smid::global_slot(GLOBAL_SLOT_TAG | 3);
        assert_eq!(smid.as_global_slot(), Some(3));
    }

    // ── classify_frame (eu-1tkk.7.12) ────────────────────────────────────────

    /// Build a `SourceMap` with one user-file Smid, one prelude Smid
    /// annotated `"map"`, and one prelude Smid annotated `"nth"` — mirroring
    /// Task 2's plan (source-compiled-prelude path: annotation-keyed
    /// classification).
    fn classifier_fixture() -> (SourceMap, Smid, Smid, Smid) {
        let mut source_map = SourceMap::new();
        source_map.mark_resource_file(1); // prelude file id
        let user_smid = source_map.add(0, Span::new(0u32, 5u32));
        let map_smid = source_map.add_annotated(1, Span::new(10u32, 15u32), "map");
        let nth_smid = source_map.add_annotated(1, Span::new(20u32, 25u32), "nth");
        source_map.extend_blame_table(HashMap::from([
            ("map".to_string(), FrameKind::Transparent),
            ("nth".to_string(), FrameKind::Boundary),
        ]));
        (source_map, user_smid, map_smid, nth_smid)
    }

    #[test]
    fn classify_frame_user_file_is_always_user() {
        let (source_map, user_smid, _, _) = classifier_fixture();
        assert_eq!(source_map.classify_frame(user_smid), FrameKind::User);
    }

    #[test]
    fn classify_frame_declared_transparent_combinator() {
        let (source_map, _, map_smid, _) = classifier_fixture();
        assert_eq!(source_map.classify_frame(map_smid), FrameKind::Transparent);
    }

    #[test]
    fn classify_frame_declared_boundary_combinator() {
        let (source_map, _, _, nth_smid) = classifier_fixture();
        assert_eq!(source_map.classify_frame(nth_smid), FrameKind::Boundary);
    }

    /// A prelude Smid whose annotation has no declared blame contract must
    /// default to `Transparent`, never silently `User` (design spec §4.3:
    /// "Default to Transparent, never silently User").
    #[test]
    fn classify_frame_undeclared_prelude_combinator_defaults_transparent() {
        let mut source_map = SourceMap::new();
        source_map.mark_resource_file(1);
        let undeclared = source_map.add_annotated(1, Span::new(0u32, 5u32), "undeclared-fn");
        assert_eq!(
            source_map.classify_frame(undeclared),
            FrameKind::Transparent
        );
    }

    /// A blob-mode global-slot Smid (no `SourceMap` entry at all) resolves
    /// via `slot_to_name` then `blame_by_name` — the blob-path half of the
    /// uniform classifier.
    #[test]
    fn classify_frame_blob_mode_global_slot_resolves_via_slot_names() {
        let mut source_map = SourceMap::new();
        source_map.extend_blame_table(HashMap::from([("nth".to_string(), FrameKind::Boundary)]));
        source_map.set_slot_names(HashMap::from([(42u32, "nth".to_string())]));
        let smid = Smid::global_slot(42);
        assert_eq!(source_map.classify_frame(smid), FrameKind::Boundary);
    }

    /// A global-slot Smid resolving to a slot with no declared blame
    /// contract defaults to `Transparent`, mirroring the source-path
    /// default.
    #[test]
    fn classify_frame_blob_mode_undeclared_slot_defaults_transparent() {
        let mut source_map = SourceMap::new();
        source_map.set_slot_names(HashMap::from([(7u32, "undeclared-fn".to_string())]));
        let smid = Smid::global_slot(7);
        assert_eq!(source_map.classify_frame(smid), FrameKind::Transparent);
    }

    /// A default/invalid Smid (no source info, no global-slot identity)
    /// must classify as `Transparent`, not panic and not `User`.
    #[test]
    fn classify_frame_invalid_smid_defaults_transparent() {
        let source_map = SourceMap::new();
        assert_eq!(
            source_map.classify_frame(Smid::default()),
            FrameKind::Transparent
        );
    }

    /// Fault injection: if `classify_frame` always returned `Transparent`
    /// (the pre-Task-2 status quo), the `User`/`Boundary` cases above must
    /// fail — proving the discrimination is genuinely exercised, not vacuous.
    #[test]
    fn fault_injection_classify_frame_must_discriminate() {
        let (source_map, user_smid, map_smid, nth_smid) = classifier_fixture();
        let user = source_map.classify_frame(user_smid);
        let transparent = source_map.classify_frame(map_smid);
        let boundary = source_map.classify_frame(nth_smid);
        assert!(
            user != transparent || transparent != boundary || user != boundary,
            "classify_frame must discriminate User/Transparent/Boundary, got \
             user={user:?} transparent={transparent:?} boundary={boundary:?}"
        );
        assert_eq!(user, FrameKind::User);
        assert_eq!(transparent, FrameKind::Transparent);
        assert_eq!(boundary, FrameKind::Boundary);
    }

    // ── compress_cycles generalisation (eu-1tkk.7.12) ────────────────────────

    /// The generalised, typed cycle compressor must agree with the original
    /// `String`-only behaviour: a single-element pattern repeating
    /// consecutively collapses to one run with the right count.
    #[test]
    fn compress_cycles_collapses_single_element_repeats() {
        let elements = vec!["a", "b", "b", "b", "c"];
        let runs = compress_cycles(&elements);
        let summary: Vec<(Vec<&str>, usize)> =
            runs.into_iter().map(|r| (r.pattern, r.count)).collect();
        assert_eq!(
            summary,
            vec![(vec!["a"], 1), (vec!["b"], 3), (vec!["c"], 1),]
        );
    }

    /// A multi-element repeating pattern (e.g. mutual recursion between two
    /// combinators) collapses to one run of the whole pattern, matching the
    /// pre-generalisation behaviour that `format_trace`'s doc comment
    /// describes ("mutual recursion between foldl and +").
    #[test]
    fn compress_cycles_collapses_multi_element_patterns() {
        let elements = vec!["foldl", "+", "foldl", "+", "foldl", "+"];
        let runs = compress_cycles(&elements);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].pattern, vec!["foldl", "+"]);
        assert_eq!(runs[0].count, 3);
    }

    /// No regression: `compress_trace_cycles` (the `String`-specialised
    /// wrapper `format_trace` still calls) must produce byte-identical
    /// output to before the generalisation.
    #[test]
    fn compress_trace_cycles_wrapper_matches_generalised_result() {
        let elements: Vec<String> = ["a", "b", "b", "b", "c"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        let out = compress_trace_cycles(elements);
        assert_eq!(
            out,
            vec![
                "a".to_string(),
                "b".to_string(),
                "  ... 2 frames elided (3× repetition)".to_string(),
                "c".to_string(),
            ]
        );
    }
}
