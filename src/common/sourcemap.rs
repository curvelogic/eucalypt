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

    /// Whether this Smid identifies an actual position in a source file —
    /// i.e. it is valid *and* not a [`Smid::global_slot`] identity.
    ///
    /// A `global_slot` Smid is deliberately [`is_valid`](Self::is_valid) (so
    /// blame classification has something to key on for an entered prelude
    /// combinator), but it resolves to no source position at all. Registers
    /// that track "the most recent real location" — such as the machine's
    /// `last_annotation` — must gate on this, not `is_valid`, or entering an
    /// unstamped prelude global clobbers the genuine user call site that
    /// preceded it with a location-free marker (eu-1tkk.7.21).
    pub fn is_source_location(&self) -> bool {
        self.is_valid() && self.as_global_slot().is_none()
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

/// The location half of a resolved trace entry (see
/// [`SourceMap::resolve_trace_entry`]).
///
/// A user file gets a precise, actionable coordinate. A resource
/// (bundled-library, e.g. prelude) frame gets only the library's name: the
/// exact `line:col` is useful to maintainers of that library but the user
/// cannot edit it, cannot act on the coordinate, and may reasonably think
/// they are being asked to (eu-1tkk.7.36).
#[derive(Debug, Clone, PartialEq, Eq)]
enum TraceLocation {
    /// `at file:line:col` — a site the user can open and act on.
    Site(String),
    /// `(name)` — a bundled-library frame, named but not pinpointed.
    Resource(String),
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
    /// Count of leading `Smid` indices reserved for a *foreign* `Smid`
    /// space — see [`SourceMap::reserve_foreign_range`] (eu-r4647).
    ///
    /// `source[i]` describes absolute index `foreign_floor + i`. Every
    /// absolute index below the floor belongs to some other process's
    /// `SourceMap` and resolves to `None` here; every index this map
    /// mints is at or above it.
    foreign_floor: usize,
    /// Prelude-relative global slot → span of that binding's declaration in
    /// the prelude source (blob path only, eu-7x0r).
    ///
    /// Mirrors `PreludeBlob::binding_spans`. Together with
    /// [`SourceMap::prelude_file`] this is what lets a blob-mode trace frame
    /// carry a real prelude location rather than a bare name — used by the
    /// structured JSON trace (`--error-format json`) and, before
    /// eu-1tkk.7.36, by the human `file:line:col` rendering too. The human
    /// note now shows only the library name (`(prelude)`), not the
    /// coordinate, but the coordinate is retained here for the JSON path and
    /// for maintainers using `EU_ERROR_TRACE_DUMP=1`.
    slot_spans: HashMap<u32, Span>,
    /// File id of the prelude source in `files`, registered on demand on the
    /// blob path (eu-7x0r).
    ///
    /// `None` on the source-compiled path (there, prelude Smids resolve
    /// through `source` like any other) and until the first diagnostic on
    /// the blob path, where the prelude source is not otherwise loaded.
    prelude_file: Option<usize>,
}

impl SourceMap {
    /// Create a new, empty database of files.
    pub fn new() -> Self {
        SourceMap::default()
    }

    /// The `Smid` the next `add*` will hand out.
    fn next_smid(&self) -> Smid {
        Smid::new(self.foreign_floor + self.source.len())
    }

    /// Resolve an absolute `Smid` index against this map, rejecting
    /// anything inside the reserved foreign range.
    fn info_at(&self, absolute: usize) -> Option<&SourceInfo> {
        absolute
            .checked_sub(self.foreign_floor)
            .and_then(|i| self.source.get(i))
    }

    /// Reserve the index range occupied by a *foreign* `Smid` space, so
    /// that nothing this `SourceMap` mints can collide with one, and
    /// nothing already in that space can resolve here (eu-r4647).
    ///
    /// `Smid` is a bare index into `self.source`, minted sequentially
    /// from one, which makes it meaningful only within the process that
    /// minted it. Pre-compiled artefacts break that assumption: `cargo xtask
    /// prelude-compile` bakes `Smid`s minted by *its own* `SourceMap` into
    /// the blob, and `PreludeBlob::desugared_unit_cores` carries them
    /// verbatim into the runtime type check
    /// ([`crate::driver::check::run_type_checker_from_blob_core`]). Those
    /// indices are not merely useless here — once this map grows past
    /// them they resolve against whichever *unrelated user declaration*
    /// happens to occupy the same slot, so a diagnostic sited inside the
    /// baked prelude renders a primary label pointing at innocent user
    /// code. (The same class, on the eval path, made `xs nth(99)` blame
    /// three arbitrary lines of a 2000-declaration file — eu-7x0r.)
    ///
    /// Call this with the highest `Smid` present in the foreign material,
    /// while this map is still empty. Foreign indices then resolve to
    /// `None` — "no location", the only truthful answer, since the
    /// pre-compiled prelude's source is not registered here at all —
    /// while everything subsequently minted lands above the reserved
    /// range and so can never be aliased.
    ///
    /// Reserving rather than rewriting the baked `Smid`s is deliberate:
    /// some of them are load-bearing. An [`crate::core::expr::Anaphor`]'s
    /// `Smid` is an occurrence *discriminator* that `cook`'s anaphora
    /// resolution sorts on, so collapsing those to `Smid::default()`
    /// would silently merge distinct `_` occurrences. Reserving
    /// neutralises every foreign index uniformly — including ones no
    /// rewrite would have reached — without touching the data, and costs
    /// one `usize` rather than memory proportional to a number the blob
    /// supplies.
    ///
    /// Only ever raises the floor. An invalid `Smid`, a
    /// [`Smid::global_slot`] identity (a tagged value in a disjoint
    /// space, not an index), or a value already inside the reserved range
    /// is a no-op.
    ///
    /// # Panics
    ///
    /// If this map has already minted a `Smid`. Raising the floor after
    /// the fact would silently change what every existing `Smid` means.
    pub fn reserve_foreign_range(&mut self, highest: Smid) {
        if highest.as_global_slot().is_some() {
            return;
        }
        let needed = u32::from(highest) as usize;
        if needed <= self.foreign_floor {
            return;
        }
        assert!(
            self.source.is_empty(),
            "SourceMap::reserve_foreign_range must be called before any Smid is minted"
        );
        self.foreign_floor = needed;
    }

    /// Add a new source info and get a SMID referencing it
    pub fn add(&mut self, file: usize, span: Span) -> Smid {
        let smid = self.next_smid();
        self.source.push(SourceInfo {
            file: Some(file),
            span: Some(span),
            annotation: None,
        });
        smid
    }

    /// Mint a `Smid` spanning both `first` and `second` (eu-1tkk.7.38).
    ///
    /// Used by `cook`'s shunting yard, where a syntactic construct's two
    /// halves are separate nodes with separate `Smid`s — a call's callee and
    /// its argument tuple — but the node the shunter builds from them is one
    /// expression, and the diagnostic that blames it should underline all of
    /// it. Without this, an `App`'s primary label could never reach beyond
    /// the function name it was stamped with.
    ///
    /// Falls back to `first` unless both `Smid`s resolve to a real span in
    /// the *same* file. Synthetic locations, blob-baked prelude locations
    /// (which resolve to `None` inside the reserved foreign range) and
    /// [`Smid::global_slot`] identities therefore leave the caller's existing
    /// behaviour untouched rather than inventing a span that spuriously
    /// joins two files.
    ///
    /// `first`'s annotation is carried over (falling back to `second`'s).
    /// The annotation is the enclosing declaration's name — what a stack
    /// trace frame is labelled with — so dropping it would silently
    /// relabel every widened frame with the source text under the new,
    /// wider span.
    pub fn merge(&mut self, first: Smid, second: Smid) -> Smid {
        let (Some(a), Some(b)) = (
            self.source_info_for_smid(first),
            self.source_info_for_smid(second),
        ) else {
            return first;
        };
        let (Some(file), Some(a_span), Some(b_span)) = (a.file, a.span, b.span) else {
            return first;
        };
        if a.file != b.file {
            return first;
        }
        let annotation = a.annotation.clone().or_else(|| b.annotation.clone());
        let start = std::cmp::min(a_span.start(), b_span.start());
        let end = std::cmp::max(a_span.end(), b_span.end());
        let span = Span::new(start, end);
        match annotation {
            Some(ann) => self.add_annotated(file, span, ann),
            None => self.add(file, span),
        }
    }

    /// Number of source entries minted so far.
    ///
    /// Reported by the `EU_ERROR_TRACE_DUMP` dump, where it is the difference
    /// between "this Smid is meaningless" and "this Smid is meaningful but
    /// belongs to something else": a trace Smid at or beyond this length
    /// cannot be a real entry, whereas one below it resolves — and a Smid
    /// baked at build time silently aliases whatever entry happens to sit at
    /// that index. Distinguishing the two was what pinned eu-7x0r.
    pub fn len(&self) -> usize {
        self.source.len()
    }

    /// Whether any source entries have been minted.
    pub fn is_empty(&self) -> bool {
        self.source.is_empty()
    }

    /// Add a new source info and get a SMID referencing it
    pub fn add_annotated<T: AsRef<str>>(&mut self, file: usize, span: Span, annotation: T) -> Smid {
        let smid = self.next_smid();
        self.source.push(SourceInfo {
            file: Some(file),
            span: Some(span),
            annotation: Some(annotation.as_ref().to_string()),
        });
        smid
    }

    /// Add a notional location which has no concrete file co-ordinate
    pub fn add_synthetic<T: AsRef<str>>(&mut self, annotation: T) -> Smid {
        let smid = self.next_smid();
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
        let new_smid = self.next_smid();
        let new_info = if let Some(info) = self.source_info_for_smid(smid) {
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
    ///
    /// Delegates to [`SourceMap::source_info_for_smid`] so that the
    /// global-slot guard documented there applies uniformly, whichever
    /// entry point a caller reaches for.
    pub fn source_info(&self, expr: &dyn HasSmid) -> Option<&SourceInfo> {
        self.source_info_for_smid(expr.smid())
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
        // `info_at` additionally rejects anything inside a reserved
        // foreign range (eu-r4647).
        smid.get().and_then(|idx| self.info_at(idx))
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

    /// Record the blob's slot → prelude-source-span table so a blob-mode
    /// global-slot Smid can be rendered with a real prelude location
    /// (eu-7x0r). Paired with [`SourceMap::set_prelude_file`].
    pub fn set_slot_spans(&mut self, spans: HashMap<u32, Span>) {
        self.slot_spans = spans;
    }

    /// Record the file id the prelude source was registered under, so
    /// [`SourceMap::global_slot_info`] can pair a slot span with a file
    /// (eu-7x0r). Also marks it as a resource file, so it can never be
    /// mistaken for user code.
    pub fn set_prelude_file(&mut self, file_id: usize) {
        self.prelude_file = Some(file_id);
        self.mark_resource_file(file_id);
    }

    /// Whether the prelude source has already been registered for blob-mode
    /// diagnostics (see [`SourceMap::set_prelude_file`]).
    pub fn has_prelude_file(&self) -> bool {
        self.prelude_file.is_some()
    }

    /// Resolve a blob-mode [`Smid::global_slot`] identity to the same shape
    /// an ordinary `SourceMap` entry would have: the binding's name as the
    /// annotation, plus its prelude declaration site when known (eu-7x0r).
    ///
    /// Deliberately *not* folded into
    /// [`SourceMap::source_info_for_smid`]: that method's contract is
    /// "resolve an index into `self.source`", and it explicitly rejects
    /// global-slot identities. Everything that picks a *primary* error
    /// location (`first_source_smid`, `first_user_source_smid`,
    /// `source_info`) goes through that method, and must keep rejecting
    /// these — a prelude declaration site is never a valid primary label
    /// (invariant (i): the primary must be in the user's own file). Only
    /// the trace *renderers* call this, which is exactly where naming the
    /// prelude combinator is wanted.
    pub fn global_slot_info(&self, smid: Smid) -> Option<SourceInfo> {
        let slot = smid.as_global_slot()?;
        let name = self.slot_to_name.get(&slot)?;
        let span = self.slot_spans.get(&slot).copied();
        Some(SourceInfo {
            // A span without a file is unusable, and vice versa: pair them
            // or report neither, so a frame never claims a bogus location.
            file: span.and(self.prelude_file),
            span: self.prelude_file.and(span),
            annotation: Some(name.clone()),
        })
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
    ///
    /// A blob-mode [`Smid::global_slot`] identity has no `self.source` entry
    /// by construction, so it resolves via [`SourceMap::global_slot_info`]
    /// instead — without that, every prelude frame in a shipped-binary trace
    /// was silently dropped here (eu-7x0r).
    fn resolve_trace_entry(
        &self,
        smid: Smid,
        files: &SimpleFiles<String, String>,
    ) -> Option<(String, Option<TraceLocation>)> {
        // Both fixes are load-bearing here and neither subsumes the other
        // (eu-7x0r + eu-r4647). A `Smid::global_slot` identity has no `source`
        // entry by construction, so it must resolve through the blob's slot
        // tables first; everything else resolves through `info_at`, which
        // rejects indices inside the reserved foreign range instead of
        // aliasing them onto unrelated user declarations.
        let slot_info = self.global_slot_info(smid);
        let info = match slot_info {
            Some(ref info) => info,
            None => self.info_at(smid.get()?)?,
        };

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

        // Build the location. A user file gets a precise, actionable
        // `file:line:col` site. A resource (bundled library, e.g. the
        // prelude) frame gets only the library's name, no coordinate: the
        // exact line is useful to us when maintaining the prelude but the
        // user cannot edit library source, cannot act on the coordinate,
        // and may reasonably think they are being asked to (eu-1tkk.7.36).
        let location = info.file.and_then(|id| {
            let name = files.name(id).ok()?;
            // Strip directory prefix for readability
            let short_name = std::path::Path::new(&name)
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or(&name);

            if !self.is_user_file(id) {
                let hint = short_name.trim_start_matches('[').trim_end_matches(']');
                return Some(TraceLocation::Resource(hint.to_string()));
            }

            let span = info.span?;
            let loc = files.location(id, span.start().to_usize()).ok()?;
            Some(TraceLocation::Site(format!(
                "{short_name}:{line}:{col}",
                line = loc.line_number,
                col = loc.column_number
            )))
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
    /// source expression. A frame resolving into bundled library source
    /// (e.g. the prelude) carries only the library's name, not a
    /// coordinate within it — see [`TraceLocation`].
    pub fn format_trace(&self, trace: &[Smid], files: &SimpleFiles<String, String>) -> String {
        // Collect entries in trace order (innermost-first from the VM),
        // then reverse so the output reads outermost-first (conventional order).
        let mut elements: Vec<_> = trace
            .iter()
            .filter_map(|&smid| {
                let (name, location) = self.resolve_trace_entry(smid, files)?;
                let entry = match location {
                    Some(TraceLocation::Site(loc)) => format!("- {name} at {loc}"),
                    Some(TraceLocation::Resource(res)) => format!("- {name} ({res})"),
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
                    Some(TraceLocation::Site(loc)) => format!("- {label} at {loc}"),
                    Some(TraceLocation::Resource(res)) => format!("- {label} ({res})"),
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

    /// `merge` spans both operands (eu-1tkk.7.38): a callee at 0..5 and an
    /// argument tuple at 5..11 give one location covering 0..11.
    #[test]
    fn merge_spans_both_operands() {
        let mut source_map = SourceMap::new();
        let callee = source_map.add(0, Span::new(0u32, 5u32));
        let args = source_map.add(0, Span::new(5u32, 11u32));
        let merged = source_map.merge(callee, args);
        let info = source_map.source_info_for_smid(merged).unwrap();
        assert_eq!(info.file, Some(0));
        assert_eq!(info.span, Some(Span::new(0u32, 11u32)));
    }

    /// The merged location keeps the first operand's annotation — the
    /// enclosing declaration name a stack trace frame is labelled with.
    #[test]
    fn merge_keeps_the_first_operands_annotation() {
        let mut source_map = SourceMap::new();
        let callee = source_map.add_annotated(0, Span::new(0u32, 5u32), "result");
        let args = source_map.add(0, Span::new(5u32, 11u32));
        let merged = source_map.merge(callee, args);
        let info = source_map.source_info_for_smid(merged).unwrap();
        assert_eq!(info.annotation.as_deref(), Some("result"));
    }

    /// Nothing is minted, and the first operand is returned unchanged, when
    /// either operand has no resolvable location — a synthetic Smid, an
    /// invalid one, or one inside a reserved foreign range.
    #[test]
    fn merge_falls_back_to_the_first_operand_without_two_locations() {
        let mut source_map = SourceMap::new();
        let callee = source_map.add(0, Span::new(0u32, 5u32));
        let synthetic = source_map.add_synthetic("__FOO");
        assert_eq!(source_map.merge(callee, synthetic), callee);
        assert_eq!(source_map.merge(callee, Smid::default()), callee);
        assert_eq!(source_map.merge(Smid::default(), callee), Smid::default());
    }

    /// Two locations in different files never merge — a span joining them
    /// would be meaningless.
    #[test]
    fn merge_refuses_to_join_two_files() {
        let mut source_map = SourceMap::new();
        let callee = source_map.add(0, Span::new(0u32, 5u32));
        let elsewhere = source_map.add(1, Span::new(5u32, 11u32));
        assert_eq!(source_map.merge(callee, elsewhere), callee);
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

    /// A `global_slot` identity is deliberately `is_valid()` (see
    /// `global_slot_is_valid_and_not_a_source_index` above) but must not be
    /// treated as a real source location: `is_source_location` is the gate
    /// that registers such as the machine's `last_annotation` must use
    /// instead of `is_valid`, or entering an unstamped prelude global
    /// clobbers the genuine call site it followed (eu-1tkk.7.21).
    #[test]
    fn global_slot_is_valid_but_not_a_source_location() {
        let smid = Smid::global_slot(7);
        assert!(smid.is_valid());
        assert!(!smid.is_source_location());
    }

    #[test]
    fn default_smid_is_neither_valid_nor_a_source_location() {
        let smid = Smid::default();
        assert!(!smid.is_valid());
        assert!(!smid.is_source_location());
    }

    #[test]
    fn ordinary_smid_is_a_source_location() {
        let mut source_map = SourceMap::new();
        let smid = source_map.add(0, Span::new(0u32, 5u32));
        assert!(smid.is_valid());
        assert!(smid.is_source_location());
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

    // ── reserve_foreign_range (eu-r4647) ────────────────────────────────────

    /// The core invariant: after reserving a foreign range, every index
    /// inside it resolves to `None`, and everything this map goes on to
    /// mint lands above it.
    ///
    /// Without the reservation the second assertion is what fails — the
    /// locally added entry takes index 0 and so answers to `Smid` 1,
    /// which is a foreign index, and a diagnostic sited on foreign
    /// `Smid` 1 renders a label pointing at `mine.eu`.
    #[test]
    fn reserved_foreign_indices_never_resolve_and_never_collide() {
        let mut source_map = SourceMap::new();
        source_map.reserve_foreign_range(Smid::from(500));

        for foreign in [1u32, 2, 250, 499, 500] {
            assert_eq!(
                source_map
                    .source_info_for_smid(Smid::from(foreign))
                    .map(|_| ()),
                None,
                "foreign Smid {foreign} must not resolve against a reserved range"
            );
        }

        let mine = source_map.add(0, Span::new(3u32, 7u32));
        assert_eq!(
            u32::from(mine),
            501,
            "first locally minted Smid must clear the reserved range"
        );
        let info = source_map
            .source_info_for_smid(mine)
            .expect("a locally minted Smid must resolve");
        assert_eq!(info.file, Some(0));
        assert_eq!(info.span, Some(Span::new(3u32, 7u32)));
    }

    /// Reserving is monotonic and idempotent, and ignores values that are
    /// not `SourceMap` indices at all.
    #[test]
    fn reserve_foreign_range_only_ever_raises_the_floor() {
        let mut source_map = SourceMap::new();
        source_map.reserve_foreign_range(Smid::from(100));
        source_map.reserve_foreign_range(Smid::from(40));
        source_map.reserve_foreign_range(Smid::default());
        source_map.reserve_foreign_range(Smid::global_slot(3));
        assert_eq!(u32::from(source_map.add(0, Span::new(0u32, 1u32))), 101);
    }

    /// Raising the floor after a `Smid` has been minted would silently
    /// change what that `Smid` refers to, so it is refused outright
    /// rather than quietly corrupting every diagnostic in the run.
    #[test]
    #[should_panic(expected = "before any Smid is minted")]
    fn reserve_foreign_range_refuses_to_run_after_minting() {
        let mut source_map = SourceMap::new();
        source_map.add(0, Span::new(0u32, 1u32));
        source_map.reserve_foreign_range(Smid::from(100));
    }

    /// The invariant every downstream blame decision rests on:
    /// reserving a foreign range can only ever *remove* a route to
    /// `FrameKind::User`, never create one.
    ///
    /// `classify_frame` returns `User` through exactly one path — a
    /// resolved `SourceInfo` whose `file` is a user file. A foreign index
    /// inside a reserved range no longer resolves at all, so it skips
    /// that path and falls through to `Transparent`. Before the
    /// reservation it resolved to a real user-file entry and classified
    /// as `User`, which is the aliasing eu-r4647 fixes.
    ///
    /// This matters beyond the primary label: `curate_trace` and the
    /// `last_annotation` fallback in `ExecutionError::to_diagnostic`
    /// (eu-og3u6) both gate on `classify_frame(..) == FrameKind::User`
    /// before letting a Smid become a blame target, so a foreign Smid
    /// that classified `User` would leak straight through them.
    #[test]
    fn a_foreign_smid_classifies_transparent_never_user() {
        let mut source_map = SourceMap::new();
        source_map.reserve_foreign_range(Smid::from(500));
        // File 0 is the user's own file, registered *above* the reserved
        // range — exactly the arrangement that made foreign index 250
        // resolve to a user declaration before the fix.
        let user_smid = source_map.add(0, Span::new(0u32, 5u32));
        assert!(
            source_map.is_user_file(0),
            "precondition: file 0 is a user file"
        );
        assert_eq!(
            source_map.classify_frame(user_smid),
            FrameKind::User,
            "a genuinely local user Smid must still classify as User"
        );

        for foreign in [1u32, 250, 500] {
            assert_eq!(
                source_map.classify_frame(Smid::from(foreign)),
                FrameKind::Transparent,
                "foreign Smid {foreign} must never classify as User"
            );
        }
    }

    /// A reserved range must not perturb trace rendering for the
    /// locally minted Smids either — `resolve_trace_entry` indexes the
    /// same storage.
    #[test]
    fn format_trace_resolves_local_smids_above_a_reserved_range() {
        let mut source_map = SourceMap::new();
        source_map.reserve_foreign_range(Smid::from(7357));
        let mut files: SimpleFiles<String, String> = SimpleFiles::new();
        let file_id = files.add("x.eu".to_string(), "hello".to_string());
        let smid = source_map.add(file_id, Span::new(0u32, 5u32));
        let trace = [Smid::from(42), smid];
        let out = source_map.format_trace(&trace, &files);
        assert!(
            out.contains("x.eu:1:1"),
            "local entry should still render: {out}"
        );
        assert_eq!(
            out.matches("x.eu").count(),
            1,
            "the foreign Smid must contribute nothing: {out}"
        );
    }

    // ── global-slot trace rendering (eu-7x0r) ────────────────────────────────

    /// A `SourceMap` in the blob-path shape: no prelude entries in `source`
    /// at all, just the slot → name / slot → span / prelude-file tables the
    /// loader seeds from `PreludeBlob`.
    fn blob_path_fixture() -> (SourceMap, SimpleFiles<String, String>, Smid) {
        let mut files = SimpleFiles::new();
        let user = files.add("user.eu".to_string(), "result: xs nth(10)\n".to_string());
        let prelude = files.add(
            "[prelude]".to_string(),
            "head: __HEAD\nnth(n, l): {\n  aux: 1\n}\n".to_string(),
        );
        assert_eq!(user, 0);

        let mut source_map = SourceMap::new();
        source_map.extend_blame_table(HashMap::from([("nth".to_string(), FrameKind::Boundary)]));
        source_map.set_slot_names(HashMap::from([(42u32, "nth".to_string())]));
        // Byte 13 is the start of line 2 (`nth(n, l): {`).
        source_map.set_slot_spans(HashMap::from([(42u32, Span::new(13u32, 16u32))]));
        source_map.set_prelude_file(prelude);
        (source_map, files, Smid::global_slot(42))
    }

    /// The rendering half of the blob-path classifier: a global-slot Smid has
    /// no `source` entry by construction, so before eu-7x0r the formatters
    /// dropped it and no prelude frame ever appeared in a shipped-binary
    /// trace, however well it classified. Rendered as `(prelude)` with no
    /// coordinate (eu-1tkk.7.36): the line:col is useful to us maintaining
    /// the prelude, not to a user who cannot act on it.
    #[test]
    fn curated_trace_renders_a_blob_mode_global_slot_frame() {
        let (source_map, files, smid) = blob_path_fixture();
        let rendered = source_map.format_curated_trace(&[(smid, FrameKind::Boundary)], &files);
        assert_eq!(rendered, "- in 'nth' (prelude)");
    }

    /// The raw formatter shares `resolve_trace_entry`, so it must resolve the
    /// same frame (as a plain name, not `in '...'`).
    #[test]
    fn raw_trace_renders_a_blob_mode_global_slot_frame() {
        let (source_map, files, smid) = blob_path_fixture();
        assert_eq!(source_map.format_trace(&[smid], &files), "- nth (prelude)");
    }

    /// Without a span the frame is still named — just locationless — rather
    /// than dropped or given a fabricated location. This is the pre-v6-blob
    /// fallback (`binding_spans` absent ⇒ empty).
    #[test]
    fn global_slot_frame_without_a_span_renders_by_name_only() {
        let mut source_map = SourceMap::new();
        source_map.set_slot_names(HashMap::from([(42u32, "nth".to_string())]));
        let files: SimpleFiles<String, String> = SimpleFiles::new();
        let rendered = source_map
            .format_curated_trace(&[(Smid::global_slot(42), FrameKind::Boundary)], &files);
        assert_eq!(rendered, "- in 'nth'");
    }

    /// A slot the blob knows nothing about must not be invented into a frame.
    #[test]
    fn unknown_global_slot_resolves_to_nothing() {
        let (source_map, _, _) = blob_path_fixture();
        assert!(source_map
            .global_slot_info(Smid::global_slot(999))
            .is_none());
    }

    /// `global_slot_info` must stay *out* of the primary-location machinery:
    /// a prelude declaration site is never a valid primary label (invariant
    /// (i) — the primary must be in the user's own file), and every primary
    /// selector routes through `source_info_for_smid`, which rejects
    /// global-slot identities.
    #[test]
    fn global_slot_info_does_not_leak_into_primary_location_selection() {
        let (source_map, _, smid) = blob_path_fixture();
        assert!(source_map.global_slot_info(smid).is_some());
        assert!(source_map.source_info_for_smid(smid).is_none());
        assert_eq!(source_map.first_source_smid(&[smid]), None);
        assert_eq!(source_map.first_user_source_smid(&[smid]), None);
    }
}
