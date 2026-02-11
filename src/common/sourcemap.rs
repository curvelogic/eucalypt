use codespan::Span;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
};
use moniker::*;
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

/// SMIDs are ignorable for name binding
impl BoundTerm<String> for Smid {
    fn term_eq(&self, _: &Smid) -> bool {
        true
    }

    fn close_term(&mut self, _: ScopeState, _: &impl OnFreeFn<String>) {}

    fn open_term(&mut self, _: ScopeState, _: &impl OnBoundFn<String>) {}

    fn visit_vars(&self, _: &mut impl FnMut(&Var<String>)) {}

    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<String>)) {}
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

    /// Create a default diagnostic for an exception with a SMID
    pub fn diagnostic<E>(&self, error: &E) -> Diagnostic<usize>
    where
        E: HasSmid + Display,
    {
        let diag = Diagnostic::error().with_message(format!("{error}"));

        if let Some(&SourceInfo {
            file: Some(file),
            span: Some(span),
            ..
        }) = self.source_info(error)
        {
            diag.with_labels(vec![Label::primary(file, span)])
        } else {
            diag
        }
    }

    /// Format a stack / environment trace
    pub fn format_trace(&self, trace: &[Smid], files: &SimpleFiles<String, String>) -> String {
        let elements: Vec<_> = trace
            .iter()
            .filter_map(|smid| {
                if let Some(info) = self.source.get(smid.get()) {
                    let display_text = info
                        .annotation
                        .as_deref()
                        .and_then(intrinsic_display_name)
                        .or_else(|| {
                            info.file
                                .and_then(|id| files.get(id).ok())
                                .and_then(|file| {
                                    info.span
                                        .and_then(|span| file.source().get(Range::from(span)))
                                })
                        });
                    display_text.map(|text| format!("- {text}"))
                } else {
                    None
                }
            })
            .collect();

        elements.as_slice().join("\n")
    }
}

/// Map internal intrinsic names to user-facing display names.
///
/// Returns `None` for internal machinery that should be filtered out
/// of user-visible traces.
fn intrinsic_display_name(name: &str) -> Option<&str> {
    match name {
        // Arithmetic operators
        "ADD" => Some("+"),
        "SUB" => Some("-"),
        "MUL" => Some("*"),
        "DIV" => Some("/"),
        "MOD" => Some("mod"),
        "FLOOR" => Some("floor"),
        "CEILING" => Some("ceiling"),

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

        // Date/time
        "ZDT" => Some("zdt"),

        // Internal machinery — filter out of traces
        "AND" | "OR" | "SATURATED" | "RENDER" | "EMITT" | "EMITF" | "IFIELDS" | "SUPPRESSES"
        | "KNIL" | "DQ" | "REQUIRES" => None,

        // Unknown — show as-is
        other => Some(other),
    }
}
