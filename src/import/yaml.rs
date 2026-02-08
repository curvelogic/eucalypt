//! Import YAML and transform into core syntax
use crate::core::{desugar::Desugarer, expr::*};
use crate::import::error::SourceError;
use crate::{
    common::sourcemap::{Smid, SourceMap},
    syntax::parser,
};
use chrono::{DateTime, FixedOffset, NaiveDate, NaiveDateTime};
use codespan::{ByteIndex, ByteOffset, Span};
use codespan_reporting::files::SimpleFiles;
use lazy_static::lazy_static;
use moniker::{Embed, FreeVar};
use regex::{Regex, RegexSet};
use std::{collections::HashMap, iter};
use std::{convert::TryInto, str::FromStr};
use yaml_rust::parser::{Event, MarkedEventReceiver, Parser};
use yaml_rust::scanner::{Marker, TScalarStyle, TokenType};

/// Read the YAML into a core expression
///
/// Use `source_map` to create new SMIDs and `file_id` for error reporting.
pub fn read_yaml<'smap>(
    files: &'smap mut SimpleFiles<String, String>,
    source_map: &'smap mut SourceMap,
    file_id: usize,
    text: &'smap str,
) -> Result<RcExpr, SourceError> {
    let mut receiver = Receiver::new(files, source_map, file_id);
    Parser::new(text.chars())
        .load(&mut receiver, false)
        .map_err(|scan_error| {
            let i = ByteIndex(
                scan_error
                    .marker()
                    .index()
                    .try_into()
                    .expect("YAML error index should fit in ByteIndex"),
            );
            SourceError::InvalidYaml(scan_error.to_string(), file_id, Span::new(i, i))
        })?;

    // Check for deferred errors (e.g., undefined alias)
    if let Some(err) = receiver.error {
        return Err(err);
    }

    Ok(receiver.core())
}

/// Parse event handler for accumulating core expression
struct Receiver<'smap> {
    stack: Vec<Expectation>,
    files: &'smap mut SimpleFiles<String, String>,
    file_id: usize,
    source_map: &'smap mut SourceMap,
    /// Map from anchor IDs to their resolved expressions (for alias support)
    anchors: HashMap<usize, RcExpr>,
    /// Error encountered during parsing (deferred because on_event can't return Result)
    error: Option<SourceError>,
}

impl<'smap> Receiver<'smap> {
    /// Create a blank parse event receiver for file with id `file_id`
    pub fn new(
        files: &'smap mut SimpleFiles<String, String>,
        source_map: &'smap mut SourceMap,
        file_id: usize,
    ) -> Self {
        Receiver {
            files,
            stack: Vec::new(),
            file_id,
            source_map,
            anchors: HashMap::new(),
            error: None,
        }
    }

    /// Retrieve the parsed core expression
    pub fn core(&self) -> RcExpr {
        if let Some(Expectation::Value(expr)) = self.stack.last() {
            expr.clone()
        } else {
            acore::block(iter::empty())
        }
    }

    /// Mint a new smid for the specified span
    pub fn new_smid(&mut self, span: Span) -> Smid {
        self.source_map.add(self.file_id, span)
    }

    /// Complete the currently accumulating list
    fn complete_list(&mut self, end: ByteIndex) {
        if let Some(Expectation::ListAccumulation(start, items, anchor_id)) = self.stack.pop() {
            let smid = self.new_smid(Span::new(start, end));
            let expr = core::list(smid, items);
            // Store in anchors map if this list was anchored
            if let Some(id) = anchor_id {
                self.anchors.insert(id, expr.clone());
            }
            self.feed(expr);
        } else {
            panic!("unexpected complete_list parse event")
        }
    }

    /// Complete the currently accumulating block
    fn complete_block(&mut self, end: ByteIndex) {
        if let Some(Expectation::EvenBlockAccumulation(start, entries, anchor_id)) =
            self.stack.pop()
        {
            let smid = self.new_smid(Span::new(start, end));

            let fvs: HashMap<String, FreeVar<String>> =
                entries.iter().map(|(k, _)| (k.clone(), free(k))).collect();
            let kvs = entries
                .iter()
                .map(|(k, v)| {
                    (
                        fvs.get(k)
                            .expect("free variable should exist for key")
                            .clone(),
                        v.substs_free(&|n| {
                            fvs.get(n).map(|fv| core::var(Smid::default(), fv.clone()))
                        }),
                    )
                })
                .collect();
            let expr = core::default_let(smid, kvs);
            // Store in anchors map if this block was anchored
            if let Some(id) = anchor_id {
                self.anchors.insert(id, expr.clone());
            }
            self.feed(expr);
        } else {
            panic!("unexpected complete_block parse event")
        }
    }

    /// Feed a converted value into whatever the current expectation is
    pub fn feed(&mut self, expr: RcExpr) {
        if let Some(expectation) = self.stack.pop() {
            // Check for merge key handling
            let new_expectation = match expectation {
                Expectation::OddBlockAccumulation(off, entries, key, anchor_id) if key == "<<" => {
                    match self.handle_merge(expr, off, entries, anchor_id) {
                        Ok(new_exp) => new_exp,
                        Err(err) => {
                            self.error = Some(err);
                            return;
                        }
                    }
                }
                other => other.feed(expr),
            };
            self.stack.push(new_expectation);
        } else {
            // done
            self.stack.push(Expectation::Value(expr));
        }
    }

    /// Handle YAML merge key (<<) by extracting entries from the merge value
    fn handle_merge(
        &self,
        expr: RcExpr,
        off: ByteIndex,
        mut entries: Vec<(String, RcExpr)>,
        anchor_id: Option<usize>,
    ) -> Result<Expectation, SourceError> {
        // Extract entries to merge, using block offset for error reporting
        let value_span = Span::new(off, off);
        let merge_entries = self.extract_merge_entries(&expr, value_span)?;

        // Merge: add entries that don't already exist (explicit keys override merged ones)
        for (key, value) in merge_entries {
            if !entries.iter().any(|(k, _)| k == &key) {
                entries.push((key, value));
            }
        }

        Ok(Expectation::EvenBlockAccumulation(off, entries, anchor_id))
    }

    /// Extract key-value entries from an expression for merge key handling
    ///
    /// Returns entries from a mapping, or from each mapping in a list.
    /// Returns an error if the expression is not a valid merge target.
    /// The `error_span` is used to report the location of errors.
    fn extract_merge_entries(
        &self,
        expr: &RcExpr,
        error_span: Span,
    ) -> Result<Vec<(String, RcExpr)>, SourceError> {
        match &*expr.inner {
            Expr::Let(_, scope, LetType::DefaultBlockLet) => {
                let (binders, _body) = scope.clone().unbind();
                let entries: Vec<(String, RcExpr)> = binders
                    .unrec()
                    .into_iter()
                    .map(|(binder, Embed(value))| {
                        let name = binder.0.pretty_name.clone().ok_or_else(|| {
                            SourceError::InvalidYaml(
                                "merge key source has entry without a name".to_string(),
                                self.file_id,
                                error_span,
                            )
                        })?;
                        Ok::<_, SourceError>((name, value))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(entries)
            }
            Expr::List(_, items) => {
                // For multiple merge (<<: [*a, *b]), merge in order
                // Later items in the list override earlier ones
                let mut all_entries: Vec<(String, RcExpr)> = Vec::new();
                for item in items {
                    let item_entries = self.extract_merge_entries(item, error_span)?;
                    for (key, value) in item_entries {
                        // Remove any existing entry with this key (later overrides earlier)
                        all_entries.retain(|(k, _)| k != &key);
                        all_entries.push((key, value));
                    }
                }
                Ok(all_entries)
            }
            _ => {
                // Invalid merge target - must be a mapping or list of mappings
                // Get span from the expression's source map entry
                let span = self
                    .source_map
                    .source_info(expr)
                    .and_then(|info| info.span)
                    .unwrap_or(error_span);
                Err(SourceError::InvalidYaml(
                    "merge key value must be a mapping or list of mappings".to_string(),
                    self.file_id,
                    span,
                ))
            }
        }
    }

    /// Convert a scalar from its text representation informed by tags
    /// if available
    pub fn convert_scalar(
        &mut self,
        byte_index: ByteIndex,
        text: String,
        style: TScalarStyle,
        _anchor_id: usize,
        token_type: Option<TokenType>,
    ) -> Result<RcExpr, SourceError> {
        let span = Span::new(byte_index, byte_index);
        let smid = self.new_smid(span);

        let tag = token_type
            .and_then(Tag::from_token_type)
            .unwrap_or_else(|| {
                if let TScalarStyle::Plain = style {
                    infer_tag_for_plain_scalar(&text)
                } else {
                    Tag::Str
                }
            });

        match tag {
            Tag::Null => Ok(core::null(smid)),
            Tag::Bool => Ok(core::bool_(smid, parse_bool(&text))),
            Tag::Str => Ok(core::str(smid, text)),
            Tag::Int | Tag::Float => serde_json::Number::from_str(&text)
                .map_err(|_e| SourceError::InvalidNumber(text, self.file_id, span))
                .map(|n| core::num(smid, n)),
            Tag::Timestamp => {
                // Convert timestamp to ZDT expression via ZDT.PARSE
                // Normalize space separator to T for ZDT.PARSE compatibility
                let normalized = normalize_timestamp(&text);
                Ok(core::app(
                    smid,
                    core::bif(smid, "ZDT.PARSE"),
                    vec![core::str(smid, normalized)],
                ))
            }
            Tag::General(_, content) => match content.as_ref() {
                "eu" => self.parse_eu(text),
                "eu::fn" => self.parse_eu_fn(text),
                "eu::suppress" => Ok(core::meta(
                    smid,
                    core::str(smid, text),
                    acore::block(iter::once(("export".to_string(), acore::sym("suppress")))),
                )),
                t => Ok(core::meta(
                    smid,
                    core::str(smid, text),
                    acore::block(iter::once(("tag".to_string(), acore::str(format!("!{t}"))))),
                )),
            },
        }
    }

    pub fn convert_key_scalar(
        &mut self,
        byte_index: ByteIndex,
        text: String,
    ) -> Result<RcExpr, SourceError> {
        let span = Span::new(byte_index, byte_index);
        let smid = self.new_smid(span);
        Ok(core::sym(smid, text))
    }

    /// Parse eucalypt expression embedded in YAML source
    pub fn parse_eu(&mut self, text: String) -> Result<RcExpr, SourceError> {
        let span = Span::new(ByteIndex(0), ByteIndex(0) + ByteOffset::from_str_len(&text));
        let file_id = self.files.add(format!("yaml:[{text}]"), text);
        let ast = parser::parse_expression(self.files, file_id)
            .map_err(|p| SourceError::EmbeddedParserError(p, file_id, span))?;

        let content = HashMap::new();
        let mut desugarer = Desugarer::new(&content, self.source_map);
        desugarer
            .translate_simple(file_id, &ast)
            .map_err(|c| SourceError::EmbeddedCoreError(c, file_id, span))
    }

    /// Parse eucalypt expression embedded in YAML source
    pub fn parse_eu_fn(&mut self, text: String) -> Result<RcExpr, SourceError> {
        let span = Span::new(ByteIndex(0), ByteIndex(0) + ByteOffset::from_str_len(&text));
        let file_id = self.files.add(format!("yaml:[{text}]"), text);
        let (head, body) = parser::parse_embedded_lambda(self.files, file_id)
            .map_err(|p| SourceError::EmbeddedParserError(p, file_id, span))?;

        let content = HashMap::new();
        let mut desugarer = Desugarer::new(&content, self.source_map);

        let lam_body = desugarer
            .translate_simple(file_id, &body)
            .map_err(|c| SourceError::EmbeddedCoreError(c, file_id, span))?;

        // Extract parameter names from the Rowan ApplyTuple
        let mut fvs: HashMap<String, FreeVar<String>> = HashMap::new();
        for item in head.items() {
            // Each item in the ApplyTuple should be a single identifier
            let elements: Vec<_> = item.elements().collect();
            if elements.len() == 1 {
                if let crate::syntax::rowan::ast::Element::Name(name) = &elements[0] {
                    if let Some(identifier) = name.identifier() {
                        if let Some(param_name) = identifier.name() {
                            fvs.insert(param_name.to_string(), free(param_name));
                        }
                    }
                }
            }
        }

        let lam_body =
            lam_body.substs_free(&|n| fvs.get(n).map(|fv| core::var(Smid::default(), fv.clone())));

        let core = acore::lam(fvs.values().cloned().collect(), lam_body);
        Ok(core)
    }
}

/// Represent core conversion state and what we're expecting next
pub enum Expectation {
    Value(RcExpr),
    /// List accumulation: (start_index, items, optional_anchor_id)
    ListAccumulation(ByteIndex, Vec<RcExpr>, Option<usize>),
    /// Block accumulation (even state = waiting for key): (start_index, entries, optional_anchor_id)
    EvenBlockAccumulation(ByteIndex, Vec<(String, RcExpr)>, Option<usize>),
    /// Block accumulation (odd state = waiting for value): (start_index, entries, key, optional_anchor_id)
    OddBlockAccumulation(ByteIndex, Vec<(String, RcExpr)>, String, Option<usize>),
}

impl Expectation {
    pub fn new_list(i: ByteIndex, anchor_id: Option<usize>) -> Self {
        Expectation::ListAccumulation(i, Vec::new(), anchor_id)
    }

    pub fn new_block(i: ByteIndex, anchor_id: Option<usize>) -> Self {
        Expectation::EvenBlockAccumulation(i, Vec::new(), anchor_id)
    }

    pub fn feed(self, expr: RcExpr) -> Expectation {
        match self {
            Expectation::ListAccumulation(off, mut items, anchor_id) => {
                items.push(expr);
                Expectation::ListAccumulation(off, items, anchor_id)
            }
            Expectation::EvenBlockAccumulation(off, entries, anchor_id) => {
                if let Expr::Literal(_, prim) = &*expr.inner {
                    let key = match prim {
                        Primitive::Str(s) => s,
                        Primitive::Sym(s) => s,
                        _ => panic!("bad key type"),
                    };
                    Expectation::OddBlockAccumulation(off, entries, key.to_string(), anchor_id)
                } else {
                    panic!("bad key type")
                }
            }
            Expectation::OddBlockAccumulation(off, mut entries, key, anchor_id) => {
                // Remove any existing entry with this key (allows later keys to override merged ones)
                entries.retain(|(k, _)| k != &key);
                entries.push((key, expr));
                Expectation::EvenBlockAccumulation(off, entries, anchor_id)
            }
            _ => Expectation::Value(expr),
        }
    }
}

pub fn byte_index(m: Marker) -> ByteIndex {
    ByteIndex(m.index() as u32)
}

impl MarkedEventReceiver for Receiver<'_> {
    fn on_event(&mut self, event: Event, marker: Marker) {
        // Skip processing if we already have an error
        if self.error.is_some() {
            return;
        }

        match event {
            Event::Nothing => {}
            Event::StreamStart => {}
            Event::StreamEnd => {}
            Event::DocumentStart => {}
            Event::DocumentEnd => {}
            Event::Alias(anchor_id) => {
                // Look up the anchored expression and feed it
                if let Some(expr) = self.anchors.get(&anchor_id) {
                    self.feed(expr.clone());
                } else {
                    // Record error for undefined alias
                    let i = byte_index(marker);
                    self.error = Some(SourceError::InvalidYaml(
                        format!("undefined alias for anchor {anchor_id}"),
                        self.file_id,
                        Span::new(i, i),
                    ));
                }
            }
            Event::Scalar(text, style, anchor_id, token_type) => {
                if matches!(
                    self.stack.last(),
                    Some(Expectation::EvenBlockAccumulation(..))
                ) {
                    match self.convert_key_scalar(byte_index(marker), text) {
                        Ok(key) => self.feed(key),
                        Err(err) => {
                            self.error = Some(err);
                        }
                    }
                } else {
                    match self.convert_scalar(
                        byte_index(marker),
                        text,
                        style,
                        anchor_id,
                        token_type,
                    ) {
                        Ok(value) => {
                            // Store in anchors map if this scalar was anchored.
                            // yaml-rust uses anchor_id > 0 to indicate an anchored element; 0 means not anchored.
                            if anchor_id > 0 {
                                self.anchors.insert(anchor_id, value.clone());
                            }
                            self.feed(value);
                        }
                        Err(err) => {
                            self.error = Some(err);
                        }
                    }
                }
            }
            Event::SequenceStart(anchor_id) => {
                // anchor_id > 0 indicates an anchored element (yaml-rust convention)
                let anchor = if anchor_id > 0 { Some(anchor_id) } else { None };
                self.stack
                    .push(Expectation::new_list(byte_index(marker), anchor));
            }
            Event::SequenceEnd => {
                self.complete_list(byte_index(marker));
            }
            Event::MappingStart(anchor_id) => {
                // anchor_id > 0 indicates an anchored element (yaml-rust convention)
                let anchor = if anchor_id > 0 { Some(anchor_id) } else { None };
                self.stack
                    .push(Expectation::new_block(byte_index(marker), anchor));
            }
            Event::MappingEnd => {
                self.complete_block(byte_index(marker));
            }
        }
    }
}

/// YAML tags
#[derive(Debug, PartialEq, Eq)]
pub enum Tag {
    Null,
    Bool,
    Int,
    Float,
    Str,
    Timestamp,
    General(String, String),
}

impl Tag {
    /// When rust_yaml passes token type we'll use it
    fn from_token_type(token_type: TokenType) -> Option<Self> {
        if let TokenType::Tag(handle, content) = token_type {
            if handle == "!!" {
                match content.as_ref() {
                    "null" => Some(Tag::Null),
                    "bool" => Some(Tag::Bool),
                    "int" => Some(Tag::Int),
                    "float" => Some(Tag::Float),
                    "str" => Some(Tag::Str),
                    _ => None,
                }
            } else {
                Some(Tag::General(handle, content))
            }
        } else {
            None
        }
    }
}

lazy_static! {
    /// Compiled regex set for YAML scalar tag inference.
    /// Compiled once and reused for all plain scalar processing.
    #[allow(clippy::trivial_regex)]
    static ref YAML_SCALAR_PATTERNS: RegexSet = RegexSet::new([
        r"^[nN]ull$",
        r"^NULL$",
        r"^~$",
        r"^$",
        r"^[tT]rue$",
        r"^[fF]alse$",
        r"^TRUE|FALSE$",
        r"^[-+]?[0-9]+$",
        r"^0o[0-7]+$",
        r"^0x[0-9a-fA-F]+$",
        r"^[-+]?(\.[0-9]+|[0-9]+(\.[0-9]*)?)([eE][-+]?[0-9]+)?$",
        r"^[-+]?(\.inf|\.Inf|\.INF)$",
        r"^\.nan|\.NaN|\.NAN$",
    ])
    .expect("YAML scalar patterns should compile");
}

/// If tags aren't explicit, infer from value
fn infer_tag_for_plain_scalar(text: &str) -> Tag {
    if let Some(first_match) = YAML_SCALAR_PATTERNS.matches(text).into_iter().min() {
        match first_match {
            0..=3 => Tag::Null,
            4..=6 => Tag::Bool,
            7..=9 => Tag::Int,
            10..=12 => Tag::Float,
            _ => Tag::Str,
        }
    } else if is_timestamp(text) {
        Tag::Timestamp
    } else {
        Tag::Str
    }
}

/// Check if text looks like a YAML timestamp and can be parsed
///
/// Supported formats:
/// - Date only: 2023-01-15 (midnight UTC)
/// - ISO 8601 UTC: 2023-01-15T10:30:00Z
/// - ISO 8601 offset: 2023-01-15T10:30:00+05:00
/// - Space separator: 2023-01-15 10:30:00 (UTC)
/// - Fractional seconds: 2023-01-15T10:30:00.123456Z
fn is_timestamp(text: &str) -> bool {
    // YAML timestamp pattern - matches formats like:
    // YYYY-MM-DD
    // YYYY-MM-DD[T ]HH:MM:SS
    // YYYY-MM-DD[T ]HH:MM:SS.fraction
    // YYYY-MM-DD[T ]HH:MM:SS[Z|±HH:MM|±HHMM|±HH]
    let timestamp_regex =
        Regex::new(r"^\d{4}-\d{2}-\d{2}([T ]\d{2}:\d{2}:\d{2}(\.\d+)?(Z|[+-]\d{2}(:\d{2})?)?)?$")
            .unwrap();

    if !timestamp_regex.is_match(text) {
        return false;
    }

    // Validate by attempting to parse with chrono
    parse_timestamp(text).is_some()
}

/// Normalize a YAML timestamp for ZDT.PARSE
///
/// Converts space-separated timestamps to T-separated ones and ensures
/// timestamps without timezone get 'Z' appended for UTC.
fn normalize_timestamp(text: &str) -> String {
    // Replace space separator with T (YAML allows "2023-01-15 10:30:00")
    let normalized = if text.contains(' ') && !text.contains('T') {
        text.replacen(' ', "T", 1)
    } else {
        text.to_string()
    };

    // If there's a time component but no timezone, append Z for UTC
    if let Some(t_pos) = normalized.find('T') {
        let after_t = &normalized[t_pos..];
        // Check for existing timezone: Z, +offset, or -offset (after T)
        if !after_t.ends_with('Z') && !after_t.contains('+') && !after_t.contains('-') {
            return format!("{normalized}Z");
        }
    }

    normalized
}

/// Parse a YAML timestamp string into a DateTime
///
/// Returns None if the timestamp is invalid (e.g., invalid date components)
fn parse_timestamp(text: &str) -> Option<DateTime<FixedOffset>> {
    // Try full RFC3339 format (2023-01-15T10:30:00Z or 2023-01-15T10:30:00+05:00)
    if let Ok(dt) = DateTime::parse_from_rfc3339(text) {
        return Some(dt);
    }

    // Try RFC3339 with space separator (2023-01-15 10:30:00Z)
    if let Ok(dt) = DateTime::parse_from_str(text, "%Y-%m-%d %H:%M:%S%:z") {
        return Some(dt);
    }
    if let Ok(dt) = DateTime::parse_from_str(text, "%Y-%m-%d %H:%M:%SZ") {
        return Some(DateTime::from_naive_utc_and_offset(
            dt.naive_utc(),
            FixedOffset::east_opt(0).unwrap(),
        ));
    }

    // Try NaiveDateTime with T separator (2023-01-15T10:30:00) - assume UTC
    if let Ok(ndt) = NaiveDateTime::parse_from_str(text, "%Y-%m-%dT%H:%M:%S") {
        return Some(DateTime::from_naive_utc_and_offset(
            ndt,
            FixedOffset::east_opt(0).unwrap(),
        ));
    }

    // Try NaiveDateTime with T separator and fractional seconds
    if let Ok(ndt) = NaiveDateTime::parse_from_str(text, "%Y-%m-%dT%H:%M:%S%.f") {
        return Some(DateTime::from_naive_utc_and_offset(
            ndt,
            FixedOffset::east_opt(0).unwrap(),
        ));
    }

    // Try NaiveDateTime with space separator (2023-01-15 10:30:00) - assume UTC
    if let Ok(ndt) = NaiveDateTime::parse_from_str(text, "%Y-%m-%d %H:%M:%S") {
        return Some(DateTime::from_naive_utc_and_offset(
            ndt,
            FixedOffset::east_opt(0).unwrap(),
        ));
    }

    // Try NaiveDateTime with space separator and fractional seconds
    if let Ok(ndt) = NaiveDateTime::parse_from_str(text, "%Y-%m-%d %H:%M:%S%.f") {
        return Some(DateTime::from_naive_utc_and_offset(
            ndt,
            FixedOffset::east_opt(0).unwrap(),
        ));
    }

    // Try date-only format (2023-01-15) - midnight UTC
    if let Ok(nd) = NaiveDate::parse_from_str(text, "%Y-%m-%d") {
        let ndt = nd.and_hms_opt(0, 0, 0)?;
        return Some(DateTime::from_naive_utc_and_offset(
            ndt,
            FixedOffset::east_opt(0).unwrap(),
        ));
    }

    None
}

fn parse_bool(text: &str) -> bool {
    matches!(text, "TRUE" | "True" | "true")
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::core::expr::acore;
    use codespan_reporting::files::SimpleFiles;
    use moniker::assert_term_eq;
    use std::iter;

    fn parse(text: &str) -> RcExpr {
        try_parse(text).expect("YAML parse failed")
    }

    fn try_parse(text: &str) -> Result<RcExpr, SourceError> {
        let mut sm = SourceMap::new();
        let mut files = SimpleFiles::new();
        let file_id = files.add("test.yaml".to_string(), text.to_string());
        read_yaml(&mut files, &mut sm, file_id, text)
    }

    const SAMPLE1: &str = "
hello: !blah world
x: y
z: !!int 1234232353
";

    #[test]
    pub fn test_sample() {
        let parsed = parse(SAMPLE1);
        let expected = acore::default_let(vec![
            (
                free("hello"),
                acore::meta(
                    acore::str("world"),
                    acore::block(iter::once(("tag".to_string(), acore::str("!blah")))),
                ),
            ),
            (free("x"), acore::str("y")),
            (free("z"), acore::num(12342323535_u64)),
        ]);

        assert_term_eq!(parsed, expected);
    }

    #[test]
    pub fn test_accepts_empty_input() {
        assert_term_eq!(parse(""), acore::block(iter::empty()));
    }

    #[test]
    pub fn test_parses_key_value() {
        assert_term_eq!(
            parse("a: !!int 1234"),
            acore::default_let(vec![(free("a"), acore::num(1234))])
        );
    }

    #[test]
    pub fn test_parses_json_data() {
        assert_term_eq!(
            parse(" { a: [1, 2, 3], b: {x: \"y\"} } "),
            acore::default_let(vec![
                (
                    free("a"),
                    acore::list(vec![acore::num(1), acore::num(2), acore::num(3)])
                ),
                (
                    free("b"),
                    acore::default_let(vec![(free("x"), acore::str("y"))])
                )
            ])
        );
    }

    #[test]
    pub fn test_parses_bools() {
        let sample = "a: true\nb: True\nc: TRUE\nd: false\ne: False\nf: FALSE ";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![
                (free("a"), acore::bool_(true)),
                (free("b"), acore::bool_(true)),
                (free("c"), acore::bool_(true)),
                (free("d"), acore::bool_(false)),
                (free("e"), acore::bool_(false)),
                (free("f"), acore::bool_(false))
            ])
        );
    }

    #[test]
    pub fn test_resolves_unknown_tags() {
        assert_eq!(infer_tag_for_plain_scalar("null"), Tag::Null);
        assert_eq!(infer_tag_for_plain_scalar("Null"), Tag::Null);
        assert_eq!(infer_tag_for_plain_scalar("NULL"), Tag::Null);
        assert_eq!(infer_tag_for_plain_scalar("nULL"), Tag::Str);
        assert_eq!(infer_tag_for_plain_scalar(""), Tag::Null);
        assert_eq!(infer_tag_for_plain_scalar("true"), Tag::Bool);
        assert_eq!(infer_tag_for_plain_scalar("True"), Tag::Bool);
        assert_eq!(infer_tag_for_plain_scalar("TRUE"), Tag::Bool);
        assert_eq!(infer_tag_for_plain_scalar("tRUE"), Tag::Str);
        assert_eq!(infer_tag_for_plain_scalar("false"), Tag::Bool);
        assert_eq!(infer_tag_for_plain_scalar("False"), Tag::Bool);
        assert_eq!(infer_tag_for_plain_scalar("FALSE"), Tag::Bool);
        assert_eq!(infer_tag_for_plain_scalar("fALSE"), Tag::Str);
        assert_eq!(infer_tag_for_plain_scalar("0"), Tag::Int);
        assert_eq!(infer_tag_for_plain_scalar("0o7"), Tag::Int);
        assert_eq!(infer_tag_for_plain_scalar("0x3A"), Tag::Int);
        assert_eq!(infer_tag_for_plain_scalar("-19"), Tag::Int);
        assert_eq!(infer_tag_for_plain_scalar("0."), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar("-0.0"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar(".5"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar("+12e03"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar("-2E+05"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar(".inf"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar("-.Inf"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar("+.INF"), Tag::Float);
        assert_eq!(infer_tag_for_plain_scalar(".NAN"), Tag::Float);
    }

    // Timestamp detection tests

    #[test]
    pub fn test_detects_timestamps() {
        // Date only
        assert_eq!(infer_tag_for_plain_scalar("2023-01-15"), Tag::Timestamp);
        assert_eq!(infer_tag_for_plain_scalar("1999-12-31"), Tag::Timestamp);

        // ISO 8601 UTC
        assert_eq!(
            infer_tag_for_plain_scalar("2023-01-15T10:30:00Z"),
            Tag::Timestamp
        );
        assert_eq!(
            infer_tag_for_plain_scalar("2023-01-15T00:00:00Z"),
            Tag::Timestamp
        );

        // ISO 8601 with offset
        assert_eq!(
            infer_tag_for_plain_scalar("2023-01-15T10:30:00+05:00"),
            Tag::Timestamp
        );
        assert_eq!(
            infer_tag_for_plain_scalar("2023-01-15T10:30:00-08:00"),
            Tag::Timestamp
        );

        // Space separator (UTC)
        assert_eq!(
            infer_tag_for_plain_scalar("2023-01-15 10:30:00"),
            Tag::Timestamp
        );

        // Fractional seconds
        assert_eq!(
            infer_tag_for_plain_scalar("2023-01-15T10:30:00.123Z"),
            Tag::Timestamp
        );
        assert_eq!(
            infer_tag_for_plain_scalar("2023-01-15T10:30:00.123456Z"),
            Tag::Timestamp
        );
        assert_eq!(
            infer_tag_for_plain_scalar("2023-01-15 10:30:00.5"),
            Tag::Timestamp
        );
    }

    #[test]
    pub fn test_invalid_timestamps_fall_back_to_string() {
        // Invalid dates fall back to string
        assert_eq!(infer_tag_for_plain_scalar("2023-13-45"), Tag::Str);
        assert_eq!(infer_tag_for_plain_scalar("2023-02-30"), Tag::Str);
        assert_eq!(infer_tag_for_plain_scalar("2023-00-01"), Tag::Str);

        // Invalid times fall back to string
        assert_eq!(infer_tag_for_plain_scalar("2023-01-15T25:00:00Z"), Tag::Str);
        assert_eq!(infer_tag_for_plain_scalar("2023-01-15T10:60:00Z"), Tag::Str);

        // Not matching timestamp pattern at all
        assert_eq!(infer_tag_for_plain_scalar("not-a-timestamp"), Tag::Str);
        assert_eq!(infer_tag_for_plain_scalar("2023/01/15"), Tag::Str);
        assert_eq!(infer_tag_for_plain_scalar("15-01-2023"), Tag::Str);
    }

    #[test]
    pub fn test_parses_timestamp_yaml() {
        // Parsing YAML with timestamps should produce ZDT.PARSE calls
        let parsed = parse("ts: 2023-01-15T10:30:00Z");
        // We can't easily compare the full AST because ZDT.PARSE is a BIF call,
        // but we can verify it parses without error and produces a block
        assert!(matches!(
            &*parsed.inner,
            Expr::Let(_, _, LetType::DefaultBlockLet)
        ));
    }

    #[test]
    pub fn test_quoted_timestamps_remain_strings() {
        // Quoted strings should remain strings, not be converted to timestamps
        let parsed = parse(r#"ts: "2023-01-15T10:30:00Z""#);
        let expected = acore::default_let(vec![(free("ts"), acore::str("2023-01-15T10:30:00Z"))]);
        assert_term_eq!(parsed, expected);
    }

    #[test]
    pub fn test_normalize_timestamp() {
        // Space separator gets converted to T
        assert_eq!(
            normalize_timestamp("2023-01-15 10:30:00"),
            "2023-01-15T10:30:00Z"
        );

        // T separator preserved
        assert_eq!(
            normalize_timestamp("2023-01-15T10:30:00"),
            "2023-01-15T10:30:00Z"
        );

        // Already has timezone - no changes
        assert_eq!(
            normalize_timestamp("2023-01-15T10:30:00Z"),
            "2023-01-15T10:30:00Z"
        );
        assert_eq!(
            normalize_timestamp("2023-01-15T10:30:00+05:00"),
            "2023-01-15T10:30:00+05:00"
        );

        // Date only - no changes (no time component)
        assert_eq!(normalize_timestamp("2023-01-15"), "2023-01-15");

        // Fractional seconds with space
        assert_eq!(
            normalize_timestamp("2023-01-15 10:30:00.123"),
            "2023-01-15T10:30:00.123Z"
        );
    }

    // Anchor and alias tests

    #[test]
    pub fn test_anchor_alias_with_number() {
        let sample = "value: &val 42\nref: *val";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![
                (free("value"), acore::num(42)),
                (free("ref"), acore::num(42))
            ])
        );
    }

    #[test]
    pub fn test_anchor_alias_with_list() {
        let sample = "items: &items [1, 2, 3]\ncopy: *items";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![
                (
                    free("items"),
                    acore::list(vec![acore::num(1), acore::num(2), acore::num(3)])
                ),
                (
                    free("copy"),
                    acore::list(vec![acore::num(1), acore::num(2), acore::num(3)])
                )
            ])
        );
    }

    #[test]
    pub fn test_anchor_alias_with_mapping() {
        let sample = "base: &base\n  x: 1\n  y: 2\nref: *base";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![
                (
                    free("base"),
                    acore::default_let(vec![
                        (free("x"), acore::num(1)),
                        (free("y"), acore::num(2))
                    ])
                ),
                (
                    free("ref"),
                    acore::default_let(vec![
                        (free("x"), acore::num(1)),
                        (free("y"), acore::num(2))
                    ])
                )
            ])
        );
    }

    #[test]
    pub fn test_multiple_aliases_same_anchor() {
        let sample = "source: &src hello\na: *src\nb: *src";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![
                (free("source"), acore::str("hello")),
                (free("a"), acore::str("hello")),
                (free("b"), acore::str("hello"))
            ])
        );
    }

    #[test]
    pub fn test_undefined_alias_error() {
        let sample = "value: *undefined";
        let result = try_parse(sample);
        assert!(result.is_err(), "Expected error for undefined alias");
        let err = result.unwrap_err();
        match err {
            SourceError::InvalidYaml(msg, _, _) => {
                // yaml-rust reports "unknown anchor" for undefined aliases
                assert!(
                    msg.contains("unknown anchor") || msg.contains("undefined alias"),
                    "Expected 'unknown anchor' or 'undefined alias' in error message, got: {msg}"
                );
            }
            _ => panic!("Expected InvalidYaml error, got: {err:?}"),
        }
    }

    #[test]
    pub fn test_nested_anchors() {
        // Anchor within an anchored structure
        let sample = "outer: &outer\n  inner: &inner 42\nref_outer: *outer\nref_inner: *inner";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![
                (
                    free("outer"),
                    acore::default_let(vec![(free("inner"), acore::num(42))])
                ),
                (
                    free("ref_outer"),
                    acore::default_let(vec![(free("inner"), acore::num(42))])
                ),
                (free("ref_inner"), acore::num(42))
            ])
        );
    }

    #[test]
    pub fn test_alias_in_list() {
        let sample = "name: &n Alice\npeople: [*n, Bob, *n]";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![
                (free("name"), acore::str("Alice")),
                (
                    free("people"),
                    acore::list(vec![
                        acore::str("Alice"),
                        acore::str("Bob"),
                        acore::str("Alice")
                    ])
                )
            ])
        );
    }

    // Merge key tests

    #[test]
    pub fn test_merge_key_single() {
        // Single merge: <<: *alias
        let sample = "base: &base\n  x: 1\n  y: 2\nderived:\n  <<: *base\n  z: 3";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![
                (
                    free("base"),
                    acore::default_let(vec![
                        (free("x"), acore::num(1)),
                        (free("y"), acore::num(2))
                    ])
                ),
                (
                    free("derived"),
                    acore::default_let(vec![
                        (free("x"), acore::num(1)),
                        (free("y"), acore::num(2)),
                        (free("z"), acore::num(3))
                    ])
                )
            ])
        );
    }

    #[test]
    pub fn test_merge_key_override() {
        // Later keys override merged values
        let sample = "base: &base\n  x: 1\n  y: 2\nderived:\n  <<: *base\n  y: 99";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![
                (
                    free("base"),
                    acore::default_let(vec![
                        (free("x"), acore::num(1)),
                        (free("y"), acore::num(2))
                    ])
                ),
                (
                    free("derived"),
                    acore::default_let(vec![
                        (free("x"), acore::num(1)),
                        (free("y"), acore::num(99))
                    ])
                )
            ])
        );
    }

    #[test]
    pub fn test_merge_key_explicit_before() {
        // Explicit keys before merge also override
        let sample = "base: &base\n  x: 1\nderived:\n  x: 99\n  <<: *base";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![
                (
                    free("base"),
                    acore::default_let(vec![(free("x"), acore::num(1))])
                ),
                (
                    free("derived"),
                    acore::default_let(vec![(free("x"), acore::num(99))])
                )
            ])
        );
    }

    #[test]
    pub fn test_merge_key_multiple() {
        // Multiple merge: <<: [*a, *b] - merge in order, later overrides earlier
        let sample = "a: &a\n  x: 1\nb: &b\n  x: 2\n  y: 3\nderived:\n  <<: [*a, *b]\n  z: 4";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![
                (
                    free("a"),
                    acore::default_let(vec![(free("x"), acore::num(1))])
                ),
                (
                    free("b"),
                    acore::default_let(vec![
                        (free("x"), acore::num(2)),
                        (free("y"), acore::num(3))
                    ])
                ),
                (
                    free("derived"),
                    acore::default_let(vec![
                        (free("x"), acore::num(2)), // from *b, overrides *a
                        (free("y"), acore::num(3)),
                        (free("z"), acore::num(4))
                    ])
                )
            ])
        );
    }

    #[test]
    pub fn test_merge_key_inline() {
        // Inline merge: <<: {key: value}
        let sample = "derived:\n  <<: {x: 1, y: 2}\n  z: 3";
        assert_term_eq!(
            parse(sample),
            acore::default_let(vec![(
                free("derived"),
                acore::default_let(vec![
                    (free("x"), acore::num(1)),
                    (free("y"), acore::num(2)),
                    (free("z"), acore::num(3))
                ])
            )])
        );
    }

    #[test]
    pub fn test_merge_key_invalid_target_error() {
        // Invalid merge target (non-mapping) should produce an error
        let sample = "derived:\n  <<: 42";
        let result = try_parse(sample);
        assert!(result.is_err(), "Expected error for invalid merge target");
        let err = result.unwrap_err();
        match err {
            SourceError::InvalidYaml(msg, _, span) => {
                assert!(
                    msg.contains("mapping"),
                    "Expected error about mapping, got: {msg}"
                );
                // Verify span points to the actual problematic location, not (0,0)
                // The value "42" starts at position 15 in "derived:\n  <<: 42"
                assert!(
                    span.start() != ByteIndex(0) || span.end() != ByteIndex(0),
                    "Span should not be (0,0), got: {:?}",
                    span
                );
            }
            _ => panic!("Expected InvalidYaml error, got: {err:?}"),
        }
    }

    #[test]
    pub fn test_invalid_number_scalar_produces_error() {
        // Invalid numbers (like incomplete hex literals) should produce an
        // InvalidNumber error rather than panicking
        let sample = "value: !!int 0x";
        let result = try_parse(sample);
        assert!(result.is_err(), "Expected error for invalid number scalar");
        let err = result.unwrap_err();
        match err {
            SourceError::InvalidNumber(text, _, _) => {
                assert_eq!(text, "0x", "Expected '0x' in error, got: {text}");
            }
            _ => panic!("Expected InvalidNumber error, got: {err:?}"),
        }
    }
}
