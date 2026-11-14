//! Source evidence for diagnostic hints (eu-oaz72.28).
//!
//! A hint that guesses at the cause of an error is shown only when the
//! source gives evidence for that guess. The evidence is read from the
//! text under a diagnostic's primary label, so it always describes what
//! the user is shown. Text is read through the eucalypt lexer, so what
//! counts as a name, an operator or a literal is exactly what the parser
//! reads, and nothing inside a string or a comment counts. With no source text available (the LSP, or any
//! caller that renders without the file database) no evidence is found
//! and evidence-gated hints are simply not shown.

use codespan_reporting::diagnostic::{Diagnostic, LabelStyle};
use codespan_reporting::files::{Files, SimpleFiles};

use crate::syntax::rowan::kind::SyntaxKind::{self, *};
use crate::syntax::rowan::lex::Lexer;

/// The source line under a diagnostic's primary label, with the label's
/// byte range within that line.
pub struct LabelSource<'a> {
    /// The whole line holding the start of the label.
    pub line: &'a str,
    /// Start of the label within `line`.
    pub start: usize,
    /// End of the label within `line` (clamped to the end of the line).
    pub end: usize,
    /// The rest of the source after the label, across lines.
    pub following: &'a str,
}

impl<'a> LabelSource<'a> {
    /// Locate the primary label of `diag` in `files`.
    pub fn of(diag: &Diagnostic<usize>, files: &'a SimpleFiles<String, String>) -> Option<Self> {
        let label = diag
            .labels
            .iter()
            .find(|l| l.style == LabelStyle::Primary)?;
        let source: &'a str = files.source(label.file_id).ok()?;
        let start = label.range.start;
        if start > source.len() || !source.is_char_boundary(start) {
            return None;
        }
        let line_start = source[..start].rfind('\n').map_or(0, |i| i + 1);
        let line_end = source[start..]
            .find('\n')
            .map_or(source.len(), |i| start + i);
        let end = label.range.end.clamp(start, line_end);
        let label_end = label.range.end.clamp(start, source.len());
        if !source.is_char_boundary(end) || !source.is_char_boundary(label_end) {
            return None;
        }
        Some(LabelSource {
            line: &source[line_start..line_end],
            start: start - line_start,
            end: end - line_start,
            following: &source[label_end..],
        })
    }

    /// The labelled text itself.
    pub fn text(&self) -> &'a str {
        &self.line[self.start..self.end]
    }

    /// The line after the label.
    pub fn after(&self) -> &'a str {
        &self.line[self.end..]
    }

    /// The line before the label.
    pub fn before(&self) -> &'a str {
        &self.line[..self.start]
    }

    /// Whether `word` appears on the line as a name.
    pub fn line_has_word(&self, word: &str) -> bool {
        has_word(self.line, word)
    }

    /// Whether the operator `op` appears on the line.
    pub fn line_has_operator(&self, op: &str) -> bool {
        tokens(self.line)
            .iter()
            .any(|&(k, t)| k == OPERATOR_IDENTIFIER && t == op)
    }

    /// Whether the labelled value is looked up with `.`: the label holds
    /// a lookup (`x.name`), or is followed by one, on the same line or a
    /// continuation line (`"abc".upper`, `[1, 2]` then `  .name`). A
    /// namespace call elsewhere on the line, such as `str.of`, is not
    /// a lookup on the labelled value.
    pub fn is_looked_up(&self) -> bool {
        // A lookup inside the label needs a value before its '.'.
        tokens(self.text()).windows(2).skip(1).any(is_lookup)
            || tokens(self.following).get(..2).is_some_and(is_lookup)
    }
}

/// The significant tokens of `text` as the eucalypt lexer reads them:
/// whitespace and comments are dropped.
pub fn tokens(text: &str) -> Vec<(SyntaxKind, &str)> {
    Lexer::from_text(text)
        .filter(|(k, _)| !matches!(k, WHITESPACE | COMMENT))
        .filter_map(|(k, span)| {
            text.get(span.start().to_usize()..span.end().to_usize())
                .map(|t| (k, t))
        })
        .collect()
}

/// Whether a pair of tokens is a `.` lookup of a key name: `.name`.
fn is_lookup(pair: &[(SyntaxKind, &str)]) -> bool {
    matches!(pair, [(OPERATOR_IDENTIFIER, "."), (UNQUOTED_IDENTIFIER, _)])
}

/// Whether `word` occurs in `text` as a name (not as part of a longer
/// name such as `order`, nor inside a string or comment).
pub fn has_word(text: &str, word: &str) -> bool {
    tokens(text)
        .iter()
        .any(|&(k, t)| k == UNQUOTED_IDENTIFIER && t == word)
}

/// Whether `name` occurs in `text` as a name not followed by `.` — as in
/// `str(x)`, `x str` or `x.str`, but not `str.of`.
pub fn uses_bare(text: &str, name: &str) -> bool {
    let toks = tokens(text);
    toks.iter().enumerate().any(|(i, &(k, t))| {
        k == UNQUOTED_IDENTIFIER
            && t == name
            && !matches!(toks.get(i + 1), Some((OPERATOR_IDENTIFIER, ".")))
    })
}

/// Whether `text` is a single operator, such as `+`, `=` or `>=`.
pub fn is_operator(text: &str) -> bool {
    matches!(tokens(text)[..], [(OPERATOR_IDENTIFIER, _)])
}

/// Whether `text` is a single number or string literal.
pub fn is_literal(text: &str) -> bool {
    matches!(tokens(text)[..], [(NUMBER | STRING, _)])
}

/// Whether the last token of `text` ends a value, so that a term written
/// after it is applied to it by catenation: not an operator, an opening
/// bracket, a `,` or a `:`.
pub fn ends_with_value(text: &str) -> bool {
    tokens(text).last().is_some_and(|&(k, _)| {
        !matches!(
            k,
            OPERATOR_IDENTIFIER
                | OPEN_PAREN
                | OPEN_PAREN_APPLY
                | OPEN_SQUARE
                | OPEN_SQUARE_APPLY
                | OPEN_BRACE
                | OPEN_BRACE_APPLY
                | COMMA
                | COLON
                | BACKTICK
        )
    })
}

/// Whether an unresolved `name`, with the text `before` and `after` it on
/// its line, has the shape of an arrow function: the name, possibly with
/// further parameters, followed by `->`, and either opening a call
/// argument or parameter list (`map(x -> y + 1)`, `(x, y) -> 1`) or used
/// again after the arrow (`x -> x + 1`). `xs foo -> 1` is the const
/// operator used correctly.
pub fn arrow_function_shape(before: &str, after: &str, name: &str) -> bool {
    let after = tokens(after);
    let Some(arrow) = after
        .iter()
        .position(|&(k, t)| k == OPERATOR_IDENTIFIER && t == "->")
    else {
        return false;
    };
    let params_only = after[..arrow]
        .iter()
        .all(|&(k, _)| matches!(k, UNQUOTED_IDENTIFIER | COMMA | CLOSE_PAREN));
    let opens_argument = matches!(
        tokens(before).last(),
        Some((OPEN_PAREN | OPEN_PAREN_APPLY | COMMA, _))
    );
    let used_again = after[arrow + 1..]
        .iter()
        .any(|&(k, t)| k == UNQUOTED_IDENTIFIER && t == name);
    params_only && (opens_argument || used_again)
}

/// The note stating how a catenation took an operator expression as its
/// pipeline step (eu-oaz72.29): `'c nth(i) = 2' was read as
/// 'c (nth(i) = 2)'; parenthesise the pipeline: '(c nth(i)) = 2'`.
///
/// The text is read from the source the extents in `operand` point into;
/// `None` when any of them cannot be read there.
pub fn catenation_note(
    operand: &crate::common::sourcemap::CatenationOperand,
    source_map: &crate::common::sourcemap::SourceMap,
    files: &SimpleFiles<String, String>,
) -> Option<String> {
    let file = source_map.source_info_for_smid(operand.pipeline.0)?.file?;
    let source: &str = files.source(file).ok()?;
    let atom = |smid| -> Option<(usize, usize)> {
        let info = source_map.source_info_for_smid(smid)?;
        if info.file? != file {
            return None;
        }
        let span = info.span?;
        widen_over_parens(source, span.start().to_usize(), span.end().to_usize())
    };
    let (pipeline_start, _) = atom(operand.pipeline.0)?;
    let (_, pipeline_end) = atom(operand.pipeline.1)?;
    let (step_start, _) = atom(operand.step.0)?;
    let (_, step_end) = atom(operand.step.1)?;
    let (_, head_end) = atom(operand.step_head_end)?;
    if !(pipeline_start <= pipeline_end
        && pipeline_end <= step_start
        && step_start <= head_end
        && head_end <= step_end)
    {
        return None;
    }
    let text = |a: usize, b: usize| source.get(a..b).map(one_line);
    let whole = text(pipeline_start, step_end)?;
    let pipeline = text(pipeline_start, pipeline_end)?;
    let step = text(step_start, step_end)?;
    let head = text(step_start, head_end)?;
    let rest = text(head_end, step_end)?;
    let parse = format!("note: '{whole}' was read as '{pipeline} ({step})'");
    if operand.pipeline_has_anaphor {
        // `(_ nth(0)) = 1` would make the parentheses the anaphor's scope.
        return Some(parse);
    }
    Some(format!(
        "{parse}; parenthesise the pipeline: '({pipeline} {head}){rest}'"
    ))
}

/// Widen the atom at `start..end` over parentheses written round exactly
/// it: `(x)` for the atom `x`.
fn widen_over_parens(source: &str, mut start: usize, mut end: usize) -> Option<(usize, usize)> {
    if start > end || end > source.len() {
        return None;
    }
    loop {
        let before = source.get(..start)?.trim_end();
        let after = source.get(end..)?;
        let after_trimmed = after.trim_start();
        if before.ends_with('(') && after_trimmed.starts_with(')') {
            start = before.len() - 1;
            end = source.len() - after_trimmed.len() + 1;
        } else {
            return Some((start, end));
        }
    }
}

/// `text` on one line as the lexer reads it: comments are dropped and each
/// run of whitespace, or whitespace and comments, becomes a single space
/// (eu-oaz72.133). `text` runs between atom boundaries, so it never starts
/// or ends inside a token.
fn one_line(text: &str) -> String {
    if !text.contains('\n') && !text.contains('#') {
        return text.to_string();
    }
    let mut out = String::with_capacity(text.len());
    let mut gap = false;
    for (kind, span) in Lexer::from_text(text) {
        if matches!(kind, WHITESPACE | COMMENT) {
            gap = true;
            continue;
        }
        let Some(token) = text.get(span.start().to_usize()..span.end().to_usize()) else {
            continue;
        };
        if gap && !out.is_empty() {
            out.push(' ');
        }
        gap = false;
        out.push_str(token);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn words_are_names() {
        assert!(has_word("x and y", "and"));
        assert!(has_word("(a)or(b)", "or"));
        assert!(!has_word("sort-order(band)", "or"));
        assert!(!has_word("random", "and"));
        assert!(!has_word("or-else", "or"));
        assert!(!has_word("\"this and that\"", "and"));
        assert!(!has_word("x # this and that", "and"));
    }

    #[test]
    fn bare_names() {
        assert!(uses_bare("x str", "str"));
        assert!(uses_bare("x.str", "str"));
        assert!(!uses_bare("str.of(x)", "str"));
    }

    #[test]
    fn lookups() {
        assert!(is_lookup(&tokens(".name")));
        assert!(!is_lookup(&tokens(".5")));
        assert!(!is_lookup(&tokens("{a: 1}")[..2]));
    }

    #[test]
    fn operators_and_literals() {
        assert!(is_operator("+"));
        assert!(is_operator(">="));
        assert!(!is_operator("f"));
        assert!(!is_operator("(x)"));
        assert!(is_literal("5"));
        assert!(is_literal("2.5"));
        assert!(is_literal("\"a\""));
        assert!(!is_literal("x"));
        assert!(!is_literal("-"));
    }

    #[test]
    fn arrow_shapes() {
        assert!(arrow_function_shape("xs map(", " -> y + 1)", "x"));
        assert!(arrow_function_shape("r: ", " -> x + 1", "x"));
        assert!(arrow_function_shape("(", ", y) -> 1", "x"));
        assert!(!arrow_function_shape("r: xs ", " -> 1", "foo"));
    }
}
