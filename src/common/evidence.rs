//! Source evidence for diagnostic hints (eu-oaz72.28).
//!
//! A hint that guesses at the cause of an error is shown only when the
//! source gives evidence for that guess. The evidence is read from the
//! text under a diagnostic's primary label, so it always describes what
//! the user is shown. With no source text available (the LSP, or any
//! caller that renders without the file database) no evidence is found
//! and evidence-gated hints are simply not shown.

use codespan_reporting::diagnostic::{Diagnostic, LabelStyle};
use codespan_reporting::files::{Files, SimpleFiles};

/// The source line under a diagnostic's primary label, with the label's
/// byte range within that line.
pub struct LabelSource<'a> {
    /// The whole line holding the start of the label.
    pub line: &'a str,
    /// Start of the label within `line`.
    pub start: usize,
    /// End of the label within `line` (clamped to the end of the line).
    pub end: usize,
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
        if !source.is_char_boundary(end) {
            return None;
        }
        Some(LabelSource {
            line: &source[line_start..line_end],
            start: start - line_start,
            end: end - line_start,
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

    /// Whether `word` appears on the line as a whole identifier (not as
    /// part of a longer name such as `order` or `band`).
    pub fn line_has_word(&self, word: &str) -> bool {
        has_word(self.line, word)
    }
}

/// Whether the left operand of an operator, given the text `before` it on
/// its line, is a catenation: two or more juxtaposed terms, as in `x f(y)`
/// of `x f(y) + 1`. The operand runs back to the nearest unmatched opening
/// bracket, or a `:` or `,` at its own nesting level.
pub fn left_operand_is_catenation(before: &str) -> bool {
    let mut depth = 0i32;
    let mut start = 0;
    for (i, c) in before.char_indices().rev() {
        match c {
            ')' | ']' | '}' => depth += 1,
            '(' | '[' | '{' if depth == 0 => {
                start = i + 1;
                break;
            }
            '(' | '[' | '{' => depth -= 1,
            ':' | ',' if depth == 0 => {
                start = i + 1;
                break;
            }
            _ => {}
        }
    }
    let operand = &before[start..];
    let mut terms = vec![];
    let mut depth = 0i32;
    let mut current = String::new();
    for c in operand.chars() {
        match c {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            _ => {}
        }
        if c.is_whitespace() && depth == 0 {
            if !current.is_empty() {
                terms.push(std::mem::take(&mut current));
            }
        } else {
            current.push(c);
        }
    }
    if !current.is_empty() {
        terms.push(current);
    }
    terms.len() >= 2 && !terms.iter().any(|t| is_operator(t))
}

/// The infix operator under or straight after the label whose left operand
/// is a catenation, if any: `+` in `x f(y) + z`, whether the label is the
/// `+` itself or the `f(y)` before it.
pub fn operator_after_catenation(source: &LabelSource) -> Option<String> {
    let label = source.text().trim();
    if is_operator(label) {
        if !binds_tighter_than_catenation(label) {
            return None;
        }
        return left_operand_is_catenation(source.before()).then(|| label.to_string());
    }
    let after = source.after().trim_start();
    let op: String = after.chars().take_while(|c| !c.is_whitespace()).collect();
    let operand = &source.line[..source.end];
    (is_operator(&op)
        && binds_tighter_than_catenation(&op)
        && after[op.len()..].starts_with(char::is_whitespace)
        && left_operand_is_catenation(operand))
    .then_some(op)
}

/// Whether an infix operator binds more tightly than catenation
/// (precedence 20). The prelude operators that do not — `=>` and `⇒`
/// (15), `@` (`:apply`, 10), and the `:meta` operators `//`, `//=`,
/// `//=>`, `=!`, `%!` and the rest of the `//` family (5) — would make
/// the catenation note false. A user-defined operator is assumed to bind
/// tightly.
fn binds_tighter_than_catenation(op: &str) -> bool {
    !(op.starts_with("//") || matches!(op, "=>" | "⇒" | "@" | "=!" | "%!"))
}

/// Whether `c` can occur in a eucalypt normal identifier.
fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || matches!(c, '_' | '-' | '?' | '!' | '$' | '\'' | '*')
}

/// Whether `word` occurs in `text` delimited by non-identifier characters.
pub fn has_word(text: &str, word: &str) -> bool {
    text.match_indices(word).any(|(i, _)| {
        let before_ok = text[..i]
            .chars()
            .next_back()
            .is_none_or(|c| !is_ident_char(c));
        let after_ok = text[i + word.len()..]
            .chars()
            .next()
            .is_none_or(|c| !is_ident_char(c));
        before_ok && after_ok
    })
}

/// Whether `text` is an infix operator made of symbol characters, such as
/// `+`, `=`, `&&` or `>=`.
pub fn is_operator(text: &str) -> bool {
    !text.is_empty()
        && text.chars().all(|c| {
            !c.is_alphanumeric()
                && !c.is_whitespace()
                && !matches!(c, '(' | ')' | '[' | ']' | '{' | '}' | '"' | ',' | ':' | '_')
        })
}

/// Whether `text` is a number or string literal.
pub fn is_literal(text: &str) -> bool {
    let t = text.trim();
    (t.len() >= 2 && t.starts_with('"') && t.ends_with('"'))
        || (!t.is_empty()
            && t.trim_start_matches('-')
                .chars()
                .all(|c| c.is_ascii_digit() || c == '.')
            && t.chars().any(|c| c.is_ascii_digit()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn words_are_delimited() {
        assert!(has_word("x and y", "and"));
        assert!(has_word("(a)or(b)", "or"));
        assert!(!has_word("sort-order(band)", "or"));
        assert!(!has_word("random", "and"));
        assert!(!has_word("or-else", "or"));
    }

    #[test]
    fn loose_operators_are_not_catenation_traps() {
        assert!(binds_tighter_than_catenation("+"));
        assert!(binds_tighter_than_catenation("="));
        assert!(!binds_tighter_than_catenation("=>"));
        assert!(!binds_tighter_than_catenation("//"));
        assert!(!binds_tighter_than_catenation("//=>"));
        assert!(!binds_tighter_than_catenation("@"));
    }

    #[test]
    fn operators_and_literals() {
        assert!(is_operator("+"));
        assert!(is_operator(">="));
        assert!(!is_operator("f"));
        assert!(!is_operator("(x)"));
        assert!(is_literal("5"));
        assert!(is_literal("-2.5"));
        assert!(is_literal("\"a\""));
        assert!(!is_literal("x"));
        assert!(!is_literal("-"));
    }
}
