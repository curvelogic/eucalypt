lalrpop_mod!(pub string_pattern, "/syntax/string_pattern.rs");

use crate::syntax::ast::*;
use crate::syntax::error::SyntaxError;
use crate::syntax::string_lexer::StringLexer;
use codespan::{ByteIndex, ByteOffset, Span};

/// Parse string pattern during an AST parse
pub fn parse_string_literal(
    file_id: usize,
    content: &str,
    left: ByteIndex,
    right: ByteIndex,
) -> Result<Expression, SyntaxError> {
    // left is the quote of the string literal:
    let lexer = StringLexer::new(content, ByteOffset(left.to_usize() as i64 + 1));
    let mut chunks = string_pattern::StringPatternParser::new()
        .parse(lexer)
        .map_err(|e| SyntaxError::from_string_pattern_error(file_id, e))?;

    // join literals if necessary
    chunks = compress(chunks);

    let span = Span::new(left, right);

    if chunks.is_empty() {
        Ok(lit(str_at(span, "")))
    } else if chunks.len() == 1 && chunks[0].is_literal() {
        Ok(lit(str_at(span, chunks[0].literal_content())))
    } else {
        Ok(pattern_at(span, chunks))
    }
}

pub fn compress(mut chunks: Vec<StringChunk>) -> Vec<StringChunk> {
    let mut v = vec![];
    for ch in chunks.drain(..) {
        match ch {
            StringChunk::Interpolation(_, _) => v.push(ch),
            StringChunk::LiteralContent(ref span_b, ref str_b) => match v.pop() {
                Some(last) => {
                    if let StringChunk::LiteralContent(span_a, str_a) = last {
                        let mut joined = str_a.to_string();
                        joined.push_str(&str_b);
                        v.push(StringChunk::LiteralContent(span_a.merge(*span_b), joined));
                    } else {
                        v.push(last);
                        v.push(ch);
                    }
                }
                None => {
                    v.push(ch);
                }
            },
        }
    }
    v
}

#[cfg(test)]
pub mod tests {
    use super::*;

    fn parse(content: &str) -> Vec<StringChunk> {
        let lexer = StringLexer::new(content, ByteOffset(0));
        string_pattern::StringPatternParser::new()
            .parse(lexer)
            .unwrap()
    }

    #[test]
    pub fn test_single_anaphor() {
        assert_eq!(
            parse("{0}"),
            vec![simple_interpolation_at(
                Span::new(ByteIndex(0), ByteIndex(3)),
                str_anaphor_at(Span::new(1, 2), Some(0))
            )]
        );
    }
}
