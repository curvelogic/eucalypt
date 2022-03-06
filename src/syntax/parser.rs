lalrpop_mod!(pub grammar, "/syntax/grammar.rs");

use crate::syntax::ast::{Block, Expression};
use crate::syntax::error::ParserError;
use crate::syntax::lexer::Lexer;

use codespan_reporting::files::SimpleFiles;

use super::{ast::ArgTuple, error::SyntaxError, span::HasSpan};

/// Parse a unit into an AST Block
pub fn parse_unit<N, T>(files: &SimpleFiles<N, T>, id: usize) -> Result<Block, ParserError>
where
    N: AsRef<str>,
    N: Clone,
    N: std::fmt::Display,
    T: AsRef<str>,
{
    let lexer = Lexer::from_file_id(files, id);
    grammar::UnitParser::new()
        .parse(id, lexer)
        .map_err(|err| ParserError::from_lalrpop(id, err))
}

/// Parse an expression into an AST block
pub fn parse_expression<N, T>(
    files: &SimpleFiles<N, T>,
    id: usize,
) -> Result<Expression, ParserError>
where
    N: AsRef<str>,
    N: Clone,
    N: std::fmt::Display,
    T: AsRef<str>,
{
    let lexer = Lexer::from_file_id(files, id);
    grammar::ExpressionParser::new()
        .parse(id, lexer)
        .map_err(|err| ParserError::from_lalrpop(id, err))
}

/// Parse an expression into an AST block
pub fn parse_embedded_lambda<N, T>(
    files: &SimpleFiles<N, T>,
    id: usize,
) -> Result<(ArgTuple, Expression), ParserError>
where
    N: AsRef<str>,
    N: Clone,
    N: std::fmt::Display,
    T: AsRef<str>,
{
    let lexer = Lexer::from_file_id(files, id);
    let (head, body) = grammar::EmbeddedLambdaParser::new()
        .parse(id, lexer)
        .map_err(|err| ParserError::from_lalrpop(id, err))?;
    let head_span = head.span();
    let tuple = ArgTuple::from_apply_tuple(head).ok_or(ParserError::Syntax(
        SyntaxError::InvalidDeclarationHead(id, head_span),
    ))?;
    Ok((tuple, body))
}

#[cfg(test)]
pub mod tests {
    use std::str::Chars;

    use super::*;
    use crate::syntax::ast::*;
    use crate::syntax::lexer::Lexer;
    use codespan::{ByteIndex, Span};
    use codespan_reporting::files::SimpleFiles;
    use serde_json::Number;

    fn sp(s: u32, e: u32) -> Span {
        Span::new(ByteIndex(s), ByteIndex(e))
    }

    pub struct ParseTester {
        files: SimpleFiles<String, String>,
    }

    impl ParseTester {
        pub fn new() -> Self {
            ParseTester {
                files: SimpleFiles::new(),
            }
        }

        /// Create a lexer for a literal string
        pub fn lexer<'a>(
            self: &mut ParseTester,
            input: &'a str,
        ) -> Lexer<String, String, Chars<'_>> {
            let file_id = self.files.add("test".to_string(), input.to_string());
            Lexer::from_file_id(&self.files, file_id)
        }

        pub fn parse_literal(self: &mut ParseTester, txt: &'static str) -> Expression {
            let lexer = self.lexer(&txt);
            grammar::LiteralParser::new()
                .parse(lexer.file_id(), lexer)
                .unwrap()
        }

        pub fn parse_id(self: &mut ParseTester, txt: &'static str) -> Name {
            let lexer = self.lexer(&txt);
            grammar::NameParser::new()
                .parse(lexer.file_id(), lexer)
                .unwrap()
        }

        pub fn parse_expr(self: &mut ParseTester, txt: &'static str) -> Expression {
            let lexer = self.lexer(&txt);
            grammar::ExpressionParser::new()
                .parse(lexer.file_id(), lexer)
                .unwrap()
        }

        pub fn accepts_expr(self: &mut ParseTester, txt: &'static str) -> bool {
            let lexer = self.lexer(&txt);
            grammar::ExpressionParser::new()
                .parse(lexer.file_id(), lexer)
                .is_ok()
        }

        pub fn rejects_expr(self: &mut ParseTester, txt: &'static str) -> bool {
            let lexer = self.lexer(&txt);
            grammar::ExpressionParser::new()
                .parse(lexer.file_id(), lexer)
                .is_err()
        }

        pub fn parse_block(self: &mut ParseTester, txt: &'static str) -> Block {
            let lexer = self.lexer(&txt);
            grammar::BlockParser::new()
                .parse(lexer.file_id(), lexer)
                .unwrap()
        }

        pub fn accepts_block(self: &mut ParseTester, txt: &'static str) -> bool {
            let lexer = self.lexer(&txt);
            grammar::BlockParser::new()
                .parse(lexer.file_id(), lexer)
                .is_ok()
        }

        pub fn rejects_block(self: &mut ParseTester, txt: &'static str) -> bool {
            let lexer = self.lexer(&txt);
            grammar::BlockParser::new()
                .parse(lexer.file_id(), lexer)
                .is_err()
        }

        pub fn parse_unit(self: &mut ParseTester, txt: &'static str) -> Block {
            let lexer = self.lexer(&txt);
            grammar::UnitParser::new()
                .parse(lexer.file_id(), lexer)
                .unwrap()
        }

        pub fn accepts_unit(self: &mut ParseTester, txt: &'static str) -> bool {
            let lexer = self.lexer(&txt);
            grammar::UnitParser::new()
                .parse(lexer.file_id(), lexer)
                .is_ok()
        }

        pub fn rejects_unit(self: &mut ParseTester, txt: &'static str) -> bool {
            let lexer = self.lexer(&txt);
            grammar::UnitParser::new()
                .parse(lexer.file_id(), lexer)
                .is_err()
        }
    }

    #[test]
    fn test_literals() {
        let mut p = ParseTester::new();
        assert_eq!(p.parse_literal(":foo"), lit(sym_at(sp(0, 4), "foo")));
        assert_eq!(p.parse_literal(":'bar'"), lit(sym_at(sp(0, 6), "bar")));
        assert_eq!(p.parse_literal(":'~*&^%'"), lit(sym_at(sp(0, 8), "~*&^%")));
        assert_eq!(p.parse_literal("\"foo\""), lit(str_at(sp(0, 5), "foo")));
        assert_eq!(
            p.parse_literal("\"{0}\""),
            pattern_at(
                sp(0, 5),
                vec![simple_interpolation_at(
                    sp(1, 4),
                    str_anaphor_at(sp(2, 3), Some(0))
                )]
            )
        );
        assert_eq!(
            p.parse_literal("\"{{}}{}\""),
            pattern_at(
                sp(0, 8),
                vec![
                    lit_content_at(sp(1, 5), "{}"),
                    simple_interpolation_at(sp(5, 7), str_anaphor_at(sp(6, 6), None))
                ]
            )
        );
        assert_eq!(p.parse_literal("\"إ\""), lit(str_at(sp(0, 4), "إ")));
        assert_eq!(
            p.parse_literal("-1234.1234"),
            lit(num_at(sp(0, 10), Number::from_f64(-1234.1234).unwrap()))
        );
        assert_eq!(
            p.parse_literal("-1234"),
            lit(num_at(sp(0, 5), Number::from(-1234)))
        );
        assert_eq!(
            p.parse_literal("999"),
            lit(num_at(sp(0, 3), Number::from(999)))
        );
    }

    #[test]
    fn test_ids() {
        let mut p = ParseTester::new();
        assert_eq!(p.parse_id("xyz"), normal_at(sp(0, 3), "xyz"));
        assert_eq!(p.parse_id("?xyz"), normal_at(sp(0, 4), "?xyz"));
        assert_eq!(p.parse_id("a-b-c?"), normal_at(sp(0, 6), "a-b-c?"));
        assert_eq!(p.parse_id("•1"), normal_at(sp(0, 4), "•1"));
        assert_eq!(p.parse_id("__BIF"), normal_at(sp(0, 5), "__BIF"));
        assert_eq!(p.parse_id("_1"), normal_at(sp(0, 2), "_1"));
        assert_eq!(p.parse_id("∨"), operator_at(sp(0, 3), "∨"));
        assert_eq!(p.parse_id("∘"), operator_at(sp(0, 3), "∘"));
        assert_eq!(p.parse_id("&&"), operator_at(sp(0, 2), "&&"));
        assert_eq!(p.parse_id("-"), operator_at(sp(0, 1), "-"));
        assert_eq!(p.parse_id("'asdf'"), normal_at(sp(0, 6), "asdf"));
        assert_eq!(
            p.parse_id("'::||\t||::'"),
            normal_at(sp(0, 11), "::||\t||::")
        );
    }

    #[test]
    fn test_expressions() {
        let mut p = ParseTester::new();
        assert_eq!(p.parse_expr("x"), name(normal_at(sp(0, 1), "x")));

        assert_eq!(
            p.parse_expr("3.3"),
            lit(num_at(sp(0, 3), Number::from_f64(3.3).unwrap()))
        );

        assert_eq!(
            p.parse_expr("(2 + 3)"),
            soup_at(
                sp(0, 7),
                vec![
                    lit(num_at(sp(1, 2), 2)),
                    name(operator_at(sp(3, 4), "+")),
                    lit(num_at(sp(5, 6), 3))
                ]
            )
        );

        assert_eq!(
            p.parse_expr("(2+3)"),
            soup_at(
                sp(0, 5),
                vec![
                    lit(num_at(sp(1, 2), 2)),
                    name(operator_at(sp(2, 3), "+")),
                    lit(num_at(sp(3, 4), 3))
                ]
            )
        );

        assert_eq!(
            p.parse_expr("[x, y, z]"),
            list_at(
                sp(0, 9),
                vec![
                    name(normal_at(sp(1, 2), "x")),
                    name(normal_at(sp(4, 5), "y")),
                    name(normal_at(sp(7, 8), "z"))
                ]
            )
        );

        assert_eq!(
            p.parse_expr("f(x)"),
            soup_at(
                sp(0, 4),
                vec![
                    name(normal_at(sp(0, 1), "f")),
                    tuple_at(sp(1, 4), vec![name(normal_at(sp(2, 3), "x"))]),
                ]
            )
        );

        assert!(p.accepts_expr("x f g h"));
        assert!(p.accepts_expr(" x f g h "));
        assert!(p.accepts_expr("#\n x #\n f #\n g #\n h "));
        assert!(p.accepts_expr("[:x,]"));
        assert!(p.accepts_expr("[:x, ]"));

        assert!(p.rejects_expr(""));
        assert!(p.rejects_expr("`"));
    }

    #[test]
    fn test_blocks() {
        let mut p = ParseTester::new();
        assert_eq!(
            p.parse_block("{x: 3}"),
            block_at(
                sp(0, 6),
                None,
                vec![prop_at(
                    sp(1, 5),
                    None,
                    normal_at(sp(1, 2), "x"),
                    lit(num_at(sp(4, 5), Number::from(3)))
                )]
            )
        );

        assert_eq!(
            p.parse_block("{f(x, y): x + 3}"),
            block_at(
                sp(0, 16),
                None,
                vec![fn_at(
                    sp(1, 15),
                    None,
                    normal_at(sp(1, 2), "f"),
                    args_at(
                        sp(2, 8),
                        vec![normal_at(sp(3, 4), "x"), normal_at(sp(6, 7), "y")]
                    ),
                    soup_at(
                        sp(10, 15),
                        vec![
                            name(normal_at(sp(10, 11), "x")),
                            name(operator_at(sp(12, 13), "+")),
                            lit(num_at(sp(14, 15), Number::from(3)))
                        ]
                    )
                )]
            )
        );

        assert_eq!(
            p.parse_block("{ (x &&& y): y }"),
            block_at(
                sp(0, 16),
                None,
                vec![infix_at(
                    sp(2, 14),
                    None,
                    normal_at(sp(3, 4), "x"),
                    operator_at(sp(5, 8), "&&&"),
                    normal_at(sp(9, 10), "y"),
                    name(normal_at(sp(13, 14), "y"))
                )]
            )
        );

        assert_eq!(
            p.parse_block("{(! y): y}"),
            block_at(
                sp(0, 10),
                None,
                vec![prefix_at(
                    sp(1, 9),
                    None,
                    operator_at(sp(2, 3), "!"),
                    normal_at(sp(4, 5), "y"),
                    name(normal_at(sp(8, 9), "y"))
                )]
            )
        );

        assert_eq!(
            p.parse_block("{ ( y * ): y }"),
            block_at(
                sp(0, 14),
                None,
                vec![postfix_at(
                    sp(2, 12),
                    None,
                    normal_at(sp(4, 5), "y"),
                    operator_at(sp(6, 7), "*"),
                    name(normal_at(sp(11, 12), "y"))
                )]
            )
        );

        assert_eq!(
            p.parse_block("{x: 3 y: 4}"),
            block_at(
                sp(0, 11),
                None,
                vec![
                    prop_at(
                        sp(1, 5),
                        None,
                        normal_at(sp(1, 2), "x"),
                        lit(num_at(sp(4, 5), Number::from(3)))
                    ),
                    prop_at(
                        sp(6, 10),
                        None,
                        normal_at(sp(6, 7), "y"),
                        lit(num_at(sp(9, 10), Number::from(4)))
                    )
                ]
            )
        );

        assert!(p.accepts_block("{}"));
        assert!(p.accepts_block("{ x: 3 f }"));
        assert!(p.accepts_block("{ x: 3 f y(x): x * 3}"));
        assert!(p.accepts_block("{ \"meta\" x: 3 f g h (x && y): x * 3}"));
        assert!(p.accepts_block("{ x: 3, y: 4 f, }"));
        assert!(p.accepts_block("{ (! x): 3 + x (y %): y}"));
        assert!(p.accepts_block("{ x }"));
        assert!(p.accepts_block("{ (    ב    ∗  Ъ  )}"));
        assert!(p.accepts_block("{ m n o p q r x: y z: f g h i}"));
        assert!(p.accepts_block("{ ` m3 z: 877 f}"));
        assert!(p.accepts_block("{ MMMM ` m x: y `m3 z: 877 f}"));
        assert!(p.accepts_block("{ M M M M M ` m m m m m (x £ y): y1 y2 y3 y4 y5 }"));
        assert!(p.accepts_block("{ α: [:x] = [:x, ] }"));
        assert!(p.accepts_block("{ α: [:x, :y, :z] = [:x, :y, :z, ] }"));

        // various error cases
        assert!(p.rejects_block("{ x: }"));
        assert!(p.rejects_block("{ x: 3, 3 }"));
        assert!(p.rejects_block("{ x: 3: 3 }"));
        assert!(p.rejects_block("{ x: 3"));
        assert!(p.rejects_block("x: 3}"));
        assert!(p.rejects_block("{ x: a b c, d e f, y: 3}"));
        assert!(p.rejects_block("{ (x & &): 3}"));
        assert!(p.rejects_block("{ (x & y && z): 3}"));
        assert!(p.rejects_block("{ (x & y z): 3}"));
        assert!(p.rejects_block("{ ((x) & (y)): 3}"));
        assert!(p.rejects_block("{ x: 3 ` }"));
        assert!(p.rejects_block("{ x: ` 4 }"));
        assert!(p.rejects_block("{ ` ` x: 4 }"));

        // nested blocks
        assert!(p.accepts_block("{ x: { y: value } }"));
        assert!(p.accepts_block("{ { m: v} x: { y: value } }"));
        assert!(p.accepts_block("{ ` { m: v } x: { y: value } }"));
        assert!(p.accepts_block("{ { m: v } ` { m: v } x: { y: value } }"));
        assert!(p.accepts_block("{ { m: v } ` { ` :m m: v } x: { ` \"m\" y: value } }"));
        assert!(p.accepts_block("{ { m: v } ` { ` :m m: v } x: { ` \"m\" y: value } }"));
        assert!(p.accepts_block("{ x: { y: { z: { f(x): x }} } }"));
        assert!(p.rejects_block("{ { k: v } : v  } }"));

        // comments
        assert!(p.accepts_block("{f( #\n x): x}"));
        assert!(p.accepts_block("{f(x #\n): x}"));
        assert!(p.accepts_block("{f(x): x #\n}"));
        assert!(p.accepts_block("{ #\n f( #\n x #\n )#\n   : x}"));
    }

    #[test]
    fn test_units() {
        let mut p = ParseTester::new();
        assert!(p.accepts_unit(
            r#"#!/usr/bin/env eu

{ doc: "unit meta" }

x: 123

` "docs"
f(y): y + 234

ns: {
  a: b d c e # comment
}

"#
        ));

        assert_eq!(
            p.parse_unit("x: y"),
            block_at(
                sp(0, 4),
                None,
                vec![prop_at(
                    sp(0, 4),
                    None,
                    normal_at(sp(0, 1), "x"),
                    name(normal_at(sp(3, 4), "y"))
                )]
            )
        );
        assert!(p.rejects_unit("9 ``"));
    }

    #[test]
    fn test_parse_embedded_lambda() {
        let lam = "(x, y) x * y";
        let mut files = SimpleFiles::new();
        let file_id = files.add("test".to_string(), lam.to_string());
        let result = parse_embedded_lambda(&mut files, file_id);
        dbg!(&result);
        assert!(result.is_ok());
    }
}
