//! Hand-written recursive descent parser for type annotation strings.
//!
//! Parses the type grammar defined in the gradual typing spec (section 4).
//! This is entirely separate from the eucalypt expression parser — type
//! annotations live in metadata strings and are parsed on demand.
//!
//! Grammar (summary):
//! ```text
//! type       ::= union
//! union      ::= arrow ( '|' arrow )*
//! arrow      ::= primary ( '->' primary )*         # right-associative
//! primary    ::= 'number' | 'string' | 'symbol' | 'bool' | 'null'
//!              | 'datetime' | 'any' | 'top' | 'never'
//!              | 'set' | 'vec' | 'array'
//!              | LOWER_IDENT                        # type variable
//!              | '[' type ']'                       # list
//!              | 'IO' '(' type ')'
//!              | 'Lens' '(' type ',' type ')'
//!              | 'Traversal' '(' type ',' type ')'
//!              | '{' row '}'                        # record
//!              | '(' paren_body ')'                 # grouping / tuple
//! paren_body ::= type
//!              | type ','
//!              | type ( ',' type )+ ','?
//! row        ::= field ( ',' field )* ( ',' '..' )?
//! field      ::= IDENT ':' type
//! ```

use std::collections::BTreeMap;
use std::fmt;

use super::types::{Type, TypeVarId};

// ── Error type ──────────────────────────────────────────────────────────────

/// A parse error with a byte-offset position into the original annotation string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    /// Byte offset in the source string where the error was detected.
    pub position: usize,
    pub message: String,
}

impl ParseError {
    fn new(position: usize, message: impl Into<String>) -> Self {
        ParseError {
            position,
            message: message.into(),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "type annotation parse error at position {}: {}",
            self.position, self.message
        )
    }
}

impl std::error::Error for ParseError {}

// ── Lexer ───────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    // Keywords / primitives
    Number,
    String,
    Symbol,
    Bool,
    Null,
    Datetime,
    Any,
    Top,
    Never,
    Set,
    Vec,
    Array,
    // Type constructors
    Io,
    Lens,
    Traversal,
    // Identifiers (type variables and record field names)
    Ident(String),
    // Punctuation
    LBracket, // [
    RBracket, // ]
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    Arrow,    // ->
    Pipe,     // |
    Colon,    // :
    Comma,    // ,
    DotDot,   // ..
    // End of input
    Eof,
}

struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer { input, pos: 0 }
    }

    fn remaining(&self) -> &str {
        &self.input[self.pos..]
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len()
            && self.input[self.pos..].starts_with(|c: char| c.is_ascii_whitespace())
        {
            self.pos += 1;
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    /// Lex the next token, returning it together with the position at which it started.
    fn next_token(&mut self) -> Result<(Token, usize), ParseError> {
        self.skip_whitespace();
        let start = self.pos;

        if self.pos >= self.input.len() {
            return Ok((Token::Eof, start));
        }

        let ch = self.peek_char().unwrap();

        match ch {
            '[' => {
                self.pos += 1;
                Ok((Token::LBracket, start))
            }
            ']' => {
                self.pos += 1;
                Ok((Token::RBracket, start))
            }
            '(' => {
                self.pos += 1;
                Ok((Token::LParen, start))
            }
            ')' => {
                self.pos += 1;
                Ok((Token::RParen, start))
            }
            '{' => {
                self.pos += 1;
                Ok((Token::LBrace, start))
            }
            '}' => {
                self.pos += 1;
                Ok((Token::RBrace, start))
            }
            '|' => {
                self.pos += 1;
                Ok((Token::Pipe, start))
            }
            ':' => {
                self.pos += 1;
                Ok((Token::Colon, start))
            }
            ',' => {
                self.pos += 1;
                Ok((Token::Comma, start))
            }
            '.' => {
                if self.remaining().starts_with("..") {
                    self.pos += 2;
                    Ok((Token::DotDot, start))
                } else {
                    Err(ParseError::new(
                        start,
                        "unexpected character '.' (did you mean '..'?)".to_string(),
                    ))
                }
            }
            '-' => {
                if self.remaining().starts_with("->") {
                    self.pos += 2;
                    Ok((Token::Arrow, start))
                } else {
                    Err(ParseError::new(
                        start,
                        "unexpected character '-' (did you mean '->'?)".to_string(),
                    ))
                }
            }
            c if c.is_ascii_alphabetic() => {
                // Consume identifier
                let ident_start = self.pos;
                while self.pos < self.input.len() {
                    let next = self.input[self.pos..].chars().next().unwrap();
                    if next.is_ascii_alphanumeric() || next == '_' || next == '-' {
                        self.pos += next.len_utf8();
                    } else {
                        break;
                    }
                }
                let ident = &self.input[ident_start..self.pos];
                let tok = match ident {
                    "number" => Token::Number,
                    "string" => Token::String,
                    "symbol" => Token::Symbol,
                    "bool" => Token::Bool,
                    "null" => Token::Null,
                    "datetime" => Token::Datetime,
                    "any" => Token::Any,
                    "top" => Token::Top,
                    "never" => Token::Never,
                    "set" => Token::Set,
                    "vec" => Token::Vec,
                    "array" => Token::Array,
                    "IO" => Token::Io,
                    "Lens" => Token::Lens,
                    "Traversal" => Token::Traversal,
                    other => Token::Ident(other.to_string()),
                };
                Ok((tok, start))
            }
            other => Err(ParseError::new(
                start,
                format!("unexpected character '{other}'"),
            )),
        }
    }
}

// ── Parser ──────────────────────────────────────────────────────────────────

struct Parser<'a> {
    lexer: Lexer<'a>,
    /// One-token lookahead buffer: `(token, position)`.
    peeked: Option<(Token, usize)>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Parser {
            lexer: Lexer::new(input),
            peeked: None,
        }
    }

    /// Current byte position in the source string.
    fn pos(&mut self) -> usize {
        if let Some((_, p)) = &self.peeked {
            *p
        } else {
            self.lexer.pos
        }
    }

    fn peek(&mut self) -> Result<&Token, ParseError> {
        if self.peeked.is_none() {
            let tok = self.lexer.next_token()?;
            self.peeked = Some(tok);
        }
        Ok(&self.peeked.as_ref().unwrap().0)
    }

    fn advance(&mut self) -> Result<(Token, usize), ParseError> {
        if let Some(t) = self.peeked.take() {
            Ok(t)
        } else {
            self.lexer.next_token()
        }
    }

    fn expect(&mut self, expected: &Token) -> Result<usize, ParseError> {
        let (tok, tok_pos) = self.advance()?;
        if &tok == expected {
            Ok(tok_pos)
        } else {
            Err(ParseError::new(
                tok_pos,
                format!("expected {expected:?}, got {tok:?}"),
            ))
        }
    }

    // ── Grammar productions ─────────────────────────────────────────────────

    /// Parse a complete type expression and verify no trailing input remains.
    fn parse_type_toplevel(&mut self) -> Result<Type, ParseError> {
        let ty = self.parse_union()?;
        let (tok, pos) = self.advance()?;
        if tok != Token::Eof {
            return Err(ParseError::new(
                pos,
                format!("unexpected trailing input: {tok:?}"),
            ));
        }
        Ok(ty)
    }

    /// `union ::= arrow ( '|' arrow )*`
    fn parse_union(&mut self) -> Result<Type, ParseError> {
        let mut variants = vec![self.parse_arrow()?];
        loop {
            if self.peek()? == &Token::Pipe {
                self.advance()?;
                variants.push(self.parse_arrow()?);
            } else {
                break;
            }
        }
        if variants.len() == 1 {
            Ok(variants.remove(0))
        } else {
            Ok(Type::Union(variants))
        }
    }

    /// `arrow ::= primary ( '->' primary )*`  (right-associative)
    fn parse_arrow(&mut self) -> Result<Type, ParseError> {
        let lhs = self.parse_primary()?;
        if self.peek()? == &Token::Arrow {
            self.advance()?;
            // Right-associative: recurse via parse_arrow
            let rhs = self.parse_arrow()?;
            Ok(Type::Function(Box::new(lhs), Box::new(rhs)))
        } else {
            Ok(lhs)
        }
    }

    /// Parse a primary type expression.
    fn parse_primary(&mut self) -> Result<Type, ParseError> {
        let (tok, tok_pos) = self.advance()?;
        match tok {
            Token::Number => Ok(Type::Number),
            Token::String => Ok(Type::String),
            Token::Symbol => Ok(Type::Symbol),
            Token::Bool => Ok(Type::Bool),
            Token::Null => Ok(Type::Null),
            Token::Datetime => Ok(Type::DateTime),
            Token::Any => Ok(Type::Any),
            Token::Top => Ok(Type::Top),
            Token::Never => Ok(Type::Never),
            Token::Set => Ok(Type::Set),
            Token::Vec => Ok(Type::Vec),
            Token::Array => Ok(Type::Array),
            Token::Ident(name) => {
                // Type variable (lowercase) or type alias reference (uppercase).
                //
                // Both are represented as `Type::Var` at parse time.  Lowercase
                // idents are universally-quantified type variables; uppercase
                // idents are alias references resolved by the checker against
                // its alias map.  The checker erases any unresolved `Var`s to
                // `any` via `erase_type_vars`.
                if name.starts_with(|c: char| c.is_ascii_alphabetic()) {
                    Ok(Type::Var(TypeVarId(name)))
                } else {
                    Err(ParseError::new(
                        tok_pos,
                        format!("unknown type '{name}' (type names must start with a letter)"),
                    ))
                }
            }
            Token::LBracket => {
                // `[` type `]`
                let inner = self.parse_union()?;
                self.expect(&Token::RBracket)?;
                Ok(Type::List(Box::new(inner)))
            }
            Token::LParen => self.parse_paren_body(tok_pos),
            Token::LBrace => self.parse_record(tok_pos),
            Token::Io => {
                // `IO` `(` type `)`
                self.expect(&Token::LParen)?;
                let inner = self.parse_union()?;
                self.expect(&Token::RParen)?;
                Ok(Type::IO(Box::new(inner)))
            }
            Token::Lens => {
                // `Lens` `(` type `,` type `)`
                self.expect(&Token::LParen)?;
                let a = self.parse_union()?;
                self.expect(&Token::Comma)?;
                let b = self.parse_union()?;
                self.expect(&Token::RParen)?;
                Ok(Type::Lens(Box::new(a), Box::new(b)))
            }
            Token::Traversal => {
                // `Traversal` `(` type `,` type `)`
                self.expect(&Token::LParen)?;
                let a = self.parse_union()?;
                self.expect(&Token::Comma)?;
                let b = self.parse_union()?;
                self.expect(&Token::RParen)?;
                Ok(Type::Traversal(Box::new(a), Box::new(b)))
            }
            other => Err(ParseError::new(
                tok_pos,
                format!("expected a type, got {other:?}"),
            )),
        }
    }

    /// Parse the interior of `(...)` — grouping or tuple.
    ///
    /// Called after `(` has been consumed.
    ///
    /// ```text
    /// paren_body ::= type               -- grouping: (A -> B)
    ///              | type ','           -- 1-tuple: (A,)
    ///              | type (',' type)+ ','?  -- n-tuple: (A, B) or (A, B,)
    /// ```
    fn parse_paren_body(&mut self, _open_pos: usize) -> Result<Type, ParseError> {
        // Empty parens `()` are not valid in the grammar.
        if self.peek()? == &Token::RParen {
            let (_, pos) = self.advance()?;
            return Err(ParseError::new(
                pos,
                "empty parentheses '()' are not a valid type (use 'null' for the unit type)",
            ));
        }

        let first = self.parse_union()?;

        match self.peek()? {
            Token::RParen => {
                // Grouping: `(T)`
                self.advance()?;
                Ok(first)
            }
            Token::Comma => {
                // Could be 1-tuple `(A,)` or multi-tuple `(A, B, ...)`
                self.advance()?; // consume ','
                let mut elems = vec![first];

                loop {
                    match self.peek()? {
                        Token::RParen => {
                            // Trailing comma — close the tuple
                            self.advance()?;
                            break;
                        }
                        _ => {
                            elems.push(self.parse_union()?);
                            match self.peek()? {
                                Token::Comma => {
                                    self.advance()?;
                                }
                                Token::RParen => {
                                    self.advance()?;
                                    break;
                                }
                                _ => {
                                    let pos = self.pos();
                                    return Err(ParseError::new(
                                        pos,
                                        "expected ',' or ')' in tuple type",
                                    ));
                                }
                            }
                        }
                    }
                }

                Ok(Type::Tuple(elems))
            }
            _ => {
                let pos = self.pos();
                Err(ParseError::new(
                    pos,
                    "expected ')' or ',' after type in parentheses",
                ))
            }
        }
    }

    /// Parse the interior of `{...}` — a record type.
    ///
    /// Called after `{` has been consumed.
    ///
    /// ```text
    /// row   ::= field (',' field)* (',' '..')?
    ///         | '..'
    /// field ::= IDENT ':' type
    /// ```
    fn parse_record(&mut self, _open_pos: usize) -> Result<Type, ParseError> {
        // Empty open record: `{..}`
        if self.peek()? == &Token::DotDot {
            self.advance()?;
            self.expect(&Token::RBrace)?;
            return Ok(Type::Record {
                fields: BTreeMap::new(),
                open: true,
            });
        }

        // Empty closed record: `{}`
        if self.peek()? == &Token::RBrace {
            self.advance()?;
            return Ok(Type::Record {
                fields: BTreeMap::new(),
                open: false,
            });
        }

        let mut fields = BTreeMap::new();
        let mut open = false;

        loop {
            // Check for `..` (open marker) or a field name
            let pos = self.pos();
            match self.peek()? {
                Token::DotDot => {
                    self.advance()?;
                    open = true;
                    self.expect(&Token::RBrace)?;
                    break;
                }
                Token::RBrace => {
                    self.advance()?;
                    break;
                }
                Token::Ident(_) => {
                    let (tok, field_pos) = self.advance()?;
                    let field_name = match tok {
                        Token::Ident(name) => name,
                        _ => unreachable!(),
                    };
                    self.expect(&Token::Colon)?;
                    let field_type = self.parse_union()?;
                    if fields.contains_key(&field_name) {
                        return Err(ParseError::new(
                            field_pos,
                            format!("duplicate field '{field_name}' in record type"),
                        ));
                    }
                    fields.insert(field_name, field_type);

                    // After a field: expect ',' or '}'
                    match self.peek()? {
                        Token::Comma => {
                            self.advance()?;
                            // After comma: could be another field, `..`, or `}`
                        }
                        Token::RBrace => {
                            self.advance()?;
                            break;
                        }
                        _ => {
                            let p = self.pos();
                            return Err(ParseError::new(p, "expected ',' or '}' in record type"));
                        }
                    }
                }
                _ => {
                    return Err(ParseError::new(
                        pos,
                        "expected a field name or '..' in record type",
                    ));
                }
            }
        }

        Ok(Type::Record { fields, open })
    }
}

// ── Public API ───────────────────────────────────────────────────────────────

/// Parse a type annotation string into a `Type`.
///
/// Returns a `ParseError` with byte-offset position information if the
/// annotation is malformed.
///
/// # Examples
///
/// ```
/// use eucalypt::core::typecheck::parse::parse_type;
/// use eucalypt::core::typecheck::types::Type;
///
/// assert_eq!(parse_type("number").unwrap(), Type::Number);
/// assert_eq!(
///     parse_type("[string]").unwrap(),
///     Type::List(Box::new(Type::String))
/// );
/// ```
pub fn parse_type(input: &str) -> Result<Type, ParseError> {
    let mut parser = Parser::new(input);
    parser.parse_type_toplevel()
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::typecheck::types::{Type, TypeVarId};
    use std::collections::BTreeMap;

    fn var(name: &str) -> Type {
        Type::Var(TypeVarId(name.to_string()))
    }

    // ── Primitives ──────────────────────────────────────────────────────────

    #[test]
    fn parse_primitives() {
        assert_eq!(parse_type("number").unwrap(), Type::Number);
        assert_eq!(parse_type("string").unwrap(), Type::String);
        assert_eq!(parse_type("symbol").unwrap(), Type::Symbol);
        assert_eq!(parse_type("bool").unwrap(), Type::Bool);
        assert_eq!(parse_type("null").unwrap(), Type::Null);
        assert_eq!(parse_type("datetime").unwrap(), Type::DateTime);
        assert_eq!(parse_type("any").unwrap(), Type::Any);
        assert_eq!(parse_type("top").unwrap(), Type::Top);
        assert_eq!(parse_type("never").unwrap(), Type::Never);
        assert_eq!(parse_type("set").unwrap(), Type::Set);
        assert_eq!(parse_type("vec").unwrap(), Type::Vec);
        assert_eq!(parse_type("array").unwrap(), Type::Array);
    }

    // ── Type variables ──────────────────────────────────────────────────────

    #[test]
    fn parse_type_var() {
        assert_eq!(parse_type("a").unwrap(), var("a"));
        assert_eq!(parse_type("result").unwrap(), var("result"));
    }

    #[test]
    fn parse_uppercase_ident_returns_var() {
        // Uppercase idents are alias references — parsed as Type::Var,
        // resolved to concrete types by the checker's alias map.
        assert_eq!(parse_type("Person").unwrap(), var("Person"));
        assert_eq!(parse_type("Point").unwrap(), var("Point"));
    }

    #[test]
    fn parse_non_alpha_ident_fails() {
        // Underscore-prefixed names start with a non-alpha char — the lexer
        // rejects them before the parser even sees an ident token.
        let err = parse_type("_foo").unwrap_err();
        assert!(err.message.contains("unexpected character"));
    }

    // ── List ────────────────────────────────────────────────────────────────

    #[test]
    fn parse_list() {
        assert_eq!(
            parse_type("[number]").unwrap(),
            Type::List(Box::new(Type::Number))
        );
    }

    #[test]
    fn parse_list_of_var() {
        assert_eq!(parse_type("[a]").unwrap(), Type::List(Box::new(var("a"))));
    }

    // ── Tuple ───────────────────────────────────────────────────────────────

    #[test]
    fn parse_1_tuple() {
        assert_eq!(
            parse_type("(string,)").unwrap(),
            Type::Tuple(vec![Type::String])
        );
    }

    #[test]
    fn parse_2_tuple() {
        assert_eq!(
            parse_type("(symbol, any)").unwrap(),
            Type::Tuple(vec![Type::Symbol, Type::Any])
        );
    }

    #[test]
    fn parse_3_tuple_trailing_comma() {
        assert_eq!(
            parse_type("(number, string, bool,)").unwrap(),
            Type::Tuple(vec![Type::Number, Type::String, Type::Bool])
        );
    }

    #[test]
    fn parse_grouping() {
        // `(A -> B)` is grouping, not a tuple
        assert_eq!(
            parse_type("(number -> string)").unwrap(),
            Type::Function(Box::new(Type::Number), Box::new(Type::String))
        );
    }

    #[test]
    fn parse_empty_parens_fails() {
        assert!(parse_type("()").is_err());
    }

    // ── Function (arrow) ────────────────────────────────────────────────────

    #[test]
    fn parse_simple_function() {
        assert_eq!(
            parse_type("number -> string").unwrap(),
            Type::Function(Box::new(Type::Number), Box::new(Type::String))
        );
    }

    #[test]
    fn parse_curried_function() {
        // Right-associative: a -> b -> c  ≡  a -> (b -> c)
        assert_eq!(
            parse_type("number -> number -> number").unwrap(),
            Type::Function(
                Box::new(Type::Number),
                Box::new(Type::Function(
                    Box::new(Type::Number),
                    Box::new(Type::Number)
                ))
            )
        );
    }

    #[test]
    fn parse_higher_order_function() {
        // (a -> b) -> [a] -> [b]
        assert_eq!(
            parse_type("(a -> b) -> [a] -> [b]").unwrap(),
            Type::Function(
                Box::new(Type::Function(Box::new(var("a")), Box::new(var("b")))),
                Box::new(Type::Function(
                    Box::new(Type::List(Box::new(var("a")))),
                    Box::new(Type::List(Box::new(var("b"))))
                ))
            )
        );
    }

    // ── Union ───────────────────────────────────────────────────────────────

    #[test]
    fn parse_union() {
        assert_eq!(
            parse_type("number | string").unwrap(),
            Type::Union(vec![Type::Number, Type::String])
        );
    }

    #[test]
    fn parse_union_three_variants() {
        assert_eq!(
            parse_type("number | string | bool").unwrap(),
            Type::Union(vec![Type::Number, Type::String, Type::Bool])
        );
    }

    #[test]
    fn parse_union_of_functions() {
        // number -> number | string -> string
        // = (number -> number) | (string -> string) since arrow binds tighter than pipe
        assert_eq!(
            parse_type("number -> number | string -> string").unwrap(),
            Type::Union(vec![
                Type::Function(Box::new(Type::Number), Box::new(Type::Number)),
                Type::Function(Box::new(Type::String), Box::new(Type::String)),
            ])
        );
    }

    // ── Record ──────────────────────────────────────────────────────────────

    #[test]
    fn parse_closed_record() {
        let mut fields = BTreeMap::new();
        fields.insert("name".to_string(), Type::String);
        assert_eq!(
            parse_type("{name: string}").unwrap(),
            Type::Record {
                fields,
                open: false
            }
        );
    }

    #[test]
    fn parse_open_record() {
        let mut fields = BTreeMap::new();
        fields.insert("name".to_string(), Type::String);
        assert_eq!(
            parse_type("{name: string, ..}").unwrap(),
            Type::Record { fields, open: true }
        );
    }

    #[test]
    fn parse_fully_open_record() {
        assert_eq!(
            parse_type("{..}").unwrap(),
            Type::Record {
                fields: BTreeMap::new(),
                open: true
            }
        );
    }

    #[test]
    fn parse_empty_closed_record() {
        assert_eq!(
            parse_type("{}").unwrap(),
            Type::Record {
                fields: BTreeMap::new(),
                open: false
            }
        );
    }

    #[test]
    fn parse_multi_field_record() {
        let mut fields = BTreeMap::new();
        fields.insert("stdout".to_string(), Type::String);
        fields.insert("stderr".to_string(), Type::String);
        fields.insert("exit-code".to_string(), Type::Number);
        assert_eq!(
            parse_type("{stdout: string, stderr: string, exit-code: number}").unwrap(),
            Type::Record {
                fields,
                open: false
            }
        );
    }

    #[test]
    fn parse_duplicate_field_fails() {
        assert!(parse_type("{a: number, a: string}").is_err());
    }

    // ── IO, Lens, Traversal ─────────────────────────────────────────────────

    #[test]
    fn parse_io() {
        assert_eq!(
            parse_type("IO(string)").unwrap(),
            Type::IO(Box::new(Type::String))
        );
    }

    #[test]
    fn parse_io_record() {
        assert_eq!(
            parse_type("IO({stdout: string, stderr: string, exit-code: number})").unwrap(),
            Type::IO(Box::new(Type::Record {
                fields: {
                    let mut m = BTreeMap::new();
                    m.insert("stdout".to_string(), Type::String);
                    m.insert("stderr".to_string(), Type::String);
                    m.insert("exit-code".to_string(), Type::Number);
                    m
                },
                open: false
            }))
        );
    }

    #[test]
    fn parse_lens() {
        assert_eq!(
            parse_type("Lens({..}, any)").unwrap(),
            Type::Lens(
                Box::new(Type::Record {
                    fields: BTreeMap::new(),
                    open: true
                }),
                Box::new(Type::Any)
            )
        );
    }

    #[test]
    fn parse_traversal() {
        assert_eq!(
            parse_type("Traversal([a], a)").unwrap(),
            Type::Traversal(Box::new(Type::List(Box::new(var("a")))), Box::new(var("a")))
        );
    }

    // ── Round-trip ──────────────────────────────────────────────────────────

    fn roundtrip(input: &str) {
        let ty = parse_type(input).unwrap_or_else(|e| panic!("parse failed for '{input}': {e}"));
        let displayed = ty.to_string();
        let ty2 = parse_type(&displayed)
            .unwrap_or_else(|e| panic!("re-parse failed for '{displayed}': {e}"));
        assert_eq!(
            ty, ty2,
            "round-trip mismatch for '{input}': display gave '{displayed}'"
        );
    }

    #[test]
    fn roundtrip_primitives() {
        for prim in &[
            "number", "string", "symbol", "bool", "null", "datetime", "any", "top", "never", "set",
            "vec", "array",
        ] {
            roundtrip(prim);
        }
    }

    #[test]
    fn roundtrip_list() {
        roundtrip("[number]");
        roundtrip("[a]");
        roundtrip("[[string]]");
    }

    #[test]
    fn roundtrip_function() {
        roundtrip("number -> string");
        roundtrip("number -> number -> number");
    }

    #[test]
    fn roundtrip_io() {
        roundtrip("IO(string)");
        roundtrip("IO({stdout: string, stderr: string, exit-code: number})");
    }

    #[test]
    fn roundtrip_lens() {
        roundtrip("Lens({..}, any)");
    }

    #[test]
    fn roundtrip_traversal() {
        roundtrip("Traversal([a], a)");
    }

    #[test]
    fn roundtrip_record() {
        roundtrip("{name: string, ..}");
        roundtrip("{name: string}");
        roundtrip("{..}");
    }

    #[test]
    fn roundtrip_union() {
        roundtrip("number | string");
        roundtrip("number | string | bool");
    }

    #[test]
    fn roundtrip_higher_order() {
        roundtrip("(a -> b) -> [a] -> [b]");
        roundtrip("(b -> a -> b) -> b -> [a] -> b");
    }

    #[test]
    fn roundtrip_function_with_union_argument() {
        // LHS union: `(number | string) -> bool` must not display as
        // `number | string -> bool` (which re-parses as a union).
        roundtrip("(number | string) -> bool");
        // RHS union: `number -> (string | bool)` must not display as
        // `number -> string | bool` (which re-parses as a union).
        roundtrip("number -> (string | bool)");
        // Both sides union.
        roundtrip("(number | null) -> (string | bool)");
    }

    // ── Error cases ─────────────────────────────────────────────────────────

    #[test]
    fn error_empty_input() {
        let err = parse_type("").unwrap_err();
        assert!(err.message.contains("expected a type"));
    }

    #[test]
    fn error_trailing_junk() {
        let err = parse_type("number garbage").unwrap_err();
        assert!(err.message.contains("unexpected trailing input"));
    }

    #[test]
    fn error_unclosed_bracket() {
        assert!(parse_type("[number").is_err());
    }

    #[test]
    fn error_unclosed_paren() {
        assert!(parse_type("(number").is_err());
    }

    #[test]
    fn error_unclosed_brace() {
        assert!(parse_type("{name: string").is_err());
    }

    #[test]
    fn error_bad_char() {
        let err = parse_type("number @ string").unwrap_err();
        assert!(err.message.contains("unexpected character"));
    }

    #[test]
    fn error_position_reported() {
        let err = parse_type("[number").unwrap_err();
        // Position should be non-zero (after `[number`)
        assert!(err.position > 0);
    }

    // ── Spec examples ───────────────────────────────────────────────────────

    #[test]
    fn spec_map_type() {
        // (a -> b) -> [a] -> [b]
        let ty = parse_type("(a -> b) -> [a] -> [b]").unwrap();
        assert!(matches!(ty, Type::Function(_, _)));
    }

    #[test]
    fn spec_fold_type() {
        // (b -> a -> b) -> b -> [a] -> b
        let ty = parse_type("(b -> a -> b) -> b -> [a] -> b").unwrap();
        assert!(matches!(ty, Type::Function(_, _)));
    }

    #[test]
    fn spec_io_shell_type() {
        // string -> IO({stdout: string, stderr: string, exit-code: number})
        parse_type("string -> IO({stdout: string, stderr: string, exit-code: number})").unwrap();
    }

    #[test]
    fn spec_lens_at_type() {
        // symbol -> Lens({..}, any)
        parse_type("symbol -> Lens({..}, any)").unwrap();
    }

    #[test]
    fn spec_elements_type() {
        // {..} -> [(symbol, any)]
        let ty = parse_type("{..} -> [(symbol, any)]").unwrap();
        assert!(matches!(ty, Type::Function(_, _)));
    }
}
