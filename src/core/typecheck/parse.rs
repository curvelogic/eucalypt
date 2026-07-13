//! Hand-written recursive descent parser for type annotation strings.
//!
//! Parses the type grammar defined in the gradual typing spec (section 4).
//! This is entirely separate from the eucalypt expression parser — type
//! annotations live in metadata strings and are parsed on demand.
//!
//! Grammar (summary):
//! ```text
//! type        ::= forall | union
//! forall      ::= 'forall' binder+ '.' type         # rank-N quantification
//! binder      ::= IDENT | '(' IDENT '::' kind ')'
//! kind        ::= '*' | kind '->' kind | '(' kind ')'
//! union       ::= arrow ( '|' arrow )*
//! arrow       ::= application ( '->' application )*  # right-associative
//! application ::= primary primary*                   # left-associative HKT app
//! primary     ::= 'number' | 'string' | 'symbol' | 'bool' | 'null'
//!              | 'datetime' | 'any' | 'top' | 'never'
//!              | 'set' | 'vec' | 'array'
//!              | LOWER_IDENT                        # type variable (Star kind)
//!              | '"' STRING '"'                     # literal string type
//!              | '[' type ']'                       # list sugar → App(Con("List"), T)
//!              | 'IO' '(' type ')'                  # IO sugar  → App(Con("IO"), T)
//!              | 'Lens' '(' type ',' type ')'       # Lens sugar → App(App(Con("Lens"), A), B)
//!              | 'Traversal' '(' type ',' type ')'  # Traversal sugar
//!              | '{' row '}'                        # record
//!              | '(' paren_body ')'                 # grouping / tuple
//! paren_body  ::= type
//!              | type ','
//!              | type ( ',' type )+ ','?
//! row         ::= field ( ',' field )* ( ',' ('..' IDENT?)? )?
//!              | '..' IDENT?
//! field       ::= IDENT ':' type
//! ```

use std::collections::BTreeMap;
use std::fmt;

use super::types::{Constraint, Kind, Type, TypeVarId};

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
    Block,
    Forall,
    // Type constructors
    Io,
    Lens,
    Traversal,
    Dict,
    NonEmpty,
    // Identifiers (type variables and record field names)
    Ident(String),
    // Punctuation
    LBracket,   // [
    RBracket,   // ]
    LParen,     // (
    RParen,     // )
    LBrace,     // {
    RBrace,     // }
    Arrow,      // ->
    Pipe,       // |
    Colon,      // :
    ColonColon, // ::
    Comma,      // ,
    DotDot,     // ..
    Ellipsis,   // `…` (U+2026) or `...` — the prefix-list rest marker
    Dot,        // . (for forall binders: `forall a.`)
    Star,       // *  (kind)
    // The ExecutionError nullary type (written as `ExecutionError` or implied by `T?`)
    ExecutionError,
    // Postfix `?` — sugar for `| ExecutionError`
    Question,
    // Literal string value (e.g. `"read"` in type position)
    StringLit(String),
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
                if self.remaining().starts_with("::") {
                    self.pos += 2;
                    Ok((Token::ColonColon, start))
                } else {
                    self.pos += 1;
                    Ok((Token::Colon, start))
                }
            }
            ',' => {
                self.pos += 1;
                Ok((Token::Comma, start))
            }
            '…' => {
                // U+2026 HORIZONTAL ELLIPSIS — the canonical rest marker.
                self.pos += '…'.len_utf8();
                Ok((Token::Ellipsis, start))
            }
            '.' => {
                // Longest-match first: `...` (prefix-list rest) before `..`
                // (record row tail).
                if self.remaining().starts_with("...") {
                    self.pos += 3;
                    Ok((Token::Ellipsis, start))
                } else if self.remaining().starts_with("..") {
                    self.pos += 2;
                    Ok((Token::DotDot, start))
                } else {
                    self.pos += 1;
                    Ok((Token::Dot, start))
                }
            }
            '*' => {
                self.pos += 1;
                Ok((Token::Star, start))
            }
            '?' => {
                self.pos += 1;
                Ok((Token::Question, start))
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
            '→' => {
                self.pos += '→'.len_utf8();
                Ok((Token::Arrow, start))
            }
            '"' => {
                // Lex a double-quoted string literal for use in type position.
                self.pos += 1; // consume opening '"'
                let mut s = String::new();
                loop {
                    if self.pos >= self.input.len() {
                        return Err(ParseError::new(start, "unterminated string literal"));
                    }
                    let ch = self.input[self.pos..].chars().next().unwrap();
                    self.pos += ch.len_utf8();
                    match ch {
                        '"' => break, // closing quote
                        '\\' => {
                            // Escape: \\ → \, \" → "
                            if self.pos >= self.input.len() {
                                return Err(ParseError::new(start, "unterminated escape"));
                            }
                            let esc = self.input[self.pos..].chars().next().unwrap();
                            self.pos += esc.len_utf8();
                            match esc {
                                '\\' => s.push('\\'),
                                '"' => s.push('"'),
                                other => {
                                    s.push('\\');
                                    s.push(other);
                                }
                            }
                        }
                        other => s.push(other),
                    }
                }
                Ok((Token::StringLit(s), start))
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
                    "block" => Token::Block,
                    "forall" => Token::Forall,
                    "IO" => Token::Io,
                    "Dict" => Token::Dict,
                    "Lens" => Token::Lens,
                    "Traversal" => Token::Traversal,
                    "NonEmpty" => Token::NonEmpty,
                    "ExecutionError" => Token::ExecutionError,
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

// ── AliasRef ────────────────────────────────────────────────────────────────

/// A reference to a type alias inside a type-annotation string.
///
/// The `span` is a `(start, end)` byte range within the **content** of the
/// type-string (i.e. after stripping the surrounding `"…"`).  Only
/// capitalised identifiers are recorded; lowercase identifiers are
/// type-variable binders and are not alias references.
///
/// Used by [`parse_type_with_refs`] for LSP go-to-definition / hover /
/// rename support (§A7.1).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasRef {
    /// The alias name as it appears in the type string (e.g. `"Person"`).
    pub name: String,
    /// Byte range `(start, end)` within the type-string content.
    pub span: (usize, usize),
}

// ── Parser ──────────────────────────────────────────────────────────────────

struct Parser<'a> {
    lexer: Lexer<'a>,
    /// One-token lookahead buffer: `(token, position)`.
    peeked: Option<(Token, usize)>,
    /// Alias references collected during parsing (only populated when the
    /// caller uses [`parse_type_with_refs`]).
    alias_refs: Vec<AliasRef>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Parser {
            lexer: Lexer::new(input),
            peeked: None,
            alias_refs: Vec::new(),
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
        let ty = self.parse_type()?;
        let (tok, pos) = self.advance()?;
        if tok != Token::Eof {
            return Err(ParseError::new(
                pos,
                format!("unexpected trailing input: {tok:?}"),
            ));
        }
        Ok(ty)
    }

    /// `type ::= forall | union`
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        if self.peek()? == &Token::Forall {
            self.parse_forall()
        } else {
            self.parse_union()
        }
    }

    /// `forall ::= 'forall' binder+ '.' type`
    ///
    /// binder ::= IDENT | '(' IDENT '::' kind ')'
    fn parse_forall(&mut self) -> Result<Type, ParseError> {
        let pos = self.pos();
        self.advance()?; // consume 'forall'

        let mut binders: Vec<(TypeVarId, Kind)> = Vec::new();

        // Parse at least one binder before the dot
        loop {
            match self.peek()? {
                Token::Dot => {
                    // Consumed the '.', move on to the body
                    self.advance()?;
                    break;
                }
                Token::Ident(_) => {
                    // Simple binder with inferred Star kind
                    let (tok, _) = self.advance()?;
                    if let Token::Ident(name) = tok {
                        binders.push((TypeVarId(name), Kind::Star));
                    }
                }
                Token::LParen => {
                    // `( IDENT :: kind )`
                    self.advance()?; // consume '('
                    let (tok, binder_pos) = self.advance()?;
                    let name = if let Token::Ident(name) = tok {
                        name
                    } else {
                        return Err(ParseError::new(
                            binder_pos,
                            "expected a type variable name in forall binder",
                        ));
                    };
                    self.expect(&Token::ColonColon)?;
                    let kind = self.parse_kind()?;
                    self.expect(&Token::RParen)?;
                    binders.push((TypeVarId(name), kind));
                }
                _ => {
                    break;
                }
            }
        }

        if binders.is_empty() {
            return Err(ParseError::new(
                pos,
                "forall requires at least one type variable binder",
            ));
        }

        let body = self.parse_type()?;
        Ok(Type::Forall(binders, Box::new(body)))
    }

    /// `kind ::= kind_atom ( '->' kind )*`  (right-associative)
    fn parse_kind(&mut self) -> Result<Kind, ParseError> {
        let lhs = self.parse_kind_atom()?;
        if self.peek()? == &Token::Arrow {
            self.advance()?;
            let rhs = self.parse_kind()?;
            Ok(Kind::Arrow(Box::new(lhs), Box::new(rhs)))
        } else {
            Ok(lhs)
        }
    }

    /// `kind_atom ::= '*' | '(' kind ')'`
    fn parse_kind_atom(&mut self) -> Result<Kind, ParseError> {
        let (tok, tok_pos) = self.advance()?;
        match tok {
            Token::Star => Ok(Kind::Star),
            Token::LParen => {
                let k = self.parse_kind()?;
                self.expect(&Token::RParen)?;
                Ok(k)
            }
            other => Err(ParseError::new(
                tok_pos,
                format!("expected a kind ('*' or '(...)'), got {other:?}"),
            )),
        }
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

    /// `arrow ::= application ( '->' application )*`  (right-associative)
    fn parse_arrow(&mut self) -> Result<Type, ParseError> {
        let lhs = self.parse_application()?;
        if self.peek()? == &Token::Arrow {
            self.advance()?;
            // Right-associative: recurse via parse_arrow
            let rhs = self.parse_arrow()?;
            Ok(Type::Function(Box::new(lhs), Box::new(rhs)))
        } else {
            Ok(lhs)
        }
    }

    /// `application ::= primary primary* '?'?`
    ///
    /// Juxtaposition applies a type constructor to its arguments:
    /// `m a` → `App(m, a)`, `m a b` → `App(App(m, a), b)`.
    ///
    /// A trailing `?` desugars to `| ExecutionError`:
    /// `T?` → `T | ExecutionError`.
    fn parse_application(&mut self) -> Result<Type, ParseError> {
        let head = self.parse_primary()?;
        let mut result = head;
        loop {
            if self.peek()? == &Token::Question {
                self.advance()?;
                result = Type::partial(result);
            } else if self.can_start_primary()? {
                let arg = self.parse_primary()?;
                result = Type::App(Box::new(result), Box::new(arg));
            } else {
                break;
            }
        }
        Ok(result)
    }

    /// Returns true if the next token can begin a `primary` type expression.
    fn can_start_primary(&mut self) -> Result<bool, ParseError> {
        Ok(matches!(
            self.peek()?,
            Token::Number
                | Token::String
                | Token::Symbol
                | Token::Bool
                | Token::Null
                | Token::Datetime
                | Token::Any
                | Token::Top
                | Token::Never
                | Token::Set
                | Token::Vec
                | Token::Array
                | Token::Block
                | Token::Io
                | Token::Lens
                | Token::Traversal
                | Token::Dict
                | Token::NonEmpty
                | Token::Ident(_)
                | Token::LBracket
                | Token::LParen
                | Token::LBrace
                | Token::Colon
                | Token::StringLit(_)
                | Token::Star
        ))
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
            Token::ExecutionError => Ok(Type::ExecutionError),
            Token::Set => Ok(Type::Set),
            Token::Vec => Ok(Type::Vec),
            Token::Array => Ok(Type::Array),
            Token::Block => Ok(Type::Record {
                fields: BTreeMap::new(),
                open: true,
                rows: vec![],
            }),
            Token::Dict => {
                // `Dict` `(` type `)`
                self.expect(&Token::LParen)?;
                let inner = self.parse_type()?;
                self.expect(&Token::RParen)?;
                Ok(Type::dict(inner))
            }
            Token::NonEmpty => {
                // `NonEmpty` `(` `[` type `]` `)`
                self.expect(&Token::LParen)?;
                self.expect(&Token::LBracket)?;
                let inner = self.parse_type()?;
                self.expect(&Token::RBracket)?;
                self.expect(&Token::RParen)?;
                Ok(Type::non_empty(inner))
            }
            Token::Star => {
                // `*` as a type (used only in kind annotations — but if it
                // appears in a type position here it means a kind-level `*`
                // was parsed as a primary; treat as Con("*") so kind checker
                // can report a sensible error).
                Ok(Type::Con("*".to_string()))
            }
            Token::Ident(name) => {
                // Type variable (lowercase) or type alias reference (uppercase).
                //
                // Both are represented as `Type::Var` at parse time.  Lowercase
                // idents are universally-quantified type variables; uppercase
                // idents are alias references resolved by the checker against
                // its alias map.  The checker erases any unresolved `Var`s to
                // `any` via `erase_type_vars`.
                if name.starts_with(|c: char| c.is_ascii_alphabetic()) {
                    // Record capitalised idents as alias references.  Identifier
                    // bytes are all ASCII so `name.len()` == byte length.
                    if name.starts_with(|c: char| c.is_ascii_uppercase()) {
                        self.alias_refs.push(AliasRef {
                            span: (tok_pos, tok_pos + name.len()),
                            name: name.clone(),
                        });
                    }
                    Ok(Type::var(TypeVarId(name)))
                } else {
                    Err(ParseError::new(
                        tok_pos,
                        format!("unknown type '{name}' (type names must start with a letter)"),
                    ))
                }
            }
            Token::LBracket => {
                // Two forms share the `[` bracket:
                //   `[` type `]`                          — homogeneous list
                //   `[` type ( `,` type )* ELLIPSIS `]`   — prefix-list
                // The element immediately before the ellipsis is the tail; the
                // earlier elements form the fixed prefix.
                let mut elems = vec![self.parse_type()?];
                while self.peek()? == &Token::Comma {
                    self.advance()?; // consume ','
                    elems.push(self.parse_type()?);
                }
                if self.peek()? == &Token::Ellipsis {
                    self.advance()?; // consume the ellipsis marker
                    self.expect(&Token::RBracket)?;
                    // Safe: `elems` always has at least one element. The final
                    // element is the homogeneous tail; the rest is the prefix.
                    let tail = elems.pop().unwrap();
                    Ok(Type::prefix_list(elems, tail))
                } else if elems.len() == 1 {
                    self.expect(&Token::RBracket)?;
                    Ok(Type::list(elems.pop().unwrap()))
                } else {
                    // `[A, B]` with no trailing ellipsis is deliberately not a
                    // tuple surface (that is `(A, B)`); reserve comma-in-brackets
                    // for the ellipsis-terminated prefix-list only (design §2.1).
                    Err(ParseError::new(
                        tok_pos,
                        "comma-separated bracket type requires a trailing ellipsis \
                         (`[A, B, C…]`); use `(A, B)` for a fixed tuple",
                    ))
                }
            }
            Token::LParen => self.parse_paren_body(tok_pos),
            Token::LBrace => self.parse_record(tok_pos),
            Token::Io => {
                // `IO` `(` type `)`
                self.expect(&Token::LParen)?;
                let inner = self.parse_type()?;
                self.expect(&Token::RParen)?;
                Ok(Type::io(inner))
            }
            Token::Lens => {
                // `Lens` `(` type `,` type `)`
                self.expect(&Token::LParen)?;
                let a = self.parse_type()?;
                self.expect(&Token::Comma)?;
                let b = self.parse_type()?;
                self.expect(&Token::RParen)?;
                Ok(Type::lens(a, b))
            }
            Token::Traversal => {
                // `Traversal` `(` type `,` type `)`
                self.expect(&Token::LParen)?;
                let a = self.parse_type()?;
                self.expect(&Token::Comma)?;
                let b = self.parse_type()?;
                self.expect(&Token::RParen)?;
                Ok(Type::traversal(a, b))
            }
            Token::StringLit(s) => {
                // Literal string type: `"some value"` in type position.
                Ok(Type::LiteralString(s))
            }
            Token::Colon => {
                // Literal symbol type: `:ident`
                //
                // After `:`, we accept any identifier-like token — including
                // keywords that the lexer maps to their own token variants
                // (e.g. `number`, `string`).  These are all valid symbol names
                // in eucalypt.
                let (next_tok, next_pos) = self.advance()?;
                let name = match &next_tok {
                    Token::Ident(name) => name.clone(),
                    Token::Number => "number".to_string(),
                    Token::String => "string".to_string(),
                    Token::Symbol => "symbol".to_string(),
                    Token::Bool => "bool".to_string(),
                    Token::Null => "null".to_string(),
                    Token::Datetime => "datetime".to_string(),
                    Token::Any => "any".to_string(),
                    Token::Top => "top".to_string(),
                    Token::Never => "never".to_string(),
                    Token::Set => "set".to_string(),
                    Token::Vec => "vec".to_string(),
                    Token::Array => "array".to_string(),
                    Token::Block => "block".to_string(),
                    Token::Forall => "forall".to_string(),
                    Token::Io => "IO".to_string(),
                    Token::Dict => "Dict".to_string(),
                    Token::Lens => "Lens".to_string(),
                    Token::Traversal => "Traversal".to_string(),
                    Token::NonEmpty => "NonEmpty".to_string(),
                    _ => {
                        return Err(ParseError::new(
                            next_pos,
                            format!("expected an identifier after ':' in literal symbol type, got {next_tok:?}"),
                        ));
                    }
                };
                Ok(Type::LiteralSymbol(name))
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

        let first = self.parse_type()?;

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
                            elems.push(self.parse_type()?);
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
    /// row   ::= field (',' field)* (',' ('..' IDENT?)?)?
    ///         | '..' IDENT?
    /// field ::= IDENT ':' type
    /// ```
    ///
    /// When `..` is followed by a lowercase identifier, it is a named row
    /// variable (e.g. `{x: number, ..r}`).  When `..` appears alone the
    /// record is open with an anonymous tail (e.g. `{x: number, ..}`).
    fn parse_record(&mut self, _open_pos: usize) -> Result<Type, ParseError> {
        use super::types::{FieldPresence, TypeVarId};

        // After consuming `..`, optionally consume a following ident as the
        // row variable name.  Returns a named TypeVarId or None for anonymous open.
        let parse_row_var = |parser: &mut Parser<'_>| -> Result<Option<TypeVarId>, ParseError> {
            match parser.peek()? {
                Token::Ident(_) => {
                    let (tok, _) = parser.advance()?;
                    if let Token::Ident(name) = tok {
                        Ok(Some(TypeVarId(name)))
                    } else {
                        unreachable!()
                    }
                }
                _ => Ok(None),
            }
        };

        // Empty open record — `{..}` or `{..r}`.
        //
        // Semantics:
        //   `{..}`    — anonymously open: `open: true, rows: []`
        //   `{..r}`   — named row var:   `open: false, rows: [r]`
        //   `{..r, ..s}` — two row vars: `open: false, rows: [r, s]`
        //   `{..r, ..}` — row var + anon open: `open: true, rows: [r]`
        if self.peek()? == &Token::DotDot {
            self.advance()?;
            let mut anon_open = false;
            let mut more_rows: Vec<TypeVarId> = Vec::new();
            let row_var = parse_row_var(self)?;
            match row_var {
                Some(rv) => more_rows.push(rv),
                None => anon_open = true,
            }
            if !anon_open {
                // Possibly more `,..<row>` or `,..<anon>` entries
                loop {
                    match self.peek()? {
                        Token::Comma => {
                            self.advance()?;
                            if self.peek()? == &Token::DotDot {
                                self.advance()?;
                                let rv = parse_row_var(self)?;
                                match rv {
                                    Some(r) => more_rows.push(r),
                                    None => {
                                        anon_open = true;
                                        break;
                                    }
                                }
                            } else {
                                let p = self.pos();
                                return Err(ParseError::new(
                                    p,
                                    "expected '..' after ',' in open record tail",
                                ));
                            }
                        }
                        Token::RBrace => break,
                        _ => {
                            let p = self.pos();
                            return Err(ParseError::new(
                                p,
                                "expected ',' or '}' after row variable",
                            ));
                        }
                    }
                }
            }
            self.expect(&Token::RBrace)?;
            return Ok(Type::Record {
                fields: BTreeMap::new(),
                open: anon_open,
                rows: more_rows,
            });
        }

        // Empty closed record: `{}`
        if self.peek()? == &Token::RBrace {
            self.advance()?;
            return Ok(Type::Record {
                fields: BTreeMap::new(),
                open: false,
                rows: vec![],
            });
        }

        let mut fields: BTreeMap<String, FieldPresence> = BTreeMap::new();
        let mut open = false;
        let mut rows: Vec<TypeVarId> = vec![];

        loop {
            // Check for `..` (open marker) or a field name
            let pos = self.pos();
            match self.peek()? {
                Token::DotDot => {
                    self.advance()?;
                    let rv = parse_row_var(self)?;
                    match rv {
                        Some(r) => rows.push(r),
                        None => {
                            open = true;
                        }
                    }
                    // Allow additional `,..<row>` tail entries
                    if !open {
                        loop {
                            match self.peek()? {
                                Token::Comma => {
                                    self.advance()?;
                                    if self.peek()? == &Token::DotDot {
                                        self.advance()?;
                                        let extra = parse_row_var(self)?;
                                        match extra {
                                            Some(r) => rows.push(r),
                                            None => {
                                                open = true;
                                                break;
                                            }
                                        }
                                    } else {
                                        let p = self.pos();
                                        return Err(ParseError::new(
                                            p,
                                            "expected '..' after ',' in open record tail",
                                        ));
                                    }
                                }
                                Token::RBrace => break,
                                _ => {
                                    let p = self.pos();
                                    return Err(ParseError::new(
                                        p,
                                        "expected ',' or '}' after row variable",
                                    ));
                                }
                            }
                        }
                    }
                    self.advance()?; // consume '}'
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
                    // Optional field: `name?:`
                    let is_optional = if self.peek()? == &Token::Question {
                        self.advance()?;
                        true
                    } else {
                        false
                    };
                    self.expect(&Token::Colon)?;
                    let field_type = self.parse_type()?;
                    if fields.contains_key(&field_name) {
                        return Err(ParseError::new(
                            field_pos,
                            format!("duplicate field '{field_name}' in record type"),
                        ));
                    }
                    let presence = if is_optional {
                        FieldPresence::Optional(field_type)
                    } else {
                        FieldPresence::Required(field_type)
                    };
                    fields.insert(field_name, presence);

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

        Ok(Type::Record { fields, open, rows })
    }
}

// ── Constraint parsing helpers ───────────────────────────────────────────────

/// Find the byte position of the first top-level `=>` sequence (not inside
/// `()`, `[]`, or `{}`).  Returns `None` if no such sequence exists.
fn find_top_level_fat_arrow(input: &str) -> Option<usize> {
    let bytes = input.as_bytes();
    let mut depth: usize = 0;
    let mut i = 0;
    while i < bytes.len() {
        // UTF-8: continuation bytes (0x80–0xBF) never match ASCII punctuation,
        // so byte-level scanning is safe here.
        match bytes[i] {
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' => depth = depth.saturating_sub(1),
            b'=' if depth == 0 && bytes.get(i + 1) == Some(&b'>') => {
                return Some(i);
            }
            _ => {}
        }
        i += 1;
    }
    None
}

/// Split `input` on commas that are NOT inside any `()`, `[]`, or `{}`.
fn split_top_level_commas(input: &str) -> Vec<&str> {
    let bytes = input.as_bytes();
    let mut parts: Vec<&str> = Vec::new();
    let mut depth: usize = 0;
    let mut start = 0usize;
    for i in 0..bytes.len() {
        match bytes[i] {
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' => depth = depth.saturating_sub(1),
            b',' if depth == 0 => {
                parts.push(&input[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    parts.push(&input[start..]);
    parts
}

/// Parse a single constraint such as `<(a, a)` or `str.of(a)`.
///
/// The operator name is everything before the opening `(`, trimmed of
/// whitespace.  The argument list is the comma-separated type sequence
/// inside the parentheses.  The `base_offset` is the byte position of
/// `input` within the original annotation string, used for error positions.
fn parse_single_constraint(input: &str, base_offset: usize) -> Result<Constraint, ParseError> {
    // Locate the opening parenthesis
    let paren_pos = input.find('(').ok_or_else(|| {
        ParseError::new(base_offset, format!("expected '(' in constraint '{input}'"))
    })?;

    let func_name = input[..paren_pos].trim().to_string();
    if func_name.is_empty() {
        return Err(ParseError::new(
            base_offset,
            "constraint operator name cannot be empty",
        ));
    }

    // Find the matching close parenthesis
    let after_open = &input[paren_pos + 1..];
    let mut depth = 1usize;
    let mut close_pos: Option<usize> = None;
    for (i, b) in after_open.bytes().enumerate() {
        match b {
            b'(' => depth += 1,
            b')' => {
                depth -= 1;
                if depth == 0 {
                    close_pos = Some(i);
                    break;
                }
            }
            _ => {}
        }
    }
    let close = close_pos.ok_or_else(|| {
        ParseError::new(
            base_offset + paren_pos,
            "unclosed '(' in constraint argument list",
        )
    })?;

    // Ensure no unexpected trailing characters after the closing paren
    let trailing = after_open[close + 1..].trim();
    if !trailing.is_empty() {
        return Err(ParseError::new(
            base_offset + paren_pos + 1 + close + 1,
            format!("unexpected trailing input after constraint: '{trailing}'"),
        ));
    }

    // Parse the comma-separated argument types
    let arg_str = &after_open[..close];
    let arg_parts = split_top_level_commas(arg_str);
    let mut args: Vec<Type> = Vec::new();
    for part in arg_parts {
        let part = part.trim();
        if !part.is_empty() {
            let ty = parse_type(part)
                .map_err(|e| ParseError::new(base_offset + e.position, e.message))?;
            args.push(ty);
        }
    }

    Ok(Constraint {
        function: func_name,
        args,
    })
}

/// Parse a comma-separated list of constraints (the part before `=>`).
fn parse_constraint_list(input: &str, base_offset: usize) -> Result<Vec<Constraint>, ParseError> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return Ok(Vec::new());
    }
    // Split on top-level commas that separate individual constraints
    let parts = split_top_level_commas(trimmed);
    let mut constraints: Vec<Constraint> = Vec::new();
    let mut offset = base_offset;
    for part in parts {
        let trimmed_part = part.trim();
        if !trimmed_part.is_empty() {
            constraints.push(parse_single_constraint(trimmed_part, offset)?);
        }
        offset += part.len() + 1; // +1 for the comma
    }
    Ok(constraints)
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
///     Type::list(Type::String)
/// );
/// ```
pub fn parse_type(input: &str) -> Result<Type, ParseError> {
    let mut parser = Parser::new(input);
    parser.parse_type_toplevel()
}

/// Parse a type-annotation string that may include an operator constraint prefix.
///
/// The constraint DSL (B2) allows annotations of the form:
///
/// ```text
/// <(a, a) => a -> a -> a
/// <(a, a), +(a, a) => a -> a -> a
/// str.of(a) => [a] -> [string]
/// ```
///
/// A comma-separated list of constraints precedes `=>`.  Each constraint is
/// `operator_name(type, …)`.  If no `=>` is present at the top level (i.e. not
/// inside parentheses/brackets/braces), the string is parsed as a plain type.
///
/// Returns `(body_type, constraints)`.
///
/// # Examples
///
/// ```
/// use eucalypt::core::typecheck::parse::parse_scheme;
/// use eucalypt::core::typecheck::types::Type;
///
/// let (ty, constraints) = parse_scheme("<(a, a) => a -> a -> a").unwrap();
/// assert!(matches!(ty, Type::Function(_, _)));
/// assert_eq!(constraints.len(), 1);
/// assert_eq!(constraints[0].function, "<");
///
/// let (ty2, cs2) = parse_scheme("number -> number").unwrap();
/// assert_eq!(ty2, Type::Function(Box::new(Type::Number), Box::new(Type::Number)));
/// assert!(cs2.is_empty());
/// ```
pub fn parse_scheme(input: &str) -> Result<(Type, Vec<Constraint>), ParseError> {
    if let Some(arrow_pos) = find_top_level_fat_arrow(input) {
        let constraint_part = &input[..arrow_pos];
        let body_part = input[arrow_pos + 2..].trim();
        let constraints = parse_constraint_list(constraint_part, 0)?;
        let body = parse_type(body_part)?;
        Ok((body, constraints))
    } else {
        let ty = parse_type(input)?;
        Ok((ty, Vec::new()))
    }
}

/// Render a parsed scheme (type + constraints) back to its canonical string
/// form.  If there are no constraints the result is just `format!("{ty}")`.
pub fn render_scheme(ty: &Type, constraints: &[Constraint]) -> String {
    if constraints.is_empty() {
        format!("{ty}")
    } else {
        use std::fmt::Write;
        let mut out = String::new();
        for (i, c) in constraints.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            let _ = write!(out, "{c}");
        }
        let _ = write!(out, " => {ty}");
        out
    }
}

/// Parse a type-annotation string and return all alias references found.
///
/// This is the tooling-oriented entry point (§A7.1).  It is identical to
/// [`parse_type`] in every respect except that it also returns a
/// `Vec<AliasRef>` recording the byte spans of every **capitalised**
/// identifier encountered — these are type-alias references that can be
/// resolved by the LSP for go-to-definition, hover, and rename.
///
/// Lowercase identifiers (universally-quantified type variables) are **not**
/// recorded.  A malformed type string still returns an `Err` as before.
///
/// # Example
/// ```
/// # use eucalypt::core::typecheck::parse::parse_type_with_refs;
/// let (ty, refs) = parse_type_with_refs("Person -> Json").unwrap();
/// assert_eq!(refs.len(), 2);
/// assert_eq!(refs[0].name, "Person");
/// assert_eq!(refs[0].span, (0, 6));
/// assert_eq!(refs[1].name, "Json");
/// assert_eq!(refs[1].span, (10, 14));
/// ```
pub fn parse_type_with_refs(input: &str) -> Result<(Type, Vec<AliasRef>), ParseError> {
    let mut parser = Parser::new(input);
    let ty = parser.parse_type_toplevel()?;
    Ok((ty, parser.alias_refs))
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::typecheck::types::{FieldPresence, Type, TypeVarId};
    use std::collections::BTreeMap;

    fn var(name: &str) -> Type {
        Type::var(TypeVarId(name.to_string()))
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
        // `block` is shorthand for open empty record `{..}`
        assert_eq!(
            parse_type("block").unwrap(),
            Type::Record {
                fields: BTreeMap::new(),
                open: true,
                rows: vec![],
            }
        );
        assert_eq!(
            parse_type("symbol → block → any").unwrap(),
            parse_type("symbol → {..} → any").unwrap(),
        );
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
        assert_eq!(parse_type("[number]").unwrap(), Type::list(Type::Number));
    }

    #[test]
    fn parse_list_of_var() {
        assert_eq!(parse_type("[a]").unwrap(), Type::list(var("a")));
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
    fn parse_unicode_arrow() {
        // → is accepted as an alternative to ->
        assert_eq!(
            parse_type("number → string").unwrap(),
            Type::Function(Box::new(Type::Number), Box::new(Type::String))
        );
    }

    #[test]
    fn parse_unicode_arrow_curried() {
        assert_eq!(
            parse_type("(a → b) → [a] → [b]").unwrap(),
            parse_type("(a -> b) -> [a] -> [b]").unwrap(),
        );
    }

    #[test]
    fn parse_mixed_arrows() {
        // Mixing -> and → in the same annotation is accepted
        assert_eq!(
            parse_type("number -> string → bool").unwrap(),
            parse_type("number -> string -> bool").unwrap(),
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
                    Box::new(Type::list(var("a"))),
                    Box::new(Type::list(var("b")))
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
        fields.insert("name".to_string(), FieldPresence::Required(Type::String));
        assert_eq!(
            parse_type("{name: string}").unwrap(),
            Type::Record {
                fields,
                open: false,
                rows: vec![],
            }
        );
    }

    #[test]
    fn parse_open_record() {
        let mut fields = BTreeMap::new();
        fields.insert("name".to_string(), FieldPresence::Required(Type::String));
        assert_eq!(
            parse_type("{name: string, ..}").unwrap(),
            Type::Record {
                fields,
                open: true,
                rows: vec![]
            }
        );
    }

    #[test]
    fn parse_optional_field() {
        let ty = parse_type("{name: string, age?: number}").unwrap();
        if let Type::Record { fields, open, rows } = ty {
            assert!(!open);
            assert!(rows.is_empty());
            assert_eq!(
                fields.get("name"),
                Some(&FieldPresence::Required(Type::String))
            );
            assert_eq!(
                fields.get("age"),
                Some(&FieldPresence::Optional(Type::Number))
            );
        } else {
            panic!("expected Record type");
        }
    }

    #[test]
    fn roundtrip_optional_field() {
        roundtrip("{age?: number, name: string}");
    }

    #[test]
    fn parse_fully_open_record() {
        assert_eq!(
            parse_type("{..}").unwrap(),
            Type::Record {
                fields: BTreeMap::new(),
                open: true,
                rows: vec![],
            }
        );
    }

    #[test]
    fn parse_empty_closed_record() {
        assert_eq!(
            parse_type("{}").unwrap(),
            Type::Record {
                fields: BTreeMap::new(),
                open: false,
                rows: vec![],
            }
        );
    }

    #[test]
    fn parse_multi_field_record() {
        let mut fields = BTreeMap::new();
        fields.insert("stdout".to_string(), FieldPresence::Required(Type::String));
        fields.insert("stderr".to_string(), FieldPresence::Required(Type::String));
        fields.insert(
            "exit-code".to_string(),
            FieldPresence::Required(Type::Number),
        );
        assert_eq!(
            parse_type("{stdout: string, stderr: string, exit-code: number}").unwrap(),
            Type::Record {
                fields,
                open: false,
                rows: vec![],
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
        assert_eq!(parse_type("IO(string)").unwrap(), Type::io(Type::String));
    }

    #[test]
    fn parse_io_record() {
        assert_eq!(
            parse_type("IO({stdout: string, stderr: string, exit-code: number})").unwrap(),
            Type::io(Type::Record {
                fields: {
                    let mut m = BTreeMap::new();
                    m.insert("stdout".to_string(), FieldPresence::Required(Type::String));
                    m.insert("stderr".to_string(), FieldPresence::Required(Type::String));
                    m.insert(
                        "exit-code".to_string(),
                        FieldPresence::Required(Type::Number),
                    );
                    m
                },
                open: false,
                rows: vec![],
            })
        );
    }

    #[test]
    fn parse_lens() {
        assert_eq!(
            parse_type("Lens({..}, any)").unwrap(),
            Type::lens(
                Type::Record {
                    fields: BTreeMap::new(),
                    open: true,
                    rows: vec![],
                },
                Type::Any
            )
        );
    }

    #[test]
    fn parse_traversal() {
        assert_eq!(
            parse_type("Traversal([a], a)").unwrap(),
            Type::traversal(Type::list(var("a")), var("a"))
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

    // ── Prefix-lists ──────────────────────────────────────────────────────────

    #[test]
    fn parse_prefix_list_basic() {
        // `[A, B, C…]` → PrefixList { prefix: [A, B], tail: C }.
        assert_eq!(
            parse_type("[symbol, block, string…]").unwrap(),
            Type::prefix_list(
                vec![
                    Type::Symbol,
                    Type::Record {
                        fields: BTreeMap::new(),
                        open: true,
                        rows: vec![],
                    },
                ],
                Type::String,
            ),
        );
    }

    #[test]
    fn parse_prefix_list_grouped_union_tail() {
        // The tail may be a grouped union: `(string | Element)…`.
        assert_eq!(
            parse_type("[symbol, block, (string | Element)…]").unwrap(),
            Type::prefix_list(
                vec![
                    Type::Symbol,
                    Type::Record {
                        fields: BTreeMap::new(),
                        open: true,
                        rows: vec![],
                    },
                ],
                Type::union([Type::String, var("Element")]),
            ),
        );
    }

    #[test]
    fn ellipsis_spellings_are_equivalent() {
        // Both `…` (U+2026) and ASCII `...` lex to the same token, so the two
        // spellings produce the exact same `Type` value (owner decision Q1).
        let uni = parse_type("[symbol, number, string…]").unwrap();
        let ascii = parse_type("[symbol, number, string...]").unwrap();
        assert_eq!(uni, ascii);
        // Display always renders the canonical `…` regardless of spelling.
        assert_eq!(format!("{ascii}"), "[symbol, number, string…]");
    }

    #[test]
    fn prefix_list_single_element_normalises_to_list() {
        // `[C…]` (empty prefix) canonicalises to `List(C)` at construction time,
        // not a distinct `PrefixList` (owner decision Q3).
        assert_eq!(parse_type("[string…]").unwrap(), Type::list(Type::String));
        assert_eq!(parse_type("[string...]").unwrap(), Type::list(Type::String));
    }

    #[test]
    fn prefix_list_without_ellipsis_errors() {
        // `[A, B]` (comma, no trailing ellipsis) is deliberately a parse error;
        // fixed tuples are spelled `(A, B)`.
        assert!(parse_type("[symbol, string]").is_err());
    }

    #[test]
    fn ellipsis_does_not_disturb_record_row_tail() {
        // The `...` longest-match must not break the record row tail `..r`.
        roundtrip("{name: string, ..}");
        roundtrip("{a: number, ..r}");
    }

    #[test]
    fn roundtrip_prefix_list() {
        roundtrip("[symbol, block, string…]");
        roundtrip("[symbol, block, (string | Element)…]");
        roundtrip("[number, number, number…]");
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
        // `number garbage` now parses as `App(Number, Var("garbage"))` via
        // the application level — a kind error, not a parse error. Test that
        // genuinely non-parseable trailing input is still rejected.
        let err = parse_type("number | ").unwrap_err();
        assert!(err.message.contains("expected a type"));
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

    // ── Literal symbol types ───────────────────────────────────────────────

    #[test]
    fn parse_literal_symbol() {
        assert_eq!(
            parse_type(":active").unwrap(),
            Type::LiteralSymbol("active".to_string())
        );
    }

    #[test]
    fn parse_literal_symbol_union() {
        assert_eq!(
            parse_type(":active | :inactive").unwrap(),
            Type::Union(vec![
                Type::LiteralSymbol("active".to_string()),
                Type::LiteralSymbol("inactive".to_string()),
            ])
        );
    }

    #[test]
    fn parse_literal_symbol_keyword_name() {
        // Symbol names that collide with type keywords should still work
        assert_eq!(
            parse_type(":number").unwrap(),
            Type::LiteralSymbol("number".to_string())
        );
    }

    #[test]
    fn roundtrip_literal_symbol() {
        roundtrip(":active");
        roundtrip(":active | :inactive");
    }

    // ── Named row variables ─────────────────────────────────────────────────

    #[test]
    fn parse_record_named_row_with_field() {
        // {x: number, ..r} — named row variable after a field
        // open: false because openness is captured in the named row var, not anonymous
        let mut fields = BTreeMap::new();
        fields.insert("x".to_string(), FieldPresence::Required(Type::Number));
        assert_eq!(
            parse_type("{x: number, ..r}").unwrap(),
            Type::Record {
                fields,
                open: false,
                rows: vec![TypeVarId("r".to_string())],
            }
        );
    }

    #[test]
    fn parse_record_only_row_var() {
        // {..r} — just a named row variable, no explicit fields
        // open: false — the row var captures all extras
        assert_eq!(
            parse_type("{..r}").unwrap(),
            Type::Record {
                fields: BTreeMap::new(),
                open: false,
                rows: vec![TypeVarId("r".to_string())],
            }
        );
    }

    #[test]
    fn parse_record_named_row_multi_field() {
        // {name: string, age: number, ..rest}
        let mut fields = BTreeMap::new();
        fields.insert("name".to_string(), FieldPresence::Required(Type::String));
        fields.insert("age".to_string(), FieldPresence::Required(Type::Number));
        assert_eq!(
            parse_type("{name: string, age: number, ..rest}").unwrap(),
            Type::Record {
                fields,
                open: false,
                rows: vec![TypeVarId("rest".to_string())],
            }
        );
    }

    #[test]
    fn parse_record_two_row_vars() {
        // {..r, ..s} — two named row variables (row concatenation)
        // open: false — both row vars together capture all extras
        assert_eq!(
            parse_type("{..r, ..s}").unwrap(),
            Type::Record {
                fields: BTreeMap::new(),
                open: false,
                rows: vec![TypeVarId("r".to_string()), TypeVarId("s".to_string())],
            }
        );
    }

    #[test]
    fn parse_record_field_then_two_row_vars() {
        // {x: number, ..r, ..s}
        let mut fields = BTreeMap::new();
        fields.insert("x".to_string(), FieldPresence::Required(Type::Number));
        assert_eq!(
            parse_type("{x: number, ..r, ..s}").unwrap(),
            Type::Record {
                fields,
                open: false,
                rows: vec![TypeVarId("r".to_string()), TypeVarId("s".to_string())],
            }
        );
    }

    #[test]
    fn roundtrip_named_row_var() {
        roundtrip("{x: number, ..r}");
        roundtrip("{..r}");
        roundtrip("{name: string, age: number, ..rest}");
        roundtrip("{..r, ..s}");
    }

    // ── Higher-kinded types (HKT) ──────────────────────────────────────────

    #[test]
    fn parse_hkt_application_juxtaposition() {
        // `m a` → App(Var("m", Star→Star), Var("a", Star))
        // Both vars default to Star at parse time; kind tracking happens in checker
        let ty = parse_type("m a").unwrap();
        assert!(matches!(&ty, Type::App(_, _)), "expected App, got {ty:?}");
    }

    #[test]
    fn parse_hkt_application_two_args() {
        // `f a b` → App(App(f, a), b)
        let ty = parse_type("f a b").unwrap();
        assert!(matches!(&ty, Type::App(_, _)));
        if let Type::App(head, _) = &ty {
            assert!(matches!(head.as_ref(), Type::App(_, _)));
        }
    }

    #[test]
    fn parse_hkt_application_in_arrow() {
        // `m a -> m b` should parse with application binding tighter than arrow
        let ty = parse_type("m a -> m b").unwrap();
        assert!(
            matches!(&ty, Type::Function(_, _)),
            "expected Function, got {ty:?}"
        );
        if let Type::Function(lhs, rhs) = &ty {
            assert!(matches!(lhs.as_ref(), Type::App(_, _)));
            assert!(matches!(rhs.as_ref(), Type::App(_, _)));
        }
    }

    #[test]
    fn parse_forall_simple() {
        use super::super::types::Kind;
        // `forall a. a`
        let ty = parse_type("forall a. a").unwrap();
        assert!(
            matches!(&ty, Type::Forall(binders, _) if binders.len() == 1),
            "expected Forall, got {ty:?}"
        );
        if let Type::Forall(binders, _) = &ty {
            assert_eq!(binders[0].0, TypeVarId("a".to_string()));
            assert_eq!(binders[0].1, Kind::Star);
        }
    }

    #[test]
    fn parse_forall_multiple_vars() {
        // `forall a b. a -> b`
        let ty = parse_type("forall a b. a -> b").unwrap();
        if let Type::Forall(binders, body) = &ty {
            assert_eq!(binders.len(), 2);
            assert!(matches!(body.as_ref(), Type::Function(_, _)));
        } else {
            panic!("expected Forall, got {ty:?}");
        }
    }

    #[test]
    fn parse_forall_with_kind_annotation() {
        use super::super::types::Kind;
        // `forall (m :: * -> *) a. m a`
        let ty = parse_type("forall (m :: * -> *) a. m a").unwrap();
        if let Type::Forall(binders, body) = &ty {
            assert_eq!(binders.len(), 2);
            assert_eq!(binders[0].0, TypeVarId("m".to_string()));
            assert_eq!(
                binders[0].1,
                Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star))
            );
            assert_eq!(binders[1].1, Kind::Star);
            assert!(matches!(body.as_ref(), Type::App(_, _)));
        } else {
            panic!("expected Forall, got {ty:?}");
        }
    }

    #[test]
    fn parse_forall_star_to_star_to_star() {
        use super::super::types::Kind;
        // `forall (f :: * -> * -> *) a b. f a b`
        let ty = parse_type("forall (f :: * -> * -> *) a b. f a b").unwrap();
        if let Type::Forall(binders, _) = &ty {
            assert_eq!(binders.len(), 3);
            let expected_kind = Kind::Arrow(
                Box::new(Kind::Star),
                Box::new(Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star))),
            );
            assert_eq!(binders[0].1, expected_kind);
        } else {
            panic!("expected Forall");
        }
    }

    #[test]
    fn parse_forall_monad_map_type() {
        // `forall (m :: * -> *) a b. (a -> b) -> m a -> m b`
        let ty = parse_type("forall (m :: * -> *) a b. (a -> b) -> m a -> m b").unwrap();
        assert!(
            matches!(&ty, Type::Forall(_, _)),
            "expected Forall, got {ty:?}"
        );
    }

    #[test]
    fn roundtrip_hkt_application() {
        roundtrip("m a");
        roundtrip("m a b");
    }

    #[test]
    fn roundtrip_forall() {
        roundtrip("forall a. a");
        roundtrip("forall a b. a -> b");
        roundtrip("forall (m :: * -> *) a. m a -> m a");
    }

    #[test]
    fn parse_dict_type() {
        assert_eq!(
            parse_type("Dict(number)").unwrap(),
            Type::dict(Type::Number)
        );
    }

    #[test]
    fn parse_dict_with_var() {
        assert_eq!(parse_type("Dict(a)").unwrap(), Type::dict(var("a")));
    }

    #[test]
    fn parse_dict_in_function() {
        // symbol -> Dict(a) -> a
        let ty = parse_type("symbol -> Dict(a) -> a").unwrap();
        assert!(matches!(ty, Type::Function(_, _)));
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

    // ── Constraint parsing (B2) ─────────────────────────────────────────────

    #[test]
    fn parse_scheme_no_constraints() {
        let (ty, cs) = super::parse_scheme("number -> number").unwrap();
        assert_eq!(
            ty,
            Type::Function(Box::new(Type::Number), Box::new(Type::Number))
        );
        assert!(cs.is_empty());
    }

    #[test]
    fn parse_scheme_single_constraint() {
        let (ty, cs) = super::parse_scheme("<(a, a) => a -> a -> a").unwrap();
        assert!(matches!(ty, Type::Function(_, _)));
        assert_eq!(cs.len(), 1);
        assert_eq!(cs[0].function, "<");
        assert_eq!(cs[0].args.len(), 2);
        assert_eq!(cs[0].args[0], var("a"));
        assert_eq!(cs[0].args[1], var("a"));
    }

    #[test]
    fn parse_scheme_two_constraints() {
        let (ty, cs) = super::parse_scheme("<(a, a), +(a, a) => a -> a -> a").unwrap();
        assert!(matches!(ty, Type::Function(_, _)));
        assert_eq!(cs.len(), 2);
        assert_eq!(cs[0].function, "<");
        assert_eq!(cs[1].function, "+");
    }

    #[test]
    fn parse_scheme_dotted_operator_name() {
        let (ty, cs) = super::parse_scheme("str.of(a) => [a] -> [string]").unwrap();
        assert!(matches!(ty, Type::Function(_, _)));
        assert_eq!(cs.len(), 1);
        assert_eq!(cs[0].function, "str.of");
        assert_eq!(cs[0].args[0], var("a"));
    }

    #[test]
    fn parse_scheme_unicode_arrow_body() {
        // The `=>` separator and the `→` body arrow must not interfere.
        let (ty, cs) = super::parse_scheme(">(a, a) => a → a → a").unwrap();
        assert!(matches!(ty, Type::Function(_, _)));
        assert_eq!(cs.len(), 1);
        assert_eq!(cs[0].function, ">");
    }

    #[test]
    fn parse_scheme_arrow_inside_parens_not_fat_arrow() {
        // `=>` inside parentheses should NOT be treated as the constraint separator.
        // This annotation has no constraints — just a plain function type.
        let (ty, cs) = super::parse_scheme("number -> number").unwrap();
        assert!(matches!(ty, Type::Function(_, _)));
        assert!(cs.is_empty());
    }

    // ── render_scheme roundtrip tests ─────────────────────────────────────────

    #[test]
    fn render_scheme_no_constraints() {
        let (ty, cs) = super::parse_scheme("number -> number").unwrap();
        let rendered = super::render_scheme(&ty, &cs);
        assert_eq!(rendered, "number → number");
    }

    #[test]
    fn render_scheme_single_constraint() {
        let (ty, cs) = super::parse_scheme(">(a, a) => a -> a -> a").unwrap();
        let rendered = super::render_scheme(&ty, &cs);
        assert_eq!(rendered, ">(a, a) => a → a → a");
    }

    #[test]
    fn render_scheme_two_constraints() {
        let (ty, cs) = super::parse_scheme("<(a, a), +(a, a) => a -> a -> a").unwrap();
        let rendered = super::render_scheme(&ty, &cs);
        assert_eq!(rendered, "<(a, a), +(a, a) => a → a → a");
    }

    #[test]
    fn render_scheme_roundtrip_forall_kind() {
        let (ty, cs) =
            super::parse_scheme("forall (m :: * -> *) a b. (a -> b) -> m a -> m b").unwrap();
        let rendered = super::render_scheme(&ty, &cs);
        assert_eq!(rendered, "forall (m :: * → *) a b. (a → b) → m a → m b");
    }
}
