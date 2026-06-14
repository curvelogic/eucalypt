//! Rowan parser

use std::marker::PhantomData;
use std::mem::swap;

use super::brackets;
use super::kind::SyntaxKind::{self, *};

use super::lex::Lexer;
use super::{ast, Parse, ParseError};

use rowan::{GreenNode, GreenNodeBuilder};
use rowan::{TextRange, TextSize};

/// Parse generates a sequence of events, which read alongside the
/// lexer's token sequence can be used to create a GreenNode.
#[derive(Debug)]
enum ParseEvent {
    StartNode(SyntaxKind),
    Finish,
    Token(SyntaxKind),
}

/// Parser to consume lexer and product a green tree
pub struct Parser<'text> {
    /// tokens
    tokens: Vec<(SyntaxKind, &'text str)>,
    /// index into tokens
    next_token: usize,
    /// stack of event sinks to capture events into
    sink_stack: Vec<Box<dyn EventSink>>,
    /// accumulated errors
    errors: Vec<ParseError>,
}

impl<'text> Parser<'text> {
    /// Construct directly from text.
    ///
    /// A pre-scan of the token stream is performed to populate the bracket
    /// registry before the main parse begins.
    pub fn new(text: &'text str) -> Self {
        let tokens: Vec<_> = Lexer::from_text(text)
            .map(|(tok, span)| (tok, &text[(span.start().into())..span.end().into()]))
            .collect();

        Parser {
            tokens,
            next_token: 0,
            sink_stack: vec![Box::new(SimpleEventSink::default())],
            errors: vec![],
        }
    }

    /// Parse a unit, including top level metadata and all declarations
    pub fn parse_unit(&mut self) -> Parse<ast::Unit> {
        self.sink().start_node(UNIT);
        self.add_trivia();
        self.parse_block_content();
        self.add_trivia();
        self.sink().finish_node();

        // anything else is an error

        Parse {
            green_node: self.build(),
            errors: self.errors.split_off(0),
            _ty: PhantomData,
        }
    }

    /// Parse a single expression (as might be passed to `-e`)
    pub fn parse_expression(&mut self) -> Parse<ast::Soup> {
        self.parse_soup();
        Parse {
            green_node: self.build(),
            errors: self.errors.split_off(0),
            _ty: PhantomData,
        }
    }

    /// Calculate the text range of the next token for when reporting errors.
    fn next_range(&self) -> TextRange {
        let mut ch: TextSize = 0.into();
        for (_, s) in &self.tokens[0..self.next_token] {
            ch += TextSize::of(*s);
        }
        TextRange::at(ch, TextSize::of(self.tokens[self.next_token].1))
    }

    /// Calculate a text range covering the last consumed token and the next token.
    /// Used for reporting errors that span two adjacent tokens (e.g. `::` is two COLON tokens).
    fn prev_and_next_range(&self) -> TextRange {
        let mut ch: TextSize = 0.into();
        let start_token = self.next_token.saturating_sub(1);
        for (_, s) in &self.tokens[0..start_token] {
            ch += TextSize::of(*s);
        }
        let start = ch;
        // span from prev token start to end of next token
        ch += TextSize::of(self.tokens[start_token].1);
        if self.next_token < self.tokens.len() {
            ch += TextSize::of(self.tokens[self.next_token].1);
        }
        TextRange::new(start, ch)
    }

    /// Calculate the start offset of the token at the given index.
    fn token_start_at(&self, idx: usize) -> TextSize {
        let mut ch: TextSize = 0.into();
        for (_, s) in &self.tokens[0..idx] {
            ch += TextSize::of(*s);
        }
        ch
    }

    /// Return the byte length of the most recently consumed token.
    ///
    /// Panics if no token has been consumed yet (`next_token == 0`).
    fn prev_token_len(&self) -> TextSize {
        debug_assert!(self.next_token > 0, "no token has been consumed yet");
        TextSize::of(self.tokens[self.next_token - 1].1)
    }

    /// Operate on the event sink at the top of the stack
    fn sink(&mut self) -> &mut dyn EventSink {
        self.sink_stack.last_mut().unwrap().as_mut()
    }

    /// Pop and complete the top event sink, passing its events and the total
    /// byte length of those events' tokens to the caller.
    ///
    /// Also merges any sink-generated errors into `self.errors`.
    fn finish_sink(&mut self) -> (Vec<ParseEvent>, TextSize) {
        if let Some(mut sink) = self.sink_stack.pop() {
            let (events, mut errs) = sink.as_mut().complete();
            self.errors.append(&mut errs);
            // Compute the total byte length of all tokens in these events by
            // counting how many Token events exist and summing their text
            // lengths from self.tokens.
            //
            // The tokens in `events` correspond to a contiguous run of tokens
            // ending at `self.next_token - 1` (unless `push_back` was called).
            // Count Token events to find how far back to look.
            let token_count = events
                .iter()
                .filter(|e| matches!(e, ParseEvent::Token(_)))
                .count();
            let start_idx = self.next_token.saturating_sub(token_count);
            let total_len: TextSize = self.tokens[start_idx..self.next_token]
                .iter()
                .map(|(_, s)| TextSize::of(*s))
                .sum();
            (events, total_len)
        } else {
            unreachable!()
        }
    }

    /// Convert the parse events into a GreenNode
    fn build(&mut self) -> GreenNode {
        let (mut events, _) = self.finish_sink();
        let mut surplus_events = self.read_surplus();

        if !surplus_events.is_empty() {
            // tuck any surplus events inside the top level node in a
            // stowaways wrapper
            let last = events.pop().unwrap();
            events.append(&mut surplus_events);
            events.push(last);
        }

        // check that the number of tokens in the event stream is
        // correct
        let mut event_tokens = vec![];
        for e in &events {
            if let ParseEvent::Token(k) = e {
                event_tokens.push(k);
            }
        }
        assert_eq!(
            event_tokens,
            self.tokens
                .iter()
                .map(|(k, _)| k)
                .collect::<Vec<&SyntaxKind>>()
        );

        let mut builder = GreenNodeBuilder::default();
        let mut idx = 0;
        for e in &events {
            match e {
                ParseEvent::StartNode(k) => builder.start_node((*k).into()),
                ParseEvent::Finish => builder.finish_node(),
                ParseEvent::Token(k) => {
                    builder.token((*k).into(), self.tokens[idx].1);
                    idx += 1;
                }
            }
        }
        builder.finish()
    }

    /// Try to parse literal, pushing token back into tokens on failure
    fn try_parse_literal(&mut self) -> bool {
        if let Some((k, text)) = self.next() {
            if k.is_literal_terminal()
                || k == STRING_PATTERN_START
                || k == C_STRING_PATTERN_START
                || k == RAW_STRING_PATTERN_START
            {
                let start_len = TextSize::of(text);
                match k {
                    STRING_PATTERN_START => {
                        // This is a string pattern, parse it specially
                        self.parse_string_pattern(
                            STRING_PATTERN,
                            STRING_PATTERN_START,
                            STRING_PATTERN_END,
                            start_len,
                        );
                        true
                    }
                    C_STRING_PATTERN_START => {
                        // This is a c-string pattern, parse it specially
                        self.parse_string_pattern(
                            C_STRING_PATTERN,
                            C_STRING_PATTERN_START,
                            C_STRING_PATTERN_END,
                            start_len,
                        );
                        true
                    }
                    RAW_STRING_PATTERN_START => {
                        // This is a raw string pattern, parse it specially
                        self.parse_string_pattern(
                            RAW_STRING_PATTERN,
                            RAW_STRING_PATTERN_START,
                            RAW_STRING_PATTERN_END,
                            start_len,
                        );
                        true
                    }
                    _ => {
                        // Regular literal
                        self.sink().start_node(LITERAL);
                        self.sink().token(k, start_len);
                        self.sink().finish_node();
                        true
                    }
                }
            } else {
                self.push_back();
                false
            }
        } else {
            false
        }
    }

    /// Try to parse name, pushing token back into tokens failure
    fn try_parse_name(&mut self) -> bool {
        if let Some((k, text)) = self.next() {
            if k.is_name_terminal() {
                self.sink().start_node(NAME);
                self.sink().token(k, TextSize::of(text));
                self.sink().finish_node();
                true
            } else {
                self.push_back();
                false
            }
        } else {
            false
        }
    }

    /// An atom is a name or a literal
    fn try_parse_atom(&mut self) -> bool {
        self.try_parse_literal() || self.try_parse_name()
    }

    /// Parse a self contained element
    fn try_parse_element(&mut self) -> bool {
        match self.peek() {
            Some((OPEN_PAREN, _)) => {
                self.parse_paren_expression();
                true
            }
            Some((OPEN_PAREN_APPLY, _)) => {
                self.parse_apply_tuple();
                true
            }
            Some((OPEN_SQUARE, _)) => self.try_parse_list_expression(),
            Some((OPEN_BRACE, _)) => {
                self.parse_block_expression();
                true
            }
            Some((OPEN_BRACE_APPLY, _)) => {
                // f{x: 1} — juxtaposed block call: sugar for f({x: 1})
                self.parse_block_apply_tuple();
                true
            }
            Some((OPEN_SQUARE_APPLY, _)) => {
                // f[1, 2] — juxtaposed list call: sugar for f([1, 2])
                self.parse_list_apply_tuple();
                true
            }
            Some((BRACKET_OPEN, _)) => {
                self.parse_bracket_expression();
                true
            }
            Some((RESERVED_OPEN, _)) => {
                self.parse_reserved_paren_expression();
                true
            }
            Some((_k, _s)) => self.try_parse_atom(),
            None => false,
        }
    }

    /// Parse a list expression `[x, y, z]` or a cons pattern `[h : t]`.
    ///
    /// A cons pattern has exactly one soup item before a `:` token (with no
    /// preceding comma), and one soup item after the `:`.  Everything else is
    /// a normal comma-separated list.
    fn try_parse_list_expression(&mut self) -> bool {
        if let Some((k, text)) = self.next() {
            if k == OPEN_SQUARE {
                // Speculatively parse the first soup item, then decide
                // whether this is a cons pattern or a normal list.
                self.sink().start_node(LIST);
                self.sink().token(k, TextSize::of(text));
                self.add_trivia();
                while self.try_parse_soup() {
                    if self.try_accept(COMMA) {
                        self.add_trivia();
                    } else if self.try_accept(COLON) {
                        // Head/tail separator in list pattern: [heads... : tail]
                        // Emit the colon token and parse exactly one more soup.
                        self.add_trivia();
                        self.try_parse_soup();
                        break;
                    } else {
                        break;
                    }
                }
                self.add_trivia();
                if !self.expect(CLOSE_SQUARE) {
                    // Recovery: consume to matching ']' or a block boundary.
                    self.recover_to(
                        CLOSE_SQUARE,
                        &[CLOSE_BRACE, CLOSE_PAREN, RESERVED_CLOSE, BRACKET_CLOSE],
                        OPEN_SQUARE,
                    );
                }
                self.sink().finish_node();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Assuming all trivia has been passed, try parsing soup
    /// (requiring at least one element), returning None otherwise
    fn try_parse_soup(&mut self) -> bool {
        if let Some((k, _)) = self.peek() {
            if matches!(
                k,
                CLOSE_BRACE | CLOSE_PAREN | CLOSE_SQUARE | RESERVED_CLOSE | BRACKET_CLOSE | COMMA
            ) {
                return false;
            }
        }

        self.sink().start_node(SOUP);
        while self.try_parse_element() {
            self.add_trivia();
        }
        self.add_trivia();
        self.sink().finish_node();
        true
    }

    fn parse_soup(&mut self) {
        self.sink().start_node(SOUP);
        loop {
            match self.next() {
                Some((WHITESPACE, text)) => {
                    self.sink().token(WHITESPACE, TextSize::of(text));
                }
                Some((COMMENT, text)) => {
                    self.sink().token(COMMENT, TextSize::of(text));
                }
                Some((_k, _s)) => {
                    self.push_back();
                    if !self.try_parse_element() {
                        break;
                    }
                }
                None => break,
            }
        }
        self.add_trivia();
        self.sink().finish_node();
    }

    /// Parse a paren expression: (x y z)
    fn parse_paren_expression(&mut self) {
        self.sink().start_node(PAREN_EXPR);
        self.expect(OPEN_PAREN);
        self.add_trivia();

        self.parse_soup();
        if !self.expect(CLOSE_PAREN) {
            // Recovery: consume to matching ')' or a block/list boundary.
            self.recover_to(
                CLOSE_PAREN,
                &[CLOSE_BRACE, CLOSE_SQUARE, RESERVED_CLOSE, BRACKET_CLOSE],
                OPEN_PAREN,
            );
        }
        self.sink().finish_node();
    }

    /// A paren expression using illegal open / close characters.
    ///
    /// Not valid (and exotic brackets are reserved for future use)
    /// but we'll treat as a paren expression for error handling / LSP
    /// use cases
    fn parse_reserved_paren_expression(&mut self) {
        self.sink().start_node(PAREN_EXPR);
        self.expect(RESERVED_OPEN);
        self.add_trivia();

        self.parse_soup();
        self.expect(RESERVED_CLOSE);
        self.sink().finish_node();
    }

    /// Return `true` if the bracket content starting at `idx` (the token
    /// immediately after `BRACKET_OPEN`) contains a `COLON` token at
    /// nesting depth zero before the matching `BRACKET_CLOSE`.
    ///
    /// Depth is tracked across all bracket-like delimiters:
    /// `()`, `[]`, `{}`, `⟦⟧` (and reserved parens).  An empty bracket
    /// pair or one with no top-level colons returns `false` (soup mode).
    ///
    /// This is the **colon heuristic**: bracket content with top-level
    /// colons is declarations (block mode); without, it is a soup expression.
    fn bracket_content_has_top_level_colon(&self) -> bool {
        let mut idx = self.next_token;

        // Consume the BRACKET_OPEN itself.
        if idx >= self.tokens.len() || self.tokens[idx].0 != BRACKET_OPEN {
            return false;
        }
        idx += 1;

        let mut depth: usize = 0;
        while idx < self.tokens.len() {
            let kind = self.tokens[idx].0;
            idx += 1;
            match kind {
                WHITESPACE | COMMENT => continue,
                // Depth-increasing openers
                OPEN_PAREN | OPEN_PAREN_APPLY | OPEN_BRACE | OPEN_BRACE_APPLY | OPEN_SQUARE
                | OPEN_SQUARE_APPLY | BRACKET_OPEN | RESERVED_OPEN => depth += 1,
                // Depth-decreasing closers
                CLOSE_PAREN | CLOSE_BRACE | CLOSE_SQUARE | RESERVED_CLOSE => {
                    if depth == 0 {
                        // Mismatched close — give up
                        return false;
                    }
                    depth -= 1;
                }
                BRACKET_CLOSE => {
                    if depth == 0 {
                        // Reached the matching close: no top-level colon found.
                        return false;
                    }
                    depth -= 1;
                }
                COLON if depth == 0 => return true,
                _ => {}
            }
        }
        false
    }

    /// Parse a bracket expression using a Unicode idiot bracket pair.
    ///
    /// For example: `⟦ x ⟧` or `⌈ x ⌉`.
    ///
    /// Block vs soup mode is determined by the **colon heuristic**: if the
    /// bracket content contains a `COLON` token at the top level (nesting
    /// depth zero inside the brackets), the content is parsed as block
    /// declarations (`BRACKET_BLOCK`).  Otherwise it is parsed as a soup
    /// expression (`BRACKET_EXPR`).  An empty bracket pair is soup mode.
    ///
    /// This replaces the earlier `BracketRegistry` pre-scan and makes the
    /// parser self-contained: no cross-file seeding of bracket modes is
    /// required.
    fn parse_bracket_expression(&mut self) {
        let open_char = if let Some((BRACKET_OPEN, open_text)) = self.peek() {
            open_text.chars().next()
        } else {
            None
        };

        let is_block_mode = self.bracket_content_has_top_level_colon();

        if is_block_mode {
            self.sink().start_node(BRACKET_BLOCK);
            self.expect(BRACKET_OPEN);
            self.add_trivia();
            self.parse_block_content_until_bracket_close();
            self.add_trivia();
            if !self.try_accept(BRACKET_CLOSE) {
                self.errors.push(ParseError::UnclosedBracketExpr {
                    range: self.next_range(),
                });
            }
            self.sink().finish_node();
        } else {
            self.sink().start_node(BRACKET_EXPR);
            // Determine expected close char for validation (deferred to validate.rs)
            let expected_close = open_char.and_then(brackets::close_for_open);
            self.expect(BRACKET_OPEN);
            self.add_trivia();
            self.parse_soup();
            if !self.try_accept(BRACKET_CLOSE) {
                self.errors.push(ParseError::UnclosedBracketExpr {
                    range: self.next_range(),
                });
            } else if let Some(expected) = expected_close {
                let _ = expected; // validation deferred to validate.rs
            }
            self.sink().finish_node();
        }
    }

    /// Parse block content terminated by BRACKET_CLOSE instead of CLOSE_BRACE.
    fn parse_block_content_until_bracket_close(&mut self) {
        let start_offset = self.token_start_at(self.next_token);
        self.sink_stack
            .push(Box::new(BlockEventSink::with_start(start_offset)));
        while self.parse_protoblock_element_until_bracket_close() {}
        let (events, len) = self.finish_sink();
        self.sink().accept(events, len);
    }

    /// Like `parse_protoblock_element` but stops at BRACKET_CLOSE instead of CLOSE_BRACE.
    fn parse_protoblock_element_until_bracket_close(&mut self) -> bool {
        match self.next() {
            Some((COLON, _)) => {
                let first_colon_len = self.prev_token_len();
                if let Some((COLON, _)) = self.peek() {
                    let range = self.prev_and_next_range();
                    self.next();
                    let second_colon_len = self.prev_token_len();
                    self.errors.push(ParseError::InvalidDoubleColon { range });
                    self.sink().start_node(ERROR_STOWAWAYS);
                    self.sink().token(COLON, first_colon_len);
                    self.sink().token(COLON, second_colon_len);
                    self.sink().finish_node();
                } else {
                    self.sink().token(COLON, first_colon_len);
                }
                self.add_trivia();
                true
            }
            Some((k, text)) if k == BACKTICK || k == COMMA || k == WHITESPACE || k == COMMENT => {
                let len = TextSize::of(text);
                self.sink().token(k, len);
                self.add_trivia();
                true
            }
            Some((BRACKET_CLOSE, _)) => {
                self.push_back();
                false
            }
            Some(_) => {
                self.push_back();
                self.try_parse_element()
            }
            None => false,
        }
    }

    /// Parse an apply tuple [...](x, y, z)
    fn parse_apply_tuple(&mut self) {
        self.sink().start_node(ARG_TUPLE);
        self.expect(OPEN_PAREN_APPLY);
        self.add_trivia();

        while self.try_parse_soup() {
            if !self.try_accept(COMMA) {
                break;
            }
            self.add_trivia();
        }
        self.add_trivia();

        if !self.expect(CLOSE_PAREN) {
            // Error recovery: consume unexpected tokens up to and including the
            // closing paren. Without this, stray tokens (e.g. a bare colon in
            // `f(2, 2:)`) escape into the block parser, which misinterprets them
            // as declaration separators and drops buffered events, causing a
            // token-accounting assertion failure.
            while let Some((k, _)) = self.next() {
                if k == CLOSE_PAREN {
                    let len = self.prev_token_len();
                    self.sink().token(CLOSE_PAREN, len);
                    break;
                }
                // Don't cross block or list boundaries.
                if matches!(
                    k,
                    CLOSE_BRACE | CLOSE_SQUARE | RESERVED_CLOSE | BRACKET_CLOSE
                ) {
                    self.push_back();
                    break;
                }
                let tok_len = self.prev_token_len();
                self.sink().start_node(ERROR_STOWAWAYS);
                self.sink().token(k, tok_len);
                self.sink().finish_node();
            }
        }

        self.sink().finish_node();
    }

    /// Parse a juxtaposed block call `f{x: 1}` as an `ARG_TUPLE` containing a single block.
    ///
    /// This desugars `f{x: 1}` to the same AST as `f({x: 1})`.
    fn parse_block_apply_tuple(&mut self) {
        self.sink().start_node(ARG_TUPLE);
        self.sink().start_node(SOUP);
        self.sink().start_node(BLOCK);
        // Consume the OPEN_BRACE_APPLY token as-is.
        self.expect(OPEN_BRACE_APPLY);
        self.add_trivia();
        self.parse_block_content();
        self.add_trivia();
        self.expect(CLOSE_BRACE);
        self.sink().finish_node(); // BLOCK
        self.sink().finish_node(); // SOUP
        self.sink().finish_node(); // ARG_TUPLE
    }

    /// Parse a juxtaposed list call `f[1, 2]` as an `ARG_TUPLE` containing a single list.
    ///
    /// This desugars `f[1, 2]` to the same AST as `f([1, 2])`.
    fn parse_list_apply_tuple(&mut self) {
        self.sink().start_node(ARG_TUPLE);
        self.sink().start_node(SOUP);
        self.sink().start_node(LIST);
        // Consume the OPEN_SQUARE_APPLY token as-is.
        self.expect(OPEN_SQUARE_APPLY);
        self.add_trivia();
        while self.try_parse_soup() {
            if self.try_accept(COMMA) {
                self.add_trivia();
            } else if self.try_accept(COLON) {
                // Head/tail separator inside a list apply argument
                self.add_trivia();
                self.try_parse_soup();
                break;
            } else {
                break;
            }
        }
        self.add_trivia();
        self.expect(CLOSE_SQUARE);
        self.sink().finish_node(); // LIST
        self.sink().finish_node(); // SOUP
        self.sink().finish_node(); // ARG_TUPLE
    }

    /// Parse a block expression (next token is '{')
    fn parse_block_expression(&mut self) {
        self.sink().start_node(BLOCK);
        self.expect(OPEN_BRACE);
        self.add_trivia();
        self.parse_block_content();
        self.add_trivia();
        if !self.expect(CLOSE_BRACE) {
            // Recovery: consume to matching '}' or EOF, wrapping stray tokens.
            self.recover_to(
                CLOSE_BRACE,
                &[CLOSE_PAREN, CLOSE_SQUARE, RESERVED_CLOSE, BRACKET_CLOSE],
                OPEN_BRACE,
            );
        }
        self.sink().finish_node();
    }

    /// Parse content of block or unit
    fn parse_block_content(&mut self) {
        // Pass the starting byte offset so the BlockEventSink can compute
        // accurate source positions for declaration-level errors.
        let start_offset = self.token_start_at(self.next_token);
        self.sink_stack
            .push(Box::new(BlockEventSink::with_start(start_offset)));
        while self.parse_protoblock_element() {}
        let (events, len) = self.finish_sink();
        self.sink().accept(events, len);
    }

    /// Parse a temporary protoblock element
    fn parse_protoblock_element(&mut self) -> bool {
        match self.next() {
            Some((COLON, _)) => {
                // Check for '::' — this is not valid eucalypt syntax and, if both
                // COLON tokens reach the BlockEventSink unguarded, causes a panic in
                // token accounting. Catch it here: wrap both tokens in an ERROR node so
                // the BlockEventSink treats them as ordinary buffered content rather
                // than declaration separators.
                let first_colon_len = self.prev_token_len();
                if let Some((COLON, _)) = self.peek() {
                    let range = self.prev_and_next_range();
                    self.next(); // consume second COLON
                    let second_colon_len = self.prev_token_len();
                    self.errors.push(ParseError::InvalidDoubleColon { range });
                    self.sink().start_node(ERROR_STOWAWAYS);
                    self.sink().token(COLON, first_colon_len);
                    self.sink().token(COLON, second_colon_len);
                    self.sink().finish_node();
                } else {
                    self.sink().token(COLON, first_colon_len);
                }
                self.add_trivia();
                true
            }
            Some((k, text)) if k == BACKTICK || k == COMMA || k == WHITESPACE || k == COMMENT => {
                let len = TextSize::of(text);
                self.sink().token(k, len);
                self.add_trivia();
                true
            }
            Some((CLOSE_BRACE, _)) => {
                self.push_back();
                false
            }
            Some(_) => {
                self.push_back();
                self.try_parse_element()
            }
            None => false,
        }
    }

    /// Gather up any unparsed tokens for insertion as stowaways
    fn read_surplus(&mut self) -> Vec<ParseEvent> {
        let mut surplus_events = vec![];
        while let Some((token, _)) = self.next() {
            if surplus_events.is_empty() {
                surplus_events.push(ParseEvent::StartNode(ERROR_STOWAWAYS));
            }
            surplus_events.push(ParseEvent::Token(token));
        }
        if !surplus_events.is_empty() {
            surplus_events.push(ParseEvent::Finish);
        }
        surplus_events
    }

    /// Expects and adds as leaf a particular token, otherwise recording
    /// an `UnexpectedToken` error and returning false
    fn expect(&mut self, kind: SyntaxKind) -> bool {
        if let Some((k, text)) = self.next() {
            if k == kind {
                let len = TextSize::of(text);
                self.sink().token(k, len);
                true
            } else {
                self.push_back();
                self.errors.push(ParseError::UnexpectedToken {
                    expected: kind,
                    actual: k,
                    range: self.next_range(),
                });
                false
            }
        } else {
            false
        }
    }

    /// Consume tokens, wrapping them in `ERROR_STOWAWAYS`, until the given
    /// closing delimiter is found (consuming it) or a higher-level boundary
    /// (`stop_at`) is encountered (pushing it back).
    ///
    /// Nesting is tracked so inner delimiter pairs are consumed as a unit.
    /// Used for expression-level error recovery on missing closing delimiters.
    fn recover_to(&mut self, close: SyntaxKind, stop_at: &[SyntaxKind], open: SyntaxKind) {
        let mut depth: usize = 0;
        let mut has_stowaways = false;

        loop {
            match self.peek() {
                None => break,
                Some((k, text)) if k == close && depth == 0 => {
                    // Found the matching close delimiter — consume it.
                    let len = TextSize::of(text);
                    self.next();
                    if has_stowaways {
                        self.sink().finish_node();
                    }
                    self.sink().token(close, len);
                    return;
                }
                Some((k, _)) if stop_at.contains(&k) => {
                    // Higher-level boundary: leave it for the parent parser.
                    break;
                }
                Some((k, _)) => {
                    // Track nesting.
                    let is_open = matches!(
                        k,
                        OPEN_BRACE
                            | OPEN_BRACE_APPLY
                            | OPEN_SQUARE
                            | OPEN_SQUARE_APPLY
                            | OPEN_PAREN
                            | OPEN_PAREN_APPLY
                            | BRACKET_OPEN
                            | RESERVED_OPEN
                    );
                    let is_close = matches!(
                        k,
                        CLOSE_BRACE | CLOSE_SQUARE | CLOSE_PAREN | BRACKET_CLOSE | RESERVED_CLOSE
                    );
                    if k == open || is_open {
                        depth += 1;
                    } else if is_close && depth > 0 {
                        depth -= 1;
                    }

                    if !has_stowaways {
                        self.sink().start_node(ERROR_STOWAWAYS);
                        has_stowaways = true;
                    }
                    self.next();
                    let tok_len = self.prev_token_len();
                    self.sink().token(k, tok_len);
                }
            }
        }

        if has_stowaways {
            self.sink().finish_node();
        }
    }

    fn try_accept(&mut self, kind: SyntaxKind) -> bool {
        if let Some((k, text)) = self.next() {
            if k == kind {
                let len = TextSize::of(text);
                self.sink().token(k, len);
                true
            } else {
                self.push_back();
                false
            }
        } else {
            false
        }
    }

    /// Back up one token
    fn push_back(&mut self) {
        if self.next_token > 0 {
            self.next_token -= 1
        } else {
            panic!("reversing beyond start of stream")
        }
    }

    fn peek(&self) -> Option<(SyntaxKind, &'text str)> {
        self.tokens.get(self.next_token).cloned()
    }

    /// Take the next token from tokens
    fn next(&mut self) -> Option<(SyntaxKind, &'text str)> {
        let ret = self.tokens.get(self.next_token);
        if ret.is_some() {
            self.next_token += 1;
        }
        ret.cloned()
    }

    fn add_trivia(&mut self) {
        while let Some((k, text)) = self.peek() {
            if k.is_trivial() {
                let len = TextSize::of(text);
                self.next();
                self.sink().token(k, len);
            } else {
                break;
            }
        }
    }

    /// Parse a string pattern with interpolation
    ///
    /// The pattern_kind, start_kind, and end_kind parameters allow this method
    /// to work for plain strings, c-strings, and r-strings.
    ///
    /// `start_len` is the byte length of the opening delimiter token, which
    /// was already consumed by the caller before this method is entered.
    fn parse_string_pattern(
        &mut self,
        pattern_kind: SyntaxKind,
        start_kind: SyntaxKind,
        end_kind: SyntaxKind,
        start_len: TextSize,
    ) {
        self.sink().start_node(pattern_kind);
        self.sink().token(start_kind, start_len); // emit already-consumed opening quote

        // Process tokens until we reach the end kind
        while let Some((kind, text)) = self.peek() {
            let tok_len = TextSize::of(text);
            if kind == end_kind {
                self.next(); // consume closing quote
                self.sink().token(end_kind, tok_len);
                break;
            }

            match kind {
                STRING_LITERAL_CONTENT => {
                    self.next();
                    self.sink().start_node(STRING_LITERAL_CONTENT);
                    self.sink().token(STRING_LITERAL_CONTENT, tok_len);
                    self.sink().finish_node();
                }
                STRING_ESCAPED_OPEN => {
                    self.next();
                    self.sink().start_node(STRING_ESCAPED_OPEN);
                    self.sink().token(STRING_ESCAPED_OPEN, tok_len);
                    self.sink().finish_node();
                }
                STRING_ESCAPED_CLOSE => {
                    self.next();
                    self.sink().start_node(STRING_ESCAPED_CLOSE);
                    self.sink().token(STRING_ESCAPED_CLOSE, tok_len);
                    self.sink().finish_node();
                }
                OPEN_BRACE => {
                    // Start of interpolation — record the token index before consuming
                    let open_brace_idx = self.next_token;
                    self.sink().start_node(STRING_INTERPOLATION);
                    self.next(); // consume {
                    self.sink().token(OPEN_BRACE, tok_len);

                    // Parse interpolation content
                    self.parse_string_interpolation_content();

                    // Consume closing brace
                    if let Some((CLOSE_BRACE, close_text)) = self.peek() {
                        let close_len = TextSize::of(close_text);
                        self.next();
                        self.sink().token(CLOSE_BRACE, close_len);
                    } else {
                        // No closing brace immediately — consume any stray
                        // interpolation tokens that the content parser did not
                        // consume.  We stop if we encounter a closing brace (which
                        // means the format spec or dotted path was merely malformed,
                        // not truly unclosed) or the end of the enclosing string.
                        let mut found_close = false;
                        while let Some((k, stray_text)) = self.peek() {
                            let stray_len = TextSize::of(stray_text);
                            if k == CLOSE_BRACE {
                                // A closing brace is present; consume it and stop.
                                // This handles malformed-but-closed interpolations
                                // such as `{x:.2f}` where the parser cannot parse
                                // the full format spec.
                                self.next();
                                self.sink().token(CLOSE_BRACE, stray_len);
                                found_close = true;
                                break;
                            }
                            if k == STRING_LITERAL_CONTENT
                                || k == STRING_PATTERN_END
                                || k == C_STRING_PATTERN_END
                                || k == RAW_STRING_PATTERN_END
                                || k == end_kind
                            {
                                // Stop before the end of the string — these
                                // will be consumed by the outer loop.
                                break;
                            }
                            self.next();
                            self.sink().token(k, stray_len);
                        }
                        if !found_close {
                            let open_start = self.token_start_at(open_brace_idx);
                            let open_len = TextSize::of(self.tokens[open_brace_idx].1);
                            self.errors.push(ParseError::UnclosedStringInterpolation {
                                range: TextRange::at(open_start, open_len),
                            });
                        }
                    }

                    self.sink().finish_node(); // end STRING_INTERPOLATION
                }
                _ => {
                    // Unexpected token - consume and continue
                    self.next();
                }
            }
        }

        self.sink().finish_node(); // end pattern
    }

    /// Parse the content inside an interpolation {...}
    fn parse_string_interpolation_content(&mut self) {
        // Check if we have dotted references by looking ahead
        let has_dots = self
            .tokens
            .iter()
            .skip(self.next_token)
            .take_while(|(kind, _)| *kind != CLOSE_BRACE && *kind != COLON)
            .any(|(kind, _)| *kind == OPERATOR_IDENTIFIER);

        if has_dots {
            // Create a soup to hold the interpolation expression
            self.sink().start_node(SOUP);

            // Parse the first target (must exist)
            if let Some((STRING_INTERPOLATION_TARGET, text)) = self.peek() {
                let len = TextSize::of(text);
                self.next();
                self.sink().start_node(NAME);
                self.sink().token(STRING_INTERPOLATION_TARGET, len);
                self.sink().finish_node(); // end NAME
            }

            // Parse alternating dots and targets
            while let Some((OPERATOR_IDENTIFIER, text)) = self.peek() {
                if text == "." {
                    let dot_len = TextSize::of(text);
                    // Parse the dot
                    self.next();
                    self.sink().start_node(NAME);
                    self.sink().token(OPERATOR_IDENTIFIER, dot_len);
                    self.sink().finish_node(); // end NAME

                    // Parse the following target
                    if let Some((STRING_INTERPOLATION_TARGET, tgt_text)) = self.peek() {
                        let tgt_len = TextSize::of(tgt_text);
                        self.next();
                        self.sink().start_node(NAME);
                        self.sink().token(STRING_INTERPOLATION_TARGET, tgt_len);
                        self.sink().finish_node(); // end NAME
                    } else {
                        // Malformed - dot without following target
                        break;
                    }
                } else {
                    break;
                }
            }

            self.sink().finish_node(); // end SOUP
        } else {
            // Simple case - just one target
            if let Some((STRING_INTERPOLATION_TARGET, text)) = self.peek() {
                let len = TextSize::of(text);
                self.next();
                self.sink().start_node(STRING_INTERPOLATION_TARGET);
                self.sink().token(STRING_INTERPOLATION_TARGET, len);
                self.sink().finish_node();
            }
        }

        // Check for format spec (colon followed by format)
        if let Some((COLON, colon_text)) = self.peek() {
            let colon_len = TextSize::of(colon_text);
            self.next(); // consume :
            self.sink().token(COLON, colon_len);

            if let Some((STRING_FORMAT_SPEC, spec_text)) = self.peek() {
                let spec_len = TextSize::of(spec_text);
                self.next();
                self.sink().start_node(STRING_FORMAT_SPEC);
                self.sink().token(STRING_FORMAT_SPEC, spec_len);
                self.sink().finish_node();
            }
        }
    }
}

trait EventSink {
    fn start_node(&mut self, kind: SyntaxKind);
    fn finish_node(&mut self);
    /// Accept events from a sub-sink.
    ///
    /// `len` is the total byte length of all tokens in `events`.  Sinks that
    /// track source positions (e.g. [`BlockEventSink`]) use it to advance
    /// their running offset; sinks that do not (e.g. [`SimpleEventSink`])
    /// ignore it.
    fn accept(&mut self, events: Vec<ParseEvent>, len: TextSize);
    /// Emit a token of the given kind.
    ///
    /// `len` is the byte length of the token text.  Sinks that track source
    /// positions (e.g. [`BlockEventSink`]) use it to maintain a running
    /// offset; sinks that do not (e.g. [`SimpleEventSink`]) ignore it.
    fn token(&mut self, kind: SyntaxKind, len: TextSize);
    /// Complete and return `(events, additional_errors)`.
    ///
    /// Most sinks return an empty error vec; `BlockEventSink` also returns
    /// errors for malformed declarations (e.g. missing colon).
    fn complete(&mut self) -> (Vec<ParseEvent>, Vec<ParseError>);
}

#[derive(Default)]
struct SimpleEventSink(Vec<ParseEvent>);

impl EventSink for SimpleEventSink {
    fn start_node(&mut self, kind: SyntaxKind) {
        self.0.push(ParseEvent::StartNode(kind));
    }

    fn finish_node(&mut self) {
        self.0.push(ParseEvent::Finish);
    }

    fn accept(&mut self, events: Vec<ParseEvent>, _len: TextSize) {
        self.0.extend(events);
    }

    fn token(&mut self, kind: SyntaxKind, _len: TextSize) {
        self.0.push(ParseEvent::Token(kind));
    }

    fn complete(&mut self) -> (Vec<ParseEvent>, Vec<ParseError>) {
        let mut ret = vec![];
        swap(&mut self.0, &mut ret);
        (ret, vec![])
    }
}

/// Stores state while accumulating a block
#[derive(Default)]
struct BlockEventSink {
    committed: Vec<ParseEvent>,
    /// Events for the block metadata
    block_meta: Vec<ParseEvent>,
    /// Events waiting for assigning to metadata or declaration
    buffer: Vec<ParseEvent>,
    /// Current declaration if any
    declaration: Option<PendingDeclaration>,
    /// Node depth so we can identify top level delimiters
    depth: usize,
    /// Parse errors discovered while committing declarations (e.g. missing colon)
    errors: Vec<ParseError>,
    /// Running byte offset into the source text, updated by every `token()` call.
    ///
    /// Used to compute accurate `TextRange` values for declaration-level errors
    /// such as [`ParseError::MalformedDeclarationHead`].
    current_offset: TextSize,
    /// Byte offset at the start of the current pending declaration.
    ///
    /// Set when we first see a `COLON` token that begins a new declaration.
    /// `None` when no declaration is being accumulated.
    decl_start_offset: Option<TextSize>,
}

struct PendingDeclaration {
    /// Metadata content (if signalled by backtick)
    meta: Option<Vec<ParseEvent>>,
    /// Head content
    head: Vec<ParseEvent>,
    /// Content and trivia around colon
    colon: Vec<ParseEvent>,
    /// Body content
    body: Vec<ParseEvent>,
}

impl BlockEventSink {
    /// Construct a `BlockEventSink` whose running offset begins at `start`.
    ///
    /// This should be the byte offset of the first token that will be
    /// delivered to this sink, so that error ranges reflect absolute file
    /// positions rather than positions relative to the block start.
    fn with_start(start: TextSize) -> Self {
        Self {
            current_offset: start,
            ..Default::default()
        }
    }
}

impl EventSink for BlockEventSink {
    fn start_node(&mut self, kind: SyntaxKind) {
        self.depth += 1;
        self.buffer.push(ParseEvent::StartNode(kind))
    }

    fn finish_node(&mut self) {
        self.depth -= 1;
        self.buffer.push(ParseEvent::Finish)
    }

    fn accept(&mut self, events: Vec<ParseEvent>, len: TextSize) {
        self.current_offset += len;
        self.buffer.extend(events);
    }

    fn token(&mut self, kind: SyntaxKind, len: TextSize) {
        if self.depth == 0 {
            match kind {
                BACKTICK => {
                    if !self.buffer.is_empty() {
                        match self.declaration {
                            Some(ref mut d) => {
                                swap(&mut d.body, &mut self.buffer);
                                self.commit_declaration();
                                self.decl_start_offset = None;
                            }
                            None => {
                                swap(&mut self.block_meta, &mut self.buffer);
                                self.commit_meta();
                            }
                        }
                    }
                    self.declaration = Some(PendingDeclaration {
                        meta: Some(vec![]), // expect meta
                        head: vec![],
                        colon: vec![],
                        body: vec![],
                    });
                    // Record the start of this declaration at the backtick.
                    self.decl_start_offset = Some(self.current_offset);
                    self.current_offset += len;
                    self.buffer.push(ParseEvent::Token(kind));
                }
                COLON => {
                    let mut head_nodes = self.split_off_head();

                    if !self.buffer.is_empty() {
                        match self.declaration {
                            Some(ref mut d) => {
                                // if meta is set and empty it is
                                // expecting metadata
                                if d.meta.as_ref().is_some_and(|m| m.is_empty()) {
                                    swap(d.meta.as_mut().unwrap(), &mut self.buffer);
                                } else {
                                    // expecting body of last block
                                    swap(&mut d.body, &mut self.buffer);
                                    self.commit_declaration();
                                    self.decl_start_offset = None;
                                }
                            }
                            None => {
                                swap(&mut self.block_meta, &mut self.buffer);
                                self.commit_meta();
                            }
                        }
                    }

                    // If we have empty head but we have a declaration with metadata,
                    // try to backtrack and extract the last expression from metadata as head
                    if head_nodes.is_empty() {
                        if let Some(ref mut d) = self.declaration {
                            if let Some(ref mut meta) = d.meta {
                                if let Some(extracted_head) =
                                    Self::extract_last_expression_from_metadata(meta)
                                {
                                    head_nodes = extracted_head;
                                }
                            }
                        }
                    }

                    // Record the start of this new declaration at the colon's
                    // byte position, which is the current offset before we
                    // consume the colon.  A backtick-prefixed declaration may
                    // have set decl_start_offset already; overwrite it here so
                    // the range always starts at the colon when no valid head
                    // preceded it.
                    self.decl_start_offset = Some(self.current_offset);

                    self.current_offset += len;

                    match self.declaration {
                        Some(ref mut d) => {
                            swap(&mut head_nodes, &mut d.head);
                            d.colon.push(ParseEvent::Token(kind));
                        }
                        None => {
                            self.declaration = Some(PendingDeclaration {
                                meta: None,
                                head: head_nodes,
                                colon: vec![ParseEvent::Token(kind)],
                                body: vec![],
                            })
                        }
                    }
                }
                COMMA => {
                    self.current_offset += len;
                    // comma always tacks on the end of what we're buffering
                    self.buffer.push(ParseEvent::Token(kind));
                    match self.declaration {
                        Some(ref mut d) => {
                            if d.meta.is_some() {
                                // error case ' { ` x , }'
                            } else {
                                // expecting body of last block
                                swap(&mut d.body, &mut self.buffer);
                                self.commit_declaration();
                                self.decl_start_offset = None;
                            }
                        }
                        None => {
                            // Bare names separated by commas, e.g. `{x, y}` in a
                            // block pattern. Commas are optional separators just as
                            // in block literals, so leave the buffer as-is and let
                            // complete() accumulate everything into block metadata,
                            // producing the same result as `{x y}`.
                        }
                    }
                }
                _ => {
                    self.current_offset += len;
                    self.buffer.push(ParseEvent::Token(kind));
                }
            }
        } else {
            self.current_offset += len;
            self.buffer.push(ParseEvent::Token(kind));
        }
    }

    /// Complete the block parse and return `(events, errors)`.
    fn complete(&mut self) -> (Vec<ParseEvent>, Vec<ParseError>) {
        let mut ret = vec![];
        if !self.buffer.is_empty() {
            match self.declaration {
                Some(ref mut d) => {
                    // expecting body of last block
                    swap(&mut d.body, &mut self.buffer);
                    self.commit_declaration();
                }
                None => {
                    swap(&mut self.block_meta, &mut self.buffer);
                    self.commit_meta();
                }
            }
        } else if self.declaration.is_some() {
            // Orphaned declaration with empty body (e.g. `{ : }` while
            // typing a monad tag).  Commit it so all tokens are accounted
            // for — it will have an empty body which is valid for error
            // recovery and LSP completion.
            self.commit_declaration();
        }
        swap(&mut self.committed, &mut ret);
        let mut errs = vec![];
        swap(&mut self.errors, &mut errs);
        (ret, errs)
    }
}

impl BlockEventSink {
    /// Block metadata is complete, copy it to the underlying builder,
    /// unless we've already committed other content in which this is
    /// not meta but surplus content
    fn commit_meta(&mut self) {
        if self.committed.is_empty() {
            self.committed.push(ParseEvent::StartNode(BLOCK_META));
            self.committed.push(ParseEvent::StartNode(SOUP));
            self.committed.append(&mut self.block_meta);
            self.committed.push(ParseEvent::Finish);
            self.committed.push(ParseEvent::Finish);
        } else {
            // loose content at this stage is no longer metadata -
            // it's trivia or stowaways
            if self
                .block_meta
                .iter()
                .all(|e| matches!(e, ParseEvent::Token(k) if k.is_trivial()))
            {
                self.committed.append(&mut self.block_meta);
            } else {
                self.committed.push(ParseEvent::StartNode(ERROR_STOWAWAYS));
                self.committed.append(&mut self.block_meta);
                self.committed.push(ParseEvent::Finish);
            }
        }
    }

    /// Pending declaration is complete, copy it to the underlying builder.
    ///
    /// If the declaration head is empty (i.e. `split_off_head` failed to
    /// extract a valid head), the entire declaration is wrapped in an `ERROR`
    /// node for declaration-level error recovery.  This isolates the malformed
    /// binding so that surrounding valid declarations still parse correctly.
    fn commit_declaration(&mut self) {
        if let Some(ref mut decl) = self.declaration {
            // Determine whether the head is non-trivially populated.
            let has_non_trivial_head = decl
                .head
                .iter()
                .any(|e| !matches!(e, ParseEvent::Token(k) if k.is_trivial()));

            if !has_non_trivial_head && !decl.colon.is_empty() {
                // Malformed declaration: colon present but no valid head was
                // extracted.  Wrap the whole thing in an ERROR node so that
                // adjacent valid declarations are not affected.
                //
                // The range covers from the declaration start (the colon or
                // backtick that opened it) up to the current offset.  If no
                // start was recorded (shouldn't happen), fall back to the
                // current offset as a single-byte range.
                let range_start = self.decl_start_offset.unwrap_or(self.current_offset);
                let range = rowan::TextRange::new(range_start, self.current_offset);
                self.errors
                    .push(ParseError::MalformedDeclarationHead { range });
                self.committed.push(ParseEvent::StartNode(ERROR));
                if let Some(m) = &mut decl.meta {
                    self.committed.append(m);
                }
                self.committed.append(&mut decl.head);
                self.committed.append(&mut decl.colon);
                self.committed.append(&mut decl.body);
                self.committed.push(ParseEvent::Finish);
            } else {
                self.committed.push(ParseEvent::StartNode(DECLARATION));
                if let Some(m) = &mut decl.meta {
                    self.committed.push(ParseEvent::StartNode(DECL_META));
                    self.committed.push(ParseEvent::StartNode(SOUP));
                    self.committed.append(m);
                    self.committed.push(ParseEvent::Finish);
                    self.committed.push(ParseEvent::Finish);
                }

                self.committed.push(ParseEvent::StartNode(DECL_HEAD));
                self.committed.append(&mut decl.head);
                self.committed.push(ParseEvent::Finish);

                self.committed.append(&mut decl.colon);

                self.committed.push(ParseEvent::StartNode(DECL_BODY));
                self.committed.push(ParseEvent::StartNode(SOUP));

                // split trailing comma & trivia off the body
                let mut idx = decl.body.len();
                for e in decl.body.iter().rev() {
                    match e {
                        ParseEvent::Token(COMMA)
                        | ParseEvent::Token(WHITESPACE)
                        | ParseEvent::Token(COMMENT) => {
                            idx -= 1;
                        }
                        _ => {
                            break;
                        }
                    }
                }
                let trailer = decl.body.split_off(idx);

                self.committed.append(&mut decl.body);
                self.committed.push(ParseEvent::Finish);
                self.committed.push(ParseEvent::Finish);
                self.committed.push(ParseEvent::Finish);

                for e in trailer {
                    self.committed.push(e);
                }
            }
        }
        self.declaration = None;
    }

    /// After encoutering a colon, split what must be the declaration
    /// head off the end of the buffer.
    ///
    /// If a sensible declaration head cannot be split off, return an
    /// empty split as best effot to continue the parse
    fn split_off_head(&mut self) -> Vec<ParseEvent> {
        if self.buffer.is_empty() {
            return vec![];
        }

        let mut depth = 0;
        let mut idx = self.buffer.len();
        for e in self.buffer.iter().rev() {
            idx -= 1;
            match e {
                ParseEvent::Finish => {
                    depth += 1;
                }
                ParseEvent::StartNode(_) if depth > 1 => {
                    depth -= 1;
                }
                ParseEvent::StartNode(PAREN_EXPR)
                | ParseEvent::StartNode(NAME)
                | ParseEvent::StartNode(BRACKET_EXPR) => {
                    break;
                }
                ParseEvent::StartNode(ARG_TUPLE) => {
                    depth -= 1;
                }
                ParseEvent::StartNode(_t) => {
                    // invalid - refuse split
                    return vec![];
                }
                ParseEvent::Token(k) if k.is_trivial() => {}
                ParseEvent::Token(_) if depth == 0 => {
                    // invalid - refuse split
                    return vec![];
                }
                _ => {}
            }
        }
        self.buffer.split_off(idx)
    }

    /// Extract the last complete expression from metadata events to use as declaration head.
    /// This handles cases like: ` { complex: metadata } (head): body
    /// where (head) should be the declaration head, not part of metadata.
    fn extract_last_expression_from_metadata(
        meta: &mut Vec<ParseEvent>,
    ) -> Option<Vec<ParseEvent>> {
        if meta.is_empty() {
            return None;
        }

        let mut depth = 0;
        let mut idx = meta.len();
        let mut found_expression = false;

        // Scan backwards to find the last complete expression
        for e in meta.iter().rev() {
            idx -= 1;
            match e {
                ParseEvent::Finish => {
                    depth += 1;
                }
                ParseEvent::StartNode(_) if depth > 1 => {
                    depth -= 1;
                }
                ParseEvent::StartNode(PAREN_EXPR)
                | ParseEvent::StartNode(NAME)
                | ParseEvent::StartNode(ARG_TUPLE) => {
                    found_expression = true;
                    if depth == 0 {
                        break;
                    }
                    depth -= 1;
                }
                ParseEvent::StartNode(_) => {
                    if depth == 0 {
                        // We've gone too far back - this is not part of the expression
                        idx += 1;
                        break;
                    }
                    depth -= 1;
                }
                ParseEvent::Token(k) if k.is_trivial() => {
                    // Skip whitespace, comments
                    if depth == 0 && found_expression {
                        idx += 1;
                        break;
                    }
                }
                ParseEvent::Token(_) => {
                    if depth == 0 {
                        // We've gone too far back
                        idx += 1;
                        break;
                    }
                }
            }
        }

        if found_expression && idx < meta.len() {
            // Split off the expression from the metadata
            let mut extracted = meta.split_off(idx);

            // Convert ARG_TUPLE to PAREN_EXPR for operator declarations
            // ARG_TUPLE is used in parameter context, but for declaration heads
            // we need PAREN_EXPR to match the validation logic
            if let Some(ParseEvent::StartNode(ARG_TUPLE)) = extracted.first() {
                extracted[0] = ParseEvent::StartNode(PAREN_EXPR);
            }

            Some(extracted)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {

    use std::path::Path;

    use crate::syntax::rowan::{parse_expr, parse_unit};
    use rowan::ast::AstNode;

    fn verify_expr(text: &str, parse_repr: &str) {
        let parse = parse_expr(text);
        println!("checking: {text}");
        assert_eq!(format!("\n{:#?}", parse.syntax_node()), parse_repr);
        assert!(parse.ok().is_ok());
    }

    fn verify_unit(text: &str, parse_repr: &str) {
        let parse = parse_unit(text);
        assert_eq!(format!("\n{:#?}", parse.syntax_node()), parse_repr);
        assert!(parse.ok().is_ok());
    }

    #[test]
    fn test_literals() {
        verify_expr(
            ":foo",
            r#"
SOUP@0..4
  LITERAL@0..4
    SYMBOL@0..4 ":foo"
"#,
        );

        verify_expr(
            ":'bar'",
            r#"
SOUP@0..6
  LITERAL@0..6
    SYMBOL@0..6 ":'bar'"
"#,
        );

        verify_expr(
            ":'~*&^%'",
            r#"
SOUP@0..8
  LITERAL@0..8
    SYMBOL@0..8 ":'~*&^%'"
"#,
        );

        verify_expr(
            "\"foo\"",
            r#"
SOUP@0..5
  LITERAL@0..5
    STRING@0..5 "\"foo\""
"#,
        );

        verify_expr(
            "\"إ\"",
            r#"
SOUP@0..4
  LITERAL@0..4
    STRING@0..4 "\"إ\""
"#,
        );

        verify_expr(
            "-1234.1234",
            r#"
SOUP@0..10
  LITERAL@0..10
    NUMBER@0..10 "-1234.1234"
"#,
        );

        verify_expr(
            "-1234",
            r#"
SOUP@0..5
  LITERAL@0..5
    NUMBER@0..5 "-1234"
"#,
        );

        verify_expr(
            "999",
            r#"
SOUP@0..3
  LITERAL@0..3
    NUMBER@0..3 "999"
"#,
        );
    }

    #[test]
    fn test_identifiers() {
        verify_expr(
            "xyz",
            r#"
SOUP@0..3
  NAME@0..3
    UNQUOTED_IDENTIFIER@0..3 "xyz"
"#,
        );

        verify_expr(
            "?xyz",
            r#"
SOUP@0..4
  NAME@0..4
    UNQUOTED_IDENTIFIER@0..4 "?xyz"
"#,
        );

        verify_expr(
            "a-b-c?",
            r#"
SOUP@0..6
  NAME@0..6
    UNQUOTED_IDENTIFIER@0..6 "a-b-c?"
"#,
        );

        verify_expr(
            "•1",
            r#"
SOUP@0..4
  NAME@0..4
    UNQUOTED_IDENTIFIER@0..4 "•1"
"#,
        );

        verify_expr(
            "__BIF",
            r#"
SOUP@0..5
  NAME@0..5
    UNQUOTED_IDENTIFIER@0..5 "__BIF"
"#,
        );

        verify_expr(
            "__1",
            r#"
SOUP@0..3
  NAME@0..3
    UNQUOTED_IDENTIFIER@0..3 "__1"
"#,
        );

        verify_expr(
            "∨",
            r#"
SOUP@0..3
  NAME@0..3
    OPERATOR_IDENTIFIER@0..3 "∨"
"#,
        );

        verify_expr(
            "∘",
            r#"
SOUP@0..3
  NAME@0..3
    OPERATOR_IDENTIFIER@0..3 "∘"
"#,
        );

        verify_expr(
            "&&",
            r#"
SOUP@0..2
  NAME@0..2
    OPERATOR_IDENTIFIER@0..2 "&&"
"#,
        );

        verify_expr(
            "-",
            r#"
SOUP@0..1
  NAME@0..1
    OPERATOR_IDENTIFIER@0..1 "-"
"#,
        );

        verify_expr(
            "'asdf'",
            r#"
SOUP@0..6
  NAME@0..6
    SINGLE_QUOTE_IDENTIFIER@0..6 "'asdf'"
"#,
        );

        verify_expr(
            "'::||\t||::'",
            r#"
SOUP@0..11
  NAME@0..11
    SINGLE_QUOTE_IDENTIFIER@0..11 "'::||\t||::'"
"#,
        );
    }

    #[test]
    fn test_empty_list() {
        verify_expr(
            "[]",
            r#"
SOUP@0..2
  LIST@0..2
    OPEN_SQUARE@0..1 "["
    CLOSE_SQUARE@1..2 "]"
"#,
        );
    }

    #[test]
    fn test_expressions() {
        verify_expr(
            "(2 + 3)",
            r#"
SOUP@0..7
  PAREN_EXPR@0..7
    OPEN_PAREN@0..1 "("
    SOUP@1..6
      LITERAL@1..2
        NUMBER@1..2 "2"
      WHITESPACE@2..3 " "
      NAME@3..4
        OPERATOR_IDENTIFIER@3..4 "+"
      WHITESPACE@4..5 " "
      LITERAL@5..6
        NUMBER@5..6 "3"
    CLOSE_PAREN@6..7 ")"
"#,
        );

        verify_expr(
            "(2+3)",
            r#"
SOUP@0..5
  PAREN_EXPR@0..5
    OPEN_PAREN@0..1 "("
    SOUP@1..4
      LITERAL@1..2
        NUMBER@1..2 "2"
      NAME@2..3
        OPERATOR_IDENTIFIER@2..3 "+"
      LITERAL@3..4
        NUMBER@3..4 "3"
    CLOSE_PAREN@4..5 ")"
"#,
        );

        verify_expr(
            "[x, y, z]",
            r#"
SOUP@0..9
  LIST@0..9
    OPEN_SQUARE@0..1 "["
    SOUP@1..2
      NAME@1..2
        UNQUOTED_IDENTIFIER@1..2 "x"
    COMMA@2..3 ","
    WHITESPACE@3..4 " "
    SOUP@4..5
      NAME@4..5
        UNQUOTED_IDENTIFIER@4..5 "y"
    COMMA@5..6 ","
    WHITESPACE@6..7 " "
    SOUP@7..8
      NAME@7..8
        UNQUOTED_IDENTIFIER@7..8 "z"
    CLOSE_SQUARE@8..9 "]"
"#,
        );

        verify_expr(
            "f(x)",
            r#"
SOUP@0..4
  NAME@0..1
    UNQUOTED_IDENTIFIER@0..1 "f"
  ARG_TUPLE@1..4
    OPEN_PAREN_APPLY@1..2 "("
    SOUP@2..3
      NAME@2..3
        UNQUOTED_IDENTIFIER@2..3 "x"
    CLOSE_PAREN@3..4 ")"
"#,
        );

        verify_expr(
            "x f g h",
            r#"
SOUP@0..7
  NAME@0..1
    UNQUOTED_IDENTIFIER@0..1 "x"
  WHITESPACE@1..2 " "
  NAME@2..3
    UNQUOTED_IDENTIFIER@2..3 "f"
  WHITESPACE@3..4 " "
  NAME@4..5
    UNQUOTED_IDENTIFIER@4..5 "g"
  WHITESPACE@5..6 " "
  NAME@6..7
    UNQUOTED_IDENTIFIER@6..7 "h"
"#,
        );

        verify_expr(
            " x f g h ",
            r#"
SOUP@0..9
  WHITESPACE@0..1 " "
  NAME@1..2
    UNQUOTED_IDENTIFIER@1..2 "x"
  WHITESPACE@2..3 " "
  NAME@3..4
    UNQUOTED_IDENTIFIER@3..4 "f"
  WHITESPACE@4..5 " "
  NAME@5..6
    UNQUOTED_IDENTIFIER@5..6 "g"
  WHITESPACE@6..7 " "
  NAME@7..8
    UNQUOTED_IDENTIFIER@7..8 "h"
  WHITESPACE@8..9 " "
"#,
        );

        verify_expr(
            "#\n x #\n f #\n g #\n h ",
            r##"
SOUP@0..20
  COMMENT@0..1 "#"
  WHITESPACE@1..3 "\n "
  NAME@3..4
    UNQUOTED_IDENTIFIER@3..4 "x"
  WHITESPACE@4..5 " "
  COMMENT@5..6 "#"
  WHITESPACE@6..8 "\n "
  NAME@8..9
    UNQUOTED_IDENTIFIER@8..9 "f"
  WHITESPACE@9..10 " "
  COMMENT@10..11 "#"
  WHITESPACE@11..13 "\n "
  NAME@13..14
    UNQUOTED_IDENTIFIER@13..14 "g"
  WHITESPACE@14..15 " "
  COMMENT@15..16 "#"
  WHITESPACE@16..18 "\n "
  NAME@18..19
    UNQUOTED_IDENTIFIER@18..19 "h"
  WHITESPACE@19..20 " "
"##,
        );

        verify_expr(
            "[:x,]",
            r#"
SOUP@0..5
  LIST@0..5
    OPEN_SQUARE@0..1 "["
    SOUP@1..3
      LITERAL@1..3
        SYMBOL@1..3 ":x"
    COMMA@3..4 ","
    CLOSE_SQUARE@4..5 "]"
"#,
        );

        verify_expr(
            "[:x, ]",
            r#"
SOUP@0..6
  LIST@0..6
    OPEN_SQUARE@0..1 "["
    SOUP@1..3
      LITERAL@1..3
        SYMBOL@1..3 ":x"
    COMMA@3..4 ","
    WHITESPACE@4..5 " "
    CLOSE_SQUARE@5..6 "]"
"#,
        );
    }

    #[test]
    fn test_blocks() {
        verify_expr(
            "{x: 3}",
            r#"
SOUP@0..6
  BLOCK@0..6
    OPEN_BRACE@0..1 "{"
    DECLARATION@1..5
      DECL_HEAD@1..2
        NAME@1..2
          UNQUOTED_IDENTIFIER@1..2 "x"
      COLON@2..3 ":"
      DECL_BODY@3..5
        SOUP@3..5
          WHITESPACE@3..4 " "
          LITERAL@4..5
            NUMBER@4..5 "3"
    CLOSE_BRACE@5..6 "}"
"#,
        );

        verify_expr(
            "{x: 3 y: 4}",
            r#"
SOUP@0..11
  BLOCK@0..11
    OPEN_BRACE@0..1 "{"
    DECLARATION@1..5
      DECL_HEAD@1..2
        NAME@1..2
          UNQUOTED_IDENTIFIER@1..2 "x"
      COLON@2..3 ":"
      DECL_BODY@3..5
        SOUP@3..5
          WHITESPACE@3..4 " "
          LITERAL@4..5
            NUMBER@4..5 "3"
    WHITESPACE@5..6 " "
    DECLARATION@6..10
      DECL_HEAD@6..7
        NAME@6..7
          UNQUOTED_IDENTIFIER@6..7 "y"
      COLON@7..8 ":"
      DECL_BODY@8..10
        SOUP@8..10
          WHITESPACE@8..9 " "
          LITERAL@9..10
            NUMBER@9..10 "4"
    CLOSE_BRACE@10..11 "}"
"#,
        );

        verify_expr(
            "{}",
            r#"
SOUP@0..2
  BLOCK@0..2
    OPEN_BRACE@0..1 "{"
    CLOSE_BRACE@1..2 "}"
"#,
        );
    }

    #[test]
    fn test_fn_declaration() {
        verify_expr(
            "{f(x, y): x + 3}",
            r#"
SOUP@0..16
  BLOCK@0..16
    OPEN_BRACE@0..1 "{"
    DECLARATION@1..15
      DECL_HEAD@1..8
        NAME@1..2
          UNQUOTED_IDENTIFIER@1..2 "f"
        ARG_TUPLE@2..8
          OPEN_PAREN_APPLY@2..3 "("
          SOUP@3..4
            NAME@3..4
              UNQUOTED_IDENTIFIER@3..4 "x"
          COMMA@4..5 ","
          WHITESPACE@5..6 " "
          SOUP@6..7
            NAME@6..7
              UNQUOTED_IDENTIFIER@6..7 "y"
          CLOSE_PAREN@7..8 ")"
      COLON@8..9 ":"
      DECL_BODY@9..15
        SOUP@9..15
          WHITESPACE@9..10 " "
          NAME@10..11
            UNQUOTED_IDENTIFIER@10..11 "x"
          WHITESPACE@11..12 " "
          NAME@12..13
            OPERATOR_IDENTIFIER@12..13 "+"
          WHITESPACE@13..14 " "
          LITERAL@14..15
            NUMBER@14..15 "3"
    CLOSE_BRACE@15..16 "}"
"#,
        );
    }

    #[test]
    fn test_plain_binop_decl() {
        verify_expr(
            "{ (x &&& y): y }",
            r#"
SOUP@0..16
  BLOCK@0..16
    OPEN_BRACE@0..1 "{"
    WHITESPACE@1..2 " "
    DECLARATION@2..14
      DECL_HEAD@2..11
        PAREN_EXPR@2..11
          OPEN_PAREN@2..3 "("
          SOUP@3..10
            NAME@3..4
              UNQUOTED_IDENTIFIER@3..4 "x"
            WHITESPACE@4..5 " "
            NAME@5..8
              OPERATOR_IDENTIFIER@5..8 "&&&"
            WHITESPACE@8..9 " "
            NAME@9..10
              UNQUOTED_IDENTIFIER@9..10 "y"
          CLOSE_PAREN@10..11 ")"
      COLON@11..12 ":"
      DECL_BODY@12..14
        SOUP@12..14
          WHITESPACE@12..13 " "
          NAME@13..14
            UNQUOTED_IDENTIFIER@13..14 "y"
    WHITESPACE@14..15 " "
    CLOSE_BRACE@15..16 "}"
"#,
        );
    }

    #[test]
    fn test_plain_prefixop_decl() {
        verify_expr(
            "{(! y): y}",
            r#"
SOUP@0..10
  BLOCK@0..10
    OPEN_BRACE@0..1 "{"
    DECLARATION@1..9
      DECL_HEAD@1..6
        PAREN_EXPR@1..6
          OPEN_PAREN@1..2 "("
          SOUP@2..5
            NAME@2..3
              OPERATOR_IDENTIFIER@2..3 "!"
            WHITESPACE@3..4 " "
            NAME@4..5
              UNQUOTED_IDENTIFIER@4..5 "y"
          CLOSE_PAREN@5..6 ")"
      COLON@6..7 ":"
      DECL_BODY@7..9
        SOUP@7..9
          WHITESPACE@7..8 " "
          NAME@8..9
            UNQUOTED_IDENTIFIER@8..9 "y"
    CLOSE_BRACE@9..10 "}"
"#,
        );
    }

    #[test]
    fn test_plain_postfixop_decl() {
        verify_expr(
            "{ ( y * ): y }",
            r#"
SOUP@0..14
  BLOCK@0..14
    OPEN_BRACE@0..1 "{"
    WHITESPACE@1..2 " "
    DECLARATION@2..12
      DECL_HEAD@2..9
        PAREN_EXPR@2..9
          OPEN_PAREN@2..3 "("
          WHITESPACE@3..4 " "
          SOUP@4..8
            NAME@4..5
              UNQUOTED_IDENTIFIER@4..5 "y"
            WHITESPACE@5..6 " "
            NAME@6..7
              OPERATOR_IDENTIFIER@6..7 "*"
            WHITESPACE@7..8 " "
          CLOSE_PAREN@8..9 ")"
      COLON@9..10 ":"
      DECL_BODY@10..12
        SOUP@10..12
          WHITESPACE@10..11 " "
          NAME@11..12
            UNQUOTED_IDENTIFIER@11..12 "y"
    WHITESPACE@12..13 " "
    CLOSE_BRACE@13..14 "}"
"#,
        );
    }

    #[test]
    fn test_unary_op_decl() {
        verify_expr(
            "{(∅):0}",
            r#"
SOUP@0..9
  BLOCK@0..9
    OPEN_BRACE@0..1 "{"
    DECLARATION@1..8
      DECL_HEAD@1..6
        PAREN_EXPR@1..6
          OPEN_PAREN@1..2 "("
          SOUP@2..5
            NAME@2..5
              OPERATOR_IDENTIFIER@2..5 "∅"
          CLOSE_PAREN@5..6 ")"
      COLON@6..7 ":"
      DECL_BODY@7..8
        SOUP@7..8
          LITERAL@7..8
            NUMBER@7..8 "0"
    CLOSE_BRACE@8..9 "}"
"#,
        );
    }

    #[test]
    fn test_block_meta() {
        verify_expr(
            "{ a b c foo: :bar}",
            r#"
SOUP@0..18
  BLOCK@0..18
    OPEN_BRACE@0..1 "{"
    WHITESPACE@1..2 " "
    BLOCK_META@2..8
      SOUP@2..8
        NAME@2..3
          UNQUOTED_IDENTIFIER@2..3 "a"
        WHITESPACE@3..4 " "
        NAME@4..5
          UNQUOTED_IDENTIFIER@4..5 "b"
        WHITESPACE@5..6 " "
        NAME@6..7
          UNQUOTED_IDENTIFIER@6..7 "c"
        WHITESPACE@7..8 " "
    DECLARATION@8..17
      DECL_HEAD@8..11
        NAME@8..11
          UNQUOTED_IDENTIFIER@8..11 "foo"
      COLON@11..12 ":"
      DECL_BODY@12..17
        SOUP@12..17
          WHITESPACE@12..13 " "
          LITERAL@13..17
            SYMBOL@13..17 ":bar"
    CLOSE_BRACE@17..18 "}"
"#,
        );
    }

    #[test]
    fn test_complex_blocks() {
        verify_expr(
            "{ α: [:x, :y, :z] = [:x, :y, :z, ] }",
            r#"
SOUP@0..37
  BLOCK@0..37
    OPEN_BRACE@0..1 "{"
    WHITESPACE@1..2 " "
    DECLARATION@2..35
      DECL_HEAD@2..4
        NAME@2..4
          UNQUOTED_IDENTIFIER@2..4 "α"
      COLON@4..5 ":"
      DECL_BODY@5..35
        SOUP@5..35
          WHITESPACE@5..6 " "
          LIST@6..18
            OPEN_SQUARE@6..7 "["
            SOUP@7..9
              LITERAL@7..9
                SYMBOL@7..9 ":x"
            COMMA@9..10 ","
            WHITESPACE@10..11 " "
            SOUP@11..13
              LITERAL@11..13
                SYMBOL@11..13 ":y"
            COMMA@13..14 ","
            WHITESPACE@14..15 " "
            SOUP@15..17
              LITERAL@15..17
                SYMBOL@15..17 ":z"
            CLOSE_SQUARE@17..18 "]"
          WHITESPACE@18..19 " "
          NAME@19..20
            OPERATOR_IDENTIFIER@19..20 "="
          WHITESPACE@20..21 " "
          LIST@21..35
            OPEN_SQUARE@21..22 "["
            SOUP@22..24
              LITERAL@22..24
                SYMBOL@22..24 ":x"
            COMMA@24..25 ","
            WHITESPACE@25..26 " "
            SOUP@26..28
              LITERAL@26..28
                SYMBOL@26..28 ":y"
            COMMA@28..29 ","
            WHITESPACE@29..30 " "
            SOUP@30..32
              LITERAL@30..32
                SYMBOL@30..32 ":z"
            COMMA@32..33 ","
            WHITESPACE@33..34 " "
            CLOSE_SQUARE@34..35 "]"
    WHITESPACE@35..36 " "
    CLOSE_BRACE@36..37 "}"
"#,
        );
    }

    #[test]
    pub fn test_simple_unit() {
        verify_unit(
            "x: :foo",
            r#"
UNIT@0..7
  DECLARATION@0..7
    DECL_HEAD@0..1
      NAME@0..1
        UNQUOTED_IDENTIFIER@0..1 "x"
    COLON@1..2 ":"
    DECL_BODY@2..7
      SOUP@2..7
        WHITESPACE@2..3 " "
        LITERAL@3..7
          SYMBOL@3..7 ":foo"
"#,
        );
    }

    #[test]
    pub fn test_block_with_decl_meta() {
        verify_expr(
            "{ `:meta foo: :bar }",
            r#"
SOUP@0..20
  BLOCK@0..20
    OPEN_BRACE@0..1 "{"
    WHITESPACE@1..2 " "
    DECLARATION@2..18
      DECL_META@2..9
        SOUP@2..9
          BACKTICK@2..3 "`"
          LITERAL@3..8
            SYMBOL@3..8 ":meta"
          WHITESPACE@8..9 " "
      DECL_HEAD@9..12
        NAME@9..12
          UNQUOTED_IDENTIFIER@9..12 "foo"
      COLON@12..13 ":"
      DECL_BODY@13..18
        SOUP@13..18
          WHITESPACE@13..14 " "
          LITERAL@14..18
            SYMBOL@14..18 ":bar"
    WHITESPACE@18..19 " "
    CLOSE_BRACE@19..20 "}"
"#,
        );
    }

    #[test]
    pub fn test_several_decls() {
        verify_unit(
            "` :foo z(f): g g(x): y x: 0",
            r#"
UNIT@0..27
  DECLARATION@0..14
    DECL_META@0..7
      SOUP@0..7
        BACKTICK@0..1 "`"
        WHITESPACE@1..2 " "
        LITERAL@2..6
          SYMBOL@2..6 ":foo"
        WHITESPACE@6..7 " "
    DECL_HEAD@7..11
      NAME@7..8
        UNQUOTED_IDENTIFIER@7..8 "z"
      ARG_TUPLE@8..11
        OPEN_PAREN_APPLY@8..9 "("
        SOUP@9..10
          NAME@9..10
            UNQUOTED_IDENTIFIER@9..10 "f"
        CLOSE_PAREN@10..11 ")"
    COLON@11..12 ":"
    DECL_BODY@12..14
      SOUP@12..14
        WHITESPACE@12..13 " "
        NAME@13..14
          UNQUOTED_IDENTIFIER@13..14 "g"
  WHITESPACE@14..15 " "
  DECLARATION@15..22
    DECL_HEAD@15..19
      NAME@15..16
        UNQUOTED_IDENTIFIER@15..16 "g"
      ARG_TUPLE@16..19
        OPEN_PAREN_APPLY@16..17 "("
        SOUP@17..18
          NAME@17..18
            UNQUOTED_IDENTIFIER@17..18 "x"
        CLOSE_PAREN@18..19 ")"
    COLON@19..20 ":"
    DECL_BODY@20..22
      SOUP@20..22
        WHITESPACE@20..21 " "
        NAME@21..22
          UNQUOTED_IDENTIFIER@21..22 "y"
  WHITESPACE@22..23 " "
  DECLARATION@23..27
    DECL_HEAD@23..24
      NAME@23..24
        UNQUOTED_IDENTIFIER@23..24 "x"
    COLON@24..25 ":"
    DECL_BODY@25..27
      SOUP@25..27
        WHITESPACE@25..26 " "
        LITERAL@26..27
          NUMBER@26..27 "0"
"#,
        );
    }

    #[test]
    pub fn test_block_with_commas() {
        verify_expr(
            "{ x: y, z: w, }",
            r#"
SOUP@0..15
  BLOCK@0..15
    OPEN_BRACE@0..1 "{"
    WHITESPACE@1..2 " "
    DECLARATION@2..6
      DECL_HEAD@2..3
        NAME@2..3
          UNQUOTED_IDENTIFIER@2..3 "x"
      COLON@3..4 ":"
      DECL_BODY@4..6
        SOUP@4..6
          WHITESPACE@4..5 " "
          NAME@5..6
            UNQUOTED_IDENTIFIER@5..6 "y"
    COMMA@6..7 ","
    WHITESPACE@7..8 " "
    DECLARATION@8..12
      DECL_HEAD@8..9
        NAME@8..9
          UNQUOTED_IDENTIFIER@8..9 "z"
      COLON@9..10 ":"
      DECL_BODY@10..12
        SOUP@10..12
          WHITESPACE@10..11 " "
          NAME@11..12
            UNQUOTED_IDENTIFIER@11..12 "w"
    COMMA@12..13 ","
    WHITESPACE@13..14 " "
    CLOSE_BRACE@14..15 "}"
"#,
        );
    }

    #[test]
    pub fn test_complex_unit() {
        verify_unit(
            r#"#!/usr/bin/env eu

{ doc: "unit meta" }

x: 123

` "docs"
f(y): y + 234

ns: {
  a: b d c e # comment
}

"#,
            r##"
UNIT@0..105
  COMMENT@0..17 "#!/usr/bin/env eu"
  WHITESPACE@17..19 "\n\n"
  BLOCK_META@19..41
    SOUP@19..41
      BLOCK@19..39
        OPEN_BRACE@19..20 "{"
        WHITESPACE@20..21 " "
        DECLARATION@21..37
          DECL_HEAD@21..24
            NAME@21..24
              UNQUOTED_IDENTIFIER@21..24 "doc"
          COLON@24..25 ":"
          DECL_BODY@25..37
            SOUP@25..37
              WHITESPACE@25..26 " "
              LITERAL@26..37
                STRING@26..37 "\"unit meta\""
        WHITESPACE@37..38 " "
        CLOSE_BRACE@38..39 "}"
      WHITESPACE@39..41 "\n\n"
  DECLARATION@41..47
    DECL_HEAD@41..42
      NAME@41..42
        UNQUOTED_IDENTIFIER@41..42 "x"
    COLON@42..43 ":"
    DECL_BODY@43..47
      SOUP@43..47
        WHITESPACE@43..44 " "
        LITERAL@44..47
          NUMBER@44..47 "123"
  WHITESPACE@47..49 "\n\n"
  DECLARATION@49..71
    DECL_META@49..58
      SOUP@49..58
        BACKTICK@49..50 "`"
        WHITESPACE@50..51 " "
        LITERAL@51..57
          STRING@51..57 "\"docs\""
        WHITESPACE@57..58 "\n"
    DECL_HEAD@58..62
      NAME@58..59
        UNQUOTED_IDENTIFIER@58..59 "f"
      ARG_TUPLE@59..62
        OPEN_PAREN_APPLY@59..60 "("
        SOUP@60..61
          NAME@60..61
            UNQUOTED_IDENTIFIER@60..61 "y"
        CLOSE_PAREN@61..62 ")"
    COLON@62..63 ":"
    DECL_BODY@63..71
      SOUP@63..71
        WHITESPACE@63..64 " "
        NAME@64..65
          UNQUOTED_IDENTIFIER@64..65 "y"
        WHITESPACE@65..66 " "
        NAME@66..67
          OPERATOR_IDENTIFIER@66..67 "+"
        WHITESPACE@67..68 " "
        LITERAL@68..71
          NUMBER@68..71 "234"
  WHITESPACE@71..73 "\n\n"
  DECLARATION@73..103
    DECL_HEAD@73..75
      NAME@73..75
        UNQUOTED_IDENTIFIER@73..75 "ns"
    COLON@75..76 ":"
    DECL_BODY@76..103
      SOUP@76..103
        WHITESPACE@76..77 " "
        BLOCK@77..103
          OPEN_BRACE@77..78 "{"
          WHITESPACE@78..81 "\n  "
          DECLARATION@81..91
            DECL_HEAD@81..82
              NAME@81..82
                UNQUOTED_IDENTIFIER@81..82 "a"
            COLON@82..83 ":"
            DECL_BODY@83..91
              SOUP@83..91
                WHITESPACE@83..84 " "
                NAME@84..85
                  UNQUOTED_IDENTIFIER@84..85 "b"
                WHITESPACE@85..86 " "
                NAME@86..87
                  UNQUOTED_IDENTIFIER@86..87 "d"
                WHITESPACE@87..88 " "
                NAME@88..89
                  UNQUOTED_IDENTIFIER@88..89 "c"
                WHITESPACE@89..90 " "
                NAME@90..91
                  UNQUOTED_IDENTIFIER@90..91 "e"
          WHITESPACE@91..92 " "
          COMMENT@92..101 "# comment"
          WHITESPACE@101..102 "\n"
          CLOSE_BRACE@102..103 "}"
  WHITESPACE@103..105 "\n\n"
"##,
        );
    }

    fn parse_eucalypt_file(path: &Path) {
        let text = std::fs::read_to_string(path).unwrap();
        let parse = parse_unit(&text);
        if !parse.errors().is_empty() {
            print!(
                "
{:#?}",
                parse.syntax_node()
            );
            assert!(parse.ok().is_ok());
        }
    }

    #[test]
    fn test_string_patterns() {
        // Simple string pattern with interpolation
        let text = r#""Hello {name}!""#;
        let parse = parse_expr(text);
        println!("String pattern parse tree:\n{:#?}", parse.syntax_node());
        assert!(parse.ok().is_ok());

        // String pattern with format specifier
        let text = r#""Value: {num:%03d}""#;
        let parse = parse_expr(text);
        println!(
            "String pattern with format spec:\n{:#?}",
            parse.syntax_node()
        );
        assert!(parse.ok().is_ok());

        // String pattern with escaped braces
        let text = r#""Escaped {{braces}} here""#;
        let parse = parse_expr(text);
        println!(
            "String pattern with escaped braces:\n{:#?}",
            parse.syntax_node()
        );
        assert!(parse.ok().is_ok());

        // Test examples from actual harness files

        // From 024_interpolation.eu - basic variable interpolation
        let text = r#"test: "{x}+{y}={z}""#;
        let parse = parse_unit(text);
        println!(
            "Harness example - basic interpolation:\n{:#?}",
            parse.syntax_node()
        );
        assert!(
            parse.ok().is_ok(),
            "Failed to parse basic interpolation from harness"
        );

        // From 024_interpolation.eu - dotted reference with format
        let text = r#"test: "{data.foo.bar:%06d}""#;
        let parse = parse_unit(text);
        println!(
            "Harness example - dotted reference with format:\n{:#?}",
            parse.syntax_node()
        );
        assert!(
            parse.ok().is_ok(),
            "Failed to parse dotted reference with format from harness"
        );

        // From 041_numeric_formats.eu - complex format
        let text = r#"test: "{a:%04d}-{b:%04f}""#;
        let parse = parse_unit(text);
        println!(
            "Harness example - complex format:\n{:#?}",
            parse.syntax_node()
        );
        assert!(
            parse.ok().is_ok(),
            "Failed to parse complex format from harness"
        );

        // From 041_numeric_formats.eu - anaphora with format
        let text = r#"test: "{:%03d}{:%05x}""#;
        let parse = parse_unit(text);
        println!(
            "Harness example - anaphora with format:\n{:#?}",
            parse.syntax_node()
        );
        assert!(
            parse.ok().is_ok(),
            "Failed to parse anaphora with format from harness"
        );
    }

    #[test]
    fn test_specific_string_pattern_files() {
        // Test specific files known to contain string patterns
        let test_files = [
            "tests/harness/024_interpolation.eu",
            "tests/harness/041_numeric_formats.eu",
        ];

        for file_path in &test_files {
            println!("Testing string pattern file: {file_path}");
            let content = std::fs::read_to_string(file_path).unwrap();
            let parse = parse_unit(&content);

            if !parse.errors().is_empty() {
                println!("Errors in {}: {:?}", file_path, parse.errors());
            }

            assert!(
                parse.errors().is_empty(),
                "{file_path} should parse without errors"
            );
            assert!(parse.ok().is_ok(), "{file_path} should parse successfully");
        }
    }

    #[test]
    fn test_block_anaphora() {
        // Test basic block anaphora patterns

        // Unnumbered anaphora
        let text = r#"test: { x: • y: • }"#;
        let parse = parse_unit(text);
        println!("Block anaphora - unnumbered:\n{:#?}", parse.syntax_node());
        assert!(
            parse.ok().is_ok(),
            "Failed to parse unnumbered block anaphora"
        );

        // Numbered anaphora
        let text = r#"test: { third: •2 second: •1 first: •0 }"#;
        let parse = parse_unit(text);
        println!("Block anaphora - numbered:\n{:#?}", parse.syntax_node());
        assert!(
            parse.ok().is_ok(),
            "Failed to parse numbered block anaphora"
        );

        // Anaphora in lambda context
        let text = r#"test: {it: •}"#;
        let parse = parse_unit(text);
        println!("Block anaphora - lambda:\n{:#?}", parse.syntax_node());
        assert!(
            parse.ok().is_ok(),
            "Failed to parse block anaphora in lambda"
        );

        // Test the actual harness file
        let content = std::fs::read_to_string("tests/harness/031_block_anaphora.eu").unwrap();
        let parse = parse_unit(&content);

        if !parse.errors().is_empty() {
            println!("Errors in block anaphora harness: {:?}", parse.errors());
        }

        assert!(
            parse.errors().is_empty(),
            "031_block_anaphora.eu should parse without errors"
        );
        assert!(
            parse.ok().is_ok(),
            "031_block_anaphora.eu should parse successfully"
        );
    }

    #[test]
    fn test_rowan_integration() {
        // Comprehensive integration test for Rowan parser functionality
        // Tests string patterns, block anaphora, and complex structures together

        let complex_code = r#"
        // Test file demonstrating key Rowan parser features
        "Integration test metadata"
        
        string-patterns: {
            basic: "Hello {name}!"
            formatted: "Value: {num:%03d}"
            escaped: "Literal {{braces}} preserved"
            complex: "{a:%04d}-{b:%04f}"
            anaphora: "{:%03d}{:%05x}"
        }
        
        block-anaphora: {
            unnumbered: { x: • y: • }
            numbered: { third: •2 second: •1 first: •0 }
            lambda: [1, 2, 3] map({it: •})
        }
        
        complex-structure: {
            nested: {
                deep: {
                    values: [:a, :b, :c]
                    pattern: "Items: {values}"
                    func: { result: • * •1 }
                }
            }
        }
        
        RESULT: :INTEGRATION_SUCCESS
        "#;

        println!("Testing comprehensive Rowan integration...");
        let parse = parse_unit(complex_code);

        if !parse.errors().is_empty() {
            println!("Integration test errors: {:?}", parse.errors());
            for error in parse.errors() {
                println!("  - {error:?}");
            }
        }

        // Verify parsing succeeds
        assert!(
            parse.errors().is_empty(),
            "Integration test should parse without errors"
        );

        // Get the tree before consuming parse with ok()
        let tree = parse.tree();
        let syntax = tree.syntax();

        // Verify the tree structure is reasonable

        // Should have declarations
        assert!(
            syntax.to_string().contains("string-patterns"),
            "Should contain string-patterns declaration"
        );
        assert!(
            syntax.to_string().contains("block-anaphora"),
            "Should contain block-anaphora declaration"
        );
        assert!(
            syntax.to_string().contains("complex-structure"),
            "Should contain complex-structure declaration"
        );

        println!("✓ Rowan parser integration test passed!");
    }

    #[test]
    pub fn parse_harness_eucalypt_files() {
        for entry in std::fs::read_dir(Path::new("tests/harness"))
            .unwrap()
            .flatten()
        {
            let path = entry.path();
            if path.is_file() && path.ends_with(".eu") {
                println!("parsing {}", path.display());
                parse_eucalypt_file(&path);
            }
        }
    }

    #[test]
    pub fn test_cstring_parsing() {
        use crate::syntax::rowan::kind::SyntaxKind;

        // Simple c-string should parse as a C_STRING literal
        let text = r#"test: c"hello""#;
        let parse = parse_unit(text);
        println!("C-string parse tree:\n{:#?}", parse.syntax_node());

        // Check that the literal token is C_STRING
        let syntax = parse.syntax_node().to_string();
        println!("Syntax: {}", syntax);

        // The tree should contain C_STRING
        let has_cstring = parse.syntax_node().descendants_with_tokens().any(|elem| {
            if let rowan::NodeOrToken::Token(token) = elem {
                token.kind() == SyntaxKind::C_STRING
            } else {
                false
            }
        });
        assert!(has_cstring, "C-string should be parsed as C_STRING token");
    }

    #[test]
    pub fn test_tstring_parsing() {
        use crate::syntax::rowan::kind::SyntaxKind;

        // Simple t-string should parse as a T_STRING literal
        let text = r#"meeting: t"2023-01-15T10:30:00Z""#;
        let parse = parse_unit(text);

        // Should parse without errors
        assert!(
            parse.errors().is_empty(),
            "Valid t-string should parse without errors: {:?}",
            parse.errors()
        );

        // The tree should contain T_STRING
        let has_tstring = parse.syntax_node().descendants_with_tokens().any(|elem| {
            if let rowan::NodeOrToken::Token(token) = elem {
                token.kind() == SyntaxKind::T_STRING
            } else {
                false
            }
        });
        assert!(has_tstring, "T-string should be parsed as T_STRING token");
    }

    #[test]
    pub fn test_tstring_invalid_produces_error() {
        // Invalid t-string content should produce a validation error
        let text = r#"bad: t"not-a-date""#;
        let parse = parse_unit(text);

        assert!(
            !parse.errors().is_empty(),
            "Invalid t-string should produce validation error"
        );
    }

    #[test]
    pub fn test_tstring_date_only_parsing() {
        // Date-only t-string should parse successfully
        let text = r#"birthday: t"1990-06-15""#;
        let parse = parse_unit(text);

        assert!(
            parse.errors().is_empty(),
            "Date-only t-string should parse without errors: {:?}",
            parse.errors()
        );
    }

    #[test]
    pub fn test_cons_pattern_in_list() {
        // A cons pattern [h : t] should parse without errors inside a function param
        let text = r#"{ f([h : t]): h }"#;
        let parse = parse_expr(text);

        assert!(
            parse.errors().is_empty(),
            "Cons pattern [h : t] should parse without errors: {:?}",
            parse.errors()
        );

        // Verify the shape: LIST with COLON token child
        verify_expr(
            "[x : xs]",
            r#"
SOUP@0..8
  LIST@0..8
    OPEN_SQUARE@0..1 "["
    SOUP@1..3
      NAME@1..2
        UNQUOTED_IDENTIFIER@1..2 "x"
      WHITESPACE@2..3 " "
    COLON@3..4 ":"
    WHITESPACE@4..5 " "
    SOUP@5..7
      NAME@5..7
        UNQUOTED_IDENTIFIER@5..7 "xs"
    CLOSE_SQUARE@7..8 "]"
"#,
        );
    }

    // ── W4p2: Declaration-level error recovery tests ──────────────────────────

    /// A block with a leading bare colon (no head) should wrap the malformed
    /// part in an `ERROR` node while leaving other declarations intact.
    #[test]
    pub fn test_error_recovery_bare_colon_wraps_in_error() {
        let text = "{ : bad\ngood: 1 }";
        let parse = parse_unit(text);

        // There should be at least one parse error for the malformed declaration.
        assert!(
            !parse.errors().is_empty(),
            "expected a parse error for the bare-colon declaration, got none"
        );

        // The tree should contain an ERROR node for the malformed declaration.
        let debug = format!("{:#?}", parse.syntax_node());
        assert!(
            debug.contains("ERROR@"),
            "expected ERROR node in parse tree for bare-colon declaration\ntree:\n{debug}"
        );

        // The valid declaration `good: 1` should still produce a DECLARATION node.
        assert!(
            debug.contains("DECLARATION@"),
            "expected DECLARATION node for `good: 1` to survive error recovery\ntree:\n{debug}"
        );
    }

    /// A block where the first declaration is malformed and three valid ones follow —
    /// all three valid declarations should still be present in the parse tree.
    #[test]
    pub fn test_error_recovery_valid_siblings_survive() {
        let text = "{ : bad\na: 1\nb: 2\nc: 3 }";
        let parse = parse_unit(text);

        let debug = format!("{:#?}", parse.syntax_node());

        // All three valid bindings should be present as DECLARATION nodes.
        let decl_count = debug.matches("DECLARATION@").count();
        assert!(
            decl_count >= 3,
            "expected at least 3 DECLARATION nodes but found {decl_count}\ntree:\n{debug}"
        );

        // An ERROR node should isolate the malformed part.
        assert!(
            debug.contains("ERROR@"),
            "expected ERROR node for the malformed declaration\ntree:\n{debug}"
        );
    }

    /// When a valid declaration is sandwiched between two malformed ones, it
    /// should still parse as a DECLARATION.
    #[test]
    pub fn test_error_recovery_valid_sandwiched_between_errors() {
        let text = "{ : bad1\ngood: 42\n: bad2 }";
        let parse = parse_unit(text);

        let debug = format!("{:#?}", parse.syntax_node());

        // The valid binding should survive.
        assert!(
            debug.contains("DECLARATION@"),
            "expected DECLARATION node for `good: 42`\ntree:\n{debug}"
        );
    }
}
