//! Rowan parser

use std::marker::PhantomData;
use std::mem::swap;

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
    /// Construct directly from text
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

    /// Operate on the event sink at the top of the stack
    fn sink(&mut self) -> &mut dyn EventSink {
        self.sink_stack.last_mut().unwrap().as_mut()
    }

    /// Pop and complete the top event sink and pass its events to the
    /// next sink in the stack
    fn finish_sink(&mut self) -> Vec<ParseEvent> {
        if let Some(mut sink) = self.sink_stack.pop() {
            sink.as_mut().complete()
        } else {
            unreachable!()
        }
    }

    /// Convert the parse events into a GreenNode
    fn build(&mut self) -> GreenNode {
        let mut events = self.finish_sink();
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
        if let Some((k, _text)) = self.next() {
            if k.is_literal_terminal() || k == STRING_PATTERN_START {
                if k == STRING_PATTERN_START {
                    // This is a string pattern, parse it specially
                    self.parse_string_pattern();
                    true
                } else {
                    // Regular literal
                    self.sink().start_node(LITERAL);
                    self.sink().token(k);
                    self.sink().finish_node();
                    true
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
        if let Some((k, _)) = self.next() {
            if k.is_name_terminal() {
                self.sink().start_node(NAME);
                self.sink().token(k);
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
            Some((RESERVED_OPEN, _)) => {
                self.parse_reserved_paren_expression();
                true
            }
            Some((_k, _s)) => self.try_parse_atom(),
            None => false,
        }
    }

    /// Parse a list expression [x, y, z]
    fn try_parse_list_expression(&mut self) -> bool {
        if let Some((k, _)) = self.next() {
            if k == OPEN_SQUARE {
                self.sink().start_node(LIST);
                self.sink().token(k);
                self.add_trivia();
                while self.try_parse_soup() {
                    if !self.try_accept(COMMA) {
                        break;
                    }
                    self.add_trivia();
                }
                self.add_trivia();
                self.expect(CLOSE_SQUARE);
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
                CLOSE_BRACE | CLOSE_PAREN | CLOSE_SQUARE | RESERVED_CLOSE | COMMA
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
                Some((WHITESPACE, _)) => self.sink().token(WHITESPACE),
                Some((COMMENT, _)) => self.sink().token(COMMENT),
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
        self.expect(CLOSE_PAREN);
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
        self.expect(CLOSE_PAREN);
        // if instead unterminated, there will be a missing close paren

        self.sink().finish_node();
    }

    /// Parse a block expression (next token is '{')
    fn parse_block_expression(&mut self) {
        self.sink().start_node(BLOCK);
        self.expect(OPEN_BRACE);
        self.add_trivia();
        self.parse_block_content();
        self.add_trivia();
        self.expect(CLOSE_BRACE);
        self.sink().finish_node();
    }

    /// Parse content of block or unit
    fn parse_block_content(&mut self) {
        self.sink_stack.push(Box::new(BlockEventSink::default()));
        while self.parse_protoblock_element() {}
        let events = self.finish_sink();
        self.sink().accept(events);
    }

    /// Parse a temporary protoblock element
    fn parse_protoblock_element(&mut self) -> bool {
        match self.next() {
            Some((k, _))
                if k == BACKTICK || k == COMMA || k == COLON || k == WHITESPACE || k == COMMENT =>
            {
                self.sink().token(k);
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

    /// Expects and adds as leaf a particular token, otherwise adding
    /// an ERROR and returning false
    fn expect(&mut self, kind: SyntaxKind) -> bool {
        if let Some((k, _)) = self.next() {
            if k == kind {
                self.sink().token(k);
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

    fn try_accept(&mut self, kind: SyntaxKind) -> bool {
        if let Some((k, _)) = self.next() {
            if k == kind {
                self.sink().token(k);
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
        while let Some((k, _)) = self.peek() {
            if k.is_trivial() {
                self.next();
                self.sink().token(k);
            } else {
                break;
            }
        }
    }

    /// Parse a string pattern with interpolation
    fn parse_string_pattern(&mut self) {
        self.sink().start_node(STRING_PATTERN);
        self.sink().token(STRING_PATTERN_START); // consume opening quote

        // Process tokens until we reach STRING_PATTERN_END
        while let Some((kind, _text)) = self.peek() {
            match kind {
                STRING_PATTERN_END => {
                    self.sink().token(STRING_PATTERN_END);
                    self.next(); // consume closing quote
                    break;
                }
                STRING_LITERAL_CONTENT => {
                    self.sink().start_node(STRING_LITERAL_CONTENT);
                    self.sink().token(STRING_LITERAL_CONTENT);
                    self.next();
                    self.sink().finish_node();
                }
                STRING_ESCAPED_OPEN => {
                    self.sink().start_node(STRING_ESCAPED_OPEN);
                    self.sink().token(STRING_ESCAPED_OPEN);
                    self.next();
                    self.sink().finish_node();
                }
                STRING_ESCAPED_CLOSE => {
                    self.sink().start_node(STRING_ESCAPED_CLOSE);
                    self.sink().token(STRING_ESCAPED_CLOSE);
                    self.next();
                    self.sink().finish_node();
                }
                OPEN_BRACE => {
                    // Start of interpolation
                    self.sink().start_node(STRING_INTERPOLATION);
                    self.sink().token(OPEN_BRACE);
                    self.next(); // consume {

                    // Parse interpolation content
                    self.parse_string_interpolation_content();

                    // Consume closing brace
                    if let Some((CLOSE_BRACE, _)) = self.peek() {
                        self.sink().token(CLOSE_BRACE);
                        self.next();
                    }

                    self.sink().finish_node(); // end STRING_INTERPOLATION
                }
                _ => {
                    // Unexpected token - consume and continue
                    self.next();
                }
            }
        }

        self.sink().finish_node(); // end STRING_PATTERN
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
            if let Some((STRING_INTERPOLATION_TARGET, _)) = self.peek() {
                self.sink().start_node(NAME);
                self.sink().token(STRING_INTERPOLATION_TARGET);
                self.next();
                self.sink().finish_node(); // end NAME
            }

            // Parse alternating dots and targets
            while let Some((OPERATOR_IDENTIFIER, text)) = self.peek() {
                if text == "." {
                    // Parse the dot
                    self.sink().start_node(NAME);
                    self.sink().token(OPERATOR_IDENTIFIER);
                    self.next();
                    self.sink().finish_node(); // end NAME

                    // Parse the following target
                    if let Some((STRING_INTERPOLATION_TARGET, _)) = self.peek() {
                        self.sink().start_node(NAME);
                        self.sink().token(STRING_INTERPOLATION_TARGET);
                        self.next();
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
            if let Some((STRING_INTERPOLATION_TARGET, _)) = self.peek() {
                self.sink().start_node(STRING_INTERPOLATION_TARGET);
                self.sink().token(STRING_INTERPOLATION_TARGET);
                self.next();
                self.sink().finish_node();
            }
        }

        // Check for format spec (colon followed by format)
        if let Some((COLON, _)) = self.peek() {
            self.sink().token(COLON);
            self.next(); // consume :

            if let Some((STRING_FORMAT_SPEC, _)) = self.peek() {
                self.sink().start_node(STRING_FORMAT_SPEC);
                self.sink().token(STRING_FORMAT_SPEC);
                self.next();
                self.sink().finish_node();
            }
        }
    }
}

trait EventSink {
    fn start_node(&mut self, kind: SyntaxKind);
    fn finish_node(&mut self);
    fn accept(&mut self, events: Vec<ParseEvent>);
    fn token(&mut self, kind: SyntaxKind);
    fn complete(&mut self) -> Vec<ParseEvent>;
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

    fn accept(&mut self, events: Vec<ParseEvent>) {
        self.0.extend(events);
    }

    fn token(&mut self, kind: SyntaxKind) {
        self.0.push(ParseEvent::Token(kind));
    }

    fn complete(&mut self) -> Vec<ParseEvent> {
        let mut ret = vec![];
        swap(&mut self.0, &mut ret);
        ret
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

impl EventSink for BlockEventSink {
    fn start_node(&mut self, kind: SyntaxKind) {
        self.depth += 1;
        self.buffer.push(ParseEvent::StartNode(kind))
    }

    fn finish_node(&mut self) {
        self.depth -= 1;
        self.buffer.push(ParseEvent::Finish)
    }

    fn accept(&mut self, events: Vec<ParseEvent>) {
        self.buffer.extend(events);
    }

    fn token(&mut self, kind: SyntaxKind) {
        if self.depth == 0 {
            match kind {
                BACKTICK => {
                    if !self.buffer.is_empty() {
                        match self.declaration {
                            Some(ref mut d) => {
                                swap(&mut d.body, &mut self.buffer);
                                self.commit_declaration();
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
                            }
                        }
                        None => {
                            // error case '{ x , ... }'
                            let mut head = vec![];
                            swap(&mut head, &mut self.buffer);
                            self.declaration = Some(PendingDeclaration {
                                meta: None,
                                head, // TODO: add errors
                                colon: vec![],
                                body: vec![],
                            });
                            self.commit_declaration();
                        }
                    }
                }
                _ => self.buffer.push(ParseEvent::Token(kind)),
            }
        } else {
            self.buffer.push(ParseEvent::Token(kind))
        }
    }

    /// Complete the block parse and return the events
    fn complete(&mut self) -> Vec<ParseEvent> {
        let mut ret = vec![];
        if !self.buffer.is_empty() {
            match self.declaration {
                Some(ref mut d) => {
                    // expecting body of last block
                    swap(&mut d.body, &mut self.buffer);
                    self.commit_declaration();
                }
                None => {
                    // TODO not if we already have declarations

                    swap(&mut self.block_meta, &mut self.buffer);
                    self.commit_meta();
                }
            }
        }
        swap(&mut self.committed, &mut ret);
        ret
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

    /// Pending declaration is complete, copy it to the underlying builder
    fn commit_declaration(&mut self) {
        if let Some(ref mut decl) = self.declaration {
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
                ParseEvent::StartNode(PAREN_EXPR) | ParseEvent::StartNode(NAME) => {
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
        assert_eq!(format!("\n{}", parse.syntax_node()), parse_repr);
        assert!(parse.ok().is_ok());
    }

    fn verify_unit(text: &str, parse_repr: &str) {
        let parse = parse_unit(text);
        assert_eq!(format!("\n{}", parse.syntax_node()), parse_repr);
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

        // TODO string patterns

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

        // TODO rejects
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
            "harness/test/024_interpolation.eu",
            "harness/test/041_numeric_formats.eu",
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
        let content = std::fs::read_to_string("harness/test/031_block_anaphora.eu").unwrap();
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
        for entry in std::fs::read_dir(Path::new("harness/test"))
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
}
