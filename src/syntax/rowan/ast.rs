//! AST types wrapping SyntaxNodes

use std::marker::PhantomData;

use super::{
    kind::{EucalyptLanguage, SyntaxKind, SyntaxNode, SyntaxNodeChildren, SyntaxToken},
    ParseError,
};

use rowan::{ast::AstNode, TextRange, TextSize};
use serde_json::Number;

/// Basic interface of a syntax token (as per Rowan's own AstNode)
pub trait AstToken {
    fn can_cast(token: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &str {
        self.syntax().text()
    }
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: SyntaxNodeChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &SyntaxNode) -> Self {
        AstChildren {
            inner: parent.children(),
            ph: PhantomData,
        }
    }
}

impl<N: AstNode<Language = EucalyptLanguage>> Iterator for AstChildren<N> {
    type Item = N;
    fn next(&mut self) -> Option<N> {
        self.inner.find_map(N::cast)
    }
}

/// Shortcut implementation of AstNode members for an AST struct
/// corresponding directly to a syntax kind
macro_rules! ast_node {
    ($ast:ident, $kind:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $ast(SyntaxNode);
        impl AstNode for $ast {
            type Language = EucalyptLanguage;

            fn can_cast(kind: SyntaxKind) -> bool
            where
                Self: Sized,
            {
                kind == SyntaxKind::$kind
            }

            fn cast(node: SyntaxNode) -> Option<Self>
            where
                Self: Sized,
            {
                if node.kind() == SyntaxKind::$kind {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }
    };
}

/// Shortcut implementation of AstToken members for an AST struct
/// corresponding directly to a syntax kind
macro_rules! ast_token {
    ($ast:ident, $kind:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $ast(SyntaxToken);
        impl AstToken for $ast {
            fn can_cast(kind: SyntaxKind) -> bool
            where
                Self: Sized,
            {
                kind == SyntaxKind::$kind
            }

            fn cast(node: SyntaxToken) -> Option<Self>
            where
                Self: Sized,
            {
                if node.kind() == SyntaxKind::$kind {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxToken {
                &self.0
            }
        }
    };
}

/// Helpers for AST method implementation
mod support {
    use crate::syntax::rowan::kind::{EucalyptLanguage, SyntaxKind};

    use super::{AstChildren, AstNode, AstToken, SyntaxNode, SyntaxToken};

    pub(super) fn child<N: AstNode<Language = EucalyptLanguage>>(parent: &SyntaxNode) -> Option<N> {
        parent.children().find_map(N::cast)
    }

    pub(super) fn children<N: AstNode>(parent: &SyntaxNode) -> AstChildren<N> {
        AstChildren::new(parent)
    }

    pub(super) fn syntax_token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
        parent
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|t| t.kind() == kind)
    }

    pub(super) fn token<T>(parent: &SyntaxNode) -> Option<T>
    where
        T: AstToken,
    {
        parent
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(T::cast)
    }
}

//
// Literal tokens and nodes
//

ast_token!(Sym, SYMBOL);

impl Sym {
    pub fn value(&self) -> Option<&str> {
        self.text().strip_prefix(':').and_then(|s| {
            if s.starts_with('\'') {
                s.strip_prefix('\'').and_then(|s| s.strip_suffix('\''))
            } else {
                Some(s)
            }
        })
    }
}

ast_token!(Str, STRING);
impl Str {
    pub fn value(&self) -> Option<&str> {
        self.text()
            .strip_prefix('\"')
            .and_then(|s| s.strip_suffix('\"'))
    }
}

ast_token!(Num, NUMBER);

impl Num {
    pub fn value(&self) -> Option<Number> {
        self.text().parse().ok()
    }
}

pub enum LiteralValue {
    /// A symbol e.g. :foo
    Sym(Sym),
    /// A string e.g. "foo"
    Str(Str),
    /// A number e.g. 99
    Num(Num),
}

impl AstToken for LiteralValue {
    fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            SyntaxKind::NUMBER => Num::cast(token).map(LiteralValue::Num),
            SyntaxKind::STRING => Str::cast(token).map(LiteralValue::Str),
            SyntaxKind::SYMBOL => Sym::cast(token).map(LiteralValue::Sym),
            _ => None,
        }
    }

    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        matches!(
            kind,
            SyntaxKind::NUMBER | SyntaxKind::STRING | SyntaxKind::SYMBOL
        )
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            LiteralValue::Num(n) => n.syntax(),
            LiteralValue::Str(s) => s.syntax(),
            LiteralValue::Sym(s) => s.syntax(),
        }
    }
}

impl LiteralValue {
    pub fn symbol_name(&self) -> Option<&str> {
        if let LiteralValue::Sym(s) = self {
            s.value()
        } else {
            None
        }
    }

    pub fn string_value(&self) -> Option<&str> {
        if let LiteralValue::Str(s) = self {
            s.value()
        } else {
            None
        }
    }

    pub fn number_value(&self) -> Option<Number> {
        if let LiteralValue::Num(n) = self {
            n.value()
        } else {
            None
        }
    }
}

ast_node!(Literal, LITERAL);

impl Literal {
    pub fn value(&self) -> Option<LiteralValue> {
        support::token::<LiteralValue>(&self.0)
    }
}

//
// Identifier tokens and nodes
//

pub trait ContainsName {
    fn name_range(&self) -> Option<TextRange>;
}

ast_token!(UnquotedIdentifier, UNQUOTED_IDENTIFIER);

impl ContainsName for UnquotedIdentifier {
    fn name_range(&self) -> Option<TextRange> {
        Some(TextRange::up_to(TextSize::of(self.text())))
    }
}

ast_token!(OperatorIdentifier, OPERATOR_IDENTIFIER);

impl ContainsName for OperatorIdentifier {
    fn name_range(&self) -> Option<TextRange> {
        Some(TextRange::up_to(TextSize::of(self.text())))
    }
}

ast_token!(SingleQuoteIdentifier, SINGLE_QUOTE_IDENTIFIER);

impl ContainsName for SingleQuoteIdentifier {
    fn name_range(&self) -> Option<TextRange> {
        let text = self.text();
        let lquote = text.find('\'');
        let rquote = text.find('\'');
        match (lquote, rquote) {
            (Some(0), Some(n)) if n == text.len() - 1 => {
                Some(TextRange::new(1.into(), TextSize::try_from(n).unwrap()))
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum NormalIdentifier {
    /// e.g. foo
    UnquotedIdentifier(UnquotedIdentifier),
    /// e.g. 'foo'
    SingleQuoteIdentifier(SingleQuoteIdentifier),
}

impl AstToken for NormalIdentifier {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        matches!(
            kind,
            SyntaxKind::UNQUOTED_IDENTIFIER | SyntaxKind::SINGLE_QUOTE_IDENTIFIER
        )
    }

    fn cast(token: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        match token.kind() {
            SyntaxKind::UNQUOTED_IDENTIFIER => {
                UnquotedIdentifier::cast(token).map(Self::UnquotedIdentifier)
            }
            SyntaxKind::SINGLE_QUOTE_IDENTIFIER => {
                SingleQuoteIdentifier::cast(token).map(Self::SingleQuoteIdentifier)
            }
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            NormalIdentifier::UnquotedIdentifier(n) => n.syntax(),
            NormalIdentifier::SingleQuoteIdentifier(n) => n.syntax(),
        }
    }
}

impl ContainsName for NormalIdentifier {
    fn name_range(&self) -> Option<TextRange> {
        match self {
            NormalIdentifier::UnquotedIdentifier(t) => t.name_range(),
            NormalIdentifier::SingleQuoteIdentifier(t) => t.name_range(),
        }
    }
}

/// An identifier token
pub enum Identifier {
    /// A normal identifier e.g. foo
    NormalIdentifier(NormalIdentifier),
    /// An operator identifier e.g. ++
    OperatorIdentifier(OperatorIdentifier),
}

impl AstToken for Identifier {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        NormalIdentifier::can_cast(kind) || OperatorIdentifier::can_cast(kind)
    }

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        if NormalIdentifier::can_cast(syntax.kind()) {
            NormalIdentifier::cast(syntax).map(Self::NormalIdentifier)
        } else {
            OperatorIdentifier::cast(syntax).map(Self::OperatorIdentifier)
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            Identifier::NormalIdentifier(n) => n.syntax(),
            Identifier::OperatorIdentifier(n) => n.syntax(),
        }
    }
}

impl Identifier {
    pub fn name(&self) -> Option<&str> {
        let range = match self {
            Identifier::NormalIdentifier(n) => n.name_range(),
            Identifier::OperatorIdentifier(n) => n.name_range(),
        };
        range.map(|range| &self.syntax().text()[range])
    }

    pub fn as_normal(&self) -> Option<NormalIdentifier> {
        if let Identifier::NormalIdentifier(n) = self {
            Some(n.clone())
        } else {
            None
        }
    }
}

ast_node!(Name, NAME);

impl Name {
    pub fn identifier(&self) -> Option<Identifier> {
        support::token::<Identifier>(&self.0)
    }
}

//
// Expression elements
//

pub trait HasSoup: AstNode<Language = EucalyptLanguage> {
    fn soup(&self) -> Option<Soup> {
        support::child::<Soup>(self.syntax())
    }
}

ast_node!(Soup, SOUP);

impl Soup {
    pub fn elements(&self) -> AstChildren<Element> {
        support::children::<Element>(self.syntax())
    }

    /// return the one and only element if it exists
    pub fn singleton(&self) -> Option<Element> {
        let mut elements = self.elements();
        let existing = elements.next();
        let unique = elements.next().is_none();

        if unique {
            existing
        } else {
            None
        }
    }
}

ast_node!(ParenExpr, PAREN_EXPR);

impl ParenExpr {
    pub fn open_paren(&self) -> Option<SyntaxToken> {
        support::syntax_token(self.syntax(), SyntaxKind::OPEN_PAREN)
    }

    pub fn close_paren(&self) -> Option<SyntaxToken> {
        support::syntax_token(self.syntax(), SyntaxKind::CLOSE_PAREN)
    }
}

impl HasSoup for ParenExpr {}

ast_node!(BlockMetadata, BLOCK_META);

impl HasSoup for BlockMetadata {}

ast_node!(DeclarationMetadata, DECL_META);

impl HasSoup for DeclarationMetadata {}

/// The kind of declaration signalled by the format of the declaration head
pub enum DeclarationKind {
    /// e.g. x: ...
    Property(NormalIdentifier),
    /// e.g. f(x, y, z): ...
    Function(NormalIdentifier, ApplyTuple),
    /// e.g (∅): ...
    Nullary(ParenExpr, OperatorIdentifier),
    /// e.g. (!x): ...
    Prefix(ParenExpr, OperatorIdentifier, NormalIdentifier),
    /// e.g. (x^^): ...
    Postfix(ParenExpr, NormalIdentifier, OperatorIdentifier),
    /// e.g. (x + y): ...
    Binary(
        ParenExpr,
        NormalIdentifier,
        OperatorIdentifier,
        NormalIdentifier,
    ),
    /// anything else
    MalformedHead(Vec<ParseError>),
}

ast_node!(DeclarationHead, DECL_HEAD);

/// Classify a paren expression into a DeclarationKind
fn classify_operator(pe: ParenExpr) -> DeclarationKind {
    let elements: Vec<_> = pe
        .soup()
        .map(|s| s.elements().collect())
        .unwrap_or_default();
    match elements.len() {
        0 => DeclarationKind::MalformedHead(vec![ParseError::MalformedDeclarationHead {
            range: pe.syntax().text_range(),
        }]),
        1 => {
            // nullary
            if let Some(op) = elements[0].as_operator_identifier() {
                DeclarationKind::Nullary(pe, op)
            } else {
                DeclarationKind::MalformedHead(vec![ParseError::InvalidOperatorName {
                    head_range: pe.syntax().text_range(),
                    range: elements[0].syntax().text_range(),
                }])
            }
        }
        2 => {
            // unary
            if let Some(prefix_op) = elements[0].as_operator_identifier() {
                if let Some(operand) = elements[1].as_normal_identifier() {
                    DeclarationKind::Prefix(pe, prefix_op, operand)
                } else {
                    DeclarationKind::MalformedHead(vec![ParseError::InvalidFormalParameter {
                        head_range: pe.syntax().text_range(),
                        range: elements[1].syntax().text_range(),
                    }])
                }
            } else if let Some(operand) = elements[0].as_normal_identifier() {
                if let Some(postfix_op) = elements[1].as_operator_identifier() {
                    DeclarationKind::Postfix(pe, operand, postfix_op)
                } else {
                    DeclarationKind::MalformedHead(vec![ParseError::InvalidOperatorName {
                        head_range: pe.syntax().text_range(),
                        range: elements[1].syntax().text_range(),
                    }])
                }
            } else {
                DeclarationKind::MalformedHead(vec![ParseError::MalformedDeclarationHead {
                    range: pe.syntax().text_range(),
                }])
            }
        }
        3 => {
            // binary
            if let Some(x) = elements[0].as_normal_identifier() {
                if let Some(op) = elements[1].as_operator_identifier() {
                    if let Some(y) = elements[2].as_normal_identifier() {
                        DeclarationKind::Binary(pe, x, op, y)
                    } else {
                        DeclarationKind::MalformedHead(vec![ParseError::InvalidFormalParameter {
                            head_range: pe.syntax().text_range(),
                            range: elements[2].syntax().text_range(),
                        }])
                    }
                } else {
                    DeclarationKind::MalformedHead(vec![ParseError::InvalidOperatorName {
                        head_range: pe.syntax().text_range(),
                        range: elements[1].syntax().text_range(),
                    }])
                }
            } else {
                DeclarationKind::MalformedHead(vec![ParseError::InvalidFormalParameter {
                    head_range: pe.syntax().text_range(),
                    range: elements[0].syntax().text_range(),
                }])
            }
        }
        _ => DeclarationKind::MalformedHead(vec![ParseError::MalformedDeclarationHead {
            range: pe.syntax().text_range(),
        }]),
    }
}

impl DeclarationHead {
    /// Classify a DeclarationHead into a DeclarationKind
    pub fn classify_declaration(&self) -> DeclarationKind {
        let items: Vec<_> = self
            .syntax()
            .children()
            .filter(|e| e.kind() != SyntaxKind::COMMENT && e.kind() != SyntaxKind::WHITESPACE)
            .collect();

        let malformed = || {
            DeclarationKind::MalformedHead(vec![ParseError::MalformedDeclarationHead {
                range: self.syntax().text_range(),
            }])
        };

        match items.len() {
            // single item must be property or operator
            1 => {
                if let Some(n) = Name::cast(items[0].clone()) {
                    let prop = n.identifier().and_then(|id| id.as_normal());
                    if let Some(prop) = prop {
                        DeclarationKind::Property(prop)
                    } else {
                        DeclarationKind::MalformedHead(vec![ParseError::InvalidPropertyName {
                            head_range: self.syntax().text_range(),
                            range: n.syntax().text_range(),
                        }])
                    }
                } else if let Some(pe) = ParenExpr::cast(items[0].clone()) {
                    classify_operator(pe)
                } else {
                    malformed()
                }
            }
            // two items must be function head: f(x, y, z)
            2 => {
                // must be function
                let f = Name::cast(items[0].clone())
                    .and_then(|n| n.identifier())
                    .and_then(|id| id.as_normal());
                let args = ApplyTuple::cast(items[1].clone());

                if let Some(f) = f {
                    if let Some(args) = args {
                        let mut errors = vec![];
                        for arg in args.items() {
                            if arg
                                .singleton()
                                .and_then(|e| e.as_normal_identifier())
                                .is_none()
                            {
                                errors.push(ParseError::InvalidFormalParameter {
                                    head_range: self.syntax().text_range(),
                                    range: arg.syntax().text_range(),
                                })
                            }
                        }
                        if !errors.is_empty() {
                            DeclarationKind::MalformedHead(errors)
                        } else {
                            DeclarationKind::Function(f, args)
                        }
                    } else {
                        malformed()
                    }
                } else {
                    malformed()
                }
            }
            _ => malformed(),
        }
    }
}

ast_node!(DeclarationBody, DECL_BODY);

impl HasSoup for DeclarationBody {}

ast_node!(Declaration, DECLARATION);

impl Declaration {
    pub fn meta(&self) -> Option<DeclarationMetadata> {
        support::child::<DeclarationMetadata>(&self.0)
    }

    pub fn head(&self) -> Option<DeclarationHead> {
        support::child::<DeclarationHead>(&self.0)
    }

    pub fn colon(&self) -> Option<SyntaxToken> {
        support::syntax_token(self.syntax(), SyntaxKind::COLON)
    }

    pub fn body(&self) -> Option<DeclarationBody> {
        support::child::<DeclarationBody>(&self.0)
    }
}

ast_node!(Block, BLOCK);

impl Block {
    pub fn meta(&self) -> Option<BlockMetadata> {
        support::child::<BlockMetadata>(&self.0)
    }

    pub fn declarations(&self) -> AstChildren<Declaration> {
        support::children::<Declaration>(&self.0)
    }

    pub fn open_brace(&self) -> Option<SyntaxToken> {
        support::syntax_token(self.syntax(), SyntaxKind::OPEN_BRACE)
    }

    pub fn close_brace(&self) -> Option<SyntaxToken> {
        support::syntax_token(self.syntax(), SyntaxKind::CLOSE_BRACE)
    }
}

ast_node!(List, LIST);

impl List {
    pub fn items(&self) -> AstChildren<Soup> {
        support::children::<Soup>(&self.0)
    }
}

ast_node!(ApplyTuple, ARG_TUPLE);

impl ApplyTuple {
    pub fn items(&self) -> AstChildren<Soup> {
        support::children::<Soup>(&self.0)
    }

    pub fn open_paren(&self) -> Option<SyntaxToken> {
        support::syntax_token(self.syntax(), SyntaxKind::OPEN_PAREN_APPLY)
    }

    pub fn close_paren(&self) -> Option<SyntaxToken> {
        support::syntax_token(self.syntax(), SyntaxKind::CLOSE_PAREN)
    }
}

//
// String Pattern AST nodes
//

ast_node!(StringPattern, STRING_PATTERN);
ast_node!(StringLiteralContent, STRING_LITERAL_CONTENT);
ast_node!(StringInterpolation, STRING_INTERPOLATION);
ast_node!(StringInterpolationTarget, STRING_INTERPOLATION_TARGET);
ast_node!(StringFormatSpec, STRING_FORMAT_SPEC);
ast_node!(StringConversionSpec, STRING_CONVERSION_SPEC);
ast_node!(StringDottedReference, STRING_DOTTED_REFERENCE);
ast_node!(StringEscapedOpen, STRING_ESCAPED_OPEN);
ast_node!(StringEscapedClose, STRING_ESCAPED_CLOSE);

impl StringPattern {
    pub fn chunks(&self) -> AstChildren<StringChunk> {
        support::children::<StringChunk>(self.syntax())
    }
}

impl StringLiteralContent {
    pub fn value(&self) -> Option<String> {
        self.syntax().first_token().map(|t| t.text().to_string())
    }
}

impl StringInterpolation {
    pub fn target(&self) -> Option<StringInterpolationTarget> {
        support::child::<StringInterpolationTarget>(self.syntax())
    }
    
    pub fn format_spec(&self) -> Option<StringFormatSpec> {
        support::child::<StringFormatSpec>(self.syntax())
    }
    
    pub fn conversion_spec(&self) -> Option<StringConversionSpec> {
        support::child::<StringConversionSpec>(self.syntax())
    }
}

impl StringInterpolationTarget {
    pub fn value(&self) -> Option<String> {
        self.syntax().first_token().map(|t| t.text().to_string())
    }
}

impl StringFormatSpec {
    pub fn value(&self) -> Option<String> {
        self.syntax().first_token().map(|t| t.text().to_string())
    }
}

impl StringConversionSpec {
    pub fn value(&self) -> Option<String> {
        self.syntax().first_token().map(|t| t.text().to_string())
    }
}

impl StringEscapedOpen {
    pub fn value(&self) -> &str {
        "{"
    }
}

impl StringEscapedClose {
    pub fn value(&self) -> &str {
        "}"
    }
}

// String chunk enum for pattern contents
pub enum StringChunk {
    LiteralContent(StringLiteralContent),
    Interpolation(StringInterpolation),
    EscapedOpen(StringEscapedOpen),
    EscapedClose(StringEscapedClose),
}

impl AstNode for StringChunk {
    type Language = EucalyptLanguage;

    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, 
            SyntaxKind::STRING_LITERAL_CONTENT |
            SyntaxKind::STRING_INTERPOLATION |
            SyntaxKind::STRING_ESCAPED_OPEN |
            SyntaxKind::STRING_ESCAPED_CLOSE
        )
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::STRING_LITERAL_CONTENT => {
                StringLiteralContent::cast(node).map(StringChunk::LiteralContent)
            }
            SyntaxKind::STRING_INTERPOLATION => {
                StringInterpolation::cast(node).map(StringChunk::Interpolation)
            }
            SyntaxKind::STRING_ESCAPED_OPEN => {
                StringEscapedOpen::cast(node).map(StringChunk::EscapedOpen)
            }
            SyntaxKind::STRING_ESCAPED_CLOSE => {
                StringEscapedClose::cast(node).map(StringChunk::EscapedClose)
            }
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            StringChunk::LiteralContent(n) => n.syntax(),
            StringChunk::Interpolation(n) => n.syntax(),
            StringChunk::EscapedOpen(n) => n.syntax(),
            StringChunk::EscapedClose(n) => n.syntax(),
        }
    }
}

/// One of the items concatenated in an operator soup expression
pub enum Element {
    Lit(Literal),
    Block(Block),
    List(List),
    ParenExpr(ParenExpr),
    Name(Name),
    StringPattern(StringPattern),
    ApplyTuple(ApplyTuple),
}

impl AstNode for Element {
    type Language = EucalyptLanguage;

    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        matches!(
            kind,
            SyntaxKind::LITERAL
                | SyntaxKind::BLOCK
                | SyntaxKind::LIST
                | SyntaxKind::PAREN_EXPR
                | SyntaxKind::NAME
                | SyntaxKind::STRING_PATTERN
                | SyntaxKind::ARG_TUPLE
        )
    }

    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        match node.kind() {
            SyntaxKind::LITERAL => Literal::cast(node).map(Element::Lit),
            SyntaxKind::LIST => List::cast(node).map(Element::List),
            SyntaxKind::BLOCK => Block::cast(node).map(Element::Block),
            SyntaxKind::PAREN_EXPR => ParenExpr::cast(node).map(Element::ParenExpr),
            SyntaxKind::ARG_TUPLE => ApplyTuple::cast(node).map(Element::ApplyTuple),
            SyntaxKind::NAME => Name::cast(node).map(Element::Name),
            SyntaxKind::STRING_PATTERN => StringPattern::cast(node).map(Element::StringPattern),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Element::Lit(l) => l.syntax(),
            Element::Block(b) => b.syntax(),
            Element::List(l) => l.syntax(),
            Element::ParenExpr(e) => e.syntax(),
            Element::Name(n) => n.syntax(),
            Element::StringPattern(s) => s.syntax(),
            Element::ApplyTuple(t) => t.syntax(),
        }
    }
}

impl Element {
    /// Cast to normal identifier if possible
    ///
    /// Useful in checking formal parameters of function or operator declaration
    pub fn as_normal_identifier(&self) -> Option<NormalIdentifier> {
        match self {
            Element::Name(n) => n.identifier().and_then(|id| match id {
                Identifier::NormalIdentifier(id) => Some(id),
                _ => None,
            }),
            _ => None,
        }
    }

    /// Cast to operator identifier if possible
    ///
    /// Useful in checking formal parameters of operator declaration
    pub fn as_operator_identifier(&self) -> Option<OperatorIdentifier> {
        match self {
            Element::Name(n) => n.identifier().and_then(|id| match id {
                Identifier::OperatorIdentifier(id) => Some(id),
                _ => None,
            }),
            _ => None,
        }
    }
}

//
// Unit
//

ast_node!(Unit, UNIT);

impl Unit {
    pub fn meta(&self) -> Option<BlockMetadata> {
        support::child::<BlockMetadata>(&self.0)
    }

    pub fn declarations(&self) -> AstChildren<Declaration> {
        support::children::<Declaration>(&self.0)
    }
}
