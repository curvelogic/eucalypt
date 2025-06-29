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

/// Literal value types
/// 
/// AST embedding syntax:
/// - `Sym`: `[:a-sym "name"]` - Symbol literal (e.g. `:foo`)
/// - `Str`: `[:a-str "text"]` - String literal (e.g. `"hello"`) 
/// - `Num`: `[:a-num value]` - Number literal (e.g. `42`, `3.14`)
pub enum LiteralValue {
    /// A symbol e.g. :foo - Embedding: `[:a-sym "foo"]`
    Sym(Sym),
    /// A string e.g. "foo" - Embedding: `[:a-str "foo"]`
    Str(Str),
    /// A number e.g. 99 - Embedding: `[:a-num 99]`
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

/// A literal value node
/// 
/// AST embedding syntax:
/// - `[:a-literal value]` - Contains a literal value (symbol, string, or number)
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
        let rquote = text.rfind('\'');
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
/// 
/// AST embedding syntax:
/// - `NormalIdentifier`: `[:a-norm "name"]` - Normal identifier (e.g. `x`, `'quoted'`)
/// - `OperatorIdentifier`: `[:a-oper "operator"]` - Operator identifier (e.g. `+`, `&&`)
pub enum Identifier {
    /// A normal identifier e.g. foo - Embedding: `[:a-norm "name"]`
    NormalIdentifier(NormalIdentifier),
    /// An operator identifier e.g. ++ - Embedding: `[:a-oper "operator"]`
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

/// A name (identifier) node
/// 
/// AST embedding syntax:
/// - `[:a-name identifier]` - Contains an identifier (normal or operator)
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

/// A soup expression (sequence of elements for operator resolution)
/// 
/// AST embedding syntax:
/// - `[:a-soup elements...]` - Operator soup containing elements to be resolved
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

/// A parenthesised expression
/// 
/// AST embedding syntax:
/// - `[:a-paren-expr soup]` - Expression enclosed in parentheses
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

/// Metadata for a block expression
/// 
/// AST embedding syntax:
/// - `[:a-block-meta soup]` - Metadata expression for a block
ast_node!(BlockMetadata, BLOCK_META);

impl HasSoup for BlockMetadata {}

/// Metadata for a declaration
/// 
/// AST embedding syntax:
/// - `[:a-decl-meta soup]` - Metadata expression for a declaration
ast_node!(DeclarationMetadata, DECL_META);

impl HasSoup for DeclarationMetadata {}

/// The kind of declaration signalled by the format of the declaration head
/// 
/// AST embedding syntax:
/// - `Property`: `[:a-decl-prop name]` - Property declaration (e.g. `x: ...`)
/// - `Function`: `[:a-decl-func name args]` - Function declaration (e.g. `f(x, y): ...`)
/// - `Nullary`: `[:a-decl-nullary paren op]` - Nullary operator (e.g. `(∅): ...`)
/// - `Prefix`: `[:a-decl-prefix paren op param]` - Prefix operator (e.g. `(!x): ...`)
/// - `Postfix`: `[:a-decl-postfix paren param op]` - Postfix operator (e.g. `(x^^): ...`)
/// - `Binary`: `[:a-decl-binary paren left op right]` - Binary operator (e.g. `(x + y): ...`)
/// - `MalformedHead`: `[:a-decl-malformed errors...]` - Invalid declaration head
pub enum DeclarationKind {
    /// Property declaration (e.g. x: ...) - Embedding: `[:a-decl-prop name]`
    Property(NormalIdentifier),
    /// Function declaration (e.g. f(x, y, z): ...) - Embedding: `[:a-decl-func name args]`
    Function(NormalIdentifier, ApplyTuple),
    /// Nullary operator (e.g (∅): ...) - Embedding: `[:a-decl-nullary paren op]`
    Nullary(ParenExpr, OperatorIdentifier),
    /// Prefix operator (e.g. (!x): ...) - Embedding: `[:a-decl-prefix paren op param]`
    Prefix(ParenExpr, OperatorIdentifier, NormalIdentifier),
    /// Postfix operator (e.g. (x^^): ...) - Embedding: `[:a-decl-postfix paren param op]`
    Postfix(ParenExpr, NormalIdentifier, OperatorIdentifier),
    /// Binary operator (e.g. (x + y): ...) - Embedding: `[:a-decl-binary paren left op right]`
    Binary(
        ParenExpr,
        NormalIdentifier,
        OperatorIdentifier,
        NormalIdentifier,
    ),
    /// Invalid declaration head - Embedding: `[:a-decl-malformed errors...]`
    MalformedHead(Vec<ParseError>),
}

/// The head of a declaration (name and parameters)
/// 
/// AST embedding syntax:
/// - `[:a-decl-head elements...]` - Declaration head containing name and parameter patterns
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

/// The body of a declaration (expression after colon)
/// 
/// AST embedding syntax:
/// - `[:a-decl-body soup]` - Declaration body containing the defining expression
ast_node!(DeclarationBody, DECL_BODY);

impl HasSoup for DeclarationBody {}

/// A declaration in a block
/// 
/// AST embedding syntax:
/// - `[:a-declaration head colon body metadata]` - Complete declaration with head, body and optional metadata
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

/// A block expression with declarations
/// 
/// AST embedding syntax:
/// - `[:a-block declarations... metadata]` - Block containing declarations and optional metadata
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

/// A list literal expression
/// 
/// AST embedding syntax:
/// - `[:a-list items...]` - List containing comma-separated items
ast_node!(List, LIST);

impl List {
    pub fn items(&self) -> AstChildren<Soup> {
        support::children::<Soup>(&self.0)
    }
}

/// Function application arguments tuple
/// 
/// AST embedding syntax:
/// - `[:a-apply-tuple args...]` - Function application arguments in parentheses
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

/// A string pattern with interpolation
/// 
/// AST embedding syntax:
/// - `[:a-string-pattern chunks...]` - String with interpolation and literal chunks
ast_node!(StringPattern, STRING_PATTERN);

/// Literal text content in a string pattern
/// 
/// AST embedding syntax:
/// - `[:a-str-literal "text"]` - Plain text content within string
ast_node!(StringLiteralContent, STRING_LITERAL_CONTENT);

/// Variable interpolation in a string pattern  
/// 
/// AST embedding syntax:
/// - `[:a-str-interp target format conversion]` - Variable interpolation with optional formatting
ast_node!(StringInterpolation, STRING_INTERPOLATION);

/// Target of string interpolation (variable reference)
/// 
/// AST embedding syntax:
/// - `[:a-str-target "name"]` - Variable name to interpolate
ast_node!(StringInterpolationTarget, STRING_INTERPOLATION_TARGET);

/// Format specification for string interpolation
/// 
/// AST embedding syntax:
/// - `[:a-str-format "spec"]` - Format specification (e.g. ":.2f")
ast_node!(StringFormatSpec, STRING_FORMAT_SPEC);

/// Conversion specification for string interpolation
/// 
/// AST embedding syntax:
/// - `[:a-str-conversion "spec"]` - Conversion specification (e.g. "!r", "!s")
ast_node!(StringConversionSpec, STRING_CONVERSION_SPEC);

/// Dotted reference in string interpolation
/// 
/// AST embedding syntax:
/// - `[:a-str-dotted "path"]` - Dotted path reference (e.g. "obj.field.subfield")
ast_node!(StringDottedReference, STRING_DOTTED_REFERENCE);

/// Escaped opening brace in string pattern
/// 
/// AST embedding syntax:
/// - `[:a-str-esc-open]` - Escaped `{{` becomes literal `{`
ast_node!(StringEscapedOpen, STRING_ESCAPED_OPEN);

/// Escaped closing brace in string pattern
/// 
/// AST embedding syntax:
/// - `[:a-str-esc-close]` - Escaped `}}` becomes literal `}`
ast_node!(StringEscapedClose, STRING_ESCAPED_CLOSE);

impl StringPattern {
    pub fn chunks(&self) -> AstChildren<StringChunk> {
        // For now, return empty children since we're not parsing the string content during parse phase
        // This will be implemented properly once we fix the parser to create the proper structure
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

/// String chunk enum for pattern contents
/// 
/// AST embedding syntax:
/// - `LiteralContent`: `[:a-str-literal "text"]` - Plain text content
/// - `Interpolation`: `[:a-str-interp target format conversion]` - Variable interpolation
/// - `EscapedOpen`: `[:a-str-esc-open]` - Escaped opening brace `{{`
/// - `EscapedClose`: `[:a-str-esc-close]` - Escaped closing brace `}}`
pub enum StringChunk {
    /// Plain text content - Embedding: `[:a-str-literal "text"]`
    LiteralContent(StringLiteralContent),
    /// Variable interpolation - Embedding: `[:a-str-interp target format conversion]`
    Interpolation(StringInterpolation),
    /// Escaped opening brace - Embedding: `[:a-str-esc-open]`
    EscapedOpen(StringEscapedOpen),
    /// Escaped closing brace - Embedding: `[:a-str-esc-close]`
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
/// 
/// AST embedding syntax:
/// - `Lit`: `[:a-lit literal]` - Literal value (number, string, symbol)
/// - `Block`: `[:a-block declarations... metadata]` - Block expression with declarations
/// - `List`: `[:a-list items...]` - List literal with comma-separated items
/// - `ParenExpr`: `[:a-paren-expr soup]` - Parenthesised expression
/// - `Name`: `[:a-name identifier]` - Identifier reference (normal or operator)
/// - `StringPattern`: `[:a-string-pattern chunks...]` - String with interpolation
/// - `ApplyTuple`: `[:a-apply-tuple args...]` - Function application arguments
pub enum Element {
    /// Literal value - Embedding: `[:a-lit literal]`
    Lit(Literal),
    /// Block expression - Embedding: `[:a-block declarations... metadata]`
    Block(Block),
    /// List literal - Embedding: `[:a-list items...]`
    List(List),
    /// Parenthesised expression - Embedding: `[:a-paren-expr soup]`
    ParenExpr(ParenExpr),
    /// Identifier reference - Embedding: `[:a-name identifier]`
    Name(Name),
    /// String with interpolation - Embedding: `[:a-string-pattern chunks...]`
    StringPattern(StringPattern),
    /// Function arguments - Embedding: `[:a-apply-tuple args...]`
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

/// A top-level unit (complete program or module)
/// 
/// AST embedding syntax:
/// - `[:a-unit declarations... metadata]` - Top-level unit containing declarations
ast_node!(Unit, UNIT);

impl Unit {
    pub fn meta(&self) -> Option<BlockMetadata> {
        support::child::<BlockMetadata>(&self.0)
    }

    pub fn declarations(&self) -> AstChildren<Declaration> {
        support::children::<Declaration>(&self.0)
    }
}
