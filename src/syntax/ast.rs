//! The concrete AST targeted by initial parse.
//!
//! Parsing of the block DSL proceeds in two phases: parsing into
//! `Protoblock` and then `Block` so as not to require too much
//! cleverness from the underlying parser (LALRPOP).
use crate::syntax::error::SyntaxError;
use crate::syntax::span::HasSpan;
use codespan::Span;
use itertools::Itertools;
use serde_json::Number;
use std::collections::VecDeque;

/// A literal value
#[derive(PartialEq, Debug, Clone, Eq)]
pub enum Literal {
    /// e.g. :foo
    Sym(Span, String),
    /// e.g. "foo"
    Str(Span, String),
    /// e.g. -1234.64364e99
    Num(Span, Number),
}

impl HasSpan for Literal {
    fn span(&self) -> Span {
        match self {
            Literal::Sym(s, _) | Literal::Str(s, _) | Literal::Num(s, _) => *s,
        }
    }
}

/// Construct a symbol literal at a source location
pub fn sym_at(span: Span, name: &str) -> Literal {
    Literal::Sym(span, name.to_string())
}

/// Construct a symbol literal
pub fn sym(name: &str) -> Literal {
    sym_at(Span::default(), name)
}

/// Construct a string literal at a source location
pub fn str_at<T>(span: Span, text: T) -> Literal
where
    T: AsRef<str>,
{
    Literal::Str(span, text.as_ref().to_string())
}

/// Construct a string literal
pub fn str<T>(text: T) -> Literal
where
    T: AsRef<str>,
{
    str_at(Span::default(), text)
}

/// Construct a number literal at a source location
pub fn num_at<N>(span: Span, n: N) -> Literal
where
    N: Into<Number>,
{
    Literal::Num(span, n.into())
}

/// Construct a number literal
pub fn num<N>(n: N) -> Literal
where
    N: Into<Number>,
{
    num_at(Span::default(), n)
}

/// A name (normal or operator)
#[derive(Clone, PartialEq, Debug, Eq)]
pub enum Name {
    /// normal (non-operator) name
    Normal(Span, String),
    /// operator name
    Operator(Span, String),
}

impl HasSpan for Name {
    fn span(&self) -> Span {
        match self {
            Name::Normal(s, _) | Name::Operator(s, _) => *s,
        }
    }
}

impl Name {
    /// Convert the name to a string
    pub fn name(&self) -> &str {
        match self {
            Name::Normal(_, n) => n,
            Name::Operator(_, n) => n,
        }
    }
}

/// Create a normal name
pub fn normal_at(span: Span, id: &str) -> Name {
    Name::Normal(span, id.into())
}

/// Create a normal name
pub fn normal(id: &str) -> Name {
    normal_at(Span::default(), id)
}

/// Create a operator name
pub fn operator_at(span: Span, id: &str) -> Name {
    Name::Operator(span, id.into())
}

/// Create a operator name
pub fn operator(id: &str) -> Name {
    operator_at(Span::default(), id)
}

/// Types of expression
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    /// e.g. 123, "blah", :symbol
    Lit(Literal),
    /// e.g. { x: y }
    Block(Box<Block>),
    /// e.g. [x, y, z]
    List(Span, Vec<Expression>),
    /// possibly parenthesised operator soup e.g. (x &&& y!)
    OpSoup(Span, Vec<Expression>),
    /// e.g. x, $$
    Name(Name),
    /// e.g. "{1} {2}!"
    StringPattern(Span, Vec<StringChunk>),
    /// e.g. ...(1, :foo, :bar)
    ApplyTuple(Span, Vec<Expression>),
}

impl HasSpan for Expression {
    fn span(&self) -> Span {
        match self {
            Expression::Lit(lit) => lit.span(),
            Expression::Block(b) => b.span(),
            Expression::List(s, _) => *s,
            Expression::OpSoup(s, _) => *s,
            Expression::Name(n) => n.span(),
            Expression::StringPattern(s, _) => *s,
            Expression::ApplyTuple(s, _) => *s,
        }
    }
}

/// Create a literal expression
pub fn lit(l: Literal) -> Expression {
    Expression::Lit(l)
}

/// Create a name expression
pub fn name(n: Name) -> Expression {
    Expression::Name(n)
}

/// Create operator soup for plural expressions, pass-through otherwise
pub fn maybe_soup_at(span: Span, mut exps: Vec<Expression>) -> Expression {
    if exps.len() == 1 {
        exps.pop().unwrap()
    } else {
        Expression::OpSoup(span, exps)
    }
}

/// Create operator soup expression
pub fn soup_at(span: Span, exps: Vec<Expression>) -> Expression {
    Expression::OpSoup(span, exps)
}

/// Create operator soup expression
pub fn soup(exps: Vec<Expression>) -> Expression {
    soup_at(Span::default(), exps)
}

/// Create a list expression
pub fn list_at(span: Span, exps: Vec<Expression>) -> Expression {
    Expression::List(span, exps)
}

/// Create a list expression
pub fn list(exps: Vec<Expression>) -> Expression {
    list_at(Span::default(), exps)
}

/// Create an application tuple expression
pub fn tuple_at(span: Span, exps: Vec<Expression>) -> Expression {
    Expression::ApplyTuple(span, exps)
}

/// Create an application tuple expression
pub fn tuple(exps: Vec<Expression>) -> Expression {
    tuple_at(Span::default(), exps)
}

impl Expression {
    /// Convert an expression into just a name if possible
    pub fn as_name(&self) -> Option<Name> {
        match self {
            Expression::Name(n) => Some(n.clone()),
            _ => None,
        }
    }
}

/// Prior to parsing into declarations a block is a sequence of proto
/// block elements. The LALRPOP parser generates this and it is
/// structured into a full block by
#[derive(PartialEq, Debug, Clone, Eq)]
pub enum ProtoblockElement {
    /// A backtick to preface metadata expressions
    DeclarationBacktick(Span),
    /// A colon to separate a name / pattern from declaration content
    DeclarationColon(Span),
    /// An (optional colon) terminating a declaration
    DeclarationComma(Span),
    /// A (delimited, single) expression element - several of these
    /// may form a soup expression later
    ExpressionElement(Expression),
}

pub fn elt(expr: Expression) -> ProtoblockElement {
    ProtoblockElement::from_expression(expr)
}

impl ProtoblockElement {
    pub fn from_expression(expr: Expression) -> Self {
        ProtoblockElement::ExpressionElement(expr)
    }

    /// Extract expression from the protoblock element if possible
    pub fn into_expression(self) -> Option<Expression> {
        match self {
            ProtoblockElement::ExpressionElement(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn is_backtick(&self) -> bool {
        matches!(self, ProtoblockElement::DeclarationBacktick(_))
    }

    pub fn is_colon(&self) -> bool {
        matches!(self, ProtoblockElement::DeclarationColon(_))
    }

    pub fn is_comma(&self) -> bool {
        matches!(self, ProtoblockElement::DeclarationComma(_))
    }
}

impl HasSpan for ProtoblockElement {
    fn span(&self) -> Span {
        match self {
            ProtoblockElement::DeclarationBacktick(s)
            | ProtoblockElement::DeclarationColon(s)
            | ProtoblockElement::DeclarationComma(s) => *s,
            ProtoblockElement::ExpressionElement(expr) => expr.span(),
        }
    }
}

/// A `Protoblock_` is a sequence of protoblock elements
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Protoblock(Span, Vec<ProtoblockElement>);

impl Protoblock {
    /// Construct Protoblock from elements
    pub fn new(span: Span, elements: Vec<ProtoblockElement>) -> Self {
        Protoblock(span, elements)
    }
}

impl HasSpan for Protoblock {
    fn span(&self) -> Span {
        match self {
            Protoblock(s, _) => *s,
        }
    }
}

/// A declaration argument tuple is a tuple of names (for now)
#[derive(PartialEq, Debug, Clone, Eq)]
pub struct ArgTuple(Span, Vec<Name>);

impl ArgTuple {
    /// Construct ArgTuple from elements
    pub fn new(span: Span, args: Vec<Name>) -> Self {
        ArgTuple(span, args)
    }

    /// Construct from and ApplyTuple expression
    pub fn from_apply_tuple(expr: Expression) -> Option<Self> {
        if let Expression::ApplyTuple(span, args) = expr {
            Some(ArgTuple(
                span,
                args.into_iter().flat_map(|e| e.as_name()).collect(),
            ))
        } else {
            None
        }
    }
}

impl ArgTuple {
    pub fn names(&self) -> &Vec<Name> {
        &self.1
    }
}

impl HasSpan for ArgTuple {
    fn span(&self) -> Span {
        match self {
            ArgTuple(s, _) => *s,
        }
    }
}

#[allow(clippy::from_over_into)]
impl Into<Vec<Name>> for ArgTuple {
    fn into(self) -> Vec<Name> {
        self.1
    }
}

pub fn args_at(span: Span, names: Vec<Name>) -> ArgTuple {
    ArgTuple(span, names)
}

pub fn args(names: Vec<Name>) -> ArgTuple {
    ArgTuple(Span::default(), names)
}

/// Types of declaration head pattern
#[derive(PartialEq, Debug, Clone, Eq)]
pub enum DeclarationHead {
    /// e.g. `k: ...`
    PropertyPattern(Name),
    /// e.g. f(x, y, z): ...
    FunctionPattern(Span, Name, ArgTuple),
    /// e.g. (x &&& y): ...
    InfixOperatorPattern(Span, Name, Name, Name),
    /// e.g. (!x): ...
    PrefixOperatorPattern(Span, Name, Name),
    /// e.g. (x!): ...
    PostfixOperatorPattern(Span, Name, Name),
}

impl HasSpan for DeclarationHead {
    fn span(&self) -> Span {
        match self {
            DeclarationHead::PropertyPattern(n) => n.span(),
            DeclarationHead::FunctionPattern(s, _, _)
            | DeclarationHead::InfixOperatorPattern(s, _, _, _)
            | DeclarationHead::PrefixOperatorPattern(s, _, _)
            | DeclarationHead::PostfixOperatorPattern(s, _, _) => *s,
        }
    }
}

/// Takes elements from the back of a vector of expressions to form a
/// declaration head.
pub fn take_declaration_head(
    file_id: usize,
    exprs: &mut Vec<ProtoblockElement>,
) -> Result<DeclarationHead, SyntaxError> {
    use self::DeclarationHead::*;
    use self::Expression::*;
    use self::Name::*;
    use self::ProtoblockElement::*;

    if let Some(ExpressionElement(expr)) = exprs.pop() {
        match expr {
            // property patterns are normal names: `x: y`
            Name(n @ Normal(_, _)) => Ok(PropertyPattern(n)),
            // operator patterns are parenthesised op soup
            OpSoup(s, xs) => match xs.as_slice() {
                [Name(l @ Normal(_, _)), Name(op @ Operator(_, _)), Name(r @ Normal(_, _))] => {
                    Ok(InfixOperatorPattern(s, l.clone(), op.clone(), r.clone()))
                }
                [Name(x @ Normal(_, _)), Name(op @ Operator(_, _))] => {
                    Ok(PostfixOperatorPattern(s, x.clone(), op.clone()))
                }
                [Name(op @ Operator(_, _)), Name(x @ Normal(_, _))] => {
                    Ok(PrefixOperatorPattern(s, op.clone(), x.clone()))
                }
                _ => Err(SyntaxError::InvalidDeclarationHead(file_id, s)),
            },
            // apply tuple requires us to take the preceding function
            // name too
            ApplyTuple(s, args) => {
                if let Some(ExpressionElement(Name(f @ Normal(_, _)))) = exprs.pop() {
                    let len = args.len();
                    let params: Vec<_> = args.into_iter().flat_map(|e| e.as_name()).collect();
                    if params.len() == len {
                        Ok(FunctionPattern(f.span().merge(s), f, ArgTuple(s, params)))
                    } else {
                        Err(SyntaxError::InvalidDeclarationHead(file_id, s))
                    }
                } else {
                    Err(SyntaxError::InvalidDeclarationHead(file_id, s))
                }
            }
            e => Err(SyntaxError::InvalidDeclarationHead(file_id, e.span())),
        }
    } else {
        Err(SyntaxError::MissingDeclarationHead(file_id, exprs.span()))
    }
}

/// Convert a vector of protoblock elements into an expression
pub fn elements_to_expression(
    file_id: usize,
    elements: &mut Vec<ProtoblockElement>,
) -> Result<Expression, SyntaxError> {
    let span = elements.span();
    let size = elements.len();

    let mut exprs: Vec<_> = elements
        .drain(..)
        .flat_map(ProtoblockElement::into_expression)
        .collect();

    if exprs.len() != size {
        Err(SyntaxError::InvalidExpression(file_id, span))
    } else if exprs.len() > 1 {
        Ok(Expression::OpSoup(exprs.span(), exprs))
    } else if exprs.len() == 1 {
        Ok(exprs.pop().unwrap())
    } else {
        Err(SyntaxError::EmptyExpression(file_id, span))
    }
}

/// Take metadata expression from the back of a vector of expressions
/// if available.
pub fn take_metadata_expression(
    file_id: usize,
    elements: &mut Vec<ProtoblockElement>,
) -> Result<Option<Expression>, SyntaxError> {
    if let Some(backtick) = elements.iter().position(ProtoblockElement::is_backtick) {
        let mut meta = elements.split_off(backtick + 1);
        chomp_backtick(elements);
        elements_to_expression(file_id, &mut meta).map(Option::Some)
    } else {
        Ok(None)
    }
}

/// Chomp any trailing comma from a vec of protoblock elements
pub fn chomp_comma(elements: &mut Vec<ProtoblockElement>) {
    if elements.last().filter(|e| e.is_comma()).is_some() {
        elements.pop();
    }
}

/// Chomp any trailing colon from a vec of protoblock elements
pub fn chomp_colon(elements: &mut Vec<ProtoblockElement>) {
    if elements.last().filter(|e| e.is_colon()).is_some() {
        elements.pop();
    }
}

/// Chomp any trailing backtick from a vec of protoblock elements
pub fn chomp_backtick(elements: &mut Vec<ProtoblockElement>) {
    if elements.last().filter(|e| e.is_backtick()).is_some() {
        elements.pop();
    }
}

/// Types of declaration
#[derive(PartialEq, Debug, Clone, Eq)]
pub enum Declaration {
    /// e.g. k: expr
    PropertyDeclaration(Span, Option<Expression>, Name, Expression),
    /// e.g. f(x, y, z): expr
    FunctionDeclaration(Span, Option<Expression>, Name, ArgTuple, Expression),
    /// e.g. (x &&& y): expr
    InfixOperatorDeclaration(Span, Option<Expression>, Name, Name, Name, Expression),
    /// e.g. (!x): expr
    PrefixOperatorDeclaration(Span, Option<Expression>, Name, Name, Expression),
    /// e.g. (x!): expr
    PostfixOperatorDeclaration(Span, Option<Expression>, Name, Name, Expression),
}

impl HasSpan for Declaration {
    fn span(&self) -> Span {
        use self::Declaration::*;
        match self {
            PropertyDeclaration(s, _, _, _) => *s,
            FunctionDeclaration(s, _, _, _, _) => *s,
            InfixOperatorDeclaration(s, _, _, _, _, _) => *s,
            PrefixOperatorDeclaration(s, _, _, _, _) => *s,
            PostfixOperatorDeclaration(s, _, _, _, _) => *s,
        }
    }
}

impl Declaration {
    /// Get the name declared by the declaration
    pub fn name(&self) -> &Name {
        use self::Declaration::*;
        match self {
            PropertyDeclaration(_, _, n, _) => n,
            FunctionDeclaration(_, _, f, _, _) => f,
            InfixOperatorDeclaration(_, _, _, op, _, _) => op,
            PrefixOperatorDeclaration(_, _, op, _, _) => op,
            PostfixOperatorDeclaration(_, _, _, op, _) => op,
        }
    }

    /// Get any declaration metadata
    pub fn metadata(&self) -> &Option<Expression> {
        use self::Declaration::*;
        match self {
            PropertyDeclaration(_, m, _, _)
            | FunctionDeclaration(_, m, _, _, _)
            | InfixOperatorDeclaration(_, m, _, _, _, _)
            | PrefixOperatorDeclaration(_, m, _, _, _)
            | PostfixOperatorDeclaration(_, m, _, _, _) => m,
        }
    }

    /// Get the defining expression
    pub fn definition(&self) -> &Expression {
        use self::Declaration::*;
        match self {
            PropertyDeclaration(_, _, _, x)
            | FunctionDeclaration(_, _, _, _, x)
            | InfixOperatorDeclaration(_, _, _, _, _, x)
            | PrefixOperatorDeclaration(_, _, _, _, x)
            | PostfixOperatorDeclaration(_, _, _, _, x) => x,
        }
    }

    /// True if and only if this declares an operator
    pub fn is_operator(&self) -> bool {
        use self::Declaration::*;
        matches!(
            self,
            InfixOperatorDeclaration(_, _, _, _, _, _)
                | PrefixOperatorDeclaration(_, _, _, _, _)
                | PostfixOperatorDeclaration(_, _, _, _, _)
        )
    }
}

/// Declarations can be analysed into a common decomposition of their components
pub struct DeclarationComponents<'decl> {
    pub span: Span,
    pub metadata: Option<&'decl Expression>,
    pub name: &'decl Name,
    pub args: Vec<&'decl Name>,
    pub body: &'decl Expression,
}

impl<'decl> DeclarationComponents<'decl> {
    pub fn new(
        span: Span,
        metadata: Option<&'decl Expression>,
        name: &'decl Name,
        args: Vec<&'decl Name>,
        body: &'decl Expression,
    ) -> Self {
        DeclarationComponents {
            span,
            metadata,
            name,
            args,
            body,
        }
    }
}

impl<'decl> From<&'decl Declaration> for DeclarationComponents<'decl> {
    fn from(decl: &'decl Declaration) -> Self {
        use self::Declaration::*;
        match decl {
            PropertyDeclaration(s, meta, name, expr) => {
                DeclarationComponents::new(*s, meta.as_ref(), name, vec![], expr)
            }
            FunctionDeclaration(s, meta, f, xs, body) => {
                DeclarationComponents::new(*s, meta.as_ref(), f, xs.names().iter().collect(), body)
            }
            InfixOperatorDeclaration(s, meta, l, op, r, body) => {
                DeclarationComponents::new(*s, meta.as_ref(), op, vec![l, r], body)
            }
            PrefixOperatorDeclaration(s, meta, op, r, body) => {
                DeclarationComponents::new(*s, meta.as_ref(), op, vec![r], body)
            }
            PostfixOperatorDeclaration(s, meta, l, op, body) => {
                DeclarationComponents::new(*s, meta.as_ref(), op, vec![l], body)
            }
        }
    }
}

/// Construct a property declaration at a specified source location
pub fn prop_at(span: Span, meta: Option<Expression>, name: Name, body: Expression) -> Declaration {
    Declaration::PropertyDeclaration(span, meta, name, body)
}

/// Construct a property declaration at a specified source location
pub fn prop(meta: Option<Expression>, name: Name, body: Expression) -> Declaration {
    Declaration::PropertyDeclaration(Span::default(), meta, name, body)
}

/// Construct a property declaration at a specified source location
pub fn fn_at(
    span: Span,
    meta: Option<Expression>,
    name: Name,
    args: ArgTuple,
    body: Expression,
) -> Declaration {
    Declaration::FunctionDeclaration(span, meta, name, args, body)
}

/// Construct a function declaration at an unspecified location
pub fn fn_(meta: Option<Expression>, name: Name, args: ArgTuple, body: Expression) -> Declaration {
    Declaration::FunctionDeclaration(Span::default(), meta, name, args, body)
}

/// Construct an infix operator declaration at a specified source location
pub fn infix_at(
    span: Span,
    meta: Option<Expression>,
    l: Name,
    op: Name,
    r: Name,
    body: Expression,
) -> Declaration {
    Declaration::InfixOperatorDeclaration(span, meta, l, op, r, body)
}

/// Construct an prefix operator declaration at a specified source location
pub fn prefix_at(
    span: Span,
    meta: Option<Expression>,
    op: Name,
    r: Name,
    body: Expression,
) -> Declaration {
    Declaration::PrefixOperatorDeclaration(span, meta, op, r, body)
}

/// Construct an postfix operator declaration at a specified source location
pub fn postfix_at(
    span: Span,
    meta: Option<Expression>,
    op: Name,
    r: Name,
    body: Expression,
) -> Declaration {
    Declaration::PostfixOperatorDeclaration(span, meta, op, r, body)
}

impl Declaration {
    /// Construct a new declaration
    pub fn new(head: DeclarationHead, meta: Option<Expression>, expr: Expression) -> Declaration {
        use self::Declaration::*;
        use self::DeclarationHead::*;
        match head {
            PropertyPattern(n) => PropertyDeclaration(n.span().merge(expr.span()), meta, n, expr),
            FunctionPattern(s, f, xs) => {
                FunctionDeclaration(s.merge(expr.span()), meta, f, xs, expr)
            }
            InfixOperatorPattern(s, l, op, r) => {
                InfixOperatorDeclaration(s.merge(expr.span()), meta, l, op, r, expr)
            }
            PrefixOperatorPattern(s, op, x) => {
                PrefixOperatorDeclaration(s.merge(expr.span()), meta, op, x, expr)
            }
            PostfixOperatorPattern(s, x, op) => {
                PostfixOperatorDeclaration(s.merge(expr.span()), meta, x, op, expr)
            }
        }
    }
}

/// A block is a sequence of declarations with optional block metadata
/// expression
#[derive(PartialEq, Debug, Clone, Eq)]
pub struct Block {
    pub span: Span,
    pub metadata: Option<Expression>,
    pub declarations: Vec<Declaration>,
}

impl HasSpan for Block {
    fn span(&self) -> Span {
        self.span
    }
}

impl Block {
    /// Construct from a vector of protoblock elements
    pub fn from_protoblock_elements(
        file_id: usize,
        span: Span,
        mut elements: Vec<ProtoblockElement>,
    ) -> Result<Self, SyntaxError> {
        // split on colons
        let colon_indices: Vec<usize> = elements
            .iter()
            .positions(ProtoblockElement::is_colon)
            .rev()
            .collect();

        // break into chunks around the colons
        let mut chunks: VecDeque<Vec<ProtoblockElement>> =
            VecDeque::with_capacity(elements.len() / 3);
        for index in colon_indices {
            chunks.push_back(elements.split_off(index + 1));
            chomp_colon(&mut elements);
        }
        if !elements.is_empty() {
            chunks.push_back(elements);
        }

        // process into declarations
        let mut decls: Vec<Declaration> = Vec::with_capacity(chunks.len());

        if let Some(mut last_chunk) = chunks.pop_front() {
            chomp_comma(&mut last_chunk);
            let mut remainder = last_chunk;

            while let Some(mut chunk) = chunks.pop_front() {
                let head = take_declaration_head(file_id, &mut chunk)?;
                let meta = take_metadata_expression(file_id, &mut chunk)?;
                let expr = elements_to_expression(file_id, &mut remainder)?;
                decls.push(Declaration::new(head, meta, expr));

                chomp_comma(&mut chunk);
                remainder = chunk;
            }

            decls.reverse();

            let block_meta = if !remainder.is_empty() {
                Some(elements_to_expression(file_id, &mut remainder)?)
            } else {
                None
            };

            Ok(Block {
                span,
                metadata: block_meta,
                declarations: decls,
            })
        } else {
            Ok(Block {
                span,
                metadata: None,
                declarations: vec![],
            })
        }
    }
}

/// Construct a block at the specified source location
pub fn block_at(span: Span, meta: Option<Expression>, decls: Vec<Declaration>) -> Block {
    Block {
        span,
        metadata: meta,
        declarations: decls,
    }
}

/// Construct a block
pub fn block(meta: Option<Expression>, decls: Vec<Declaration>) -> Block {
    Block {
        span: Span::default(),
        metadata: meta,
        declarations: decls,
    }
}

/// A chunk of a string pattern, either literal or interpolation
#[derive(PartialEq, Debug, Clone, Eq)]
pub enum StringChunk {
    /// Literal content
    LiteralContent(Span, String),
    /// An interpolation reference "{x}", "{}", "{1}"
    Interpolation(Span, InterpolationRequest),
}

impl StringChunk {
    pub fn is_literal(&self) -> bool {
        matches!(*self, StringChunk::LiteralContent(_, _))
    }

    pub fn literal_content(&self) -> &str {
        if let StringChunk::LiteralContent(_, c) = self {
            c
        } else {
            panic!("taking literal content of interpolation")
        }
    }
}

impl HasSpan for StringChunk {
    fn span(&self) -> Span {
        match self {
            StringChunk::LiteralContent(s, _) => *s,
            StringChunk::Interpolation(s, _) => *s,
        }
    }
}

/// A request for interpolation
#[derive(PartialEq, Debug, Clone, Eq)]
pub struct InterpolationRequest {
    /// Span of source code
    pub span: Span,
    /// The interpolation target (name in scope or anaphor)
    pub target: InterpolationTarget,
    /// Optional format / parse specification
    pub format: Option<String>,
    /// Optional conversion function
    pub conversion: Option<String>,
}

impl InterpolationRequest {
    pub fn new(
        span: Span,
        target: InterpolationTarget,
        format: Option<String>,
        conversion: Option<String>,
    ) -> Self {
        InterpolationRequest {
            span,
            target,
            format,
            conversion,
        }
    }
}

/// Create an interpolation string chunk
pub fn interpolation_at<S, T>(
    span: Span,
    target: InterpolationTarget,
    format: Option<S>,
    conversion: Option<T>,
) -> StringChunk
where
    S: AsRef<str>,
    T: AsRef<str>,
{
    StringChunk::Interpolation(
        span,
        InterpolationRequest {
            span,
            target,
            format: format.map(|s| s.as_ref().to_string()),
            conversion: conversion.map(|s| s.as_ref().to_string()),
        },
    )
}

pub fn interpolation<S, T>(
    target: InterpolationTarget,
    format: Option<S>,
    conversion: Option<T>,
) -> StringChunk
where
    S: AsRef<str>,
    T: AsRef<str>,
{
    interpolation_at(Span::default(), target, format, conversion)
}

pub fn simple_interpolation_at(span: Span, target: InterpolationTarget) -> StringChunk {
    StringChunk::Interpolation(
        span,
        InterpolationRequest {
            span,
            target,
            format: None,
            conversion: None,
        },
    )
}

pub fn simple_interpolation(target: InterpolationTarget) -> StringChunk {
    simple_interpolation_at(Span::default(), target)
}

/// Construct a literal string chunk at a source location
pub fn lit_content_at<T>(span: Span, text: T) -> StringChunk
where
    T: AsRef<str>,
{
    StringChunk::LiteralContent(span, text.as_ref().to_string())
}

/// Construct a literal string chunk at a source location
pub fn lit_content<T>(text: T) -> StringChunk
where
    T: AsRef<str>,
{
    lit_content_at(Span::default(), text)
}

/// An interpolation target
#[derive(PartialEq, Debug, Clone, Eq)]
pub enum InterpolationTarget {
    /// An anaphor (turning the string pattern into a lambda)
    StringAnaphor(Span, Option<i32>),
    /// Reference to a dotted name to be bound / looked up in scope
    Reference(Span, Vec<Name>),
}

pub fn str_anaphor_at(span: Span, index: Option<i32>) -> InterpolationTarget {
    InterpolationTarget::StringAnaphor(span, index)
}

pub fn str_anaphor(index: Option<i32>) -> InterpolationTarget {
    str_anaphor_at(Span::default(), index)
}

/// Create a string pattern from span and chunks
pub fn pattern_at(span: Span, chunks: Vec<StringChunk>) -> Expression {
    Expression::StringPattern(span, chunks)
}

/// Create a string pattern from span and chunks
pub fn pattern(chunks: Vec<StringChunk>) -> Expression {
    pattern_at(Span::default(), chunks)
}

#[cfg(test)]
mod tests {
    use super::*;
    use codespan_reporting::files::SimpleFiles;

    #[test]
    pub fn test_literals() {
        assert_eq!(sym("x"), sym("x"));
        assert_eq!(num(3i32), num(3u64));
    }

    #[test]
    pub fn test_eq() {
        assert!(sym("foo") != str("foo"));
        assert!(sym("foo") == sym("foo"));
    }

    pub fn fake_file_id() -> usize {
        SimpleFiles::new().add("", "")
    }

    #[test]
    pub fn test_take_declaration_head() {
        let mut prop_head = vec![elt(name(normal("x")))];
        assert_eq!(
            take_declaration_head(fake_file_id(), &mut prop_head),
            Ok(DeclarationHead::PropertyPattern(normal("x")))
        );
        assert!(prop_head.is_empty());

        let mut fn_head = vec![
            elt(name(normal("f"))),
            elt(tuple(vec![name(normal("x")), name(normal("y"))])),
        ];
        assert_eq!(
            take_declaration_head(fake_file_id(), &mut fn_head),
            Ok(DeclarationHead::FunctionPattern(
                Span::default(),
                normal("f"),
                ArgTuple(Span::default(), vec![normal("x"), normal("y")])
            ))
        );
        assert!(fn_head.is_empty());
    }

    #[test]
    pub fn test_take_metadata_expression() {
        let mut remainder = vec![
            ProtoblockElement::DeclarationBacktick(Span::default()),
            elt(name(normal("m"))),
        ];
        assert_eq!(
            take_metadata_expression(fake_file_id(), &mut remainder),
            Ok(Some(name(normal("m"))))
        );
        assert!(remainder.is_empty());
    }
}
