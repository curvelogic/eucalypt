//! Desugarable implementations for Rowan AST elements
//!
//! This mirrors the logic in ast.rs but uses Rowan's method-based API
//! instead of direct pattern matching on data structures.

use super::{
    desugarable::Desugarable, desugarer::Desugarer,
    literal::desugar_literal, disembed::core_from_embedding,
};
use crate::core::metadata::{
    normalise_metadata, strip_desugar_phase_metadata,
    DesugarPhaseBlockMetadata, DesugarPhaseDeclarationMetadata,
    ReadMetadata,
};
use crate::{
    common::sourcemap::{HasSmid, Smid},
    core::{
        anaphora::{BLOCK_ANAPHORA, EXPR_ANAPHORA},
        error::CoreError,
        expr::*,
        transform::dynamise,
    },
    syntax::rowan::{
        ast::{self as rowan_ast, Element, HasSoup, AstToken},
        kind::SyntaxKind,
    },
};
use rowan::{ast::AstNode, TextRange};
use codespan::{ByteIndex, Span};
use moniker::{Binder, Embed, Scope, Rec};

/// Convert a TextRange to a Span
fn text_range_to_span(range: TextRange) -> Span {
    let start = ByteIndex(range.start().into());
    let end = ByteIndex(range.end().into());
    Span::new(start, end)
}

/// Convert Rowan literal value to legacy AST literal for reuse
fn rowan_literal_to_legacy(lit: &rowan_ast::Literal) -> Result<crate::syntax::ast::Literal, CoreError> {
    let span = text_range_to_span(lit.syntax().text_range());
    
    if let Some(value) = lit.value() {
        match value {
            rowan_ast::LiteralValue::Sym(sym) => {
                if let Some(s) = sym.value() {
                    Ok(crate::syntax::ast::Literal::Sym(span, s.to_string()))
                } else {
                    Err(CoreError::InvalidEmbedding("invalid symbol literal".to_string(), 
                        crate::common::sourcemap::Smid::from(span.start().0 as u32)))
                }
            }
            rowan_ast::LiteralValue::Str(s) => {
                if let Some(text) = s.value() {
                    Ok(crate::syntax::ast::Literal::Str(span, text.to_string()))
                } else {
                    Err(CoreError::InvalidEmbedding("invalid string literal".to_string(), 
                        crate::common::sourcemap::Smid::from(span.start().0 as u32)))
                }
            }
            rowan_ast::LiteralValue::Num(n) => {
                if let Some(num) = n.value() {
                    Ok(crate::syntax::ast::Literal::Num(span, num))
                } else {
                    Err(CoreError::InvalidEmbedding("invalid number literal".to_string(), 
                        crate::common::sourcemap::Smid::from(span.start().0 as u32)))
                }
            }
        }
    } else {
        Err(CoreError::InvalidEmbedding("malformed literal".to_string(), 
            crate::common::sourcemap::Smid::from(span.start().0 as u32)))
    }
}

/// Convert Rowan AST Element to legacy Expression for core embedding
fn rowan_element_to_legacy(element: &Element) -> Result<crate::syntax::ast::Expression, CoreError> {
    match element {
        Element::Lit(lit) => {
            let legacy_lit = rowan_literal_to_legacy(lit)?;
            Ok(crate::syntax::ast::Expression::Lit(legacy_lit))
        }
        Element::List(list) => {
            let span = text_range_to_span(list.syntax().text_range());
            let mut items = Vec::new();
            
            for soup in list.items() {
                let legacy_expr = rowan_soup_to_legacy(&soup)?;
                items.push(legacy_expr);
            }
            
            Ok(crate::syntax::ast::Expression::List(span, items))
        }
        Element::Block(block) => {
            let span = text_range_to_span(block.syntax().text_range());
            let mut declarations = Vec::new();
            
            for decl in block.declarations() {
                let name_span = text_range_to_span(decl.syntax().text_range());
                let name = if let Some(head) = decl.head() {
                    let kind = head.classify_declaration();
                    match kind {
                        rowan_ast::DeclarationKind::Property(prop) => {
                            crate::syntax::ast::Name::Normal(name_span, prop.text().to_string())
                        }
                        _ => {
                            return Err(CoreError::InvalidEmbedding(
                                "complex declaration not supported in core embedding".to_string(),
                                crate::common::sourcemap::Smid::from(span.start().0 as u32)
                            ));
                        }
                    }
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "malformed declaration in core embedding".to_string(),
                        crate::common::sourcemap::Smid::from(span.start().0 as u32)
                    ));
                };
                
                let definition = if let Some(body) = decl.body() {
                    if let Some(body_soup) = body.soup() {
                        rowan_soup_to_legacy(&body_soup)?
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "empty declaration body in core embedding".to_string(),
                            crate::common::sourcemap::Smid::from(span.start().0 as u32)
                        ));
                    }
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "missing declaration body in core embedding".to_string(),
                        crate::common::sourcemap::Smid::from(span.start().0 as u32)
                    ));
                };
                
                let decl_head = crate::syntax::ast::DeclarationHead::PropertyPattern(name);
                declarations.push(crate::syntax::ast::Declaration::new(decl_head, None, definition));
            }
            
            Ok(crate::syntax::ast::Expression::Block(Box::new(
                crate::syntax::ast::Block { span, metadata: None, declarations }
            )))
        }
        Element::Name(name) => {
            let span = text_range_to_span(name.syntax().text_range());
            if let Some(ident) = name.identifier() {
                match ident {
                    rowan_ast::Identifier::NormalIdentifier(normal) => {
                        Ok(crate::syntax::ast::Expression::Name(
                            crate::syntax::ast::Name::Normal(span, normal.text().to_string())
                        ))
                    }
                    rowan_ast::Identifier::OperatorIdentifier(op) => {
                        Ok(crate::syntax::ast::Expression::Name(
                            crate::syntax::ast::Name::Operator(span, op.text().to_string())
                        ))
                    }
                }
            } else {
                Err(CoreError::InvalidEmbedding(
                    "malformed name in core embedding".to_string(),
                    crate::common::sourcemap::Smid::from(span.start().0 as u32)
                ))
            }
        }
        Element::ParenExpr(paren) => {
            // For parenthesized expressions, just convert the inner soup
            if let Some(soup) = paren.soup() {
                rowan_soup_to_legacy(&soup)
            } else {
                let span = text_range_to_span(paren.syntax().text_range());
                Err(CoreError::InvalidEmbedding(
                    "empty parenthesized expression in core embedding".to_string(),
                    crate::common::sourcemap::Smid::from(span.start().0 as u32)
                ))
            }
        }
        Element::StringPattern(string_pattern) => {
            let span = text_range_to_span(string_pattern.syntax().text_range());
            Err(CoreError::InvalidEmbedding(
                "string patterns not supported in core embedding".to_string(),
                crate::common::sourcemap::Smid::from(span.start().0 as u32)
            ))
        }
        Element::ApplyTuple(apply_tuple) => {
            let span = text_range_to_span(apply_tuple.syntax().text_range());
            let mut items = Vec::new();
            
            for soup in apply_tuple.items() {
                let legacy_expr = rowan_soup_to_legacy(&soup)?;
                items.push(legacy_expr);
            }
            
            Ok(crate::syntax::ast::Expression::ApplyTuple(span, items))
        }
    }
}

/// Convert Rowan AST Soup to legacy Expression for core embedding
pub fn rowan_soup_to_legacy(soup: &rowan_ast::Soup) -> Result<crate::syntax::ast::Expression, CoreError> {
    let elements: Vec<_> = soup.elements().collect();
    
    if elements.len() == 1 {
        // Single element soup - convert the element directly
        rowan_element_to_legacy(&elements[0])
    } else {
        // Multi-element soup - create an OpSoup expression
        let span = text_range_to_span(soup.syntax().text_range());
        let mut legacy_elements = Vec::new();
        
        for element in elements {
            let legacy_expr = rowan_element_to_legacy(&element)?;
            legacy_elements.push(legacy_expr);
        }
        
        Ok(crate::syntax::ast::Expression::OpSoup(span, legacy_elements))
    }
}

/// Literals desugar into core Primitives
impl Desugarable for rowan_ast::Literal {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        // Convert to legacy literal and reuse existing desugar_literal function
        let legacy_lit = rowan_literal_to_legacy(self)?;
        Ok(desugar_literal(desugarer, &legacy_lit))
    }
}

/// Element (equivalent to Expression) desugaring
impl Desugarable for Element {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        match self {
            Element::Lit(lit) => lit.desugar(desugarer),
            Element::Block(block) => block.desugar(desugarer),
            Element::List(list) => {
                let span = text_range_to_span(list.syntax().text_range());
                let items: Result<Vec<RcExpr>, CoreError> = list.items()
                    .map(|soup| {
                        // Each item is a Soup, get its singleton element if it exists
                        let expr = if let Some(elem) = soup.singleton() {
                            elem.desugar(desugarer)?
                        } else {
                            // Multiple elements in soup - desugar as soup
                            soup.desugar(desugarer)?
                        };
                        // Apply varify to convert Name expressions to Var expressions
                        Ok(desugarer.varify(expr))
                    })
                    .collect();
                
                Ok(RcExpr::from(Expr::List(
                    desugarer.new_smid(span),
                    items?
                )))
            }
            Element::ParenExpr(paren) => {
                // ParenExpr contains a soup
                if let Some(soup) = paren.soup() {
                    soup.desugar(desugarer)
                } else {
                    let span = text_range_to_span(paren.syntax().text_range());
                    Err(CoreError::InvalidEmbedding("empty parentheses".to_string(), desugarer.new_smid(span)))
                }
            }
            Element::Name(name) => {
                let span = text_range_to_span(name.syntax().text_range());
                if let Some(id) = name.identifier() {
                    desugar_rowan_name(span, &id, desugarer)
                } else {
                    Err(CoreError::InvalidEmbedding("invalid name".to_string(), desugarer.new_smid(span)))
                }
            }
            Element::StringPattern(pattern) => {
                let span = text_range_to_span(pattern.syntax().text_range());
                desugar_rowan_string_pattern(span, pattern, desugarer)
            }
            Element::ApplyTuple(tuple) => {
                let span = text_range_to_span(tuple.syntax().text_range());
                let args: Result<Vec<RcExpr>, CoreError> = tuple.items()
                    .map(|soup| {
                        let expr = if let Some(elem) = soup.singleton() {
                            elem.desugar(desugarer)?
                        } else {
                            soup.desugar(desugarer)?
                        };
                        // Apply varify to convert Name expressions to Var expressions
                        Ok(desugarer.varify(expr))
                    })
                    .collect();
                
                Ok(RcExpr::from(Expr::ArgTuple(
                    desugarer.new_smid(span),
                    args?
                )))
            }
        }
    }
}

/// Rowan equivalent of DeclarationComponents for extracting declaration information
struct RowanDeclarationComponents {
    pub span: Span,
    pub metadata: Option<RcExpr>,
    pub name: String,
    pub args: Vec<String>,
    pub body: RcExpr,
    pub arg_vars: Vec<moniker::FreeVar<String>>,
    pub is_operator: bool,
    pub fixity: Option<crate::core::expr::Fixity>,
}

/// Helper to desugar declaration body with arguments in scope
/// Returns the desugared body and the FreeVars for the arguments
fn desugar_declaration_body(
    decl: &rowan_ast::Declaration,
    desugarer: &mut Desugarer,
    args: &[String],
    span: Span,
) -> Result<(RcExpr, Vec<moniker::FreeVar<String>>), CoreError> {
    // Push args to environment if any and collect the FreeVars
    let arg_vars = if !args.is_empty() {
        desugarer.env_mut().push_keys(args.iter().cloned());
        // Get the FreeVars that were created
        args.iter()
            .map(|name| desugarer.env().get(name).unwrap().clone())
            .collect()
    } else {
        Vec::new()
    };
    
    // Desugar body
    let mut body = if let Some(body) = decl.body() {
        if let Some(body_soup) = body.soup() {
            body_soup.desugar(desugarer)?
        } else {
            return Err(CoreError::InvalidEmbedding(
                "empty declaration body".to_string(),
                desugarer.new_smid(span)
            ));
        }
    } else {
        return Err(CoreError::InvalidEmbedding(
            "missing declaration body".to_string(),
            desugarer.new_smid(span)
        ));
    };
    
    // Apply varify to convert Name expressions to Var expressions
    body = desugarer.varify(body);
    
    // Pop args from environment if any
    if !args.is_empty() {
        desugarer.env_mut().pop();
    }
    
    Ok((body, arg_vars))
}

/// Extract declaration components from a Rowan Declaration
fn extract_rowan_declaration_components(
    decl: &rowan_ast::Declaration, 
    desugarer: &mut Desugarer
) -> Result<RowanDeclarationComponents, CoreError> {
    let span = text_range_to_span(decl.syntax().text_range());
    
    // Extract metadata first
    let metadata = if let Some(meta) = decl.meta() {
        if let Some(meta_soup) = meta.soup() {
            Some(meta_soup.desugar(desugarer)?)
        } else {
            None
        }
    } else {
        None
    };
    
    // Extract head information (name, args, operator status) BEFORE body
    // so we can push args to environment for body desugaring
    if let Some(head) = decl.head() {
        let kind = head.classify_declaration();
        
        match kind {
            rowan_ast::DeclarationKind::Property(prop) => {
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &[], span)?;
                
                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: prop.text().to_string(),
                    args: vec![],
                    body,
                    arg_vars,
                    is_operator: false,
                    fixity: None,
                })
            }
            rowan_ast::DeclarationKind::Function(func, args_tuple) => {
                // Extract argument names from the apply tuple
                let arg_names: Vec<String> = args_tuple.items()
                    .filter_map(|soup| {
                        // Each argument should be a single name
                        if let Some(elem) = soup.singleton() {
                            if let rowan_ast::Element::Name(name) = elem {
                                if let Some(id) = name.identifier() {
                                    if let rowan_ast::Identifier::NormalIdentifier(normal) = id {
                                        Some(normal.text().to_string())
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .collect();
                
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &arg_names, span)?;
                
                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: func.text().to_string(),
                    args: arg_names,
                    body,
                    arg_vars,
                    is_operator: false,
                    fixity: None,
                })
            }
            rowan_ast::DeclarationKind::Prefix(_, op, arg) => {
                let args = vec![arg.text().to_string()];
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &args, span)?;
                
                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: op.text().to_string(),
                    args,
                    body,
                    arg_vars,
                    is_operator: true,
                    fixity: Some(crate::core::expr::Fixity::UnaryPrefix),
                })
            }
            rowan_ast::DeclarationKind::Postfix(_, arg, op) => {
                let args = vec![arg.text().to_string()];
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &args, span)?;
                
                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: op.text().to_string(),
                    args,
                    body,
                    arg_vars,
                    is_operator: true,
                    fixity: Some(crate::core::expr::Fixity::UnaryPostfix),
                })
            }
            rowan_ast::DeclarationKind::Binary(_, left, op, right) => {
                let args = vec![left.text().to_string(), right.text().to_string()];
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &args, span)?;
                
                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: op.text().to_string(),
                    args,
                    body,
                    arg_vars,
                    is_operator: true,
                    fixity: None,
                })
            }
            rowan_ast::DeclarationKind::Nullary(_, op) => {
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &[], span)?;
                
                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: op.text().to_string(),
                    args: vec![],
                    body,
                    arg_vars,
                    is_operator: true,
                    fixity: None,
                })
            }
            rowan_ast::DeclarationKind::MalformedHead(_) => {
                Err(CoreError::InvalidEmbedding(
                    "malformed declaration head".to_string(),
                    desugarer.new_smid(span)
                ))
            }
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "missing declaration head".to_string(),
            desugarer.new_smid(span)
        ))
    }
}


/// Translate special operator names (".") to operators but all other
/// names to Expr::Name for further analysis
fn desugar_rowan_name(span: Span, id: &rowan_ast::Identifier, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
    match id {
        rowan_ast::Identifier::OperatorIdentifier(op) => {
            let name = op.text();
            if name == "." {
                Ok(RcExpr::from(ops::dot()))
            } else {
                Ok(RcExpr::from(Expr::Name(desugarer.new_smid(span), name.to_string())))
            }
        }
        rowan_ast::Identifier::NormalIdentifier(normal) => {
            // Use the parent Identifier's name() method to properly extract name content (strips quotes)
            let full_id = rowan_ast::Identifier::NormalIdentifier(normal.clone());
            let name = full_id.name().unwrap_or("");
            if name.starts_with("__") && name.chars().nth(2).is_some_and(|c| c.is_uppercase()) {
                Ok(RcExpr::from(Expr::Intrinsic(desugarer.new_smid(span), name[2..].to_string())))
            } else if BLOCK_ANAPHORA.is_anaphor(name) {
                let smid = desugarer.new_smid(span);
                Ok(RcExpr::from(Expr::BlockAnaphor(
                    smid,
                    BLOCK_ANAPHORA.to_explicit_anaphor(smid, name),
                )))
            } else if EXPR_ANAPHORA.is_anaphor(name) {
                let smid = desugarer.new_smid(span);
                Ok(RcExpr::from(Expr::ExprAnaphor(
                    smid,
                    EXPR_ANAPHORA.to_explicit_anaphor(smid, name),
                )))
            } else {
                Ok(RcExpr::from(Expr::Name(desugarer.new_smid(span), name.to_string())))
            }
        }
    }
}

/// Desugar string pattern
fn desugar_rowan_string_pattern(
    span: Span,
    pattern: &rowan_ast::StringPattern,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    use crate::syntax::ast::{InterpolationTarget, InterpolationRequest, StringChunk};
    
    // Convert Rowan AST chunks to legacy AST chunks for reuse of existing logic
    let mut legacy_chunks = Vec::new();
    
    for chunk in pattern.chunks() {
        match chunk {
            rowan_ast::StringChunk::LiteralContent(content) => {
                if let Some(text) = content.value() {
                    let chunk_span = text_range_to_span(content.syntax().text_range());
                    legacy_chunks.push(StringChunk::LiteralContent(chunk_span, text));
                }
            }
            rowan_ast::StringChunk::Interpolation(interp) => {
                let chunk_span = text_range_to_span(interp.syntax().text_range());
                
                // Extract target - check for soup first (dotted lookups), then fallback to simple target
                let target = if let Some(soup) = interp.soup() {
                    // We have a soup (dotted lookup like data.foo.bar)
                    // Convert the soup elements to a list of Names for the Reference variant
                    let elements: Vec<_> = soup.elements().collect();
                    let mut names = Vec::new();
                    
                    for element in elements {
                        if let rowan_ast::Element::Name(name) = element {
                            // Only process STRING_INTERPOLATION_TARGET tokens, skip dot operators
                            // The dots will be handled automatically by InterpolationTarget::Reference desugaring
                            if let Some(token) = name.syntax().first_token() {
                                let token_text = token.text();
                                match token.kind() {
                                    SyntaxKind::STRING_INTERPOLATION_TARGET => {
                                        names.push(crate::syntax::ast::normal(token_text));
                                    }
                                    SyntaxKind::OPERATOR_IDENTIFIER => {
                                        // Skip dot operators - they will be added by the Reference desugaring
                                    }
                                    _ => {
                                        // Skip unknown tokens
                                    }
                                }
                            }
                        }
                    }
                    
                    if names.is_empty() {
                        // Fallback to anonymous if no names found
                        InterpolationTarget::StringAnaphor(chunk_span, None)
                    } else {
                        InterpolationTarget::Reference(chunk_span, names)
                    }
                } else if let Some(target_token) = interp.target() {
                    if let Some(target_text) = target_token.value() {
                        if target_text.is_empty() {
                            // Empty {} means anonymous positional parameter
                            InterpolationTarget::StringAnaphor(chunk_span, None)
                        } else if let Ok(num) = target_text.parse::<i32>() {
                            // Numeric index like {0}, {1}, {2}
                            InterpolationTarget::StringAnaphor(chunk_span, Some(num))
                        } else {
                            // Variable reference like {name}
                            InterpolationTarget::Reference(
                                chunk_span,
                                vec![crate::syntax::ast::normal(&target_text)]
                            )
                        }
                    } else {
                        // No value means anonymous positional parameter
                        InterpolationTarget::StringAnaphor(chunk_span, None)
                    }
                } else {
                    // No target means anonymous positional parameter
                    InterpolationTarget::StringAnaphor(chunk_span, None)
                };
                
                // Extract format spec
                let format = interp.format_spec()
                    .and_then(|spec| spec.value())
                    .filter(|s| !s.is_empty());
                
                // Extract conversion spec  
                let conversion = interp.conversion_spec()
                    .and_then(|spec| spec.value())
                    .filter(|s| !s.is_empty());
                
                let request = InterpolationRequest::new(chunk_span, target, format, conversion);
                legacy_chunks.push(StringChunk::Interpolation(chunk_span, request));
            }
            rowan_ast::StringChunk::EscapedOpen(_) => {
                // Escaped {{ becomes literal {
                let chunk_span = text_range_to_span(chunk.syntax().text_range());
                legacy_chunks.push(StringChunk::LiteralContent(chunk_span, "{".to_string()));
            }
            rowan_ast::StringChunk::EscapedClose(_) => {
                // Escaped }} becomes literal }
                let chunk_span = text_range_to_span(chunk.syntax().text_range());
                legacy_chunks.push(StringChunk::LiteralContent(chunk_span, "}".to_string()));
            }
        }
    }
    
    // Use the existing string pattern desugaring from ast.rs
    crate::core::desugar::ast::desugar_string_pattern(span, &legacy_chunks, desugarer)
}

/// Soup desugaring
impl Desugarable for rowan_ast::Soup {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        let span = text_range_to_span(self.syntax().text_range());
        let elements: Vec<Element> = self.elements().collect();
        
        // If there's only one element, desugar it and apply variable resolution
        if elements.len() == 1 {
            let expr = elements[0].desugar(desugarer)?;
            // Apply varify to ensure Name expressions become Var expressions
            Ok(desugarer.varify(expr))
        } else {
            // Multiple elements - desugar as operator soup
            desugar_rowan_soup(span, elements, desugarer)
        }
    }
}

/// Desugar operator soup - mirrors the logic from ast.rs
fn desugar_rowan_soup(
    span: Span,
    elements: Vec<Element>,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    use crate::core::desugar::ast::PendingLookup;
    
    let mut soup: Vec<RcExpr> = Vec::with_capacity(elements.len() * 2);
    let mut lookup = PendingLookup::None;

    for element in elements {
        let expr = element.desugar(desugarer)?;
        let top: Option<RcExpr> = soup.last().cloned();

        match &*expr.inner {
            // Insert explicit call ops
            Expr::ArgTuple(_, _) => {
                soup.push(RcExpr::from(ops::call()));
                soup.push(expr.clone());
            }
            // Initial name in lookup chains must become a var
            Expr::Name(s, ref n) => {
                if lookup == PendingLookup::Static {
                    // simple static lookup
                    soup.pop();
                    if let Some(dlet) = soup.pop() {
                        let rebodied = dlet.rebody(core::var(*s, desugarer.var(n)));
                        // Convert to OtherLet to prevent subsequent static lookups
                        let fixed_rebodied = match &*rebodied.inner {
                            Expr::Let(smid, scope, LetType::DefaultBlockLet) => {
                                RcExpr::from(Expr::Let(*smid, scope.clone(), LetType::OtherLet))
                            }
                            _ => rebodied
                        };
                        soup.push(fixed_rebodied);
                    } else {
                        panic!("Expected default let");
                    }
                    lookup = PendingLookup::None;
                } else if lookup == PendingLookup::Dynamic {
                    // simple dynamic lookup
                    soup.push(expr.clone());
                    lookup = PendingLookup::None;
                } else {
                    soup.push(core::var(*s, desugarer.var(n)))
                }
            }
            Expr::Operator(s, _, _, ref body) => {
                // If we're propagating a dot, we must classify it first
                if body.inner.is_pseudodot() {
                    if soup.is_empty() {
                        soup.push(RcExpr::from(Expr::ExprAnaphor(
                            *s,
                            Anaphor::Implicit(*s, ImplicitAnaphorSide::Left),
                        )));
                        lookup = PendingLookup::Dynamic;
                    } else if top.unwrap().inner.is_default_let() {
                        lookup = PendingLookup::Static;
                    } else {
                        lookup = PendingLookup::Dynamic;
                    }
                }
                soup.push(expr.clone());
            }
            _ => {
                if lookup == PendingLookup::Dynamic {
                    soup.push(dynamise::dynamise(&expr)?);
                    lookup = PendingLookup::None;
                } else if lookup == PendingLookup::Static {
                    soup.pop();
                    if let Some(dlet) = soup.pop() {
                        let rebodied = dlet.rebody(expr.clone());
                        // Convert to OtherLet to prevent subsequent static lookups
                        let fixed_rebodied = match &*rebodied.inner {
                            Expr::Let(smid, scope, LetType::DefaultBlockLet) => {
                                RcExpr::from(Expr::Let(*smid, scope.clone(), LetType::OtherLet))
                            }
                            _ => rebodied
                        };
                        soup.push(fixed_rebodied);
                    } else {
                        panic!("Expected default let");
                    }
                    lookup = PendingLookup::None;
                } else {
                    soup.push(expr.clone());
                }
            }
        }
    }

    // Optimise away the soup wrapping in cases where it is definitely
    // not needed but leave vars for cooking phase (in case of sections)
    if soup.len() == 1 {
        match &*soup.first().unwrap().inner {
            Expr::Var(_, _) => Ok(RcExpr::from(Expr::Soup(desugarer.new_smid(span), soup))),
            _ => Ok(soup.first().unwrap().clone()),
        }
    } else {
        Ok(RcExpr::from(Expr::Soup(desugarer.new_smid(span), soup)))
    }
}

/// Extract just the declaration name without desugaring body
fn extract_declaration_name(decl: &rowan_ast::Declaration) -> Result<String, CoreError> {
    if let Some(head) = decl.head() {
        let kind = head.classify_declaration();
        match kind {
            rowan_ast::DeclarationKind::Property(prop) => Ok(prop.text().to_string()),
            rowan_ast::DeclarationKind::Function(func, _) => Ok(func.text().to_string()),
            rowan_ast::DeclarationKind::Prefix(_, op, _) => Ok(op.text().to_string()),
            rowan_ast::DeclarationKind::Postfix(_, _, op) => Ok(op.text().to_string()),
            rowan_ast::DeclarationKind::Binary(_, _, op, _) => Ok(op.text().to_string()),
            rowan_ast::DeclarationKind::Nullary(_, op) => Ok(op.text().to_string()),
            rowan_ast::DeclarationKind::MalformedHead(_) => {
                Err(CoreError::InvalidEmbedding(
                    "malformed declaration head".to_string(),
                    Smid::default()
                ))
            }
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "missing declaration head".to_string(),
            Smid::default()
        ))
    }
}

/// Convert Rowan declaration to binding for let expression
fn rowan_declaration_to_binding(
    decl: &rowan_ast::Declaration,
    desugarer: &mut Desugarer,
) -> Result<(Binder<String>, Embed<RcExpr>), CoreError> {
    
    // Extract declaration name first and push to stack before body desugaring
    let name = extract_declaration_name(decl)?;
    desugarer.push(&name);
    
    let components = extract_rowan_declaration_components(decl, desugarer)?;
    
    // Process metadata for desugar-phase information
    let (core_meta, metadata) = match components.metadata {
        Some(m) => {
            let mut core_meta = normalise_metadata(&m);
            let metadata: DesugarPhaseDeclarationMetadata = core_meta.read_metadata().unwrap_or_default();
            (Some(core_meta), metadata)
        }
        None => {
            let metadata = DesugarPhaseDeclarationMetadata::default();
            (None, metadata)
        }
    };
    
    // Handle documentation if in base file
    if let Some(doc) = &metadata.doc {
        if desugarer.in_base_file() {
            // Convert to legacy DeclarationComponents for documentation
            let name_ref = crate::syntax::ast::normal(&components.name);
            let body_expr = crate::syntax::ast::Expression::Lit(crate::syntax::ast::Literal::Str(components.span, "placeholder".to_string()));
            let args_refs: Vec<_> = components.args.iter().map(|a| crate::syntax::ast::normal(a)).collect();
            let legacy_components = crate::syntax::ast::DeclarationComponents {
                span: components.span,
                metadata: None, // Already processed
                name: &name_ref,
                args: args_refs.iter().collect(),
                body: &body_expr,
            };
            desugarer.record_doc(doc.to_string(), &legacy_components);
        }
    }
    
    // Handle target metadata
    if let Some(target) = &metadata.target {
        desugarer.record_target(
            target.to_string(),
            metadata.doc.unwrap_or_default(),
            metadata.format,
            metadata.validations.unwrap_or_default(),
        );
    }
    
    // Handle imports
    let mut imports = Vec::new();
    if let Some(inputs) = metadata.imports {
        for input in inputs {
            let import_smid = desugarer.new_smid(components.span);
            imports.push(
                desugarer
                    .translate_import(import_smid, input)
                    .expect("failure translating import"),
            );
        }
    }
    
    // Note: argument names were already resolved during body desugaring
    // We need to get the original FreeVars that were used, not create new ones
    
    // Desugar the body expression
    let mut expr = if let Some(embedding) = &metadata.embedding {
        if embedding == "core" {
            // For core embedding, we need to access the raw Rowan body before desugaring
            // and convert it to legacy Expression format
            let raw_body = if let Some(body) = decl.body() {
                if let Some(body_soup) = body.soup() {
                    rowan_soup_to_legacy(&body_soup)?
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "empty declaration body in core embedding".to_string(),
                        desugarer.new_smid(components.span)
                    ));
                }
            } else {
                return Err(CoreError::InvalidEmbedding(
                    "missing declaration body in core embedding".to_string(),
                    desugarer.new_smid(components.span)
                ));
            };
            
            core_from_embedding(desugarer, &raw_body)?
        } else {
            return Err(CoreError::UnknownEmbedding(embedding.clone()));
        }
    } else {
        // Body is already desugared and variable resolution was done during soup processing
        components.body
    };
    
    // Wrap in lambda if there are arguments
    if !components.args.is_empty() {
        expr = core::lam(
            desugarer.new_annotated_smid(components.span, &components.name),
            components.arg_vars,
            expr.clone(),
        );
    }
    
    // Wrap in operator if this is an operator declaration
    if components.is_operator {
        expr = RcExpr::from(Expr::Operator(
            desugarer.new_smid(components.span),
            components.fixity
                .or(metadata.fixity)
                .unwrap_or(Fixity::InfixLeft),
            metadata.precedence.unwrap_or(50),
            expr.clone(),
        ));
    }
    
    // Attach metadata if present
    if let Some(m) = core_meta {
        let stripped_meta = strip_desugar_phase_metadata(&m);
        if !matches!(&*stripped_meta.inner, Expr::ErrEliminated) {
            expr = RcExpr::from(Expr::Meta(
                desugarer.new_smid(components.span),
                expr,
                stripped_meta,
            ));
        }
    }
    
    // Get the declared variable from environment
    let declared_var = desugarer
        .env()
        .get(&components.name)
        .expect("declaration var should have been prepared in block")
        .clone();
    
    // Embed in imports if required
    expr = imports.iter().rfold(expr, |acc, import| import.rebody(acc));
    
    let ret = (Binder(declared_var), Embed(expr));
    
    // Note: Don't pop the environment here - it's shared across all declarations
    // The caller (Unit or Block) will pop it after processing all declarations
    desugarer.pop();
    Ok(ret)
}

/// Block desugaring - proper implementation following legacy architecture
impl Desugarable for rowan_ast::Block {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        
        let span = text_range_to_span(self.syntax().text_range());
        
        // Transform metadata for attachment later
        let mut metadata = if let Some(meta) = self.meta() {
            if let Some(meta_soup) = meta.soup() {
                Some(normalise_metadata(&meta_soup.desugar(desugarer)?))
            } else {
                None
            }
        } else {
            None
        };
        
        // Extract block metadata for desugar-phase processing
        let block_meta: DesugarPhaseBlockMetadata = match metadata {
            Some(ref mut concrete_meta) => concrete_meta.read_metadata().unwrap_or_default(),
            None => DesugarPhaseBlockMetadata::default(),
        };
        
        // Handle imports
        let mut imports = Vec::new();
        if let Some(inputs) = block_meta.imports {
            let meta_smid = metadata.as_ref().unwrap().smid();
            for input in inputs {
                imports.push(
                    desugarer
                        .translate_import(meta_smid, input)
                        .expect("failure translating import"),
                );
            }
        }
        
        // Collect declaration names for environment
        let keys: Vec<_> = self.declarations()
            .filter_map(|decl| {
                if let Some(head) = decl.head() {
                    let kind = head.classify_declaration();
                    match kind {
                        rowan_ast::DeclarationKind::Property(prop) => Some(prop.text().to_string()),
                        rowan_ast::DeclarationKind::Function(func, _) => Some(func.text().to_string()),
                        rowan_ast::DeclarationKind::Prefix(_, op, _) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::Postfix(_, _, op) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::Binary(_, _, op, _) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::Nullary(_, op) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::MalformedHead(_) => None,
                    }
                } else {
                    None
                }
            })
            .collect();
        
        desugarer.env_mut().push_keys(keys);
        
        // Convert declarations to bindings
        let bindings = self.declarations()
            .map(|decl| rowan_declaration_to_binding(&decl, desugarer))
            .collect::<Result<Vec<_>, CoreError>>()?;
        
        // Create block body mapping names to variables
        let body_elements: BlockMap<RcExpr> = bindings
            .iter()
            .map(|(binder, embed)| {
                (
                    binder.0.clone().pretty_name.unwrap(),
                    core::var(embed.0.smid(), binder.0.clone()),
                )
            })
            .collect();
        
        // Create body - check for parse-embed override
        let body = if let Some(embed_key) = block_meta.parse_embed {
            let fv_opt = desugarer.env().get(&embed_key).cloned();
            match fv_opt {
                Some(fv) => core::var(desugarer.new_smid(span), fv),
                None => {
                    let meta_smid = metadata.as_ref().map(|m| m.smid()).unwrap_or_default();
                    RcExpr::from(Expr::ErrUnresolved(meta_smid, embed_key))
                }
            }
        } else {
            RcExpr::from(Expr::Block(desugarer.new_smid(span), body_elements))
        };
        
        // Create let expression with bindings
        let mut expr = RcExpr::from(Expr::Let(
            desugarer.new_smid(span),
            Scope::new(Rec::new(bindings), body),
            LetType::DefaultBlockLet,
        ));
        
        // Attach metadata if present
        if let Some(m) = metadata {
            let stripped_meta = strip_desugar_phase_metadata(&m);
            if !matches!(&*stripped_meta.inner, Expr::ErrEliminated) {
                expr = RcExpr::from(Expr::Meta(
                    desugarer.new_smid(span),
                    expr,
                    stripped_meta,
                ));
            }
        }
        
        // Embed in imports if required
        expr = imports.iter().rfold(expr, |acc, import| import.rebody(acc));
        
        desugarer.env_mut().pop();
        Ok(expr)
    }
}

/// Unit desugaring - proper implementation following legacy architecture
impl Desugarable for rowan_ast::Unit {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        
        let span = text_range_to_span(self.syntax().text_range());
        
        // Transform metadata for attachment later
        let mut metadata = if let Some(meta) = self.meta() {
            if let Some(meta_soup) = meta.soup() {
                Some(normalise_metadata(&meta_soup.desugar(desugarer)?))
            } else {
                None
            }
        } else {
            None
        };
        
        // Extract unit metadata for desugar-phase processing
        let unit_meta: DesugarPhaseBlockMetadata = match metadata {
            Some(ref mut concrete_meta) => concrete_meta.read_metadata().unwrap_or_default(),
            None => DesugarPhaseBlockMetadata::default(),
        };
        
        // Handle imports
        let mut imports = Vec::new();
        if let Some(inputs) = unit_meta.imports {
            let meta_smid = metadata.as_ref().unwrap().smid();
            for input in inputs {
                imports.push(
                    desugarer
                        .translate_import(meta_smid, input)
                        .expect("failure translating import"),
                );
            }
        }
        
        // Collect declaration names for environment
        let keys: Vec<_> = self.declarations()
            .filter_map(|decl| {
                if let Some(head) = decl.head() {
                    let kind = head.classify_declaration();
                    match kind {
                        rowan_ast::DeclarationKind::Property(prop) => Some(prop.text().to_string()),
                        rowan_ast::DeclarationKind::Function(func, _) => Some(func.text().to_string()),
                        rowan_ast::DeclarationKind::Prefix(_, op, _) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::Postfix(_, _, op) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::Binary(_, _, op, _) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::Nullary(_, op) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::MalformedHead(_) => None,
                    }
                } else {
                    None
                }
            })
            .collect();
        
        desugarer.env_mut().push_keys(keys);
        
        // Convert declarations to bindings
        let bindings = self.declarations()
            .map(|decl| rowan_declaration_to_binding(&decl, desugarer))
            .collect::<Result<Vec<_>, CoreError>>()?;
        
        // Create unit body mapping names to variables
        let body_elements: BlockMap<RcExpr> = bindings
            .iter()
            .map(|(binder, embed)| {
                (
                    binder.0.clone().pretty_name.unwrap(),
                    core::var(embed.0.smid(), binder.0.clone()),
                )
            })
            .collect();
        
        // Create body - check for parse-embed override
        let body = if let Some(embed_key) = unit_meta.parse_embed {
            let fv_opt = desugarer.env().get(&embed_key).cloned();
            match fv_opt {
                Some(fv) => core::var(desugarer.new_smid(span), fv),
                None => {
                    let meta_smid = metadata.as_ref().map(|m| m.smid()).unwrap_or_default();
                    RcExpr::from(Expr::ErrUnresolved(meta_smid, embed_key))
                }
            }
        } else {
            RcExpr::from(Expr::Block(desugarer.new_smid(span), body_elements))
        };
        
        // Create let expression with bindings
        let mut expr = RcExpr::from(Expr::Let(
            desugarer.new_smid(span),
            Scope::new(Rec::new(bindings), body),
            LetType::DefaultBlockLet,
        ));
        
        // Attach metadata if present
        if let Some(m) = metadata {
            let stripped_meta = strip_desugar_phase_metadata(&m);
            if !matches!(&*stripped_meta.inner, Expr::ErrEliminated) {
                expr = RcExpr::from(Expr::Meta(
                    desugarer.new_smid(span),
                    expr,
                    stripped_meta,
                ));
            }
        }
        
        // Embed in imports if required
        expr = imports.iter().rfold(expr, |acc, import| import.rebody(acc));
        
        desugarer.env_mut().pop();
        Ok(expr)
    }
}