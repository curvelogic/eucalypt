//! Desugarable implementations for Rowan AST elements
//!
//! This mirrors the logic in ast.rs but uses Rowan's method-based API
//! instead of direct pattern matching on data structures.

use super::{
    desugarable::Desugarable, desugarer::Desugarer, rowan_disembed::core_from_rowan_embedding,
};
use crate::core::metadata::{
    normalise_metadata, strip_desugar_phase_metadata, DesugarPhaseBlockMetadata,
    DesugarPhaseDeclarationMetadata, ReadMetadata,
};
use crate::{
    common::sourcemap::{HasSmid, Smid},
    core::{
        anaphora::{BLOCK_ANAPHORA, EXPR_ANAPHORA},
        error::CoreError,
        expr::*,
        rt,
        transform::dynamise,
    },
    syntax::rowan::{
        ast::{self as rowan_ast, AstToken, Element, HasSoup},
        lex,
    },
};
use codespan::{ByteIndex, Span};
use moniker::{Binder, Embed, Rec, Scope};
use rowan::{ast::AstNode, TextRange};

/// Convert a TextRange to a Span
fn text_range_to_span(range: TextRange) -> Span {
    let start = ByteIndex(range.start().into());
    let end = ByteIndex(range.end().into());
    Span::new(start, end)
}

/// Return type of `desugar_declaration_body_with_patterns`:
/// `(body_expr, lambda_param_names, lambda_param_vars)`
type PatternBodyResult = (RcExpr, Vec<String>, Vec<moniker::FreeVar<String>>);

/// Entry for a single field binding in a destructuring let.
struct FieldBinding {
    /// Name of the field in the source block (for Lookup)
    field_name: String,
    /// FreeVar for the binding name in the body
    binding_var: moniker::FreeVar<String>,
}

/// Entry for a destructuring block let: synthetic param + field bindings.
struct DestructureEntry {
    /// Synthetic parameter name (e.g. `__p0`)
    synthetic_name: String,
    /// Field bindings to generate
    fields: Vec<FieldBinding>,
}

/// Entry for a single list element binding.
struct ListElementBinding {
    /// Zero-based index of the element
    index: usize,
    /// FreeVar for the binding name in the body
    binding_var: moniker::FreeVar<String>,
}

/// Entry for a destructuring list let: synthetic param + element bindings.
struct ListDestructureEntry {
    /// Synthetic parameter name (e.g. `__p0`)
    synthetic_name: String,
    /// Element bindings at each position
    elements: Vec<ListElementBinding>,
}

/// Entry for a head/tail cons destructuring let.
struct ConsDestructureEntry {
    /// Synthetic parameter name (e.g. `__p0`)
    synthetic_name: String,
    /// FreeVar for the head binding (`HEAD(__p0)`)
    head_var: moniker::FreeVar<String>,
    /// FreeVar for the tail binding (`TAIL(__p0)`)
    tail_var: moniker::FreeVar<String>,
}

/// A parsed parameter pattern in a function declaration
enum ParamPattern {
    /// Simple identifier: `x`
    Simple(String),
    /// Block destructuring: `{x y}` or `{x: a  y: b}`.
    ///
    /// Each entry is `(field_name, binding_name)`. For shorthand `{x}`,
    /// field and binding are both `"x"`. For rename `{x: a}`,
    /// field is `"x"` and binding is `"a"`.
    Block(Vec<(String, String)>),
    /// Fixed-length list destructuring: `[a, b, c]`.
    ///
    /// Contains the binding names for each positional element.
    List(Vec<String>),
    /// Head/tail cons destructuring: `[h : t]`.
    ///
    /// `head_name` binds to `HEAD(param)` and `tail_name` binds to `TAIL(param)`.
    Cons {
        /// Name to bind to the head element.
        head_name: String,
        /// Name to bind to the tail list.
        tail_name: String,
    },
}

/// Parse a block parameter pattern `{x y}` or `{x: a  y: b}` from a
/// Rowan `Block` AST node in a function parameter position.
///
/// Block patterns may contain:
/// - Shorthand names in block metadata: `{x y}` → `[(x, x), (y, y)]`
/// - Rename declarations: `{x: a  y: b}` → `[(x, a), (y, b)]`
/// - Mixed: `{x  y: b}` → `[(x, x), (y, b)]`
fn parse_block_pattern(block: &rowan_ast::Block) -> Result<Vec<(String, String)>, CoreError> {
    let mut fields: Vec<(String, String)> = Vec::new();

    // Extract shorthand names from block metadata soup (e.g. `x y` in `{x y}`)
    if let Some(meta) = block.meta() {
        if let Some(soup) = meta.soup() {
            for element in soup.elements() {
                if let rowan_ast::Element::Name(name) = element {
                    if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier()
                    {
                        let field_name = normal.text().to_string();
                        // Shorthand: field name = binding name
                        fields.push((field_name.clone(), field_name));
                    }
                }
            }
        }
    }

    // Extract rename fields from declarations (e.g. `x: a  y: b`)
    for decl in block.declarations() {
        if let Some(head) = decl.head() {
            let kind = head.classify_declaration();
            if let rowan_ast::DeclarationKind::Property(prop) = kind {
                let field_name = prop.text().to_string();
                // Check if there's a body (rename) or not
                let binding_name = if let Some(body) = decl.body() {
                    if let Some(body_soup) = body.soup() {
                        // Body should be a single normal identifier (the binding name)
                        if let Some(rowan_ast::Element::Name(name)) = body_soup.singleton() {
                            if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) =
                                name.identifier()
                            {
                                normal.text().to_string()
                            } else {
                                // Not a normal identifier — use field name as fallback
                                field_name.clone()
                            }
                        } else {
                            // Complex expression in body — use field name as fallback
                            field_name.clone()
                        }
                    } else {
                        field_name.clone()
                    }
                } else {
                    // No body in declaration — field name = binding name (shorthand)
                    field_name.clone()
                };
                fields.push((field_name, binding_name));
            }
        }
    }

    Ok(fields)
}

/// Parse a fixed-length list parameter pattern `[a, b, c]` from a
/// Rowan `List` AST node in a function parameter position.
///
/// Each item in the list should be a single normal identifier.
/// Returns a list of binding names in positional order.
fn parse_list_pattern(list: &rowan_ast::List) -> Option<Vec<String>> {
    let mut elements: Vec<String> = Vec::new();
    for item in list.items() {
        if let Some(rowan_ast::Element::Name(name)) = item.singleton() {
            if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier() {
                elements.push(normal.text().to_string());
            } else {
                return None; // Not a normal identifier — invalid pattern
            }
        } else {
            return None; // Not a single name — invalid pattern
        }
    }
    if elements.is_empty() {
        None // Empty list pattern not valid
    } else {
        Some(elements)
    }
}

/// Parse a head/tail cons parameter pattern `[h : t]` from a Rowan `List`
/// AST node that was identified as a cons pattern (has a COLON child token).
///
/// Both the head soup and the tail soup must be single normal identifiers.
/// Returns `(head_name, tail_name)` or `None` if the pattern is malformed.
fn parse_cons_pattern(list: &rowan_ast::List) -> Option<(String, String)> {
    let (head_soup, tail_soup) = list.cons_parts()?;

    let head_name = if let Some(rowan_ast::Element::Name(name)) = head_soup.singleton() {
        if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier() {
            normal.text().to_string()
        } else {
            return None; // Head is not a normal identifier
        }
    } else {
        return None; // Head is not a single name
    };

    let tail_name = if let Some(rowan_ast::Element::Name(name)) = tail_soup.singleton() {
        if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier() {
            normal.text().to_string()
        } else {
            return None; // Tail is not a normal identifier
        }
    } else {
        return None; // Tail is not a single name
    };

    Some((head_name, tail_name))
}

/// Parse a function parameter soup into a `ParamPattern`.
///
/// Returns `None` if the soup is not a valid single-element parameter.
fn parse_param_pattern(soup: &rowan_ast::Soup) -> Option<ParamPattern> {
    match soup.singleton() {
        Some(rowan_ast::Element::Name(name)) => {
            if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier() {
                Some(ParamPattern::Simple(normal.text().to_string()))
            } else {
                None
            }
        }
        Some(rowan_ast::Element::Block(block)) => {
            parse_block_pattern(&block).ok().map(ParamPattern::Block)
        }
        Some(rowan_ast::Element::List(list)) => {
            if list.is_cons_pattern() {
                parse_cons_pattern(&list).map(|(head_name, tail_name)| ParamPattern::Cons {
                    head_name,
                    tail_name,
                })
            } else {
                parse_list_pattern(&list).map(ParamPattern::List)
            }
        }
        _ => None,
    }
}

/// Ordered record of a destructuring let to emit after the body is desugared.
enum DestructureLet {
    /// Block destructuring: lookup each field by name.
    Block(DestructureEntry),
    /// List destructuring: index each element with LIST.NTH.
    List(ListDestructureEntry),
    /// Cons destructuring: bind head and tail via HEAD/TAIL.
    Cons(ConsDestructureEntry),
}

impl DestructureLet {
    fn synthetic_name(&self) -> &str {
        match self {
            DestructureLet::Block(e) => &e.synthetic_name,
            DestructureLet::List(e) => &e.synthetic_name,
            DestructureLet::Cons(e) => &e.synthetic_name,
        }
    }
}

/// Desugar a function body with support for destructuring parameter patterns.
///
/// For simple parameter patterns, this behaves like `desugar_declaration_body`.
/// For block patterns, a synthetic parameter (`__pN`) is introduced as the
/// lambda binder, and let bindings for each field are added around the body.
/// For list patterns, the same synthetic-parameter approach is used, with
/// `LIST.NTH` calls for each positional element.
///
/// Returns `(body_expr, lambda_param_names, lambda_param_vars)`.
fn desugar_declaration_body_with_patterns(
    decl: &rowan_ast::Declaration,
    desugarer: &mut Desugarer,
    patterns: &[ParamPattern],
    span: Span,
) -> Result<PatternBodyResult, CoreError> {
    // Build the complete list of names to push into the environment,
    // tracking which ones are lambda binders (synthetic params) and
    // which are only binding names for let bindings.
    let mut all_env_names: Vec<String> = Vec::new();
    let mut lambda_param_names: Vec<String> = Vec::new();
    // Raw data for block patterns: (synthetic_name, [(field_name, binding_name)])
    let mut block_raw: Vec<(String, Vec<(String, String)>)> = Vec::new();
    // Raw data for list patterns: (synthetic_name, [binding_name])
    let mut list_raw: Vec<(String, Vec<String>)> = Vec::new();
    // Raw data for cons patterns: (synthetic_name, head_name, tail_name)
    let mut cons_raw: Vec<(String, String, String)> = Vec::new();
    // Ordered sequence of lets to emit — preserves argument order for correct nesting.
    let mut let_order: Vec<DestructureLet> = Vec::new();
    let mut synthetic_counter = 0usize;

    for pattern in patterns {
        match pattern {
            ParamPattern::Simple(name) => {
                all_env_names.push(name.clone());
                lambda_param_names.push(name.clone());
            }
            ParamPattern::Block(fields) => {
                let synthetic_name = format!("__p{}", synthetic_counter);
                synthetic_counter += 1;
                all_env_names.push(synthetic_name.clone());
                lambda_param_names.push(synthetic_name.clone());
                for (_, binding_name) in fields {
                    all_env_names.push(binding_name.clone());
                }
                block_raw.push((synthetic_name.clone(), fields.clone()));
                // Placeholder — resolved after env push
                let_order.push(DestructureLet::Block(DestructureEntry {
                    synthetic_name,
                    fields: Vec::new(),
                }));
            }
            ParamPattern::List(element_names) => {
                let synthetic_name = format!("__p{}", synthetic_counter);
                synthetic_counter += 1;
                all_env_names.push(synthetic_name.clone());
                lambda_param_names.push(synthetic_name.clone());
                for binding_name in element_names {
                    all_env_names.push(binding_name.clone());
                }
                list_raw.push((synthetic_name.clone(), element_names.clone()));
                // Placeholder — resolved after env push
                let_order.push(DestructureLet::List(ListDestructureEntry {
                    synthetic_name,
                    elements: Vec::new(),
                }));
            }
            ParamPattern::Cons {
                head_name,
                tail_name,
            } => {
                let synthetic_name = format!("__p{}", synthetic_counter);
                synthetic_counter += 1;
                all_env_names.push(synthetic_name.clone());
                lambda_param_names.push(synthetic_name.clone());
                all_env_names.push(head_name.clone());
                all_env_names.push(tail_name.clone());
                cons_raw.push((synthetic_name.clone(), head_name.clone(), tail_name.clone()));
                // Placeholder — resolved after env push
                let_order.push(DestructureLet::Cons(ConsDestructureEntry {
                    synthetic_name,
                    head_var: moniker::FreeVar::fresh_named("__head_placeholder"),
                    tail_var: moniker::FreeVar::fresh_named("__tail_placeholder"),
                }));
            }
        }
    }

    // Push all names into the environment as a single frame
    if !all_env_names.is_empty() {
        desugarer.env_mut().push_keys(all_env_names.iter().cloned());
    }

    // Collect FreeVars for lambda binders (before desugaring body)
    let lambda_param_vars: Vec<moniker::FreeVar<String>> = lambda_param_names
        .iter()
        .map(|name| desugarer.env().get(name).unwrap().clone())
        .collect();

    // Resolve block destructure entries now that names are in the environment.
    let mut block_raw_iter = block_raw.into_iter();
    let mut list_raw_iter = list_raw.into_iter();
    let mut cons_raw_iter = cons_raw.into_iter();
    for slot in &mut let_order {
        match slot {
            DestructureLet::Block(entry) => {
                let (_, fields) = block_raw_iter.next().unwrap();
                entry.fields = fields
                    .iter()
                    .map(|(field_name, binding_name)| {
                        let binding_var = desugarer.env().get(binding_name).unwrap().clone();
                        FieldBinding {
                            field_name: field_name.clone(),
                            binding_var,
                        }
                    })
                    .collect();
            }
            DestructureLet::List(entry) => {
                let (_, element_names) = list_raw_iter.next().unwrap();
                entry.elements = element_names
                    .iter()
                    .enumerate()
                    .map(|(index, binding_name)| {
                        let binding_var = desugarer.env().get(binding_name).unwrap().clone();
                        ListElementBinding { index, binding_var }
                    })
                    .collect();
            }
            DestructureLet::Cons(entry) => {
                let (_, head_name, tail_name) = cons_raw_iter.next().unwrap();
                entry.head_var = desugarer.env().get(&head_name).unwrap().clone();
                entry.tail_var = desugarer.env().get(&tail_name).unwrap().clone();
            }
        }
    }

    // Desugar body with all names in scope
    let mut body = if let Some(body_node) = decl.body() {
        if let Some(body_soup) = body_node.soup() {
            body_soup.desugar(desugarer)?
        } else {
            return Err(CoreError::InvalidEmbedding(
                "empty declaration body".to_string(),
                desugarer.new_smid(span),
            ));
        }
    } else {
        return Err(CoreError::InvalidEmbedding(
            "missing declaration body".to_string(),
            desugarer.new_smid(span),
        ));
    };

    // Apply varify to convert Name expressions to Var expressions
    body = desugarer.varify(body);

    // Pop environment frame
    if !all_env_names.is_empty() {
        desugarer.env_mut().pop();
    }

    // Wrap body in destructuring lets for each pattern, innermost first
    // (last pattern wraps outermost), preserving argument order.
    for slot in let_order.iter().rev() {
        // Look up the synthetic param FreeVar from the lambda_param_vars
        // (the env frame has already been popped)
        let synthetic_fv = lambda_param_vars
            .iter()
            .zip(lambda_param_names.iter())
            .find(|(_, n)| **n == slot.synthetic_name())
            .map(|(fv, _)| fv.clone())
            .unwrap();

        let smid = desugarer.new_smid(span);
        let synthetic_var = RcExpr::from(Expr::Var(smid, moniker::Var::Free(synthetic_fv)));

        match slot {
            DestructureLet::Block(entry) => {
                // Each binding is: binding_var = Lookup(Var(synthetic), "field_name", None)
                let bindings: Vec<(Binder<String>, Embed<RcExpr>)> = entry
                    .fields
                    .iter()
                    .map(|fb| {
                        let lookup = RcExpr::from(Expr::Lookup(
                            smid,
                            synthetic_var.clone(),
                            fb.field_name.clone(),
                            None,
                        ));
                        (Binder(fb.binding_var.clone()), Embed(lookup))
                    })
                    .collect();

                body = RcExpr::from(Expr::Let(
                    smid,
                    Scope::new(Rec::new(bindings), body),
                    LetType::DestructureBlockLet,
                ));
            }
            DestructureLet::List(entry) => {
                // Each binding uses HEAD/TAIL chaining:
                //   a = HEAD(__p0)
                //   b = HEAD(TAIL(__p0))
                //   c = HEAD(TAIL(TAIL(__p0)))
                //
                // This correctly handles lists built by the STG compiler
                // where the nil tail is a global ref, not a local ref.
                let bindings: Vec<(Binder<String>, Embed<RcExpr>)> = entry
                    .elements
                    .iter()
                    .map(|eb| {
                        // Build TAIL applied `index` times to synthetic_var
                        let mut list_expr = synthetic_var.clone();
                        for _ in 0..eb.index {
                            list_expr = core::app(smid, core::bif(smid, "TAIL"), vec![list_expr]);
                        }
                        // Apply HEAD to get the element at this position
                        let head_call = core::app(smid, core::bif(smid, "HEAD"), vec![list_expr]);
                        (Binder(eb.binding_var.clone()), Embed(head_call))
                    })
                    .collect();

                body = RcExpr::from(Expr::Let(
                    smid,
                    Scope::new(Rec::new(bindings), body),
                    LetType::DestructureListLet,
                ));
            }
            DestructureLet::Cons(entry) => {
                // Cons pattern `[h : t]` desugars to:
                //   h = HEAD(__p0)
                //   t = TAIL(__p0)
                let head_call =
                    core::app(smid, core::bif(smid, "HEAD"), vec![synthetic_var.clone()]);
                let tail_call = core::app(smid, core::bif(smid, "TAIL"), vec![synthetic_var]);

                let bindings: Vec<(Binder<String>, Embed<RcExpr>)> = vec![
                    (Binder(entry.head_var.clone()), Embed(head_call)),
                    (Binder(entry.tail_var.clone()), Embed(tail_call)),
                ];

                body = RcExpr::from(Expr::Let(
                    smid,
                    Scope::new(Rec::new(bindings), body),
                    LetType::DestructureListLet,
                ));
            }
        }
    }

    Ok((body, lambda_param_names, lambda_param_vars))
}

/// Literals desugar into core Primitives
impl Desugarable for rowan_ast::Literal {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        let span = text_range_to_span(self.syntax().text_range());

        if let Some(value) = self.value() {
            let primitive = match value {
                rowan_ast::LiteralValue::Sym(sym) => {
                    if let Some(s) = sym.value() {
                        Primitive::Sym(s.to_string())
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid symbol literal".to_string(),
                            desugarer.new_smid(span),
                        ));
                    }
                }
                rowan_ast::LiteralValue::Str(s) => {
                    if let Some(text) = s.value() {
                        Primitive::Str(text.to_string())
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid string literal".to_string(),
                            desugarer.new_smid(span),
                        ));
                    }
                }
                rowan_ast::LiteralValue::CStr(s) => {
                    if let Some(raw_text) = s.raw_value() {
                        // Process escape sequences for c-strings
                        use super::escape;
                        match escape::process_escapes(raw_text) {
                            Ok(processed) => Primitive::Str(processed),
                            Err(e) => {
                                return Err(CoreError::InvalidEmbedding(
                                    format!("invalid c-string escape: {}", e),
                                    desugarer.new_smid(span),
                                ));
                            }
                        }
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid c-string literal".to_string(),
                            desugarer.new_smid(span),
                        ));
                    }
                }
                rowan_ast::LiteralValue::RawStr(s) => {
                    // Raw strings are literal - no escape processing
                    if let Some(text) = s.value() {
                        Primitive::Str(text.to_string())
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid raw string literal".to_string(),
                            desugarer.new_smid(span),
                        ));
                    }
                }
                rowan_ast::LiteralValue::TStr(s) => {
                    // ZDT literal desugars to ZDT.PARSE(normalized_content)
                    if let Some(content) = s.value() {
                        let smid = desugarer.new_smid(span);
                        let normalized = lex::normalize_zdt_for_parse(content);
                        return Ok(core::app(
                            smid,
                            core::bif(smid, "ZDT.PARSE"),
                            vec![core::str(smid, normalized)],
                        ));
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid ZDT literal".to_string(),
                            desugarer.new_smid(span),
                        ));
                    }
                }
                rowan_ast::LiteralValue::Num(n) => {
                    if let Some(num) = n.value() {
                        Primitive::Num(num)
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid number literal".to_string(),
                            desugarer.new_smid(span),
                        ));
                    }
                }
            };
            Ok(RcExpr::from(Expr::Literal(
                desugarer.new_smid(span),
                primitive,
            )))
        } else {
            Err(CoreError::InvalidEmbedding(
                "malformed literal".to_string(),
                desugarer.new_smid(span),
            ))
        }
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
                let items: Result<Vec<RcExpr>, CoreError> = list
                    .items()
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

                Ok(RcExpr::from(Expr::List(desugarer.new_smid(span), items?)))
            }
            Element::ParenExpr(paren) => {
                // ParenExpr contains a soup
                if let Some(soup) = paren.soup() {
                    soup.desugar(desugarer)
                } else {
                    let span = text_range_to_span(paren.syntax().text_range());
                    Err(CoreError::InvalidEmbedding(
                        "empty parentheses".to_string(),
                        desugarer.new_smid(span),
                    ))
                }
            }
            Element::Name(name) => {
                let span = text_range_to_span(name.syntax().text_range());
                if let Some(id) = name.identifier() {
                    desugar_rowan_name(span, &id, desugarer)
                } else {
                    Err(CoreError::InvalidEmbedding(
                        "invalid name".to_string(),
                        desugarer.new_smid(span),
                    ))
                }
            }
            Element::StringPattern(pattern) => {
                let span = text_range_to_span(pattern.syntax().text_range());
                desugar_rowan_string_pattern(
                    span,
                    pattern.chunks(),
                    StringPatternMode::Plain,
                    desugarer,
                )
            }
            Element::CStringPattern(pattern) => {
                let span = text_range_to_span(pattern.syntax().text_range());
                desugar_rowan_string_pattern(
                    span,
                    pattern.chunks(),
                    StringPatternMode::CString,
                    desugarer,
                )
            }
            Element::RawStringPattern(pattern) => {
                let span = text_range_to_span(pattern.syntax().text_range());
                desugar_rowan_string_pattern(
                    span,
                    pattern.chunks(),
                    StringPatternMode::Raw,
                    desugarer,
                )
            }
            Element::BracketExpr(bracket) => {
                // A bracket expression ⟦ x ⟧ desugars to applying the bracket pair function
                // (named e.g. "⟦⟧") to the inner soup expression.
                //
                // This is equivalent to: ⟦⟧(x)  where ⟦⟧ is looked up in scope.
                let span = text_range_to_span(bracket.syntax().text_range());
                let smid = desugarer.new_smid(span);

                let pair_name = bracket.bracket_pair_name().ok_or_else(|| {
                    CoreError::InvalidEmbedding(
                        "bracket expression missing bracket characters".to_string(),
                        smid,
                    )
                })?;

                // Desugar the inner soup
                let inner = if let Some(soup) = bracket.soup() {
                    soup.desugar(desugarer)?
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "empty bracket expression".to_string(),
                        smid,
                    ));
                };

                // Build: name(⟦⟧) applied to inner
                // Desugars the bracket expression ⟦ x ⟧ as a call to the
                // bracket pair function by name, with the inner expression as
                // the sole argument.  Both the function name and the argument
                // must be varified so that Name nodes are resolved to Var
                // references before the STG compiler sees them.
                let bracket_fn_name = RcExpr::from(Expr::Name(smid, pair_name));
                let bracket_fn = desugarer.varify(bracket_fn_name);
                let arg = desugarer.varify(inner);
                Ok(RcExpr::from(Expr::App(smid, bracket_fn, vec![arg])))
            }
            Element::ApplyTuple(tuple) => {
                let span = text_range_to_span(tuple.syntax().text_range());
                let args: Result<Vec<RcExpr>, CoreError> = tuple
                    .items()
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
                    args?,
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
                desugarer.new_smid(span),
            ));
        }
    } else {
        return Err(CoreError::InvalidEmbedding(
            "missing declaration body".to_string(),
            desugarer.new_smid(span),
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
    desugarer: &mut Desugarer,
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
                // Parse each argument into a ParamPattern (simple name or
                // destructuring pattern). Fall back to simple name extraction
                // for any arg that doesn't parse as a pattern (shouldn't
                // happen after Task 2 validation, but be defensive).
                let patterns: Vec<ParamPattern> = args_tuple
                    .items()
                    .filter_map(|soup| parse_param_pattern(&soup))
                    .collect();

                // If all patterns are Simple (no destructuring), use the
                // existing fast path so as not to disturb existing behaviour.
                let all_simple = patterns
                    .iter()
                    .all(|p| matches!(p, ParamPattern::Simple(_)));

                if all_simple {
                    let arg_names: Vec<String> = patterns
                        .into_iter()
                        .map(|p| match p {
                            ParamPattern::Simple(n) => n,
                            _ => unreachable!(),
                        })
                        .collect();
                    let (body, arg_vars) =
                        desugar_declaration_body(decl, desugarer, &arg_names, span)?;
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
                } else {
                    // At least one destructuring pattern — use pattern-aware
                    // desugaring which injects synthetic params and let bindings.
                    let (body, lambda_param_names, lambda_param_vars) =
                        desugar_declaration_body_with_patterns(decl, desugarer, &patterns, span)?;
                    Ok(RowanDeclarationComponents {
                        span,
                        metadata,
                        name: func.text().to_string(),
                        args: lambda_param_names,
                        body,
                        arg_vars: lambda_param_vars,
                        is_operator: false,
                        fixity: None,
                    })
                }
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
                    fixity: Some(crate::core::expr::Fixity::Nullary),
                })
            }
            rowan_ast::DeclarationKind::BracketPair(_, bracket_expr, param) => {
                let pair_name = bracket_expr.bracket_pair_name().ok_or_else(|| {
                    CoreError::InvalidEmbedding(
                        "bracket pair declaration has no bracket pair name".to_string(),
                        desugarer.new_smid(span),
                    )
                })?;
                let args = vec![param.text().to_string()];
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &args, span)?;

                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: pair_name,
                    args,
                    body,
                    arg_vars,
                    is_operator: false,
                    fixity: None,
                })
            }
            rowan_ast::DeclarationKind::MalformedHead(_) => Err(CoreError::InvalidEmbedding(
                "malformed declaration head".to_string(),
                desugarer.new_smid(span),
            )),
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "missing declaration head".to_string(),
            desugarer.new_smid(span),
        ))
    }
}

/// Translate special operator names (".") to operators but all other
/// names to Expr::Name for further analysis
fn desugar_rowan_name(
    span: Span,
    id: &rowan_ast::Identifier,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    match id {
        rowan_ast::Identifier::OperatorIdentifier(op) => {
            let name = op.text();
            if name == "." {
                Ok(RcExpr::from(ops::dot()))
            } else {
                Ok(RcExpr::from(Expr::Name(
                    desugarer.new_smid(span),
                    name.to_string(),
                )))
            }
        }
        rowan_ast::Identifier::NormalIdentifier(normal) => {
            // Use the parent Identifier's name() method to properly extract name content (strips quotes)
            let full_id = rowan_ast::Identifier::NormalIdentifier(normal.clone());
            let name = full_id.name().unwrap_or("");
            if name.starts_with("__") && name.chars().nth(2).is_some_and(|c| c.is_uppercase()) {
                Ok(RcExpr::from(Expr::Intrinsic(
                    desugarer.new_smid(span),
                    name[2..].to_string(),
                )))
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
                Ok(RcExpr::from(Expr::Name(
                    desugarer.new_smid(span),
                    name.to_string(),
                )))
            }
        }
    }
}

/// Mode for string pattern processing
#[derive(Clone, Copy, PartialEq, Eq)]
enum StringPatternMode {
    /// Plain string - no backslash escape processing
    Plain,
    /// C-string - process backslash escape sequences
    CString,
    /// Raw string - no backslash escape processing (same as plain)
    Raw,
}

/// Desugar string pattern directly from Rowan AST
fn desugar_rowan_string_pattern(
    span: Span,
    chunks: rowan_ast::AstChildren<rowan_ast::StringChunk>,
    mode: StringPatternMode,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    // Desugar chunks directly without converting to legacy AST
    let mut translated_chunks = Vec::new();

    for chunk in chunks {
        let chunk_expr = match chunk {
            rowan_ast::StringChunk::LiteralContent(content) => {
                if let Some(text) = content.value() {
                    let chunk_span = text_range_to_span(content.syntax().text_range());
                    // Process escapes for c-strings
                    let processed_text = if mode == StringPatternMode::CString {
                        use super::escape;
                        match escape::process_escapes(&text) {
                            Ok(processed) => processed,
                            Err(e) => {
                                return Err(CoreError::InvalidEmbedding(
                                    format!("invalid c-string escape: {}", e),
                                    desugarer.new_smid(chunk_span),
                                ));
                            }
                        }
                    } else {
                        text.to_string()
                    };
                    RcExpr::from(Expr::Literal(
                        desugarer.new_smid(chunk_span),
                        Primitive::Str(processed_text),
                    ))
                } else {
                    RcExpr::from(Expr::Literal(
                        desugarer.new_smid(span),
                        Primitive::Str("".to_string()),
                    ))
                }
            }
            rowan_ast::StringChunk::Interpolation(interp) => {
                let chunk_span = text_range_to_span(interp.syntax().text_range());

                // Extract target expression using the exact same approach as legacy AST
                let mut target_expr = if let Some(soup) = interp.soup() {
                    // Extract names from the soup (equivalent to Vec<Name> in legacy AST)
                    let elements: Vec<_> = soup.elements().collect();
                    let names: Vec<_> = elements
                        .iter()
                        .filter_map(|elem| match elem {
                            rowan_ast::Element::Name(name) => {
                                // Exclude dots - they're parsed as names but aren't actual identifiers
                                if name.syntax().text() == "." {
                                    None
                                } else {
                                    Some(name)
                                }
                            }
                            _ => None,
                        })
                        .collect();

                    // Apply the exact legacy algorithm for InterpolationTarget::Reference
                    let smid = desugarer.new_smid(chunk_span);
                    if names.len() == 1 {
                        // Single name case: just desugar the name and varify it
                        let name = &names[0];
                        let name_text = name.syntax().text().to_string();
                        let name_expr = core::name(smid, name_text);
                        desugarer.varify(name_expr)
                    } else {
                        // Multiple names case: build [name, dot, name, dot, name] soup
                        let mut v = Vec::new();
                        let mut items: Vec<_> = names.to_vec();

                        // Process from end to beginning, just like legacy AST
                        while items.len() > 1 {
                            let name = items.pop().unwrap();
                            let name_text = name.syntax().text().to_string();
                            v.push(core::name(smid, name_text));
                            v.push(acore::dot());
                        }

                        // Add the final name (the first one in the original order)
                        if let Some(final_name) = items.pop() {
                            let final_name_text = final_name.syntax().text().to_string();
                            let name_expr = core::name(smid, final_name_text);
                            v.push(desugarer.varify(name_expr));
                        }

                        // Reverse the entire vector, just like legacy AST
                        v.reverse();
                        core::soup(smid, v)
                    }
                } else if let Some(target_token) = interp.target() {
                    if let Some(target_text) = target_token.value() {
                        if target_text.is_empty() {
                            // Empty {} means anonymous positional parameter
                            let smid = desugarer.new_smid(chunk_span);
                            let fv = desugarer
                                .add_pending_string_anaphor(Anaphor::ExplicitAnonymous(smid));
                            core::var(smid, fv)
                        } else if let Ok(num) = target_text.parse::<i32>() {
                            // Numeric index like {0}, {1}, {2}
                            let smid = desugarer.new_smid(chunk_span);
                            let fv = desugarer
                                .add_pending_string_anaphor(Anaphor::ExplicitNumbered(num));
                            core::var(smid, fv)
                        } else {
                            // Variable reference like {name}
                            core::var(desugarer.new_smid(chunk_span), desugarer.var(&target_text))
                        }
                    } else {
                        // No value means anonymous positional parameter
                        let smid = desugarer.new_smid(chunk_span);
                        let fv =
                            desugarer.add_pending_string_anaphor(Anaphor::ExplicitAnonymous(smid));
                        core::var(smid, fv)
                    }
                } else {
                    // No target means anonymous positional parameter
                    let smid = desugarer.new_smid(chunk_span);
                    let fv = desugarer.add_pending_string_anaphor(Anaphor::ExplicitAnonymous(smid));
                    core::var(smid, fv)
                };

                // Apply format spec if present
                if let Some(format_spec) = interp.format_spec() {
                    if let Some(format) = format_spec.value() {
                        if !format.is_empty() {
                            if format.as_bytes()[0] as char == '%' {
                                target_expr =
                                    acore::app(rt::fmt(), vec![target_expr, acore::str(format)]);
                            } else {
                                let smid = desugarer.new_smid(chunk_span);
                                target_expr = acore::app(
                                    core::var(smid, desugarer.var(&format)),
                                    vec![target_expr],
                                );
                            }
                        }
                    }
                }

                // Apply default string conversion for interpolated expressions
                acore::app(rt::str(), vec![target_expr])
            }
            rowan_ast::StringChunk::EscapedOpen(_) => {
                // Escaped {{ becomes literal {
                let chunk_span = text_range_to_span(chunk.syntax().text_range());
                RcExpr::from(Expr::Literal(
                    desugarer.new_smid(chunk_span),
                    Primitive::Str("{".to_string()),
                ))
            }
            rowan_ast::StringChunk::EscapedClose(_) => {
                // Escaped }} becomes literal }
                let chunk_span = text_range_to_span(chunk.syntax().text_range());
                RcExpr::from(Expr::Literal(
                    desugarer.new_smid(chunk_span),
                    Primitive::Str("}".to_string()),
                ))
            }
        };
        translated_chunks.push(chunk_expr);
    }

    // Create the final string expression using JOIN intrinsic
    let smid = desugarer.new_smid(span);
    let mut expr = core::app(
        smid,
        rt::join(),
        vec![core::list(smid, translated_chunks), acore::str("")],
    );

    // If there are any anaphora wrap into a lambda
    if desugarer.has_pending_string_anaphora() {
        use crate::core::anaphora;
        use crate::core::transform::succ;
        let binders = anaphora::to_binding_pattern(desugarer.pending_string_anaphora())?;
        desugarer.clear_pending_string_anaphora();
        expr = core::lam(expr.smid(), binders, succ::succ(&expr)?);
    }

    Ok(expr)
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
    // Define PendingLookup enum locally since we removed the legacy AST module
    #[derive(Debug, Clone, PartialEq)]
    enum PendingLookup {
        None,
        Static,
        Dynamic,
    }

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
                            _ => rebodied,
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
                            _ => rebodied,
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
            rowan_ast::DeclarationKind::BracketPair(_, bracket_expr, _) => {
                bracket_expr.bracket_pair_name().ok_or_else(|| {
                    CoreError::InvalidEmbedding(
                        "bracket pair declaration has no bracket pair name".to_string(),
                        Smid::default(),
                    )
                })
            }
            rowan_ast::DeclarationKind::MalformedHead(_) => Err(CoreError::InvalidEmbedding(
                "malformed declaration head".to_string(),
                Smid::default(),
            )),
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "missing declaration head".to_string(),
            Smid::default(),
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
            let metadata: DesugarPhaseDeclarationMetadata =
                core_meta.read_metadata().unwrap_or_default();
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
            desugarer.record_doc(doc.clone(), &components.name, components.args.clone());
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
            // For core embedding, we use the raw Rowan body before desugaring
            let raw_body = if let Some(body) = decl.body() {
                if let Some(body_soup) = body.soup() {
                    body_soup
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "empty declaration body in core embedding".to_string(),
                        desugarer.new_smid(components.span),
                    ));
                }
            } else {
                return Err(CoreError::InvalidEmbedding(
                    "missing declaration body in core embedding".to_string(),
                    desugarer.new_smid(components.span),
                ));
            };

            core_from_rowan_embedding(desugarer, &raw_body)?
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
            components
                .fixity
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
        let keys: Vec<_> = self
            .declarations()
            .filter_map(|decl| {
                if let Some(head) = decl.head() {
                    let kind = head.classify_declaration();
                    match kind {
                        rowan_ast::DeclarationKind::Property(prop) => Some(prop.text().to_string()),
                        rowan_ast::DeclarationKind::Function(func, _) => {
                            Some(func.text().to_string())
                        }
                        rowan_ast::DeclarationKind::Prefix(_, op, _) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::Postfix(_, _, op) => {
                            Some(op.text().to_string())
                        }
                        rowan_ast::DeclarationKind::Binary(_, _, op, _) => {
                            Some(op.text().to_string())
                        }
                        rowan_ast::DeclarationKind::Nullary(_, op) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::BracketPair(_, bracket_expr, _) => {
                            bracket_expr.bracket_pair_name()
                        }
                        rowan_ast::DeclarationKind::MalformedHead(_) => None,
                    }
                } else {
                    None
                }
            })
            .collect();

        desugarer.env_mut().push_keys(keys);

        // Convert declarations to bindings
        let bindings = self
            .declarations()
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
                expr = RcExpr::from(Expr::Meta(desugarer.new_smid(span), expr, stripped_meta));
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
        let keys: Vec<_> = self
            .declarations()
            .filter_map(|decl| {
                if let Some(head) = decl.head() {
                    let kind = head.classify_declaration();
                    match kind {
                        rowan_ast::DeclarationKind::Property(prop) => Some(prop.text().to_string()),
                        rowan_ast::DeclarationKind::Function(func, _) => {
                            Some(func.text().to_string())
                        }
                        rowan_ast::DeclarationKind::Prefix(_, op, _) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::Postfix(_, _, op) => {
                            Some(op.text().to_string())
                        }
                        rowan_ast::DeclarationKind::Binary(_, _, op, _) => {
                            Some(op.text().to_string())
                        }
                        rowan_ast::DeclarationKind::Nullary(_, op) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::BracketPair(_, bracket_expr, _) => {
                            bracket_expr.bracket_pair_name()
                        }
                        rowan_ast::DeclarationKind::MalformedHead(_) => None,
                    }
                } else {
                    None
                }
            })
            .collect();

        desugarer.env_mut().push_keys(keys);

        // Convert declarations to bindings
        let bindings = self
            .declarations()
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
                expr = RcExpr::from(Expr::Meta(desugarer.new_smid(span), expr, stripped_meta));
            }
        }

        // Embed in imports if required
        expr = imports.iter().rfold(expr, |acc, import| import.rebody(acc));

        desugarer.env_mut().pop();
        Ok(expr)
    }
}
