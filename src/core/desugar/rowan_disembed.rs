//! Support desugaring embedded core syntax from Rowan AST
//!
//! This mirrors the functionality in disembed.rs but works directly with Rowan AST
//! instead of legacy AST.

use crate::{
    common::sourcemap::Smid,
    core::{error::CoreError, expr::*},
    syntax::rowan::ast::{self as rowan_ast, AstToken, Element, HasSoup},
};

use rowan::{ast::AstNode, TextRange};

use super::desugarer::Desugarer;

/// Convert a TextRange to a Span for legacy compatibility
fn text_range_to_span(range: TextRange) -> codespan::Span {
    let start = codespan::ByteIndex(range.start().into());
    let end = codespan::ByteIndex(range.end().into());
    codespan::Span::new(start, end)
}

/// Convert Rowan AST expression containing core embedding to core expr
pub fn core_from_rowan_embedding(
    desugarer: &mut Desugarer,
    soup: &rowan_ast::Soup,
) -> Result<RcExpr, CoreError> {
    let span = text_range_to_span(soup.syntax().text_range());
    let smid = desugarer.new_smid(span);

    let elements: Vec<_> = soup.elements().collect();

    // Core embedding expects a list structure
    if elements.len() == 1 {
        if let Element::List(list) = &elements[0] {
            let items: Vec<_> = list.items().collect();
            dispatch_rowan(desugarer, smid, &items)
        } else {
            Err(CoreError::InvalidEmbedding("not a list".to_string(), smid))
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "not a single list".to_string(),
            smid,
        ))
    }
}

/// Inspect tag header and dispatch to appropriate handler
fn dispatch_rowan(
    desugarer: &mut Desugarer,
    smid: Smid,
    items: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let head_soup = items
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("empty embedding list".to_string(), smid))?;

    // Extract the tag from the first item
    let head_elements: Vec<_> = head_soup.elements().collect();
    if head_elements.len() != 1 {
        return Err(CoreError::InvalidEmbedding(
            "embedding tag not single element".to_string(),
            smid,
        ));
    }

    if let Element::Lit(literal) = &head_elements[0] {
        if let Some(rowan_ast::LiteralValue::Sym(sym)) = literal.value() {
            if let Some(tag) = sym.value() {
                match tag {
                    "c-var" => c_var_rowan(desugarer, smid, &items[1..]),
                    "c-let" => c_let_rowan(desugarer, smid, &items[1..]),
                    "c-bif" => c_bif_rowan(desugarer, smid, &items[1..]),
                    "c-lit" => c_lit_rowan(desugarer, smid, &items[1..]),
                    "c-lookup" => c_lookup_rowan(desugarer, smid, &items[1..]),
                    "c-name" => c_name_rowan(desugarer, smid, &items[1..]),
                    "c-bk-ana" => c_bk_ana_rowan(desugarer, smid, &items[1..]),
                    "c-ex-ana" => c_ex_ana_rowan(desugarer, smid, &items[1..]),
                    "c-list" => c_list_rowan(desugarer, smid, &items[1..]),
                    "c-block" => c_block_rowan(desugarer, smid, &items[1..]),
                    "c-meta" => c_meta_rowan(desugarer, smid, &items[1..]),
                    "c-args" => c_args_rowan(desugarer, smid, &items[1..]),
                    "c-lam" => c_lam_rowan(desugarer, smid, &items[1..]),
                    "c-app" => c_app_rowan(desugarer, smid, &items[1..]),
                    "c-soup" => c_soup_rowan(desugarer, smid, &items[1..]),
                    "c-op" => c_op_rowan(desugarer, smid, &items[1..]),
                    "e-unresolved" => e_unresolved_rowan(desugarer, smid, &items[1..]),
                    "e-redeclaration" => e_redeclaration_rowan(desugarer, smid, &items[1..]),
                    "e-eliminated" => e_eliminated_rowan(desugarer, smid, &items[1..]),
                    "e-pseudodot" => e_pseudodot_rowan(desugarer, smid, &items[1..]),
                    "e-pseudocall" => e_pseudocall_rowan(desugarer, smid, &items[1..]),
                    "e-pseudocat" => e_pseudocat_rowan(desugarer, smid, &items[1..]),
                    _ => Err(CoreError::InvalidEmbedding(
                        format!("unknown embedding tag: {tag}"),
                        smid,
                    )),
                }
            } else {
                Err(CoreError::InvalidEmbedding(
                    "invalid symbol in embedding tag".to_string(),
                    smid,
                ))
            }
        } else {
            Err(CoreError::InvalidEmbedding(
                "embedding tag not symbol".to_string(),
                smid,
            ))
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "embedding tag not literal".to_string(),
            smid,
        ))
    }
}

/// Helper to extract string from a soup that should contain a single string literal
fn extract_string_from_soup(soup: &rowan_ast::Soup) -> Result<String, CoreError> {
    let elements: Vec<_> = soup.elements().collect();
    if elements.len() != 1 {
        return Err(CoreError::InvalidEmbedding(
            "expected single string element".to_string(),
            Smid::default(),
        ));
    }

    if let Element::Lit(literal) = &elements[0] {
        if let Some(rowan_ast::LiteralValue::Str(s)) = literal.value() {
            if let Some(text) = s.value() {
                Ok(text.to_string())
            } else {
                Err(CoreError::InvalidEmbedding(
                    "invalid string literal".to_string(),
                    Smid::default(),
                ))
            }
        } else {
            Err(CoreError::InvalidEmbedding(
                "not a string literal".to_string(),
                Smid::default(),
            ))
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "not a literal".to_string(),
            Smid::default(),
        ))
    }
}

/// [:c-var "x"]
fn c_var_rowan(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let name_soup = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing variable name".to_string(), smid))?;

    let name = extract_string_from_soup(name_soup)?;
    let fv = desugarer
        .env()
        .get(&name)
        .ok_or_else(|| CoreError::UnresolvedVariable(smid, name.clone()))?;
    Ok(core::var(smid, fv.clone()))
}

/// [:c-bif :symbol]  
fn c_bif_rowan(
    _desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let name_soup = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing intrinsic name".to_string(), smid))?;

    let elements: Vec<_> = name_soup.elements().collect();
    if elements.len() != 1 {
        return Err(CoreError::InvalidEmbedding(
            "expected single symbol element".to_string(),
            smid,
        ));
    }

    if let Element::Lit(literal) = &elements[0] {
        if let Some(rowan_ast::LiteralValue::Sym(sym)) = literal.value() {
            if let Some(name) = sym.value() {
                Ok(RcExpr::from(Expr::Intrinsic(smid, name.to_string())))
            } else {
                Err(CoreError::InvalidEmbedding(
                    "invalid symbol literal".to_string(),
                    smid,
                ))
            }
        } else {
            Err(CoreError::InvalidEmbedding(
                "not a symbol literal".to_string(),
                smid,
            ))
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "not a literal".to_string(),
            smid,
        ))
    }
}

/// [:c-name "x"]
fn c_name_rowan(
    _desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let name_soup = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing name".to_string(), smid))?;

    let name = extract_string_from_soup(name_soup)?;
    Ok(RcExpr::from(Expr::Name(smid, name)))
}

/// [:c-lit primitive]
fn c_lit_rowan(
    _desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let lit_soup = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing literal".to_string(), smid))?;

    let elements: Vec<_> = lit_soup.elements().collect();
    if elements.len() != 1 {
        return Err(CoreError::InvalidEmbedding(
            "expected single literal element".to_string(),
            smid,
        ));
    }

    if let Element::Lit(literal) = &elements[0] {
        if let Some(value) = literal.value() {
            let primitive = match value {
                rowan_ast::LiteralValue::Str(s) => {
                    if let Some(text) = s.value() {
                        Primitive::Str(text.to_string())
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid string literal".to_string(),
                            smid,
                        ));
                    }
                }
                rowan_ast::LiteralValue::Sym(sym) => {
                    if let Some(text) = sym.value() {
                        Primitive::Sym(text.to_string())
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid symbol literal".to_string(),
                            smid,
                        ));
                    }
                }
                rowan_ast::LiteralValue::Num(n) => {
                    if let Some(num) = n.value() {
                        Primitive::Num(num)
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid number literal".to_string(),
                            smid,
                        ));
                    }
                }
            };
            Ok(RcExpr::from(Expr::Literal(smid, primitive)))
        } else {
            Err(CoreError::InvalidEmbedding(
                "malformed literal".to_string(),
                smid,
            ))
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "not a literal".to_string(),
            smid,
        ))
    }
}

/// Placeholder implementations for less critical embedding types
/// These can be expanded as needed
///
/// [:c-let {x: binding ...} body]
fn c_let_rowan(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let bindings_soup = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing bindings".to_string(), smid))?;
    let body_soup = args
        .get(1)
        .ok_or_else(|| CoreError::InvalidEmbedding("missing body".to_string(), smid))?;

    // Extract bindings from block
    let bindings_elements: Vec<_> = bindings_soup.elements().collect();
    if bindings_elements.len() != 1 {
        return Err(CoreError::InvalidEmbedding(
            "bindings should be single block element".to_string(),
            smid,
        ));
    }

    if let Element::Block(block) = &bindings_elements[0] {
        let keys: Vec<String> = block
            .declarations()
            .filter_map(|decl| {
                if let Some(head) = decl.head() {
                    let kind = head.classify_declaration();
                    match kind {
                        rowan_ast::DeclarationKind::Property(prop) => Some(prop.text().to_string()),
                        _ => None,
                    }
                } else {
                    None
                }
            })
            .collect();

        desugarer.env_mut().push_keys(keys.clone());

        let items: Result<Vec<(moniker::FreeVar<String>, RcExpr)>, CoreError> = block
            .declarations()
            .map(|decl| {
                let name = if let Some(head) = decl.head() {
                    let kind = head.classify_declaration();
                    match kind {
                        rowan_ast::DeclarationKind::Property(prop) => prop.text().to_string(),
                        _ => {
                            return Err(CoreError::InvalidEmbedding(
                                "complex declaration not supported in let embedding".to_string(),
                                smid,
                            ))
                        }
                    }
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "malformed declaration in let embedding".to_string(),
                        smid,
                    ));
                };

                let key = desugarer.env().get(&name).cloned().unwrap();

                let value = if let Some(body) = decl.body() {
                    if let Some(body_soup) = body.soup() {
                        core_from_rowan_embedding(desugarer, &body_soup)?
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "empty declaration body in let embedding".to_string(),
                            smid,
                        ));
                    }
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "missing declaration body in let embedding".to_string(),
                        smid,
                    ));
                };

                Ok((key, value))
            })
            .collect();

        let body_expr = core_from_rowan_embedding(desugarer, body_soup)?;
        let items = items?;

        desugarer.env_mut().pop();

        Ok(core::let_(smid, items, body_expr))
    } else {
        Err(CoreError::InvalidEmbedding(
            "bindings not a block".to_string(),
            smid,
        ))
    }
}

/// [:c-lookup expr "k" fallback]
fn c_lookup_rowan(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let expr_soup = args.first().ok_or_else(|| {
        CoreError::InvalidEmbedding("missing target expression".to_string(), smid)
    })?;
    let obj = core_from_rowan_embedding(desugarer, expr_soup)?;

    let name_soup = args
        .get(1)
        .ok_or_else(|| CoreError::InvalidEmbedding("missing lookup key".to_string(), smid))?;

    let name = extract_string_from_soup(name_soup)
        .map_err(|_| CoreError::InvalidEmbedding("non-string lookup key".to_string(), smid))?;

    let fb = args
        .get(2)
        .and_then(|soup| core_from_rowan_embedding(desugarer, soup).ok());

    Ok(core::lookup(smid, obj, &name, fb))
}

/// [:c-bk-ana "anaphor-string"]
fn c_bk_ana_rowan(
    _desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let anaphor_soup = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing anaphor string".to_string(), smid))?;

    let anaphor_str = extract_string_from_soup(anaphor_soup)
        .map_err(|_| CoreError::InvalidEmbedding("non-string anaphor".to_string(), smid))?;

    // Parse the anaphor string to create the appropriate anaphor type
    use crate::core::anaphora::BLOCK_ANAPHORA;
    if BLOCK_ANAPHORA.is_anaphor(&anaphor_str) {
        let anaphor = BLOCK_ANAPHORA.to_explicit_anaphor(smid, &anaphor_str);
        Ok(RcExpr::from(Expr::BlockAnaphor(smid, anaphor)))
    } else {
        Err(CoreError::InvalidEmbedding(
            "invalid block anaphor".to_string(),
            smid,
        ))
    }
}

/// [:c-ex-ana "anaphor-string"]
fn c_ex_ana_rowan(
    _desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let anaphor_soup = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing anaphor string".to_string(), smid))?;

    let anaphor_str = extract_string_from_soup(anaphor_soup)
        .map_err(|_| CoreError::InvalidEmbedding("non-string anaphor".to_string(), smid))?;

    // Parse the anaphor string to create the appropriate anaphor type
    use crate::core::anaphora::EXPR_ANAPHORA;
    if EXPR_ANAPHORA.is_anaphor(&anaphor_str) {
        let anaphor = EXPR_ANAPHORA.to_explicit_anaphor(smid, &anaphor_str);
        Ok(RcExpr::from(Expr::ExprAnaphor(smid, anaphor)))
    } else {
        Err(CoreError::InvalidEmbedding(
            "invalid expression anaphor".to_string(),
            smid,
        ))
    }
}

/// [:c-list x y z]
fn c_list_rowan(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let exprs = args
        .iter()
        .map(|soup| core_from_rowan_embedding(desugarer, soup))
        .collect::<Result<Vec<RcExpr>, CoreError>>()?;
    Ok(core::list(smid, exprs))
}

/// [:c-block {x: x, y: y...}]
fn c_block_rowan(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let arg_soup = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing block content".to_string(), smid))?;

    let elements: Vec<_> = arg_soup.elements().collect();
    if elements.len() != 1 {
        return Err(CoreError::InvalidEmbedding(
            "block content should be single element".to_string(),
            smid,
        ));
    }

    if let Element::Block(block) = &elements[0] {
        let entries: Result<Vec<(String, RcExpr)>, CoreError> = block
            .declarations()
            .map(|decl| {
                let name = if let Some(head) = decl.head() {
                    let kind = head.classify_declaration();
                    match kind {
                        rowan_ast::DeclarationKind::Property(prop) => prop.text().to_string(),
                        _ => {
                            return Err(CoreError::InvalidEmbedding(
                                "complex declaration not supported in block embedding".to_string(),
                                smid,
                            ))
                        }
                    }
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "malformed declaration in block embedding".to_string(),
                        smid,
                    ));
                };

                let value = if let Some(body) = decl.body() {
                    if let Some(body_soup) = body.soup() {
                        core_from_rowan_embedding(desugarer, &body_soup)?
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "empty declaration body in block embedding".to_string(),
                            smid,
                        ));
                    }
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "missing declaration body in block embedding".to_string(),
                        smid,
                    ));
                };

                Ok((name, value))
            })
            .collect();

        Ok(core::block(smid, entries?))
    } else {
        Err(CoreError::InvalidEmbedding(
            "bad block content".to_string(),
            smid,
        ))
    }
}

/// [:c-meta x meta]
fn c_meta_rowan(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let expr = core_from_rowan_embedding(
        desugarer,
        args.first().ok_or_else(|| {
            CoreError::InvalidEmbedding("missing metadata target".to_string(), smid)
        })?,
    )?;
    let meta = core_from_rowan_embedding(
        desugarer,
        args.get(1).ok_or_else(|| {
            CoreError::InvalidEmbedding("missing metadata value".to_string(), smid)
        })?,
    )?;
    Ok(core::meta(smid, expr, meta))
}

/// [:c-args x y z]
fn c_args_rowan(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let exprs = args
        .iter()
        .map(|soup| core_from_rowan_embedding(desugarer, soup))
        .collect::<Result<Vec<RcExpr>, CoreError>>()?;
    Ok(core::arg_tuple(smid, exprs))
}

/// [:c-lam ["x" "y"] ...x...]
fn c_lam_rowan(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let bound_vars_soup = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing bindings".to_string(), smid))?;

    // Extract variable names from list
    let elements: Vec<_> = bound_vars_soup.elements().collect();
    if elements.len() != 1 {
        return Err(CoreError::InvalidEmbedding(
            "bindings should be single element".to_string(),
            smid,
        ));
    }

    let mut var_names = vec![];
    if let Element::List(list) = &elements[0] {
        for item in list.items() {
            let name = extract_string_from_soup(&item).map_err(|_| {
                CoreError::InvalidEmbedding("non-string bound variable name".to_string(), smid)
            })?;
            var_names.push(name);
        }
        desugarer.env_mut().push_keys(var_names.clone());
    } else {
        return Err(CoreError::InvalidEmbedding(
            "non-list bound vars".to_string(),
            smid,
        ));
    }

    let body = core_from_rowan_embedding(
        desugarer,
        args.get(1)
            .ok_or_else(|| CoreError::InvalidEmbedding("missing body".to_string(), smid))?,
    )?;

    let vars = var_names.iter().map(|n| desugarer.var(n)).collect();

    desugarer.env_mut().pop();

    Ok(core::lam(smid, vars, body))
}

/// [:c-app f [x y z]]
fn c_app_rowan(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let f = core_from_rowan_embedding(
        desugarer,
        args.first()
            .ok_or_else(|| CoreError::InvalidEmbedding("missing function".to_string(), smid))?,
    )?;
    let params_soup = args
        .get(1)
        .ok_or_else(|| CoreError::InvalidEmbedding("missing parameters".to_string(), smid))?;

    let params_elements: Vec<_> = params_soup.elements().collect();
    if params_elements.len() != 1 {
        return Err(CoreError::InvalidEmbedding(
            "parameters should be single element".to_string(),
            smid,
        ));
    }

    if let Element::List(list) = &params_elements[0] {
        let exprs = list
            .items()
            .map(|soup| core_from_rowan_embedding(desugarer, &soup))
            .collect::<Result<Vec<RcExpr>, CoreError>>()?;
        Ok(core::app(smid, f, exprs))
    } else {
        Err(CoreError::InvalidEmbedding(
            "non-list application arguments".to_string(),
            smid,
        ))
    }
}

/// [:c-soup x y z]
fn c_soup_rowan(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let exprs = args
        .iter()
        .map(|soup| core_from_rowan_embedding(desugarer, soup))
        .collect::<Result<Vec<RcExpr>, CoreError>>()?;
    Ok(core::soup(smid, exprs))
}

/// [:c-op :unary-prefix 88 expr]
fn c_op_rowan(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let fixity_soup = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing fixity".to_string(), smid))?;
    let precedence_soup = args
        .get(1)
        .ok_or_else(|| CoreError::InvalidEmbedding("missing precedence".to_string(), smid))?;
    let expr_soup = args.get(2).ok_or_else(|| {
        CoreError::InvalidEmbedding("missing operator definition".to_string(), smid)
    })?;

    let expr = core_from_rowan_embedding(desugarer, expr_soup)?;

    // Extract fixity from symbol
    let fixity_elements: Vec<_> = fixity_soup.elements().collect();
    if fixity_elements.len() != 1 {
        return Err(CoreError::InvalidEmbedding(
            "fixity should be single element".to_string(),
            smid,
        ));
    }

    let fixity = if let Element::Lit(literal) = &fixity_elements[0] {
        if let Some(rowan_ast::LiteralValue::Sym(sym)) = literal.value() {
            if let Some(fixity_name) = sym.value() {
                match fixity_name {
                    "unary-prefix" => Ok(Fixity::UnaryPrefix),
                    "unary-postfix" => Ok(Fixity::UnaryPostfix),
                    "infix-left" => Ok(Fixity::InfixLeft),
                    "infix-right" => Ok(Fixity::InfixRight),
                    _ => Err(CoreError::InvalidEmbedding(
                        "unknown fixity".to_string(),
                        smid,
                    )),
                }
            } else {
                Err(CoreError::InvalidEmbedding(
                    "bad fixity symbol".to_string(),
                    smid,
                ))
            }
        } else {
            Err(CoreError::InvalidEmbedding(
                "fixity not a symbol".to_string(),
                smid,
            ))
        }
    } else {
        Err(CoreError::InvalidEmbedding("bad fixity".to_string(), smid))
    }?;

    // Extract precedence from number
    let precedence_elements: Vec<_> = precedence_soup.elements().collect();
    if precedence_elements.len() != 1 {
        return Err(CoreError::InvalidEmbedding(
            "precedence should be single element".to_string(),
            smid,
        ));
    }

    let precedence = if let Element::Lit(literal) = &precedence_elements[0] {
        if let Some(rowan_ast::LiteralValue::Num(num)) = literal.value() {
            if let Some(precedence_num) = num.value() {
                match precedence_num.as_i64() {
                    Some(p) => Ok(p as i32),
                    None => Err(CoreError::InvalidEmbedding(
                        "bad precedence".to_string(),
                        smid,
                    )),
                }
            } else {
                Err(CoreError::InvalidEmbedding(
                    "bad precedence number".to_string(),
                    smid,
                ))
            }
        } else {
            Err(CoreError::InvalidEmbedding(
                "precedence not a number".to_string(),
                smid,
            ))
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "non-numeric precedence".to_string(),
            smid,
        ))
    }?;

    Ok(core::op(smid, fixity, precedence, expr))
}

/// [:e-unresolved "x"]
fn e_unresolved_rowan(
    _desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let name_soup = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing variable name".to_string(), smid))?;

    let name = extract_string_from_soup(name_soup)
        .map_err(|_| CoreError::InvalidEmbedding("non-string variable name".to_string(), smid))?;

    Ok(RcExpr::from(Expr::ErrUnresolved(smid, name)))
}

/// [:e-redeclaration "x"]
fn e_redeclaration_rowan(
    _desugarer: &mut Desugarer,
    smid: Smid,
    args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    let name_soup = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing variable name".to_string(), smid))?;

    let name = extract_string_from_soup(name_soup)
        .map_err(|_| CoreError::InvalidEmbedding("non-string variable name".to_string(), smid))?;

    Ok(RcExpr::from(Expr::ErrRedeclaration(smid, name)))
}

fn e_eliminated_rowan(
    _desugarer: &mut Desugarer,
    _smid: Smid,
    _args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    Ok(RcExpr::from(Expr::ErrEliminated))
}

fn e_pseudodot_rowan(
    _desugarer: &mut Desugarer,
    _smid: Smid,
    _args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    Ok(RcExpr::from(Expr::ErrPseudoDot))
}

fn e_pseudocall_rowan(
    _desugarer: &mut Desugarer,
    _smid: Smid,
    _args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    Ok(RcExpr::from(Expr::ErrPseudoCall))
}

fn e_pseudocat_rowan(
    _desugarer: &mut Desugarer,
    _smid: Smid,
    _args: &[rowan_ast::Soup],
) -> Result<RcExpr, CoreError> {
    Ok(RcExpr::from(Expr::ErrPseudoCat))
}

#[cfg(test)]
#[path = "rowan_disembed_test.rs"]
mod rowan_disembed_test;
