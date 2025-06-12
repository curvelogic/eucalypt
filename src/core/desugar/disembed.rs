//! Support desugaring embedded core syntax

use crate::{
    common::sourcemap::Smid,
    core::{error::CoreError, expr::*},
    syntax::{ast::*, span::HasSpan},
};

use moniker::FreeVar;

use super::{desugarer::Desugarer, literal::desugar_literal};
use crate::syntax::ast::Expression;

/// Convert AST expression containing core embedding to core expr
pub fn core_from_embedding(
    desugarer: &mut Desugarer,
    ast: &Expression,
) -> Result<RcExpr, CoreError> {
    let smid = desugarer.new_smid(ast.span());
    match ast {
        Expression::List(_, xs) => dispatch(desugarer, smid, xs),
        _ => Err(CoreError::InvalidEmbedding("not a list".to_string(), smid)),
    }
}

/// Inspect tag header and dispatch
fn dispatch(
    desugarer: &mut Desugarer,
    smid: Smid,
    items: &[Expression],
) -> Result<RcExpr, CoreError> {
    let head = items
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("empty embedding list".to_string(), smid))?;
    if let Expression::Lit(Literal::Sym(_, tag)) = head {
        match tag.as_str() {
            "c-var" => c_var(desugarer, smid, &items[1..]),
            "c-let" => c_let(desugarer, smid, &items[1..]),
            "c-bif" => c_bif(desugarer, smid, &items[1..]),
            "c-lit" => c_lit(desugarer, smid, &items[1..]),
            "c-lookup" => c_lookup(desugarer, smid, &items[1..]),
            "c-name" => c_name(desugarer, smid, &items[1..]),
            "c-bk-ana" => c_bk_ana(desugarer, smid, &items[1..]),
            "c-ex-ana" => c_ex_ana(desugarer, smid, &items[1..]),
            "c-list" => c_list(desugarer, smid, &items[1..]),
            "c-block" => c_block(desugarer, smid, &items[1..]),
            "c-meta" => c_meta(desugarer, smid, &items[1..]),
            "c-args" => c_args(desugarer, smid, &items[1..]),
            "c-lam" => c_lam(desugarer, smid, &items[1..]),
            "c-app" => c_app(desugarer, smid, &items[1..]),
            "c-soup" => c_soup(desugarer, smid, &items[1..]),
            "c-op" => c_op(desugarer, smid, &items[1..]),
            "e-unresolved" => e_unresolved(desugarer, smid, &items[1..]),
            "e-redeclaration" => e_redeclaration(desugarer, smid, &items[1..]),
            "e-eliminated" => e_eliminated(desugarer, smid, &items[1..]),
            "e-pseudodot" => e_pseudodot(desugarer, smid, &items[1..]),
            "e-pseudocall" => e_pseudocall(desugarer, smid, &items[1..]),
            "e-pseudocat" => e_pseudocat(desugarer, smid, &items[1..]),
            _ => Err(CoreError::InvalidEmbedding(
                "unknown embedding tag".to_string(),
                smid,
            )),
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "embedding tag not symbol".to_string(),
            smid,
        ))
    }
}

/// [:c-var "x"]
fn c_var(desugarer: &mut Desugarer, smid: Smid, args: &[Expression]) -> Result<RcExpr, CoreError> {
    let name = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing variable name".to_string(), smid))?;
    if let Expression::Lit(Literal::Str(_, n)) = name {
        let fv = desugarer
            .env()
            .get(n)
            .ok_or_else(|| CoreError::UnresolvedVariable(smid, n.clone()))?;
        Ok(core::var(smid, fv.clone()))
    } else {
        Err(CoreError::InvalidEmbedding(
            "variable name not string".to_string(),
            smid,
        ))
    }
}

/// [:c-let {x: binding ...} body]
fn c_let(desugarer: &mut Desugarer, smid: Smid, args: &[Expression]) -> Result<RcExpr, CoreError> {
    let bindings = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing bindings".to_string(), smid))?;
    let body = args
        .get(1)
        .ok_or_else(|| CoreError::InvalidEmbedding("missing body".to_string(), smid))?;

    if let Expression::Block(block) = bindings {
        let keys = block
            .declarations
            .iter()
            .map(|decl| decl.name().name().to_string());
        desugarer.env_mut().push_keys(keys);

        let items = block
            .declarations
            .iter()
            .map(|decl| {
                let key = desugarer
                    .env()
                    .get(&decl.name().name().to_string())
                    .cloned()
                    .unwrap();
                core_from_embedding(desugarer, decl.definition()).map(|v| (key, v))
            })
            .collect::<Result<Vec<(FreeVar<String>, RcExpr)>, CoreError>>()?;
        let body_expr = core_from_embedding(desugarer, body)?;
        Ok(core::let_(smid, items, body_expr))
    } else {
        Err(CoreError::InvalidEmbedding(
            "bindings not a block".to_string(),
            smid,
        ))
    }
}

/// [:c-bif "EQ"]
fn c_bif(_desugarer: &mut Desugarer, smid: Smid, args: &[Expression]) -> Result<RcExpr, CoreError> {
    let name = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing intrinsic name".to_string(), smid))?;
    if let Expression::Lit(Literal::Sym(_, n)) = name {
        Ok(core::bif(smid, n))
    } else {
        Err(CoreError::InvalidEmbedding(
            "non-symbol intrinsic name".to_string(),
            smid,
        ))
    }
}

/// [:c-lit x]
fn c_lit(desugarer: &mut Desugarer, smid: Smid, args: &[Expression]) -> Result<RcExpr, CoreError> {
    let lit = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing literal value".to_string(), smid))?;
    if let Expression::Lit(literal) = lit {
        Ok(desugar_literal(desugarer, literal))
    } else {
        Err(CoreError::InvalidEmbedding("bad literal".to_string(), smid))
    }
}

/// [:c-lookup expr "k" fallback]
fn c_lookup(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[Expression],
) -> Result<RcExpr, CoreError> {
    let expr = args.first().ok_or_else(|| {
        CoreError::InvalidEmbedding("missing target expression".to_string(), smid)
    })?;
    let obj = core_from_embedding(desugarer, expr)?;

    let name = match args
        .get(1)
        .ok_or_else(|| CoreError::InvalidEmbedding("missing lookup key".to_string(), smid))?
    {
        Expression::Lit(Literal::Str(_, n)) => Ok(n),
        _ => Err(CoreError::InvalidEmbedding(
            "non-string lookup key".to_string(),
            smid,
        )),
    }?;

    let fb = args
        .get(2)
        .and_then(|x| core_from_embedding(desugarer, x).ok());

    Ok(core::lookup(smid, obj, name, fb))
}

/// [:c-name "x"]
fn c_name(
    _desugarer: &mut Desugarer,
    smid: Smid,
    args: &[Expression],
) -> Result<RcExpr, CoreError> {
    let name = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing name".to_string(), smid))?;
    if let Expression::Lit(Literal::Str(_, n)) = name {
        Ok(core::name(smid, n))
    } else {
        Err(CoreError::InvalidEmbedding(
            "non-string name".to_string(),
            smid,
        ))
    }
}

fn c_bk_ana(
    _desugarer: &mut Desugarer,
    _smid: Smid,
    _args: &[Expression],
) -> Result<RcExpr, CoreError> {
    todo!();
}

fn c_ex_ana(
    _desugarer: &mut Desugarer,
    _smid: Smid,
    _args: &[Expression],
) -> Result<RcExpr, CoreError> {
    todo!();
}

/// [:c-list x y z]
fn c_list(desugarer: &mut Desugarer, smid: Smid, args: &[Expression]) -> Result<RcExpr, CoreError> {
    let exprs = args
        .iter()
        .map(|arg: &Expression| core_from_embedding(desugarer, arg))
        .collect::<Result<Vec<RcExpr>, CoreError>>()?;
    Ok(core::list(smid, exprs))
}

/// [:c-block {x: x, y: y...}]
fn c_block(
    desugarer: &mut Desugarer,
    smid: Smid,
    args: &[Expression],
) -> Result<RcExpr, CoreError> {
    let arg = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing block content".to_string(), smid))?;

    if let Expression::Block(block) = arg {
        let entries = block
            .declarations
            .iter()
            .map(|decl| {
                core_from_embedding(desugarer, decl.definition())
                    .map(|val| (decl.name().name().to_string(), val))
            })
            .collect::<Result<Vec<(String, RcExpr)>, CoreError>>()?;

        Ok(core::block(smid, entries))
    } else {
        Err(CoreError::InvalidEmbedding(
            "bad block content".to_string(),
            smid,
        ))
    }
}

// [:c-meta x meta]
fn c_meta(desugarer: &mut Desugarer, smid: Smid, args: &[Expression]) -> Result<RcExpr, CoreError> {
    let expr = core_from_embedding(
        desugarer,
        args.first().ok_or_else(|| {
            CoreError::InvalidEmbedding("missing metadata target".to_string(), smid)
        })?,
    )?;
    let meta = core_from_embedding(
        desugarer,
        args.get(1).ok_or_else(|| {
            CoreError::InvalidEmbedding("missing metadata value".to_string(), smid)
        })?,
    )?;
    Ok(core::meta(smid, expr, meta))
}

/// [:c-args x y z]
fn c_args(desugarer: &mut Desugarer, smid: Smid, args: &[Expression]) -> Result<RcExpr, CoreError> {
    let exprs = args
        .iter()
        .map(|arg: &Expression| core_from_embedding(desugarer, arg))
        .collect::<Result<Vec<RcExpr>, CoreError>>()?;
    Ok(core::arg_tuple(smid, exprs))
}

/// [:c-lam ["x" "y"] ...x...]
fn c_lam(desugarer: &mut Desugarer, smid: Smid, args: &[Expression]) -> Result<RcExpr, CoreError> {
    let bound_vars = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing bindings".to_string(), smid))?;
    let mut var_names = vec![];
    if let Expression::List(span, vars) = bound_vars {
        for var in vars {
            if let Expression::Lit(Literal::Str(_, v)) = var {
                var_names.push(v.clone());
            } else {
                return Err(CoreError::InvalidEmbedding(
                    "non-string bound variable name".to_string(),
                    desugarer.new_smid(*span),
                ));
            }
        }
        desugarer.env_mut().push_keys(var_names.clone());
    } else {
        return Err(CoreError::InvalidEmbedding(
            "non-list bound vars".to_string(),
            smid,
        ));
    }

    let body = core_from_embedding(
        desugarer,
        args.get(1)
            .ok_or_else(|| CoreError::InvalidEmbedding("missing body".to_string(), smid))?,
    )?;

    let vars = var_names.iter().map(|n| desugarer.var(n)).collect();
    Ok(core::lam(smid, vars, body))
}

/// [:c-app f [x y z]]
fn c_app(desugarer: &mut Desugarer, smid: Smid, args: &[Expression]) -> Result<RcExpr, CoreError> {
    let f = core_from_embedding(
        desugarer,
        args.first()
            .ok_or_else(|| CoreError::InvalidEmbedding("missing function".to_string(), smid))?,
    )?;
    let params = args
        .get(1)
        .ok_or_else(|| CoreError::InvalidEmbedding("missing parameters".to_string(), smid))?;

    if let Expression::List(_, xs) = params {
        let exprs = xs
            .iter()
            .map(|arg: &Expression| core_from_embedding(desugarer, arg))
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
fn c_soup(desugarer: &mut Desugarer, smid: Smid, args: &[Expression]) -> Result<RcExpr, CoreError> {
    let exprs = args
        .iter()
        .map(|arg: &Expression| core_from_embedding(desugarer, arg))
        .collect::<Result<Vec<RcExpr>, CoreError>>()?;
    Ok(core::soup(smid, exprs))
}

/// [:c-op :unary-prefix 88]
fn c_op(desugarer: &mut Desugarer, smid: Smid, args: &[Expression]) -> Result<RcExpr, CoreError> {
    let fixity_arg = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing fixity".to_string(), smid))?;
    let precedence_arg = args
        .get(1)
        .ok_or_else(|| CoreError::InvalidEmbedding("missing precedence".to_string(), smid))?;
    let expr = core_from_embedding(
        desugarer,
        args.get(2).ok_or_else(|| {
            CoreError::InvalidEmbedding("missing operator definition".to_string(), smid)
        })?,
    )?;

    let fixity = match fixity_arg {
        Expression::Lit(Literal::Sym(span, fixity_name)) => match fixity_name.as_str() {
            "unary-prefix" => Ok(Fixity::UnaryPrefix),
            "unary-postfix" => Ok(Fixity::UnaryPostfix),
            "infix-left" => Ok(Fixity::InfixLeft),
            "infix-right" => Ok(Fixity::InfixRight),
            _ => Err(CoreError::InvalidEmbedding(
                "unknown fixity".to_string(),
                desugarer.new_smid(*span),
            )),
        },
        _ => Err(CoreError::InvalidEmbedding("bad fixity".to_string(), smid)),
    }?;

    let precedence_num = match precedence_arg {
        Expression::Lit(Literal::Num(_, p)) => Ok(p),
        _ => Err(CoreError::InvalidEmbedding(
            "non-numeric precedence".to_string(),
            smid,
        )),
    }?;

    let precedence = match precedence_num.as_i64() {
        Some(p) => Ok(p as i32),
        None => Err(CoreError::InvalidEmbedding(
            "bad precedence".to_string(),
            smid,
        )),
    }?;

    Ok(core::op(smid, fixity, precedence, expr))
}

/// [:e-unresolved "x"]
fn e_unresolved(
    _desugarer: &mut Desugarer,
    smid: Smid,
    args: &[Expression],
) -> Result<RcExpr, CoreError> {
    let name = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing variable name".to_string(), smid))?;
    if let Expression::Lit(Literal::Str(_, n)) = name {
        Ok(RcExpr::from(Expr::ErrUnresolved(smid, n.clone())))
    } else {
        Err(CoreError::InvalidEmbedding(
            "non-string variable name".to_string(),
            smid,
        ))
    }
}

/// [:e-redeclaration "x"]
fn e_redeclaration(
    _desugarer: &mut Desugarer,
    smid: Smid,
    args: &[Expression],
) -> Result<RcExpr, CoreError> {
    let name = args
        .first()
        .ok_or_else(|| CoreError::InvalidEmbedding("missing variable name".to_string(), smid))?;
    if let Expression::Lit(Literal::Str(_, n)) = name {
        Ok(RcExpr::from(Expr::ErrRedeclaration(smid, n.clone())))
    } else {
        Err(CoreError::InvalidEmbedding(
            "non-string variable name".to_string(),
            smid,
        ))
    }
}

/// [:e-eliminated]
#[allow(clippy::unnecessary_wraps)]
fn e_eliminated(
    _desugarer: &mut Desugarer,
    _smid: Smid,
    _args: &[Expression],
) -> Result<RcExpr, CoreError> {
    Ok(RcExpr::from(Expr::ErrEliminated))
}

/// [:e-pseudodot]
#[allow(clippy::unnecessary_wraps)]
fn e_pseudodot(
    _desugarer: &mut Desugarer,
    _smid: Smid,
    _args: &[Expression],
) -> Result<RcExpr, CoreError> {
    Ok(RcExpr::from(Expr::ErrPseudoDot))
}

/// [:e-pseudocall]
#[allow(clippy::unnecessary_wraps)]
fn e_pseudocall(
    _desugarer: &mut Desugarer,
    _smid: Smid,
    _args: &[Expression],
) -> Result<RcExpr, CoreError> {
    Ok(RcExpr::from(Expr::ErrPseudoCall))
}

/// [:e-pseudocat]
#[allow(clippy::unnecessary_wraps)]
fn e_pseudocat(
    _desugarer: &mut Desugarer,
    _smid: Smid,
    _args: &[Expression],
) -> Result<RcExpr, CoreError> {
    Ok(RcExpr::from(Expr::ErrPseudoCat))
}
