//! Post-check alias resolution for `Primitive::TypeData` values.
//!
//! After the type checker runs and builds the complete alias map (from
//! `type-def:` and `types:` metadata), this pass walks the core
//! expression tree and resolves any `Type::Var` references inside
//! `TypeData` strings that correspond to known aliases.
//!
//! Without this pass, s-strings like `s"Shape"` are parsed as
//! `Type::Var("Shape")` but never resolved — `to-data` projects them
//! as `[:t-var :Shape]` and `to-spec` treats them as `any?`, so
//! `match?` always returns true.

use std::collections::HashMap;

use crate::core::expr::{Expr, Primitive, RcExpr};
use crate::core::typecheck::parse::{parse_scheme, render_scheme};
use crate::core::typecheck::types::Type;

use super::check::contains_var_named;

/// Walk `ty`, replacing every `Type::Var(name)` that appears in the alias
/// map with its registered concrete type.
///
/// This is a standalone version of `Checker::resolve_aliases_in_type` that
/// operates on an externally provided alias map.
fn resolve_aliases(ty: Type, aliases: &HashMap<String, Type>, resolving: &mut Vec<String>) -> Type {
    match ty {
        Type::Var(ref v, _) => {
            if let Some(alias_ty) = aliases.get(&v.0) {
                if resolving.contains(&v.0) {
                    // Cycle detected: return the bare Var as a back-reference.
                    return ty;
                }
                resolving.push(v.0.clone());
                let resolved = resolve_aliases(alias_ty.clone(), aliases, resolving);
                resolving.pop();
                if contains_var_named(&resolved, &v.0) {
                    Type::Mu(v.clone(), Box::new(resolved))
                } else {
                    resolved
                }
            } else {
                ty
            }
        }
        Type::App(f, x) => Type::App(
            Box::new(resolve_aliases(*f, aliases, resolving)),
            Box::new(resolve_aliases(*x, aliases, resolving)),
        ),
        Type::Con(_) => ty,
        Type::Forall(binders, body) => Type::Forall(
            binders,
            Box::new(resolve_aliases(*body, aliases, resolving)),
        ),
        Type::Tuple(elems) => Type::Tuple(
            elems
                .into_iter()
                .map(|e| resolve_aliases(e, aliases, resolving))
                .collect(),
        ),
        Type::PrefixList { prefix, tail } => Type::prefix_list(
            prefix
                .into_iter()
                .map(|p| resolve_aliases(p, aliases, resolving))
                .collect(),
            resolve_aliases(*tail, aliases, resolving),
        ),
        Type::Function(a, b) => Type::Function(
            Box::new(resolve_aliases(*a, aliases, resolving)),
            Box::new(resolve_aliases(*b, aliases, resolving)),
        ),
        Type::Record { fields, open, rows } => Type::Record {
            fields: fields
                .into_iter()
                .map(|(k, fp)| (k, fp.map_type(|v| resolve_aliases(v, aliases, resolving))))
                .collect(),
            open,
            rows,
        },
        Type::Union(variants) => Type::Union(
            variants
                .into_iter()
                .map(|v| resolve_aliases(v, aliases, resolving))
                .collect(),
        ),
        Type::Mu(x, body) => Type::Mu(x, Box::new(resolve_aliases(*body, aliases, resolving))),
        Type::Lam(x, body) => Type::Lam(x, Box::new(resolve_aliases(*body, aliases, resolving))),
        other => other,
    }
}

/// Walk the core expression tree, resolving aliases inside every
/// `Primitive::TypeData` value.
///
/// For each `TypeData(s)`:
/// 1. Parse `s` via `parse_scheme`
/// 2. Resolve aliases in the parsed type
/// 3. Re-render and replace with the resolved string
///
/// Nodes without `TypeData` are returned unchanged (sharing the `Rc`).
pub fn resolve_typedata_aliases(expr: &RcExpr, aliases: &HashMap<String, Type>) -> RcExpr {
    if aliases.is_empty() {
        return expr.clone();
    }
    resolve_inner(expr, aliases)
}

fn resolve_inner(expr: &RcExpr, aliases: &HashMap<String, Type>) -> RcExpr {
    match &*expr.inner {
        Expr::Literal(smid, Primitive::TypeData(s)) => {
            if let Ok((ty, constraints)) = parse_scheme(s) {
                let resolved = resolve_aliases(ty, aliases, &mut Vec::new());
                let new_s = render_scheme(&resolved, &constraints);
                if new_s != *s {
                    RcExpr::from(Expr::Literal(*smid, Primitive::TypeData(new_s)))
                } else {
                    expr.clone()
                }
            } else {
                expr.clone()
            }
        }
        _ => {
            // walk_safe expects Result; use infallible Ok
            expr.walk_safe(&mut |e| Ok::<_, std::convert::Infallible>(resolve_inner(&e, aliases)))
                .unwrap_or_else(|e| match e {})
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::sourcemap::Smid;
    use crate::core::typecheck::types::FieldPresence;

    #[test]
    fn resolves_alias_in_typedata() {
        // Build an alias map: Shape => {type: symbol, area: number}
        let mut aliases = HashMap::new();
        let shape_type = Type::Record {
            fields: vec![
                (
                    "type".to_string(),
                    FieldPresence::Required(Type::Con("symbol".to_string())),
                ),
                (
                    "area".to_string(),
                    FieldPresence::Required(Type::Con("number".to_string())),
                ),
            ]
            .into_iter()
            .collect(),
            open: false,
            rows: vec![],
        };
        aliases.insert("Shape".to_string(), shape_type);

        // Build a TypeData expression containing `s"Shape"`
        let expr = RcExpr::from(Expr::Literal(
            Smid::default(),
            Primitive::TypeData("Shape".to_string()),
        ));

        let resolved = resolve_typedata_aliases(&expr, &aliases);

        // The resolved TypeData should no longer be "Shape"
        if let Expr::Literal(_, Primitive::TypeData(s)) = &*resolved.inner {
            assert!(
                !s.contains("Shape"),
                "expected alias to be resolved, got: {s}"
            );
            assert!(s.contains("type"), "expected record field 'type' in: {s}");
            assert!(s.contains("area"), "expected record field 'area' in: {s}");
        } else {
            panic!("expected Literal(TypeData(...))");
        }
    }

    #[test]
    fn leaves_non_alias_vars_unchanged() {
        let aliases = HashMap::new();
        let expr = RcExpr::from(Expr::Literal(
            Smid::default(),
            Primitive::TypeData("a".to_string()),
        ));

        let resolved = resolve_typedata_aliases(&expr, &aliases);
        if let Expr::Literal(_, Primitive::TypeData(s)) = &*resolved.inner {
            assert_eq!(s, "a");
        } else {
            panic!("expected Literal(TypeData(...))");
        }
    }

    #[test]
    fn empty_aliases_is_noop() {
        let aliases = HashMap::new();
        let expr = RcExpr::from(Expr::Literal(
            Smid::default(),
            Primitive::TypeData("Shape".to_string()),
        ));

        let resolved = resolve_typedata_aliases(&expr, &aliases);
        assert!(std::rc::Rc::ptr_eq(&resolved.inner, &expr.inner));
    }
}
