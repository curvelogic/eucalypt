//! Distribute fixity metadata from definition site to call site.
use crate::common::environment::SimpleEnvironment;
use crate::common::sourcemap::Smid;
use crate::core::binding::Var;
use crate::core::error::CoreError;
use crate::core::expr::*;
use std::collections::HashMap;

pub fn distribute(expr: RcExpr) -> Result<RcExpr, CoreError> {
    Distributor::default().dist(expr)
}

/// Extract type annotation strings for operator bindings from the raw
/// (pre-cook) expression.
///
/// Before `distribute_fixities` (which strips `Meta` wrappers from operator
/// definitions), this function collects the `type:` annotation associated with
/// each operator binding so the type checker can use them for constraint
/// discharge even after cook has removed the metadata.
///
/// Returns a map from operator name (e.g. `"<"`, `">"`) to annotation string.
pub fn extract_operator_type_strings(expr: &RcExpr) -> HashMap<String, String> {
    let mut map = HashMap::new();
    collect_operator_type_strings(expr, &mut map);
    map
}

fn collect_operator_type_strings(expr: &RcExpr, out: &mut HashMap<String, String>) {
    match &*expr.inner {
        Expr::Let(_, scope, _) => {
            // Walk bindings: check each value for an operator with type annotation.
            // We only open one Let level shallowly here because `dist()` is called
            // on the closed form; the body (which may contain nested Lets from user
            // files merged with the prelude) is walked recursively.
            for (name, value) in &scope.pattern {
                if let Some(type_str) = extract_op_type_string(value) {
                    out.insert(name.clone(), type_str);
                }
            }
            // Recurse into the Let body (may contain nested Lets).
            collect_operator_type_strings(&scope.body, out);
        }
        // Peek through Meta wrappers — the merged expression is wrapped in a
        // unit-level Meta node before the outer prelude Let.
        Expr::Meta(_, inner, _) => collect_operator_type_strings(inner, out),
        _ => {}
    }
}

/// Given a binding value, extract its `type:` annotation if it wraps an
/// operator definition (`Operator` node inside zero or more `Meta` layers).
fn extract_op_type_string(value: &RcExpr) -> Option<String> {
    match &*value.inner {
        Expr::Meta(_, inner, meta_block) => {
            // If the inner expression is (or contains) an Operator, extract
            // the type: string from this Meta block.
            if contains_operator(inner) {
                extract_type_str_from_block(meta_block).or_else(|| extract_op_type_string(inner))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Returns `true` if `expr` is, or wraps (via Meta layers), an `Operator` node.
fn contains_operator(expr: &RcExpr) -> bool {
    match &*expr.inner {
        Expr::Operator(_, _, _, _) => true,
        Expr::Meta(_, inner, _) => contains_operator(inner),
        _ => false,
    }
}

/// Extract the value of the `type:` key from a block expression, if present.
fn extract_type_str_from_block(block_expr: &RcExpr) -> Option<String> {
    let block = match &*block_expr.inner {
        Expr::Block(_, b) => b,
        _ => return None,
    };
    let type_expr = block.get("type")?;
    extract_str_literal(type_expr)
}

fn extract_str_literal(expr: &RcExpr) -> Option<String> {
    match &*expr.inner {
        Expr::Literal(_, Primitive::Str(s)) => Some(s.clone()),
        _ => None,
    }
}

type Binding = (String, RcExpr);
type OpMeta = (Smid, Fixity, Precedence);
type Frame = HashMap<String, OpMeta>;
type Env = SimpleEnvironment<String, OpMeta>;

/// Distribute maintains state as we traverse through the tree
/// accumulating correspondence between names and operator metadata
#[derive(Default)]
pub struct Distributor {
    env: Env,
}

impl Distributor {
    /// If an expression defines an operator, strip off (and return)
    /// the operator metadata and expose the operator callable.
    fn expose_definition(expr: RcExpr) -> (RcExpr, Option<OpMeta>) {
        match &*expr.inner {
            Expr::Meta(_, e, _) => {
                let (e, m) = Distributor::expose_definition(e.clone());
                if m.is_some() {
                    (e, m)
                } else {
                    (expr, None)
                }
            }
            Expr::Operator(s, f, p, _e) => (_e.clone(), Some((*s, *f, *p))),
            _ => (expr, None),
        }
    }

    /// Strip the operator metadata from bindings and construct an
    /// environment frame to record them.
    ///
    /// This is shallow and does not recurse into bindings by itself.
    fn process_bindings(bindings: &[Binding]) -> (Vec<Binding>, Frame) {
        let mut frame = Frame::new();
        let mut rebound = Vec::new();

        for (name, value) in bindings {
            let (expr, op_meta) = Self::expose_definition(value.clone());
            rebound.push((name.clone(), expr));
            if let Some(op_data) = op_meta {
                frame.insert(name.clone(), op_data);
            }
        }

        (rebound, frame)
    }

    /// Distribute fixity and precedence data from definitions to call sites.
    pub fn dist(&mut self, expr: RcExpr) -> Result<RcExpr, CoreError> {
        match &*expr.inner {
            Expr::Let(s, scope, t) => {
                // Open the scope fully (both binding values and body), mirroring
                // the original `scope.clone().unbind()` from the moniker API.
                // This is necessary so that `dist` can find operator free
                // variables inside binding values as well as in the body.
                let (open_bindings, body) = open_let_scope_full(scope);

                let (mut new_bindings, ops) = Self::process_bindings(&open_bindings);

                self.env.push(ops);

                for (_, value) in &mut new_bindings {
                    *value = self.dist(value.clone())?;
                }

                // Re-close both the binding values and the body with
                // `close_let_scope` (equivalent to moniker's `Scope::new`).
                let ret = RcExpr::from(Expr::Let(
                    *s,
                    close_let_scope(new_bindings, self.dist(body)?),
                    *t,
                ));

                self.env.pop();

                Ok(ret)
            }
            Expr::Var(call_smid, Var::Free(name)) => {
                if let Some((def_smid, fixity, precedence)) = self.env.get(name) {
                    // Prefer the call-site Smid (from the user's source) over the
                    // definition-site Smid (from the prelude) so that infix operator
                    // applications carry a source location pointing at the actual
                    // usage, not the operator's definition.
                    let smid = if call_smid.is_valid() {
                        *call_smid
                    } else {
                        *def_smid
                    };
                    Ok(RcExpr::from(Expr::Operator(
                        smid,
                        *fixity,
                        *precedence,
                        expr,
                    )))
                } else {
                    Ok(expr)
                }
            }
            _ => expr.walk_safe(&mut |x| self.dist(x)),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::acore::*;
    use super::*;

    #[test]
    pub fn test_sample_1() {
        let plus = free("+");
        let minus = free("-");

        let expr = let_(
            vec![
                (plus.clone(), infixl(50, bif("FOO"))),
                (minus.clone(), infixl(50, bif("BAR"))),
            ],
            soup(vec![num(1), var(plus.clone()), num(2)]),
        );

        let expected = let_(
            vec![(plus.clone(), bif("FOO")), (minus, bif("BAR"))],
            soup(vec![num(1), infixl(50, var(plus)), num(2)]),
        );

        assert_eq!(distribute(expr).unwrap(), expected);
    }

    #[test]
    pub fn test_sample_2() {
        let plus = free("+");
        let minus = free("-");

        let expr = let_(
            vec![(plus.clone(), infixl(50, bif("FOO")))],
            let_(
                vec![(minus.clone(), infixl(50, bif("BAR")))],
                soup(vec![num(1), var(plus.clone()), num(2)]),
            ),
        );

        let expected = let_(
            vec![(plus.clone(), bif("FOO"))],
            let_(
                vec![(minus, bif("BAR"))],
                soup(vec![num(1), infixl(50, var(plus)), num(2)]),
            ),
        );

        assert_eq!(distribute(expr).unwrap(), expected);
    }

    #[test]
    pub fn test_sample_3() {
        let plus = free("+");
        let minus = free("-");

        let expr = let_(
            vec![
                (plus.clone(), infixl(50, bif("FOO"))),
                (minus.clone(), infixr(90, bif("BAR"))),
            ],
            soup(vec![num(1), var(minus.clone()), num(2)]),
        );

        let expected = let_(
            vec![(plus, bif("FOO")), (minus.clone(), bif("BAR"))],
            soup(vec![num(1), infixr(90, var(minus)), num(2)]),
        );

        assert_eq!(distribute(expr).unwrap(), expected);
    }
}
