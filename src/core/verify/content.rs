//! Verify integrity of core expression before eval / compiles
use crate::core::error::CoreError;
use crate::core::expr::*;

/// Scan the core expression for errors
pub fn verify(expr: RcExpr) -> Result<Vec<CoreError>, CoreError> {
    let mut verifier = Verifier::default();
    verifier.verify(expr)?;
    Ok(verifier.errors)
}

pub struct Verifier {
    errors: Vec<CoreError>,
}

impl Default for Verifier {
    fn default() -> Self {
        Verifier { errors: Vec::new() }
    }
}

impl Verifier {
    fn verify(&mut self, expr: RcExpr) -> Result<RcExpr, CoreError> {
        match &*expr.inner {
            Expr::ErrPseudoCall | Expr::ErrPseudoCat | Expr::ErrPseudoDot => {
                self.errors.push(CoreError::UneliminatedPseudoOperators);
                Ok(expr)
            }
            Expr::ErrEliminated => {
                // self.errors.push(CoreError::UnprunedEliminations);
                Ok(expr)
            }
            Expr::ErrUnresolved(s, n) => {
                self.errors
                    .push(CoreError::UnresolvedVariable(*s, n.clone()));
                Ok(expr)
            }
            Expr::ErrRedeclaration(s, n) => {
                self.errors
                    .push(CoreError::RedeclaredVariable(*s, n.clone()));
                Ok(expr)
            }
            Expr::Soup(s, _) => {
                self.errors.push(CoreError::UneliminatedSoup(*s));
                Ok(expr)
            }
            _ => expr.walk_safe(&mut |x| self.verify(x)),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{common::sourcemap::Smid, core::expr::acore::*};

    #[test]
    pub fn test_verify_pseudo_ops() {
        assert_eq!(
            verify(soup(vec![var(free("x")), dot(), name("a")])).unwrap(),
            vec![CoreError::UneliminatedSoup(Smid::default())]
        );
        assert_eq!(
            verify(app(
                var(free("f")),
                vec![app(cat(), vec![var(free("x")), var(free("g"))])]
            ))
            .unwrap(),
            vec![CoreError::UneliminatedPseudoOperators]
        );
        assert_eq!(
            verify(app(
                var(free("f")),
                vec![app(
                    call(),
                    vec![var(free("f")), arg_tuple(vec![var(free("g"))])]
                )]
            ))
            .unwrap(),
            vec![CoreError::UneliminatedPseudoOperators]
        );
    }
}
