//! Facilities for handling various species of anaphor
use crate::common::sourcemap::{HasSmid, Smid};
use crate::core::error::CoreError;
use crate::core::expr::*;
use moniker::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;

/// Retrieve anaphor if expression is anaphora at top level
pub fn naked_anaphora(expr: &RcExpr) -> Option<Anaphor<Smid, i32>> {
    match &*expr.inner {
        Expr::ExprAnaphor(_, anaphor) => Some(*anaphor),
        _ => None,
    }
}

/// Sort and arrange anaphora into a binding pattern
pub fn to_binding_pattern(
    anaphora: &HashMap<Anaphor<Smid, i32>, FreeVar<String>>,
) -> Result<Vec<FreeVar<String>>, CoreError> {
    let (numberless, numbered, section) = sort_anaphora(anaphora.keys().cloned());

    let mut types = 0;
    if !numberless.is_empty() {
        types += 1;
    }
    if !numbered.is_empty() {
        types += 1;
    }
    if !section.is_empty() {
        types += 1;
    }

    if types > 1 {
        let ana = numberless.first().or_else(|| section.first()).unwrap();
        Err(CoreError::MixedAnaphora(ana.smid()))
    } else if !numberless.is_empty() {
        Ok(numberless
            .iter()
            .map(|an| anaphora.get(an).unwrap().clone())
            .collect())
    } else if !section.is_empty() {
        Ok(section
            .iter()
            .map(|an| anaphora.get(an).unwrap().clone())
            .collect())
    } else if !numbered.is_empty() {
        let numbers: Vec<_> = numbered.iter().filter_map(|ana| ana.number()).collect();
        let max = numbers.iter().max().unwrap();
        let mut v: Vec<Option<FreeVar<String>>> = vec![None; *max as usize + 1];
        for n in numbers {
            v[n as usize] = Some(anaphora.get(&Anaphor::ExplicitNumbered(n)).unwrap().clone());
        }
        Ok(v.into_iter()
            .enumerate()
            .map(|(i, x)| {
                x.or_else(|| Some(FreeVar::fresh_named(format!("_d{i}"))))
                    .unwrap()
            })
            .collect())
    } else {
        panic!("empty anaphora when forming binding pattern")
    }
}

/// Sort anaphora into ordered sequences of explicit numberless,
/// explicit, number and section anaphora.
#[allow(clippy::type_complexity)]
fn sort_anaphora<U, T, N>(
    anaphora: U,
) -> (Vec<Anaphor<T, N>>, Vec<Anaphor<T, N>>, Vec<Anaphor<T, N>>)
where
    U: Iterator<Item = Anaphor<T, N>>,
    N: Ord + Hash + Clone,
    T: Ord + Hash + Clone,
{
    let mut section = Vec::new();
    let mut numbered = Vec::new();
    let mut numberless = Vec::new();

    for anaphor in anaphora {
        match anaphor {
            Anaphor::ExplicitAnonymous(_) => numberless.push(anaphor),
            Anaphor::ExplicitNumbered(_) => numbered.push(anaphor),
            Anaphor::Implicit(_, _) => section.push(anaphor),
        }
    }

    numberless.sort();
    numbered.sort();
    section.sort();

    (numberless, numbered, section)
}

/// A means of specifying an anaphor by prefix character
pub struct PrefixAnaphorIncantation {
    prefix: char,
}

pub const BLOCK_ANAPHORA: PrefixAnaphorIncantation = PrefixAnaphorIncantation { prefix: '•' };
pub const EXPR_ANAPHORA: PrefixAnaphorIncantation = PrefixAnaphorIncantation { prefix: '_' };

impl PrefixAnaphorIncantation {
    /// Is this string an anaphor of this type?
    pub fn is_anaphor(&self, s: &str) -> bool {
        s.starts_with(self.prefix) && s.chars().skip(1).all(|c| c.is_numeric())
    }

    /// Determine the anaphor number from its string representation
    pub fn number_str(&self, s: &str) -> Option<i32> {
        if let Some((i, _)) = s.char_indices().nth(1) {
            s[i..].parse::<i32>().ok()
        } else {
            None
        }
    }

    /// Convert an anaphor incantation into an explicit `Anaphor`
    pub fn to_explicit_anaphor(&self, smid: Smid, s: &str) -> Anaphor<Smid, i32> {
        match self.number_str(s) {
            Some(n) => Anaphor::ExplicitNumbered(n),
            None => Anaphor::ExplicitAnonymous(smid),
        }
    }

    /// Determine the number of the anaphor (e.g. _8 -> 8, •0 -> 0)
    pub fn number(&self, free_var: &FreeVar<String>) -> Option<i32> {
        match free_var.pretty_name {
            Some(ref n) => self.number_str(n),
            None => None,
        }
    }

    /// Create a fresh FreeVar for this number
    pub fn with_number(&self, n: i32) -> FreeVar<String> {
        FreeVar::fresh_named(format!("{}{}", self.prefix, n))
    }

    /// Create a fresh FreeVar for the unnumbered anaphor
    pub fn numberless(&self) -> FreeVar<String> {
        FreeVar::fresh_named(format!("{}", self.prefix))
    }

    /// Take a set of free var anaphora occuring in an expression and
    /// return a list of binders for a lambda (including "skipped" anaphora)
    pub fn to_binding_pattern(&self, free_vars: HashSet<FreeVar<String>>) -> Vec<Binder<String>> {
        let mut vars: Vec<FreeVar<String>> = free_vars.into_iter().collect();
        vars.sort_unstable_by_key(|v| v.pretty_name.clone()); // TODO: !?
        let max = vars.last().and_then(|v| self.number(v)).unwrap();

        if max as usize > vars.len() - 1 {
            let mut padded: Vec<Binder<String>> = Vec::with_capacity((max + 1) as usize);
            let mut i = 0;
            let mut v = 0;
            while v <= max {
                if self.number(&vars[i]).unwrap() == v {
                    padded.push(Binder(vars[i].clone()));
                    i += 1;
                } else {
                    padded.push(Binder(self.with_number(v)));
                }
                v += 1;
            }
            padded
        } else {
            vars.into_iter().map(Binder).collect()
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    pub fn test_number() {
        assert_eq!(BLOCK_ANAPHORA.number(&FreeVar::fresh_named("•8")), Some(8));
        assert_eq!(BLOCK_ANAPHORA.number(&FreeVar::fresh_named("•")), None);
        assert_eq!(
            EXPR_ANAPHORA.number(&FreeVar::fresh_named("_999")),
            Some(999)
        );
        assert_eq!(EXPR_ANAPHORA.number(&FreeVar::fresh_named("_")), None);
    }

    #[test]
    pub fn test_with_number() {
        assert_eq!(
            BLOCK_ANAPHORA.number(&BLOCK_ANAPHORA.with_number(1)),
            Some(1)
        );
        assert_eq!(
            EXPR_ANAPHORA.number(&EXPR_ANAPHORA.with_number(700)),
            Some(700)
        );
    }

    #[test]
    pub fn to_binding_pattern_with_complete_anaphora_usage() {
        let fvs: HashSet<_> = (0..3).map(|i| BLOCK_ANAPHORA.with_number(i)).collect();
        let pattern = BLOCK_ANAPHORA.to_binding_pattern(fvs);
        let pattern_numbers: Vec<Option<i32>> = pattern
            .iter()
            .map(|binder: &Binder<String>| BLOCK_ANAPHORA.number(&binder.0))
            .collect();
        assert_eq!(pattern_numbers, vec![Some(0), Some(1), Some(2)]);
    }

    #[test]
    pub fn to_binding_pattern_with_incomplete_anaphora_usage() {
        let mut fvs: HashSet<FreeVar<String>> = HashSet::new();
        fvs.insert(BLOCK_ANAPHORA.with_number(4));
        fvs.insert(BLOCK_ANAPHORA.with_number(2));

        let pattern = BLOCK_ANAPHORA.to_binding_pattern(fvs);
        let pattern_numbers: Vec<Option<i32>> = pattern
            .iter()
            .map(|binder: &Binder<String>| BLOCK_ANAPHORA.number(&binder.0))
            .collect();
        assert_eq!(
            pattern_numbers,
            vec![Some(0), Some(1), Some(2), Some(3), Some(4)]
        );
    }

    #[test]
    pub fn test_to_binding_pattern() {
        let ana0 = free("_0");
        let ana1 = free("_1");

        let mut hm: HashMap<Anaphor<Smid, i32>, FreeVar<String>> = HashMap::new();
        hm.insert(Anaphor::ExplicitNumbered(0), ana0.clone());
        hm.insert(Anaphor::ExplicitNumbered(1), ana1.clone());
        assert_eq!(to_binding_pattern(&hm).unwrap(), vec![ana0, ana1]);
    }
}
