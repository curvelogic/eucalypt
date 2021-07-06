//! Unit of core syntax with targets and imports extracted
use crate::core::doc::DeclarationDocumentation;
use crate::core::error::CoreError;
use crate::core::expr::RcExpr;
use crate::core::target::Target;
use crate::syntax::export::embed::Embed;
use crate::syntax::export::pretty;
use std::collections::HashSet;

use super::expr::acore;

/// A unit of core syntax (initially corresponding to an input) but
/// potentially merged from several later in processing.
#[derive(Default, Clone)]
pub struct TranslationUnit {
    /// Core expression for the unit
    pub expr: RcExpr,
    /// Targets discovered within the unit
    pub targets: HashSet<Target>,
    /// Doc strings read from the unit
    pub docs: Vec<DeclarationDocumentation>,
}

impl TranslationUnit {
    /// Return the specified target if it exists
    pub fn target<T: AsRef<str>>(&self, target: T) -> Option<&Target> {
        self.targets.iter().find(|n| n.name() == target.as_ref())
    }

    /// True if this unit has the target specified
    pub fn has_target<T: AsRef<str>>(&self, target: T) -> bool {
        self.targets.iter().any(|n| n.name() == target.as_ref())
    }

    /// Merge units (with bindings in self empowered to capture
    /// variables in other).)
    pub fn merge_with(self, other: Self) -> Result<TranslationUnit, CoreError> {
        Ok(TranslationUnit {
            expr: self.expr.merge_in(other.expr)?,
            targets: self.targets.union(&other.targets).cloned().collect(),
            docs: other.docs, // don't include docs from prior inputs
        })
    }

    /// Combine the units as a unit containing a single list
    ///
    /// No targets or
    pub fn listify<'a, I>(units: I) -> Result<Self, CoreError>
    where
        I: Iterator<Item = &'a Self>,
    {
        Ok(TranslationUnit {
            expr: acore::list(units.map(|u| u.expr.clone()).collect()),
            ..Default::default()
        })
    }

    /// Combine the units as a unit containing a single block with
    /// each units keys derived from existing name or generated name
    /// if required
    pub fn blockify<'a, I, S>(keys: &'a [S], units: I) -> Result<Self, CoreError>
    where
        I: Iterator<Item = &'a Self>,
        S: AsRef<str>,
    {
        let mut targets = HashSet::new();
        let mut docs = vec![];
        let mut entries = vec![];

        for (k, u) in keys.iter().zip(units) {
            let key = k.as_ref();
            targets.extend(u.targets.iter().map(|t| t.under(key)));
            docs.extend(u.docs.iter().map(|d| d.under(key)));
            entries.push((key.to_string(), u.expr.clone()));
        }

        Ok(TranslationUnit {
            expr: acore::block(entries),
            targets,
            docs,
        })
    }

    /// Return a new unit which merges the expression and targets etc.
    /// of the specified units.
    pub fn merge<I>(units: I) -> Result<Self, CoreError>
    where
        I: Iterator<Item = Self>,
    {
        // not efficient
        let mut iter = units;
        let first: Result<TranslationUnit, CoreError> = iter.next().ok_or(CoreError::EmptyMerge());

        iter.fold(first, |l, r| l.and_then(|u| u.merge_with(r)))
    }

    /// Express the parsed core syntax as an embedding in the eucalypt
    /// AST and then stream to pretty printing to output as eucalypt.
    pub fn express(&self) -> String {
        let ast = self.expr.embed();
        pretty::express(&ast)
    }

    /// Construct a rebodied version with a specified target and
    /// return the target's format if specified
    pub fn retarget(&self, target: &str) -> Result<Self, CoreError> {
        let path = self
            .targets
            .iter()
            .find(|t| t.name() == target)
            .ok_or_else(|| CoreError::TargetNotFound(target.to_string()))?
            .path();

        let body = acore::path(&path).ok_or_else(|| CoreError::BadTarget(target.to_string()))?;

        Ok(TranslationUnit {
            expr: self.expr.clone().rebody(body),
            targets: self.targets.clone(),
            docs: self.docs.clone(), // don't include docs from prior inputs
        })
    }
}
