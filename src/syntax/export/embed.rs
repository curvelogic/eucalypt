//! Allow parsed AST to be quote-embedded as eucalypt
use crate::syntax::rowan::ast as rowan_ast;

/// Embed core representation in eucalypt syntax
pub trait Embed {
    /// Represent core expression in a eucalypt AST structure
    fn embed(&self) -> Option<rowan_ast::Soup>;
}

// For now, provide a simple implementation that returns None
// This trait is primarily used for core embedding functionality
// which we've implemented directly in core/export/embed.rs

impl Embed for rowan_ast::Soup {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        // Return self - already a Soup
        Some(self.clone())
    }
}

impl Embed for rowan_ast::Element {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        // For elements, we'd need to create a soup containing this element
        // This is a simplified implementation
        None
    }
}

impl Embed for rowan_ast::Literal {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        // For literals, we'd need to create a soup containing this literal
        // This is a simplified implementation
        None
    }
}

impl Embed for rowan_ast::Block {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        // For blocks, we'd need to create a soup representation
        // This is a simplified implementation
        None
    }
}

impl Embed for rowan_ast::List {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        // For lists, we'd need to create a soup representation
        // This is a simplified implementation
        None
    }
}

impl Embed for rowan_ast::Name {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        // For names, we'd need to create a soup representation
        // This is a simplified implementation
        None
    }
}

impl Embed for rowan_ast::ApplyTuple {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        // For apply tuples, we'd need to create a soup representation
        // This is a simplified implementation
        None
    }
}

impl Embed for rowan_ast::StringPattern {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        // For string patterns, we'd need to create a soup representation
        // This is a simplified implementation
        None
    }
}

impl Embed for rowan_ast::Declaration {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        // For declarations, we'd need to create a soup representation
        // This is a simplified implementation
        None
    }
}

impl Embed for rowan_ast::Unit {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        // For units, we'd need to create a soup representation
        // This is a simplified implementation
        None
    }
}
