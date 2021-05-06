//! Documentation extracted during desugaring phase

/// Documentation extracted during desugaring
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct DeclarationDocumentation {
    /// Item name
    pub name: String,
    /// Any item args
    pub args: Vec<String>,
    /// Static path
    pub path: Vec<String>,
    /// Documentation metadata
    pub doc: String,
}
