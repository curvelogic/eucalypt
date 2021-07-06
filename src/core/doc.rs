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

impl DeclarationDocumentation {
    /// Construct a documentation item one level deeper under a named
    /// root
    pub fn under(&self, root: &str) -> Self {
        let mut path = vec![root.to_string()];
        path.extend(self.path.clone());
        Self {
            path,
            ..self.clone()
        }
    }
}
