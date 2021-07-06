//! A target specification
use std::fmt;

/// A Target represents an entry point in eucalypt code.
///
/// Targets are defined by the `target` metadata key and the `:main`
/// metadata symbol which is a shortcut for `target: :main`.
///
/// As well as identifying entry points they can define output format
/// and documentation. `path` is the statically determined path within
/// eucalypt block structure where the target is found (and hence the
/// evaluand for entering at the target).
///
/// The default target (all blank) is used to represent the whole unit.
#[derive(Eq, PartialEq, Hash, Clone, Debug, Default)]
pub struct Target {
    /// Target name
    name: String,
    /// Documentation for target
    doc: String,
    /// Optionally default format for target
    format: Option<String>,
    /// Name components
    path: Vec<String>,
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let format_str = match &self.format {
            Some(fstr) => format!("(as: {}) ", fstr),
            None => "".to_string(),
        };

        let path_str = self.path.join(".");

        write!(f, "{} {}[path: {}]", self.name, format_str, path_str)
    }
}

impl Target {
    /// Create a Target from information in metadata
    pub fn new(name: String, doc: String, format: Option<String>, path: Vec<String>) -> Target {
        Target {
            name,
            doc,
            format,
            path,
        }
    }

    /// Return name
    pub fn name(&self) -> &String {
        &self.name
    }

    /// Return documentation associated with the target
    pub fn doc(&self) -> &String {
        &self.doc
    }

    /// Return format
    pub fn format(&self) -> &Option<String> {
        &self.format
    }

    pub fn path(&self) -> &Vec<String> {
        &self.path
    }

    /// Return a version of the target where the path is
    pub fn under(&self, root: &str) -> Self {
        let mut path = vec![root.to_string()];
        path.extend(self.path.clone());
        Self {
            path,
            ..self.clone()
        }
    }
}
