//! Scoped type environment mapping names to `TypeScheme`s.
//!
//! `TypeEnv` tracks the types of all names currently in scope. It supports
//! push/pop for entering and leaving nested scopes (blocks, let bindings).

use std::collections::HashMap;

use super::types::TypeScheme;

/// A single scope frame: a mapping from names to their type schemes.
type Frame = HashMap<String, TypeScheme>;

/// Scoped type environment.
///
/// Scopes are maintained as a stack of frames. When looking up a name, frames
/// are searched from innermost (most recent) to outermost.
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    frames: Vec<Frame>,
}

impl TypeEnv {
    /// Create an empty type environment.
    pub fn new() -> Self {
        TypeEnv { frames: Vec::new() }
    }

    /// Push a new (empty) scope frame.
    pub fn push_scope(&mut self) {
        self.frames.push(Frame::new());
    }

    /// Pop the innermost scope frame, discarding its bindings.
    ///
    /// # Panics
    ///
    /// Panics if there are no frames to pop (indicates a logic error in the
    /// caller — mismatched push/pop calls).
    pub fn pop_scope(&mut self) {
        self.frames
            .pop()
            .expect("TypeEnv::pop_scope called with no frames on the stack");
    }

    /// Bind `name` to `scheme` in the innermost scope.
    ///
    /// If there are no frames, a top-level frame is created automatically so
    /// that bindings can be inserted before any explicit `push_scope` call.
    pub fn bind(&mut self, name: impl Into<String>, scheme: TypeScheme) {
        if self.frames.is_empty() {
            self.frames.push(Frame::new());
        }
        self.frames.last_mut().unwrap().insert(name.into(), scheme);
    }

    /// Look up `name`, searching from innermost to outermost scope.
    ///
    /// Returns `None` if the name is not bound in any frame.
    pub fn lookup(&self, name: &str) -> Option<&TypeScheme> {
        for frame in self.frames.iter().rev() {
            if let Some(scheme) = frame.get(name) {
                return Some(scheme);
            }
        }
        None
    }

    /// Returns `true` if `name` is bound in any scope.
    pub fn contains(&self, name: &str) -> bool {
        self.lookup(name).is_some()
    }

    /// Number of scope frames currently on the stack.
    pub fn depth(&self) -> usize {
        self.frames.len()
    }

    /// Iterate over all bindings visible from the current scope (innermost
    /// binding wins when there are name collisions across frames).
    pub fn bindings(&self) -> impl Iterator<Item = (&str, &TypeScheme)> {
        // Collect names we've already seen so inner frames shadow outer ones.
        let mut seen = std::collections::HashSet::new();
        self.frames
            .iter()
            .rev()
            .flat_map(|frame| frame.iter())
            .filter_map(move |(name, scheme)| {
                if seen.insert(name.as_str()) {
                    Some((name.as_str(), scheme))
                } else {
                    None
                }
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::typecheck::types::Type;

    fn number_scheme() -> TypeScheme {
        TypeScheme::mono(Type::Number)
    }

    fn string_scheme() -> TypeScheme {
        TypeScheme::mono(Type::String)
    }

    #[test]
    fn empty_env_lookup_returns_none() {
        let env = TypeEnv::new();
        assert!(env.lookup("x").is_none());
    }

    #[test]
    fn bind_and_lookup() {
        let mut env = TypeEnv::new();
        env.bind("x", number_scheme());
        assert_eq!(env.lookup("x"), Some(&number_scheme()));
    }

    #[test]
    fn inner_scope_shadows_outer() {
        let mut env = TypeEnv::new();
        env.bind("x", number_scheme());

        env.push_scope();
        env.bind("x", string_scheme());

        assert_eq!(env.lookup("x"), Some(&string_scheme()));

        env.pop_scope();
        assert_eq!(env.lookup("x"), Some(&number_scheme()));
    }

    #[test]
    fn outer_binding_visible_in_inner_scope() {
        let mut env = TypeEnv::new();
        env.bind("y", number_scheme());

        env.push_scope();
        assert_eq!(env.lookup("y"), Some(&number_scheme()));
        env.pop_scope();
    }

    #[test]
    fn pop_removes_inner_bindings() {
        let mut env = TypeEnv::new();
        env.push_scope();
        env.bind("z", string_scheme());
        env.pop_scope();

        assert!(env.lookup("z").is_none());
    }

    #[test]
    fn depth_tracks_frames() {
        let mut env = TypeEnv::new();
        assert_eq!(env.depth(), 0);
        env.push_scope();
        assert_eq!(env.depth(), 1);
        env.push_scope();
        assert_eq!(env.depth(), 2);
        env.pop_scope();
        assert_eq!(env.depth(), 1);
    }

    #[test]
    fn contains() {
        let mut env = TypeEnv::new();
        env.bind("a", number_scheme());
        assert!(env.contains("a"));
        assert!(!env.contains("b"));
    }

    #[test]
    fn bindings_iterator_shadows_correctly() {
        let mut env = TypeEnv::new();
        env.bind("x", number_scheme());
        env.bind("y", string_scheme());
        env.push_scope();
        env.bind("x", string_scheme()); // shadows outer x

        let bindings: HashMap<_, _> = env.bindings().collect();
        assert_eq!(bindings["x"], &string_scheme());
        assert_eq!(bindings["y"], &string_scheme());
        assert_eq!(bindings.len(), 2);
    }
}
