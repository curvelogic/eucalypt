//! Type representation for eucalypt's gradual type system.
//!
//! This module defines the `Type` enum covering all type forms from the
//! gradual typing spec, plus `TypeScheme` for polymorphic types.

use std::collections::BTreeMap;
use std::fmt;

/// A unique identifier for a type variable within a `TypeScheme`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVarId(pub String);

impl fmt::Display for TypeVarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A constraint on a type variable (reserved for future use).
///
/// Currently unused — constraints are always an empty `Vec`. Included
/// in `TypeScheme` so the representation is future-proof without
/// breaking changes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constraint {
    pub function: String,
    pub args: Vec<Type>,
}

/// A polymorphic type scheme: `forall vars. body` with optional constraints.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeScheme {
    pub vars: Vec<TypeVarId>,
    /// Reserved for future constraint support — always empty for now.
    pub constraints: Vec<Constraint>,
    pub body: Type,
}

impl TypeScheme {
    /// Monomorphic scheme: no type variables.
    pub fn mono(ty: Type) -> Self {
        TypeScheme {
            vars: Vec::new(),
            constraints: Vec::new(),
            body: ty,
        }
    }

    /// Polymorphic scheme with the given type variables.
    pub fn poly(vars: Vec<TypeVarId>, ty: Type) -> Self {
        TypeScheme {
            vars,
            constraints: Vec::new(),
            body: ty,
        }
    }
}

impl fmt::Display for TypeScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.body)
        } else {
            write!(f, "forall")?;
            for v in &self.vars {
                write!(f, " {v}")?;
            }
            write!(f, ". {}", self.body)
        }
    }
}

/// All type forms in eucalypt's gradual type system.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    // ── Primitives ──────────────────────────────────────────────────────────
    /// Integer and floating-point numbers.
    Number,
    /// String values.
    String,
    /// Symbolic atoms (`:name`).
    Symbol,
    /// Boolean: `true` or `false`.
    Bool,
    /// The null value.
    Null,
    /// Zoned date-time values.
    DateTime,

    // ── Special ─────────────────────────────────────────────────────────────
    /// Gradual/dynamic — consistent with every type in both directions.
    /// The checker does not flag errors involving `any`.
    Any,
    /// Supertype of all types — accepts any value, but nothing can be done
    /// with it without narrowing first.
    Top,
    /// Bottom type — subtype of all types. Represents unreachable code or
    /// empty collections.
    Never,

    // ── Composite ───────────────────────────────────────────────────────────
    /// Homogeneous list: `[T]`.
    List(Box<Type>),
    /// Tuple: `(A, B)` or the 1-tuple `(A,)`.
    Tuple(Vec<Type>),
    /// Ordered set of primitives.
    Set,
    /// Flat vector of primitives (O(1) indexed access).
    Vec,
    /// N-dimensional array of numbers (floats).
    Array,
    /// IO action producing a value of type `T`.
    IO(Box<Type>),
    /// Lens focusing on a `B` within an `A`.
    Lens(Box<Type>, Box<Type>),
    /// Traversal over zero or more `B`s within an `A`.
    Traversal(Box<Type>, Box<Type>),
    /// Record type.
    ///
    /// `open = true` means at least the listed fields are present (open record
    /// — the `{k: T, ..}` form). `open = false` means exactly those fields
    /// (closed record — the `{k: T}` form).
    Record {
        fields: BTreeMap<String, Type>,
        open: bool,
    },
    /// Function type: `A -> B`.
    Function(Box<Type>, Box<Type>),
    /// Union type: `A | B`.
    Union(Vec<Type>),

    // ── Variables ────────────────────────────────────────────────────────────
    /// Type variable (lowercase identifier, e.g. `a`, `b`, `result`).
    Var(TypeVarId),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Number => write!(f, "number"),
            Type::String => write!(f, "string"),
            Type::Symbol => write!(f, "symbol"),
            Type::Bool => write!(f, "bool"),
            Type::Null => write!(f, "null"),
            Type::DateTime => write!(f, "datetime"),
            Type::Any => write!(f, "any"),
            Type::Top => write!(f, "top"),
            Type::Never => write!(f, "never"),
            Type::Set => write!(f, "set"),
            Type::Vec => write!(f, "vec"),
            Type::Array => write!(f, "array"),
            Type::Var(v) => write!(f, "{v}"),
            Type::List(inner) => write!(f, "[{inner}]"),
            Type::Tuple(elems) => {
                write!(f, "(")?;
                match elems.as_slice() {
                    [] => write!(f, ")")?,
                    [single] => write!(f, "{single},)")?,
                    _ => {
                        let mut iter = elems.iter();
                        write!(f, "{}", iter.next().unwrap())?;
                        for t in iter {
                            write!(f, ", {t}")?;
                        }
                        write!(f, ")")?;
                    }
                }
                Ok(())
            }
            Type::IO(inner) => write!(f, "IO({inner})"),
            Type::Lens(a, b) => write!(f, "Lens({a}, {b})"),
            Type::Traversal(a, b) => write!(f, "Traversal({a}, {b})"),
            Type::Record { fields, open } => {
                write!(f, "{{")?;
                let mut iter = fields.iter();
                if let Some((k, v)) = iter.next() {
                    write!(f, "{k}: {v}")?;
                    for (k, v) in iter {
                        write!(f, ", {k}: {v}")?;
                    }
                }
                if *open {
                    if fields.is_empty() {
                        write!(f, "..")?;
                    } else {
                        write!(f, ", ..")?;
                    }
                }
                write!(f, "}}")
            }
            Type::Function(a, b) => {
                // Parenthesise LHS if it is itself a function or a union, to
                // avoid ambiguity: `(a -> b) -> c` and `(a | b) -> c` must
                // be distinct from the un-parenthesised forms which parse
                // differently. Parenthesise RHS if it is a union, because
                // `a -> b | c` parses as `(a -> b) | c`, not `a -> (b | c)`.
                let lhs_needs_parens = matches!(a.as_ref(), Type::Function(_, _) | Type::Union(_));
                let rhs_needs_parens = matches!(b.as_ref(), Type::Union(_));
                match (lhs_needs_parens, rhs_needs_parens) {
                    (true, true) => write!(f, "({a}) -> ({b})"),
                    (true, false) => write!(f, "({a}) -> {b}"),
                    (false, true) => write!(f, "{a} -> ({b})"),
                    (false, false) => write!(f, "{a} -> {b}"),
                }
            }
            Type::Union(variants) => {
                let mut iter = variants.iter();
                if let Some(first) = iter.next() {
                    write!(f, "{first}")?;
                    for t in iter {
                        write!(f, " | {t}")?;
                    }
                }
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(name: &str) -> Type {
        Type::Var(TypeVarId(name.to_string()))
    }

    #[test]
    fn display_primitives() {
        assert_eq!(Type::Number.to_string(), "number");
        assert_eq!(Type::String.to_string(), "string");
        assert_eq!(Type::Symbol.to_string(), "symbol");
        assert_eq!(Type::Bool.to_string(), "bool");
        assert_eq!(Type::Null.to_string(), "null");
        assert_eq!(Type::DateTime.to_string(), "datetime");
        assert_eq!(Type::Any.to_string(), "any");
        assert_eq!(Type::Top.to_string(), "top");
        assert_eq!(Type::Never.to_string(), "never");
        assert_eq!(Type::Set.to_string(), "set");
        assert_eq!(Type::Vec.to_string(), "vec");
        assert_eq!(Type::Array.to_string(), "array");
    }

    #[test]
    fn display_list() {
        let t = Type::List(Box::new(Type::Number));
        assert_eq!(t.to_string(), "[number]");
    }

    #[test]
    fn display_tuple_1() {
        let t = Type::Tuple(vec![Type::String]);
        assert_eq!(t.to_string(), "(string,)");
    }

    #[test]
    fn display_tuple_2() {
        let t = Type::Tuple(vec![Type::Symbol, Type::Any]);
        assert_eq!(t.to_string(), "(symbol, any)");
    }

    #[test]
    fn display_function() {
        let t = Type::Function(Box::new(var("a")), Box::new(var("b")));
        assert_eq!(t.to_string(), "a -> b");
    }

    #[test]
    fn display_curried_function() {
        let t = Type::Function(
            Box::new(Type::Number),
            Box::new(Type::Function(
                Box::new(Type::Number),
                Box::new(Type::Number),
            )),
        );
        assert_eq!(t.to_string(), "number -> number -> number");
    }

    #[test]
    fn display_io() {
        let t = Type::IO(Box::new(Type::String));
        assert_eq!(t.to_string(), "IO(string)");
    }

    #[test]
    fn display_lens() {
        let t = Type::Lens(
            Box::new(Type::Record {
                fields: BTreeMap::new(),
                open: true,
            }),
            Box::new(Type::Any),
        );
        assert_eq!(t.to_string(), "Lens({..}, any)");
    }

    #[test]
    fn display_closed_record() {
        let mut fields = BTreeMap::new();
        fields.insert("name".to_string(), Type::String);
        fields.insert("age".to_string(), Type::Number);
        let t = Type::Record {
            fields,
            open: false,
        };
        // BTreeMap is sorted alphabetically
        assert_eq!(t.to_string(), "{age: number, name: string}");
    }

    #[test]
    fn display_open_record() {
        let mut fields = BTreeMap::new();
        fields.insert("name".to_string(), Type::String);
        let t = Type::Record { fields, open: true };
        assert_eq!(t.to_string(), "{name: string, ..}");
    }

    #[test]
    fn display_union() {
        let t = Type::Union(vec![Type::Number, Type::String]);
        assert_eq!(t.to_string(), "number | string");
    }

    #[test]
    fn display_type_var() {
        assert_eq!(var("a").to_string(), "a");
        assert_eq!(var("result").to_string(), "result");
    }

    #[test]
    fn type_scheme_mono() {
        let s = TypeScheme::mono(Type::Number);
        assert_eq!(s.to_string(), "number");
    }

    #[test]
    fn type_scheme_poly() {
        let s = TypeScheme::poly(
            vec![TypeVarId("a".to_string()), TypeVarId("b".to_string())],
            Type::Function(
                Box::new(Type::Function(Box::new(var("a")), Box::new(var("b")))),
                Box::new(Type::Function(
                    Box::new(Type::List(Box::new(var("a")))),
                    Box::new(Type::List(Box::new(var("b")))),
                )),
            ),
        );
        assert_eq!(s.to_string(), "forall a b. (a -> b) -> [a] -> [b]");
    }
}
