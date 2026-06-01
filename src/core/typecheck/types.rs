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
    ///
    /// `rows` holds named row variables (the `..r` tail variables).  Each entry
    /// represents one row variable capturing extra fields.  Substituting a row
    /// variable with a concrete record type merges those fields in.
    ///
    /// - `rows: []` — no named row variables (anonymous open or closed).
    /// - `rows: [r]` — single named row: `{k: T, ..r}`.
    /// - `rows: [r, s]` — row concatenation: `{..r, ..s}` (as in `merge`'s
    ///   return type).  After substitution, fields from both `r` and `s` are
    ///   merged in.
    ///
    /// Non-empty `rows` implies `open = true`.
    Record {
        fields: BTreeMap<String, Type>,
        open: bool,
        /// Named row variables — the `..r` tail form.  Usually 0 or 1 entries;
        /// 2 entries represent row concatenation `{..r, ..s}`.
        rows: Vec<TypeVarId>,
    },
    /// Homogeneous block: arbitrary symbol keys, every value of type `T`.
    ///
    /// Distinct from `Record` (which names individual fields).  `Dict(T)`
    /// is the honest type of `map-values`, `group-by`, `values`, `keys`,
    /// and `lookup` — any block whose values all share a common type.
    ///
    /// Display: `Dict(T)`.
    Dict(Box<Type>),
    /// Function type: `A -> B`.
    Function(Box<Type>, Box<Type>),
    /// Union type: `A | B`.
    Union(Vec<Type>),

    // ── Literal types ────────────────────────────────────────────────────────
    /// Literal symbol type: a specific symbol value (e.g. `:active`).
    ///
    /// `LiteralSymbol(s)` is a subtype of `Symbol`.
    LiteralSymbol(String),

    // ── Variables ────────────────────────────────────────────────────────────
    /// Type variable (lowercase identifier, e.g. `a`, `b`, `result`).
    Var(TypeVarId),
}

impl Type {
    /// Construct a `Union` from an iterator, flattening singleton and empty cases.
    ///
    /// - Empty iterator → `Type::Never` (empty union = bottom type).
    /// - Single type → that type (no wrapper needed).
    /// - Multiple types → `Type::Union(types)`.
    pub fn union(types: impl IntoIterator<Item = Type>) -> Type {
        // Deduplicate while preserving order.
        let mut seen: Vec<Type> = Vec::new();
        for ty in types {
            if !seen.contains(&ty) {
                seen.push(ty);
            }
        }
        match seen.len() {
            0 => Type::Never,
            1 => seen.remove(0),
            _ => Type::Union(seen),
        }
    }
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
            Type::LiteralSymbol(name) => write!(f, ":{name}"),
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
            Type::Record { fields, open, rows } => {
                write!(f, "{{")?;
                let mut any_written = false;
                for (k, v) in fields.iter() {
                    if any_written {
                        write!(f, ", ")?;
                    }
                    write!(f, "{k}: {v}")?;
                    any_written = true;
                }
                // Print named row variables as `..r`, `..s`, …
                for r in rows {
                    if any_written {
                        write!(f, ", ")?;
                    }
                    write!(f, "..{r}")?;
                    any_written = true;
                }
                // Print anonymous open tail `..` if the record is anonymously open
                // (independently of any named row variables).
                if *open {
                    if any_written {
                        write!(f, ", ")?;
                    }
                    write!(f, "..")?;
                }
                write!(f, "}}")
            }
            Type::Dict(inner) => write!(f, "Dict({inner})"),
            Type::Function(a, b) => {
                // Parenthesise LHS if it is itself a function or a union, to
                // avoid ambiguity: `(a -> b) -> c` and `(a | b) -> c` must
                // be distinct from the un-parenthesised forms which parse
                // differently. Parenthesise RHS if it is a union, because
                // `a -> b | c` parses as `(a -> b) | c`, not `a -> (b | c)`.
                let lhs_needs_parens = matches!(a.as_ref(), Type::Function(_, _) | Type::Union(_));
                let rhs_needs_parens = matches!(b.as_ref(), Type::Union(_));
                match (lhs_needs_parens, rhs_needs_parens) {
                    (true, true) => write!(f, "({a}) → ({b})"),
                    (true, false) => write!(f, "({a}) → {b}"),
                    (false, true) => write!(f, "{a} → ({b})"),
                    (false, false) => write!(f, "{a} → {b}"),
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

/// Replace internal unification variables (`_t0`, `_t1`, etc.) with
/// user-friendly names (`a`, `b`, `c`, ...) for display in diagnostics.
///
/// Named type variables (e.g. from annotations) are left unchanged.
/// Only variables whose name starts with `_t` are replaced.
pub fn humanise(ty: &Type) -> Type {
    use std::collections::HashMap;

    fn collect_fresh_vars(ty: &Type, seen: &mut Vec<String>) {
        match ty {
            Type::Var(v) if v.0.starts_with("_t") && !seen.contains(&v.0) => {
                seen.push(v.0.clone());
            }
            Type::List(inner) | Type::IO(inner) | Type::Dict(inner) => {
                collect_fresh_vars(inner, seen);
            }
            Type::Tuple(elems) => {
                for e in elems {
                    collect_fresh_vars(e, seen);
                }
            }
            Type::Function(a, b) | Type::Lens(a, b) | Type::Traversal(a, b) => {
                collect_fresh_vars(a, seen);
                collect_fresh_vars(b, seen);
            }
            Type::Record { fields, rows, .. } => {
                for v in fields.values() {
                    collect_fresh_vars(v, seen);
                }
                // Row variables with `_t` prefix are fresh vars to be renamed.
                for r in rows {
                    if r.0.starts_with("_t") && !seen.contains(&r.0) {
                        seen.push(r.0.clone());
                    }
                }
            }
            Type::Union(variants) => {
                for v in variants {
                    collect_fresh_vars(v, seen);
                }
            }
            _ => {}
        }
    }

    fn replace(ty: &Type, mapping: &HashMap<String, String>) -> Type {
        match ty {
            Type::Var(v) => {
                if let Some(replacement) = mapping.get(&v.0) {
                    Type::Var(TypeVarId(replacement.clone()))
                } else {
                    ty.clone()
                }
            }
            Type::List(inner) => Type::List(Box::new(replace(inner, mapping))),
            Type::IO(inner) => Type::IO(Box::new(replace(inner, mapping))),
            Type::Dict(inner) => Type::Dict(Box::new(replace(inner, mapping))),
            Type::Tuple(elems) => Type::Tuple(elems.iter().map(|e| replace(e, mapping)).collect()),
            Type::Function(a, b) => {
                Type::Function(Box::new(replace(a, mapping)), Box::new(replace(b, mapping)))
            }
            Type::Lens(a, b) => {
                Type::Lens(Box::new(replace(a, mapping)), Box::new(replace(b, mapping)))
            }
            Type::Traversal(a, b) => {
                Type::Traversal(Box::new(replace(a, mapping)), Box::new(replace(b, mapping)))
            }
            Type::Record { fields, open, rows } => Type::Record {
                fields: fields
                    .iter()
                    .map(|(k, v)| (k.clone(), replace(v, mapping)))
                    .collect(),
                open: *open,
                rows: rows
                    .iter()
                    .map(|r| {
                        if let Some(replacement) = mapping.get(&r.0) {
                            TypeVarId(replacement.clone())
                        } else {
                            r.clone()
                        }
                    })
                    .collect(),
            },
            Type::Union(variants) => {
                Type::Union(variants.iter().map(|v| replace(v, mapping)).collect())
            }
            _ => ty.clone(),
        }
    }

    let mut fresh_vars = Vec::new();
    collect_fresh_vars(ty, &mut fresh_vars);

    if fresh_vars.is_empty() {
        return ty.clone();
    }

    let mut mapping = HashMap::new();
    for (i, var_name) in fresh_vars.iter().enumerate() {
        let letter = (b'a' + (i as u8 % 26)) as char;
        let name = if i < 26 {
            letter.to_string()
        } else {
            format!("{letter}{}", i / 26)
        };
        mapping.insert(var_name.clone(), name);
    }

    replace(ty, &mapping)
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
    fn display_literal_symbol() {
        assert_eq!(
            Type::LiteralSymbol("active".to_string()).to_string(),
            ":active"
        );
        assert_eq!(Type::LiteralSymbol("foo".to_string()).to_string(), ":foo");
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
        assert_eq!(t.to_string(), "a → b");
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
        assert_eq!(t.to_string(), "number → number → number");
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
                rows: vec![],
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
            rows: vec![],
        };
        // BTreeMap is sorted alphabetically
        assert_eq!(t.to_string(), "{age: number, name: string}");
    }

    #[test]
    fn display_open_record() {
        let mut fields = BTreeMap::new();
        fields.insert("name".to_string(), Type::String);
        let t = Type::Record {
            fields,
            open: true,
            rows: vec![],
        };
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
        assert_eq!(s.to_string(), "forall a b. (a → b) → [a] → [b]");
    }

    #[test]
    fn humanise_replaces_fresh_vars() {
        let ty = Type::Function(
            Box::new(var("_t0")),
            Box::new(Type::List(Box::new(var("_t1")))),
        );
        let h = humanise(&ty);
        assert_eq!(h.to_string(), "a → [b]");
    }

    #[test]
    fn humanise_preserves_named_vars() {
        let ty = Type::Function(Box::new(var("x")), Box::new(var("y")));
        let h = humanise(&ty);
        assert_eq!(h.to_string(), "x → y");
    }

    #[test]
    fn humanise_mixed_vars() {
        let ty = Type::Function(
            Box::new(var("_t5")),
            Box::new(Type::Function(Box::new(var("x")), Box::new(var("_t5")))),
        );
        let h = humanise(&ty);
        // _t5 becomes 'a', x stays 'x', second _t5 also becomes 'a'
        assert_eq!(h.to_string(), "a → x → a");
    }

    #[test]
    fn humanise_no_change_for_concrete() {
        let ty = Type::Function(Box::new(Type::Number), Box::new(Type::String));
        let h = humanise(&ty);
        assert_eq!(h.to_string(), "number → string");
    }

    // ── Dict display ────────────────────────────────────────────────────────

    #[test]
    fn display_dict_number() {
        let t = Type::Dict(Box::new(Type::Number));
        assert_eq!(t.to_string(), "Dict(number)");
    }

    #[test]
    fn display_dict_string() {
        let t = Type::Dict(Box::new(Type::String));
        assert_eq!(t.to_string(), "Dict(string)");
    }

    #[test]
    fn display_dict_list() {
        let t = Type::Dict(Box::new(Type::List(Box::new(Type::Number))));
        assert_eq!(t.to_string(), "Dict([number])");
    }

    // ── Row variable display ────────────────────────────────────────────────

    #[test]
    fn display_single_row_var() {
        // open: false — the named row var is the only extension
        let t = Type::Record {
            fields: BTreeMap::new(),
            open: false,
            rows: vec![TypeVarId("r".to_string())],
        };
        assert_eq!(t.to_string(), "{..r}");
    }

    #[test]
    fn display_row_var_with_fields() {
        let mut fields = BTreeMap::new();
        fields.insert("x".to_string(), Type::Number);
        // open: false — the named row var captures the extension
        let t = Type::Record {
            fields,
            open: false,
            rows: vec![TypeVarId("r".to_string())],
        };
        assert_eq!(t.to_string(), "{x: number, ..r}");
    }

    #[test]
    fn display_two_row_vars() {
        // open: false — both named row vars together capture the extension
        let t = Type::Record {
            fields: BTreeMap::new(),
            open: false,
            rows: vec![TypeVarId("r".to_string()), TypeVarId("s".to_string())],
        };
        assert_eq!(t.to_string(), "{..r, ..s}");
    }

    #[test]
    fn display_row_var_and_anon_open() {
        // open: true AND rows: [r] — row var plus extra anonymous tail
        let t = Type::Record {
            fields: BTreeMap::new(),
            open: true,
            rows: vec![TypeVarId("r".to_string())],
        };
        assert_eq!(t.to_string(), "{..r, ..}");
    }

    // ── Type::union constructor ─────────────────────────────────────────────

    #[test]
    fn union_empty_is_never() {
        assert_eq!(Type::union(std::iter::empty()), Type::Never);
    }

    #[test]
    fn union_singleton_unwraps() {
        assert_eq!(Type::union(std::iter::once(Type::Number)), Type::Number);
    }

    #[test]
    fn union_multiple_wraps() {
        let u = Type::union(vec![Type::Number, Type::String]);
        assert_eq!(u, Type::Union(vec![Type::Number, Type::String]));
    }

    // ── Humanise Dict ───────────────────────────────────────────────────────

    #[test]
    fn humanise_dict_with_fresh_var() {
        let ty = Type::Dict(Box::new(var("_t0")));
        let h = humanise(&ty);
        assert_eq!(h.to_string(), "Dict(a)");
    }
}
