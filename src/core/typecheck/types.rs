//! Type representation for eucalypt's gradual type system.
//!
//! This module defines the `Type` enum covering all type forms from the
//! gradual typing spec, plus `TypeScheme` for polymorphic types.
//!
//! ## Higher-kinded types (B1)
//!
//! Parametric type constructors (`List`, `IO`, `Dict`, …) are now represented
//! uniformly via `Con` + `App` rather than dedicated enum variants.  The smart
//! constructors (`Type::list`, `Type::io`, …) build the applied form; `Display`
//! re-sugars it back to the familiar notation.
//!
//! A `Kind` tracks the arity of each constructor.  Type variables carry a kind
//! (`Var(id, kind)`) so that higher-kinded variables (`m :: * → *`) can be
//! distinguished from ordinary type variables (`a :: *`).
//!
//! `Type::Forall` provides explicit, potentially rank-N quantification.

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

// ── Kind ─────────────────────────────────────────────────────────────────────

/// The kind of a type or type constructor.
///
/// `*` is the kind of ordinary types.
/// `k₁ → k₂` is the kind of a type constructor.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind {
    /// The kind of ordinary types (`*`).
    Star,
    /// The kind of a type constructor (`k₁ → k₂`).
    Arrow(Box<Kind>, Box<Kind>),
}

impl Kind {
    /// Convenience: `* → *`.
    pub fn star_to_star() -> Self {
        Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star))
    }

    /// Convenience: `* → * → *`.
    pub fn star_to_star_to_star() -> Self {
        Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::star_to_star()))
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Kind::Star => write!(f, "*"),
            Kind::Arrow(lhs, rhs) => {
                if matches!(lhs.as_ref(), Kind::Arrow(_, _)) {
                    write!(f, "({lhs}) → {rhs}")
                } else {
                    write!(f, "{lhs} → {rhs}")
                }
            }
        }
    }
}

// ── Kind table for built-in constructors ──────────────────────────────────────

/// Return the kind of a built-in named type constructor, or `None` for unknown.
pub fn constructor_kind(name: &str) -> Option<Kind> {
    match name {
        "List" | "IO" | "Dict" | "NonEmpty" => Some(Kind::star_to_star()),
        "Lens" | "Traversal" => Some(Kind::star_to_star_to_star()),
        _ => None,
    }
}

/// Compute the kind of a type expression.
pub fn kind_of(ty: &Type) -> Kind {
    match ty {
        Type::Number
        | Type::String
        | Type::Symbol
        | Type::Bool
        | Type::Null
        | Type::DateTime
        | Type::Any
        | Type::Top
        | Type::Never
        | Type::ExecutionError
        | Type::Set
        | Type::Vec
        | Type::Array
        | Type::LiteralSymbol(_)
        | Type::LiteralString(_)
        | Type::Record { .. }
        | Type::Function(_, _)
        | Type::Tuple(_)
        | Type::Union(_)
        | Type::Mu(_, _) => Kind::Star,

        Type::Con(name) => constructor_kind(name).unwrap_or(Kind::Star),

        // App(f, _): if f :: k1 → k2, result is k2.
        Type::App(f, _) => match kind_of(f) {
            Kind::Arrow(_, result) => *result,
            Kind::Star => Kind::Star,
        },

        Type::Var(_, k) => k.clone(),

        Type::Forall(_, _) => Kind::Star,
    }
}

// ── Constraint ───────────────────────────────────────────────────────────────

/// A constraint on a type variable (reserved for future use).
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

// ── Type ──────────────────────────────────────────────────────────────────────

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
    Any,
    /// Supertype of all types.
    Top,
    /// Bottom type — subtype of all types.
    Never,
    /// The type of a raised `ExecutionError` — produced by partial functions.
    ///
    /// `T?` is display sugar for `T | ExecutionError`.
    ExecutionError,

    // ── Opaque collection singletons (nullary, kind `*`) ────────────────────
    /// Ordered set of primitives.
    Set,
    /// Flat vector of primitives.
    Vec,
    /// N-dimensional array of numbers (floats).
    Array,

    // ── Constructor representation ───────────────────────────────────────────
    /// A named type constructor.
    ///
    /// Built-in constructors: `List`, `IO`, `Dict`, `NonEmpty`, `Lens`,
    /// `Traversal`.  User-defined constructors may also appear here.
    Con(String),

    /// Constructor application: `App(f, x)` — the type `f` applied to `x`.
    ///
    /// `List(a)` is `App(Con("List"), a)`.
    /// `Lens(a, b)` is `App(App(Con("Lens"), a), b)`.
    App(Box<Type>, Box<Type>),

    // ── Composite ───────────────────────────────────────────────────────────
    /// Tuple: `(A, B)` or the 1-tuple `(A,)`.
    Tuple(Vec<Type>),

    /// Record type (open or closed, with optional named row variables).
    Record {
        fields: BTreeMap<String, Type>,
        open: bool,
        /// Named row variables — the `..r` tail form.  Usually 0 or 1 entries.
        rows: Vec<TypeVarId>,
    },

    /// Function type: `A -> B`.
    Function(Box<Type>, Box<Type>),

    /// Union type: `A | B`.
    Union(Vec<Type>),

    // ── Literal types ────────────────────────────────────────────────────────
    /// Literal symbol type: a specific symbol value (e.g. `:active`).
    LiteralSymbol(String),
    /// Literal string type: a specific string value (e.g. `"read"`).
    LiteralString(String),

    // ── Variables ────────────────────────────────────────────────────────────
    /// Type variable with an explicit kind.
    ///
    /// Ordinary type variables have kind `Kind::Star`.
    /// Higher-kinded variables (e.g. `m :: * → *`) carry `Kind::Arrow`.
    Var(TypeVarId, Kind),

    // ── Recursive types ──────────────────────────────────────────────────────
    /// Equirecursive type: `Mu(x, body)` ≡ `body[x := Mu(x, body)]`.
    Mu(TypeVarId, Box<Type>),

    // ── Explicit quantification ───────────────────────────────────────────────
    /// Explicit, potentially rank-N quantification.
    ///
    /// `Forall([(m, * → *), (a, *)], m a → m a)` quantifies `m` at kind
    /// `* → *` and `a` at kind `*`.
    Forall(Vec<(TypeVarId, Kind)>, Box<Type>),
}

// ── Smart constructors ────────────────────────────────────────────────────────

impl Type {
    /// `[inner]` — homogeneous list.
    pub fn list(inner: Type) -> Self {
        Type::App(Box::new(Type::Con("List".into())), Box::new(inner))
    }

    /// `IO(inner)` — IO action producing `inner`.
    pub fn io(inner: Type) -> Self {
        Type::App(Box::new(Type::Con("IO".into())), Box::new(inner))
    }

    /// `Dict(inner)` — homogeneous block with values of type `inner`.
    pub fn dict(inner: Type) -> Self {
        Type::App(Box::new(Type::Con("Dict".into())), Box::new(inner))
    }

    /// `NonEmpty([inner])` — non-empty list.
    pub fn non_empty(inner: Type) -> Self {
        Type::App(Box::new(Type::Con("NonEmpty".into())), Box::new(inner))
    }

    /// `Lens(a, b)` — a lens focusing on `b` within `a`.
    pub fn lens(a: Type, b: Type) -> Self {
        Type::App(
            Box::new(Type::App(Box::new(Type::Con("Lens".into())), Box::new(a))),
            Box::new(b),
        )
    }

    /// `Traversal(a, b)` — a traversal over `b`s within `a`.
    pub fn traversal(a: Type, b: Type) -> Self {
        Type::App(
            Box::new(Type::App(
                Box::new(Type::Con("Traversal".into())),
                Box::new(a),
            )),
            Box::new(b),
        )
    }

    /// `T?` — `T | ExecutionError` (partial function return type).
    pub fn partial(inner: Type) -> Self {
        Type::Union(vec![inner, Type::ExecutionError])
    }

    /// Star-kinded type variable (the common case).
    pub fn var(id: TypeVarId) -> Self {
        Type::Var(id, Kind::Star)
    }

    /// Higher-kinded type variable.
    pub fn hk_var(id: TypeVarId, kind: Kind) -> Self {
        Type::Var(id, kind)
    }
}

// ── Decomposition helpers ─────────────────────────────────────────────────────

impl Type {
    /// If `self` is `App(Con(name), inner)`, return `(name, inner)`.
    pub fn as_applied_single(&self) -> Option<(&str, &Type)> {
        if let Type::App(f, inner) = self {
            if let Type::Con(name) = f.as_ref() {
                return Some((name.as_str(), inner.as_ref()));
            }
        }
        None
    }

    /// If `self` is `App(App(Con(name), a), b)`, return `(name, a, b)`.
    pub fn as_applied_two(&self) -> Option<(&str, &Type, &Type)> {
        if let Type::App(outer, b) = self {
            if let Type::App(f, a) = outer.as_ref() {
                if let Type::Con(name) = f.as_ref() {
                    return Some((name.as_str(), a.as_ref(), b.as_ref()));
                }
            }
        }
        None
    }

    /// If `self` is `App(Con("List"), inner)`, return `inner`.
    pub fn as_list(&self) -> Option<&Type> {
        self.as_applied_single()
            .filter(|(n, _)| *n == "List")
            .map(|(_, t)| t)
    }

    /// If `self` is `App(Con("IO"), inner)`, return `inner`.
    pub fn as_io(&self) -> Option<&Type> {
        self.as_applied_single()
            .filter(|(n, _)| *n == "IO")
            .map(|(_, t)| t)
    }

    /// If `self` is `App(Con("Dict"), inner)`, return `inner`.
    pub fn as_dict(&self) -> Option<&Type> {
        self.as_applied_single()
            .filter(|(n, _)| *n == "Dict")
            .map(|(_, t)| t)
    }

    /// If `self` is `App(Con("NonEmpty"), inner)`, return `inner`.
    pub fn as_non_empty(&self) -> Option<&Type> {
        self.as_applied_single()
            .filter(|(n, _)| *n == "NonEmpty")
            .map(|(_, t)| t)
    }

    /// If `self` is a single-argument built-in constructor application
    /// (List, IO, Dict, or NonEmpty), return the inner type.
    pub fn as_unary_con_inner(&self) -> Option<&Type> {
        self.as_applied_single()
            .filter(|(n, _)| matches!(*n, "List" | "IO" | "Dict" | "NonEmpty"))
            .map(|(_, t)| t)
    }
}

// ── union smart constructor ───────────────────────────────────────────────────

impl Type {
    /// Canonical union constructor implementing the §6.1 smart-constructor algorithm.
    ///
    /// Steps (in order):
    /// 1. Flatten nested unions.
    /// 2. Absorb `Any` — if any member is `Any`, return `Any`.
    /// 3. Drop `Never`.
    /// 4. Deduplicate.
    /// 5. Absorb literals into bases.
    /// 6. Normalise — empty → `Never`; singleton → unwrap; else `Union`.
    pub fn union(types: impl IntoIterator<Item = Type>) -> Type {
        let mut flat: Vec<Type> = Vec::new();
        for ty in types {
            match ty {
                Type::Union(vs) => flat.extend(vs),
                other => flat.push(other),
            }
        }

        if flat.iter().any(|t| matches!(t, Type::Any)) {
            return Type::Any;
        }

        flat.retain(|t| !matches!(t, Type::Never));

        let mut seen: Vec<Type> = Vec::new();
        for ty in flat {
            if !seen.contains(&ty) {
                seen.push(ty);
            }
        }

        let has_symbol = seen.iter().any(|t| matches!(t, Type::Symbol));
        let has_string = seen.iter().any(|t| matches!(t, Type::String));
        if has_symbol || has_string {
            seen.retain(|t| {
                !(has_symbol && matches!(t, Type::LiteralSymbol(_))
                    || has_string && matches!(t, Type::LiteralString(_)))
            });
        }

        match seen.len() {
            0 => Type::Never,
            1 => seen.remove(0),
            _ => Type::Union(seen),
        }
    }
}

// ── Display ───────────────────────────────────────────────────────────────────

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
            Type::ExecutionError => write!(f, "ExecutionError"),
            Type::Set => write!(f, "set"),
            Type::Vec => write!(f, "vec"),
            Type::Array => write!(f, "array"),
            Type::LiteralSymbol(name) => write!(f, ":{name}"),
            Type::LiteralString(s) => {
                let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
                write!(f, "\"{escaped}\"")
            }
            Type::Var(v, _) => write!(f, "{v}"),

            // Re-sugar known constructors; generic application by juxtaposition.
            Type::App(head, inner) => fmt_app(f, head, inner),

            // Bare constructor name.
            Type::Con(name) => write!(f, "{name}"),

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
                for r in rows {
                    if any_written {
                        write!(f, ", ")?;
                    }
                    write!(f, "..{r}")?;
                    any_written = true;
                }
                if *open {
                    if any_written {
                        write!(f, ", ")?;
                    }
                    write!(f, "..")?;
                }
                write!(f, "}}")
            }
            // Recursive type: print just the alias name — never unfold.
            Type::Mu(x, _) => write!(f, "{x}"),
            Type::Function(a, b) => {
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
                // Re-sugar `T | ExecutionError` or `ExecutionError | T` → `T?`.
                if variants.len() == 2 {
                    if variants[1] == Type::ExecutionError {
                        return write!(f, "{}?", variants[0]);
                    }
                    if variants[0] == Type::ExecutionError {
                        return write!(f, "{}?", variants[1]);
                    }
                }
                let mut iter = variants.iter();
                if let Some(first) = iter.next() {
                    write!(f, "{first}")?;
                    for t in iter {
                        write!(f, " | {t}")?;
                    }
                }
                Ok(())
            }
            Type::Forall(binders, body) => {
                write!(f, "forall")?;
                for (id, kind) in binders {
                    if *kind == Kind::Star {
                        write!(f, " {id}")?;
                    } else {
                        write!(f, " ({id} :: {kind})")?;
                    }
                }
                write!(f, ". {body}")
            }
        }
    }
}

/// Format a constructor application, re-sugaring known constructors.
fn fmt_app(f: &mut fmt::Formatter<'_>, head: &Type, arg: &Type) -> fmt::Result {
    // Two-arg constructors: `App(App(Con("Lens"), a), b)` → `Lens(a, b)`
    if let Type::App(inner_head, a) = head {
        if let Type::Con(name) = inner_head.as_ref() {
            match name.as_str() {
                "Lens" => return write!(f, "Lens({a}, {arg})"),
                "Traversal" => return write!(f, "Traversal({a}, {arg})"),
                _ => {}
            }
        }
    }
    // Single-arg constructors.
    if let Type::Con(name) = head {
        match name.as_str() {
            "List" => return write!(f, "[{arg}]"),
            "IO" => return write!(f, "IO({arg})"),
            "Dict" => return write!(f, "Dict({arg})"),
            "NonEmpty" => return write!(f, "NonEmpty([{arg}])"),
            "Random" => return write!(f, "Random({arg})"),
            "State" => return write!(f, "State({arg})"),
            _ => {}
        }
    }
    // Generic application by juxtaposition.
    write!(f, "{head} {arg}")
}

// ── humanise ─────────────────────────────────────────────────────────────────

/// Replace internal unification variables (`_t0`, `_t1`, etc.) with
/// user-friendly names (`a`, `b`, `c`, ...) for display in diagnostics.
///
/// Named type variables (e.g. from annotations) are left unchanged.
/// Only variables whose name starts with `_t` are replaced.
pub fn humanise(ty: &Type) -> Type {
    use std::collections::HashMap;

    fn collect_fresh_vars(ty: &Type, seen: &mut Vec<String>) {
        match ty {
            Type::Var(v, _) if v.0.starts_with("_t") && !seen.contains(&v.0) => {
                seen.push(v.0.clone());
            }
            Type::App(f, inner) => {
                collect_fresh_vars(f, seen);
                collect_fresh_vars(inner, seen);
            }
            Type::Con(_) => {}
            Type::Tuple(elems) => {
                for e in elems {
                    collect_fresh_vars(e, seen);
                }
            }
            Type::Function(a, b) => {
                collect_fresh_vars(a, seen);
                collect_fresh_vars(b, seen);
            }
            Type::Record { fields, rows, .. } => {
                for v in fields.values() {
                    collect_fresh_vars(v, seen);
                }
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
            Type::Mu(_, body) => {
                collect_fresh_vars(body, seen);
            }
            Type::Forall(_, body) => {
                collect_fresh_vars(body, seen);
            }
            _ => {}
        }
    }

    fn replace(ty: &Type, mapping: &HashMap<String, String>) -> Type {
        match ty {
            Type::Var(v, kind) => {
                if let Some(replacement) = mapping.get(&v.0) {
                    Type::Var(TypeVarId(replacement.clone()), kind.clone())
                } else {
                    ty.clone()
                }
            }
            Type::App(f, inner) => Type::App(
                Box::new(replace(f, mapping)),
                Box::new(replace(inner, mapping)),
            ),
            Type::Con(_) => ty.clone(),
            Type::Tuple(elems) => Type::Tuple(elems.iter().map(|e| replace(e, mapping)).collect()),
            Type::Function(a, b) => {
                Type::Function(Box::new(replace(a, mapping)), Box::new(replace(b, mapping)))
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
            Type::Mu(x, body) => Type::Mu(x.clone(), Box::new(replace(body, mapping))),
            Type::Forall(binders, body) => {
                Type::Forall(binders.clone(), Box::new(replace(body, mapping)))
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

// ── unfold_mu ─────────────────────────────────────────────────────────────────

/// One-step unfolding of a `Mu` type.
///
/// Substitutes `replacement` for every free occurrence of `x` in `ty`.
pub fn unfold_mu(x: &TypeVarId, ty: &Type, replacement: &Type) -> Type {
    match ty {
        Type::Var(id, _) if id == x => replacement.clone(),
        // Binder shadowing.
        Type::Mu(y, _) if y == x => ty.clone(),
        Type::Mu(y, body) => Type::Mu(y.clone(), Box::new(unfold_mu(x, body, replacement))),
        Type::App(f, inner) => Type::App(
            Box::new(unfold_mu(x, f, replacement)),
            Box::new(unfold_mu(x, inner, replacement)),
        ),
        Type::Con(_) => ty.clone(),
        Type::Tuple(elems) => {
            Type::Tuple(elems.iter().map(|e| unfold_mu(x, e, replacement)).collect())
        }
        Type::Function(a, b) => Type::Function(
            Box::new(unfold_mu(x, a, replacement)),
            Box::new(unfold_mu(x, b, replacement)),
        ),
        Type::Record { fields, open, rows } => Type::Record {
            fields: fields
                .iter()
                .map(|(k, v)| (k.clone(), unfold_mu(x, v, replacement)))
                .collect(),
            open: *open,
            rows: rows.clone(),
        },
        Type::Union(variants) => Type::Union(
            variants
                .iter()
                .map(|v| unfold_mu(x, v, replacement))
                .collect(),
        ),
        Type::Forall(binders, body) => {
            Type::Forall(binders.clone(), Box::new(unfold_mu(x, body, replacement)))
        }
        other => other.clone(),
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn var(name: &str) -> Type {
        Type::var(TypeVarId(name.to_string()))
    }

    fn hk_var(name: &str) -> Type {
        Type::hk_var(TypeVarId(name.to_string()), Kind::star_to_star())
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
    }

    #[test]
    fn display_list() {
        assert_eq!(Type::list(Type::Number).to_string(), "[number]");
    }

    #[test]
    fn display_io() {
        assert_eq!(Type::io(Type::String).to_string(), "IO(string)");
    }

    #[test]
    fn display_dict() {
        assert_eq!(Type::dict(Type::Number).to_string(), "Dict(number)");
    }

    #[test]
    fn display_non_empty() {
        assert_eq!(
            Type::non_empty(Type::Number).to_string(),
            "NonEmpty([number])"
        );
    }

    #[test]
    fn display_lens() {
        let t = Type::lens(
            Type::Record {
                fields: BTreeMap::new(),
                open: true,
                rows: vec![],
            },
            Type::Any,
        );
        assert_eq!(t.to_string(), "Lens({..}, any)");
    }

    #[test]
    fn display_traversal() {
        let t = Type::traversal(Type::list(var("a")), var("a"));
        assert_eq!(t.to_string(), "Traversal([a], a)");
    }

    #[test]
    fn display_generic_app() {
        let t = Type::App(Box::new(hk_var("m")), Box::new(var("a")));
        assert_eq!(t.to_string(), "m a");
    }

    #[test]
    fn display_forall_star() {
        let t = Type::Forall(
            vec![
                (TypeVarId("a".into()), Kind::Star),
                (TypeVarId("b".into()), Kind::Star),
            ],
            Box::new(Type::Function(Box::new(var("a")), Box::new(var("b")))),
        );
        assert_eq!(t.to_string(), "forall a b. a → b");
    }

    #[test]
    fn display_forall_hk() {
        let m = TypeVarId("m".into());
        let a = TypeVarId("a".into());
        let m_a = Type::App(
            Box::new(Type::Var(m.clone(), Kind::star_to_star())),
            Box::new(Type::Var(a.clone(), Kind::Star)),
        );
        let t = Type::Forall(
            vec![(m, Kind::star_to_star()), (a, Kind::Star)],
            Box::new(Type::Function(Box::new(m_a.clone()), Box::new(m_a))),
        );
        assert_eq!(t.to_string(), "forall (m :: * → *) a. m a → m a");
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
    fn display_closed_record() {
        let mut fields = BTreeMap::new();
        fields.insert("name".to_string(), Type::String);
        fields.insert("age".to_string(), Type::Number);
        let t = Type::Record {
            fields,
            open: false,
            rows: vec![],
        };
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
                    Box::new(Type::list(var("a"))),
                    Box::new(Type::list(var("b"))),
                )),
            ),
        );
        assert_eq!(s.to_string(), "forall a b. (a → b) → [a] → [b]");
    }

    #[test]
    fn humanise_replaces_fresh_vars() {
        let ty = Type::Function(
            Box::new(Type::Var(TypeVarId("_t0".into()), Kind::Star)),
            Box::new(Type::list(Type::Var(TypeVarId("_t1".into()), Kind::Star))),
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
            Box::new(Type::Var(TypeVarId("_t5".into()), Kind::Star)),
            Box::new(Type::Function(
                Box::new(var("x")),
                Box::new(Type::Var(TypeVarId("_t5".into()), Kind::Star)),
            )),
        );
        let h = humanise(&ty);
        assert_eq!(h.to_string(), "a → x → a");
    }

    #[test]
    fn humanise_no_change_for_concrete() {
        let ty = Type::Function(Box::new(Type::Number), Box::new(Type::String));
        let h = humanise(&ty);
        assert_eq!(h.to_string(), "number → string");
    }

    #[test]
    fn display_dict_number() {
        assert_eq!(Type::dict(Type::Number).to_string(), "Dict(number)");
    }

    #[test]
    fn display_dict_list() {
        assert_eq!(
            Type::dict(Type::list(Type::Number)).to_string(),
            "Dict([number])"
        );
    }

    #[test]
    fn display_single_row_var() {
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
        let t = Type::Record {
            fields,
            open: false,
            rows: vec![TypeVarId("r".to_string())],
        };
        assert_eq!(t.to_string(), "{x: number, ..r}");
    }

    #[test]
    fn display_two_row_vars() {
        let t = Type::Record {
            fields: BTreeMap::new(),
            open: false,
            rows: vec![TypeVarId("r".to_string()), TypeVarId("s".to_string())],
        };
        assert_eq!(t.to_string(), "{..r, ..s}");
    }

    #[test]
    fn display_row_var_and_anon_open() {
        let t = Type::Record {
            fields: BTreeMap::new(),
            open: true,
            rows: vec![TypeVarId("r".to_string())],
        };
        assert_eq!(t.to_string(), "{..r, ..}");
    }

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

    #[test]
    fn humanise_dict_with_fresh_var() {
        let ty = Type::dict(Type::Var(TypeVarId("_t0".into()), Kind::Star));
        let h = humanise(&ty);
        assert_eq!(h.to_string(), "Dict(a)");
    }

    #[test]
    fn display_mu_shows_binder_name() {
        let body = Type::Union(vec![
            Type::Number,
            Type::Var(TypeVarId("Json".to_string()), Kind::Star),
        ]);
        let mu = Type::Mu(TypeVarId("Json".to_string()), Box::new(body));
        assert_eq!(mu.to_string(), "Json");
    }

    #[test]
    fn display_mu_in_function_type() {
        let body = Type::Union(vec![
            Type::Number,
            Type::Var(TypeVarId("Json".to_string()), Kind::Star),
        ]);
        let mu = Type::Mu(TypeVarId("Json".to_string()), Box::new(body));
        let func = Type::Function(Box::new(Type::String), Box::new(mu));
        assert_eq!(func.to_string(), "string → Json");
    }

    #[test]
    fn humanise_mu_body_renames_fresh_vars() {
        let tree_id = TypeVarId("Tree".to_string());
        let body = Type::Record {
            fields: {
                let mut m = BTreeMap::new();
                m.insert(
                    "value".to_string(),
                    Type::Var(TypeVarId("_t0".into()), Kind::Star),
                );
                m.insert("left".to_string(), Type::Var(tree_id.clone(), Kind::Star));
                m
            },
            open: false,
            rows: vec![],
        };
        let mu = Type::Mu(tree_id, Box::new(body));
        let h = humanise(&mu);
        assert_eq!(h.to_string(), "Tree");
    }

    // ── Kind tests ────────────────────────────────────────────────────────────

    #[test]
    fn kind_of_primitives_is_star() {
        assert_eq!(kind_of(&Type::Number), Kind::Star);
        assert_eq!(kind_of(&Type::String), Kind::Star);
        assert_eq!(kind_of(&Type::Any), Kind::Star);
    }

    #[test]
    fn kind_of_con_list_is_star_to_star() {
        assert_eq!(kind_of(&Type::Con("List".into())), Kind::star_to_star());
        assert_eq!(kind_of(&Type::Con("IO".into())), Kind::star_to_star());
        assert_eq!(kind_of(&Type::Con("Dict".into())), Kind::star_to_star());
    }

    #[test]
    fn kind_of_applied_list_is_star() {
        assert_eq!(kind_of(&Type::list(Type::Number)), Kind::Star);
    }

    #[test]
    fn kind_of_var_star() {
        assert_eq!(kind_of(&var("a")), Kind::Star);
    }

    #[test]
    fn kind_of_hk_var() {
        assert_eq!(kind_of(&hk_var("m")), Kind::star_to_star());
    }

    #[test]
    fn kind_of_partially_applied_lens() {
        // App(Con("Lens"), a) :: * → *  (Lens is * → * → *, applied to one arg)
        let partial = Type::App(Box::new(Type::Con("Lens".into())), Box::new(Type::Any));
        assert_eq!(kind_of(&partial), Kind::star_to_star());
    }

    #[test]
    fn smart_constructors_produce_applied_form() {
        assert_eq!(
            Type::list(var("a")),
            Type::App(Box::new(Type::Con("List".into())), Box::new(var("a")))
        );
        assert_eq!(
            Type::lens(var("a"), var("b")),
            Type::App(
                Box::new(Type::App(
                    Box::new(Type::Con("Lens".into())),
                    Box::new(var("a"))
                )),
                Box::new(var("b"))
            )
        );
    }

    #[test]
    fn decomposition_helpers() {
        let list_n = Type::list(Type::Number);
        assert_eq!(list_n.as_list(), Some(&Type::Number));
        assert!(list_n.as_io().is_none());
        assert_eq!(list_n.as_applied_single(), Some(("List", &Type::Number)));

        let lens_ab = Type::lens(var("a"), var("b"));
        assert_eq!(
            lens_ab.as_applied_two(),
            Some(("Lens", &var("a"), &var("b")))
        );
    }

    #[test]
    fn kind_display() {
        assert_eq!(Kind::Star.to_string(), "*");
        assert_eq!(Kind::star_to_star().to_string(), "* → *");
        assert_eq!(Kind::star_to_star_to_star().to_string(), "* → * → *");
        let k = Kind::Arrow(Box::new(Kind::star_to_star()), Box::new(Kind::Star));
        assert_eq!(k.to_string(), "(* → *) → *");
    }
}
