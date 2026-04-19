//! Bidirectional type checker for eucalypt core expressions.
//!
//! Implements two modes:
//!
//! - **Synthesis** (⇒): compute the type of an expression from its structure.
//! - **Checking** (⇐): verify that an expression is consistent with an expected type.
//!
//! This is the first phase of the bidirectional checker (eu-mptm). Type
//! variable instantiation is deferred to eu-wq59; for now, type variables
//! (`a`, `b`, etc.) are erased to `any` before checking.
//!
//! Type issues are always warnings — they never prevent evaluation.

use std::collections::{BTreeMap, HashMap, VecDeque};

use crate::{
    common::sourcemap::{HasSmid, Smid},
    core::{
        binding::Var,
        expr::{BlockMap, Expr, Primitive, RcExpr},
        typecheck::{error::TypeWarning, parse, subtype::is_consistent, types::Type},
    },
};

// ── Alias table ──────────────────────────────────────────────────────────────

/// A flat map from alias name to its concrete `Type`.
///
/// Alias names are capitalised identifiers (`Person`, `Point`, …). They are
/// accumulated during a checker walk from two sources:
///
/// 1. `type-def:` metadata on a declaration — the declaration's type (from its
///    `type:` annotation, or synthesised) is registered under the given name.
/// 2. `types:` key inside a metadata block — each entry in the nested block is
///    parsed as a type and registered.
///
/// `Type::Var` nodes whose name is in this map are replaced with the stored
/// concrete type before type variables are erased to `any`.
type AliasMap = HashMap<String, Type>;

// ── Checker ─────────────────────────────────────────────────────────────────

/// Bidirectional type checker for core expressions.
///
/// Walk a `RcExpr` with `check_expr()` to collect all `TypeWarning`s.
/// The checker is stateful — it builds up a scope stack as it descends
/// into let-bindings and lambdas.
pub struct Checker {
    /// Scope stack.  Front = innermost scope.
    ///
    /// Each frame maps binding name → type for that scope.  When we see a
    /// `Var(_, Bound(bv))` and `bv.name` is `Some(n)`, we search from the
    /// innermost frame outwards for `n`.
    scope_stack: VecDeque<HashMap<String, Type>>,

    /// Accumulated warnings.
    warnings: Vec<TypeWarning>,

    /// Type alias map: capitalised names → concrete `Type`.
    ///
    /// Populated from `type-def:` metadata and `types:` blocks as the
    /// checker walks the expression tree.  Used in `extract_annotation` to
    /// resolve alias references before erasing type variables.
    aliases: AliasMap,
}

impl Default for Checker {
    fn default() -> Self {
        Checker::new()
    }
}

impl Checker {
    /// Create a new, empty checker.
    pub fn new() -> Self {
        Checker {
            scope_stack: VecDeque::new(),
            warnings: Vec::new(),
            aliases: AliasMap::new(),
        }
    }

    /// Consume the checker and return all accumulated warnings.
    pub fn into_warnings(self) -> Vec<TypeWarning> {
        self.warnings
    }

    /// Primary entry point: walk `expr` and collect warnings.
    ///
    /// Equivalent to synthesising the type of the whole expression tree and
    /// recursing into all sub-expressions along the way.
    pub fn check_expr(&mut self, expr: &RcExpr) {
        self.synthesise(expr);
    }

    // ── Scope management ────────────────────────────────────────────────────

    fn push_scope(&mut self, frame: HashMap<String, Type>) {
        self.scope_stack.push_front(frame);
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop_front();
    }

    /// Look up a name by searching from the innermost scope outwards.
    fn lookup_name(&self, name: &str) -> Type {
        for frame in &self.scope_stack {
            if let Some(ty) = frame.get(name) {
                return ty.clone();
            }
        }
        Type::Any
    }

    // ── Alias management ────────────────────────────────────────────────────

    /// Register a type alias.
    fn register_alias(&mut self, name: String, ty: Type) {
        self.aliases.insert(name, ty);
    }

    /// Walk `ty`, replacing every `Type::Var(name)` that appears in the alias
    /// map with its registered concrete type.
    ///
    /// Lowercase type-variable names (e.g. `a`, `b`) are left untouched if they
    /// are not in the alias map, so `erase_type_vars` still handles them.
    fn resolve_aliases_in_type(&self, ty: Type) -> Type {
        match ty {
            Type::Var(ref v) => {
                if let Some(alias_ty) = self.aliases.get(&v.0) {
                    alias_ty.clone()
                } else {
                    ty
                }
            }
            Type::List(inner) => Type::List(Box::new(self.resolve_aliases_in_type(*inner))),
            Type::Tuple(elems) => Type::Tuple(
                elems
                    .into_iter()
                    .map(|e| self.resolve_aliases_in_type(e))
                    .collect(),
            ),
            Type::IO(inner) => Type::IO(Box::new(self.resolve_aliases_in_type(*inner))),
            Type::Lens(a, b) => Type::Lens(
                Box::new(self.resolve_aliases_in_type(*a)),
                Box::new(self.resolve_aliases_in_type(*b)),
            ),
            Type::Traversal(a, b) => Type::Traversal(
                Box::new(self.resolve_aliases_in_type(*a)),
                Box::new(self.resolve_aliases_in_type(*b)),
            ),
            Type::Function(a, b) => Type::Function(
                Box::new(self.resolve_aliases_in_type(*a)),
                Box::new(self.resolve_aliases_in_type(*b)),
            ),
            Type::Record { fields, open } => Type::Record {
                fields: fields
                    .into_iter()
                    .map(|(k, v)| (k, self.resolve_aliases_in_type(v)))
                    .collect(),
                open,
            },
            Type::Union(variants) => Type::Union(
                variants
                    .into_iter()
                    .map(|v| self.resolve_aliases_in_type(v))
                    .collect(),
            ),
            other => other,
        }
    }

    /// Register aliases from a `types:` sub-block in a metadata expression.
    ///
    /// Looks for a `types: { Name: "type string", … }` entry in `meta`.
    /// Each key becomes an alias name; each value is parsed as a type string
    /// and registered.  Entries with unparseable type strings are silently
    /// skipped.
    fn register_aliases_from_meta(&mut self, meta: &RcExpr) {
        let meta_block = match &*meta.inner {
            Expr::Block(_, b) => b,
            _ => return,
        };

        let types_expr = match meta_block.get("types") {
            Some(e) => e,
            None => return,
        };

        let type_entries = match &*types_expr.inner {
            Expr::Block(_, b) => b,
            _ => return,
        };

        for (alias_name, type_str_expr) in type_entries.iter() {
            if let Some(type_str) = extract_string_literal(type_str_expr) {
                if let Ok(ty) = parse::parse_type(&type_str) {
                    self.register_alias(alias_name.clone(), ty);
                }
            }
        }
    }

    /// Extract the `type-def:` alias name from a binding value, if present.
    ///
    /// Returns `Some("Name")` when the value is a `Meta` node whose metadata
    /// block contains `type-def: "Name"`.
    fn extract_type_def_name(value: &RcExpr) -> Option<String> {
        let meta = match &*value.inner {
            Expr::Meta(_, _, m) => m,
            _ => return None,
        };
        let block = match &*meta.inner {
            Expr::Block(_, b) => b,
            _ => return None,
        };
        block.get("type-def").and_then(extract_string_literal)
    }

    // ── Type annotation extraction ───────────────────────────────────────────

    /// Try to extract a `Type` from a metadata block expression.
    ///
    /// Looks for the `type:` key (user annotation) and the `__type_hint:` key
    /// (desugarer hint).  User annotations take priority.
    ///
    /// After parsing, alias references (`Type::Var` with a capitalised name)
    /// are resolved against the checker's alias map.
    fn extract_annotation(&self, meta: &RcExpr) -> Option<Type> {
        let block = match &*meta.inner {
            Expr::Block(_, b) => b,
            _ => return None,
        };

        // User annotation takes priority over desugarer hint.
        let type_str: String = if let Some(e) = block.get("type") {
            extract_string_literal(e)?
        } else if let Some(e) = block.get("__type_hint") {
            extract_string_literal(e)?
        } else {
            return None;
        };

        let parsed = parse::parse_type(&type_str).ok()?;
        Some(self.resolve_aliases_in_type(parsed))
    }

    // ── Type variable erasure ────────────────────────────────────────────────

    /// Replace all type variables with `any` (Phase 1 — no instantiation yet).
    fn erase_type_vars(ty: Type) -> Type {
        match ty {
            Type::Var(_) => Type::Any,
            Type::List(inner) => Type::List(Box::new(Self::erase_type_vars(*inner))),
            Type::Tuple(elems) => {
                Type::Tuple(elems.into_iter().map(Self::erase_type_vars).collect())
            }
            Type::IO(inner) => Type::IO(Box::new(Self::erase_type_vars(*inner))),
            Type::Lens(a, b) => Type::Lens(
                Box::new(Self::erase_type_vars(*a)),
                Box::new(Self::erase_type_vars(*b)),
            ),
            Type::Traversal(a, b) => Type::Traversal(
                Box::new(Self::erase_type_vars(*a)),
                Box::new(Self::erase_type_vars(*b)),
            ),
            Type::Function(a, b) => Type::Function(
                Box::new(Self::erase_type_vars(*a)),
                Box::new(Self::erase_type_vars(*b)),
            ),
            Type::Record { fields, open } => Type::Record {
                fields: fields
                    .into_iter()
                    .map(|(k, v)| (k, Self::erase_type_vars(v)))
                    .collect(),
                open,
            },
            Type::Union(variants) => {
                Type::Union(variants.into_iter().map(Self::erase_type_vars).collect())
            }
            other => other,
        }
    }

    // ── Synthesis ────────────────────────────────────────────────────────────

    /// Synthesise the type of `expr`, recursing to gather warnings along the
    /// way.
    pub fn synthesise(&mut self, expr: &RcExpr) -> Type {
        match &*expr.inner {
            // ── Literals ─────────────────────────────────────────────────────
            Expr::Literal(_, prim) => synthesise_primitive(prim),

            // ── List ─────────────────────────────────────────────────────────
            Expr::List(_, items) => {
                let elem_types: Vec<Type> = items.iter().map(|e| self.synthesise(e)).collect();
                synthesise_list_type(elem_types)
            }

            // ── Block ─────────────────────────────────────────────────────────
            Expr::Block(_, fields) => self.synthesise_block(fields),

            // ── Variables ────────────────────────────────────────────────────
            Expr::Var(_, Var::Free(name)) => self.lookup_name(name),
            Expr::Var(_, Var::Bound(bv)) => bv
                .name
                .as_deref()
                .map(|n| self.lookup_name(n))
                .unwrap_or(Type::Any),

            // ── Name (pre-varify) ─────────────────────────────────────────────
            Expr::Name(_, name) => self.lookup_name(name),

            // ── Intrinsic ─────────────────────────────────────────────────────
            Expr::Intrinsic(_, name) => self.lookup_name(name),

            // ── Metadata ─────────────────────────────────────────────────────
            //
            // A Meta node carries type annotations.  When a `type:` or
            // `__type_hint:` annotation is present, it is the authoritative
            // type for the wrapped expression — we synthesise the inner type
            // purely to check consistency and to populate the scope for any
            // nested let-bindings.
            Expr::Meta(smid, inner, meta) => self.synthesise_meta(*smid, inner, meta),

            // ── Let ───────────────────────────────────────────────────────────
            Expr::Let(_, scope, _) => {
                // Two-pass approach so that annotated bindings are visible when
                // synthesising the values of their sibling bindings.
                //
                // Pass 1: extract annotation types for all bindings.  For
                // unannotated bindings, seed with `any` as a placeholder.
                let mut frame: HashMap<String, Type> = HashMap::new();
                for (name, value) in &scope.pattern {
                    let ty = self.annotation_type_of(value).unwrap_or(Type::Any);
                    frame.insert(name.clone(), ty);
                }

                // Push the pre-seeded frame so sibling bindings are in scope.
                self.push_scope(frame);

                // Pass 2: synthesise each binding value — this triggers
                // consistency checks against any annotations present.
                for (name, value) in &scope.pattern {
                    let synthesised = self.synthesise_binding_value(value);
                    // For unannotated bindings, replace the `any` placeholder
                    // with the synthesised type so later bindings can use it.
                    if let Some(frame) = self.scope_stack.front_mut() {
                        if frame.get(name) == Some(&Type::Any) {
                            frame.insert(name.clone(), synthesised.clone());
                        }
                    }
                    // Register `type-def:` alias when present.
                    if let Some(alias_name) = Self::extract_type_def_name(value) {
                        // Use the explicit `type:` annotation if given; otherwise
                        // the synthesised type (inferred from the value shape).
                        let alias_ty = self.annotation_type_of(value).unwrap_or(synthesised);
                        self.register_alias(alias_name, alias_ty);
                    }
                }

                let body_type = self.synthesise(&scope.body);
                self.pop_scope();
                body_type
            }

            // ── Lambda ────────────────────────────────────────────────────────
            //
            // We cannot synthesise a lambda's type without knowing the
            // parameter types.  Return `any` here; the checking direction
            // (called from `check_against`) handles the case where the
            // expected type is known.
            Expr::Lam(_, _, _) => Type::Any,

            // ── Application ──────────────────────────────────────────────────
            Expr::App(smid, func, args) => self.synthesise_app(*smid, func, args),

            // ── Lookup ───────────────────────────────────────────────────────
            //
            // `.field` on an expression with a known record type resolves to
            // the field's type.  Unknown fields on a closed record emit a
            // warning; unknown fields on an open record return `any`
            // (the record might have more fields at runtime).
            Expr::Lookup(smid, obj, field, fallback) => {
                let obj_type = self.synthesise(obj);
                if let Some(f) = fallback {
                    self.synthesise(f);
                }
                self.synthesise_lookup(*smid, &obj_type, field)
            }

            // ── Everything else ───────────────────────────────────────────────
            _ => Type::Any,
        }
    }

    /// Extract the annotated type from a binding value without synthesising it.
    ///
    /// Returns `Some(T)` when the value is wrapped in a `Meta` node that carries
    /// a `type:` or `__type_hint:` annotation, `None` otherwise.  The type has
    /// alias references resolved and type variables erased.
    fn annotation_type_of(&self, value: &RcExpr) -> Option<Type> {
        if let Expr::Meta(_, _, meta) = &*value.inner {
            self.extract_annotation(meta).map(Self::erase_type_vars)
        } else {
            None
        }
    }

    /// Synthesise the type of a let-binding value, giving priority to any
    /// type annotation present in a `Meta` wrapper.
    ///
    /// This is a thin wrapper around `synthesise_meta` that unwraps the outer
    /// `Meta` layer when present, so that `synthesise_meta` can handle alias
    /// registration, annotation checking, and synthesis in one place.
    fn synthesise_binding_value(&mut self, value: &RcExpr) -> Type {
        if let Expr::Meta(smid, inner, meta) = &*value.inner {
            return self.synthesise_meta(*smid, inner, meta);
        }
        self.synthesise(value)
    }

    /// Synthesise the type from a `Meta(smid, inner, meta)` node.
    ///
    /// Steps:
    /// 1. Register any `types:` aliases declared in `meta` so they are
    ///    available for annotations later in the same scope.
    /// 2. If `meta` carries a `type:` or `__type_hint:` annotation, check the
    ///    inner expression against it and return the annotated type
    ///    (authoritative).
    /// 3. Otherwise synthesise the inner expression.
    fn synthesise_meta(&mut self, smid: Smid, inner: &RcExpr, meta: &RcExpr) -> Type {
        // Register `types:` block aliases before reading any annotation so that
        // the annotation itself can reference freshly-declared aliases.
        self.register_aliases_from_meta(meta);

        if let Some(annotated_type) = self.extract_annotation(meta) {
            // The annotation is authoritative.  Check the inner expression
            // against it to emit warnings for obvious mismatches.
            let erased = Self::erase_type_vars(annotated_type.clone());
            self.check_against(inner, &erased, smid);
            erased
        } else {
            self.synthesise(inner)
        }
    }

    /// Synthesise a record type from a block's fields.
    ///
    /// Only fields with known (non-`any`, non-`never`) types are included in
    /// the synthesised record — unannotated fields synthesise as `any` and
    /// don't add useful information.  Annotated function members (type
    /// `A -> B`) ARE included; the type system treats all block members
    /// equally for type checking even though functions don't appear in
    /// rendered output.
    ///
    /// The record is always marked **open** (`{k: T, ..}`) because a block
    /// may have additional unannotated members beyond what we can enumerate.
    fn synthesise_block(&mut self, fields: &BlockMap<RcExpr>) -> Type {
        let mut field_types: BTreeMap<String, Type> = BTreeMap::new();
        for (key, value) in fields.iter() {
            let ty = self.synthesise(value);
            // Only include fields whose type carries real information.
            if is_informative(&ty) {
                field_types.insert(key.clone(), ty);
            }
        }
        Type::Record {
            fields: field_types,
            open: true, // open: unannotated members may exist at runtime
        }
    }

    /// Synthesise the result type of a field lookup `.field` on `obj_type`.
    ///
    /// - Known record + present field → return the field's type.
    /// - Known open record + absent field → return `any` (may be present at runtime).
    /// - Known closed record + absent field → emit a warning and return `any`.
    /// - `any` object type → return `any` (gradual boundary, no warning).
    /// - Non-record object type → return `any` (cannot reason about field access).
    fn synthesise_lookup(&mut self, smid: Smid, obj_type: &Type, field: &str) -> Type {
        match obj_type {
            Type::Record { fields, open } => {
                if let Some(field_ty) = fields.get(field) {
                    // Field is known — return its type directly.
                    field_ty.clone()
                } else if *open {
                    // Open record may have this field at runtime.
                    Type::Any
                } else {
                    // Closed record: field is definitely absent.
                    let known: Vec<&str> = fields.keys().map(String::as_str).collect();
                    let warning = TypeWarning::new(format!(
                        "field '{field}' not found in closed record type"
                    ))
                    .at(smid)
                    .with_types(format!("{{{}}}", known.join(", ")), format!(".{field}"));
                    self.warnings.push(warning);
                    Type::Any
                }
            }
            // Gradual boundary or unknown — no warning.
            _ => Type::Any,
        }
    }

    /// Synthesise the result type of a function application.
    ///
    /// Synthesises the function type, then checks each argument against the
    /// expected parameter type, currying through function types.
    fn synthesise_app(&mut self, smid: Smid, func: &RcExpr, args: &[RcExpr]) -> Type {
        let func_type = self.synthesise(func);

        // Curry through the function type, checking each argument in turn.
        let mut current_type = func_type;
        for arg in args {
            current_type = self.apply_one(smid, current_type, arg);
        }
        current_type
    }

    /// Apply a single argument to the current function type, returning the
    /// result type.  Emits a warning when the types are not consistent.
    fn apply_one(&mut self, smid: Smid, func_type: Type, arg: &RcExpr) -> Type {
        match func_type {
            Type::Function(param_type, result_type) => {
                let arg_type = self.synthesise(arg);
                if !is_informative(&arg_type) || !is_informative(&param_type) {
                    // One side is uninformative (`any`) — no warning.
                    return *result_type;
                }
                if !is_consistent(&arg_type, &param_type) {
                    self.emit_type_mismatch(
                        smid,
                        &param_type,
                        &arg_type,
                        "argument type does not match function parameter",
                    );
                }
                *result_type
            }
            // `any` function — we cannot say anything about arguments.
            Type::Any => Type::Any,
            // Applied to something that is not a function — still recurse to
            // collect warnings from the argument sub-expression.
            _ => {
                self.synthesise(arg);
                Type::Any
            }
        }
    }

    // ── Checking ─────────────────────────────────────────────────────────────

    /// Check `expr` against `expected`, emitting warnings on mismatch.
    ///
    /// `smid` is the source location used for warning annotation.
    pub fn check_against(&mut self, expr: &RcExpr, expected: &Type, smid: Smid) {
        // Gradual boundary: `any` is consistent with everything.
        if !is_informative(expected) {
            // Recurse to collect warnings from sub-expressions.
            self.synthesise(expr);
            return;
        }

        // Lambda checking: bind parameter type when target is known.
        if let (Expr::Lam(_, _, scope), Type::Function(param_type, result_type)) =
            (&*expr.inner, expected)
        {
            return self.check_lambda(scope, param_type, result_type);
        }

        let found = self.synthesise(expr);

        if !is_informative(&found) {
            // `any` found type — no warning.
            return;
        }

        if !is_consistent(&found, expected) {
            self.emit_type_mismatch(
                smid,
                expected,
                &found,
                "expression type does not match annotation",
            );
        }
    }

    /// Check a lambda body against an expected function type.
    ///
    /// Binds the first parameter to `param_type`, then checks the body against
    /// `result_type` (which may be another function type for curried lambdas).
    fn check_lambda(
        &mut self,
        scope: &crate::core::expr::LamScope<RcExpr>,
        param_type: &Type,
        result_type: &Type,
    ) {
        // Bind the first parameter to the expected parameter type.
        let mut frame: HashMap<String, Type> = HashMap::new();
        if let Some(first_param) = scope.pattern.first() {
            frame.insert(first_param.clone(), param_type.clone());
        }

        // Remaining parameters get `any` for now (full currying deferred).
        for param in scope.pattern.iter().skip(1) {
            frame.insert(param.clone(), Type::Any);
        }

        self.push_scope(frame);

        if scope.pattern.len() <= 1 {
            self.check_against(&scope.body, result_type, scope.body.smid());
        } else {
            // Multi-param lambda with only the first param bound to the known
            // type — check the body against `any` for remaining params.
            self.synthesise(&scope.body);
        }

        self.pop_scope();
    }

    // ── Warning emission ─────────────────────────────────────────────────────

    fn emit_type_mismatch(&mut self, smid: Smid, expected: &Type, found: &Type, message: &str) {
        let warning = TypeWarning::new(message)
            .at(smid)
            .with_types(expected.to_string(), found.to_string());
        self.warnings.push(warning);
    }
}

// ── Helpers ─────────────────────────────────────────────────────────────────

/// Synthesise the type for a primitive literal.
fn synthesise_primitive(prim: &Primitive) -> Type {
    match prim {
        Primitive::Num(_) => Type::Number,
        Primitive::Str(_) => Type::String,
        Primitive::Sym(_) => Type::Symbol,
        Primitive::Bool(_) => Type::Bool,
        Primitive::Null => Type::Null,
    }
}

/// Build the list element type from a Vec of synthesised element types.
///
/// - Empty list → `[never]`
/// - All same type → `[T]`
/// - Mixed types → `[T1 | T2 | ... | Tn]` (deduplicated)
fn synthesise_list_type(types: Vec<Type>) -> Type {
    // Filter out `any` (it would widen the union uselessly) and `never`
    // (empty contributions), then deduplicate.
    let mut seen: Vec<Type> = Vec::new();
    for ty in types {
        if is_informative(&ty) && !seen.contains(&ty) {
            seen.push(ty);
        }
    }

    let elem_type = match seen.len() {
        0 => Type::Never,
        1 => seen.into_iter().next().unwrap(),
        _ => Type::Union(seen),
    };

    Type::List(Box::new(elem_type))
}

/// Extract a string literal value from a core expression.
fn extract_string_literal(expr: &RcExpr) -> Option<String> {
    if let Expr::Literal(_, Primitive::Str(s)) = &*expr.inner {
        Some(s.clone())
    } else {
        None
    }
}

/// Returns `true` when `ty` carries useful information for checking.
///
/// `any` (and `never`) are considered uninformative because:
/// - `any` is the gradual boundary — it suppresses warnings.
/// - `never` represents empty/unreachable code.
fn is_informative(ty: &Type) -> bool {
    !matches!(ty, Type::Any | Type::Never)
}

// ── Public entry point ───────────────────────────────────────────────────────

/// Run the type checker over `expr` and return all warnings found.
pub fn type_check(expr: &RcExpr) -> Vec<TypeWarning> {
    let mut checker = Checker::new();
    checker.check_expr(expr);
    checker.into_warnings()
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::expr::core;

    fn num_lit(n: i64) -> RcExpr {
        core::num(Smid::default(), n)
    }

    fn str_lit(s: &str) -> RcExpr {
        core::str(Smid::default(), s)
    }

    fn sym_lit(s: &str) -> RcExpr {
        core::sym(Smid::default(), s)
    }

    fn bool_lit(b: bool) -> RcExpr {
        core::bool_(Smid::default(), b)
    }

    fn null_lit() -> RcExpr {
        core::null(Smid::default())
    }

    fn list(items: Vec<RcExpr>) -> RcExpr {
        RcExpr::from(Expr::List(Smid::default(), items))
    }

    fn meta_with_type(inner: RcExpr, type_str: &str) -> RcExpr {
        let type_val = core::str(Smid::default(), type_str);
        let meta_block = core::block(Smid::default(), [("type".to_string(), type_val)]);
        core::meta(Smid::default(), inner, meta_block)
    }

    fn meta_with_hint(inner: RcExpr, hint_str: &str) -> RcExpr {
        let hint_val = core::str(Smid::default(), hint_str);
        let meta_block = core::block(Smid::default(), [("__type_hint".to_string(), hint_val)]);
        core::meta(Smid::default(), inner, meta_block)
    }

    // ── Literal synthesis ───────────────────────────────────────────────────

    #[test]
    fn synthesise_number_literal() {
        let mut c = Checker::new();
        assert_eq!(c.synthesise(&num_lit(42)), Type::Number);
    }

    #[test]
    fn synthesise_string_literal() {
        let mut c = Checker::new();
        assert_eq!(c.synthesise(&str_lit("hello")), Type::String);
    }

    #[test]
    fn synthesise_symbol_literal() {
        let mut c = Checker::new();
        assert_eq!(c.synthesise(&sym_lit("foo")), Type::Symbol);
    }

    #[test]
    fn synthesise_bool_literal() {
        let mut c = Checker::new();
        assert_eq!(c.synthesise(&bool_lit(true)), Type::Bool);
    }

    #[test]
    fn synthesise_null_literal() {
        let mut c = Checker::new();
        assert_eq!(c.synthesise(&null_lit()), Type::Null);
    }

    // ── List synthesis ──────────────────────────────────────────────────────

    #[test]
    fn empty_list_is_never_list() {
        let mut c = Checker::new();
        assert_eq!(
            c.synthesise(&list(vec![])),
            Type::List(Box::new(Type::Never))
        );
    }

    #[test]
    fn homogeneous_list_synthesises_element_type() {
        let mut c = Checker::new();
        let l = list(vec![num_lit(1), num_lit(2), num_lit(3)]);
        assert_eq!(c.synthesise(&l), Type::List(Box::new(Type::Number)));
    }

    #[test]
    fn heterogeneous_list_synthesises_union() {
        let mut c = Checker::new();
        let l = list(vec![num_lit(1), str_lit("hello")]);
        assert_eq!(
            c.synthesise(&l),
            Type::List(Box::new(Type::Union(vec![Type::Number, Type::String])))
        );
    }

    // ── Unknown variable is any ──────────────────────────────────────────────

    #[test]
    fn unknown_free_var_is_any() {
        let mut c = Checker::new();
        let var_expr = RcExpr::from(Expr::Var(Smid::default(), Var::Free("unknown".to_string())));
        assert_eq!(c.synthesise(&var_expr), Type::Any);
    }

    // ── Meta annotation ─────────────────────────────────────────────────────

    #[test]
    fn meta_with_type_annotation_returns_annotation_type() {
        let mut c = Checker::new();
        // Annotated number → type annotation is authoritative
        let expr = meta_with_type(num_lit(1), "number");
        assert_eq!(c.synthesise(&expr), Type::Number);
    }

    #[test]
    fn meta_with_wrong_annotation_emits_warning() {
        let mut c = Checker::new();
        // `"hello"` annotated as `number` — should warn
        let expr = meta_with_type(str_lit("hello"), "number");
        c.synthesise(&expr);
        let warnings = c.into_warnings();
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].expected.as_deref() == Some("number"));
        assert!(warnings[0].found.as_deref() == Some("string"));
    }

    #[test]
    fn meta_with_correct_annotation_no_warning() {
        let mut c = Checker::new();
        let expr = meta_with_type(num_lit(42), "number");
        c.synthesise(&expr);
        assert!(c.into_warnings().is_empty());
    }

    #[test]
    fn meta_type_hint_used_as_fallback() {
        let mut c = Checker::new();
        let expr = meta_with_hint(num_lit(1), "number");
        // Hint is used when no `type:` key
        assert_eq!(c.synthesise(&expr), Type::Number);
    }

    #[test]
    fn user_type_annotation_takes_priority_over_hint() {
        let mut c = Checker::new();
        // Both keys present: user `type:` wins
        let type_val = core::str(Smid::default(), "string");
        let hint_val = core::str(Smid::default(), "number");
        let meta_block = core::block(
            Smid::default(),
            [
                ("type".to_string(), type_val),
                ("__type_hint".to_string(), hint_val),
            ],
        );
        let expr = core::meta(Smid::default(), str_lit("hello"), meta_block);
        assert_eq!(c.synthesise(&expr), Type::String);
    }

    // ── Type variable erasure ────────────────────────────────────────────────

    #[test]
    fn type_vars_erased_to_any_in_function() {
        use crate::core::typecheck::types::TypeVarId;
        let ty = Type::Function(
            Box::new(Type::Var(TypeVarId("a".to_string()))),
            Box::new(Type::Var(TypeVarId("b".to_string()))),
        );
        assert_eq!(
            Checker::erase_type_vars(ty),
            Type::Function(Box::new(Type::Any), Box::new(Type::Any))
        );
    }

    #[test]
    fn type_vars_erased_in_list() {
        use crate::core::typecheck::types::TypeVarId;
        let ty = Type::List(Box::new(Type::Var(TypeVarId("a".to_string()))));
        assert_eq!(
            Checker::erase_type_vars(ty),
            Type::List(Box::new(Type::Any))
        );
    }

    // ── Application checking ─────────────────────────────────────────────────

    #[test]
    fn app_with_correct_arg_no_warning() {
        let mut c = Checker::new();
        // Seed env: `double : number -> number`
        let mut frame = HashMap::new();
        frame.insert(
            "double".to_string(),
            Type::Function(Box::new(Type::Number), Box::new(Type::Number)),
        );
        c.push_scope(frame);

        let double_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("double".to_string())));
        let app = RcExpr::from(Expr::App(Smid::default(), double_var, vec![num_lit(5)]));

        c.synthesise(&app);
        assert!(c.into_warnings().is_empty());
    }

    #[test]
    fn app_with_wrong_arg_type_emits_warning() {
        let mut c = Checker::new();
        // Seed env: `double : number -> number`
        let mut frame = HashMap::new();
        frame.insert(
            "double".to_string(),
            Type::Function(Box::new(Type::Number), Box::new(Type::Number)),
        );
        c.push_scope(frame);

        let double_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("double".to_string())));
        let app = RcExpr::from(Expr::App(
            Smid::default(),
            double_var,
            vec![str_lit("oops")], // wrong type
        ));

        c.synthesise(&app);
        let warnings = c.into_warnings();
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].expected.as_deref(), Some("number"));
        assert_eq!(warnings[0].found.as_deref(), Some("string"));
    }

    #[test]
    fn app_returns_result_type_of_function() {
        let mut c = Checker::new();
        let mut frame = HashMap::new();
        frame.insert(
            "str_of".to_string(),
            Type::Function(Box::new(Type::Number), Box::new(Type::String)),
        );
        c.push_scope(frame);

        let func_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("str_of".to_string())));
        let app = RcExpr::from(Expr::App(Smid::default(), func_var, vec![num_lit(42)]));

        let result = c.synthesise(&app);
        assert_eq!(result, Type::String);
    }

    #[test]
    fn app_with_any_function_type_no_warning() {
        let mut c = Checker::new();
        // Unknown function — no type info, no warning
        let func_var = RcExpr::from(Expr::Var(
            Smid::default(),
            Var::Free("unknown_fn".to_string()),
        ));
        let app = RcExpr::from(Expr::App(Smid::default(), func_var, vec![str_lit("hello")]));

        c.synthesise(&app);
        assert!(c.into_warnings().is_empty());
    }

    // ── Let binding ──────────────────────────────────────────────────────────

    #[test]
    fn let_binding_seeds_scope_for_body() {
        let mut c = Checker::new();

        // let x = 42
        //     double = (__DOUBLE : number -> number)
        // in double(x)
        let double_meta = meta_with_type(
            RcExpr::from(Expr::Intrinsic(Smid::default(), "__DOUBLE".to_string())),
            "number -> number",
        );

        // Build the body using free variables; close_let_scope will bind them.
        let x_ref = RcExpr::from(Expr::Var(Smid::default(), Var::Free("x".to_string())));
        let double_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("double".to_string())));
        let app = RcExpr::from(Expr::App(Smid::default(), double_var, vec![x_ref]));

        let let_expr = core::let_(
            Smid::default(),
            vec![
                ("x".to_string(), num_lit(42)),
                ("double".to_string(), double_meta),
            ],
            app,
        );

        let result_type = c.synthesise(&let_expr);
        assert_eq!(result_type, Type::Number);
        assert!(c.into_warnings().is_empty());
    }

    // ── Lambda checking ──────────────────────────────────────────────────────

    #[test]
    fn lambda_checked_against_function_type_no_warning() {
        use crate::core::expr::close_lam_scope;

        let mut c = Checker::new();

        // λ x. x — checked against `number -> number`
        // Use close_lam_scope so the param is properly bound in the body.
        let x_free = RcExpr::from(Expr::Var(Smid::default(), Var::Free("x".to_string())));
        let scope = close_lam_scope(vec!["x".to_string()], x_free);
        let lam = RcExpr::from(Expr::Lam(Smid::default(), false, scope));

        let fn_type = Type::Function(Box::new(Type::Number), Box::new(Type::Number));
        c.check_against(&lam, &fn_type, Smid::default());
        assert!(c.into_warnings().is_empty());
    }

    // ── Pipeline / catenation style ──────────────────────────────────────────

    #[test]
    fn catenation_type_flows_left_to_right() {
        let mut c = Checker::new();
        // Simulates: `42 double str_of`
        // = str_of(double(42)) = App(str_of, [App(double, [42])])
        let mut frame = HashMap::new();
        frame.insert(
            "double".to_string(),
            Type::Function(Box::new(Type::Number), Box::new(Type::Number)),
        );
        frame.insert(
            "str_of".to_string(),
            Type::Function(Box::new(Type::Number), Box::new(Type::String)),
        );
        c.push_scope(frame);

        let double_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("double".to_string())));
        let str_of_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("str_of".to_string())));

        // App(str_of, [App(double, [42])])
        let inner = RcExpr::from(Expr::App(Smid::default(), double_var, vec![num_lit(42)]));
        let outer = RcExpr::from(Expr::App(Smid::default(), str_of_var, vec![inner]));

        let result = c.synthesise(&outer);
        assert_eq!(result, Type::String);
        assert!(c.into_warnings().is_empty());
    }

    // ── Consistency boundary ─────────────────────────────────────────────────

    #[test]
    fn any_arg_passes_any_function_parameter_without_warning() {
        let mut c = Checker::new();
        // function `f : any -> string` called with `any` arg — no warning
        let mut frame = HashMap::new();
        frame.insert(
            "f".to_string(),
            Type::Function(Box::new(Type::Any), Box::new(Type::String)),
        );
        c.push_scope(frame);

        let f_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("f".to_string())));
        // unknown var — type is `any`
        let arg = RcExpr::from(Expr::Var(Smid::default(), Var::Free("unknown".to_string())));
        let app = RcExpr::from(Expr::App(Smid::default(), f_var, vec![arg]));

        c.synthesise(&app);
        assert!(c.into_warnings().is_empty());
    }

    #[test]
    fn string_arg_to_number_param_emits_warning() {
        let mut c = Checker::new();
        let mut frame = HashMap::new();
        frame.insert(
            "f".to_string(),
            Type::Function(Box::new(Type::Number), Box::new(Type::String)),
        );
        c.push_scope(frame);

        let f_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("f".to_string())));
        let app = RcExpr::from(Expr::App(Smid::default(), f_var, vec![str_lit("wrong")]));

        c.synthesise(&app);
        let warnings = c.into_warnings();
        assert_eq!(warnings.len(), 1);
    }

    #[test]
    fn type_check_top_level_collects_all_warnings() {
        let warnings = type_check(&str_lit("hello"));
        assert!(warnings.is_empty(), "plain string literal has no warnings");
    }

    // ── Block synthesis ──────────────────────────────────────────────────────

    #[test]
    fn block_synthesis_includes_annotated_fields() {
        let mut c = Checker::new();
        // { name: "Alice", age: 30 } — both fields have known types
        let fields = vec![
            ("name".to_string(), str_lit("Alice")),
            ("age".to_string(), num_lit(30)),
        ];
        let block = core::block(Smid::default(), fields);
        let ty = c.synthesise(&block);
        assert_eq!(
            ty,
            Type::Record {
                fields: {
                    let mut m = std::collections::BTreeMap::new();
                    m.insert("age".to_string(), Type::Number);
                    m.insert("name".to_string(), Type::String);
                    m
                },
                open: true,
            }
        );
    }

    #[test]
    fn block_synthesis_includes_annotated_function_member() {
        let mut c = Checker::new();
        // { greet: (greet_impl : string -> string) }
        let greet_impl = meta_with_type(
            RcExpr::from(Expr::Intrinsic(Smid::default(), "__GREET".to_string())),
            "string -> string",
        );
        let block = core::block(Smid::default(), [("greet".to_string(), greet_impl)]);
        let ty = c.synthesise(&block);
        assert_eq!(
            ty,
            Type::Record {
                fields: {
                    let mut m = std::collections::BTreeMap::new();
                    m.insert(
                        "greet".to_string(),
                        Type::Function(Box::new(Type::String), Box::new(Type::String)),
                    );
                    m
                },
                open: true,
            }
        );
    }

    // ── Lookup typing ────────────────────────────────────────────────────────

    fn lookup(obj: RcExpr, field: &str) -> RcExpr {
        RcExpr::from(Expr::Lookup(Smid::default(), obj, field.to_string(), None))
    }

    #[test]
    fn lookup_known_field_returns_field_type() {
        let mut c = Checker::new();
        // Seed scope with `rec : {name: string, age: number, ..}`
        let mut frame = HashMap::new();
        frame.insert(
            "rec".to_string(),
            Type::Record {
                fields: {
                    let mut m = std::collections::BTreeMap::new();
                    m.insert("name".to_string(), Type::String);
                    m.insert("age".to_string(), Type::Number);
                    m
                },
                open: true,
            },
        );
        c.push_scope(frame);

        let rec_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("rec".to_string())));
        let ty = c.synthesise(&lookup(rec_var, "name"));
        assert_eq!(ty, Type::String);
        assert!(c.into_warnings().is_empty());
    }

    #[test]
    fn lookup_unknown_field_on_open_record_returns_any() {
        let mut c = Checker::new();
        let mut frame = HashMap::new();
        frame.insert(
            "rec".to_string(),
            Type::Record {
                fields: std::collections::BTreeMap::new(),
                open: true,
            },
        );
        c.push_scope(frame);

        let rec_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("rec".to_string())));
        let ty = c.synthesise(&lookup(rec_var, "missing"));
        assert_eq!(ty, Type::Any);
        // Open record — no warning
        assert!(c.into_warnings().is_empty());
    }

    #[test]
    fn lookup_unknown_field_on_closed_record_emits_warning() {
        let mut c = Checker::new();
        let mut frame = HashMap::new();
        frame.insert(
            "rec".to_string(),
            Type::Record {
                fields: {
                    let mut m = std::collections::BTreeMap::new();
                    m.insert("x".to_string(), Type::Number);
                    m
                },
                open: false,
            },
        );
        c.push_scope(frame);

        let rec_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("rec".to_string())));
        let ty = c.synthesise(&lookup(rec_var, "missing"));
        assert_eq!(ty, Type::Any);
        let warnings = c.into_warnings();
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("'missing'"));
    }

    #[test]
    fn lookup_on_any_returns_any_no_warning() {
        let mut c = Checker::new();
        // Unknown var — type is `any`
        let unknown = RcExpr::from(Expr::Var(Smid::default(), Var::Free("x".to_string())));
        let ty = c.synthesise(&lookup(unknown, "field"));
        assert_eq!(ty, Type::Any);
        assert!(c.into_warnings().is_empty());
    }

    // ── Namespace block typing ───────────────────────────────────────────────

    #[test]
    fn namespace_block_lookup_resolves_function_member() {
        let mut c = Checker::new();
        // Simulate a namespace block like `str` with an annotated `length` member.
        //   { length: (__LEN : string -> number) }
        let length_impl = meta_with_type(
            RcExpr::from(Expr::Intrinsic(Smid::default(), "__LEN".to_string())),
            "string -> number",
        );
        let ns_block = core::block(Smid::default(), [("length".to_string(), length_impl)]);
        // Synthesise the block to get its record type, then look up `length`.
        let ns_type = c.synthesise(&ns_block);

        // Simulate `str.length` — a lookup of `length` on the namespace.
        let result = c.synthesise_lookup(Smid::default(), &ns_type, "length");
        assert_eq!(
            result,
            Type::Function(Box::new(Type::String), Box::new(Type::Number))
        );
        assert!(c.into_warnings().is_empty());
    }

    // ── Type aliases ─────────────────────────────────────────────────────────

    #[test]
    fn type_def_registers_alias() {
        use crate::core::expr::core;

        let mut c = Checker::new();

        // ` { type-def: "Point" }
        // origin: { x: 0, y: 0 }
        let type_def_val = core::str(Smid::default(), "Point");
        let meta_block = core::block(Smid::default(), [("type-def".to_string(), type_def_val)]);
        let inner_block = core::block(
            Smid::default(),
            [("x".to_string(), num_lit(0)), ("y".to_string(), num_lit(0))],
        );
        let annotated = core::meta(Smid::default(), inner_block, meta_block);

        let let_expr = core::let_(
            Smid::default(),
            vec![("origin".to_string(), annotated)],
            RcExpr::from(Expr::Var(Smid::default(), Var::Free("origin".to_string()))),
        );

        c.synthesise(&let_expr);
        // The alias "Point" should now be registered.
        assert!(c.aliases.contains_key("Point"));
    }

    #[test]
    fn types_block_in_metadata_registers_aliases() {
        use crate::core::expr::core;

        let mut c = Checker::new();

        // { types: { MyStr: "string" } }
        // bound to some expression
        let alias_type_str = core::str(Smid::default(), "string");
        let types_inner = core::block(Smid::default(), [("MyStr".to_string(), alias_type_str)]);
        let meta_block = core::block(Smid::default(), [("types".to_string(), types_inner)]);
        let meta_expr = core::meta(Smid::default(), num_lit(0), meta_block);

        c.synthesise(&meta_expr);
        assert!(c.aliases.contains_key("MyStr"));
        assert_eq!(c.aliases.get("MyStr"), Some(&Type::String));
    }

    #[test]
    fn alias_resolved_in_type_annotation() {
        use crate::core::expr::core;

        let mut c = Checker::new();
        // Register alias manually.
        c.register_alias("Num".to_string(), Type::Number);

        // Annotation `"Num"` should resolve to `number`.
        let type_val = core::str(Smid::default(), "Num");
        let meta_block = core::block(Smid::default(), [("type".to_string(), type_val)]);
        let annotated = core::meta(Smid::default(), num_lit(42), meta_block);

        let ty = c.synthesise(&annotated);
        // Alias "Num" → number, erased type vars don't change it.
        assert_eq!(ty, Type::Number);
        assert!(c.into_warnings().is_empty());
    }
}
