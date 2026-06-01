//! Bidirectional type checker for eucalypt core expressions.
//!
//! Implements two modes:
//!
//! - **Synthesis** (⇒): compute the type of an expression from its structure.
//! - **Checking** (⇐): verify that an expression is consistent with an expected type.
//!
//! ## Polymorphism
//!
//! Type annotations may contain type variables (`a`, `b`, etc.), forming
//! polymorphic schemes (`forall a b. (a -> b) -> [a] -> [b]`).  The checker
//! stores schemes in the scope; on each variable reference the scheme is
//! *freshened* — its quantified variables are replaced with fresh unification
//! variables (`_t0`, `_t1`, …).  As arguments are applied, the fresh variables
//! are unified with the concrete argument types, propagating information
//! through the return type.
//!
//! ## Overloaded operators
//!
//! Operators such as `+` carry a `Union` type covering all overloads.  When
//! an argument is applied to a union-typed function, the checker tries each
//! variant in order and commits to the first that unifies.
//!
//! Type issues are always warnings — they never prevent evaluation.

use std::collections::{BTreeMap, HashMap, VecDeque};

use crate::{
    common::sourcemap::{intrinsic_display_name, HasSmid, Smid},
    core::{
        binding::{BoundVar, Var},
        expr::{BlockMap, Expr, Primitive, RcExpr},
        typecheck::{
            error::TypeWarning,
            parse,
            subtype::{is_consistent, is_subtype},
            types::{Type, TypeScheme},
            unify::{apply_subst, freshen, infer_scheme, unify, Substitution},
        },
    },
};

// ── Flow-sensitive narrowing types ───────────────────────────────────────────

/// Stable binding identity for a narrowed bound variable.
///
/// The first element is the *anchored frame index*:
/// `scope_stack.len() - 1 - bv.scope` at the point narrowing is
/// established. This value is invariant under inner scope pushes — when
/// an extra frame is pushed, both `len` and the variable's `bv.scope`
/// grow by one, so the difference is unchanged. A narrowing fact installed
/// for a particular variable therefore continues to match that same
/// variable deeper inside the branch without matching inner rebindings of
/// the same name (which have a different anchored index).
type BoundKey = (usize, String);

/// One narrowing frame: a set of type overrides for variables within a branch.
///
/// Installed on the `narrow_stack` before synthesising a branch body and
/// popped immediately after. Keyed by `BoundKey` so a fact matches the
/// *specific binding* whose type was narrowed, not just any variable with
/// the same name.
#[derive(Debug, Default, Clone)]
struct NarrowFrame {
    entries: HashMap<BoundKey, Type>,
}

impl NarrowFrame {
    fn new() -> Self {
        NarrowFrame {
            entries: HashMap::new(),
        }
    }

    /// Merge `other` into `self`. For the same key, the existing entry wins
    /// (the caller controls priority by choosing which frame is `self`).
    fn merge_from(&mut self, other: NarrowFrame) {
        for (k, v) in other.entries {
            self.entries.entry(k).or_insert(v);
        }
    }
}

/// Facts derived from analysing a condition expression.
///
/// `positive` applies in the branch where the condition is *true*.
/// `negative` applies in the branch where the condition is *false*.
#[derive(Debug, Default, Clone)]
struct ConditionFacts {
    positive: NarrowFrame,
    negative: NarrowFrame,
}

impl ConditionFacts {
    fn empty() -> Self {
        ConditionFacts::default()
    }

    /// Swap positive and negative (models `not(c)` inversion).
    fn negated(self) -> Self {
        ConditionFacts {
            positive: self.negative,
            negative: self.positive,
        }
    }
}

/// The kind of branching construct recognised by the checker.
#[derive(Debug, Clone)]
enum BranchKind {
    /// `if(condition, true_branch, false_branch)` — 3-argument form.
    If,
    /// `and(left, right)` — synthesise `right` under positive facts from `left`.
    And,
    /// `or(left, right)` — synthesise `right` under negative facts from `left`.
    Or,
    /// `cond(clause_list)` — multi-way conditional with `=>` clauses.
    Cond,
}

/// A type predicate recognisable from the condition expression.
#[derive(Debug, Clone)]
enum PredicateKind {
    Number,
    String,
    Symbol,
    Bool,
    List,
    Block,
    Nil,    // nil? or null? — tests for null / empty list
    NotNil, // not-nil? — positive = non-null, negative = null
}

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
///
/// # Scope stack invariant
///
/// The scope stack corresponds 1:1 with de Bruijn scope levels.  Every
/// scope-introducing form the checker enters (Let bindings, Lambda
/// parameters via `check_lambda`) pushes exactly one frame.  This means
/// `scope_stack[i]` is the frame for de Bruijn level `i` (0 = innermost).
///
/// Bound variables use their de Bruijn `scope` index to look up directly
/// in the correct frame via `lookup_bound`, avoiding name-shadowing bugs
/// that plague name-based lookup.
///
/// The checker only encounters `BoundVar` nodes inside code it has
/// actually traversed (you can't reach a variable without entering its
/// enclosing scopes), so the scope index is always within range for
/// variables the checker synthesises — except for references to scopes
/// outside the checker's root expression (the global/prelude scope),
/// which fall back to name-based lookup.
pub struct Checker {
    /// Scope stack.  Front = innermost scope.
    ///
    /// Each frame maps binding name → `TypeScheme`.  Polymorphic schemes are
    /// freshened on every lookup; monomorphic schemes are returned as-is.
    ///
    /// Position in the deque corresponds to de Bruijn scope level:
    /// index 0 is the innermost scope, index N is N scopes outward.
    scope_stack: VecDeque<HashMap<String, TypeScheme>>,

    /// Counter for generating unique fresh type-variable names (`_t0`, `_t1`, …).
    var_counter: u32,

    /// Accumulated warnings.
    warnings: Vec<TypeWarning>,

    /// Type alias map: capitalised names → concrete `Type`.
    ///
    /// Populated from `type-def:` metadata and `types:` blocks as the
    /// checker walks the expression tree.  Used in `extract_annotation` to
    /// resolve alias references before erasing type variables.
    aliases: AliasMap,

    /// Lambda parameter types inferred during function argument checking.
    ///
    /// Maps `Smid` (source location of the lambda) → parameter types.
    /// Used by LSP features (inlay hints on monadic block bound
    /// variables) to show the unwrapped element type.
    lambda_params: HashMap<Smid, Vec<(String, Type)>>,

    /// Flow-sensitive narrowing stack.
    ///
    /// Each entry overrides a variable's type within the branch currently
    /// being synthesised. This is a *parallel* stack — it is never pushed
    /// onto `scope_stack`, which would shift all de Bruijn indices. Instead,
    /// `lookup_bound` consults this stack before the normal scope lookup.
    /// Push before synthesising a branch body; pop immediately after.
    narrowing: Vec<NarrowFrame>,
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
            var_counter: 0,
            warnings: Vec::new(),
            aliases: AliasMap::new(),
            lambda_params: HashMap::new(),
            narrowing: Vec::new(),
        }
    }

    /// Consume the checker and return all accumulated warnings.
    pub fn into_warnings(self) -> Vec<TypeWarning> {
        self.warnings
    }

    /// Return the flattened type environment from the scope stack.
    ///
    /// Merges all scope frames (innermost wins) into a single map of
    /// name → `Type`.  This is the checker's view of all bindings it
    /// has seen, suitable for IDE features like hover and completion.
    pub fn type_env(&self) -> HashMap<String, Type> {
        let mut env = HashMap::new();
        // Iterate outermost-first so inner scopes overwrite
        for frame in self.scope_stack.iter().rev() {
            for (name, scheme) in frame {
                env.insert(name.clone(), scheme.body.clone());
            }
        }
        env
    }

    /// Consume the checker and return warnings plus the type environment.
    pub fn into_results(self) -> TypeCheckResult {
        let type_env = self.type_env();
        TypeCheckResult {
            warnings: self.warnings,
            types: type_env,
            lambda_params: self.lambda_params,
        }
    }

    /// Primary entry point: walk `expr` and collect warnings.
    pub fn check_expr(&mut self, expr: &RcExpr) {
        self.synthesise(expr);
    }

    // ── Scope management ────────────────────────────────────────────────────

    fn push_scope(&mut self, frame: HashMap<String, TypeScheme>) {
        self.scope_stack.push_front(frame);
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop_front();
    }

    /// Look up a name, searching from the innermost scope outward.
    ///
    /// Polymorphic schemes are freshened on each lookup so every use gets its
    /// own set of unification variables.  Monomorphic schemes are returned
    /// with their body type unchanged.
    fn lookup_name(&mut self, name: &str) -> Type {
        let scheme = self
            .scope_stack
            .iter()
            .find_map(|frame| frame.get(name))
            .cloned();
        match scheme {
            Some(s) => freshen(&s, &mut self.var_counter),
            None => Type::Any,
        }
    }

    /// Look up a bound variable using its de Bruijn scope index.
    ///
    /// Uses the scope index to look directly in the correct frame, avoiding
    /// name-shadowing false positives.  Falls back to name-based lookup only
    /// when the scope index exceeds the stack depth — this happens for
    /// references to bindings outside the checker's root expression (e.g.
    /// prelude globals when checking a single unit).
    ///
    /// Before the normal scope lookup, consults the narrowing stack.  A hit
    /// overrides the stored type with the narrowed type for the current branch.
    fn lookup_bound(&mut self, bv: &BoundVar) -> Type {
        let name = match bv.name.as_deref() {
            Some(n) => n,
            None => return Type::Any,
        };
        let idx = bv.scope as usize;

        // ── Narrowing check ──────────────────────────────────────────────────
        // Compute the stable anchored index for this variable (invariant under
        // inner scope pushes).  Only check narrowing for in-scope variables
        // (scope index within the stack) — prelude fall-through refs are not
        // narrowed.
        if idx < self.scope_stack.len() {
            let anchored = self.scope_stack.len() - 1 - idx;
            let key = (anchored, name.to_string());
            // Innermost narrowing frame wins.
            for frame in self.narrowing.iter().rev() {
                if let Some(narrowed_ty) = frame.entries.get(&key) {
                    return narrowed_ty.clone();
                }
            }
        }

        if let Some(frame) = self.scope_stack.get(idx) {
            if let Some(scheme) = frame.get(name) {
                return freshen(scheme, &mut self.var_counter);
            }
            // The frame exists but doesn't contain the name.  This shouldn't
            // happen for well-formed core expressions — the de Bruijn index
            // should point to the frame that actually binds this name.
            debug_assert!(
                false,
                "BoundVar '{name}' at scope {idx} not found in frame (frame has: {:?})",
                frame.keys().collect::<Vec<_>>()
            );
            return Type::Any;
        }
        // Scope index beyond the stack — this variable refers to a binding
        // outside the checker's root (e.g. prelude globals).  Fall back to
        // name-based search which covers pre-seeded outer scopes.
        self.lookup_name(name)
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
        self.resolve_aliases_inner(ty, &mut Vec::new())
    }

    /// Inner alias resolution with cycle detection via a `resolving` stack.
    ///
    /// When we encounter an alias name that is already on the stack, we return
    /// `Type::Var(name)` as a back-reference.  After resolving the alias body,
    /// if the back-reference is still free in the result we wrap it in
    /// `Type::Mu(name, body)` to form an equirecursive type.
    fn resolve_aliases_inner(&self, ty: Type, resolving: &mut Vec<String>) -> Type {
        match ty {
            Type::Var(ref v) => {
                if let Some(alias_ty) = self.aliases.get(&v.0) {
                    if resolving.contains(&v.0) {
                        // Cycle detected: return the bare Var as a back-reference.
                        // The caller that pushed this name will wrap it in Mu.
                        return ty;
                    }
                    resolving.push(v.0.clone());
                    let resolved = self.resolve_aliases_inner(alias_ty.clone(), resolving);
                    resolving.pop();
                    // If the alias name still appears free in the resolved body,
                    // the alias is self-referential — wrap in Mu for an
                    // equirecursive type.
                    if contains_var_named(&resolved, &v.0) {
                        Type::Mu(v.clone(), Box::new(resolved))
                    } else {
                        resolved
                    }
                } else {
                    ty
                }
            }
            Type::List(inner) => {
                Type::List(Box::new(self.resolve_aliases_inner(*inner, resolving)))
            }
            Type::Tuple(elems) => Type::Tuple(
                elems
                    .into_iter()
                    .map(|e| self.resolve_aliases_inner(e, resolving))
                    .collect(),
            ),
            Type::IO(inner) => Type::IO(Box::new(self.resolve_aliases_inner(*inner, resolving))),
            Type::Lens(a, b) => Type::Lens(
                Box::new(self.resolve_aliases_inner(*a, resolving)),
                Box::new(self.resolve_aliases_inner(*b, resolving)),
            ),
            Type::Traversal(a, b) => Type::Traversal(
                Box::new(self.resolve_aliases_inner(*a, resolving)),
                Box::new(self.resolve_aliases_inner(*b, resolving)),
            ),
            Type::Function(a, b) => Type::Function(
                Box::new(self.resolve_aliases_inner(*a, resolving)),
                Box::new(self.resolve_aliases_inner(*b, resolving)),
            ),
            Type::Dict(inner) => {
                Type::Dict(Box::new(self.resolve_aliases_inner(*inner, resolving)))
            }
            Type::Record { fields, open, rows } => Type::Record {
                fields: fields
                    .into_iter()
                    .map(|(k, v)| (k, self.resolve_aliases_inner(v, resolving)))
                    .collect(),
                open,
                rows,
            },
            Type::Union(variants) => Type::Union(
                variants
                    .into_iter()
                    .map(|v| self.resolve_aliases_inner(v, resolving))
                    .collect(),
            ),
            // Mu: pass through, resolving inside the body without pushing
            // the binder (it is not an alias name).
            Type::Mu(x, body) => {
                Type::Mu(x, Box::new(self.resolve_aliases_inner(*body, resolving)))
            }
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
    /// Extract a type annotation from metadata, returning the parsed type
    /// and whether it was asserted (prefixed with `!`).
    ///
    /// Asserted annotations are trusted without body verification.
    /// Extract a type annotation from metadata.
    ///
    /// Returns `(type, asserted, is_hint)`:
    /// - `asserted`: `!` prefix — body not verified
    /// - `is_hint`: came from `__type_hint` (monad binding) — the
    ///   annotation is for checking only, not for overriding the
    ///   synthesised type
    fn extract_annotation(&self, meta: &RcExpr) -> Option<(Type, bool, bool)> {
        let block = match &*meta.inner {
            Expr::Block(_, b) => b,
            _ => return None,
        };

        let (type_str, is_hint): (String, bool) = if let Some(e) = block.get("type") {
            (extract_string_literal(e)?, false)
        } else if let Some(e) = block.get("__type_hint") {
            (extract_string_literal(e)?, true)
        } else {
            return None;
        };

        // A leading `!` marks the annotation as asserted (body not verified)
        let (type_str, asserted) = if let Some(stripped) = type_str.strip_prefix('!') {
            (stripped.trim().to_string(), true)
        } else {
            (type_str, false)
        };

        let parsed = parse::parse_type(&type_str).ok()?;
        Some((self.resolve_aliases_in_type(parsed), asserted, is_hint))
    }

    /// Try to extract a `TypeScheme` from a binding value's `Meta` wrapper.
    ///
    /// Returns `Some(scheme)` when the value is wrapped in a `Meta` node that
    /// carries a `type:` or `__type_hint:` annotation, `None` otherwise.  The
    /// type variables in the annotation are quantified into the scheme via
    /// `infer_scheme`.  Alias references are resolved before quantification.
    fn annotation_scheme_of(&self, value: &RcExpr) -> Option<TypeScheme> {
        if let Expr::Meta(_, _, meta) = &*value.inner {
            self.extract_annotation(meta)
                .map(|(ty, _asserted, _is_hint)| infer_scheme(ty))
        } else {
            None
        }
    }

    // ── Synthesis ────────────────────────────────────────────────────────────

    /// Synthesise the type of `expr`, recursing to gather warnings along the way.
    pub fn synthesise(&mut self, expr: &RcExpr) -> Type {
        match &*expr.inner {
            // ── Literals ─────────────────────────────────────────────────────
            Expr::Literal(_, prim) => synthesise_primitive(prim),

            // ── List ─────────────────────────────────────────────────────────
            //
            // Small fixed-length list literals (2-4 elements) synthesise as
            // tuples since tuples are subtypes of lists and carry more
            // information.  Single-element lists stay as lists (not 1-tuples)
            // and large lists stay as lists for practicality.
            Expr::List(_, items) => {
                let elem_types: Vec<Type> = items.iter().map(|e| self.synthesise(e)).collect();
                let n = elem_types.len();
                if (2..=4).contains(&n) {
                    Type::Tuple(elem_types)
                } else {
                    synthesise_list_type(elem_types)
                }
            }

            // ── Block ─────────────────────────────────────────────────────────
            Expr::Block(_, fields) => self.synthesise_block(fields),

            // ── Variables ────────────────────────────────────────────────────
            Expr::Var(_, Var::Free(name)) => self.lookup_name(name),
            Expr::Var(_, Var::Bound(bv)) => self.lookup_bound(bv),

            // ── Name (pre-varify) ─────────────────────────────────────────────
            Expr::Name(_, name) => self.lookup_name(name),

            // ── Intrinsic ─────────────────────────────────────────────────────
            Expr::Intrinsic(_, name) => self.lookup_name(name),

            // ── Metadata ─────────────────────────────────────────────────────
            Expr::Meta(smid, inner, meta) => self.synthesise_meta(*smid, inner, meta),

            // ── Let ───────────────────────────────────────────────────────────
            Expr::Let(_, scope, _) => {
                // Two-pass approach so that annotated bindings are visible when
                // synthesising the values of their sibling bindings.
                //
                // Pass 1: pre-seed the frame with annotation schemes (or a
                // `mono(any)` placeholder for unannotated bindings).
                let mut frame: HashMap<String, TypeScheme> = HashMap::new();
                for (name, value) in &scope.pattern {
                    let scheme = self
                        .annotation_scheme_of(value)
                        .unwrap_or(TypeScheme::mono(Type::Any));
                    frame.insert(name.clone(), scheme);
                }

                self.push_scope(frame);

                // Pass 2: synthesise each binding value — triggers consistency
                // checks against any annotations.  For unannotated bindings,
                // replace the placeholder with the synthesised mono type so
                // later sibling bindings and the body can use it.
                for (name, value) in &scope.pattern {
                    let synthesised = self.synthesise_binding_value(value);
                    let placeholder = TypeScheme::mono(Type::Any);
                    if let Some(frame) = self.scope_stack.front_mut() {
                        if frame.get(name) == Some(&placeholder) {
                            frame.insert(name.clone(), TypeScheme::mono(synthesised.clone()));
                        }
                    }
                    // Register `type-def:` alias when present.
                    if let Some(alias_name) = Self::extract_type_def_name(value) {
                        // Use the explicit `type:` annotation if given; otherwise
                        // the synthesised type (inferred from the value shape).
                        let alias_ty = if let Expr::Meta(_, _, meta) = &*value.inner {
                            self.extract_annotation(meta).map(|(ty, _, _)| ty)
                        } else {
                            None
                        }
                        .unwrap_or(synthesised);
                        self.register_alias(alias_name, alias_ty);
                    }
                }

                let body_type = self.synthesise(&scope.body);
                self.pop_scope();
                body_type
            }

            // ── Lambda ────────────────────────────────────────────────────────
            // Cannot synthesise without knowing parameter types.  Return `any`
            // and let `check_against` handle the case where the type is known.
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

    /// Synthesise the type of a `Meta(smid, inner, meta)` node.
    ///
    /// Steps:
    /// 1. Register any `types:` aliases declared in `meta` so they are
    ///    available for annotations later in the same scope.
    /// 2. If `meta` carries a `type:` or `__type_hint:` annotation, freshen its
    ///    scheme (instantiating type variables), check the inner expression
    ///    against the working type, and return it (authoritative).
    /// 3. Otherwise synthesise the inner expression.
    fn synthesise_meta(&mut self, smid: Smid, inner: &RcExpr, meta: &RcExpr) -> Type {
        // Register `types:` block aliases before reading any annotation so that
        // the annotation itself can reference freshly-declared aliases.
        self.register_aliases_from_meta(meta);

        if let Some((annotated_type, asserted, is_hint)) = self.extract_annotation(meta) {
            let scheme = infer_scheme(annotated_type);
            let working_type = freshen(&scheme, &mut self.var_counter);
            // Asserted annotations (prefixed with `!`) are trusted without
            // checking the body.  Used when the body has type-level assumptions
            // the checker cannot verify (e.g. dependent-length lists as tuples).
            if is_hint {
                // For __type_hint (monad bindings), synthesise the inner
                // expression ONCE, emit a warning if it doesn't match the
                // hint type, then return the synthesised type (not the hint)
                // so downstream unification sees concrete types.
                let inner_type = self.synthesise(inner);

                // Check consistency and warn (replicates check_against logic
                // without double-synthesising).
                if !asserted
                    && is_informative(&inner_type)
                    && is_informative(&working_type)
                    && !is_consistent(&inner_type, &working_type)
                {
                    self.emit_type_mismatch(
                        smid,
                        &working_type,
                        &inner_type,
                        "expression type does not match annotation",
                    );
                }

                // Small list literals synthesise as tuples (e.g. [1,2,3] →
                // (number, number, number)) which don't unify with the
                // wrapper's list type [a].  Convert homogeneous tuples to
                // lists; heterogeneous tuples fall back to the wrapper type.
                if is_informative(&inner_type) {
                    match &inner_type {
                        Type::Tuple(elems) if !elems.is_empty() => {
                            let first = &elems[0];
                            if elems.iter().all(|e| e == first) {
                                // Homogeneous tuple → list of element type
                                Type::List(Box::new(first.clone()))
                            } else {
                                // Heterogeneous tuple → list of union
                                // Type::union deduplicates and normalises
                                // (absorbs LiteralString into String, etc.)
                                let elem_type = Type::union(elems.iter().cloned());
                                Type::List(Box::new(elem_type))
                            }
                        }
                        _ => inner_type,
                    }
                } else {
                    working_type
                }
            } else if !asserted {
                self.check_against(inner, &working_type, smid);
                working_type
            } else {
                working_type
            }
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
            rows: vec![],
        }
    }

    /// Synthesise the result type of a field lookup `.field` on `obj_type`.
    ///
    /// - Known record + present field → return the field's type.
    /// - Known open record + absent field → return `any` (may be present at runtime).
    /// - Known closed record + absent field → emit a warning and return `any`.
    /// - `any` object type → return `any` (gradual boundary, no warning).
    /// - Non-record object type → return `any` (cannot reason about field access).
    pub fn synthesise_lookup(&mut self, smid: Smid, obj_type: &Type, field: &str) -> Type {
        match obj_type {
            Type::Record { fields, open, .. } => {
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
    /// Maintains a `Substitution` across all arguments so that type variables
    /// unified by one argument automatically constrain later arguments and the
    /// return type.
    ///
    /// Before the generic application loop, attempts to recognise the function
    /// as a branching construct (`if`, `and`, `or`, `cond`) and delegates to
    /// the narrowing-aware `synthesise_branch_*` helpers when recognised.
    fn synthesise_app(&mut self, _smid: Smid, func: &RcExpr, args: &[RcExpr]) -> Type {
        // ── Branch recognition (flow-sensitive narrowing) ─────────────────────
        //
        // Check whether the function is a recognised brancher before the normal
        // argument loop.  Only fires when the full argument list is present
        // (partial applications fall through to the generic path).
        if let Some(kind) = classify_brancher(func, self.scope_stack.len()) {
            let result = self.synthesise_branch(kind, func, args);
            if let Some(ty) = result {
                return ty;
            }
            // Fall through: synthesise_branch returned None (arity mismatch,
            // etc.) — use the generic path below.
        }

        // Extract the function name for use in warning messages.
        // For intrinsics, map to the user-facing display name (e.g. "ADD" → "+").
        // Bound variables preserve their original name in the `name` field even
        // after varify replaces free variables with de Bruijn indices.
        let func_name: Option<String> = match &*func.inner {
            Expr::Var(_, Var::Free(name)) | Expr::Name(_, name) => Some(name.clone()),
            Expr::Var(_, Var::Bound(bv)) => bv.name.clone(),
            Expr::Intrinsic(_, name) => {
                // Prefer the user-facing display name; fall back to the raw name.
                Some(
                    intrinsic_display_name(name)
                        .map(str::to_owned)
                        .unwrap_or_else(|| name.clone()),
                )
            }
            _ => None,
        };

        let func_type = self.synthesise(func);
        let mut subst = Substitution::new();
        let mut current_type = func_type;

        for arg in args {
            // Use the argument's own Smid so warnings point at the offending
            // argument, not the function call site.
            let arg_smid = arg.smid();
            current_type = self.apply_one_with_subst(
                arg_smid,
                current_type,
                arg,
                &mut subst,
                func_name.as_deref(),
            );
        }

        // Apply the accumulated substitution to resolve any remaining vars.
        apply_subst(&current_type, &subst)
    }

    /// Apply a single argument to the current function type using unification.
    ///
    /// Unifies the parameter type with the argument type, updating `subst`.
    /// Emits a warning when the types do not unify and neither is uninformative.
    /// `func_name` is the caller-supplied name of the function being applied
    /// (e.g. `"add"`, `"+"`) and is included in the warning message when present.
    fn apply_one_with_subst(
        &mut self,
        smid: Smid,
        func_type: Type,
        arg: &RcExpr,
        subst: &mut Substitution,
        func_name: Option<&str>,
    ) -> Type {
        // Apply any substitutions accumulated so far.
        let func_type = apply_subst(&func_type, subst);

        match func_type {
            Type::Function(param_type, result_type) => {
                let param_applied = apply_subst(&param_type, subst);

                // When the parameter is a Tuple and the argument is a list
                // literal with matching arity, use checking mode (element-wise)
                // rather than synthesis (which loses tuple structure).
                if let Type::Tuple(elem_types) = &param_applied {
                    if let Expr::List(_, items) = &*arg.inner {
                        if items.len() == elem_types.len() {
                            for (item, expected_elem) in items.iter().zip(elem_types.iter()) {
                                let item_smid = item.smid();
                                self.check_against(item, expected_elem, item_smid);
                            }
                            return apply_subst(&result_type, subst);
                        }
                    }
                }

                // When the argument is a lambda and the parameter type is a
                // known function type, use check_against so that check_lambda
                // fires — this infers parameter types for the lambda's bound
                // variables (needed for LSP inlay hints on monadic bindings).
                if let Expr::Lam(lam_smid, _, scope) = &*arg.inner {
                    if matches!(&param_applied, Type::Function(_, _))
                        && is_informative(&param_applied)
                    {
                        self.check_against(arg, &param_applied, smid);
                        // Record resolved lambda parameter types for LSP,
                        // keyed by the lambda's Smid (source location) to
                        // avoid name collisions between different monadic
                        // blocks using the same binding name.
                        let mut params = Vec::new();
                        let mut remaining = param_applied.clone();
                        for param in &scope.pattern {
                            if let Type::Function(p, r) = remaining {
                                let resolved = apply_subst(&p, subst);
                                if is_informative(&resolved) && !matches!(&resolved, Type::Var(_)) {
                                    params.push((param.clone(), resolved));
                                }
                                remaining = *r;
                            }
                        }
                        if !params.is_empty() {
                            self.lambda_params.insert(*lam_smid, params);
                        }
                        return apply_subst(&result_type, subst);
                    }
                }

                let arg_type = self.synthesise(arg);

                if !is_informative(&arg_type) || !is_informative(&param_applied) {
                    // Gradual boundary — no warning.
                    return apply_subst(&result_type, subst);
                }

                match unify(&param_applied, &arg_type, subst) {
                    Ok(()) => apply_subst(&result_type, subst),
                    Err(_) => {
                        // Fall back to subtyping (e.g. Lens <: Traversal)
                        if !is_subtype(&arg_type, &param_applied) {
                            let message = build_arg_mismatch_message(func_name);
                            self.emit_type_mismatch(smid, &param_applied, &arg_type, &message);
                        }
                        apply_subst(&result_type, subst)
                    }
                }
            }

            // Union-typed function — try each overload variant.
            Type::Union(variants) => self.apply_union(smid, variants, arg, subst, func_name),

            // Block application (catenation): record type applied to an argument.
            //
            // In eucalypt, `base override` (catenation) is cooked to `override(base)`:
            // the new/override block is in function position (LHS), and the base
            // block is the argument (RHS).  The function-position block wins on field
            // conflicts — its declared field types take precedence over the base.
            //
            // Merge: start with RHS (base) fields, then overlay LHS (override) fields.
            // LHS wins where both declare the same field.
            //
            // If the RHS type is `any` (unannotated base), the result is an open
            // record with all LHS fields — callers still benefit from the override's
            // known field types.
            Type::Record {
                fields: lhs_fields,
                open: lhs_open,
                rows: lhs_rows,
            } => {
                let rhs_type = self.synthesise(arg);
                match rhs_type {
                    Type::Record {
                        fields: rhs_fields,
                        open: rhs_open,
                        rows: rhs_rows,
                    } => {
                        // Merge: base (RHS) as starting point, overlay LHS (function).
                        // LHS fields override RHS on conflicts.
                        let mut merged = rhs_fields;
                        for (k, v) in lhs_fields {
                            merged.insert(k, v);
                        }
                        // Combine row variables from both sides (deduplicating)
                        let mut combined_rows = lhs_rows;
                        for r in rhs_rows {
                            if !combined_rows.contains(&r) {
                                combined_rows.push(r);
                            }
                        }
                        Type::Record {
                            fields: merged,
                            open: lhs_open || rhs_open,
                            rows: combined_rows,
                        }
                    }
                    // Gradual boundary: base unknown, preserve LHS (override) fields as open.
                    Type::Any => Type::Record {
                        fields: lhs_fields,
                        open: true,
                        rows: lhs_rows,
                    },
                    // RHS is not a record: can't reason about the merge result.
                    _ => Type::Any,
                }
            }

            // Unknown function type — recurse into arg to collect sub-warnings.
            Type::Any => {
                self.synthesise(arg);
                Type::Any
            }

            // Applied to a non-function — still recurse.
            _ => {
                self.synthesise(arg);
                Type::Any
            }
        }
    }

    /// Apply a single argument to a union-typed (overloaded) function.
    ///
    /// Tries each variant in order.  Commits to the first variant whose
    /// parameter type unifies with the argument type.  If no variant matches
    /// and the argument type is informative, emits a type warning.
    /// `func_name` is included in the warning message when present.
    fn apply_union(
        &mut self,
        smid: Smid,
        variants: Vec<Type>,
        arg: &RcExpr,
        subst: &mut Substitution,
        func_name: Option<&str>,
    ) -> Type {
        let arg_type = self.synthesise(arg);

        if !is_informative(&arg_type) {
            // Gradual boundary — cannot determine which overload applies.
            return Type::Any;
        }

        for variant in &variants {
            if let Type::Function(param_type, result_type) = variant {
                let param_applied = apply_subst(param_type, subst);
                let mut trial = subst.clone();
                // Try unification first (binds type variables)
                if unify(&param_applied, &arg_type, &mut trial).is_ok() {
                    *subst = trial;
                    return apply_subst(result_type, subst);
                }
                // Fall back to subtyping (e.g. Lens <: Traversal)
                if is_subtype(&arg_type, &param_applied) {
                    return apply_subst(result_type, subst);
                }
            }
        }

        // No variant matched — build a union of expected parameter types for
        // the error message.
        let param_types: Vec<Type> = variants
            .iter()
            .filter_map(|v| {
                if let Type::Function(p, _) = v {
                    Some(apply_subst(p, subst))
                } else {
                    None
                }
            })
            .collect();

        if !param_types.is_empty() {
            let expected = if param_types.len() == 1 {
                param_types.into_iter().next().unwrap()
            } else {
                Type::Union(param_types)
            };
            let message = build_overload_mismatch_message(func_name);
            self.emit_type_mismatch(smid, &expected, &arg_type, &message);
        }

        Type::Any
    }

    // ── Flow-sensitive narrowing ──────────────────────────────────────────────

    /// Delegate to narrowing-aware branch synthesis for recognised branchers.
    ///
    /// Returns `Some(ty)` when the function is a recognised brancher *and*
    /// the argument count matches the expected arity.  Returns `None` for
    /// partial applications or unrecognised functions — the caller falls back
    /// to the generic application path.
    fn synthesise_branch(
        &mut self,
        kind: BranchKind,
        func: &RcExpr,
        args: &[RcExpr],
    ) -> Option<Type> {
        match kind {
            BranchKind::If if args.len() == 3 => {
                Some(self.synthesise_branch_if(func, &args[0], &args[1], &args[2]))
            }
            BranchKind::And if args.len() == 2 => {
                Some(self.synthesise_branch_and(func, &args[0], &args[1]))
            }
            BranchKind::Or if args.len() == 2 => {
                Some(self.synthesise_branch_or(func, &args[0], &args[1]))
            }
            BranchKind::Cond if args.len() == 1 => {
                Some(self.synthesise_branch_cond(func, &args[0]))
            }
            // Partial application or wrong arity — fall through.
            _ => None,
        }
    }

    /// Synthesise `if(condition, true_branch, false_branch)` with narrowing.
    ///
    /// Analyses the condition to derive narrowing facts, then synthesises
    /// each branch under the appropriate fact set.  Returns the union of
    /// the two branch result types (§A5.9).
    fn synthesise_branch_if(
        &mut self,
        func: &RcExpr,
        condition: &RcExpr,
        true_branch: &RcExpr,
        false_branch: &RcExpr,
    ) -> Type {
        // Synthesise the function itself (for its type — not used for the
        // result but needed so any warnings on the function position fire).
        self.synthesise(func);

        // Synthesise the condition normally (validates condition type).
        self.synthesise(condition);

        // Derive narrowing facts from the condition structure.
        let facts = self.analyse_condition(condition);

        // Synthesise true branch with positive narrowing.
        self.narrowing.push(facts.positive);
        let true_type = self.synthesise(true_branch);
        self.narrowing.pop();

        // Synthesise false branch with negative narrowing.
        self.narrowing.push(facts.negative);
        let false_type = self.synthesise(false_branch);
        self.narrowing.pop();

        // Result: union of branch types (§A5.9).
        make_union_type(true_type, false_type)
    }

    /// Synthesise `and(left, right)` with narrowing.
    ///
    /// `right` is synthesised under the positive facts from `left`.
    /// Returns `Bool`.
    fn synthesise_branch_and(&mut self, func: &RcExpr, left: &RcExpr, right: &RcExpr) -> Type {
        self.synthesise(func);
        self.synthesise(left);
        let facts = self.analyse_condition(left);
        self.narrowing.push(facts.positive);
        self.synthesise(right);
        self.narrowing.pop();
        Type::Bool
    }

    /// Synthesise `or(left, right)` with narrowing.
    ///
    /// `right` is synthesised under the negative facts from `left`.
    /// Returns `Bool`.
    fn synthesise_branch_or(&mut self, func: &RcExpr, left: &RcExpr, right: &RcExpr) -> Type {
        self.synthesise(func);
        self.synthesise(left);
        let facts = self.analyse_condition(left);
        self.narrowing.push(facts.negative);
        self.synthesise(right);
        self.narrowing.pop();
        Type::Bool
    }

    /// Synthesise `cond(clause_list)` with per-clause narrowing.
    ///
    /// Inspects the clause list statically when it is a list literal
    /// whose elements are `App(CLAUSE, [c, r])` pairs or a trailing default.
    /// Returns the union of all branch result types.
    ///
    /// When the clause list is not statically readable (a runtime-computed
    /// list), falls back to generic synthesis without narrowing.
    fn synthesise_branch_cond(&mut self, func: &RcExpr, clause_list: &RcExpr) -> Type {
        self.synthesise(func);

        // Try to read a static clause list.
        let clauses = match extract_clause_list(clause_list) {
            Some(cs) => cs,
            None => {
                // Not statically readable — synthesise the list arg normally.
                self.synthesise(clause_list);
                return Type::Any;
            }
        };

        // Per-row narrowing with accumulated negative-fact set (§A5.7).
        let mut acc = NarrowFrame::new();
        let mut result_types: Vec<Type> = Vec::new();

        for clause in &clauses {
            match clause {
                Clause::Case { condition, result } => {
                    // Synthesise condition under current accumulated negations.
                    self.narrowing.push(acc.clone());
                    self.synthesise(condition);
                    self.narrowing.pop();

                    // Derive facts from condition.
                    let facts = self.analyse_condition(condition);

                    // Synthesise result under positive facts + accumulated negations.
                    let mut branch_frame = facts.positive.clone();
                    branch_frame.merge_from(acc.clone());
                    self.narrowing.push(branch_frame);
                    let branch_type = self.synthesise(result);
                    self.narrowing.pop();

                    result_types.push(branch_type);

                    // Accumulate negative facts for subsequent clauses.
                    acc.merge_from(facts.negative);
                }
                Clause::Default(default_expr) => {
                    // Synthesise default under accumulated negations.
                    self.narrowing.push(acc.clone());
                    let default_type = self.synthesise(default_expr);
                    self.narrowing.pop();
                    result_types.push(default_type);
                }
            }
        }

        // Union of all branch and default result types.
        result_types.into_iter().fold(Type::Never, make_union_type)
    }

    /// Analyse a condition expression to derive narrowing facts.
    ///
    /// Returns a `ConditionFacts` with positive facts (hold when condition
    /// is true) and negative facts (hold when condition is false).
    /// Only bare variable references are narrowed — predicate applications
    /// to non-variables yield empty facts.
    fn analyse_condition(&self, condition: &RcExpr) -> ConditionFacts {
        match &*condition.inner {
            // `p?(x)` — predicate applied to a variable
            Expr::App(_, func, args) if args.len() == 1 => {
                if let Some(pred) = classify_predicate(func) {
                    if let Expr::Var(_, Var::Bound(bv)) = &*args[0].inner {
                        return self.facts_for_predicate(pred, bv);
                    }
                }
                ConditionFacts::empty()
            }
            // `not(c)` — invert facts
            Expr::App(_, func, args) if args.len() == 1 => {
                if is_not_func(func) {
                    return self.analyse_condition(&args[0]).negated();
                }
                ConditionFacts::empty()
            }
            _ => ConditionFacts::empty(),
        }
    }

    /// Derive narrowing facts for `predicate(variable)`.
    fn facts_for_predicate(&self, pred: PredicateKind, bv: &BoundVar) -> ConditionFacts {
        let name = match bv.name.as_deref() {
            Some(n) => n.to_string(),
            None => return ConditionFacts::empty(),
        };

        let idx = bv.scope as usize;
        // Only narrow in-scope variables (not prelude fall-throughs).
        if idx >= self.scope_stack.len() {
            return ConditionFacts::empty();
        }

        let anchored = self.scope_stack.len() - 1 - idx;
        let key = (anchored, name.clone());

        // Current type of the variable (without narrowing) — for subtraction.
        let current_ty = if let Some(frame) = self.scope_stack.get(idx) {
            if let Some(scheme) = frame.get(&name) {
                scheme.body.clone()
            } else {
                Type::Any
            }
        } else {
            Type::Any
        };

        let (positive_ty, negative_ty) = match pred {
            PredicateKind::Number => (Type::Number, subtract_type(&current_ty, &Type::Number)),
            PredicateKind::String => (Type::String, subtract_type(&current_ty, &Type::String)),
            PredicateKind::Symbol => (Type::Symbol, subtract_type(&current_ty, &Type::Symbol)),
            PredicateKind::Bool => (Type::Bool, subtract_type(&current_ty, &Type::Bool)),
            PredicateKind::List => (
                Type::List(Box::new(Type::Any)),
                subtract_type(&current_ty, &Type::List(Box::new(Type::Any))),
            ),
            PredicateKind::Block => (
                Type::Record {
                    fields: Default::default(),
                    open: true,
                    rows: vec![],
                },
                Type::Any,
            ),
            PredicateKind::Nil => (Type::Null, subtract_type(&current_ty, &Type::Null)),
            PredicateKind::NotNil => (subtract_type(&current_ty, &Type::Null), Type::Null),
        };

        let mut positive = NarrowFrame::new();
        let mut negative = NarrowFrame::new();

        if is_informative(&positive_ty) && !matches!(positive_ty, Type::Any) {
            positive.entries.insert(key.clone(), positive_ty);
        }
        if is_informative(&negative_ty) && !matches!(negative_ty, Type::Any) {
            negative.entries.insert(key, negative_ty);
        }

        ConditionFacts { positive, negative }
    }

    // ── Checking ─────────────────────────────────────────────────────────────

    /// Check `expr` against `expected`, emitting warnings on mismatch.
    pub fn check_against(&mut self, expr: &RcExpr, expected: &Type, smid: Smid) {
        if !is_informative(expected) {
            self.synthesise(expr);
            return;
        }

        // Lambda checking: bind parameter types when the target is known.
        if let (Expr::Lam(_, _, scope), Type::Function(param_type, result_type)) =
            (&*expr.inner, expected)
        {
            return self.check_lambda(scope, param_type, result_type);
        }

        // List literal against tuple: check each element against its position.
        if let (Expr::List(_, items), Type::Tuple(elem_types)) = (&*expr.inner, expected) {
            if items.len() == elem_types.len() {
                for (item, expected_elem) in items.iter().zip(elem_types.iter()) {
                    let item_smid = item.smid();
                    self.check_against(item, expected_elem, item_smid);
                }
                return;
            }
            // Length mismatch — fall through to synthesis path for error
        }

        let found = self.synthesise(expr);

        if !is_informative(&found) {
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
    /// Curries through the expected type for all parameters — a multi-param
    /// lambda `λ x y z. body` checked against `A -> B -> C -> D` binds
    /// `x : A`, `y : B`, `z : C`, and checks `body` against `D`.
    fn check_lambda(
        &mut self,
        scope: &crate::core::expr::LamScope<RcExpr>,
        param_type: &Type,
        result_type: &Type,
    ) {
        let mut frame: HashMap<String, TypeScheme> = HashMap::new();

        // Curry through the expected function type for all lambda parameters.
        // `remaining` tracks what the body is expected to produce.
        let mut remaining =
            Type::Function(Box::new(param_type.clone()), Box::new(result_type.clone()));

        for param in &scope.pattern {
            if let Type::Function(p, r) = remaining {
                frame.insert(param.clone(), TypeScheme::mono(*p));
                remaining = *r;
            } else {
                // More parameters than the function type accounts for — bind
                // the rest as `any` and leave the body unchecked.
                frame.insert(param.clone(), TypeScheme::mono(Type::Any));
            }
        }

        self.push_scope(frame);
        self.check_against(&scope.body, &remaining, scope.body.smid());
        self.pop_scope();
    }

    // ── Warning emission ─────────────────────────────────────────────────────

    fn emit_type_mismatch(&mut self, smid: Smid, expected: &Type, found: &Type, message: &str) {
        use super::types::humanise;
        let warning = TypeWarning::new(message)
            .at(smid)
            .with_types(humanise(expected).to_string(), humanise(found).to_string());
        self.warnings.push(warning);
    }
}

// ── Flow-sensitive narrowing — free functions ─────────────────────────────────

/// Classify a function expression as a branching construct.
///
/// Recognition is name-based for prelude-level bindings (scope index ≥
/// `stack_len` — i.e., the variable refers outside the checker's own
/// scope stack to a prelude binding) and intrinsic-based for `Expr::Intrinsic`
/// nodes.  Functions bound within the current scope (scope index < stack_len)
/// are *not* recognised as branchers — this implements AC6 (user rebinding
/// of `if` etc. disables narrowing).
///
/// Returns `None` when the function is not a recognised brancher.
fn classify_brancher(func: &RcExpr, stack_len: usize) -> Option<BranchKind> {
    match &*func.inner {
        // Mechanism 1: raw branch intrinsic nodes.
        Expr::Intrinsic(_, name) => match name.as_str() {
            "IF" => Some(BranchKind::If),
            "AND" => Some(BranchKind::And),
            "OR" => Some(BranchKind::Or),
            "COND" => Some(BranchKind::Cond),
            _ => None,
        },
        // Mechanism 2: bound variable referencing an outer (prelude) scope.
        // Only applies when the scope index falls *outside* the checker's
        // stack — that means the variable is from the prelude, not user code.
        Expr::Var(_, Var::Bound(bv)) => {
            let is_outer = (bv.scope as usize) >= stack_len;
            if !is_outer {
                return None;
            }
            match bv.name.as_deref() {
                Some("if") => Some(BranchKind::If),
                Some("and" | "&&") => Some(BranchKind::And),
                Some("or" | "||") => Some(BranchKind::Or),
                Some("cond") => Some(BranchKind::Cond),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Classify a function expression as a type predicate.
///
/// Recognised predicates are the standard type-test functions from the prelude.
/// Both intrinsic nodes (`IS_NUMBER`, …) and prelude-alias bound variables
/// (`number?`, …) are recognised.
fn classify_predicate(func: &RcExpr) -> Option<PredicateKind> {
    match &*func.inner {
        Expr::Intrinsic(_, name) => match name.as_str() {
            "IS_NUMBER" | "ISNUMBER" => Some(PredicateKind::Number),
            "IS_STRING" | "ISSTRING" => Some(PredicateKind::String),
            "IS_SYMBOL" | "ISSYMBOL" => Some(PredicateKind::Symbol),
            "IS_BOOL" | "ISBOOL" => Some(PredicateKind::Bool),
            "IS_LIST" | "ISLIST" => Some(PredicateKind::List),
            "IS_BLOCK" | "ISBLOCK" => Some(PredicateKind::Block),
            "IS_NIL" | "ISNIL" => Some(PredicateKind::Nil),
            _ => None,
        },
        Expr::Var(_, Var::Bound(bv)) => match bv.name.as_deref() {
            Some("number?") => Some(PredicateKind::Number),
            Some("string?") => Some(PredicateKind::String),
            Some("symbol?") => Some(PredicateKind::Symbol),
            Some("bool?") => Some(PredicateKind::Bool),
            Some("list?") => Some(PredicateKind::List),
            Some("block?") => Some(PredicateKind::Block),
            Some("nil?" | "null?") => Some(PredicateKind::Nil),
            Some("not-nil?") => Some(PredicateKind::NotNil),
            _ => None,
        },
        _ => None,
    }
}

/// Return `true` if `func` is the `not` function (boolean negation).
fn is_not_func(func: &RcExpr) -> bool {
    match &*func.inner {
        Expr::Intrinsic(_, name) => name == "NOT",
        Expr::Var(_, Var::Bound(bv)) => matches!(bv.name.as_deref(), Some("not" | "¬")),
        _ => false,
    }
}

/// Subtract `remove` from `ty`, returning the remaining type.
///
/// For union types, removes every variant that is a subtype of `remove`.
/// For singleton types equal to `remove`, returns `Never`.
/// For `any` or other opaque types, returns the original type unchanged
/// (the gradual boundary is preserved — §A5.10).
fn subtract_type(ty: &Type, remove: &Type) -> Type {
    match ty {
        Type::Union(variants) => {
            let remaining: Vec<Type> = variants
                .iter()
                .filter(|v| !is_subtype(v, remove))
                .cloned()
                .collect();
            match remaining.len() {
                0 => Type::Never,
                1 => remaining.into_iter().next().unwrap(),
                _ => Type::Union(remaining),
            }
        }
        // Exact match: subtracting the whole type gives Never.
        t if is_subtype(t, remove) => Type::Never,
        // Opaque or unrelated type: leave unchanged.
        t => t.clone(),
    }
}

/// Build a union type from two branch result types, deduplicating identical
/// members.  Returns `Never` when both are `Never`; returns the non-Never
/// side when only one is `Never`.
fn make_union_type(a: Type, b: Type) -> Type {
    match (a, b) {
        (Type::Never, b) => b,
        (a, Type::Never) => a,
        (ref a, ref b) if a == b => a.clone(),
        (Type::Union(mut vs), b) => {
            if !vs.contains(&b) {
                vs.push(b);
            }
            Type::Union(vs)
        }
        (a, Type::Union(mut vs)) => {
            if !vs.contains(&a) {
                vs.insert(0, a);
            }
            Type::Union(vs)
        }
        (a, b) => Type::Union(vec![a, b]),
    }
}

/// A parsed clause from a `cond` clause list.
enum Clause<'a> {
    /// `condition => result` — a `__CLAUSE(condition, result)` application.
    Case {
        condition: &'a RcExpr,
        result: &'a RcExpr,
    },
    /// Trailing bare expression (the default branch).
    Default(&'a RcExpr),
}

/// Try to extract a static clause list from a `cond(…)` argument.
///
/// Returns `Some(clauses)` when the argument is a list literal (or a small
/// tuple that is equivalent) whose elements are `__CLAUSE(c, r)` applications
/// or a trailing default value.  Returns `None` for runtime-computed lists.
fn extract_clause_list(expr: &RcExpr) -> Option<Vec<Clause<'_>>> {
    let items = match &*expr.inner {
        Expr::List(_, items) => items,
        _ => return None,
    };

    let mut clauses = Vec::new();
    for (i, item) in items.iter().enumerate() {
        match &*item.inner {
            Expr::App(_, func, args) if args.len() == 2 => {
                if is_clause_intrinsic(func) {
                    clauses.push(Clause::Case {
                        condition: &args[0],
                        result: &args[1],
                    });
                } else {
                    // Non-clause app in non-trailing position — fall back to generic.
                    if i + 1 < items.len() {
                        return None;
                    }
                    clauses.push(Clause::Default(item));
                }
            }
            _ => {
                // Bare expression: OK as trailing default.
                if i + 1 < items.len() {
                    // Non-trailing bare expression makes list unreadable.
                    return None;
                }
                clauses.push(Clause::Default(item));
            }
        }
    }
    Some(clauses)
}

/// Return `true` if `func` is the `CLAUSE` intrinsic or its prelude alias `=>`.
fn is_clause_intrinsic(func: &RcExpr) -> bool {
    match &*func.inner {
        Expr::Intrinsic(_, name) => name == "CLAUSE",
        Expr::Var(_, Var::Bound(bv)) => matches!(bv.name.as_deref(), Some("=>" | "⇒")),
        _ => false,
    }
}

// ── Helpers ─────────────────────────────────────────────────────────────────

/// Return `true` if `ty` contains a free `Type::Var` whose name matches `name`.
///
/// Used by `resolve_aliases_inner` to detect self-referential aliases and
/// decide whether to wrap the resolved body in `Type::Mu`.
fn contains_var_named(ty: &Type, name: &str) -> bool {
    match ty {
        Type::Var(id) => id.0 == name,
        Type::List(inner) | Type::IO(inner) | Type::Dict(inner) => contains_var_named(inner, name),
        Type::Function(a, b) | Type::Lens(a, b) | Type::Traversal(a, b) => {
            contains_var_named(a, name) || contains_var_named(b, name)
        }
        Type::Tuple(elems) => elems.iter().any(|e| contains_var_named(e, name)),
        Type::Record { fields, .. } => fields.values().any(|v| contains_var_named(v, name)),
        Type::Union(variants) => variants.iter().any(|v| contains_var_named(v, name)),
        // Mu: the binder name `x` is bound, not free — stop if it shadows `name`.
        Type::Mu(x, body) => x.0 != name && contains_var_named(body, name),
        _ => false,
    }
}

/// Synthesise the type for a primitive literal.
fn synthesise_primitive(prim: &Primitive) -> Type {
    match prim {
        Primitive::Num(_) => Type::Number,
        Primitive::Str(s) => Type::LiteralString(s.clone()),
        Primitive::Sym(name) => Type::LiteralSymbol(name.clone()),
        Primitive::Bool(_) => Type::Bool,
        Primitive::Null => Type::Null,
    }
}

/// Build the list element type from a `Vec` of synthesised element types.
///
/// - Empty list → `[never]`
/// - All same type → `[T]`
/// - Mixed types → `[T1 | T2 | … | Tn]` (deduplicated)
fn synthesise_list_type(types: Vec<Type>) -> Type {
    let informative: Vec<Type> = types.into_iter().filter(is_informative).collect();
    let elem_type = if informative.is_empty() {
        // Empty list is polymorphic — compatible with any element type.
        Type::Any
    } else {
        Type::union(informative)
    };
    Type::List(Box::new(elem_type))
}

/// Extract a string value from a core expression.
///
/// Handles two forms:
/// 1. `Literal(Str(s))` — a plain string literal.
/// 2. `App(JOIN, [List([str…]), sep])` — the desugared form of a eucalypt
///    string with `{{…}}` escapes (e.g. `"{{x: number}} -> number"`).
///    When every list element and the separator are string literals, the join
///    is evaluated at compile time and the result is returned.
fn extract_string_literal(expr: &RcExpr) -> Option<String> {
    // Plain string literal — fast path.
    if let Expr::Literal(_, Primitive::Str(s)) = &*expr.inner {
        return Some(s.clone());
    }

    // JOIN([chunk, …], sep) — produced by the desugarer for interpolated
    // strings.  Evaluate it statically when all chunks are string literals.
    if let Expr::App(_, func, args) = &*expr.inner {
        if let Expr::Intrinsic(_, name) = &*func.inner {
            if name == "JOIN" && args.len() == 2 {
                let sep = extract_plain_str(&args[1])?;
                if let Expr::List(_, items) = &*args[0].inner {
                    let parts: Option<Vec<String>> = items.iter().map(extract_plain_str).collect();
                    return parts.map(|ps| ps.join(&sep));
                }
            }
        }
    }

    None
}

/// Extract a plain string literal (no interpolation) from a core expression.
fn extract_plain_str(expr: &RcExpr) -> Option<String> {
    if let Expr::Literal(_, Primitive::Str(s)) = &*expr.inner {
        Some(s.clone())
    } else {
        None
    }
}

/// Returns `true` when `ty` carries useful information for type checking.
///
/// `any` and `never` are uninformative:
/// - `any` is the gradual boundary — it suppresses warnings.
/// - `never` represents empty or unreachable code.
fn is_informative(ty: &Type) -> bool {
    !matches!(ty, Type::Any | Type::Never)
}

/// Build the type-mismatch warning message for a single-argument call.
///
/// When `func_name` is available, the message names the function so that the
/// warning is self-contained even without reading the source snippet.
fn build_arg_mismatch_message(func_name: Option<&str>) -> String {
    match func_name {
        Some(name) => format!("type mismatch calling '{name}'"),
        None => "argument type does not match function parameter".to_string(),
    }
}

/// Build the type-mismatch warning message for an overloaded call.
///
/// When `func_name` is available, the message names the function.
fn build_overload_mismatch_message(func_name: Option<&str>) -> String {
    match func_name {
        Some(name) => format!("type mismatch calling '{name}': no matching overload"),
        None => "argument type does not match any overload".to_string(),
    }
}

// ── Public entry point ───────────────────────────────────────────────────────

/// Result of running the type checker: warnings and the inferred type environment.
pub struct TypeCheckResult {
    /// Type warnings (mismatches, missing fields, etc.).
    pub warnings: Vec<TypeWarning>,
    /// Flattened type environment mapping binding names to their inferred types.
    pub types: HashMap<String, Type>,
    /// Lambda parameter types inferred during type checking.
    ///
    /// Maps `Smid` (source location of the lambda) → list of
    /// `(param_name, inferred_type)` pairs.  Keyed by Smid to avoid
    /// name collisions between different lambdas with the same param
    /// names.
    pub lambda_params: HashMap<Smid, Vec<(String, Type)>>,
}

/// Run the type checker over `expr` and return all warnings found.
pub fn type_check(expr: &RcExpr) -> Vec<TypeWarning> {
    let mut checker = Checker::new();
    checker.check_expr(expr);
    checker.into_warnings()
}

/// Run the type checker over `expr` and return warnings plus the type environment.
pub fn type_check_full(expr: &RcExpr) -> TypeCheckResult {
    let mut checker = Checker::new();
    checker.check_expr(expr);
    checker.into_results()
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::expr::core;
    use crate::core::typecheck::types::TypeVarId;

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

    fn mono(ty: Type) -> TypeScheme {
        TypeScheme::mono(ty)
    }

    // ── Literal synthesis ───────────────────────────────────────────────────

    #[test]
    fn synthesise_number_literal() {
        let mut c = Checker::new();
        assert_eq!(c.synthesise(&num_lit(42)), Type::Number);
    }

    #[test]
    fn synthesise_string_literal() {
        // String literals now synthesise their literal type (LiteralString),
        // not the base String type — consistent with how symbols work.
        let mut c = Checker::new();
        assert_eq!(
            c.synthesise(&str_lit("hello")),
            Type::LiteralString("hello".to_string())
        );
    }

    #[test]
    fn synthesise_symbol_literal() {
        let mut c = Checker::new();
        assert_eq!(
            c.synthesise(&sym_lit("foo")),
            Type::LiteralSymbol("foo".to_string())
        );
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
    fn empty_list_is_polymorphic() {
        let mut c = Checker::new();
        assert_eq!(c.synthesise(&list(vec![])), Type::List(Box::new(Type::Any)));
    }

    #[test]
    fn small_list_synthesises_as_tuple() {
        let mut c = Checker::new();
        // 2-4 element lists synthesise as tuples (more informative).
        // String literals now synthesise as LiteralString.
        let l2 = list(vec![num_lit(1), str_lit("hello")]);
        assert_eq!(
            c.synthesise(&l2),
            Type::Tuple(vec![Type::Number, Type::LiteralString("hello".to_string())])
        );
        let l3 = list(vec![num_lit(1), num_lit(2), num_lit(3)]);
        assert_eq!(
            c.synthesise(&l3),
            Type::Tuple(vec![Type::Number, Type::Number, Type::Number])
        );
    }

    #[test]
    fn large_list_synthesises_as_list() {
        let mut c = Checker::new();
        // 5+ element lists synthesise as homogeneous lists
        let l = list(vec![
            num_lit(1),
            num_lit(2),
            num_lit(3),
            num_lit(4),
            num_lit(5),
        ]);
        assert_eq!(c.synthesise(&l), Type::List(Box::new(Type::Number)));
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
        let expr = meta_with_type(num_lit(1), "number");
        assert_eq!(c.synthesise(&expr), Type::Number);
    }

    #[test]
    fn meta_with_wrong_annotation_emits_warning() {
        let mut c = Checker::new();
        let expr = meta_with_type(str_lit("hello"), "number");
        c.synthesise(&expr);
        let warnings = c.into_warnings();
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].expected.as_deref() == Some("number"));
        // String literals now synthesise as LiteralString — display as "\"hello\"".
        assert!(warnings[0].found.as_deref() == Some("\"hello\""));
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
        assert_eq!(c.synthesise(&expr), Type::Number);
    }

    /// Regression: synthesise_meta heterogeneous-tuple branch must route
    /// through Type::union (not Type::Union directly) so that a literal
    /// string mixed with the base String type is absorbed to just `string`.
    ///
    /// Before the fix the element type was `LiteralString("a") | string`
    /// (un-normalised).  After the fix Type::union absorbs the literal into
    /// its base type, yielding `string`.
    #[test]
    fn meta_type_hint_heterogeneous_tuple_normalises_union() {
        let mut c = Checker::new();
        // Build [str_lit("a"), x_annotated_string] where the second element
        // is annotated `type: "string"` so it synthesises as base String.
        // The resulting list has tuple type (LiteralString("a"), String).
        let elem_a = str_lit("a");
        let elem_b = meta_with_type(str_lit("b"), "string"); // synthesises String
        let lst = list(vec![elem_a, elem_b]);
        // Wrap in __type_hint to trigger the is_hint code path.
        let expr = meta_with_hint(lst, "[string]");
        // Must be List(String), not List(LiteralString("a") | String).
        assert_eq!(c.synthesise(&expr), Type::List(Box::new(Type::String)));
    }

    #[test]
    fn user_type_annotation_takes_priority_over_hint() {
        let mut c = Checker::new();
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

    // ── Application checking ─────────────────────────────────────────────────

    #[test]
    fn app_with_correct_arg_no_warning() {
        let mut c = Checker::new();
        let mut frame = HashMap::new();
        frame.insert(
            "double".to_string(),
            mono(Type::Function(
                Box::new(Type::Number),
                Box::new(Type::Number),
            )),
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
        let mut frame = HashMap::new();
        frame.insert(
            "double".to_string(),
            mono(Type::Function(
                Box::new(Type::Number),
                Box::new(Type::Number),
            )),
        );
        c.push_scope(frame);

        let double_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("double".to_string())));
        let app = RcExpr::from(Expr::App(
            Smid::default(),
            double_var,
            vec![str_lit("oops")],
        ));

        c.synthesise(&app);
        let warnings = c.into_warnings();
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].expected.as_deref(), Some("number"));
        // String literal "oops" synthesises as LiteralString.
        assert_eq!(warnings[0].found.as_deref(), Some("\"oops\""));
    }

    #[test]
    fn app_returns_result_type_of_function() {
        let mut c = Checker::new();
        let mut frame = HashMap::new();
        frame.insert(
            "str_of".to_string(),
            mono(Type::Function(
                Box::new(Type::Number),
                Box::new(Type::String),
            )),
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
        let func_var = RcExpr::from(Expr::Var(
            Smid::default(),
            Var::Free("unknown_fn".to_string()),
        ));
        let app = RcExpr::from(Expr::App(Smid::default(), func_var, vec![str_lit("hello")]));

        c.synthesise(&app);
        assert!(c.into_warnings().is_empty());
    }

    // ── Polymorphic application ──────────────────────────────────────────────

    #[test]
    fn poly_identity_applied_to_number_returns_number() {
        let mut c = Checker::new();
        // identity : forall a. a -> a
        let scheme = TypeScheme::poly(
            vec![TypeVarId("a".to_string())],
            Type::Function(
                Box::new(Type::Var(TypeVarId("a".to_string()))),
                Box::new(Type::Var(TypeVarId("a".to_string()))),
            ),
        );
        let mut frame = HashMap::new();
        frame.insert("identity".to_string(), scheme);
        c.push_scope(frame);

        let id_var = RcExpr::from(Expr::Var(
            Smid::default(),
            Var::Free("identity".to_string()),
        ));
        let app = RcExpr::from(Expr::App(Smid::default(), id_var, vec![num_lit(1)]));

        let result = c.synthesise(&app);
        assert_eq!(result, Type::Number);
        assert!(c.into_warnings().is_empty());
    }

    #[test]
    fn poly_map_applied_correctly_no_warning() {
        let mut c = Checker::new();
        // map : forall a b. (a -> b) -> [a] -> [b]
        let a = TypeVarId("a".to_string());
        let b = TypeVarId("b".to_string());
        let scheme = TypeScheme::poly(
            vec![a.clone(), b.clone()],
            Type::Function(
                Box::new(Type::Function(
                    Box::new(Type::Var(a.clone())),
                    Box::new(Type::Var(b.clone())),
                )),
                Box::new(Type::Function(
                    Box::new(Type::List(Box::new(Type::Var(a.clone())))),
                    Box::new(Type::List(Box::new(Type::Var(b.clone())))),
                )),
            ),
        );
        let mut frame = HashMap::new();
        frame.insert("map".to_string(), scheme);
        // double : number -> number
        frame.insert(
            "double".to_string(),
            mono(Type::Function(
                Box::new(Type::Number),
                Box::new(Type::Number),
            )),
        );
        c.push_scope(frame);

        // map(double, [1, 2, 3])
        let map_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("map".to_string())));
        let double_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("double".to_string())));
        let nums = list(vec![num_lit(1), num_lit(2), num_lit(3)]);
        let app = RcExpr::from(Expr::App(Smid::default(), map_var, vec![double_var, nums]));

        let result = c.synthesise(&app);
        assert_eq!(result, Type::List(Box::new(Type::Number)));
        assert!(c.into_warnings().is_empty());
    }

    #[test]
    fn poly_map_applied_with_wrong_list_type_emits_warning() {
        let mut c = Checker::new();
        // map : forall a b. (a -> b) -> [a] -> [b]
        let a = TypeVarId("a".to_string());
        let b = TypeVarId("b".to_string());
        let scheme = TypeScheme::poly(
            vec![a.clone(), b.clone()],
            Type::Function(
                Box::new(Type::Function(
                    Box::new(Type::Var(a.clone())),
                    Box::new(Type::Var(b.clone())),
                )),
                Box::new(Type::Function(
                    Box::new(Type::List(Box::new(Type::Var(a.clone())))),
                    Box::new(Type::List(Box::new(Type::Var(b.clone())))),
                )),
            ),
        );
        let mut frame = HashMap::new();
        frame.insert("map".to_string(), scheme);
        // double : number -> number
        frame.insert(
            "double".to_string(),
            mono(Type::Function(
                Box::new(Type::Number),
                Box::new(Type::Number),
            )),
        );
        c.push_scope(frame);

        // map(double, ["a", "b"]) — list type mismatch: [number] expected, [string] found
        let map_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("map".to_string())));
        let double_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("double".to_string())));
        let strs = list(vec![str_lit("a"), str_lit("b")]);
        let app = RcExpr::from(Expr::App(Smid::default(), map_var, vec![double_var, strs]));

        c.synthesise(&app);
        let warnings = c.into_warnings();
        assert_eq!(warnings.len(), 1, "expected one mismatch warning");
    }

    // ── Overloaded operators (union type) ────────────────────────────────────

    #[test]
    fn union_typed_plus_with_numbers_no_warning() {
        let mut c = Checker::new();
        // + : (number -> number -> number) | (string -> string -> string)
        let plus_type = Type::Union(vec![
            Type::Function(
                Box::new(Type::Number),
                Box::new(Type::Function(
                    Box::new(Type::Number),
                    Box::new(Type::Number),
                )),
            ),
            Type::Function(
                Box::new(Type::String),
                Box::new(Type::Function(
                    Box::new(Type::String),
                    Box::new(Type::String),
                )),
            ),
        ]);
        let mut frame = HashMap::new();
        frame.insert("add".to_string(), mono(plus_type));
        c.push_scope(frame);

        let add_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("add".to_string())));
        let app = RcExpr::from(Expr::App(
            Smid::default(),
            add_var,
            vec![num_lit(1), num_lit(2)],
        ));

        let result = c.synthesise(&app);
        assert_eq!(result, Type::Number);
        assert!(c.into_warnings().is_empty());
    }

    #[test]
    fn union_typed_plus_with_mixed_types_emits_warning() {
        let mut c = Checker::new();
        let plus_type = Type::Union(vec![
            Type::Function(
                Box::new(Type::Number),
                Box::new(Type::Function(
                    Box::new(Type::Number),
                    Box::new(Type::Number),
                )),
            ),
            Type::Function(
                Box::new(Type::String),
                Box::new(Type::Function(
                    Box::new(Type::String),
                    Box::new(Type::String),
                )),
            ),
        ]);
        let mut frame = HashMap::new();
        frame.insert("add".to_string(), mono(plus_type));
        c.push_scope(frame);

        // add(1, "hello") — string arg fails all overloads for the second arg
        // after the first arg selects the number branch
        let add_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("add".to_string())));
        let app = RcExpr::from(Expr::App(
            Smid::default(),
            add_var,
            vec![num_lit(1), str_lit("hello")],
        ));

        c.synthesise(&app);
        let warnings = c.into_warnings();
        assert!(!warnings.is_empty(), "expected a type mismatch warning");
    }

    // ── Let binding ──────────────────────────────────────────────────────────

    #[test]
    fn let_binding_seeds_scope_for_body() {
        let mut c = Checker::new();

        let double_meta = meta_with_type(
            RcExpr::from(Expr::Intrinsic(Smid::default(), "__DOUBLE".to_string())),
            "number -> number",
        );

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
        let x_free = RcExpr::from(Expr::Var(Smid::default(), Var::Free("x".to_string())));
        let scope = close_lam_scope(vec!["x".to_string()], x_free);
        let lam = RcExpr::from(Expr::Lam(Smid::default(), false, scope));

        let fn_type = Type::Function(Box::new(Type::Number), Box::new(Type::Number));
        c.check_against(&lam, &fn_type, Smid::default());
        assert!(c.into_warnings().is_empty());
    }

    #[test]
    fn multi_param_lambda_checked_correctly() {
        use crate::core::expr::close_lam_scope;

        let mut c = Checker::new();

        // λ x y. x — checked against `number -> string -> number`
        // y is bound but unused; body returns x which is number.
        let x_free = RcExpr::from(Expr::Var(Smid::default(), Var::Free("x".to_string())));
        let scope = close_lam_scope(vec!["x".to_string(), "y".to_string()], x_free);
        let lam = RcExpr::from(Expr::Lam(Smid::default(), false, scope));

        let fn_type = Type::Function(
            Box::new(Type::Number),
            Box::new(Type::Function(
                Box::new(Type::String),
                Box::new(Type::Number),
            )),
        );
        c.check_against(&lam, &fn_type, Smid::default());
        assert!(c.into_warnings().is_empty());
    }

    // ── Pipeline / catenation style ──────────────────────────────────────────

    #[test]
    fn catenation_type_flows_left_to_right() {
        let mut c = Checker::new();
        let mut frame = HashMap::new();
        frame.insert(
            "double".to_string(),
            mono(Type::Function(
                Box::new(Type::Number),
                Box::new(Type::Number),
            )),
        );
        frame.insert(
            "str_of".to_string(),
            mono(Type::Function(
                Box::new(Type::Number),
                Box::new(Type::String),
            )),
        );
        c.push_scope(frame);

        let double_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("double".to_string())));
        let str_of_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("str_of".to_string())));

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
        let mut frame = HashMap::new();
        frame.insert(
            "f".to_string(),
            mono(Type::Function(Box::new(Type::Any), Box::new(Type::String))),
        );
        c.push_scope(frame);

        let f_var = RcExpr::from(Expr::Var(Smid::default(), Var::Free("f".to_string())));
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
            mono(Type::Function(
                Box::new(Type::Number),
                Box::new(Type::String),
            )),
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
                    // String literals synthesise as LiteralString.
                    m.insert("name".to_string(), Type::LiteralString("Alice".to_string()));
                    m
                },
                open: true,
                rows: vec![],
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
                rows: vec![],
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
            mono(Type::Record {
                fields: {
                    let mut m = std::collections::BTreeMap::new();
                    m.insert("name".to_string(), Type::String);
                    m.insert("age".to_string(), Type::Number);
                    m
                },
                open: true,
                rows: vec![],
            }),
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
            mono(Type::Record {
                fields: std::collections::BTreeMap::new(),
                open: true,
                rows: vec![],
            }),
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
            mono(Type::Record {
                fields: {
                    let mut m = std::collections::BTreeMap::new();
                    m.insert("x".to_string(), Type::Number);
                    m
                },
                open: false,
                rows: vec![],
            }),
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

    // ── Bound variable scope resolution ──────────────────────────────────────

    #[test]
    fn bound_var_resolves_to_correct_scope_when_shadowed() {
        // Simulates the arr.map scenario: outer scope has `map: (a→b)→[a]→[b]`,
        // inner scope shadows it with `map: (number→number)→array→array`.
        // A BoundVar at scope 1 (outer) should get the outer type, not the
        // inner shadow.
        let mut c = Checker::new();

        // Outer scope: list map
        let list_map_type = Type::Function(
            Box::new(Type::Function(
                Box::new(Type::Var(TypeVarId("a".into()))),
                Box::new(Type::Var(TypeVarId("b".into()))),
            )),
            Box::new(Type::Function(
                Box::new(Type::List(Box::new(Type::Var(TypeVarId("a".into()))))),
                Box::new(Type::List(Box::new(Type::Var(TypeVarId("b".into()))))),
            )),
        );
        let mut outer = HashMap::new();
        outer.insert("map".to_string(), TypeScheme::mono(list_map_type.clone()));
        c.push_scope(outer);

        // Inner scope: arr.map (narrower type)
        let arr_map_type = Type::Function(
            Box::new(Type::Function(
                Box::new(Type::Number),
                Box::new(Type::Number),
            )),
            Box::new(Type::Function(Box::new(Type::Array), Box::new(Type::Array))),
        );
        let mut inner = HashMap::new();
        inner.insert("map".to_string(), TypeScheme::mono(arr_map_type.clone()));
        c.push_scope(inner);

        // BoundVar at scope 0 (inner) → arr.map type
        let bv_inner = BoundVar {
            scope: 0,
            binder: 0,
            name: Some("map".to_string()),
        };
        let inner_result = c.lookup_bound(&bv_inner);
        assert_eq!(
            inner_result, arr_map_type,
            "scope 0 should resolve to inner (arr.map)"
        );

        // BoundVar at scope 1 (outer) → list map type
        let bv_outer = BoundVar {
            scope: 1,
            binder: 0,
            name: Some("map".to_string()),
        };
        let outer_result = c.lookup_bound(&bv_outer);
        assert_eq!(
            outer_result, list_map_type,
            "scope 1 should resolve to outer (list map)"
        );

        // Name-based lookup would find the inner shadow — verify they differ.
        let name_result = c.lookup_name("map");
        assert_eq!(name_result, arr_map_type, "name lookup finds innermost");
        assert_ne!(
            name_result, list_map_type,
            "name lookup does NOT find outer — this is the bug that lookup_bound fixes"
        );
    }

    #[test]
    fn bound_var_beyond_stack_falls_back_to_name_lookup() {
        // BoundVar referencing a scope outside the checker's stack (e.g. prelude
        // globals) should fall back to name-based lookup.
        let mut c = Checker::new();

        let mut frame = HashMap::new();
        frame.insert("x".to_string(), mono(Type::Number));
        c.push_scope(frame);

        // scope 5 is well beyond the stack (depth 1)
        let bv = BoundVar {
            scope: 5,
            binder: 0,
            name: Some("x".to_string()),
        };
        // Falls back to name lookup, which finds "x" in the single frame.
        let result = c.lookup_bound(&bv);
        assert_eq!(result, Type::Number);
    }
}
