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
            types::{Constraint, Kind, Type, TypeScheme, TypeVarId},
            unify::{
                apply_subst, freshen, freshen_forall, freshen_with_constraints, infer_scheme,
                unify, Substitution,
            },
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
#[derive(Debug, Clone, Copy)]
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

/// The structural shape of a recognised branch combinator.
///
/// Records the arity, condition parameter index, and branch parameter
/// indices so that `synthesise_branch` can extract the right arguments
/// from a flattened application spine.  Constructed once per binding and
/// memoised.
///
/// For `BranchKind::Cond`, `condition` is unused; `branches[0]` is the
/// clause-list argument index.
#[derive(Debug, Clone)]
pub struct BranchShape {
    /// Total number of arguments when fully applied.
    arity: usize,
    /// Index into the flattened argument list for the condition expression.
    condition: usize,
    /// Indices into the flattened argument list for the branch arms.
    branches: Vec<usize>,
    /// Semantics — determines how narrowing is applied.
    kind: BranchKind,
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
    /// `nil?` — empty-list check (`= []`).
    /// Positive branch: no structural narrowing (empty is `[a]`).
    /// Negative branch: `List(a) → NonEmpty(a)` (§A6.4).
    Nil,
    /// `non-nil?` — non-empty list check.
    /// Positive branch: `List(a) → NonEmpty(a)` (§A6.4).
    /// Negative branch: no structural narrowing.
    NonNil,
    /// `null?` — null check (`= null`).
    /// Positive branch: `Type::Null`.
    /// Negative branch: subtract `Null` from union.
    Null,
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

// ── Prelude summary ──────────────────────────────────────────────────────────

/// Cached result of type-checking the prelude once per process.
///
/// Produced by running the prelude through the full pipeline
/// (load → translate → cook, **no eliminate**), then running
/// `type_check_for_prelude` which preserves the root scope after checking.
///
/// Seeded into fresh `Checker` instances via `Checker::with_seed` when
/// checking user files, avoiding a full re-walk of the ~2 200-line
/// prelude on every check.  User files still go through the full
/// pipeline with all their imports — only the prelude is cached.
#[derive(Clone, Default)]
pub struct PreludeSummary {
    /// Exported binding name → its type scheme.
    ///
    /// Polymorphic schemes are preserved so that every use-site freshens
    /// the type variables independently.
    pub bindings: HashMap<String, TypeScheme>,

    /// Type aliases the prelude registers (from `type-def:` / `types:`
    /// metadata).  User-file annotations can reference these alias names.
    pub aliases: AliasMap,

    /// BranchShape classification for recognised brancher combinators,
    /// keyed by binding *name* (not `BoundKey`) for `Var::Free` lookup.
    ///
    /// With prelude caching, prelude functions appear as `Var::Free` in user
    /// code.  `recognise_brancher` consults this map when the head is a
    /// free variable so that `if`, `then`, `and`, etc. are still recognised.
    pub branch_shapes: HashMap<String, BranchShape>,

    /// Pre-cook operator type overloads for constraint discharge (§B2).
    ///
    /// Operator definitions (e.g. `(l < r): __LT(l, r)`) have their `Meta`
    /// wrapper stripped by cook, making their type annotations invisible to
    /// the type checker.  This map captures the annotations before cook and
    /// allows `discharge_constraint` to verify operator constraints even when
    /// the scope-stack entry for the operator is `Any`.
    pub operator_overloads: HashMap<String, TypeScheme>,
}

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

    /// Binding bodies for structural BranchShape classification (§A5.3).
    ///
    /// Populated eagerly during Let synthesis: for each binding `(name, val)`,
    /// the metadata-peeled expression is stored under the binding's `BoundKey`.
    /// Consulted during `recognise_brancher` to classify wrapper functions
    /// such as `then(t,f,c): if(c,t,f)` and user-defined equivalents.
    binding_bodies: HashMap<BoundKey, RcExpr>,

    /// Memoised BranchShape classification per binding (§A5.3).
    ///
    /// `None` = classified, not a brancher.
    /// `Some(shape)` = recognised brancher with this shape.
    ///
    /// Populated eagerly during Let synthesis alongside `binding_bodies`.
    /// The `None` entry also serves as a recursion guard: a binding already
    /// inserted as `None` (tentative) is treated as "not a brancher" if
    /// re-encountered during its own classification.
    branch_shapes: HashMap<BoundKey, Option<BranchShape>>,

    /// Memoised ProjectionShape classification per binding (§B6.3).
    ///
    /// `None` = classified, not a projection.
    /// `Some(index)` = tuple-element projector at position `index`.
    ///   index 0 = first element (head), index 1 = second (head∘tail), etc.
    ///
    /// Populated eagerly during Let synthesis alongside `binding_bodies`.
    projection_shapes: HashMap<BoundKey, Option<usize>>,

    /// When `true`, the outermost scope frame is preserved rather than
    /// popped when `synthesise(Let)` would otherwise call `pop_scope`.
    ///
    /// Set during prelude cache building so that
    /// `extract_prelude_summary` can read the root frame after
    /// `check_expr` returns.  Always `false` in user-file checks.
    keep_root_scope: bool,

    /// Pre-cook operator overload types for constraint discharge (§B2).
    ///
    /// Populated from type annotations extracted from the raw (pre-cook)
    /// expression before `distribute_fixities` strips `Meta` wrappers from
    /// operator definitions.  Cook strips these wrappers, leaving operators
    /// with `mono(any)` in the scope stack.  `discharge_constraint` consults
    /// this map as a fallback when the scope lookup returns `Any`.
    ///
    /// Keyed by operator name (e.g. `"<"`, `">"`, `"+"`).
    operator_overloads: HashMap<String, TypeScheme>,

    /// BranchShape classification seeded from a pre-checked prelude (§B7).
    ///
    /// Maps prelude binding name → `BranchShape`.  Consulted by
    /// `recognise_brancher` and `classify_lam_body` when the head
    /// expression is a `Var::Free` in prelude-cached user-file checks.
    seed_branch_shapes: HashMap<String, BranchShape>,
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
            binding_bodies: HashMap::new(),
            branch_shapes: HashMap::new(),
            projection_shapes: HashMap::new(),
            keep_root_scope: false,
            seed_branch_shapes: HashMap::new(),
            operator_overloads: HashMap::new(),
        }
    }

    /// Create a checker pre-seeded with a prelude summary (§B7).
    ///
    /// Installs the prelude's binding type schemes into the outermost
    /// scope frame (pushed at the back of `scope_stack` so that user
    /// bindings pushed at the front shadow them), pre-populates the
    /// alias map, and seeds `seed_branch_shapes` for `Var::Free`
    /// recognition of prelude combinators.
    ///
    /// Used when checking a user file with the prelude cache — the
    /// user file's own Let frame is pushed on top during normal
    /// synthesis, and `Var::Free` references to prelude names resolve
    /// via the `lookup_bound` → `lookup_name` fallback.
    pub fn with_seed(summary: &PreludeSummary) -> Self {
        let mut checker = Checker::new();
        checker.aliases = summary.aliases.clone();
        checker.seed_branch_shapes = summary.branch_shapes.clone();
        checker.operator_overloads = summary.operator_overloads.clone();
        // Push prelude bindings at the back (outermost) so user-file
        // frames pushed via push_front sit in front and shadow them.
        let frame: HashMap<String, TypeScheme> = summary.bindings.clone();
        checker.scope_stack.push_back(frame);
        checker
    }

    /// Extract a `PreludeSummary` from the current checker state.
    ///
    /// Intended to be called after `check_expr` when `keep_root_scope`
    /// was `true`, ensuring the root scope frame is still present in
    /// `scope_stack`.
    pub fn extract_prelude_summary(&self) -> PreludeSummary {
        // Collect type schemes from all frames (there should be exactly
        // one frame — the root prelude Let frame — when keep_root_scope).
        let bindings: HashMap<String, TypeScheme> = self
            .scope_stack
            .iter()
            .flat_map(|frame| frame.iter().map(|(k, v)| (k.clone(), v.clone())))
            .collect();

        // Extract named branch shapes, discarding the BoundKey's
        // anchored-index component — only the name matters for Free-var
        // lookups during prelude-cached user-file checks.
        let branch_shapes: HashMap<String, BranchShape> = self
            .branch_shapes
            .iter()
            .filter_map(|((_, name), opt_shape)| {
                opt_shape.as_ref().map(|s| (name.clone(), s.clone()))
            })
            .collect();

        PreludeSummary {
            bindings,
            aliases: self.aliases.clone(),
            branch_shapes,
            operator_overloads: self.operator_overloads.clone(),
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
            aliases: self.aliases,
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
        // When keep_root_scope is set, preserve the last remaining frame so
        // that extract_prelude_summary can read it after check_expr returns.
        if self.keep_root_scope && self.scope_stack.len() == 1 {
            return;
        }
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

    // ── Constraint support (B2) ──────────────────────────────────────────────

    /// Look up the raw (unfrenshened) `TypeScheme` for a simple named
    /// variable expression without consuming `self.var_counter`.
    ///
    /// Returns `None` when the expression is not a simple variable (e.g. it is
    /// a nested application or a literal), or when the name is not in scope.
    fn lookup_scheme_for_expr(&self, expr: &RcExpr) -> Option<TypeScheme> {
        match &*expr.inner {
            Expr::Var(_, Var::Free(name)) | Expr::Name(_, name) => self
                .scope_stack
                .iter()
                .find_map(|frame| frame.get(name))
                .cloned(),
            Expr::Var(_, Var::Bound(bv)) => {
                let name = bv.name.as_deref()?;
                let idx = bv.scope as usize;
                if let Some(frame) = self.scope_stack.get(idx) {
                    frame.get(name).cloned()
                } else {
                    // Out-of-range scope index — fall back to name search.
                    self.scope_stack.iter().find_map(|f| f.get(name)).cloned()
                }
            }
            _ => None,
        }
    }

    /// Discharge an operator constraint after all arguments have been applied.
    ///
    /// A constraint `op(T₁, …, Tₙ)` is discharged as follows:
    ///
    /// - If any `Tᵢ` (after applying `subst`) is `any`: vacuously satisfied.
    /// - If any `Tᵢ` is still an unbound type variable: the constraint is not
    ///   yet concrete — skip silently (constrained polymorphism; the outer
    ///   scheme carries it forward).
    /// - Otherwise: look up `op`'s declared type and check that some overload
    ///   accepts `(T₁, …, Tₙ)`.  Emit a warning if none match.
    fn discharge_constraint(&mut self, constraint: &Constraint, subst: &Substitution, smid: Smid) {
        let resolved: Vec<Type> = constraint
            .args
            .iter()
            .map(|a| apply_subst(a, subst))
            .collect();

        // Gradual: `any` arg means we cannot say anything — stay silent.
        if resolved.iter().any(|a| matches!(a, Type::Any)) {
            return;
        }

        // Not yet concrete — constraint propagates.
        if resolved.iter().any(|a| matches!(a, Type::Var(_, _))) {
            return;
        }

        // Look up the operator's declared type (body without freshening).
        // First check the scope stack; if the scope gives `Any` (because cook
        // stripped the operator's Meta annotation), fall back to the pre-cook
        // operator overload map which was populated before distribute_fixities.
        let op_type_from_scope = self
            .scope_stack
            .iter()
            .find_map(|f| f.get(&constraint.function))
            .cloned()
            .map(|s| s.body.clone());

        let op_type = match op_type_from_scope {
            Some(Type::Any) | None => {
                // Scope has no useful type — try the pre-cook overload registry.
                self.operator_overloads
                    .get(&constraint.function)
                    .map(|s| s.body.clone())
            }
            other => other,
        };

        let Some(op_type) = op_type else {
            // Unknown operator — cannot verify, stay silent (gradual).
            return;
        };

        // Gradual: if the overload itself is Any, stay silent.
        if matches!(op_type, Type::Any) {
            return;
        }

        // Check whether any overload of the operator accepts the resolved args.
        if !constraint_overload_matches(&op_type, &resolved) {
            let args_str = resolved
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            let warning = TypeWarning::new(format!(
                "`{}` does not accept ({})",
                constraint.function, args_str
            ))
            .at(smid);
            self.warnings.push(warning);
        }
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
            Type::Var(ref v, _) => {
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
            Type::App(f, x) => Type::App(
                Box::new(self.resolve_aliases_inner(*f, resolving)),
                Box::new(self.resolve_aliases_inner(*x, resolving)),
            ),
            Type::Con(_) => ty,
            Type::Forall(binders, body) => Type::Forall(
                binders,
                Box::new(self.resolve_aliases_inner(*body, resolving)),
            ),
            Type::Tuple(elems) => Type::Tuple(
                elems
                    .into_iter()
                    .map(|e| self.resolve_aliases_inner(e, resolving))
                    .collect(),
            ),
            Type::Function(a, b) => Type::Function(
                Box::new(self.resolve_aliases_inner(*a, resolving)),
                Box::new(self.resolve_aliases_inner(*b, resolving)),
            ),
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

        // In source, `types: { Point: "string" }` desugars the inner block to
        // `let Point = "string" in { Point: Point }`.  Walk the Let chain to
        // collect bindings, then fall back to direct Block iteration for
        // expressions that were constructed directly (e.g. in unit tests).
        self.register_aliases_from_types_expr(types_expr);
    }

    /// Extract alias entries from a `types:` value expression.
    ///
    /// Handles both:
    /// - Desugared form: `let Name = "type" in { … }` (normal source code)
    /// - Direct block form: `{ Name: "type" }` (unit-test core construction)
    fn register_aliases_from_types_expr(&mut self, expr: &RcExpr) {
        match &*expr.inner {
            Expr::Let(_, scope, _) => {
                // Each binding in the scope corresponds to one alias declaration.
                for (alias_name, type_str_expr) in &scope.pattern {
                    if let Some(type_str) = extract_string_literal(type_str_expr) {
                        if let Ok(ty) = parse::parse_type(&type_str) {
                            self.register_alias(alias_name.clone(), ty);
                        }
                    }
                }
                // Continue into the body in case of nested let-chains.
                self.register_aliases_from_types_expr(&scope.body);
            }
            Expr::Block(_, b) => {
                // Direct block form (unit tests / hand-built core expressions).
                for (alias_name, type_str_expr) in b.iter() {
                    if let Some(type_str) = extract_string_literal(type_str_expr) {
                        if let Ok(ty) = parse::parse_type(&type_str) {
                            self.register_alias(alias_name.clone(), ty);
                        }
                    }
                }
            }
            _ => {}
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
    fn extract_annotation(&self, meta: &RcExpr) -> Option<(Type, Vec<Constraint>, bool, bool)> {
        let block = match &*meta.inner {
            Expr::Block(_, b) => b,
            _ => return None,
        };

        let (type_str, is_hint): (String, bool) = if let Some(e) = block.get("type") {
            (extract_string_literal(e)?, false)
        } else if let Some(e) = block.get("__type_hint") {
            (extract_string_literal(e)?, true)
        } else if let Some(e) = block.get("monad") {
            // `monad:` metadata — derive the full monad combinator block type.
            // Accepts a string pattern like `"[a]"` or `"IO(a)"`, or the
            // boolean `true` for the Identity monad.
            //
            // `monad: true` in eucalypt metadata desugars to a Var reference
            // pointing at the prelude's `true` binding (not a raw bool literal).
            // We detect it by checking for a bound/free var named "true", as
            // well as the literal form (in case the metadata is constructed
            // programmatically).
            let is_monad_true = matches!(&*e.inner, Expr::Literal(_, Primitive::Bool(true)))
                || matches!(&*e.inner,
                    Expr::Var(_, Var::Bound(bv)) if bv.name.as_deref() == Some("true"))
                || matches!(&*e.inner, Expr::Var(_, Var::Free(name)) if name == "true");
            let pattern_str = if let Some(s) = extract_string_literal(e) {
                s
            } else if is_monad_true {
                "true".to_string()
            } else {
                return None;
            };
            let derived = self.derive_monad_block_type(&pattern_str)?;
            // Asserted (body not verified) and not a hint (type IS authoritative).
            // No operator constraints on monad-derived types.
            return Some((derived, Vec::new(), true, false));
        } else {
            return None;
        };

        // A leading `!` marks the annotation as asserted (body not verified)
        let (type_str, asserted) = if let Some(stripped) = type_str.strip_prefix('!') {
            (stripped.trim().to_string(), true)
        } else {
            (type_str, false)
        };

        let (parsed, constraints) = parse::parse_scheme(&type_str).ok()?;
        Some((
            self.resolve_aliases_in_type(parsed),
            constraints,
            asserted,
            is_hint,
        ))
    }

    /// Derive the block type for a monad namespace from its `monad:` pattern string.
    ///
    /// Given a string like `"[a]"` (List monad), `"IO(a)"` (IO monad),
    /// `"stream → {value: a, rest: stream}"` (state monad), or `"true"`
    /// (Identity monad), derives the full `{bind, return, map, then,
    /// and-then, join, sequence, map-m, filter-m}` block type by
    /// instantiating the generic monad combinator types.
    ///
    /// The convention is that `a` is the element type variable in the pattern.
    /// For the identity monad (`monad: true`), `m a = a` (identity function).
    /// Single-pass structural substitution: replace `var` with `concrete`
    /// throughout `pattern` without recursing into `concrete`.
    ///
    /// Unlike `apply_subst`, this function does NOT re-apply the substitution
    /// to `concrete` after placing it.  This is essential for `derive_monad_block_type`
    /// where `concrete` may itself contain `var` (e.g. `m [a]` when `var = a`),
    /// which would cause `apply_subst` to infinite-loop.
    fn structural_subst(pattern: &Type, var: &TypeVarId, concrete: &Type) -> Type {
        match pattern {
            Type::Var(id, _) if id == var => concrete.clone(),
            Type::Var(_, _) => pattern.clone(),
            Type::App(f, x) => Type::App(
                Box::new(Self::structural_subst(f, var, concrete)),
                Box::new(Self::structural_subst(x, var, concrete)),
            ),
            Type::Con(_) => pattern.clone(),
            Type::Function(a, b) => Type::Function(
                Box::new(Self::structural_subst(a, var, concrete)),
                Box::new(Self::structural_subst(b, var, concrete)),
            ),
            Type::Record { fields, open, rows } => Type::Record {
                fields: fields
                    .iter()
                    .map(|(k, v)| (k.clone(), Self::structural_subst(v, var, concrete)))
                    .collect(),
                open: *open,
                rows: rows.clone(),
            },
            Type::Union(vs) => Type::Union(
                vs.iter()
                    .map(|v| Self::structural_subst(v, var, concrete))
                    .collect(),
            ),
            Type::Tuple(elems) => Type::Tuple(
                elems
                    .iter()
                    .map(|e| Self::structural_subst(e, var, concrete))
                    .collect(),
            ),
            Type::Forall(binders, body) => {
                if binders.iter().any(|(id, _)| id == var) {
                    // `var` is bound here — do not substitute inside.
                    pattern.clone()
                } else {
                    Type::Forall(
                        binders.clone(),
                        Box::new(Self::structural_subst(body, var, concrete)),
                    )
                }
            }
            Type::Mu(x, body) => {
                if x == var {
                    pattern.clone()
                } else {
                    Type::Mu(
                        x.clone(),
                        Box::new(Self::structural_subst(body, var, concrete)),
                    )
                }
            }
            other => other.clone(),
        }
    }

    fn derive_monad_block_type(&self, pattern_str: &str) -> Option<Type> {
        let elem_id = TypeVarId("a".to_string());
        let ret_id = TypeVarId("b".to_string());

        // Parse the monad pattern — the type representing `m(a)`.
        let monad_pattern: Type = if pattern_str == "true" {
            // Identity monad: m a = a
            Type::Var(elem_id.clone(), Kind::Star)
        } else {
            let parsed = parse::parse_type(pattern_str).ok()?;
            self.resolve_aliases_in_type(parsed)
        };

        // Substitute elem_id with `concrete` in the monad pattern.
        // Uses structural_subst (not apply_subst) to avoid infinite recursion
        // when `concrete` itself contains `elem_id` (e.g. m([a]) = [[a]]).
        let apply_m = |concrete: &Type| -> Type {
            Self::structural_subst(&monad_pattern, &elem_id, concrete)
        };

        // Convenience: right-associative function arrow.
        let arrow = |a: Type, b: Type| -> Type { Type::Function(Box::new(a), Box::new(b)) };

        let a = Type::Var(elem_id.clone(), Kind::Star);
        let b = Type::Var(ret_id.clone(), Kind::Star);

        let m_a = apply_m(&a);
        let m_b = apply_m(&b);
        // m (m a) — used for join
        let m_m_a = apply_m(&m_a);
        // m [a] — used for sequence
        let m_list_a = apply_m(&Type::list(a.clone()));
        // m [b] — used for map-m
        let m_list_b = apply_m(&Type::list(b.clone()));
        // m bool — used for filter-m
        let m_bool = apply_m(&Type::Bool);
        // [m a] — used for sequence
        let list_m_a = Type::list(m_a.clone());

        // bind:       m a → (a → m b) → m b
        let bind = arrow(
            m_a.clone(),
            arrow(arrow(a.clone(), m_b.clone()), m_b.clone()),
        );
        // return:     a → m a
        let ret = arrow(a.clone(), m_a.clone());
        // map:        (a → b) → m a → m b
        let map = arrow(arrow(a.clone(), b.clone()), arrow(m_a.clone(), m_b.clone()));
        // then:       m b → m a → m b   (first arg = continuation, second = receiver)
        let then = arrow(m_b.clone(), arrow(m_a.clone(), m_b.clone()));
        // and-then:   (a → m b) → m a → m b
        let and_then = arrow(
            arrow(a.clone(), m_b.clone()),
            arrow(m_a.clone(), m_b.clone()),
        );
        // join:       m (m a) → m a
        let join = arrow(m_m_a, m_a.clone());
        // sequence:   [m a] → m [a]
        let sequence = arrow(list_m_a, m_list_a.clone());
        // map-m:      (a → m b) → [a] → m [b]
        let map_m = arrow(
            arrow(a.clone(), m_b),
            arrow(Type::list(a.clone()), m_list_b),
        );
        // filter-m:   (a → m bool) → [a] → m [a]
        let filter_m = arrow(arrow(a.clone(), m_bool), arrow(Type::list(a), m_list_a));

        let mut fields = BTreeMap::new();
        fields.insert("and-then".to_string(), and_then);
        fields.insert("bind".to_string(), bind);
        fields.insert("filter-m".to_string(), filter_m);
        fields.insert("join".to_string(), join);
        fields.insert("map".to_string(), map);
        fields.insert("map-m".to_string(), map_m);
        fields.insert("return".to_string(), ret);
        fields.insert("sequence".to_string(), sequence);
        fields.insert("then".to_string(), then);

        Some(Type::Record {
            fields,
            open: true,
            rows: vec![],
        })
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
                .map(|(ty, constraints, _asserted, _is_hint)| {
                    let mut scheme = infer_scheme(ty);
                    scheme.constraints = constraints;
                    scheme
                })
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
            // Non-empty list literals synthesise as `Tuple` up to a
            // practicality cap (`LIST_TUPLE_CAP`), preserving element positions
            // for precise head/tail typing (§A6.3).  Beyond the cap the tuple
            // would become unwieldy so we fall back to `NonEmpty(<elem union>)`,
            // which still captures non-emptiness. The empty literal is `[any]`.
            Expr::List(_, items) => {
                let elem_types: Vec<Type> = items.iter().map(|e| self.synthesise(e)).collect();
                synthesise_list_literal(elem_types)
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

                // BranchShape + ProjectionShape classification.
                //
                // Eagerly classify each binding as a branch combinator (§A5.3)
                // and/or a tuple-element projector (§B6.3) so that later uses
                // are recognised during synthesis.
                //
                // anchored = scope_stack.len() - 1 (the newly-pushed innermost frame).
                let anchored = self.scope_stack.len() - 1;
                for (name, value) in &scope.pattern {
                    let key: BoundKey = (anchored, name.clone());
                    // Tentative `None` entries double as recursion guards: if
                    // classification recurses back to this binding, it sees
                    // "not a brancher/projector" and terminates.
                    self.branch_shapes.insert(key.clone(), None);
                    self.projection_shapes.insert(key.clone(), None);
                    // Store the body for later structural analysis.
                    let body = peel_meta(value).clone();
                    self.binding_bodies.insert(key.clone(), body.clone());
                    // Classify and update.
                    let shape = self.classify_binding_shape(&body, self.scope_stack.len());
                    self.branch_shapes.insert(key.clone(), shape);
                    let proj = self.classify_projection_body(&body, self.scope_stack.len());
                    self.projection_shapes.insert(key, proj);
                }

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
                            self.extract_annotation(meta).map(|(ty, _, _, _)| ty)
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
            // B9: attempt row-variable inference for unannotated lambdas whose
            // parameters are used as blocks.  Falls back to `any` when no
            // block usage is detected (preserving the gradual boundary).
            Expr::Lam(_, _, scope) => self.synthesise_lam(scope),

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

        if let Some((annotated_type, _constraints, asserted, is_hint)) =
            self.extract_annotation(meta)
        {
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
                                Type::list(first.clone())
                            } else {
                                // Heterogeneous tuple → list of union
                                // Type::union deduplicates and normalises
                                // (absorbs LiteralString into String, etc.)
                                let elem_type = Type::union(elems.iter().cloned());
                                Type::list(elem_type)
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
    /// as a branching construct (`if`, `then`, `and`, `or`, `cond`, or any
    /// user-defined wrapper) and delegates to the narrowing-aware
    /// `synthesise_branch_*` helpers when recognised.
    ///
    /// Spine flattening: `x then(a, b)` cooks to `App(App(then, [a,b]), [x])`.
    /// We flatten `func` into `(head, prefix_args)` and prepend to `args` so
    /// the full argument list `[a, b, x]` is visible for recognition.
    fn synthesise_app(&mut self, smid: Smid, func: &RcExpr, args: &[RcExpr]) -> Type {
        // ── Spine flattening + branch recognition (§A5.2, §A5.3) ─────────────
        //
        // Flatten the function spine to find the outermost head and the full
        // argument list.  Required for pipelined branchers like `x then(a,b)`.
        let (head, prefix_args) = flatten_app_spine(func);
        let spine_args: Vec<&RcExpr> = prefix_args.iter().copied().chain(args.iter()).collect();

        if let Some(shape) = self.recognise_brancher(head) {
            if spine_args.len() == shape.arity {
                if let Some(ty) = self.synthesise_branch_with_shape(&shape, head, &spine_args) {
                    return ty;
                }
            }
            // Wrong arity (partial application) — fall through to generic path.
        }

        // ── LOOKUP literal-key typed access (§B6.2) ─────────────────────────
        //
        // When `lookup` / `LOOKUP` is applied to a literal-symbol key and a
        // known record type, return the precise field type (or warn on a key
        // typo in a closed record).  This fires before the generic annotation
        // path so that the precise field type overrides the `Dict(a) → a`
        // annotation.
        //
        // The type checker runs before inlining, so the head may be either:
        // - `Expr::Intrinsic(_, "LOOKUP")` — the raw intrinsic (post-inline context)
        // - `Expr::Var(Bound, "lookup" | "lookup-in")` — the prelude alias
        if spine_args.len() == 2 && is_lookup_fn(head) {
            let key_type = self.synthesise(spine_args[0]);
            if let Type::LiteralSymbol(key) = key_type {
                let block_type = self.synthesise(spine_args[1]);
                let smid = spine_args[0].smid();
                return self.synthesise_lookup_with_literal_key(&key, &block_type, smid);
            }
            // Non-literal key — gradual (key may be absent at runtime).
            return Type::Any;
        }

        // ── HEAD/TAIL-on-Tuple precise typing (§A6.8) ────────────────────────
        //
        // When `head` or `tail` is applied to a `Tuple` argument, override the
        // annotation result with precise element-type information:
        //   head(Tuple([T₀,…,Tₙ])) → T₀
        //   tail(Tuple([T₀,T₁,…])) → Tuple([T₁,…])   (1-elem → List(any))
        //
        // Also: head/tail on `List(Never)` (the empty literal `[]`) returns
        // `Never`, ensuring the annotation check warns (AC2).
        //
        // This is checked AFTER branch recognition (which takes priority) and
        // only fires when there is exactly one argument.
        if spine_args.len() == 1 {
            if let Some(proj) = is_head_or_tail(head) {
                let arg_type = self.synthesise(spine_args[0]);
                if let Some(result) = apply_head_tail_to_tuple(proj, &arg_type) {
                    return result;
                }
                // Empty list literal: `[]` synthesises as `List(Never)`.
                // `head`/`tail` on an empty list always panics at runtime —
                // emit a direct type warning so the user is alerted even if
                // the call site annotation is `any`.  Return `Any` so that
                // downstream checks can continue without cascading errors.
                if arg_type.as_list().is_some_and(|e| matches!(e, Type::Never)) {
                    let proj_name = match proj {
                        HeadTailProjection::Head => "head",
                        HeadTailProjection::Tail => "tail",
                    };
                    let smid = func.smid();
                    let warning = TypeWarning::new(format!("{proj_name} of empty list"))
                        .at(smid)
                        .with_note(format!(
                            "guard against empty lists with 'nil?', e.g. \
                         'if(xs nil?, default, xs {proj_name})'"
                        ))
                        .with_note(
                            "'head' and 'tail' are only defined on non-empty lists; \
                         use 'nth(0, xs)' or pattern matching if the list may be empty"
                                .to_string(),
                        );
                    self.warnings.push(warning);
                    return Type::Any;
                }
                // Not a Tuple or empty — fall through to generic path which uses
                // the NonEmpty([a]) annotation.
            }
        }

        // ── ProjectionShape precise tuple typing (§B6.3) ─────────────────────
        //
        // When a bound variable function has a ProjectionShape of index `i` and
        // is applied to a `Tuple([T₀,…,Tₙ])`, return `Tᵢ` precisely.
        // This handles `second(Tuple([K,V])) → V`, `value(Tuple([K,V])) → V`, etc.
        //
        // Head/first/key at index 0 are already handled by is_head_or_tail above;
        // this path covers index ≥ 1 (and also 0 for completeness via bound vars).
        if spine_args.len() == 1 {
            if let Some(index) = self.recognise_projection_index(head) {
                let arg_type = self.synthesise(spine_args[0]);
                if let Type::Tuple(ref elems) = arg_type {
                    if let Some(ty) = elems.get(index) {
                        return ty.clone();
                    } else {
                        // Out-of-range index — warn and return any.
                        let smid = head.smid();
                        let warning = TypeWarning::new(format!("tuple index {index} out of range"))
                            .at(smid)
                            .with_note(format!("tuple has {} elements", elems.len()));
                        self.warnings.push(warning);
                        return Type::Any;
                    }
                }
                // Not a Tuple — fall through to generic annotation path.
            }
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

        // ── §A10: Monadic bind element-type pre-pass ─────────────────────────
        //
        // Detect the two-argument monadic bind pattern produced by
        // `desugar_monadic_block`:
        //
        //   App(bind_fn, [Meta(inner, {__type_hint: "T(a)"}), Lam(x, body)])
        //
        // When recognised, synthesise the inner value directly (bypassing the
        // Meta wrapper to avoid double-synthesising via `synthesise_meta`),
        // normalise any homogeneous tuple to a list, and infer the concrete
        // element type `a` by unifying the freshened wrapper against the
        // synthesised value type.
        //
        // If a concrete element type is found, record the resolved lambda
        // parameter types in `self.lambda_params` (keyed by the lambda's Smid)
        // for LSP inlay hints.
        //
        // Body type-checking is deliberately left to the normal application
        // loop below — when the bind function carries a type annotation (as
        // `for.bind` and `io.bind` do), `apply_one_with_subst` fires the
        // `check_lambda` branch and emits any warnings from the body.  The
        // pre-pass avoids calling `check_against` on the lambda to prevent
        // duplicate warnings.
        if args.len() == 2 {
            if let Some((hint_str, inner_val)) = extract_monad_hint_inner(&args[0]) {
                if let Expr::Lam(lam_smid, _, scope) = &*args[1].inner {
                    let raw_val_type = self.synthesise(inner_val);
                    let val_type = normalise_tuple_to_list(raw_val_type);
                    if let Some(elem_type) =
                        self.infer_elem_type_from_hint_str(&hint_str, &val_type)
                    {
                        // Record lambda parameter types for LSP inlay hints.
                        let params: Vec<(String, Type)> = scope
                            .pattern
                            .iter()
                            .map(|name| (name.clone(), elem_type.clone()))
                            .collect();
                        if !params.is_empty() {
                            self.lambda_params.insert(*lam_smid, params);
                        }
                    }
                }
            }
        }

        // ── Constraint extraction (B2) ────────────────────────────────────────
        //
        // If the head of the application spine has a constrained scheme
        // (e.g. `<(a, a) => a -> a -> a`), freshen the constraints with the
        // same rename map as the body so that constraint variables unify
        // correctly with the accumulated substitution.
        //
        // When the head has no constraints (the common case), fall back to the
        // ordinary `synthesise(func)` path.
        let (func_type, pending_constraints): (Type, Vec<Constraint>) = {
            let head_scheme = self.lookup_scheme_for_expr(head);
            if let Some(ref scheme) = head_scheme {
                if !scheme.constraints.is_empty() {
                    // Freshen head scheme body and constraints together so
                    // they share the same rename map.
                    let (head_body, constraints) =
                        freshen_with_constraints(scheme, &mut self.var_counter);
                    // Apply any prefix args (from the flattened spine) to the
                    // freshened head body to arrive at the partially-applied
                    // type that `synthesise(func)` would normally produce.
                    let mut pre_subst = Substitution::new();
                    let ft = prefix_args.iter().fold(head_body, |t, a| {
                        self.apply_one_with_subst(
                            a.smid(),
                            t,
                            a,
                            &mut pre_subst,
                            func_name.as_deref(),
                        )
                    });
                    (apply_subst(&ft, &pre_subst), constraints)
                } else {
                    (self.synthesise(func), Vec::new())
                }
            } else {
                (self.synthesise(func), Vec::new())
            }
        };

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

        // ── Constraint discharge (B2) ─────────────────────────────────────────
        //
        // After all arguments have been applied, the accumulated substitution
        // `subst` maps fresh type variables to their concrete types.  Discharge
        // each pending constraint: if the operator does not accept the resolved
        // argument types, emit a type warning.
        for constraint in &pending_constraints {
            self.discharge_constraint(constraint, &subst, smid);
        }

        // Apply the accumulated substitution to resolve any remaining vars.
        apply_subst(&current_type, &subst)
    }

    /// Infer the element type of a monadic container from its hint string and
    /// a concrete value type.
    ///
    /// §A10: Given a hint like `"[a]"` (for `List`) or `"io(a)"` (for `IO`),
    /// and the synthesised type of the bound value (e.g. `List(number)`), this
    /// method:
    ///
    /// 1. Parses and alias-resolves the hint to get a wrapper type such as
    ///    `List(Var("a"))`.
    /// 2. Freshens the wrapper, replacing `"a"` with a fresh unification
    ///    variable `_t5`.
    /// 3. Unifies the freshened wrapper with `val_type`.
    /// 4. Extracts the element component from the freshened wrapper and applies
    ///    the substitution to resolve it to a concrete type.
    ///
    /// Returns `None` when the hint cannot be parsed, the wrapper type is
    /// not a recognised container (`List`, `IO`, `Dict`, `NonEmpty`), or
    /// the unification fails (e.g. `val_type = string` vs wrapper `[a]`).
    fn infer_elem_type_from_hint_str(&mut self, hint_str: &str, val_type: &Type) -> Option<Type> {
        let parsed = parse::parse_type(hint_str).ok()?;
        let resolved = self.resolve_aliases_in_type(parsed);
        let scheme = infer_scheme(resolved);
        let freshened = freshen(&scheme, &mut self.var_counter);

        // Extract the inner type variable from the freshened wrapper.
        // Accepts List, IO, Dict, and NonEmpty containers — all represented as
        // App(Con("X"), T) in the new HKT representation.
        // Accepts List, IO, Dict, and NonEmpty containers — all represented as
        // App(Con("X"), T) in the new HKT representation.
        let inner = freshened
            .as_list()
            .or_else(|| freshened.as_io())
            .or_else(|| freshened.as_dict())
            .or_else(|| freshened.as_non_empty())
            .cloned()?;

        // Unify the freshened wrapper with the concrete value type.
        let mut subst = Substitution::new();
        unify(&freshened, val_type, &mut subst).ok()?;

        // Resolve the inner type variable through the substitution.
        let elem = apply_subst(&inner, &subst);

        // Return the element type only if it is concrete (informative and not
        // an unresolved type variable, which would give no useful information).
        if is_informative(&elem) && !matches!(&elem, Type::Var(_, _)) {
            Some(elem)
        } else {
            None
        }
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
                                if is_informative(&resolved)
                                    && !matches!(&resolved, Type::Var(_, _))
                                {
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

                // A small list literal (≤ LIST_TUPLE_CAP items) synthesises as a tuple
                // type rather than a list type.  When the parameter expects a constructor
                // application — e.g. `[a]`, `IO(a)`, or an HKT variable `m a` — widen a
                // homogeneous tuple to a list so that calls like `hk-id([1, 2, 3])` do
                // not produce a spurious type mismatch.  Heterogeneous tuples are widened
                // to `List(union)` to avoid masking head/tail precision elsewhere.
                let arg_type = match (&arg_type, &param_applied) {
                    (Type::Tuple(elems), Type::App(_, _)) if !elems.is_empty() => {
                        let first = &elems[0];
                        if elems.iter().all(|e| e == first) {
                            Type::list(first.clone())
                        } else {
                            Type::list(Type::union(elems.iter().cloned()))
                        }
                    }
                    _ => arg_type,
                };

                if !is_informative(&arg_type) || !is_informative(&param_applied) {
                    // Gradual boundary — no warning.
                    return apply_subst(&result_type, subst);
                }

                // B5.3 — Partial argument in total parameter position.
                // When the caller passes a partial result (`T?`) where the function
                // expects a total value, warn.  The Union consistency rule would
                // otherwise suppress the warning.
                if is_partial_type(&arg_type) && !is_partial_type(&param_applied) {
                    let message = build_arg_mismatch_message(func_name);
                    self.emit_type_mismatch(smid, &param_applied, &arg_type, &message);
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

            // Polymorphic function — instantiate then apply.
            //
            // A `forall (m :: * -> *). m a → ...` type is encountered when a
            // field is accessed on a value whose type contains an explicit
            // `Forall` node (e.g. from a `monad()` annotation).  Freshen the
            // binders (allocating fresh unification variables) and recurse so
            // that the instantiated `Function` arm fires.
            Type::Forall(binders, body) => {
                let instantiated = freshen_forall(&binders, &body, &mut self.var_counter);
                self.apply_one_with_subst(smid, instantiated, arg, subst, func_name)
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

    /// Dispatch to the appropriate narrowing-aware branch synthesiser using
    /// a `BranchShape` extracted from the flattened application spine.
    ///
    /// `spine_args` contains the *full* argument list (flattened); the shape
    /// records which indices are the condition and branch arms.
    ///
    /// Returns `Some(ty)` on success, `None` for internal errors only (the
    /// arity check is done by the caller).
    fn synthesise_branch_with_shape(
        &mut self,
        shape: &BranchShape,
        func: &RcExpr,
        spine_args: &[&RcExpr],
    ) -> Option<Type> {
        match shape.kind {
            BranchKind::If => {
                let condition = spine_args[shape.condition];
                let true_branch = spine_args[shape.branches[0]];
                let false_branch = spine_args[shape.branches[1]];
                Some(self.synthesise_branch_if(func, condition, true_branch, false_branch))
            }
            BranchKind::And => {
                let left = spine_args[shape.condition];
                let right = spine_args[shape.branches[0]];
                Some(self.synthesise_branch_and(func, left, right))
            }
            BranchKind::Or => {
                let left = spine_args[shape.condition];
                let right = spine_args[shape.branches[0]];
                Some(self.synthesise_branch_or(func, left, right))
            }
            BranchKind::Cond => {
                let clause_list = spine_args[shape.branches[0]];
                Some(self.synthesise_branch_cond(func, clause_list))
            }
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
            // Single-argument application: either `p?(x)` (predicate) or `not(c)`.
            Expr::App(_, func, args) if args.len() == 1 => {
                // `p?(x)` — predicate applied to a variable.
                if let Some(pred) = classify_predicate(func) {
                    if let Expr::Var(_, Var::Bound(bv)) = &*args[0].inner {
                        return self.facts_for_predicate(pred, bv);
                    }
                // `not(c)` — invert the condition's facts.
                } else if is_not_func(func) {
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
                Type::list(Type::Any),
                subtract_type(&current_ty, &Type::list(Type::Any)),
            ),
            PredicateKind::Block => (
                Type::Record {
                    fields: Default::default(),
                    open: true,
                    rows: vec![],
                },
                Type::Any,
            ),
            // `nil?` narrows to NonEmpty in the NEGATIVE branch (§A6.4).
            // Positive branch: no useful narrowing — leave as Any (suppressed).
            PredicateKind::Nil => (Type::Any, narrow_to_nonempty(&current_ty)),
            // `non-nil?` narrows to NonEmpty in the POSITIVE branch (§A6.4).
            PredicateKind::NonNil => (narrow_to_nonempty(&current_ty), Type::Any),
            // `null?` narrows to Null (positive) / subtracts Null (negative).
            PredicateKind::Null => (Type::Null, subtract_type(&current_ty, &Type::Null)),
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

        // B5.3 — Partial type in total position.
        //
        // The Union consistency rule allows `T | ExecutionError ~ T` (consistent,
        // because the `T` variant is consistent with `T`).  However, when a caller
        // has explicitly annotated a *total* return type and the expression actually
        // returns a partial result, we want to warn so that the annotation is honest.
        //
        // We detect this by checking: if `found` is a partial type (union containing
        // ExecutionError) and `expected` is *not* partial (and not `any`), emit a
        // warning instead of relying on the consistency check which would silently
        // succeed.
        if is_partial_type(&found) && !is_partial_type(expected) && is_informative(expected) {
            self.emit_type_mismatch(
                smid,
                expected,
                &found,
                "expression type does not match annotation",
            );
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

    // ── B9: Row-variable inference at lambda boundaries ──────────────────────

    /// Synthesise a type for an unannotated lambda using use-driven row-variable
    /// inference (§B9).
    ///
    /// Scans the lambda body to identify which parameters are "used as a block"
    /// (appear in a projection or merge/over argument position).  Block-shaped
    /// parameters receive a fresh open record type `{..rN}` instead of `any`.
    ///
    /// If no parameter is block-shaped, falls back to `any` immediately,
    /// preserving the existing gradual boundary for non-block lambdas.
    fn synthesise_lam(&mut self, scope: &crate::core::expr::LamScope<RcExpr>) -> Type {
        let block_flags = detect_block_params(scope);

        // Fast path: no block-shaped params → keep `any` (unchanged behaviour).
        if block_flags.iter().all(|&b| !b) {
            return Type::Any;
        }

        // Allocate a fresh open record type for each block-shaped parameter,
        // `any` for the rest.
        let param_types: Vec<Type> = block_flags
            .iter()
            .map(|&is_block| {
                if is_block {
                    let row_id = TypeVarId(format!("_r{}", self.var_counter));
                    self.var_counter += 1;
                    Type::Record {
                        fields: BTreeMap::new(),
                        open: true,
                        rows: vec![row_id],
                    }
                } else {
                    Type::Any
                }
            })
            .collect();

        // Push a scope frame with the allocated parameter types and synthesise
        // the body under them.
        let mut frame: HashMap<String, TypeScheme> = HashMap::new();
        for (name, ty) in scope.pattern.iter().zip(param_types.iter()) {
            frame.insert(name.clone(), TypeScheme::mono(ty.clone()));
        }
        self.push_scope(frame);
        let body_type = self.synthesise(&scope.body);
        self.pop_scope();

        // Build a curried function type: T₀ → T₁ → … → body_type.
        param_types.into_iter().rev().fold(body_type, |acc, param| {
            Type::Function(Box::new(param), Box::new(acc))
        })
    }

    // ── BranchShape recognition (§A5.3) ─────────────────────────────────────

    /// Look up the `BranchShape` for an expression in the head position of an
    /// application.
    ///
    /// Covers:
    /// - Raw branch intrinsics (`Expr::Intrinsic`).
    /// - Bound variables with a memoised shape in `branch_shapes`.
    ///
    /// Returns `None` for everything else (not a recognised brancher).
    fn recognise_brancher(&self, func: &RcExpr) -> Option<BranchShape> {
        match &*func.inner {
            Expr::Intrinsic(_, name) => intrinsic_branch_shape(name),
            Expr::Var(_, Var::Bound(bv)) => {
                let name = bv.name.as_deref()?;
                let idx = bv.scope as usize;
                // Saturating sub: if idx >= scope_stack.len(), the variable
                // is from an outer scope that we haven't seen (should not
                // normally happen after varify, but guard anyway).
                let anchored = self.scope_stack.len().saturating_sub(1 + idx);
                let key = (anchored, name.to_string());
                // Flatten Option<Option<BranchShape>> via and_then.
                self.branch_shapes.get(&key).and_then(|opt| opt.clone())
            }
            // Free variable — occurs in prelude-cached checks where
            // prelude references have not been merged in.  Check the seed
            // branch shapes populated from the cached prelude summary.
            Expr::Var(_, Var::Free(name)) => intrinsic_branch_shape(name)
                .or_else(|| self.seed_branch_shapes.get(name.as_str()).cloned()),
            _ => None,
        }
    }

    /// Classify a binding's body expression as a `BranchShape`.
    ///
    /// `body` is the metadata-peeled binding value.
    /// `stack_len` is `scope_stack.len()` at the time the enclosing Let is
    /// being processed (AFTER the frame has been pushed).
    ///
    /// Classification is one level deep (§A5.3):
    /// - An alias of a raw intrinsic inherits the intrinsic's shape.
    /// - A `Lam` whose body is a pass-through application of a known
    ///   brancher acquires a composed shape.
    /// - Everything else → `None`.
    fn classify_binding_shape(&self, body: &RcExpr, stack_len: usize) -> Option<BranchShape> {
        let peeled = peel_meta(body);
        match &*peeled.inner {
            // Direct alias: `if: __IF` — inherits the intrinsic's shape.
            Expr::Intrinsic(_, name) => intrinsic_branch_shape(name),
            // Lambda — inspect the body for a pass-through call (§A5.3 Mechanism 2).
            Expr::Lam(_, _, scope) => {
                self.classify_lam_body(&scope.pattern, &scope.body, stack_len)
            }
            _ => None,
        }
    }

    /// Attempt to classify a lambda as a pass-through wrapper around a
    /// known brancher.
    ///
    /// `params` — the parameter names in order (e.g. `["t","f","c"]`).
    /// `body` — the lambda body expression.
    /// `stack_len` — `scope_stack.len()` at the time of classification
    ///   (BEFORE entering the lambda's own scope).
    ///
    /// Returns a composed `BranchShape` when the body is `inner(args…)` where
    /// `inner` is a recognised brancher and every argument is one of the
    /// lambda's own parameters.
    fn classify_lam_body(
        &self,
        params: &[String],
        body: &RcExpr,
        stack_len: usize,
    ) -> Option<BranchShape> {
        // Inside the lambda there is one extra scope level for the params.
        let effective_len = stack_len + 1;

        let peeled = peel_meta(body);
        let (app_head, app_args) = match &*peeled.inner {
            Expr::App(_, head, args) => (head, args.as_slice()),
            _ => return None,
        };

        // Determine the inner brancher's shape from the application head.
        let inner_shape: BranchShape = match &*peel_meta(app_head).inner {
            // Head is a raw intrinsic.
            Expr::Intrinsic(_, name) => intrinsic_branch_shape(name)?,
            // Head is an outer bound variable — look up in branch_shapes.
            Expr::Var(_, Var::Bound(bv)) if bv.scope as usize >= 1 => {
                let name = bv.name.as_deref()?;
                let anchored = effective_len.checked_sub(1 + bv.scope as usize)?;
                let key = (anchored, name.to_string());
                self.branch_shapes.get(&key)?.clone()?
            }
            // Head is a free variable — with prelude caching, prelude
            // branchers such as `if`, `then`, `and` are Var::Free.
            Expr::Var(_, Var::Free(name)) => intrinsic_branch_shape(name)
                .or_else(|| self.seed_branch_shapes.get(name.as_str()).cloned())?,
            _ => return None,
        };

        // Verify the call arity matches the inner shape.
        if app_args.len() != inner_shape.arity {
            return None;
        }

        // Every app_arg must be one of the lambda's own parameters (scope = 0).
        // Map each to its parameter index in `params`.
        let param_positions: Vec<usize> = app_args
            .iter()
            .map(|arg| {
                let peeled = peel_meta(arg);
                if let Expr::Var(_, Var::Bound(bv)) = &*peeled.inner {
                    if bv.scope == 0 {
                        let pname = bv.name.as_deref()?;
                        return params.iter().position(|p| p == pname);
                    }
                }
                None
            })
            .collect::<Option<Vec<_>>>()?;

        // Compose: map inner shape's condition/branch arg indices through param_positions.
        let condition_param = *param_positions.get(inner_shape.condition)?;
        let branch_params: Option<Vec<usize>> = inner_shape
            .branches
            .iter()
            .map(|&bi| param_positions.get(bi).copied())
            .collect();

        Some(BranchShape {
            arity: params.len(),
            condition: condition_param,
            branches: branch_params?,
            kind: inner_shape.kind,
        })
    }

    // ── Warning emission ─────────────────────────────────────────────────────

    fn emit_type_mismatch(&mut self, smid: Smid, expected: &Type, found: &Type, message: &str) {
        use super::types::humanise;
        let warning = TypeWarning::new(message)
            .at(smid)
            .with_types(humanise(expected).to_string(), humanise(found).to_string());
        self.warnings.push(warning);
    }

    // ── LOOKUP literal-key resolution (§B6.2) ────────────────────────────────

    /// Resolve `LOOKUP(LiteralSymbol(key), block_type)` to the precise field type.
    ///
    /// | block_type | key present | key absent | result |
    /// |------------|------------|------------|--------|
    /// | closed `Record` | yes | — | field type |
    /// | closed `Record` | — | yes | `any` + key-typo warning |
    /// | open `Record` or row-variable `Record` | — | yes | `any`, no warning |
    /// | `Dict(v)` | — | — | `v` |
    /// | other | — | — | `any` |
    fn synthesise_lookup_with_literal_key(
        &mut self,
        key: &str,
        block_type: &Type,
        smid: Smid,
    ) -> Type {
        match block_type {
            Type::Record { fields, open, rows } => {
                if let Some(field_type) = fields.get(key) {
                    field_type.clone()
                } else if !open && rows.is_empty() {
                    // Closed record without the key — key typo warning.
                    let known: Vec<String> = fields.keys().map(|k| format!(":{k}")).collect();
                    let note = if known.is_empty() {
                        "the record has no known fields".to_string()
                    } else {
                        format!("known fields: {}", known.join(", "))
                    };
                    let warning = TypeWarning::new(format!("unknown record key :{key}"))
                        .at(smid)
                        .with_note(note);
                    self.warnings.push(warning);
                    Type::Any
                } else {
                    // Open record or has row variables — key may be in the tail.
                    Type::Any
                }
            }
            other => {
                if let Some(val_type) = other.as_dict() {
                    val_type.clone()
                } else {
                    Type::Any
                }
            }
        }
    }

    // ── ProjectionShape recognition (§B6.3) ──────────────────────────────────

    /// Look up the `ProjectionShape` index for an expression in head position.
    ///
    /// Returns `Some(n)` when the function is a recognised tuple projector
    /// at index `n`.  Like `recognise_brancher`, this covers:
    /// - Bound variables whose `projection_shapes` entry is `Some(n)`.
    ///
    /// `head`/`first`/`key` at index 0 are already handled by `is_head_or_tail`
    /// earlier in `synthesise_app`; this function covers index ≥ 1 (e.g.
    /// `second`/`value`) and any user-defined projectors.
    fn recognise_projection_index(&self, func: &RcExpr) -> Option<usize> {
        match &*func.inner {
            Expr::Var(_, Var::Bound(bv)) => {
                let name = bv.name.as_deref()?;
                // head/first/key at index 0 — covered by is_head_or_tail, but
                // included here for completeness so the classifier is self-contained.
                match name {
                    "head" | "first" | "key" => Some(0),
                    _ => {
                        let idx = bv.scope as usize;
                        let anchored = self.scope_stack.len().saturating_sub(1 + idx);
                        let key = (anchored, name.to_string());
                        *self.projection_shapes.get(&key)?
                    }
                }
            }
            _ => None,
        }
    }

    /// Classify a binding body as a tuple projector and return its index.
    ///
    /// Classification cases (§B6.3):
    /// - `HEAD` intrinsic → index 0.
    /// - Bound variable named `"head"` / `"first"` / `"key"` → index 0.
    /// - Bound variable with a stored `projection_shapes` entry → inherited index.
    /// - Single-parameter lambda whose body matches `HEAD(TAIL^n(param))` → index n.
    ///
    /// Returns `None` for anything else (not a recognised projector).
    fn classify_projection_body(&self, body: &RcExpr, stack_len: usize) -> Option<usize> {
        let peeled = peel_meta(body);
        match &*peeled.inner {
            // Direct alias of HEAD intrinsic → index 0.
            Expr::Intrinsic(_, name) if name == "HEAD" => Some(0),
            // Alias of a bound variable: inherit from projection_shapes or classify by name.
            Expr::Var(_, Var::Bound(bv)) => {
                let name = bv.name.as_deref()?;
                match name {
                    "head" | "first" | "key" => Some(0),
                    _ => {
                        let anchored = stack_len.checked_sub(1 + bv.scope as usize)?;
                        let key = (anchored, name.to_string());
                        // Returns None if the binding hasn't been classified yet
                        // (the tentative-None recursion guard handles this).
                        *self.projection_shapes.get(&key)?
                    }
                }
            }
            // Single-parameter lambda — inspect body for HEAD(TAIL^n(param)).
            Expr::Lam(_, _, scope) if scope.pattern.len() == 1 => {
                let param_name = &scope.pattern[0];
                self.classify_proj_lam_body(param_name, &scope.body)
            }
            _ => None,
        }
    }

    /// Classify a single-param lambda body as a projection of its parameter.
    ///
    /// Matches `HEAD_fn(TAIL_fn^n(param))` and returns the index `n`.
    fn classify_proj_lam_body(&self, param_name: &str, body: &RcExpr) -> Option<usize> {
        let peeled = peel_meta(body);
        // Body must be App(head_fn, [inner]).
        let (app_head, app_args) = match &*peeled.inner {
            Expr::App(_, h, a) if a.len() == 1 => (h, a),
            _ => return None,
        };
        if !is_head_fn(peel_meta(app_head)) {
            return None;
        }
        // inner must be TAIL^n(param).
        self.count_tail_wraps_to_param(&app_args[0], param_name)
    }

    /// Count the number of TAIL wraps leading to `param_name`.
    ///
    /// - Returns `Some(0)` when `expr` IS the parameter variable.
    /// - Returns `Some(n)` when `expr` is `TAIL_fn(TAIL_fn(…(param)…))` with `n` tails.
    /// - Returns `None` when the pattern does not match.
    fn count_tail_wraps_to_param(&self, expr: &RcExpr, param_name: &str) -> Option<usize> {
        let peeled = peel_meta(expr);
        // Base case: the expression is the parameter itself.
        if let Expr::Var(_, Var::Bound(bv)) = &*peeled.inner {
            if bv.scope == 0 && bv.name.as_deref() == Some(param_name) {
                return Some(0);
            }
        }
        // Recursive case: App(tail_fn, [inner]).
        let (app_head, app_args) = match &*peeled.inner {
            Expr::App(_, h, a) if a.len() == 1 => (h, a),
            _ => return None,
        };
        if !is_tail_fn(peel_meta(app_head)) {
            return None;
        }
        let inner_count = self.count_tail_wraps_to_param(&app_args[0], param_name)?;
        Some(inner_count + 1)
    }
}

// ── Flow-sensitive narrowing — free functions ─────────────────────────────────

/// Return the `BranchShape` for a raw branch intrinsic, or `None`.
fn intrinsic_branch_shape(name: &str) -> Option<BranchShape> {
    match name {
        "IF" => Some(BranchShape {
            arity: 3,
            condition: 0,
            branches: vec![1, 2],
            kind: BranchKind::If,
        }),
        "AND" => Some(BranchShape {
            arity: 2,
            condition: 0,
            branches: vec![1],
            kind: BranchKind::And,
        }),
        "OR" => Some(BranchShape {
            arity: 2,
            condition: 0,
            branches: vec![1],
            kind: BranchKind::Or,
        }),
        "COND" => Some(BranchShape {
            arity: 1,
            condition: 0, // unused for cond
            branches: vec![0],
            kind: BranchKind::Cond,
        }),
        _ => None,
    }
}

/// Peel `Meta` wrappers from an expression, returning the innermost non-Meta node.
fn peel_meta(expr: &RcExpr) -> &RcExpr {
    match &*expr.inner {
        Expr::Meta(_, inner, _) => peel_meta(inner),
        _ => expr,
    }
}

/// Flatten a nested application spine into `(head, prefix_args)`.
///
/// Given `App(App(f, xs), ys)`, returns `(f, xs)` (without `ys`). The
/// caller is responsible for appending `ys` (the outer args) to get the
/// full argument list.
///
/// This handles pipelined branchers such as `x then(a, b)` which cook to
/// `App(App(then, [a, b]), [x])`: flattening yields `head = then`,
/// `prefix_args = [a, b]`, and the caller appends `[x]` to get the
/// full spine `[a, b, x]`.
fn flatten_app_spine(func: &RcExpr) -> (&RcExpr, Vec<&RcExpr>) {
    let mut head = func;
    let mut prefix: Vec<&RcExpr> = Vec::new();
    while let Expr::App(_, f, inner_args) = &*head.inner {
        // Prepend inner_args (they came before the outer args).
        let mut new_prefix: Vec<&RcExpr> = inner_args.iter().collect();
        new_prefix.extend_from_slice(&prefix);
        prefix = new_prefix;
        head = f;
    }
    (head, prefix)
}

// ── B9: use-driven block-parameter detection ──────────────────────────────────

/// Scan a lambda scope to determine which parameters are "used as a block"
/// — i.e. appear in a projection (`Lookup`) or as arguments to `merge`/`over`
/// in the body.
///
/// Returns a `Vec<bool>` of length `scope.pattern.len()`.  `true` at index `i`
/// means parameter `i` is block-shaped and should receive a fresh row-variable
/// record type during synthesis.
fn detect_block_params(scope: &crate::core::expr::LamScope<RcExpr>) -> Vec<bool> {
    let n = scope.pattern.len();
    let mut flags = vec![false; n];
    collect_block_uses(&scope.body, 0, &mut flags);
    flags
}

/// Recursively scan `expr` for block usages of the parameters introduced by
/// the lambda that `flags` belongs to.
///
/// `depth` is the number of additional lambda scopes that have been entered
/// since the original lambda boundary.  The original parameters are referenced
/// by `Var::Bound { scope: depth, binder: i }` inside nested lambdas.
fn collect_block_uses(expr: &RcExpr, depth: usize, flags: &mut Vec<bool>) {
    match &*expr.inner {
        // Projection: the object expression is used as a block.
        Expr::Lookup(_, obj, _, fallback) => {
            mark_block_param(obj, depth, flags);
            collect_block_uses(obj, depth, flags);
            if let Some(f) = fallback {
                collect_block_uses(f, depth, flags);
            }
        }

        // Application: if the function spine resolves to `merge` or `over`,
        // every argument in the full spine is used as a block.
        Expr::App(_, func, args) => {
            let (head, prefix_args) = flatten_app_spine(func);
            if is_merge_or_over_expr(head) {
                for arg in &prefix_args {
                    mark_block_param(arg, depth, flags);
                }
                for arg in args {
                    mark_block_param(arg, depth, flags);
                }
            }
            collect_block_uses(func, depth, flags);
            for arg in args {
                collect_block_uses(arg, depth, flags);
            }
        }

        // Nested lambda: increase depth so outer parameters are still found.
        Expr::Lam(_, _, inner_scope) => {
            collect_block_uses(&inner_scope.body, depth + 1, flags);
        }

        // Let binding: values are at the current depth; the body is at depth+1.
        Expr::Let(_, inner_scope, _) => {
            for (_, val) in &inner_scope.pattern {
                collect_block_uses(val, depth, flags);
            }
            collect_block_uses(&inner_scope.body, depth + 1, flags);
        }

        // Meta wrappers: recurse through both.
        Expr::Meta(_, inner, meta) => {
            collect_block_uses(inner, depth, flags);
            collect_block_uses(meta, depth, flags);
        }

        // Block literal: scan field values.
        Expr::Block(_, fields) => {
            for (_, val) in fields.iter() {
                collect_block_uses(val, depth, flags);
            }
        }

        // List literal: scan elements.
        Expr::List(_, items) => {
            for item in items {
                collect_block_uses(item, depth, flags);
            }
        }

        // Variables and primitives: nothing to do.
        _ => {}
    }
}

/// If `expr` is a direct `Var::Bound` reference to one of the lambda's own
/// parameters (at de Bruijn `scope == depth`), set the corresponding flag.
fn mark_block_param(expr: &RcExpr, depth: usize, flags: &mut [bool]) {
    let peeled = peel_meta(expr);
    if let Expr::Var(_, Var::Bound(bv)) = &*peeled.inner {
        if bv.scope as usize == depth {
            let idx = bv.binder as usize;
            if idx < flags.len() {
                flags[idx] = true;
            }
        }
    }
}

/// Return `true` if `expr` refers to `merge` or `over` — either as a free
/// variable (pre-varify), a named bound variable (post-varify), or an
/// intrinsic node (post-inline).
fn is_merge_or_over_expr(expr: &RcExpr) -> bool {
    let peeled = peel_meta(expr);
    match &*peeled.inner {
        Expr::Var(_, Var::Free(name)) => {
            matches!(name.as_str(), "merge" | "over")
        }
        Expr::Var(_, Var::Bound(bv)) => {
            matches!(bv.name.as_deref(), Some("merge" | "over"))
        }
        Expr::Intrinsic(_, name) => {
            matches!(name.as_str(), "MERGE" | "OVER")
        }
        _ => false,
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
            "IS_NIL" | "ISNIL" | "NILP" => Some(PredicateKind::Nil),
            _ => None,
        },
        Expr::Var(_, Var::Bound(bv)) => match bv.name.as_deref() {
            Some("number?") => Some(PredicateKind::Number),
            Some("string?") => Some(PredicateKind::String),
            Some("symbol?") => Some(PredicateKind::Symbol),
            Some("bool?") => Some(PredicateKind::Bool),
            Some("list?") => Some(PredicateKind::List),
            Some("block?") => Some(PredicateKind::Block),
            Some("nil?") => Some(PredicateKind::Nil),
            Some("non-nil?") => Some(PredicateKind::NonNil),
            Some("null?") => Some(PredicateKind::Null),
            _ => None,
        },
        // Free variable — occurs in prelude-cached checks where
        // prelude predicates have not been merged in (§B7).
        Expr::Var(_, Var::Free(name)) => match name.as_str() {
            "number?" => Some(PredicateKind::Number),
            "string?" => Some(PredicateKind::String),
            "symbol?" => Some(PredicateKind::Symbol),
            "bool?" => Some(PredicateKind::Bool),
            "list?" => Some(PredicateKind::List),
            "block?" => Some(PredicateKind::Block),
            "nil?" => Some(PredicateKind::Nil),
            "non-nil?" => Some(PredicateKind::NonNil),
            "null?" => Some(PredicateKind::Null),
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
        // Free variable — prelude-cached mode.
        Expr::Var(_, Var::Free(name)) => matches!(name.as_str(), "not" | "¬"),
        _ => false,
    }
}

/// Subtract `remove` from `ty`, returning the remaining type.
///
/// For union types, removes every variant that is a subtype of `remove`.
/// For singleton types equal to `remove`, returns `Never`.
/// For `any` or other opaque types, returns the original type unchanged
/// (the gradual boundary is preserved — §A5.10).
/// Narrow a list type to its non-empty refinement (§A6.4).
///
/// `List(a)` → `NonEmpty(a)`.  Any other type (including `NonEmpty`, `Tuple`,
/// and `any`) is returned unchanged — the gradual boundary is preserved.
fn narrow_to_nonempty(ty: &Type) -> Type {
    if let Some(elem) = ty.as_list() {
        Type::non_empty(elem.clone())
    } else {
        ty.clone()
    }
}

/// Which projection is being applied to a list/tuple.
#[derive(Debug, Clone, Copy)]
enum HeadTailProjection {
    Head,
    Tail,
}

/// Recognise `head` / `tail` applied in user code.
///
/// Returns `Some(projection)` for:
/// - `Expr::Intrinsic` with name `"HEAD"` or `"TAIL"`.
/// - `Expr::Var(Bound)` whose name is `"head"` / `"first"` / `"key"` / `"tail"`.
/// - `Expr::Var(Free)` with the same names, except `"key"` (prelude-cached mode).
fn is_head_or_tail(func: &RcExpr) -> Option<HeadTailProjection> {
    match &*func.inner {
        Expr::Intrinsic(_, name) => match name.as_str() {
            "HEAD" => Some(HeadTailProjection::Head),
            "TAIL" => Some(HeadTailProjection::Tail),
            _ => None,
        },
        Expr::Var(_, Var::Bound(bv)) => match bv.name.as_deref() {
            Some("head" | "first" | "key") => Some(HeadTailProjection::Head),
            Some("tail") => Some(HeadTailProjection::Tail),
            _ => None,
        },
        // Free variable — prelude-cached mode.
        Expr::Var(_, Var::Free(name)) => match name.as_str() {
            "head" | "first" => Some(HeadTailProjection::Head),
            "tail" => Some(HeadTailProjection::Tail),
            _ => None,
        },
        _ => None,
    }
}

/// Return `true` when `expr` is the lookup function.
///
/// Matches:
/// - `Expr::Intrinsic(_, "LOOKUP")` — the raw intrinsic.
/// - `Expr::Var(Bound, "lookup" | "lookup-in")` — the prelude aliases.
fn is_lookup_fn(expr: &RcExpr) -> bool {
    let peeled = peel_meta(expr);
    match &*peeled.inner {
        Expr::Intrinsic(_, name) => name == "LOOKUP",
        Expr::Var(_, Var::Bound(bv)) => matches!(bv.name.as_deref(), Some("lookup" | "lookup-in")),
        _ => false,
    }
}

/// Return `true` when `expr` is a HEAD-like function (HEAD intrinsic or a bound
/// variable named `"head"`, `"first"`, or `"key"`).
fn is_head_fn(expr: &RcExpr) -> bool {
    matches!(is_head_or_tail(expr), Some(HeadTailProjection::Head))
}

/// Return `true` when `expr` is a TAIL-like function (TAIL intrinsic or a bound
/// variable named `"tail"`).
fn is_tail_fn(expr: &RcExpr) -> bool {
    matches!(is_head_or_tail(expr), Some(HeadTailProjection::Tail))
}

/// Apply a `head`/`tail` projection to a known `Tuple` type.
///
/// - `head(Tuple([T₀, …])) → T₀`
/// - `tail(Tuple([T₀, T₁, …])) → Tuple([T₁, …])`
/// - `tail(Tuple([T₀])) → List(Any)` (tail of a 1-element list is the empty list)
///
/// Returns `None` when `arg_type` is not a `Tuple` (caller falls through to the
/// generic annotation path).
fn apply_head_tail_to_tuple(proj: HeadTailProjection, arg_type: &Type) -> Option<Type> {
    if let Type::Tuple(elems) = arg_type {
        match proj {
            HeadTailProjection::Head => elems.first().cloned(),
            HeadTailProjection::Tail => {
                if elems.len() <= 1 {
                    // tail of a singleton (or degenerate zero-elem) tuple is the empty list.
                    Some(Type::list(Type::Any))
                } else {
                    Some(Type::Tuple(elems[1..].to_vec()))
                }
            }
        }
    } else {
        None
    }
}

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

/// Build a union type from two branch result types.
///
/// Delegates to `Type::union` which deduplicates, absorbs literals into
/// their base types, and normalises singleton unions.
fn make_union_type(a: Type, b: Type) -> Type {
    Type::union([a, b])
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
        // Free variable — prelude-cached mode.
        Expr::Var(_, Var::Free(name)) => matches!(name.as_str(), "=>" | "⇒"),
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
        Type::Var(id, _) => id.0 == name,
        Type::App(f, x) => contains_var_named(f, name) || contains_var_named(x, name),
        Type::Con(_) => false,
        Type::Forall(binders, body) => {
            // Binders shadow the name — if any binder matches, it's not free.
            if binders.iter().any(|(b, _)| b.0 == name) {
                false
            } else {
                contains_var_named(body, name)
            }
        }
        Type::Function(a, b) => contains_var_named(a, name) || contains_var_named(b, name),
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

/// Maximum number of elements for which a non-empty list literal synthesises
/// as a `Tuple` (§A6.3).  Beyond this cap the type becomes `NonEmpty`.
const LIST_TUPLE_CAP: usize = 16;

/// Synthesise the type of a list literal from its element types.
///
/// Rules (§A6.3):
/// - `[]` → `List(never)` — definitively empty; `head` on it always warns.
/// - 1..=`LIST_TUPLE_CAP` elements → `Tuple(elem_types)` (preserves positions).
/// - >`LIST_TUPLE_CAP` elements → `NonEmpty(<deduplicated element union>)`.
///
/// Every non-empty result is a subtype of `NonEmpty`, so `head`/`tail` accept it.
fn synthesise_list_literal(elem_types: Vec<Type>) -> Type {
    let n = elem_types.len();
    if n == 0 {
        // `[]` is definitively empty — its element type is `Never`.
        // This causes `head([])` to synthesise `Never`, which is not
        // consistent with any concrete annotation → triggers a warning.
        Type::list(Type::Never)
    } else if n <= LIST_TUPLE_CAP {
        // Precise tuple: preserves arity and per-position types.
        Type::Tuple(elem_types)
    } else {
        // Beyond the cap: lose positional information but keep non-emptiness.
        Type::non_empty(Type::union(elem_types))
    }
}

/// Synthesise a homogeneous list type from a collection of element types.
///
/// Used by paths that produce a `List` without knowing its length
/// (e.g. the result of `map`).  Element types are unioned; an empty
/// collection gives `List(any)`.
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

/// Extract the `__type_hint` string and inner value from a `Meta` node.
///
/// Returns `(hint_str, inner)` when `expr` is `Meta(_, inner, {__type_hint: "..."})`,
/// `None` otherwise.  Used by the §A10 monadic bind element-type pre-pass.
fn extract_monad_hint_inner(expr: &RcExpr) -> Option<(String, &RcExpr)> {
    if let Expr::Meta(_, inner, meta) = &*expr.inner {
        if let Expr::Block(_, block) = &*meta.inner {
            if let Some(hint_expr) = block.get("__type_hint") {
                if let Some(hint_str) = extract_string_literal(hint_expr) {
                    return Some((hint_str, inner));
                }
            }
        }
    }
    None
}

/// Normalise a homogeneous `Tuple` to a `List` of the common element type.
///
/// Replicates the logic in `synthesise_meta`'s `is_hint` branch so that
/// `[1, 2, 3]`, which synthesises as `Tuple([number, number, number])`, is
/// treated as `List(number)` when matching against a monad wrapper type `[a]`.
///
/// Heterogeneous tuples become `List(union(elems))`.  Non-tuple types are
/// returned unchanged.
fn normalise_tuple_to_list(ty: Type) -> Type {
    match &ty {
        Type::Tuple(elems) if !elems.is_empty() => {
            let first = &elems[0];
            if elems.iter().all(|e| e == first) {
                Type::list(first.clone())
            } else {
                let elem_type = Type::union(elems.iter().cloned());
                Type::list(elem_type)
            }
        }
        _ => ty,
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

/// Returns `true` when `ty` is a partial type — i.e. a union that includes
/// `ExecutionError` at the top level.
///
/// Used by `check_against` to warn when a partial result flows into a position
/// annotated with a total type (§B5.3).
fn is_partial_type(ty: &Type) -> bool {
    if let Type::Union(variants) = ty {
        variants.contains(&Type::ExecutionError)
    } else {
        false
    }
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

// ── Constraint discharge helpers ─────────────────────────────────────────────

/// Return `true` if any overload of `op_type` accepts `args` as positional
/// parameters.
///
/// - `Union` types are tried variant-by-variant (any match succeeds).
/// - `Function(p, rest)` checks that `args[0]` is consistent with `p`, then
///   recurses with `args[1..]` and `rest`.
/// - An empty `args` slice is vacuously satisfied by any type.
fn constraint_overload_matches(op_type: &Type, args: &[Type]) -> bool {
    use crate::core::typecheck::subtype::is_consistent;
    match op_type {
        Type::Union(variants) => variants
            .iter()
            .any(|v| constraint_overload_matches(v, args)),
        Type::Function(param, rest) => {
            if args.is_empty() {
                return true;
            }
            if !is_consistent(param, &args[0]) {
                return false;
            }
            constraint_overload_matches(rest, &args[1..])
        }
        _ => args.is_empty(),
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
    /// Type alias definitions registered during type checking.
    ///
    /// Maps alias name (e.g. `"Point"`) to its resolved `Type`.  Used
    /// by the LSP hover provider to show the resolved type for alias
    /// references in `type:` annotation strings.
    pub aliases: HashMap<String, Type>,
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

/// Check the prelude expression and capture a `PreludeSummary` for caching.
///
/// The checker's `keep_root_scope` flag is set so that the outermost
/// Let frame is not popped on exit, allowing `extract_prelude_summary`
/// to read the binding type schemes.
///
/// Called once per process to build the prelude cache.
pub fn type_check_for_prelude(expr: &RcExpr) -> (Vec<TypeWarning>, PreludeSummary) {
    let mut checker = Checker::new();
    checker.keep_root_scope = true;
    checker.check_expr(expr);
    let summary = checker.extract_prelude_summary();
    let warnings = checker.into_warnings();
    (warnings, summary)
}

/// Check `expr` seeded with a cached prelude summary.
///
/// The summary's bindings populate the outermost scope frame and its
/// aliases seed the alias map, so `Var::Free` references to prelude
/// names resolve correctly without re-checking the prelude.
///
/// Returns only the warnings for the user expression (not the prelude).
pub fn type_check_with_seed(expr: &RcExpr, summary: &PreludeSummary) -> Vec<TypeWarning> {
    let mut checker = Checker::with_seed(summary);
    checker.check_expr(expr);
    checker.into_warnings()
}

/// Run the type checker over `expr` with pre-cook operator type overloads.
///
/// `operator_overloads` maps operator names (e.g. `"<"`, `">"`) to their
/// annotated `TypeScheme`s, extracted from the raw (pre-cook) expression.
/// The checker consults this map in `discharge_constraint` when the normal
/// scope-stack lookup returns `Any` (because cook stripped the operator's
/// `Meta` annotation).
pub fn type_check_with_operator_overloads(
    expr: &RcExpr,
    operator_overloads: HashMap<String, TypeScheme>,
) -> Vec<TypeWarning> {
    let mut checker = Checker::new();
    checker.operator_overloads = operator_overloads;
    checker.check_expr(expr);
    checker.into_warnings()
}

/// Parse a map of operator name → annotation string into TypeSchemes.
///
/// Strings that fail to parse are silently skipped (the constraint will
/// stay gradual for those operators).
pub fn parse_operator_overloads(raw: &HashMap<String, String>) -> HashMap<String, TypeScheme> {
    raw.iter()
        .filter_map(|(name, type_str)| {
            let (ty, constraints) = parse::parse_scheme(type_str).ok()?;
            let mut scheme = infer_scheme(ty);
            scheme.constraints = constraints;
            Some((name.clone(), scheme))
        })
        .collect()
}

/// Extract type alias definitions from `expr` without running full type checking.
///
/// Walks all `Meta` nodes and registers any `types:` block entries found,
/// then returns the alias map.  Cheaper than `type_check_full` — intended
/// for use on the pre-pruned expression where aliases may be stripped by
/// dead-code elimination before the full type-check pass.
pub fn extract_aliases(expr: &RcExpr) -> HashMap<String, Type> {
    let mut checker = Checker::new();
    checker.walk_meta_for_aliases(expr);
    checker.aliases
}

impl Checker {
    /// Walk `expr` recursively, registering any `types:` aliases found in
    /// `Meta` nodes.  Does not perform type inference — only alias collection.
    fn walk_meta_for_aliases(&mut self, expr: &RcExpr) {
        match &*expr.inner {
            Expr::Meta(_, inner, meta) => {
                self.register_aliases_from_meta(meta);
                self.walk_meta_for_aliases(inner);
                self.walk_meta_for_aliases(meta);
            }
            Expr::Let(_, scope, _) => {
                for (_, value) in &scope.pattern {
                    self.walk_meta_for_aliases(value);
                }
                self.walk_meta_for_aliases(&scope.body);
            }
            Expr::App(_, f, args) => {
                self.walk_meta_for_aliases(f);
                for a in args {
                    self.walk_meta_for_aliases(a);
                }
            }
            Expr::Lam(_, _, scope) => {
                self.walk_meta_for_aliases(&scope.body);
            }
            Expr::Block(_, bindings) => {
                for e in bindings.values() {
                    self.walk_meta_for_aliases(e);
                }
            }
            _ => {}
        }
    }
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
    fn empty_list_has_never_element_type() {
        // `[]` synthesises as `List(Never)` — definitively empty.
        // This ensures `head([])` triggers a type warning rather than
        // silently returning `Any`.
        let mut c = Checker::new();
        assert_eq!(c.synthesise(&list(vec![])), Type::list(Type::Never));
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
    fn large_list_synthesises_as_tuple_or_nonempty() {
        let mut c = Checker::new();
        // 5-element list: within LIST_TUPLE_CAP (16) → Tuple.
        let l5 = list(vec![
            num_lit(1),
            num_lit(2),
            num_lit(3),
            num_lit(4),
            num_lit(5),
        ]);
        assert_eq!(c.synthesise(&l5), Type::Tuple(vec![Type::Number; 5]));

        // >16-element list: beyond cap → NonEmpty(element union).
        let l17: Vec<_> = (0..17).map(|_| num_lit(1)).collect();
        assert_eq!(c.synthesise(&list(l17)), Type::non_empty(Type::Number));
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
        assert_eq!(c.synthesise(&expr), Type::list(Type::String));
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
                Box::new(Type::var(TypeVarId("a".to_string()))),
                Box::new(Type::var(TypeVarId("a".to_string()))),
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
                    Box::new(Type::var(a.clone())),
                    Box::new(Type::var(b.clone())),
                )),
                Box::new(Type::Function(
                    Box::new(Type::list(Type::var(a.clone()))),
                    Box::new(Type::list(Type::var(b.clone()))),
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
        assert_eq!(result, Type::list(Type::Number));
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
                    Box::new(Type::var(a.clone())),
                    Box::new(Type::var(b.clone())),
                )),
                Box::new(Type::Function(
                    Box::new(Type::list(Type::var(a.clone()))),
                    Box::new(Type::list(Type::var(b.clone()))),
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
                Box::new(Type::var(TypeVarId("a".into()))),
                Box::new(Type::var(TypeVarId("b".into()))),
            )),
            Box::new(Type::Function(
                Box::new(Type::list(Type::var(TypeVarId("a".into())))),
                Box::new(Type::list(Type::var(TypeVarId("b".into())))),
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

    // ── §A10: Monadic bound-variable element-type hints ──────────────────────

    /// Helper: make `App(Name("bind"), [Meta(inner, {__type_hint: hint}), Lam(x, body)])`
    /// which is the shape produced by `desugar_monadic_block`.
    fn monadic_bind_app(hint: &str, inner: RcExpr, lam_body: RcExpr) -> RcExpr {
        use crate::core::expr::core;
        let hint_val = core::str(Smid::default(), hint);
        let meta_block = core::block(Smid::default(), [("__type_hint".to_string(), hint_val)]);
        let value = core::meta(Smid::default(), inner, meta_block);
        let lam = RcExpr::from(Expr::Lam(
            Smid::default(),
            false,
            crate::core::expr::LamScope {
                pattern: vec!["x".to_string()],
                body: lam_body,
            },
        ));
        let bind_fn = RcExpr::from(Expr::Name(Smid::default(), "bind".to_string()));
        RcExpr::from(Expr::App(Smid::default(), bind_fn, vec![value, lam]))
    }

    #[test]
    fn a10_infer_elem_type_from_list_hint() {
        // `infer_elem_type_from_hint_str("[a]", List(number))` → `Some(number)`.
        let mut c = Checker::new();
        let val_type = Type::list(Type::Number);
        let result = c.infer_elem_type_from_hint_str("[a]", &val_type);
        assert_eq!(result, Some(Type::Number));
    }

    #[test]
    fn a10_infer_elem_type_from_io_hint() {
        // `infer_elem_type_from_hint_str("IO(a)", IO(string))` → `Some(string)`.
        let mut c = Checker::new();
        let val_type = Type::io(Type::String);
        let result = c.infer_elem_type_from_hint_str("IO(a)", &val_type);
        assert_eq!(result, Some(Type::String));
    }

    #[test]
    fn a10_infer_elem_type_no_match() {
        // Hint wrapper does not match concrete type → `None`.
        let mut c = Checker::new();
        // Hint says "[a]" (list) but val_type is number — unification fails.
        let result = c.infer_elem_type_from_hint_str("[a]", &Type::Number);
        assert_eq!(result, None);
    }

    #[test]
    fn a10_normalise_homogeneous_tuple_to_list() {
        // `[1, 2, 3]` synthesises as `Tuple([number, number, number])`.
        // `normalise_tuple_to_list` should give `List(number)`.
        let tuple = Type::Tuple(vec![Type::Number, Type::Number, Type::Number]);
        assert_eq!(normalise_tuple_to_list(tuple), Type::list(Type::Number));
    }

    #[test]
    fn a10_normalise_non_tuple_unchanged() {
        // Non-tuple types pass through unchanged.
        assert_eq!(
            normalise_tuple_to_list(Type::list(Type::Number)),
            Type::list(Type::Number)
        );
        assert_eq!(normalise_tuple_to_list(Type::Number), Type::Number);
    }

    #[test]
    fn a10_monadic_bind_lambda_params_recorded() {
        // The A10 pre-pass should record `x: number` in `lambda_params` when
        // the monadic bind pattern is `App(bind, [Meta([1,2,3], {__type_hint: "[a]"}), Lam(x, body)])`.
        let mut c = Checker::new();
        // Use `num_lit(0)` as the body — its synthesis doesn't affect param recording.
        let inner = list(vec![num_lit(1), num_lit(2), num_lit(3)]);
        let app = monadic_bind_app("[a]", inner, num_lit(0));
        c.synthesise(&app);

        // The lambda's Smid is Smid::default() (from our helper).
        let lam_smid = Smid::default();
        let params = c.lambda_params.get(&lam_smid);
        assert!(
            params.is_some(),
            "lambda_params should be recorded for the monadic lambda"
        );
        let params = params.unwrap();
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "x");
        assert_eq!(params[0].1, Type::Number, "x should be inferred as number");
    }

    #[test]
    fn contains_var_named_in_nonempty() {
        // NonEmpty(X) should detect X as a free variable.
        let ty = Type::non_empty(Type::var(TypeVarId("X".to_string())));
        assert!(contains_var_named(&ty, "X"));
        assert!(!contains_var_named(&ty, "Y"));
    }

    #[test]
    fn a10_extract_monad_hint_inner_basic() {
        // `extract_monad_hint_inner(Meta(num, {__type_hint: "[a]"}))` → `Some(("[a]", num))`.
        let meta_expr = {
            let hint_val = core::str(Smid::default(), "[a]");
            let meta_block = core::block(Smid::default(), [("__type_hint".to_string(), hint_val)]);
            core::meta(Smid::default(), num_lit(42), meta_block)
        };
        let result = extract_monad_hint_inner(&meta_expr);
        assert!(result.is_some(), "should extract hint from Meta node");
        let (hint, _inner) = result.unwrap();
        assert_eq!(hint, "[a]");
    }
}
