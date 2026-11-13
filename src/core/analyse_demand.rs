//! Backward demand analysis over core expressions.
//!
//! Walks the core expression tree bottom-up, computing a demand
//! environment (map from bound variable identities to their aggregate
//! demand) and populating `CoreBinding.demand` on every binding.
//!
//! Also produces a signature side-table mapping lambda body pointers
//! to per-argument demand vectors for use by the STG compiler.

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::common::sourcemap::Smid;
use crate::core::binding::{CoreBinding, Scope, Var};
use crate::core::demand::{Cardinality, Demand, Strictness};
use crate::core::expr::*;
use crate::eval::intrinsics;

/// A demand environment: maps `(scope, binder)` pairs to their
/// aggregate demand within a given expression.
///
/// The keys are `(scope, binder)` de Bruijn coordinates as seen from
/// the expression that produced this environment.
type DemandEnv = HashMap<(u32, u32), Demand>;

/// Key for the signature side-table: raw pointer to the `Rc` data of
/// a `Lam` scope body's inner `CoreExpr`.
type SigKey = usize;

/// Per-function demand signature: one `Demand` per parameter.
pub type DemandSignature = Vec<Demand>;

/// The demand signature side-table produced by the analysis.
///
/// Keyed by the raw pointer of the `Lam` body's `Rc<CoreExpr>` inner
/// data, mapping to per-argument demand vectors.
pub type SignatureTable = HashMap<SigKey, DemandSignature>;

/// Name-keyed demand signature table for user function calls.
///
/// Maps binding names to per-argument demand vectors. Built during
/// `analyse_let` by recording the signature of each Let binding whose
/// RHS is a Lam with a known signature. Used by the STG compiler to
/// apply per-argument demands at user function call sites.
pub type NamedSignatureTable = HashMap<String, DemandSignature>;

/// Peel `Meta` wrappers from an expression, returning the innermost non-Meta
/// node. Source-path bindings (every prelude combinator, and any
/// user-annotated binding) are `Meta(Lam(...), {doc, type})`, not a bare
/// `Lam`; used to look through that wrapper when recording named demand
/// signatures (see `analyse_let`).
fn peel_meta(expr: &RcExpr) -> &RcExpr {
    match &*expr.inner {
        Expr::Meta(_, inner, _) => peel_meta(inner),
        _ => expr,
    }
}

/// Names whose name-keyed signature would not be sound for every call
/// that uses the name: names bound anywhere as a lambda parameter or as a
/// let binding whose right-hand side is not a lambda (an alias such as
/// `step: f` may refer to a lazy function), and names bound to lambdas of
/// different arities. A parameter counts even when it is the name's only
/// binding: in blob mode the prelude is not in the tree, and a call to a
/// parameter named like a prelude function must not take its signature. Let-bound lambdas of one arity remain merged by
/// `record_named_signature`, whose lub is sound for each of them.
fn ambiguous_names(expr: &RcExpr) -> HashSet<String> {
    #[derive(Default)]
    struct Uses {
        /// Arities of let-bound lambdas with this name.
        arities: HashSet<usize>,
        /// Bound as a parameter or as a non-lambda let binding.
        other: bool,
    }

    fn visit(expr: &RcExpr, uses: &mut HashMap<String, Uses>) {
        match &*expr.inner {
            Expr::Let(_, scope, _) => {
                for binding in &scope.pattern {
                    let entry = uses.entry(binding.name.clone()).or_default();
                    match &*peel_meta(&binding.expr).inner {
                        Expr::Lam(_, _, lam) => {
                            entry.arities.insert(lam.pattern.len());
                        }
                        _ => entry.other = true,
                    }
                    visit(&binding.expr, uses);
                }
                visit(&scope.body, uses);
            }
            Expr::Lam(_, _, scope) => {
                for param in &scope.pattern {
                    let entry = uses.entry(param.clone()).or_default();
                    entry.other = true;
                }
                visit(&scope.body, uses);
            }
            Expr::Lookup(_, e, _, fallback) => {
                visit(e, uses);
                if let Some(fb) = fallback {
                    visit(fb, uses);
                }
            }
            Expr::List(_, xs) | Expr::ArgTuple(_, xs) | Expr::Soup(_, xs, _) => {
                xs.iter().for_each(|x| visit(x, uses));
            }
            Expr::Block(_, block_map) => {
                block_map.iter().for_each(|(_, v)| visit(v, uses));
            }
            Expr::Meta(_, e, m) => {
                visit(e, uses);
                visit(m, uses);
            }
            Expr::App(_, f, xs) => {
                visit(f, uses);
                xs.iter().for_each(|x| visit(x, uses));
            }
            Expr::Operator(_, _, _, e) => visit(e, uses),
            _ => {}
        }
    }

    let mut uses = HashMap::new();
    visit(expr, &mut uses);
    uses.into_iter()
        .filter(|(_, u)| u.other || u.arities.len() > 1)
        .map(|(name, _)| name)
        .collect()
}

/// Build intrinsic seed signatures from the INTRINSICS catalogue.
///
/// For each intrinsic, strict arguments get `(Strict, AtMostOnce)` per
/// the blanket rule. Non-strict arguments get `(Lazy, Multi)`.
/// Special refinements are applied for IF, AND, OR, LOOKUPOR, IO_BIND.
///
/// This function is also called by the STG compiler to populate its
/// per-intrinsic demand table, replacing the `strict_args` consultation.
pub fn build_intrinsic_signatures() -> HashMap<String, DemandSignature> {
    let mut sigs = HashMap::new();
    for info in intrinsics::catalogue() {
        let arity = info.arity();
        if arity == 0 {
            continue;
        }
        let strict_set = info.strict_indices();
        let mut sig = Vec::with_capacity(arity);
        for i in 0..arity {
            if strict_set.contains(&i) {
                sig.push(Demand::strict_once());
            } else {
                sig.push(Demand::lazy_multi());
            }
        }
        sigs.insert(info.name().to_string(), sig);
    }

    // Special refinements per spec
    // IF: condition strict+once, then/else lazy+once
    sigs.insert(
        "IF".to_string(),
        vec![
            Demand::strict_once(),
            Demand::lazy_once(),
            Demand::lazy_once(),
        ],
    );

    // AND/OR: first strict+once, second lazy+once (short-circuit)
    sigs.insert(
        "AND".to_string(),
        vec![Demand::strict_once(), Demand::lazy_once()],
    );
    sigs.insert(
        "OR".to_string(),
        vec![Demand::strict_once(), Demand::lazy_once()],
    );

    // LOOKUPOR: obj strict+once, key strict+once, fallback lazy+once, (extra arg)
    if let Some(sig) = sigs.get_mut("LOOKUPOR") {
        // LOOKUPOR has 4 args: object(0), key(1), fallback(2), extra(3)
        // object and key are strict; fallback is lazy+once
        if sig.len() >= 3 {
            sig[2] = Demand::lazy_once();
        }
    }
    if let Some(sig) = sigs.get_mut("LOOKUPOR#") {
        if sig.len() >= 3 {
            sig[2] = Demand::lazy_once();
        }
    }

    // IO_BIND: action strict+once, continuation lazy+once
    sigs.insert(
        "IO_BIND".to_string(),
        vec![
            Demand::strict_once(),
            Demand::lazy_once(),
            Demand::lazy_multi(),
            Demand::lazy_multi(),
        ],
    );

    sigs
}

/// Build demand signatures for common prelude functions.
///
/// In blob-prelude mode, the prelude is pre-compiled and its functions
/// appear as globals (`Ref::G`) with no demand analysis.  This table
/// provides conservative strictness information for the most common
/// prelude functions so that the STG compiler can apply strict-arg
/// optimisation (Seq wrapping) to complex arguments at call sites.
///
/// We use `sm()` (Strict + Multi) for strict arguments
/// because we do not know the exact cardinality within the prelude
/// function body.  Multi ensures the binding becomes a Thunk (with
/// Update/memoisation) and Strict triggers Seq wrapping for eager
/// evaluation.
pub fn build_prelude_signatures() -> HashMap<String, DemandSignature> {
    let mut sigs = HashMap::new();
    let sm = Demand::strict_multi;
    let lm = Demand::lazy_multi;

    // List operations — strict in all arguments
    // map(f, l): f applied to each element, l traversed
    sigs.insert("map".into(), vec![sm(), sm()]);
    // map2(f, l, m): zip-with
    sigs.insert("map2".into(), vec![sm(), sm(), sm()]);
    // filter(p?, l): predicate applied, list traversed
    sigs.insert("filter".into(), vec![sm(), sm()]);
    // foldl(op, i, l): all args needed
    sigs.insert("foldl".into(), vec![sm(), sm(), sm()]);
    // foldr(op, i, l): all args needed
    sigs.insert("foldr".into(), vec![sm(), sm(), sm()]);
    // take(n, l): count and list both needed
    sigs.insert("take".into(), vec![sm(), sm()]);
    // drop(n, l): count and list both needed
    sigs.insert("drop".into(), vec![sm(), sm()]);
    // nth(n, l): index and list both needed
    sigs.insert("nth".into(), vec![sm(), sm()]);
    // reverse(l): list needed
    sigs.insert("reverse".into(), vec![sm()]);
    // append(l1, l2): both lists needed
    sigs.insert("append".into(), vec![sm(), sm()]);
    // zip-with(f, a, b): all needed
    sigs.insert("zip-with".into(), vec![sm(), sm(), sm()]);
    // zip(a, b): both needed (zip = zip-with(pair))
    sigs.insert("zip".into(), vec![sm(), sm()]);
    // count(l): list traversed
    sigs.insert("count".into(), vec![sm()]);
    // sum(l): list traversed
    sigs.insert("sum".into(), vec![sm()]);
    // product(l): list traversed
    sigs.insert("product".into(), vec![sm()]);
    // cons(h, t): lazy in both — wraps __CONS which does not force args.
    // Strictness here would cause infinite forcing for recursive list
    // construction (iterate, map, scanl, etc.) over infinite lists.
    sigs.insert("cons".into(), vec![lm(), lm()]);
    // snoc(x, l): both needed
    sigs.insert("snoc".into(), vec![sm(), sm()]);
    // mapcat(f, l): catenation applied via compose, but f is strict
    sigs.insert("mapcat".into(), vec![sm()]);
    // cross(f, xs, ys): all needed
    sigs.insert("cross".into(), vec![sm(), sm(), sm()]);
    // interleave(a, b): both needed
    sigs.insert("interleave".into(), vec![sm(), sm()]);

    // take-while / drop-while: predicate and list both strict
    sigs.insert("take-while".into(), vec![sm(), sm()]);
    sigs.insert("drop-while".into(), vec![sm(), sm()]);

    // Sorting
    sigs.insert("sort-by".into(), vec![sm(), sm(), sm()]);
    sigs.insert("sort-by-num".into(), vec![sm()]);
    sigs.insert("sort-by-str".into(), vec![sm()]);
    sigs.insert("sort-nums".into(), vec![sm()]);
    sigs.insert("sort-strs".into(), vec![sm()]);

    // Grouping
    sigs.insert("group-by".into(), vec![sm(), sm()]);
    sigs.insert("discriminate".into(), vec![sm(), sm()]);
    sigs.insert("group-consecutive-by".into(), vec![sm(), sm()]);

    // Numeric — strict in all args
    sigs.insert("abs".into(), vec![sm()]);
    sigs.insert("negate".into(), vec![sm()]);
    sigs.insert("max".into(), vec![sm(), sm()]);
    sigs.insert("min".into(), vec![sm(), sm()]);
    sigs.insert("max-of".into(), vec![sm()]);
    sigs.insert("min-of".into(), vec![sm()]);
    sigs.insert("pow".into(), vec![sm(), sm()]);
    sigs.insert("div".into(), vec![sm(), sm()]);
    sigs.insert("mod".into(), vec![sm(), sm()]);
    sigs.insert("quot".into(), vec![sm(), sm()]);
    sigs.insert("rem".into(), vec![sm(), sm()]);

    // Combinators — strict in function arg, lazy in data args
    sigs.insert("compose".into(), vec![sm(), sm(), sm()]);
    sigs.insert("flip".into(), vec![sm(), sm(), sm()]);
    sigs.insert("identity".into(), vec![sm()]);
    sigs.insert("apply".into(), vec![sm(), sm()]);

    // Conditionals — condition strict, branches lazy
    sigs.insert("then".into(), vec![lm(), lm(), sm()]);
    sigs.insert("when".into(), vec![sm(), sm(), sm()]);

    // Lookup functions — strict in key/object args
    sigs.insert("has".into(), vec![sm(), sm()]);
    sigs.insert("lookup".into(), vec![sm(), sm()]);
    sigs.insert("lookup-in".into(), vec![sm(), sm()]);
    sigs.insert("lookup-or".into(), vec![sm(), lm(), sm()]);

    // Deep operations — strict in key/pattern and block
    sigs.insert("deep-find".into(), vec![sm(), sm()]);
    sigs.insert("deep-transform".into(), vec![sm(), sm()]);
    sigs.insert("deep-query".into(), vec![sm(), sm()]);

    // Head/tail — strict in list
    sigs.insert("head-or".into(), vec![sm(), sm()]);
    sigs.insert("tail-or".into(), vec![sm(), sm()]);

    // Rendering
    sigs.insert("render".into(), vec![sm()]);
    sigs.insert("render-as".into(), vec![sm(), sm()]);
    sigs.insert("parse-as".into(), vec![sm(), sm()]);

    // Iterate — strict in initial value, function
    sigs.insert("iterate".into(), vec![sm(), sm()]);
    // range(b, e)
    sigs.insert("range".into(), vec![sm(), sm()]);
    // iota(n)
    sigs.insert("iota".into(), vec![sm()]);
    // repeat(i)
    sigs.insert("repeat".into(), vec![sm()]);

    // Sliding / windowing
    sigs.insert("over-sliding-pairs".into(), vec![sm(), sm()]);

    sigs
}

/// Return the indices of strict arguments for the named intrinsic,
/// derived from the demand signature table.
///
/// Used by `wrap.rs` to generate forcing STG wrappers, replacing the
/// old `Intrinsic::strict_args()` consultation.
pub fn strict_indices_for(name: &str) -> Vec<usize> {
    use crate::core::demand::Strictness;
    build_intrinsic_signatures()
        .get(name)
        .map(|sig| {
            sig.iter()
                .enumerate()
                .filter(|(_, d)| d.strictness == Strictness::Strict)
                .map(|(i, _)| i)
                .collect()
        })
        .unwrap_or_default()
}

/// Remove entries for scope 0 (current scope) and decrement all
/// remaining scope indices by 1. Used when leaving a scope.
fn unshift_env(env: &DemandEnv) -> DemandEnv {
    env.iter()
        .filter(|&(&(scope, _), _)| scope > 0)
        .map(|(&(scope, binder), &demand)| ((scope - 1, binder), demand))
        .collect()
}

/// Find the root unit's block: follow `Let` and `Meta` bodies from the
/// analysed expression to the `Block` they end in, if any.
fn root_block(expr: &RcExpr) -> Option<usize> {
    match &*expr.inner {
        Expr::Let(_, scope, _) => root_block(&scope.body),
        Expr::Meta(_, e, _) => root_block(e),
        Expr::Block(_, _) => Some(Rc::as_ptr(&expr.inner) as usize),
        _ => None,
    }
}

/// The demand a data constructor (block field, list element) places on
/// the free variables of its contents.
///
/// The constructor does not evaluate its contents, so every demand is
/// weakened to lazy. And once built, the structure can be read any number
/// of times, so a used variable is demanded Multi: a syntactic AtMostOnce
/// there would let the STG compiler skip the update frame and recompute
/// the binding on every read, e.g. `{ m: slow(n) }.([m])` (eu-oaz72.40).
fn constructor_env(env: DemandEnv) -> DemandEnv {
    env.into_iter()
        .map(|(k, d)| {
            let cardinality = match d.cardinality {
                Cardinality::AtMostOnce => Cardinality::Multi,
                c => c,
            };
            (
                k,
                Demand {
                    strictness: Strictness::Lazy,
                    cardinality,
                    ..d
                },
            )
        })
        .collect()
}

/// The demand a lambda value places on the variables its body captures.
///
/// The lambda may be applied any number of times, so a used variable is
/// demanded Multi (eu-oaz72.41). And building the closure does not run its
/// body, which may never be applied, so every demand is lazy: carrying the
/// body's strictness out unscaled made `k(a): { y: • }.(a + y)` strict in
/// `a`, and `k(panic("x")) block?` forced the panic (eu-oaz72.27). Where
/// the lambda is applied, its signature gives the demand on its arguments;
/// what it captures stays lazy, a deliberate loss of precision (in
/// `{ b: n * 2  h(x): x + b }.(h(1))`, `b` is not found strict).
fn shared_env(env: DemandEnv) -> DemandEnv {
    env.into_iter()
        .map(|(k, d)| {
            let cardinality = match d.cardinality {
                Cardinality::AtMostOnce => Cardinality::Multi,
                c => c,
            };
            (
                k,
                Demand {
                    strictness: Strictness::Lazy,
                    cardinality,
                    ..d
                },
            )
        })
        .collect()
}

/// Signatures known for the binders of one de Bruijn scope.
///
/// The analyser keeps one frame per `Let` or `Lam` scope it is inside, so a
/// `Var::Bound` resolves to its frame by its scope index. A `Let` frame
/// holds the current signature of each binding whose RHS is a lambda; a
/// `Lam` frame holds none (parameters are not known functions).
struct SigFrame {
    sigs: Vec<Option<DemandSignature>>,
    /// Set when a call was analysed with one of this frame's signatures, so
    /// the result depends on them.
    consulted: bool,
}

/// Iterations of a `Let` group's signature fixpoint before falling back to
/// pessimistic signatures for its lambda bindings. Each iteration can only
/// weaken some parameter's demand (strict to lazy, once to multi), so a
/// group converges in a few; the cap bounds pathological cases.
const MAX_SIGNATURE_ITERATIONS: usize = 8;

/// Merge two demand environments, combining demands for the same
/// variable using `plus` (sequential composition).
fn merge_envs(a: &DemandEnv, b: &DemandEnv) -> DemandEnv {
    let mut result = a.clone();
    for (&key, &demand) in b {
        let entry = result.entry(key).or_insert(Demand::absent());
        *entry = entry.plus(demand);
    }
    result
}

/// The demand analyser.
pub struct DemandAnalyser {
    /// Intrinsic seed signatures.
    intrinsic_sigs: HashMap<String, DemandSignature>,
    /// Signatures discovered during analysis (keyed by Lam body pointer).
    signatures: SignatureTable,
    /// Name-keyed signatures for user function calls. The STG compiler
    /// looks them up by callee name, so bindings that share a name share
    /// an entry: it holds the lub of their signatures (sound for each), or
    /// `None` once two of them disagree in arity.
    named_signatures: HashMap<String, Option<DemandSignature>>,
    /// Previous values of `named_signatures` entries changed by the
    /// current fixpoint rounds, so a discarded round can be undone.
    named_log: Vec<(String, Option<Option<DemandSignature>>)>,
    /// The last signatures a `Let` group settled on, keyed by its scope's
    /// address in the input tree. An enclosing group re-analyses a nested
    /// one on each of its own rounds; starting from the previous answer
    /// lets a stable nested group settle in one round, rather than
    /// repeating its whole fixpoint, so cost does not multiply with nesting
    /// depth.
    seeds: HashMap<usize, Vec<Option<DemandSignature>>>,
    /// The root unit's block (pointer to its `Expr`), reached by following
    /// `Let` and `Meta` bodies from the analysed expression. It is the
    /// document RENDER_DOC renders, so its fields are all forced and keep
    /// strict demand; every other block's fields are lazy.
    root_block: Option<usize>,
    /// One frame per enclosing `Let`/`Lam` scope, innermost last.
    frames: Vec<SigFrame>,
    /// Keys inserted into `signatures`, in order, so that the entries
    /// recorded by a discarded fixpoint iteration can be removed: their
    /// expressions are dropped and the pointers may be reused.
    sig_log: Vec<SigKey>,
    /// Number of expressions analysed, counting re-analysis in later
    /// fixpoint rounds: the measure of the analysis' work.
    visits: usize,
}

impl Default for DemandAnalyser {
    fn default() -> Self {
        Self::new()
    }
}

impl DemandAnalyser {
    pub fn new() -> Self {
        DemandAnalyser {
            intrinsic_sigs: build_intrinsic_signatures(),
            signatures: HashMap::new(),
            named_signatures: HashMap::new(),
            named_log: Vec::new(),
            seeds: HashMap::new(),
            root_block: None,
            frames: Vec::new(),
            sig_log: Vec::new(),
            visits: 0,
        }
    }

    /// Run the demand analysis on the given expression, returning the
    /// annotated expression, the pointer-keyed signature table, and the
    /// name-keyed signature table for user function calls.
    pub fn analyse(mut self, expr: &RcExpr) -> (RcExpr, SignatureTable, NamedSignatureTable) {
        self.root_block = root_block(expr);
        let (new_expr, _env) = self.analyse_expr(expr);
        let mut named: NamedSignatureTable = self
            .named_signatures
            .into_iter()
            .filter_map(|(name, sig)| sig.map(|sig| (name, sig)))
            .collect();
        // The compiler finds a call's signature by the callee's name, so a
        // name that does not refer to let-bound lambdas of one arity alone
        // gets an empty signature: no argument is strict, and the empty
        // entry stops the compiler falling back to a prelude signature of
        // the same name.
        for name in ambiguous_names(expr) {
            named.insert(name, Vec::new());
        }
        (new_expr, self.signatures, named)
    }

    /// Analyse an expression, returning the annotated expression and
    /// the demand environment it places on its free variables.
    fn analyse_expr(&mut self, expr: &RcExpr) -> (RcExpr, DemandEnv) {
        self.visits += 1;
        match &*expr.inner {
            Expr::Var(_s, Var::Bound(bv)) => {
                let mut env = DemandEnv::new();
                // A variable reference places strict+once demand
                env.insert((bv.scope, bv.binder), Demand::strict_once());
                (expr.clone(), env)
            }

            Expr::Var(_, _) | Expr::Intrinsic(_, _) | Expr::Literal(_, _) => {
                (expr.clone(), DemandEnv::new())
            }

            Expr::Let(s, scope, let_type) => self.analyse_let(*s, scope, *let_type),

            Expr::Lam(s, inl, scope) => self.analyse_lam(*s, *inl, scope),

            Expr::App(s, f, args) => self.analyse_app(*s, f, args),

            Expr::Lookup(s, obj, key, fallback) => {
                // Object is strict+once: analyse it and collect its env.
                let (new_obj, mut env) = self.analyse_expr(obj);

                let new_fb = if let Some(fb) = fallback {
                    let (new_fb, fb_env) = self.analyse_expr(fb);
                    // Fallback is lazy+once: merge with main env.
                    env = merge_envs(&env, &fb_env);
                    Some(new_fb)
                } else {
                    None
                };

                let new_expr = RcExpr::from(Expr::Lookup(*s, new_obj, key.clone(), new_fb));
                (new_expr, env)
            }

            Expr::List(s, xs) => {
                // A list constructor does not force its elements: each
                // element's free-variable demands are lazy (eu-oaz72.40).
                let mut env = DemandEnv::new();
                let mut new_xs = Vec::with_capacity(xs.len());
                for x in xs {
                    let (new_x, x_env) = self.analyse_expr(x);
                    env = merge_envs(&env, &constructor_env(x_env));
                    new_xs.push(new_x);
                }
                (RcExpr::from(Expr::List(*s, new_xs)), env)
            }

            Expr::Block(s, block_map) => {
                let rendered = self.root_block == Some(Rc::as_ptr(&expr.inner) as usize);
                self.analyse_block(*s, block_map, rendered)
            }

            Expr::Meta(s, e, m) => {
                let (new_e, e_env) = self.analyse_expr(e);
                let (new_m, m_env) = self.analyse_expr(m);
                let env = merge_envs(&e_env, &m_env);
                (RcExpr::from(Expr::Meta(*s, new_e, new_m)), env)
            }

            Expr::ArgTuple(s, xs) => {
                let mut env = DemandEnv::new();
                let mut new_xs = Vec::with_capacity(xs.len());
                for x in xs {
                    let (new_x, x_env) = self.analyse_expr(x);
                    env = merge_envs(&env, &x_env);
                    new_xs.push(new_x);
                }
                (RcExpr::from(Expr::ArgTuple(*s, new_xs)), env)
            }

            // Remaining variants: pass through unchanged
            _ => (expr.clone(), DemandEnv::new()),
        }
    }

    /// Analyse a `Block` constructor.
    ///
    /// A block constructor does not force its field values: a field is
    /// evaluated only when it is looked up or rendered, so each value's
    /// free-variable demands are weakened to lazy. Treating them as strict
    /// forced a binding used only by an unselected field, e.g.
    /// `{ b: ds max-of  a: 1 }.a` (eu-oaz72.40): a block literal is a `Let`
    /// whose body is a `Block`, so a block body is not by itself evidence
    /// that the block is rendered. `rendered` is set only for the root
    /// unit's block, whose fields RENDER_DOC forces, and keeps their
    /// demands as they are.
    fn analyse_block(
        &mut self,
        s: Smid,
        block_map: &BlockMap<RcExpr>,
        rendered: bool,
    ) -> (RcExpr, DemandEnv) {
        let mut env = DemandEnv::new();
        let mut new_entries = Vec::new();
        for (k, v) in block_map.iter() {
            let (new_v, v_env) = self.analyse_expr(v);
            let v_env = if rendered {
                v_env
            } else {
                constructor_env(v_env)
            };
            env = merge_envs(&env, &v_env);
            new_entries.push((k.clone(), new_v));
        }
        (
            RcExpr::from(Expr::Block(s, new_entries.into_iter().collect())),
            env,
        )
    }

    /// Analyse a `Let` expression using fixed-point iteration.
    ///
    /// All core `Let` scopes are recursive (`LetRec` in STG), so a simple
    /// syntactic reference count is unsound — a binding used once in the
    /// source may be evaluated many times through mutual or self-recursion.
    ///
    /// The algorithm:
    /// 1. Analyse all RHSs, iterating until the signatures of the scope's
    ///    lambda bindings are stable (`analyse_let_rhss`), then the body.
    /// 2. Build an intra-scope dependency graph: edge i→j means binding i's
    ///    RHS references binding j at scope 0.
    /// 3. Find recursive bindings: those that can reach themselves through
    ///    the dependency graph (direct self-loop or cycle).
    /// 4. Propagate "tainted by recursion": if binding i is recursive and
    ///    i→j exists, binding j is also tainted (it could be re-entered
    ///    through i's recursion).
    /// 5. Fixed-point demand propagation: starting from body_env, iterate
    ///    propagating demands through RHS references until stable.
    /// 6. Force Multi cardinality on all recursive/tainted bindings.
    fn analyse_let(
        &mut self,
        s: Smid,
        scope: &LetScope<RcExpr>,
        let_type: LetType,
    ) -> (RcExpr, DemandEnv) {
        let n = scope.pattern.len();

        // Steps 1–2: analyse the RHSs, then the body, with the signatures
        // of the scope's lambda bindings in a frame so that calls to them
        // use their demands.
        self.frames.push(SigFrame {
            sigs: Vec::new(),
            consulted: false,
        });
        let (new_rhs_exprs, rhs_envs) = self.analyse_let_rhss(scope);
        let (new_body, body_env) = self.analyse_expr(&scope.body);
        self.frames.pop();

        // Step 3: Build the intra-scope dependency graph.
        // deps[i] = set of binding indices that binding i's RHS references (at scope 0).
        let deps: Vec<Vec<usize>> = (0..n)
            .map(|i| {
                (0..n)
                    .filter(|&j| rhs_envs[i].contains_key(&(0, j as u32)))
                    .collect()
            })
            .collect();

        // Step 4a: Find directly recursive bindings (can reach themselves).
        // A binding is recursive iff there is a path i → … → i in the
        // dependency graph.  We check this with a BFS from each node.
        let mut is_recursive = vec![false; n];
        for start in 0..n {
            // BFS from start's neighbours; stop if we reach start again.
            let mut visited = vec![false; n];
            let mut queue: std::collections::VecDeque<usize> = std::collections::VecDeque::new();
            for &j in &deps[start] {
                if j == start {
                    is_recursive[start] = true;
                    break;
                }
                if !visited[j] {
                    visited[j] = true;
                    queue.push_back(j);
                }
            }
            if !is_recursive[start] {
                'bfs: while let Some(k) = queue.pop_front() {
                    for &j in &deps[k] {
                        if j == start {
                            is_recursive[start] = true;
                            break 'bfs;
                        }
                        if !visited[j] {
                            visited[j] = true;
                            queue.push_back(j);
                        }
                    }
                }
            }
        }

        // Step 4b: Propagate "tainted by recursion".
        // If binding i is recursive and i→j, then j is also tainted:
        // a recursive binding can be evaluated many times, so any binding
        // it uses could also be evaluated many times.
        let mut is_tainted = is_recursive.clone();
        loop {
            let old = is_tainted.clone();
            for i in 0..n {
                if is_tainted[i] {
                    for &j in &deps[i] {
                        is_tainted[j] = true;
                    }
                }
            }
            if old == is_tainted {
                break;
            }
        }

        // Step 5: Fixed-point demand propagation.
        // Start with demands from the body, then propagate through RHS references
        // until the demand vector stabilises.
        let mut demands: Vec<Demand> = (0..n)
            .map(|i| {
                body_env
                    .get(&(0, i as u32))
                    .copied()
                    .unwrap_or(Demand::absent())
            })
            .collect();

        loop {
            let old = demands.clone();
            for i in 0..n {
                // Recompute from body demand fresh each iteration to avoid
                // double-counting when using `plus`.
                let mut d = body_env
                    .get(&(0, i as u32))
                    .copied()
                    .unwrap_or(Demand::absent());

                // Propagated demand: for each binding j whose RHS references i,
                // if j is demanded with demand d_j, then i gets an additional
                // demand of d_j scaled by the demand j's RHS places on i.
                //
                // These are SEQUENTIAL uses (each demanded binding's RHS is
                // evaluated independently), so combine with `plus`, not `lub`.
                for j in 0..n {
                    if let Some(&d_ji) = rhs_envs[j].get(&(0, i as u32)) {
                        // d_ji: demand j's RHS places on i
                        // demands[j]: how often binding j itself is demanded
                        let contribution = d_ji.scale(demands[j]);
                        d = d.plus(contribution);
                    }
                }

                demands[i] = d;
            }
            if demands == old {
                break;
            }
        }

        // Step 6: Force Multi cardinality on recursive/tainted bindings
        // that are actually used (not Absent).  Also mark bindings that
        // can reach themselves through the dependency graph as recursive
        // so the STG compiler preserves black-holing (no update elision).
        for i in 0..n {
            if is_tainted[i] && demands[i].cardinality != Cardinality::Absent {
                demands[i].cardinality = Cardinality::Multi;
            }
            if is_recursive[i] {
                demands[i].recursive = true;
            }
        }

        // Step 7: Fixup for rendered block scopes.
        //
        // Any Let scope whose body is a Block constructor will have its
        // bindings forced by RENDER_DOC after demand analysis. The render
        // traversal adds invisible uses that the analysis cannot see, so
        // AtMostOnce is unsound here — force Multi on any non-absent
        // binding to prevent update elision.
        //
        // Note: we check the body rather than the LetType because pipeline
        // transforms (inject_prelude_inlinable_bindings + compress) can convert
        // DefaultBlockLet scopes to OtherLet while preserving the Block body.
        if matches!(&*new_body.inner, Expr::Block(_, _)) {
            for d in &mut demands {
                if d.cardinality == Cardinality::AtMostOnce {
                    d.cardinality = Cardinality::Multi;
                }
            }
        }

        // Build annotated bindings and propagate demands to the outer scope.
        let mut outer_env = unshift_env(&body_env);
        let mut new_bindings = Vec::with_capacity(n);

        for (i, binding) in scope.pattern.iter().enumerate() {
            let binding_demand = demands[i];

            // Propagate the RHS's outward demands for non-absent bindings.
            //
            // Strictness is scaled by the demand on the binding itself:
            // the RHS is only evaluated when the binding is, so a free
            // variable it forces is strict outside only if the binding is
            // strict (eu-oaz72.1). Without this, a binding needed only in
            // an untaken branch made its RHS's free variables strict, and
            // they were forced eagerly. Cardinality is left as the RHS's
            // own: an updated thunk evaluates its RHS at most once however
            // often the binding is demanded. A `Lam` RHS is a value, not a
            // thunk, and its body runs once per call; `analyse_lam` has
            // already raised its captured variables to Multi for that
            // (eu-oaz72.41).
            //
            // The exception is an alias, a binding whose RHS is a bare bound
            // variable: the STG optimiser erases it, substituting the
            // variable for every use, so no thunk of its own stands between
            // the variable and those uses. The variable then takes the
            // binding's cardinality too (eu-oaz72.36). Without this, a
            // lambda parameter reached only through such an alias (`filter`'s
            // `{x: • xs: •}`) was credited a single use, its caller let-bound
            // the argument without an update, and the `Cons` that stored it
            // recomputed it on every traversal.
            if binding_demand.cardinality != Cardinality::Absent
                && binding_demand.cardinality != Cardinality::Unknown
            {
                let is_alias = matches!(&*new_rhs_exprs[i].inner, Expr::Var(_, Var::Bound(_)));
                let shifted: DemandEnv = unshift_env(&rhs_envs[i])
                    .into_iter()
                    .map(|(k, d)| {
                        let scaled = d.scale(binding_demand);
                        let cardinality = if is_alias {
                            scaled.cardinality
                        } else {
                            d.cardinality
                        };
                        (
                            k,
                            Demand {
                                strictness: scaled.strictness,
                                cardinality,
                                ..d
                            },
                        )
                    })
                    .collect();
                outer_env = merge_envs(&outer_env, &shifted);
            }

            // If the binding's RHS is a Lam with a known signature, record
            // the signature under the binding name for STG compiler use.
            //
            // Peel `Meta` first: on the source-prelude path every binding
            // (including recursive higher-order combinators such as
            // `foldl`) is `Meta(Lam(_, true, _), {doc, type})`, not a bare
            // `Lam`, so without peeling this check never fires there — the
            // named signature table stays empty and the STG compiler falls
            // back to conservative (lazy) treatment of the recursive copy's
            // higher-order parameter, defeating demand specialisation even
            // though `tag_combinators`/`distribute`'s meta-peel (eu-rb5n
            // sub-problem 1) has already localised the recursive copy. The
            // blob path never hits this: `inlinable_bindings` are peeled at
            // blob-gen time (`xtask`), so their injected values are already
            // bare `Lam`s here.
            if let Expr::Lam(_, _, ref lam_scope) = &*peel_meta(&new_rhs_exprs[i]).inner {
                let key = Rc::as_ptr(&lam_scope.body.inner) as usize;
                if let Some(sig) = self.signatures.get(&key).cloned() {
                    self.record_named_signature(&binding.name, sig);
                }
            }

            new_bindings.push(CoreBinding::with_demand(
                binding.name.clone(),
                new_rhs_exprs[i].clone(),
                binding_demand,
            ));
        }

        let new_scope = Scope {
            pattern: new_bindings,
            body: new_body,
        };

        (RcExpr::from(Expr::Let(s, new_scope, let_type)), outer_env)
    }

    /// Record `sig` as the signature of a binding called `name`, merging it
    /// with any other binding's of the same name (see `named_signatures`).
    fn record_named_signature(&mut self, name: &str, sig: DemandSignature) {
        let previous = self.named_signatures.get(name).cloned();
        let merged = match &previous {
            None => Some(sig),
            Some(None) => None,
            Some(Some(other)) if other.len() == sig.len() => {
                Some(other.iter().zip(&sig).map(|(a, b)| a.lub(*b)).collect())
            }
            Some(Some(_)) => None,
        };
        self.named_log.push((name.to_string(), previous));
        self.named_signatures.insert(name.to_string(), merged);
    }

    /// Analyse the RHSs of a `Let` in the scope's signature frame (the
    /// innermost; the caller pushes and pops it), leaving in it the
    /// signatures the RHSs settled on, for the body.
    ///
    /// A lambda binding's signature depends on the signatures of the calls
    /// in its body, its own recursive calls included, so the group's
    /// signatures are computed as a fixpoint. They start optimistic — every
    /// parameter strict and used once — and each round re-analyses the
    /// RHSs with the signatures the previous round produced, until they
    /// stop changing. The result is a fixpoint of the analysis, so it
    /// accounts for every path through the recursion: `loop(a, n): if(n =
    /// 0, a, loop(a + 1, n - 1))` is strict in `a` (each path forces it),
    /// so the recursive call evaluates `a + 1` eagerly instead of building
    /// a chain of thunks (eu-oaz72.27); `foldl` passes its accumulator to
    /// an unknown `op` and stays lazy in it.
    ///
    /// A round that consulted none of the group's signatures cannot depend
    /// on them and ends the iteration. After `MAX_SIGNATURE_ITERATIONS`
    /// rounds the signatures are reset to pessimistic (`lazy_multi`) and
    /// one final round is analysed with them: every demand it reports is
    /// then justified by the RHS itself, so it is sound without being a
    /// fixpoint.
    fn analyse_let_rhss(&mut self, scope: &LetScope<RcExpr>) -> (Vec<RcExpr>, Vec<DemandEnv>) {
        let arities: Vec<Option<usize>> = scope
            .pattern
            .iter()
            .map(|b| match &*peel_meta(&b.expr).inner {
                Expr::Lam(_, _, lam) => Some(lam.pattern.len()),
                _ => None,
            })
            .collect();
        let seed_key = scope as *const LetScope<RcExpr> as usize;
        let frame = self.frames.len() - 1;
        self.frames[frame].sigs = match self.seeds.get(&seed_key) {
            Some(seed) => seed.clone(),
            None => arities
                .iter()
                .map(|a| a.map(|k| vec![Demand::strict_once(); k]))
                .collect(),
        };

        let mut rounds = 0;
        loop {
            let mark = self.sig_log.len();
            let named_mark = self.named_log.len();
            self.frames[frame].consulted = false;

            let mut new_rhs_exprs = Vec::with_capacity(arities.len());
            let mut rhs_envs = Vec::with_capacity(arities.len());
            for binding in &scope.pattern {
                let (new_rhs, rhs_env) = self.analyse_expr(&binding.expr);
                new_rhs_exprs.push(new_rhs);
                rhs_envs.push(rhs_env);
            }

            let derived: Vec<Option<DemandSignature>> = new_rhs_exprs
                .iter()
                .zip(&arities)
                .map(|(rhs, arity)| {
                    (*arity)?;
                    match &*peel_meta(rhs).inner {
                        Expr::Lam(_, _, lam) => self
                            .signatures
                            .get(&(Rc::as_ptr(&lam.body.inner) as usize))
                            .cloned(),
                        _ => None,
                    }
                })
                .collect();

            let settled = !self.frames[frame].consulted
                || rounds >= MAX_SIGNATURE_ITERATIONS
                || derived == self.frames[frame].sigs;
            if settled {
                // Body calls see the signatures the RHSs actually have.
                self.seeds.insert(seed_key, derived.clone());
                self.frames[frame].sigs = derived;
                return (new_rhs_exprs, rhs_envs);
            }

            // Discard this round's pointer-keyed signatures: its
            // expressions are dropped and their addresses may be reused.
            for key in self.sig_log.drain(mark..) {
                self.signatures.remove(&key);
            }
            for (name, previous) in self.named_log.drain(named_mark..).rev() {
                match previous {
                    Some(entry) => self.named_signatures.insert(name, entry),
                    None => self.named_signatures.remove(&name),
                };
            }
            rounds += 1;
            self.frames[frame].sigs = if rounds == MAX_SIGNATURE_ITERATIONS {
                arities
                    .iter()
                    .map(|a| a.map(|k| vec![Demand::lazy_multi(); k]))
                    .collect()
            } else {
                derived
            };
        }
    }

    /// Analyse a `Lam` expression as a value.
    ///
    /// A lambda value can be applied any number of times, and every
    /// application evaluates its body afresh, so a variable the body
    /// captures from an enclosing scope is demanded Multi even when the
    /// body refers to it once. Leaving it AtMostOnce let the STG compiler
    /// skip the captured binding's update frame and recompute it on every
    /// call, e.g. `{ b: ds sum  h(x): x + b }.(h(1) + h(2))` (eu-oaz72.41).
    fn analyse_lam(&mut self, s: Smid, inl: bool, scope: &LamScope<RcExpr>) -> (RcExpr, DemandEnv) {
        let (new_lam, env) = self.analyse_lam_once(s, inl, scope);
        (new_lam, shared_env(env))
    }

    /// Analyse a `Lam` expression that is applied exactly once, returning
    /// its body's demands on captured variables unscaled.
    ///
    /// Only sound where the lambda is the function of an `App` (a
    /// beta-redex): it is applied once each time the application is
    /// evaluated, and that application's own cardinality is accounted for
    /// by the enclosing expression.
    fn analyse_lam_once(
        &mut self,
        s: Smid,
        inl: bool,
        scope: &LamScope<RcExpr>,
    ) -> (RcExpr, DemandEnv) {
        let num_params = scope.pattern.len();

        // Analyse the body in a frame of its own: parameters are not known
        // functions.
        self.frames.push(SigFrame {
            sigs: Vec::new(),
            consulted: false,
        });
        let (new_body, body_env) = self.analyse_expr(&scope.body);
        self.frames.pop();

        // Extract the demand signature: one Demand per parameter
        let mut sig = Vec::with_capacity(num_params);
        for i in 0..num_params {
            let param_demand = body_env
                .get(&(0, i as u32))
                .copied()
                .unwrap_or(Demand::absent());
            sig.push(param_demand);
        }

        // Store the signature keyed by the body's Rc pointer
        let sig_key = Rc::as_ptr(&new_body.inner) as usize;
        self.signatures.insert(sig_key, sig);
        self.sig_log.push(sig_key);

        // The outer env is the body env with scope-0 entries removed
        let outer_env = unshift_env(&body_env);

        let new_scope = Scope {
            pattern: scope.pattern.clone(),
            body: new_body,
        };

        (RcExpr::from(Expr::Lam(s, inl, new_scope)), outer_env)
    }

    /// Analyse an `App` expression.
    fn analyse_app(&mut self, s: Smid, f: &RcExpr, args: &[RcExpr]) -> (RcExpr, DemandEnv) {
        // Analyse the function. A lambda literal in function position that
        // receives all its parameters is applied once here, so its captured
        // variables keep their cardinality (see `analyse_lam_once`). A
        // partial application returns a closure that may be called any
        // number of times, so it takes the shared path.
        let (new_f, f_env) = match &*f.inner {
            Expr::Lam(ls, inl, scope) if args.len() >= scope.pattern.len() => {
                self.analyse_lam_once(*ls, *inl, scope)
            }
            _ => self.analyse_expr(f),
        };
        let mut env = f_env;

        // Try to find the function's signature
        let sig = self.lookup_signature(f, args.len());
        let is_if =
            matches!(&*f.inner, Expr::Intrinsic(_, name) if name == "IF") && args.len() == 3;
        let mut branch_envs: Vec<DemandEnv> = Vec::new();

        // Analyse each argument with the demand from the signature
        let mut new_args = Vec::with_capacity(args.len());
        for (i, arg) in args.iter().enumerate() {
            let (new_arg, arg_env) = self.analyse_expr(arg);

            // The demand the function places on this argument position.
            let arg_demand = sig
                .as_ref()
                .and_then(|s| s.get(i))
                .copied()
                .unwrap_or(Demand::lazy_multi());

            // If the argument is used (not absent), scale the argument's
            // free-variable demands by the function's demand on this
            // argument position.  If the function uses this argument
            // Multi times (e.g. map's function argument), every free
            // variable captured in the argument is also demanded Multi
            // times — without this scaling they would stay AtMostOnce,
            // causing the STG compiler to skip memoisation.
            if arg_demand.cardinality != Cardinality::Absent {
                let scaled_env: DemandEnv = arg_env
                    .iter()
                    .map(|(&k, &d)| (k, d.scale(arg_demand)))
                    .collect();
                env = merge_envs(&env, &scaled_env);
            }

            if is_if && i > 0 {
                branch_envs.push(arg_env);
            }
            new_args.push(new_arg);
        }

        // Exactly one branch of an `IF` is evaluated, so a variable that
        // both branches force is forced whichever is taken (the lub of the
        // branches). The signature's lazy branch demands cannot see that.
        // Cardinality keeps the conservative sum.
        if let [then_env, else_env] = branch_envs.as_slice() {
            for (key, d) in then_env {
                let joined = else_env.get(key).map(|e| d.lub(*e));
                if joined.is_some_and(|j| j.strictness == Strictness::Strict) {
                    if let Some(entry) = env.get_mut(key) {
                        entry.strictness = Strictness::Strict;
                    }
                }
            }
        }

        (RcExpr::from(Expr::App(s, new_f, new_args)), env)
    }

    /// Look up the demand signature for a function expression applied to
    /// `nargs` arguments.
    fn lookup_signature(&mut self, f: &RcExpr, nargs: usize) -> Option<DemandSignature> {
        match &*f.inner {
            // A let-bound lambda: its (current) signature from the scope's
            // frame. Only a saturated call runs the body; a partial
            // application builds a closure that forces nothing yet.
            Expr::Var(_, Var::Bound(bv)) => {
                let index = self.frames.len().checked_sub(1 + bv.scope as usize)?;
                let frame = &mut self.frames[index];
                let sig = frame.sigs.get(bv.binder as usize)?.as_ref()?;
                if nargs < sig.len() {
                    return None;
                }
                let sig = sig.clone();
                frame.consulted = true;
                Some(sig)
            }

            // Intrinsic function: use seed signature
            Expr::Intrinsic(_, name) => self.intrinsic_sigs.get(name).cloned(),

            // Lambda: look in the signatures table
            Expr::Lam(_, _, scope) => {
                let key = Rc::as_ptr(&scope.body.inner) as usize;
                self.signatures.get(&key).cloned()
            }

            _ => None,
        }
    }
}

/// Run the demand analysis pass on a core expression.
///
/// Returns the annotated expression and the signature side-table.
pub fn analyse_demands(expr: &RcExpr) -> (RcExpr, SignatureTable, NamedSignatureTable) {
    let analyser = DemandAnalyser::new();
    analyser.analyse(expr)
}

/// Walk the annotated core expression and print demand annotations for
/// every let-bound name to `out`. Used by `eu dump demands`.
///
/// Write failures are returned rather than panicking, so a closed pipe
/// (`eu dump demands … | head`) can end the dump quietly.
pub fn print_demands(expr: &RcExpr, out: &mut dyn std::io::Write) -> std::io::Result<()> {
    print_demands_inner(expr, 0, out)
}

fn print_demands_inner(
    expr: &RcExpr,
    depth: usize,
    out: &mut dyn std::io::Write,
) -> std::io::Result<()> {
    use crate::core::demand::{Cardinality, Strictness};

    match &*expr.inner {
        Expr::Let(_, scope, _) => {
            for b in &scope.pattern {
                let s = match b.demand.strictness {
                    Strictness::Strict => "Strict",
                    Strictness::Lazy => "Lazy",
                    Strictness::Unknown => "Unknown",
                };
                let c = match b.demand.cardinality {
                    Cardinality::AtMostOnce => "AtMostOnce",
                    Cardinality::Multi => "Multi",
                    Cardinality::Absent => "Absent",
                    Cardinality::Unknown => "Unknown",
                };
                writeln!(out, "{}{}: ({}, {})", "  ".repeat(depth), b.name, s, c)?;
                print_demands_inner(&b.expr, depth + 1, out)?;
            }
            print_demands_inner(&scope.body, depth, out)?;
        }
        Expr::Lam(_, _, scope) => {
            print_demands_inner(&scope.body, depth, out)?;
        }
        Expr::App(_, f, args) => {
            print_demands_inner(f, depth, out)?;
            for a in args {
                print_demands_inner(a, depth, out)?;
            }
        }
        Expr::Lookup(_, obj, _, fb) => {
            print_demands_inner(obj, depth, out)?;
            if let Some(f) = fb {
                print_demands_inner(f, depth, out)?;
            }
        }
        Expr::List(_, xs) => {
            for x in xs {
                print_demands_inner(x, depth, out)?;
            }
        }
        Expr::Block(_, bm) => {
            for (_, v) in bm.iter() {
                print_demands_inner(v, depth, out)?;
            }
        }
        Expr::Meta(_, e, m) => {
            print_demands_inner(e, depth, out)?;
            print_demands_inner(m, depth, out)?;
        }
        _ => {}
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::expr::acore::*;

    #[test]
    fn single_use_nonrecursive_let_binding_gets_at_most_once() {
        // A non-recursive, single-use binding should be AtMostOnce —
        // the fixed-point correctly identifies it does not participate
        // in any cycle and can safely skip the Update frame.
        let x = free("x");
        let expr = let_(vec![(x.clone(), num(1))], var(x));
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                let x_binding = &scope.pattern[0];
                assert_eq!(x_binding.demand.cardinality, Cardinality::AtMostOnce);
                assert_eq!(x_binding.demand.strictness, Strictness::Strict);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn self_recursive_let_binding_gets_multi() {
        // A binding whose RHS references itself is recursive — must stay Multi
        // so the blackhole detection fires correctly if re-entered.
        let x = free("x");
        // let x = ADD(x, 1) in x  — x references itself in RHS
        let expr = let_(
            vec![(x.clone(), app(bif("ADD"), vec![var(x.clone()), num(1)]))],
            var(x),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Multi);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn mutually_recursive_let_bindings_get_multi() {
        // Mutually recursive bindings (x references y, y references x)
        // must both be Multi — neither can skip the Update frame.
        let x = free("x");
        let y = free("y");
        // let x = ADD(y, 1); y = ADD(x, 1) in ADD(x, y)
        let expr = let_(
            vec![
                (x.clone(), app(bif("ADD"), vec![var(y.clone()), num(1)])),
                (y.clone(), app(bif("ADD"), vec![var(x.clone()), num(1)])),
            ],
            app(bif("ADD"), vec![var(x), var(y)]),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Multi);
                assert_eq!(scope.pattern[1].demand.cardinality, Cardinality::Multi);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn nonrecursive_sibling_used_by_recursive_binding_gets_multi() {
        // If a non-recursive binding `a` is used by a recursive binding `x`,
        // `a` is tainted — it may be evaluated each time x loops, so Multi.
        let a = free("a");
        let x = free("x");
        // let a = 1; x = ADD(x, a) in x
        // a is non-recursive but x is self-recursive and uses a → a tainted
        let expr = let_(
            vec![
                (a.clone(), num(1)),
                (
                    x.clone(),
                    app(bif("ADD"), vec![var(x.clone()), var(a.clone())]),
                ),
            ],
            var(x),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                // x is self-recursive
                assert_eq!(scope.pattern[1].demand.cardinality, Cardinality::Multi);
                // a is tainted by x's recursion
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Multi);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn unused_binding_gets_absent() {
        let x = free("x");
        let y = free("y");
        let expr = let_(vec![(x.clone(), num(1)), (y.clone(), num(2))], var(x));
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[1].demand.cardinality, Cardinality::Absent);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn multi_use_binding_gets_multi() {
        let x = free("x");
        let expr = let_(
            vec![(x.clone(), num(1))],
            app(bif("ADD"), vec![var(x.clone()), var(x)]),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Multi);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn binding_used_by_two_siblings_gets_multi() {
        // let x = 1; y = ADD(x, 2); z = ADD(x, 3) in ADD(y, z)
        // x is used once by y's RHS and once by z's RHS — two sequential
        // uses, so x must be Multi.  The old code used `lub` which
        // collapsed this to AtMostOnce, breaking memoisation.
        let x = free("x");
        let y = free("y");
        let z = free("z");
        let expr = let_(
            vec![
                (x.clone(), num(1)),
                (y.clone(), app(bif("ADD"), vec![var(x.clone()), num(2)])),
                (z.clone(), app(bif("ADD"), vec![var(x.clone()), num(3)])),
            ],
            app(bif("ADD"), vec![var(y), var(z)]),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::Multi,
                    "x is used by two sibling bindings — must be Multi"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn binding_used_in_body_and_sibling_rhs_gets_multi() {
        // let x = 1; y = ADD(x, 2) in ADD(x, y)
        // x is used once in the body and once in y's RHS — two sequential
        // uses, so x must be Multi.
        let x = free("x");
        let y = free("y");
        let expr = let_(
            vec![
                (x.clone(), num(1)),
                (y.clone(), app(bif("ADD"), vec![var(x.clone()), num(2)])),
            ],
            app(bif("ADD"), vec![var(x), var(y)]),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::Multi,
                    "x is used in body and sibling RHS — must be Multi"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn constructor_contents_are_lazy() {
        // eu-oaz72.40:
        //   let b = 1; c = 2 in { b: b, l: [c], a: 1 }.a
        // A block or list constructor does not evaluate its contents, so
        // `b` and `c`, used only inside one, must be Lazy. Before the fix
        // they were Strict and forced although their field is unselected.
        let b = free("b");
        let c = free("c");
        let body = lookup(
            block([
                ("b".to_string(), var(b.clone())),
                ("l".to_string(), list(vec![var(c.clone())])),
                ("a".to_string(), num(1)),
            ]),
            "a",
            None,
        );
        let expr = let_(vec![(b, num(1)), (c, num(2))], body);
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[0].demand.strictness, Strictness::Lazy);
                assert_eq!(scope.pattern[1].demand.strictness, Strictness::Lazy);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn list_elements_are_lazy() {
        //   let c = 2 in [c]
        // A list constructor does not evaluate its elements, so `c` must
        // be Lazy; and the list can be read many times, so Multi
        // (eu-oaz72.40).
        let c = free("c");
        let expr = let_(vec![(c.clone(), num(2))], list(vec![var(c)]));
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[0].demand.strictness, Strictness::Lazy);
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Multi);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn rendered_block_body_fields_stay_strict() {
        //   let b = 1 in { b: b }
        // The root unit's block is rendered: RENDER_DOC forces every
        // field, so `b` keeps strict demand.
        let b = free("b");
        let expr = let_(
            vec![(b.clone(), num(1))],
            block([("b".to_string(), var(b))]),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[0].demand.strictness, Strictness::Strict);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn nested_block_body_fields_are_lazy() {
        //   let x = (let b = 1 in { b: b }) in { x: x }
        // A block literal is a Let whose body is a Block, so a Block body
        // alone is not evidence of rendering: only the root unit's block
        // keeps strict fields. The inner `b` must be Lazy.
        let b = free("b");
        let x = free("x");
        let inner = let_(
            vec![(b.clone(), num(1))],
            block([("b".to_string(), var(b))]),
        );
        let expr = let_(vec![(x.clone(), inner)], block([("x".to_string(), var(x))]));
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => match &*scope.pattern[0].expr.inner {
                Expr::Let(_, inner_scope, _) => {
                    assert_eq!(inner_scope.pattern[0].demand.strictness, Strictness::Lazy);
                }
                other => panic!("expected inner Let, got: {other:?}"),
            },
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn lazy_binding_rhs_does_not_make_outer_free_variable_strict() {
        // eu-oaz72.1:
        //   let b = 1; cond = 0 in (let a = ADD(b, 1) in IF(cond, 0, a))
        // `a` is needed only in an IF branch, so it is lazy, and `b` —
        // used only by `a`'s RHS — must be lazy too. Before the fix the
        // inner Let propagated `a`'s RHS demands outward unscaled, so `b`
        // was marked strict and forced even when that branch is untaken.
        let b = free("b");
        let cond = free("cond");
        let a = free("a");
        let inner = let_(
            vec![(a.clone(), app(bif("ADD"), vec![var(b.clone()), num(1)]))],
            app(bif("IF"), vec![var(cond.clone()), num(0), var(a)]),
        );
        let expr = let_(vec![(b, num(1)), (cond, num(0))], inner);
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.strictness,
                    Strictness::Lazy,
                    "b is needed only through the lazy binding a — must be Lazy"
                );
                assert_eq!(scope.pattern[1].demand.strictness, Strictness::Strict);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn if_branches_share_binding_gets_at_most_once() {
        // let x = 1 in IF(cond, ADD(x, 2), ADD(x, 3))
        // x appears in both IF branches, but only one branch executes.
        // The analysis should use `lub` for IF branches (via the
        // lazy_once signature), so x is AtMostOnce, not Multi.
        let x = free("x");
        let cond = free("cond");
        let expr = let_(
            vec![(x.clone(), num(1)), (cond.clone(), num(0))],
            app(
                bif("IF"),
                vec![
                    var(cond),
                    app(bif("ADD"), vec![var(x.clone()), num(2)]),
                    app(bif("ADD"), vec![var(x), num(3)]),
                ],
            ),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                // x appears in both branches but only one executes —
                // the IF signature marks branches as lazy_once, so the
                // branch envs are each scaled by AtMostOnce. x's total
                // demand from the two branches is lub(once, once) = once,
                // but it also appears twice via merge_envs (plus) giving
                // Multi.  This is conservative-correct: the analysis
                // cannot know at compile time which branch is taken, so
                // treating the variable as Multi is safe.
                let x_card = scope.pattern[0].demand.cardinality;
                assert!(
                    x_card == Cardinality::Multi || x_card == Cardinality::AtMostOnce,
                    "x in IF branches should be AtMostOnce or conservatively Multi, got: {x_card:?}"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn nested_strict_single_use_stays_at_most_once() {
        // let x = 1 in f(g(x))
        // where f and g each use their argument once.
        // x should be AtMostOnce — it's only evaluated once through
        // the nested chain.
        let x = free("x");
        // Simulate f(g(x)) as ADD(0, ADD(0, x)) — each ADD uses its
        // args once (strict_once from the intrinsic signature).
        let expr = let_(
            vec![(x.clone(), num(1))],
            app(
                bif("ADD"),
                vec![num(0), app(bif("ADD"), vec![num(0), var(x)])],
            ),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::AtMostOnce,
                    "x used once through nested strict calls — must be AtMostOnce"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn captured_binding_scaled_by_multi_arg_demand() {
        // let x = 1 in unknown_f(\y -> ADD(x, y))
        // unknown_f has no known signature, so its argument gets the
        // default lazy_multi demand.  The lambda captures x, and since
        // the lambda may be called multiple times, x must be Multi.
        let x = free("x");
        let y = free("y");
        let f = free("f");
        let expr = let_(
            vec![
                (x.clone(), num(1)),
                (f.clone(), num(0)), // stand-in for unknown function
            ],
            app(
                var(f),
                vec![lam(
                    vec!["y".to_string()],
                    app(bif("ADD"), vec![var(x), var(y)]),
                )],
            ),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::Multi,
                    "x captured in lambda passed to unknown function — must be Multi"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn binding_captured_by_let_bound_lambda_gets_multi() {
        // let x = 1 in (let h = \y -> ADD(x, y) in ADD(h(1), h(2)))
        // h is called twice and each call evaluates its body, so x is
        // demanded twice although the body refers to it once. It must be
        // Multi, or its update is elided and it is recomputed per call
        // (eu-oaz72.41).
        let x = free("x");
        let y = free("y");
        let h = free("h");
        let expr = let_(
            vec![(x.clone(), num(1))],
            let_(
                vec![(
                    h.clone(),
                    lam(vec!["y".to_string()], app(bif("ADD"), vec![var(x), var(y)])),
                )],
                app(
                    bif("ADD"),
                    vec![app(var(h.clone()), vec![num(1)]), app(var(h), vec![num(2)])],
                ),
            ),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::Multi,
                    "x captured by a lambda that is called twice — must be Multi"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn binding_captured_by_directly_applied_lambda_stays_at_most_once() {
        // let x = 1 in (\y -> ADD(x, y))(2)
        // A lambda literal applied in place runs its body once, so the
        // binding it captures keeps its single-use cardinality.
        let x = free("x");
        let y = free("y");
        let expr = let_(
            vec![(x.clone(), num(1))],
            app(
                lam(vec!["y".to_string()], app(bif("ADD"), vec![var(x), var(y)])),
                vec![num(2)],
            ),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::AtMostOnce,
                    "x captured by a lambda applied once in place — AtMostOnce"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn binding_captured_by_partially_applied_lambda_gets_multi() {
        // let x = 1 in (\y z -> ADD(x, ADD(y, z)))(2)
        // A partial application returns a closure that may be called any
        // number of times, so x must be Multi.
        let x = free("x");
        let y = free("y");
        let z = free("z");
        let expr = let_(
            vec![(x.clone(), num(1))],
            app(
                lam(
                    vec!["y".to_string(), "z".to_string()],
                    app(
                        bif("ADD"),
                        vec![var(x), app(bif("ADD"), vec![var(y), var(z)])],
                    ),
                ),
                vec![num(2)],
            ),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::Multi,
                    "x captured by a partially applied lambda — must be Multi"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn if_branches_get_lazy_once_from_intrinsic_sig() {
        let analyser = DemandAnalyser::new();
        let if_sig = analyser.intrinsic_sigs.get("IF").unwrap();
        assert_eq!(if_sig.len(), 3);
        assert_eq!(if_sig[0], Demand::strict_once());
        assert_eq!(if_sig[1], Demand::lazy_once());
        assert_eq!(if_sig[2], Demand::lazy_once());
    }

    /// `loop(a, n) = IF(n, a, loop(ADD(a, 1), n))`: `a` is returned on
    /// one path and passed to the strict accumulator of the recursive call
    /// on the other, so the signature fixpoint finds it strict
    /// (eu-oaz72.27).
    #[test]
    fn self_recursive_accumulator_is_strict() {
        let lp = free("loop");
        let a = free("a");
        let n = free("n");
        let body = app(
            bif("IF"),
            vec![
                var(n.clone()),
                var(a.clone()),
                app(
                    var(lp.clone()),
                    vec![app(bif("ADD"), vec![var(a), num(1)]), var(n)],
                ),
            ],
        );
        let expr = let_(
            vec![(lp.clone(), lam(vec!["a".into(), "n".into()], body))],
            app(var(lp), vec![num(0), num(1)]),
        );
        let (_result, _sigs, named) = analyse_demands(&expr);
        let sig = named.get("loop").expect("loop signature");
        assert_eq!(sig[0].strictness, Strictness::Strict);
        assert_eq!(sig[1].strictness, Strictness::Strict);
    }

    /// A left fold passes its accumulator to an unknown `op`, so the
    /// optimistic start must not survive the fixpoint: `i` stays lazy.
    #[test]
    fn accumulator_passed_to_unknown_function_stays_lazy() {
        let f = free("f");
        let op = free("op");
        let i = free("i");
        let l = free("l");
        let body = app(
            bif("IF"),
            vec![
                var(l.clone()),
                var(i.clone()),
                app(
                    var(f.clone()),
                    vec![
                        var(op.clone()),
                        app(var(op), vec![var(i), var(l.clone())]),
                        var(l),
                    ],
                ),
            ],
        );
        let expr = let_(
            vec![(
                f.clone(),
                lam(vec!["op".into(), "i".into(), "l".into()], body),
            )],
            var(f),
        );
        let (_result, _sigs, named) = analyse_demands(&expr);
        let sig = named.get("f").expect("f signature");
        assert_eq!(sig[1].strictness, Strictness::Lazy);
        assert_eq!(sig[2].strictness, Strictness::Strict);
    }

    #[test]
    fn lam_signature_recorded() {
        let x = free("x");
        // \x y -> x (y is unused)
        let lam_expr = lam(vec!["x".to_string(), "y".to_string()], var(x.clone()));
        let (result, sigs, _named_sigs) = analyse_demands(&lam_expr);

        assert!(!sigs.is_empty(), "signature table should not be empty");

        if let Expr::Lam(_, _, scope) = &*result.inner {
            let key = Rc::as_ptr(&scope.body.inner) as usize;
            let sig = sigs.get(&key).expect("signature should exist for lambda");
            assert_eq!(sig.len(), 2);
            assert_eq!(sig[0].cardinality, Cardinality::AtMostOnce); // x used once
            assert_eq!(sig[1].cardinality, Cardinality::Absent); // y unused
        } else {
            panic!("expected Lam");
        }
    }

    #[test]
    fn named_signature_recorded_for_let_bound_lam() {
        let x = free("x");
        // let f = \x y -> x in f
        let lam_body = lam(vec!["x".to_string(), "y".to_string()], var(x.clone()));
        let f = free("f");
        let expr = let_(vec![(f.clone(), lam_body)], var(f));
        let (_result, _sigs, named_sigs) = analyse_demands(&expr);

        let sig = named_sigs
            .get("f")
            .expect("named signature should exist for let-bound lambda");
        assert_eq!(sig.len(), 2);
        assert_eq!(sig[0].cardinality, Cardinality::AtMostOnce); // x used once
        assert_eq!(sig[1].cardinality, Cardinality::Absent); // y unused
    }

    #[test]
    fn absent_demand_skips_update() {
        // Absent bindings are never evaluated, so skipping update is safe.
        assert!(Demand::absent().skip_update());
    }

    #[test]
    fn demand_lub_combines_branches() {
        let strict_once = Demand::strict_once();
        let lazy_once = Demand::lazy_once();
        let combined = strict_once.lub(lazy_once);
        assert_eq!(combined.strictness, Strictness::Lazy);
        assert_eq!(combined.cardinality, Cardinality::AtMostOnce);
    }

    #[test]
    fn demand_plus_combines_sequentially() {
        let once = Demand::strict_once();
        let combined = once.plus(once);
        assert_eq!(combined.cardinality, Cardinality::Multi);
        assert_eq!(combined.strictness, Strictness::Strict);
    }

    /// Demonstrate that SCC splitting improves demand analysis.
    ///
    /// Without splitting, a non-recursive single-use binding `a` that
    /// shares a scope with a self-recursive binding `x` (where `x`
    /// uses `a`) is tainted to Multi.  After SCC splitting separates
    /// `a` into its own Let scope, demand analysis correctly assigns
    /// AtMostOnce.
    #[test]
    fn scc_splitting_improves_demand_for_tainted_binding() {
        use crate::core::dependency::split_letrecs;

        let a = free("a");
        let x = free("x");
        // let a = 1; x = ADD(x, a) in x
        // Before splitting: a is tainted by x's recursion → Multi
        let unsplit = let_(
            vec![
                (a.clone(), num(1)),
                (
                    x.clone(),
                    app(bif("ADD"), vec![var(x.clone()), var(a.clone())]),
                ),
            ],
            var(x.clone()),
        );

        let (unsplit_result, _, _) = analyse_demands(&unsplit);
        match &*unsplit_result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::Multi,
                    "before splitting: a is tainted by x's recursion"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }

        // After SCC splitting: a is in its own Let scope
        let split = split_letrecs(&unsplit);
        let (split_result, _, _) = analyse_demands(&split);

        // The outermost Let should be `a` (independent, placed outermost)
        match &*split_result.inner {
            Expr::Let(_, outer_scope, _) => {
                assert_eq!(outer_scope.pattern.len(), 1, "outer scope has one binding");
                assert_eq!(outer_scope.pattern[0].name, "a");
                assert_eq!(
                    outer_scope.pattern[0].demand.cardinality,
                    Cardinality::AtMostOnce,
                    "after splitting: a is in its own scope → AtMostOnce"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    /// `depth` recursive accumulator loops, each nested in the base case of
    /// the one enclosing it:
    /// `let gK = λ(a, n). IF(EQ(n, 0), <loop K-1>, gK(ADD(a, 1), SUB(n, 1))) in gK(0, 3)`.
    fn nested_loops(depth: usize) -> RcExpr {
        (1..=depth).fold(num(0), |inner, k| {
            let g = free(&format!("g{k}"));
            let a = free("a");
            let n = free("n");
            let body = app(
                bif("IF"),
                vec![
                    app(bif("EQ"), vec![var(n.clone()), num(0)]),
                    inner,
                    app(
                        var(g.clone()),
                        vec![
                            app(bif("ADD"), vec![var(a.clone()), num(1)]),
                            app(bif("SUB"), vec![var(n.clone()), num(1)]),
                        ],
                    ),
                ],
            );
            let_(
                vec![(g.clone(), lam(vec![a, n], body))],
                app(var(g), vec![num(0), num(3)]),
            )
        })
    }

    #[test]
    fn nested_fixpoints_cost_grows_linearly_with_depth() {
        // Each nested let group is re-analysed on every round of the group
        // enclosing it. Seeded from its last result, it settles in one
        // round, so work is linear in the nesting depth; restarted from
        // optimistic signatures, it repeats its fixpoint and the work
        // doubles or more with each level (round-1 review of eu-oaz72.27).
        let visits = |depth| {
            let mut analyser = DemandAnalyser::new();
            analyser.analyse_expr(&nested_loops(depth));
            analyser.visits
        };
        let (shallow, deep) = (visits(8), visits(16));
        assert!(
            deep < shallow * 4,
            "analysis work at depth 16 ({deep}) should be about twice that at depth 8 ({shallow})"
        );
    }
}
