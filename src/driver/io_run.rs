//! IO monad interpret loop
//!
//! Interprets IO data constructors (IoReturn, IoBind, IoAction, IoFail)
//! produced by the STG machine. After the machine evaluates a target
//! expression to WHNF and yields on an IO constructor, this module drives
//! the shell-execution side effects and re-enters the machine with results.
//!
//! # Entry points
//!
//! `io_run_and_render` is called from `eval.rs` after `machine.run()` when
//! `machine.io_yielded()` is true.

use std::{
    collections::HashMap,
    io::Write as IoWrite,
    process::{Command, Stdio},
    sync::mpsc,
    thread,
    time::Duration,
};

use crate::common::sourcemap::Smid;
use crate::eval::machine::env::EnvFrame;
use crate::eval::memory::infotable::InfoTable;
use crate::eval::{
    error::ExecutionError,
    intrinsics,
    machine::{env::SynClosure, env_builder::EnvBuilder, vm::Machine},
    memory::{
        alloc::ScopedAllocator,
        array::Array,
        mutator::{Mutator, MutatorHeapView},
        symbol::SymbolPool,
        syntax::{HeapSyn, Native, Ref, RefPtr, StgBuilder},
    },
    stg::{render_to_string::extract_scalar_string, tags::DataConstructor},
};

// ─── Public error type ────────────────────────────────────────────────────────

/// Error from running the IO monad interpret loop
#[derive(Debug, thiserror::Error)]
pub enum IoRunError {
    #[error("io.fail: {0}")]
    Fail(String),
    #[error("IO operations are not permitted; use the --allow-io (-I) flag to enable")]
    IoNotAllowed(Smid),
    #[error("unknown IO action tag: {0}")]
    UnknownActionTag(String),
    #[error("malformed IO action spec block")]
    MalformedSpec,
    #[error("io.shell-with: command timed out after {0} seconds")]
    Timeout(Smid, u64),
    #[error("io.shell-with: command execution error: {0}")]
    CommandError(Smid, String),
    /// Boxed to keep the error variant size small (ExecutionError is large).
    #[error("STG machine error: {0}")]
    MachineError(Box<ExecutionError>),
}

impl From<ExecutionError> for IoRunError {
    fn from(e: ExecutionError) -> Self {
        IoRunError::MachineError(Box::new(e))
    }
}

// ─── Command result ───────────────────────────────────────────────────────────

/// The outcome of a shell command execution
#[derive(Debug)]
struct CommandResult {
    stdout: String,
    stderr: String,
    exit_code: i64,
}

// ─── Action spec (extracted from IoAction spec block) ────────────────────────

#[derive(Debug)]
enum ActionSpec {
    Shell {
        cmd: String,
        stdin: Option<String>,
        timeout_secs: u64,
    },
    Exec {
        cmd: String,
        args: Vec<String>,
        stdin: Option<String>,
        timeout_secs: u64,
    },
}

// ─── Heap navigation helpers ──────────────────────────────────────────────────

/// Resolve a `Ref` relative to a closure's environment, producing a new closure.
///
/// `Ref::V` values are wrapped in an atom closure.
/// `Ref::G` requires the globals env frame to be passed separately.
fn resolve_ref(
    view: &MutatorHeapView<'_>,
    parent: &SynClosure,
    r: Ref,
) -> Result<SynClosure, ExecutionError> {
    resolve_ref_with_globals(view, parent, r, None)
}

/// Resolve a `Ref` with optional access to the globals env frame.
///
/// When `globals` is `Some`, `G(i)` refs are resolved via that frame.
/// When `globals` is `None`, `G(i)` refs return an error.
fn resolve_ref_with_globals(
    view: &MutatorHeapView<'_>,
    parent: &SynClosure,
    r: Ref,
    globals: Option<RefPtr<EnvFrame>>,
) -> Result<SynClosure, ExecutionError> {
    match r {
        Ref::L(i) => {
            let env = view.scoped(parent.env());
            env.get(view, i)
                .ok_or(ExecutionError::BadEnvironmentIndex(i))
        }
        Ref::V(n) => {
            let atom_ptr = view
                .alloc(HeapSyn::Atom {
                    evaluand: Ref::V(n),
                })?
                .as_ptr();
            Ok(SynClosure::new(atom_ptr, parent.env()))
        }
        Ref::G(i) => {
            if let Some(g) = globals {
                let genv = view.scoped(g);
                genv.get(view, i)
                    .ok_or(ExecutionError::BadEnvironmentIndex(i))
            } else {
                Err(ExecutionError::Panic(
                    Smid::default(),
                    format!("unexpected global ref G({i}) in io spec block"),
                ))
            }
        }
    }
}

/// Follow indirection atoms through the environment until we reach a
/// non-atom node.
///
/// Returns `(derefed_closure, container_env)` where `container_env` is the
/// env frame from which the final closure was retrieved (i.e. the env of the
/// last `Atom{L(i)}` that was resolved). When the L-ref is found inside a
/// **Let** binding, the binding's own env points to the parent scope, but
/// the container_env is the Let frame itself — callers that need to resolve
/// further L-refs from the derefed node (e.g. `Meta{L(0), L(1)}`) should
/// use `container_env` rather than `derefed_closure.env()`.
///
/// # Cycle breaking
///
/// The STG compiler may produce a letrec where a binding `[k] = thunk(Atom{L(k)})`
/// is self-referential (L(k) = binding k itself in the letrec frame). This arises
/// when a block field value is a lambda argument that was captured as a free
/// variable: at compile time `L(k)` referenced the lambda arg, but at runtime
/// inside a letrec frame of N bindings, `L(k)` with k < N maps to letrec
/// binding k (not the outer arg).
///
/// To break the cycle, when we detect that `Atom{L(i)}` resolves to a closure
/// whose (code, env) pair matches the current closure, we try to escape by
/// accessing `env.get(env.logical_len() + i)` — which chains through to the
/// parent frame and picks up the actual value at offset `i` from the parent.
fn dereference(
    view: &MutatorHeapView<'_>,
    mut closure: SynClosure,
) -> (SynClosure, Option<RefPtr<EnvFrame>>) {
    let mut container_env: Option<RefPtr<EnvFrame>> = None;
    let mut depth = 0usize;
    loop {
        if depth > 64 {
            break;
        }
        let code = view.scoped(closure.code());
        match &*code {
            HeapSyn::Atom {
                evaluand: Ref::L(i),
            } => {
                let env_ptr = closure.env();
                let env = view.scoped(env_ptr);
                match env.get(view, *i) {
                    Some(inner) => {
                        // Cycle detection: if the resolved closure has the
                        // same (code, env) as the current closure, we are
                        // stuck in a self-referential letrec thunk.
                        // Break the cycle by jumping to the parent scope.
                        if inner.code() == closure.code() && inner.env() == closure.env() {
                            // Try parent scope: env.get(logical_len + i)
                            let env_len = env.logical_len();
                            if let Some(parent_val) = env.get(view, env_len + *i) {
                                container_env = Some(env_ptr);
                                closure = parent_val;
                                depth += 1;
                                continue;
                            }
                            break;
                        }
                        container_env = Some(env_ptr);
                        closure = inner;
                        depth += 1;
                    }
                    None => break,
                }
            }
            _ => break,
        }
    }
    (closure, container_env)
}

/// Strip a single `Meta` wrapper from a closure, returning the metadata
/// symbol name (if readable) and the body closure.
///
/// Returns `(None, closure)` if there is no metadata wrapper.
///
/// # Let-binding env correction
///
/// When a `Meta{L(m), L(b)}` is stored as a Let binding, the binding
/// closure's own env points to the **parent** scope (not the Let frame
/// itself), because `from_let` sets each binding's env to `next`. The
/// L-ref indices, however, are relative to the **Let frame** (the
/// container that was used when the Atom pointing to this binding was
/// resolved). `peel_meta` therefore resolves L-refs through the
/// `container_env` returned by `dereference` rather than through
/// `derefed.env()`.
fn peel_meta(
    view: &MutatorHeapView<'_>,
    pool: &SymbolPool,
    closure: SynClosure,
) -> (Option<String>, SynClosure) {
    let (derefed, container_env) = dereference(view, closure);
    let code = view.scoped(derefed.code());
    match &*code {
        HeapSyn::Meta { meta, body } => {
            // Use container_env (the Let/LetRec frame that holds this binding)
            // to resolve L(i) refs, falling back to the derefed closure's own
            // env when no container was recorded (i.e. the Meta was not reached
            // via an Atom indirection).
            let resolve_env = |i: usize| -> Option<SynClosure> {
                if let Some(cenv_ptr) = container_env {
                    let cenv = view.scoped(cenv_ptr);
                    if let Some(c) = cenv.get(view, i) {
                        return Some(c);
                    }
                }
                // Fallback: try the derefed closure's own env
                let env = view.scoped(derefed.env());
                env.get(view, i)
            };

            // Read meta ref as a symbol name (resolved via pool for readable text)
            let sym_name = match meta {
                Ref::V(Native::Sym(id)) => Some(pool.resolve(*id).to_string()),
                Ref::L(i) => resolve_env(*i).and_then(|meta_c| {
                    let (mc, _) = dereference(view, meta_c);
                    let mc_code = view.scoped(mc.code());
                    match &*mc_code {
                        HeapSyn::Atom {
                            evaluand: Ref::V(Native::Sym(id)),
                        } => Some(pool.resolve(*id).to_string()),
                        HeapSyn::Cons { tag, args }
                            if *tag == DataConstructor::BoxedSymbol.tag() =>
                        {
                            let r = args.get(0)?;
                            match r {
                                Ref::V(Native::Sym(id)) => Some(pool.resolve(id).to_string()),
                                _ => None,
                            }
                        }
                        _ => None,
                    }
                }),
                _ => None,
            };

            // Resolve body Ref to a closure
            let body_closure = match body {
                Ref::L(i) => resolve_env(*i).unwrap_or_else(|| derefed.clone()),
                Ref::V(_) | Ref::G(_) => derefed.clone(),
            };

            (sym_name, body_closure)
        }
        _ => (None, derefed),
    }
}

/// Convenience wrapper: follow indirection atoms and return just the closure.
///
/// For cases that do not need the container env (list walking, etc.),
/// this avoids cluttering every call site with `let (c, _) = …`.
#[inline]
fn deref(view: &MutatorHeapView<'_>, closure: SynClosure) -> SynClosure {
    dereference(view, closure).0
}

/// Extract the cons-list closure from a `Block` constructor.
///
/// The closure may be:
/// - A `Block(list_ref, no_index)` Cons node directly.
/// - A LetRec/Let thunk whose body is a `Block(...)` — this happens when the
///   block is compiled as a non-strict `letrec [...] in Block(...)`.  In that
///   case we instantiate the LetRec/Let env and look at the Block body without
///   fully evaluating the thunk.
fn block_list(view: &MutatorHeapView<'_>, closure: SynClosure) -> Option<SynClosure> {
    let c = deref(view, closure);
    block_list_inner(view, c, 8)
}

fn block_list_inner(view: &MutatorHeapView<'_>, c: SynClosure, depth: usize) -> Option<SynClosure> {
    if depth == 0 {
        return None;
    }
    let code = view.scoped(c.code());
    match &*code {
        HeapSyn::Cons { tag, args } if *tag == DataConstructor::Block.tag() => {
            let list_ref = args.get(0)?;
            resolve_ref(view, &c, list_ref).ok()
        }
        // A thunk-wrapped LetRec or Let: peek at the body to find the Block.
        // We build the env frame and look at the body code statically.
        HeapSyn::LetRec { bindings, body } => {
            use crate::eval::machine::env_builder::EnvBuilder;
            let env = view
                .from_letrec(
                    bindings.as_slice(),
                    c.env(),
                    crate::common::sourcemap::Smid::default(),
                )
                .ok()?;
            let body_closure = SynClosure::new(*body, env);
            let body_deref = deref(view, body_closure);
            block_list_inner(view, body_deref, depth - 1)
        }
        HeapSyn::Let { bindings, body } => {
            use crate::eval::machine::env_builder::EnvBuilder;
            let env = view
                .from_let(
                    bindings.as_slice(),
                    c.env(),
                    crate::common::sourcemap::Smid::default(),
                )
                .ok()?;
            let body_closure = SynClosure::new(*body, env);
            let body_deref = deref(view, body_closure);
            block_list_inner(view, body_deref, depth - 1)
        }
        // Source annotation node: transparent to IO spec block navigation.
        HeapSyn::Ann { body, .. } => {
            let body_closure = SynClosure::new(*body, c.env());
            let body_deref = deref(view, body_closure);
            block_list_inner(view, body_deref, depth - 1)
        }
        _ => None,
    }
}

/// If the WHNF closure is a boxed scalar (`BoxedString`, `BoxedNumber`,
/// `BoxedSymbol`, `BoxedZdt`), extract the inner closure so it can be
/// evaluated further.  Returns `None` if the closure is not a box.
///
/// This is needed when the STG intrinsic wrapper produces
/// `let [b0 = bif_result] in BoxedString(L(0))`: after `evaluate_to_whnf_for_io`
/// returns this `BoxedString`, the inner `L(0)` is still an unevaluated thunk.
/// Peeling and re-evaluating yields the actual string value.
fn peel_box_inner(machine: &Machine<'_>, closure: SynClosure) -> Option<SynClosure> {
    struct PeelBox(SynClosure);
    impl Mutator for PeelBox {
        type Input = ();
        type Output = Option<SynClosure>;
        fn run(&self, view: &MutatorHeapView, _: ()) -> Result<Option<SynClosure>, ExecutionError> {
            let c = deref(view, self.0.clone());
            let code = view.scoped(c.code());
            match &*code {
                HeapSyn::Cons { tag, args }
                    if *tag == DataConstructor::BoxedString.tag()
                        || *tag == DataConstructor::BoxedNumber.tag()
                        || *tag == DataConstructor::BoxedSymbol.tag()
                        || *tag == DataConstructor::BoxedZdt.tag() =>
                {
                    let inner_ref = match args.get(0) {
                        Some(r) => r,
                        None => return Ok(None),
                    };
                    // Only peel L(i) refs — V(native) refs are already evaluated
                    // and render_closure_to_emitter handles them directly.
                    if matches!(inner_ref, Ref::L(_)) {
                        Ok(resolve_ref(view, &c, inner_ref).ok())
                    } else {
                        Ok(None)
                    }
                }
                _ => Ok(None),
            }
        }
    }
    machine.mutate(PeelBox(closure), ()).ok().flatten()
}

/// Extract a string value from a WHNF closure.
///
/// Returns `None` if the closure contains a complex value (block, list)
/// or an empty/null value.
///
/// This is the canonical way to read a simple scalar from an evaluated
/// spec-block field.  It handles raw atoms and boxed scalars directly
/// without needing the full render emitter machinery.
fn render_whnf_to_string(
    machine: &Machine<'_>,
    closure: SynClosure,
) -> Result<Option<String>, ExecutionError> {
    struct ExtractField {
        closure: SynClosure,
        pool: SymbolPool,
    }
    impl Mutator for ExtractField {
        type Input = ();
        type Output = Option<String>;
        fn run(&self, view: &MutatorHeapView, _: ()) -> Result<Option<String>, ExecutionError> {
            Ok(extract_scalar_string(view, &self.pool, &self.closure))
        }
    }
    machine.mutate(
        ExtractField {
            closure,
            pool: machine.symbol_pool().clone(),
        },
        (),
    )
}

// ─── Mutator: extract raw (unevaluated) field closures from a spec block ──────

/// Walk a cons-list of `BlockPair` nodes and collect the raw (unevaluated)
/// value closure for each field.  Keys that cannot be read as symbol strings
/// are silently skipped.
fn collect_raw_block_fields(
    view: &MutatorHeapView<'_>,
    pool: &SymbolPool,
    list_closure: SynClosure,
) -> HashMap<String, SynClosure> {
    let mut fields = HashMap::new();
    let mut current = deref(view, list_closure);

    loop {
        let code = view.scoped(current.code());
        match &*code {
            HeapSyn::Cons { tag, .. } if *tag == DataConstructor::ListNil.tag() => break,
            HeapSyn::Cons { tag, args } if *tag == DataConstructor::ListCons.tag() => {
                let head_ref = match args.get(0) {
                    Some(r) => r,
                    None => break,
                };
                let tail_ref = match args.get(1) {
                    Some(r) => r,
                    None => break,
                };
                if let Ok(head) = resolve_ref(view, &current, head_ref) {
                    collect_raw_pair(view, pool, deref(view, head), &mut fields);
                }
                match resolve_ref(view, &current, tail_ref) {
                    Ok(tail) => current = deref(view, tail),
                    Err(_) => break,
                }
            }
            _ => break,
        }
    }

    fields
}

/// Read a single `BlockPair` node: resolve the key to a string and record the
/// raw (unevaluated) value closure.  Symbol keys only; others are skipped.
fn collect_raw_pair(
    view: &MutatorHeapView<'_>,
    pool: &SymbolPool,
    pair: SynClosure,
    fields: &mut HashMap<String, SynClosure>,
) {
    let code = view.scoped(pair.code());
    if let HeapSyn::Cons { tag, args } = &*code {
        if *tag != DataConstructor::BlockPair.tag() {
            return;
        }
        let key_ref = match args.get(0) {
            Some(r) => r,
            None => return,
        };
        let val_ref = match args.get(1) {
            Some(r) => r,
            None => return,
        };
        let key_name = match &key_ref {
            Ref::V(Native::Sym(id)) => pool.resolve(*id).to_string(),
            _ => return,
        };
        let val_ref_clone = val_ref.clone();
        if let Ok(val_closure) = resolve_ref(view, &pair, val_ref_clone) {
            fields.insert(key_name, deref(view, val_closure));
        }
    }
}

// ─── Mutators: list inspection for field evaluation ───────────────────────────

/// Mutator that returns `true` if the closure is a `ListCons` or `ListNil`.
struct IsList(SynClosure);

impl Mutator for IsList {
    type Input = ();
    type Output = bool;

    fn run(&self, view: &MutatorHeapView, _: ()) -> Result<bool, ExecutionError> {
        let c = deref(view, self.0.clone());
        let code = view.scoped(c.code());
        Ok(matches!(
            &*code,
            HeapSyn::Cons { tag, .. }
                if *tag == DataConstructor::ListCons.tag()
                    || *tag == DataConstructor::ListNil.tag()
        ))
    }
}

/// Mutator that collects the raw closures of all elements in a `ListCons` chain.
///
/// Does not evaluate the elements — returns unevaluated closures so the caller
/// can force each one to WHNF using the machine separately.
///
/// The `Input` is the globals env frame, needed to resolve `G(i)` refs that
/// appear as the list tail sentinel (the compiled `ListNil` global).
struct CollectListElements(SynClosure);

impl Mutator for CollectListElements {
    type Input = Option<RefPtr<EnvFrame>>;
    type Output = Vec<SynClosure>;

    fn run(
        &self,
        view: &MutatorHeapView,
        globals_env: Option<RefPtr<EnvFrame>>,
    ) -> Result<Vec<SynClosure>, ExecutionError> {
        let mut result = Vec::new();
        let mut current = deref(view, self.0.clone());
        loop {
            let code = view.scoped(current.code());
            match &*code {
                HeapSyn::Cons { tag, .. } if *tag == DataConstructor::ListNil.tag() => break,
                HeapSyn::Cons { tag, args } if *tag == DataConstructor::ListCons.tag() => {
                    let head_ref = args.get(0).ok_or_else(|| {
                        ExecutionError::Panic(Smid::default(), "malformed list cons".to_string())
                    })?;
                    let tail_ref = args.get(1).ok_or_else(|| {
                        ExecutionError::Panic(Smid::default(), "malformed list cons".to_string())
                    })?;
                    let head = resolve_ref_with_globals(view, &current, head_ref, globals_env)?;
                    result.push(deref(view, head));
                    current = deref(
                        view,
                        resolve_ref_with_globals(view, &current, tail_ref, globals_env)?,
                    );
                }
                _ => break,
            }
        }
        Ok(result)
    }
}

/// Build an `ActionSpec` from an `IoAction` spec block by evaluating each field
/// closure to WHNF using the machine.
///
/// Handles two cases:
///
/// 1. **Inline block** (`io.shell`, `io.exec`): the spec block is a LetRec thunk
///    with a `Meta(tag, Block(...))` body that is statically visible.  The tag
///    is extracted via `peel_meta` and the body closure is evaluated to WHNF.
///
/// 2. **Function-application block** (`io.shell-with`, `io.exec-with`): the spec
///    block is an App thunk (e.g. `io.shell-with(opts, cmd)`).  Evaluating it
///    strips the Meta wrapper (the machine discards metadata when nothing consumes
///    it), so the tag must be inferred from the resulting field set: a block with
///    `cmd` but no `args` is an `io-shell` action; a block with both is `io-exec`.
///
/// In both cases, field closures are extracted from the evaluated block and each
/// is forced to WHNF before reading, so `opts lookup-or(...)` thunks are resolved.
fn evaluate_spec_block(
    machine: &mut Machine<'_>,
    spec_block: SynClosure,
) -> Result<ActionSpec, IoRunError> {
    // ── Try to extract the metadata tag statically ────────────────────────────
    //
    // For inline blocks the Meta node is directly visible after dereferencing
    // letrec atoms.  For App thunks the Meta is only produced during evaluation
    // and `peel_meta` returns `None`.
    let pool = machine.symbol_pool().clone();

    struct PeekMeta(SynClosure);
    impl Mutator for PeekMeta {
        type Input = SymbolPool;
        type Output = (Option<String>, SynClosure);
        fn run(
            &self,
            view: &MutatorHeapView,
            pool: SymbolPool,
        ) -> Result<(Option<String>, SynClosure), ExecutionError> {
            let (tag_opt, body) = peel_meta(view, &pool, self.0.clone());
            Ok((tag_opt, body))
        }
    }

    let (static_tag, body_or_spec) = machine
        .mutate(PeekMeta(spec_block), pool)
        .map_err(IoRunError::from)?;

    // ── Evaluate the block body to WHNF ──────────────────────────────────────
    //
    // For inline blocks `body_or_spec` is the block body (LetRec thunk that
    // produces the Block cons).  For App thunks it is the whole spec block
    // (which evaluates to the Block cons, stripping the Meta).
    //
    // After this call the machine is back in IO yield state.
    let evaluated_block = machine
        .evaluate_to_whnf_for_io(body_or_spec)
        .map_err(IoRunError::from)?;

    // ── Extract raw field closures from the evaluated block ───────────────────
    //
    // The evaluated block is a Block constructor.  Its field values are either
    // already WHNF atoms or fresh thunks (e.g. `opts lookup-or(:timeout, 30)`)
    // created during the evaluation above.  Static navigation returns closures
    // that can safely be evaluated by the machine.
    struct ReadBlockFields(SynClosure);
    impl Mutator for ReadBlockFields {
        type Input = SymbolPool;
        type Output = HashMap<String, SynClosure>;
        fn run(
            &self,
            view: &MutatorHeapView,
            pool: SymbolPool,
        ) -> Result<HashMap<String, SynClosure>, ExecutionError> {
            let list_closure = block_list(view, self.0.clone()).ok_or_else(|| {
                ExecutionError::Panic(
                    Smid::default(),
                    "IoAction spec block did not evaluate to a Block constructor".to_string(),
                )
            })?;
            Ok(collect_raw_block_fields(view, &pool, list_closure))
        }
    }

    let pool = machine.symbol_pool().clone();
    let raw_fields = machine
        .mutate(ReadBlockFields(evaluated_block), pool)
        .map_err(IoRunError::from)?;

    // ── Evaluate each field closure to WHNF and render its string value ──────
    //
    // Each field is forced to WHNF by the machine, then rendered to a plain
    // string via the text emitter.  Using `render_closure_to_emitter` handles
    // all WHNF forms correctly, including `BoxedString(L(i))` closures produced
    // by the STG intrinsic wrapper (e.g. when the cmd is a format expression).
    //
    // For list-valued fields (e.g. `args` in io.exec) we walk the cons list and
    // render each element individually, then join with NUL as a separator.
    //
    // SAFETY: each call to evaluate_to_whnf_for_io runs machine.run() which
    // ends with collect::collect.  If evacuation occurs, any SynClosure values
    // held on the Rust stack (i.e. the remaining entries in raw_fields) become
    // dangling — the GC does not know about them.  We convert raw_fields into
    // an ordered Vec and stash ALL closures upfront so the GC can trace and
    // update their heap pointers during each sub-evaluation.  Each closure is
    // popped from the stash immediately before it is used.
    let raw_fields_vec: Vec<(String, SynClosure)> = raw_fields.into_iter().collect();
    // Push all closures onto the stash (bottom = index n-1, top = index 0).
    // We iterate in reverse so that after all pushes the order matches the Vec
    // (raw_fields_vec[0] is deepest, raw_fields_vec[last] is shallowest).
    // We will pop them in forward order, so push in reverse.
    for (_, c) in raw_fields_vec.iter().rev() {
        machine.stash_push(c.clone());
    }
    let mut eval_fields: HashMap<String, Option<String>> = HashMap::new();
    for (key, _) in raw_fields_vec {
        // Pop the GC-protected closure for this field (in original order).
        let raw_closure = machine.stash_pop();
        let whnf = machine
            .evaluate_to_whnf_for_io(raw_closure)
            .map_err(IoRunError::from)?;

        // If the WHNF is a boxed scalar with an unevaluated inner thunk
        // (e.g. `BoxedString(L(0))` produced by the STG intrinsic wrapper
        // when `cmd` is a format expression), peel the box and evaluate the
        // inner closure to get the actual native value.
        let whnf = if let Some(inner) = peel_box_inner(machine, whnf.clone()) {
            machine
                .evaluate_to_whnf_for_io(inner)
                .map_err(IoRunError::from)?
        } else {
            whnf
        };

        // Check whether the WHNF is a list (ListCons or ListNil).
        // List-valued fields (e.g. `args`) need each element rendered separately.
        let is_list = machine.mutate(IsList(whnf.clone()), ()).unwrap_or(false);

        let value_str = if is_list {
            // Collect list element closures, evaluate each to WHNF, render.
            // Pass globals env so G(i) list tail refs (compiled ListNil) resolve.
            let globals = machine.globals_env();
            let elements = machine
                .mutate(CollectListElements(whnf), Some(globals))
                .map_err(IoRunError::from)?;
            // Stash all element closures before the evaluation loop for the same
            // reason as raw_fields above: evaluate_to_whnf_for_io triggers GC
            // which can evacuate remaining elements, leaving their stack copies
            // dangling.  Push in reverse so we pop in forward order.
            for e in elements.iter().rev() {
                machine.stash_push(e.clone());
            }
            let count = elements.len();
            let mut parts: Vec<String> = Vec::new();
            for _ in 0..count {
                let elem = machine.stash_pop();
                let elem_whnf = machine
                    .evaluate_to_whnf_for_io(elem)
                    .map_err(IoRunError::from)?;
                if let Some(s) =
                    render_whnf_to_string(machine, elem_whnf).map_err(IoRunError::from)?
                {
                    parts.push(s);
                }
            }
            if parts.is_empty() {
                None
            } else {
                Some(parts.join("\x00"))
            }
        } else {
            render_whnf_to_string(machine, whnf).map_err(IoRunError::from)?
        };
        eval_fields.insert(key, value_str);
    }

    // ── Determine tag ─────────────────────────────────────────────────────────
    //
    // For inline blocks the tag was captured statically above.
    // For App-thunk blocks the Meta was stripped by evaluation; infer from fields.
    let tag_name = static_tag.unwrap_or_else(|| {
        if eval_fields.contains_key("args") {
            "io-exec".to_string()
        } else {
            "io-shell".to_string()
        }
    });

    // Step 4: build ActionSpec from evaluated fields.
    let timeout_secs = eval_fields
        .get("timeout")
        .and_then(|opt| opt.as_deref())
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(30);

    let stdin = eval_fields
        .get("stdin")
        .and_then(|opt| opt.clone())
        .filter(|s| !s.is_empty() && s != "null");

    let is_shell = tag_name == "io-shell";
    let is_exec = tag_name == "io-exec";

    if is_shell {
        let cmd = eval_fields
            .get("cmd")
            .and_then(|opt| opt.clone())
            .ok_or_else(|| {
                IoRunError::MachineError(Box::new(ExecutionError::Panic(
                    Smid::default(),
                    "io-shell spec missing 'cmd'".to_string(),
                )))
            })?;
        Ok(ActionSpec::Shell {
            cmd,
            stdin,
            timeout_secs,
        })
    } else if is_exec {
        let cmd = eval_fields
            .get("cmd")
            .and_then(|opt| opt.clone())
            .ok_or_else(|| {
                IoRunError::MachineError(Box::new(ExecutionError::Panic(
                    Smid::default(),
                    "io-exec spec missing 'cmd'".to_string(),
                )))
            })?;
        let args = eval_fields
            .get("args")
            .and_then(|opt| opt.clone())
            .map(|s| {
                if s.is_empty() {
                    vec![]
                } else {
                    s.split('\x00').map(|x| x.to_string()).collect()
                }
            })
            .unwrap_or_default();
        Ok(ActionSpec::Exec {
            cmd,
            args,
            stdin,
            timeout_secs,
        })
    } else if tag_name == "io-fail" {
        let message = eval_fields
            .get("message")
            .and_then(|opt| opt.clone())
            .unwrap_or_else(|| "io.fail".to_string());
        Err(IoRunError::Fail(message))
    } else {
        Err(IoRunError::MachineError(Box::new(ExecutionError::Panic(
            Smid::default(),
            format!("unrecognised IO action tag: {tag_name}"),
        ))))
    }
}

// ─── Mutator: build result block {stdout, stderr, exit-code} ─────────────────

/// Mutator that builds the result block `{stdout: Str, stderr: Str, exit-code: Num}`
/// on the heap and returns a SynClosure.
struct BuildResultBlock {
    stdout: String,
    stderr: String,
    exit_code: i64,
    root_env: RefPtr<EnvFrame>,
}

impl Mutator for BuildResultBlock {
    type Input = SymbolPool;
    type Output = SynClosure;

    fn run(
        &self,
        view: &MutatorHeapView,
        mut pool: SymbolPool,
    ) -> Result<SynClosure, ExecutionError> {
        // Build value closures (each with root_env as parent).
        //
        // Strings must be wrapped as BoxedString data constructors so that
        // eucalypt string intrinsics (which case-match on BoxedString tag 5)
        // receive a value in the expected form.  Numeric exit code is wrapped
        // as BoxedNumber for the same reason.
        let stdout_ref = view.str_ref(self.stdout.as_str())?;
        let stderr_ref = view.str_ref(self.stderr.as_str())?;
        let exit_code_ref = Ref::V(Native::Num(serde_json::Number::from(self.exit_code)));

        let stdout_atom = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::BoxedString.tag(),
                args: Array::from_slice(view, &[stdout_ref]),
            })?
            .as_ptr();
        let stderr_atom = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::BoxedString.tag(),
                args: Array::from_slice(view, &[stderr_ref]),
            })?
            .as_ptr();
        let exit_code_atom = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::BoxedNumber.tag(),
                args: Array::from_slice(view, &[exit_code_ref]),
            })?
            .as_ptr();

        let stdout_c = SynClosure::new(stdout_atom, self.root_env);
        let stderr_c = SynClosure::new(stderr_atom, self.root_env);
        let exit_code_c = SynClosure::new(exit_code_atom, self.root_env);

        // Flat env frame holding all three values: [stdout=0, stderr=1, exit_code=2]
        let value_frame = view.from_closures(
            [stdout_c, stderr_c, exit_code_c].iter().cloned(),
            3,
            self.root_env,
            Smid::default(),
        )?;

        // Symbol refs for block keys
        let stdout_sym = view.sym_ref(&mut pool, "stdout")?;
        let stderr_sym = view.sym_ref(&mut pool, "stderr")?;
        let exitcode_sym = view.sym_ref(&mut pool, "exit-code")?;

        // Build three BlockPair nodes.  Each pair's env is value_frame so that
        // Ref::L(0/1/2) resolves to the correct value atom when LOOKUP calls
        // resolve_in_closure(pair, val_ref).
        let pair0_ptr = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::BlockPair.tag(),
                args: Array::from_slice(view, &[stdout_sym, Ref::L(0)]),
            })?
            .as_ptr();
        let pair1_ptr = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::BlockPair.tag(),
                args: Array::from_slice(view, &[stderr_sym, Ref::L(1)]),
            })?
            .as_ptr();
        let pair2_ptr = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::BlockPair.tag(),
                args: Array::from_slice(view, &[exitcode_sym, Ref::L(2)]),
            })?
            .as_ptr();

        let pair0_c = SynClosure::new(pair0_ptr, value_frame);
        let pair1_c = SynClosure::new(pair1_ptr, value_frame);
        let pair2_c = SynClosure::new(pair2_ptr, value_frame);

        // Build the cons list as fully-materialised closures (no letrec thunks).
        //
        // The BlockListIterator calls resolve_in_closure(list_cell, head_ref).
        // Each ListCons cell uses Ref::L(0) for head and Ref::L(1) for tail,
        // so the closure's env must map L(0) → pair, L(1) → next list cell.
        //
        // List built tail-first: nil → c2 → c1 → c0.

        let nil_ptr = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::ListNil.tag(),
                args: Array::default(),
            })?
            .as_ptr();
        let nil_c = SynClosure::new(nil_ptr, self.root_env);

        // Shared ListCons shape: Cons{ListCons, [L(0), L(1)]}
        let list_cons_syn = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::ListCons.tag(),
                args: Array::from_slice(view, &[Ref::L(0), Ref::L(1)]),
            })?
            .as_ptr();

        // c2: head=pair2, tail=nil
        let c2_frame = view.from_closures(
            [pair2_c.clone(), nil_c].iter().cloned(),
            2,
            self.root_env,
            Smid::default(),
        )?;
        let c2_c = SynClosure::new(list_cons_syn, c2_frame);

        // c1: head=pair1, tail=c2
        let c1_frame = view.from_closures(
            [pair1_c.clone(), c2_c.clone()].iter().cloned(),
            2,
            self.root_env,
            Smid::default(),
        )?;
        let c1_c = SynClosure::new(list_cons_syn, c1_frame);

        // c0: head=pair0, tail=c1
        let c0_frame = view.from_closures(
            [pair0_c.clone(), c1_c].iter().cloned(),
            2,
            self.root_env,
            Smid::default(),
        )?;
        let c0_c = SynClosure::new(list_cons_syn, c0_frame);

        // Frame holding the list head: [list=0]
        let list_frame = view.from_closure(c0_c, self.root_env, Smid::default())?;

        // Block { list=L(0), no_index }
        let no_index = Ref::V(Native::Num(serde_json::Number::from(0)));
        let block_ptr = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::Block.tag(),
                args: Array::from_slice(view, &[Ref::L(0), no_index]),
            })?
            .as_ptr();

        // Suppress unused-variable warnings for clones used only above
        let _ = (pair1_c, pair2_c, c2_c, pair0_c);

        Ok(SynClosure::new(block_ptr, list_frame))
    }
}

// ─── Mutator: wrap value in IoReturn ─────────────────────────────────────────

struct BuildIoReturn {
    world: SynClosure,
    value: SynClosure,
    root_env: RefPtr<EnvFrame>,
}

impl Mutator for BuildIoReturn {
    type Input = ();
    type Output = SynClosure;

    fn run(&self, view: &MutatorHeapView, _: ()) -> Result<SynClosure, ExecutionError> {
        // Frame: [world=0, value=1]
        let env = view.from_closures(
            [self.world.clone(), self.value.clone()].iter().cloned(),
            2,
            self.root_env,
            Smid::default(),
        )?;
        let cons_syn = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::IoReturn.tag(),
                args: Array::from_slice(view, &[Ref::L(0), Ref::L(1)]),
            })?
            .as_ptr();
        Ok(SynClosure::new(cons_syn, env))
    }
}

// ─── Mutator: build RENDER_DOC call for the final value ──────────────────────

struct BuildRenderDoc {
    value: SynClosure,
    root_env: RefPtr<EnvFrame>,
}

impl Mutator for BuildRenderDoc {
    type Input = ();
    type Output = SynClosure;

    fn run(&self, view: &MutatorHeapView, _: ()) -> Result<SynClosure, ExecutionError> {
        let render_doc_idx = intrinsics::index("RENDER_DOC").ok_or_else(|| {
            ExecutionError::Panic(
                Smid::default(),
                "RENDER_DOC intrinsic not found in registry".to_string(),
            )
        })?;

        // Frame: [value=0]
        let env = view.from_closure(self.value.clone(), self.root_env, Smid::default())?;
        // App { callable: G(render_doc_idx), args: [L(0)] }
        let app_syn = view
            .alloc(HeapSyn::App {
                callable: Ref::G(render_doc_idx),
                args: Array::from_slice(view, &[Ref::L(0)]),
            })?
            .as_ptr();
        Ok(SynClosure::new(app_syn, env))
    }
}

// ─── Shell execution ──────────────────────────────────────────────────────────

/// Execute a shell command via the platform shell.
///
/// On Unix, uses `sh -c`. On Windows, uses `pwsh -NoProfile -Command`
/// (PowerShell Core). Shell command strings are inherently
/// platform-specific.
fn execute_shell(
    cmd: &str,
    stdin_data: Option<&str>,
    timeout_secs: u64,
) -> Result<CommandResult, IoRunError> {
    let command = if cfg!(windows) {
        let mut c = Command::new("pwsh");
        c.args(["-NoProfile", "-Command", cmd]);
        c
    } else {
        let mut c = Command::new("sh");
        c.args(["-c", cmd]);
        c
    };
    run_command(command, stdin_data, timeout_secs)
}

/// Execute a binary directly.
fn execute_exec(
    cmd: &str,
    args: &[String],
    stdin_data: Option<&str>,
    timeout_secs: u64,
) -> Result<CommandResult, IoRunError> {
    let mut command = Command::new(cmd);
    command.args(args);
    run_command(command, stdin_data, timeout_secs)
}

/// Core helper: spawn the command, optionally write stdin, and wait with timeout.
fn run_command(
    mut command: Command,
    stdin_data: Option<&str>,
    timeout_secs: u64,
) -> Result<CommandResult, IoRunError> {
    command
        .stdin(if stdin_data.is_some() {
            Stdio::piped()
        } else {
            Stdio::null()
        })
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = match command.spawn() {
        Ok(child) => child,
        Err(e) => {
            // Spawn failure (e.g. binary not found) returns a result block
            // rather than a hard error, matching shell conventions.
            let exit_code = match e.kind() {
                std::io::ErrorKind::NotFound => 127,
                std::io::ErrorKind::PermissionDenied => 126,
                _ => -1,
            };
            return Ok(CommandResult {
                stdout: String::new(),
                stderr: e.to_string(),
                exit_code,
            });
        }
    };

    if let Some(input) = stdin_data {
        if let Some(mut pipe) = child.stdin.take() {
            pipe.write_all(input.as_bytes())
                .map_err(|e| IoRunError::CommandError(Smid::default(), e.to_string()))?;
            // Drop `pipe` to close stdin and signal EOF to the child
        }
    }

    // Use a worker thread to implement timeout, since `wait_timeout` is not a
    // project dependency.
    let (tx, rx) = mpsc::channel();
    thread::spawn(move || {
        let _ = tx.send(child.wait_with_output());
    });

    match rx.recv_timeout(Duration::from_secs(timeout_secs)) {
        Ok(Ok(output)) => {
            let exit_code = output.status.code().unwrap_or(-1) as i64;
            let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
            let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
            Ok(CommandResult {
                stdout,
                stderr,
                exit_code,
            })
        }
        Ok(Err(e)) => Err(IoRunError::CommandError(Smid::default(), e.to_string())),
        Err(_timeout) => Err(IoRunError::Timeout(Smid::default(), timeout_secs)),
    }
}

/// Whether IO tracing is enabled (cached from `EU_IO_TRACE` env var).
static IO_TRACE: std::sync::LazyLock<bool> =
    std::sync::LazyLock::new(|| std::env::var("EU_IO_TRACE").is_ok());

/// Dispatch an `ActionSpec` to the appropriate executor.
fn run_spec(spec: &ActionSpec) -> Result<CommandResult, IoRunError> {
    if *IO_TRACE {
        match spec {
            ActionSpec::Shell { cmd, stdin, .. } => {
                eprintln!(
                    "IO TRACE: shell {cmd:?}{}",
                    if stdin.is_some() { " (with stdin)" } else { "" }
                );
            }
            ActionSpec::Exec {
                cmd, args, stdin, ..
            } => {
                eprintln!(
                    "IO TRACE: exec {cmd:?} {args:?}{}",
                    if stdin.is_some() { " (with stdin)" } else { "" }
                );
            }
        }
    }
    let result = match spec {
        ActionSpec::Shell {
            cmd,
            stdin,
            timeout_secs,
        } => execute_shell(cmd, stdin.as_deref(), *timeout_secs),
        ActionSpec::Exec {
            cmd,
            args,
            stdin,
            timeout_secs,
        } => execute_exec(cmd, args, stdin.as_deref(), *timeout_secs),
    };
    if *IO_TRACE {
        match &result {
            Ok(r) => eprintln!("IO TRACE: → exit {}", r.exit_code),
            Err(e) => eprintln!("IO TRACE: → error: {e}"),
        }
    }
    result
}

// ─── Error message extraction ─────────────────────────────────────────────────

/// Try to read an error string from an IoFail error closure.
fn extract_error_string(machine: &Machine<'_>, closure: &SynClosure) -> String {
    render_whnf_to_string(machine, closure.clone())
        .ok()
        .flatten()
        .unwrap_or_else(|| "IO monad failure".to_string())
}

// ─── IO monad interpret loop ──────────────────────────────────────────────────

/// Interpret the IO monad until an `IoReturn` or `IoFail` is reached.
///
/// The machine must be in the io_yielded state when this is called.
/// Returns the pure value closure from the final `IoReturn`.
pub fn io_run(machine: &mut Machine<'_>, allow_io: bool) -> Result<SynClosure, IoRunError> {
    loop {
        if !machine.io_yielded() {
            return Err(IoRunError::MachineError(Box::new(ExecutionError::Panic(
                Smid::default(),
                "io_run called on non-yielded machine".to_string(),
            ))));
        }

        let tag = machine.yielded_io_tag().unwrap();
        let args = machine.yielded_io_args().unwrap();

        match DataConstructor::try_from(tag) {
            Ok(DataConstructor::IoReturn) => {
                // IoReturn(world=0, value=1)
                return args.into_iter().nth(1).ok_or_else(|| {
                    IoRunError::MachineError(Box::new(ExecutionError::Panic(
                        Smid::default(),
                        "IoReturn missing value argument".to_string(),
                    )))
                });
            }

            Ok(DataConstructor::IoFail) => {
                let error_closure = args.into_iter().nth(1).ok_or_else(|| {
                    IoRunError::MachineError(Box::new(ExecutionError::Panic(
                        Smid::default(),
                        "IoFail missing error argument".to_string(),
                    )))
                })?;
                let msg = extract_error_string(machine, &error_closure);
                return Err(IoRunError::Fail(msg));
            }

            Ok(DataConstructor::IoAction) => {
                if !allow_io {
                    return Err(IoRunError::IoNotAllowed(machine.annotation()));
                }
                let world = args[0].clone();
                let spec_block = args[1].clone();

                // Stash world so it survives GC across the calls below.
                machine.stash_push(world);

                // Evaluate the action spec block.  `evaluate_spec_block` uses
                // machine evaluation to force each field to WHNF before reading,
                // so it handles parameterised spec blocks (shell-with, exec-with)
                // that contain unevaluated thunks (lookup-or calls, etc.).
                let spec = evaluate_spec_block(machine, spec_block)?;

                // Capture source annotation before the shell call so
                // timeout / command errors carry a source location.
                let ann = machine.annotation();

                // Execute the shell action
                let result = run_spec(&spec).map_err(|e| match e {
                    IoRunError::Timeout(_, secs) => IoRunError::Timeout(ann, secs),
                    IoRunError::CommandError(_, msg) => IoRunError::CommandError(ann, msg),
                    other => other,
                })?;

                // Pre-intern the result block key symbols into the machine's
                // pool so that the IDs embedded by BuildResultBlock are already
                // present in the machine pool.  Without this, BuildResultBlock
                // interns into a cloned pool and the heap objects contain IDs
                // the machine pool doesn't know, causing index-out-of-bounds
                // panics when the machine later resolves the result block.
                machine.intern_symbol("stdout");
                machine.intern_symbol("stderr");
                machine.intern_symbol("exit-code");

                // Build the result block closure.  World remains stashed as a GC root.
                let pool = machine.symbol_pool().clone();
                let result_c = machine
                    .mutate(
                        BuildResultBlock {
                            stdout: result.stdout,
                            stderr: result.stderr,
                            exit_code: result.exit_code,
                            root_env: machine.root_env(),
                        },
                        pool,
                    )
                    .map_err(IoRunError::from)?;

                // Stash result_c to protect it during the BuildIoReturn mutator.
                machine.stash_push(result_c);
                let result_c = machine.stash_pop();
                let world = machine.stash_pop();

                // Wrap in IoReturn(world, result_block) and resume.
                // Stash io_return_c across machine.run() for GC safety.
                let io_return_c = machine
                    .mutate(
                        BuildIoReturn {
                            world,
                            value: result_c,
                            root_env: machine.root_env(),
                        },
                        (),
                    )
                    .map_err(IoRunError::from)?;

                machine.stash_push(io_return_c);
                let io_return_c = machine.stash_peek(0);
                machine.resume(io_return_c);
                machine.run(None).map_err(IoRunError::from)?;
                machine.stash_pop();
                // Loop back to inspect the new yield
            }

            Ok(DataConstructor::IoBind) => {
                // IoBind(world=0, cont=1, action=2)
                //
                // When the prelude calls io.bind(action, cont), it produces
                // __IO_BIND(action)(cont) — a PAP waiting for world.  After
                // world injection the 3-arg lambda is saturated as
                // IO_BIND(action, cont, world), which places the args in
                // lref order [action=lref(0), cont=lref(1), world=lref(2)]
                // and the body IoBind(lref(2), lref(1), lref(0)) yields
                // IoBind(world, cont, action) → args[0]=world, args[1]=cont,
                // args[2]=action.
                let world = args[0].clone();
                let cont = args[1].clone();
                let action = args[2].clone();

                // Stash `cont` and `world` so they survive GC during the
                // recursive machine.run() calls below.  Without this, the
                // collector may sweep heap objects reachable only from these
                // Rust-stack closures.
                machine.stash_push(cont);
                machine.stash_push(world);

                // Step 1: apply world to action (action is a PAP waiting for
                // world: e.g. io.return(42) = λworld.IoReturn(world,42)).
                // We read world back from the stash (index 0 = top).
                let world_ref = machine.stash_peek(0);
                let action_with_world = machine
                    .mutate(
                        BuildApply1 {
                            func: action,
                            arg: world_ref,
                            root_env: machine.root_env(),
                        },
                        (),
                    )
                    .map_err(IoRunError::from)?;

                // Stash action_with_world so it is a GC root across machine.run().
                // The GC scans state.closure (set by resume()) but stashing here
                // provides belt-and-suspenders protection and ensures the pointer
                // is updated via scan_and_update if objects are evacuated.
                // Stash order (top to bottom): action_with_world, world, cont
                machine.stash_push(action_with_world);
                let action_with_world = machine.stash_peek(0);
                machine.resume(action_with_world);
                machine.run(None).map_err(IoRunError::from)?;
                // Pop action_with_world; state.closure now holds the updated reference.
                machine.stash_pop();

                if !machine.io_yielded() {
                    machine.stash_pop();
                    machine.stash_pop();
                    return Err(IoRunError::MachineError(Box::new(ExecutionError::Panic(
                        Smid::default(),
                        "IoBind action did not yield an IO constructor".to_string(),
                    ))));
                }

                // Step 2: recursively process the action result.
                // `cont` and `world` remain stashed throughout.
                let action_result = io_run(machine, allow_io)?;

                // Stash action_result to protect it across the BuildApply2 mutator
                // allocations.  GC runs during `from_closures` and would not see
                // `action_result` on the Rust stack otherwise.
                machine.stash_push(action_result);

                // Step 3: apply continuation to result AND world.
                // cont(action_result) gives a PAP; applying world produces
                // the next IO constructor.
                // Stash order (top to bottom): action_result, world, cont
                // Peek (not pop) so the values remain GC-rooted during the
                // BuildApply2 allocation; a GC triggered inside mutate would
                // otherwise sweep these closures from the Rust stack.
                let action_result = machine.stash_peek(0);
                let world = machine.stash_peek(1);
                let cont = machine.stash_peek(2);

                let cont_call = machine
                    .mutate(
                        BuildApply2 {
                            func: cont,
                            arg0: action_result,
                            arg1: world,
                            root_env: machine.root_env(),
                        },
                        (),
                    )
                    .map_err(IoRunError::from)?;

                // Now safe to pop — allocation is complete.
                machine.stash_pop(); // action_result
                machine.stash_pop(); // world
                machine.stash_pop(); // cont

                // Stash cont_call as a GC root across machine.run() for the
                // same reason as action_with_world above: belt-and-suspenders
                // protection in case of evacuation during the run.
                machine.stash_push(cont_call);
                let cont_call = machine.stash_peek(0);
                machine.resume(cont_call);
                machine.run(None).map_err(IoRunError::from)?;
                // Pop cont_call; state.closure holds the updated reference.
                machine.stash_pop();
                // Loop back
            }

            _ => {
                return Err(IoRunError::MachineError(Box::new(ExecutionError::Panic(
                    Smid::default(),
                    format!("unexpected IO constructor tag: {tag}"),
                ))));
            }
        }
    }
}

/// Mutator: build `App(func, [arg])` — applies a single argument.
struct BuildApply1 {
    func: SynClosure,
    arg: SynClosure,
    root_env: RefPtr<EnvFrame>,
}

impl Mutator for BuildApply1 {
    type Input = ();
    type Output = SynClosure;

    fn run(&self, view: &MutatorHeapView, _: ()) -> Result<SynClosure, ExecutionError> {
        // Frame: [arg=0, func=1]
        let env = view.from_closures(
            [self.arg.clone(), self.func.clone()].iter().cloned(),
            2,
            self.root_env,
            Smid::default(),
        )?;
        // App { callable: L(1), args: [L(0)] }
        let app_syn = view
            .alloc(HeapSyn::App {
                callable: Ref::L(1),
                args: Array::from_slice(view, &[Ref::L(0)]),
            })?
            .as_ptr();
        Ok(SynClosure::new(app_syn, env))
    }
}

/// Mutator: build `App(func, [arg0, arg1])` — applies two arguments.
struct BuildApply2 {
    func: SynClosure,
    arg0: SynClosure,
    arg1: SynClosure,
    root_env: RefPtr<EnvFrame>,
}

impl Mutator for BuildApply2 {
    type Input = ();
    type Output = SynClosure;

    fn run(&self, view: &MutatorHeapView, _: ()) -> Result<SynClosure, ExecutionError> {
        // Frame: [arg0=0, arg1=1, func=2]
        let env = view.from_closures(
            [self.arg0.clone(), self.arg1.clone(), self.func.clone()]
                .iter()
                .cloned(),
            3,
            self.root_env,
            Smid::default(),
        )?;
        // App { callable: L(2), args: [L(0), L(1)] }
        let app_syn = view
            .alloc(HeapSyn::App {
                callable: Ref::L(2),
                args: Array::from_slice(view, &[Ref::L(0), Ref::L(1)]),
            })?
            .as_ptr();
        Ok(SynClosure::new(app_syn, env))
    }
}

/// Mutator: build `App(io_fn, [unit])` — applies the world token (unit) to an
/// IO function produced by the prelude.
///
/// The eucalypt prelude represents IO actions as functions waiting for a world
/// token: e.g. `io.return(a)` compiles to `λworld. IoReturn(world, a)`.  The
/// io-run driver must apply the initial world (unit) to trigger the first
/// IO constructor yield.
struct BuildApplyWorld {
    io_fn: SynClosure,
    root_env: RefPtr<EnvFrame>,
}

impl Mutator for BuildApplyWorld {
    type Input = ();
    type Output = SynClosure;

    fn run(&self, view: &MutatorHeapView, _: ()) -> Result<SynClosure, ExecutionError> {
        // Allocate a unit data constructor: Cons(tag=0, args=[])
        let unit_syn = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::Unit.tag(),
                args: Array::from_slice(view, &[]),
            })?
            .as_ptr();

        // Create unit closure (borrowing root_env as parent frame)
        let unit_env = view.from_closures(std::iter::empty(), 0, self.root_env, Smid::default())?;
        let unit_c = SynClosure::new(unit_syn, unit_env);

        // Frame: [world=0, io_fn=1]
        let env = view.from_closures(
            [unit_c, self.io_fn.clone()].iter().cloned(),
            2,
            self.root_env,
            Smid::default(),
        )?;
        // App { callable: L(1), args: [L(0)] }  (apply io_fn to world)
        let app_syn = view
            .alloc(HeapSyn::App {
                callable: Ref::L(1),
                args: Array::from_slice(view, &[Ref::L(0)]),
            })?
            .as_ptr();
        Ok(SynClosure::new(app_syn, env))
    }
}

/// Apply the initial world token (unit) to an IO function and re-run the
/// machine so that the IO constructor is produced and the machine yields.
///
/// Called from `eval.rs` when the machine terminates normally after a headless
/// compile.  If the final closure is a function (an IO action waiting for
/// world), this injects unit and re-runs to trigger the IO yield.  Returns
/// `false` if the closure is not a function (it is a plain document value)
/// or if injection did not result in an IO yield.
pub fn inject_world_and_run(machine: &mut Machine<'_>) -> Result<bool, IoRunError> {
    let io_fn = machine.current_closure();

    // Only inject world if the closure is a function (arity > 0).
    // Plain data values (blocks, strings, numbers, etc.) are not IO
    // functions and should be rendered directly without world injection.
    if io_fn.arity() == 0 {
        return Ok(false);
    }

    let apply_c = machine
        .mutate(
            BuildApplyWorld {
                io_fn,
                root_env: machine.root_env(),
            },
            (),
        )
        .map_err(IoRunError::from)?;

    machine.resume_for_render(apply_c);
    machine.run(None).map_err(IoRunError::from)?;

    Ok(machine.io_yielded())
}

/// Render the result of a headless evaluation that did not yield an IO
/// constructor.
///
/// When compiled with `--allow-io` the machine runs in headless mode (no
/// RENDER_DOC wrapper), so a document that happens not to contain any IO
/// actions terminates normally without rendering.  This function explicitly
/// builds a `RENDER_DOC(value)` call and re-runs the machine to emit output.
///
/// Returns `Ok(exit_code)` on success; `Err` on machine error.
pub fn render_headless_result(machine: &mut Machine<'_>) -> Result<Option<u8>, IoRunError> {
    let value = machine.current_closure();
    let render_c = machine
        .mutate(
            BuildRenderDoc {
                value,
                root_env: machine.root_env(),
            },
            (),
        )
        .map_err(IoRunError::from)?;

    machine.resume_for_render(render_c);
    let exit_code = machine.run(None).map_err(IoRunError::from)?;

    Ok(exit_code)
}

/// Run the IO monad interpret loop and render the final pure value.
///
/// This is the main integration point called from `eval.rs`. It:
/// 1. Drives the io-run loop to extract the pure value from `IoReturn`
/// 2. Builds a `RENDER_DOC(value)` call and feeds it back into the machine
/// 3. The machine renders the value via its attached emitter
///
/// Returns `Ok(exit_code)` on success; `Err` on IO failure, missing
/// `--allow-io`, or machine error.
pub fn io_run_and_render(
    machine: &mut Machine<'_>,
    allow_io: bool,
) -> Result<Option<u8>, IoRunError> {
    // Extract the final pure value
    let final_value = io_run(machine, allow_io)?;

    // Build RENDER_DOC(value) and feed it back in
    let render_c = machine
        .mutate(
            BuildRenderDoc {
                value: final_value,
                root_env: machine.root_env(),
            },
            (),
        )
        .map_err(IoRunError::from)?;

    machine.resume_for_render(render_c);
    let exit_code = machine.run(None).map_err(IoRunError::from)?;

    Ok(exit_code)
}
