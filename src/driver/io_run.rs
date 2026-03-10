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
    stg::tags::DataConstructor,
};

// ─── Public error type ────────────────────────────────────────────────────────

/// Error from running the IO monad interpret loop
#[derive(Debug, thiserror::Error)]
pub enum IoRunError {
    #[error("io monad failure: {0}")]
    Fail(String),
    #[error("IO operations require the --allow-io (-I) flag")]
    IoNotAllowed,
    #[error("unknown IO action tag: {0}")]
    UnknownActionTag(String),
    #[error("malformed IO action spec block")]
    MalformedSpec,
    #[error("command timed out after {0} seconds")]
    Timeout(u64),
    #[error("command execution error: {0}")]
    CommandError(String),
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
/// `Ref::G` is not supported outside the machine and returns an error.
fn resolve_ref(
    view: &MutatorHeapView<'_>,
    parent: &SynClosure,
    r: Ref,
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
        Ref::G(i) => Err(ExecutionError::Panic(format!(
            "unexpected global ref G({i}) in io spec block"
        ))),
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
/// For cases that do not need the container env (list walking, `read_as_string`,
/// etc.), this avoids cluttering every call site with `let (c, _) = …`.
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
        _ => None,
    }
}

/// Walk a cons-list of `BlockPair` nodes, extracting string key-value pairs.
///
/// Symbol keys are resolved to their text via `pool`.
fn collect_block_fields(
    view: &MutatorHeapView<'_>,
    pool: &SymbolPool,
    list_closure: SynClosure,
) -> HashMap<String, String> {
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
                    collect_pair(view, pool, deref(view, head), &mut fields);
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

/// Read a single `BlockPair` node into the fields map.
///
/// Symbol keys are resolved to their text via `pool`.
fn collect_pair(
    view: &MutatorHeapView<'_>,
    pool: &SymbolPool,
    pair: SynClosure,
    fields: &mut HashMap<String, String>,
) {
    let code = view.scoped(pair.code());
    match &*code {
        HeapSyn::Cons { tag, args } if *tag == DataConstructor::BlockPair.tag() => {
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
                let derefed = deref(view, val_closure);
                if let Some(val_str) = read_as_string(view, derefed) {
                    fields.insert(key_name, val_str);
                }
            }
        }
        _ => {}
    }
}

/// Try to read a closure's WHNF value as a plain Rust `String`.
///
/// Handles: Atom(Str), Atom(Num), BoxedString, BoxedNumber, and
/// cons-lists of strings (stored NUL-separated under `__args_list`
/// semantics by the caller).
fn read_as_string(view: &MutatorHeapView<'_>, closure: SynClosure) -> Option<String> {
    let code = view.scoped(closure.code());
    match &*code {
        HeapSyn::Atom {
            evaluand: Ref::V(Native::Str(s)),
        } => Some(view.scoped(*s).as_str().to_string()),
        HeapSyn::Atom {
            evaluand: Ref::V(Native::Num(n)),
        } => Some(n.to_string()),
        HeapSyn::Cons { tag, args } if *tag == DataConstructor::BoxedString.tag() => {
            let r = args.get(0)?;
            match r {
                Ref::V(Native::Str(s)) => Some(view.scoped(s).as_str().to_string()),
                _ => None,
            }
        }
        HeapSyn::Cons { tag, args } if *tag == DataConstructor::BoxedNumber.tag() => {
            let r = args.get(0)?;
            match r {
                Ref::V(Native::Num(n)) => Some(n.to_string()),
                _ => None,
            }
        }
        HeapSyn::Cons { tag, .. }
            if *tag == DataConstructor::ListCons.tag()
                || *tag == DataConstructor::ListNil.tag() =>
        {
            // Collect list elements as NUL-separated string
            let mut parts: Vec<String> = Vec::new();
            let mut cur = closure.clone();
            loop {
                let c = view.scoped(cur.code());
                match &*c {
                    HeapSyn::Cons { tag: t, .. } if *t == DataConstructor::ListNil.tag() => break,
                    HeapSyn::Cons { tag: t, args } if *t == DataConstructor::ListCons.tag() => {
                        let hr = args.get(0)?;
                        let tr = args.get(1)?;
                        let head = resolve_ref(view, &cur, hr).ok()?;
                        let tail = resolve_ref(view, &cur, tr).ok()?;
                        if let Some(s) = read_as_string(view, deref(view, head)) {
                            parts.push(s);
                        }
                        cur = deref(view, tail);
                    }
                    _ => break,
                }
            }
            Some(parts.join("\x00"))
        }
        HeapSyn::Meta { body, .. } => {
            let body_closure = resolve_ref(view, &closure, body.clone()).ok()?;
            read_as_string(view, deref(view, body_closure))
        }
        _ => None,
    }
}

// ─── Mutator: read action spec from a yielded IoAction spec_block closure ────

/// Mutator that reads an `IoAction` spec block from the heap.
///
/// The spec_block closure is a `Block` potentially wrapped in `Meta(sym, body)`.
/// We strip metadata, walk the block's cons-list to extract `cmd`, `args`,
/// `stdin`, `timeout`, and read the metadata tag to distinguish `:io-shell`
/// from `:io-exec`.
struct ReadSpecBlock {
    spec_block: SynClosure,
}

impl Mutator for ReadSpecBlock {
    type Input = SymbolPool;
    type Output = ActionSpec;

    fn run(&self, view: &MutatorHeapView, pool: SymbolPool) -> Result<ActionSpec, ExecutionError> {
        let (meta_sym, body_closure) = peel_meta(view, &pool, self.spec_block.clone());

        // Extract the cons-list from the block
        let list_closure = block_list(view, body_closure.clone()).ok_or_else(|| {
            ExecutionError::Panic("IoAction spec block is not a Block constructor".to_string())
        })?;

        // Walk the list to collect fields
        let fields = collect_block_fields(view, &pool, list_closure);

        // Identify action type from metadata symbol
        let tag_name = meta_sym.ok_or_else(|| {
            ExecutionError::Panic("IO action spec block has no metadata tag".to_string())
        })?;

        let timeout_secs = fields
            .get("timeout")
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(30);

        let stdin = fields.get("stdin").filter(|s| !s.is_empty()).cloned();

        let is_shell =
            tag_name == "io-shell" || (fields.contains_key("cmd") && !fields.contains_key("args"));
        let is_exec =
            tag_name == "io-exec" || (fields.contains_key("cmd") && fields.contains_key("args"));

        if is_shell && !fields.contains_key("args") {
            let cmd = fields
                .get("cmd")
                .cloned()
                .ok_or_else(|| ExecutionError::Panic("io-shell spec missing 'cmd'".to_string()))?;
            Ok(ActionSpec::Shell {
                cmd,
                stdin,
                timeout_secs,
            })
        } else if is_exec {
            let cmd = fields
                .get("cmd")
                .cloned()
                .ok_or_else(|| ExecutionError::Panic("io-exec spec missing 'cmd'".to_string()))?;
            let args = fields
                .get("args")
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
        } else {
            Err(ExecutionError::Panic(format!(
                "unrecognised IO action tag: {tag_name}"
            )))
        }
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
            ExecutionError::Panic("RENDER_DOC intrinsic not found in registry".to_string())
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

/// Execute a shell command via `sh -c`.
fn execute_shell(
    cmd: &str,
    stdin_data: Option<&str>,
    timeout_secs: u64,
) -> Result<CommandResult, IoRunError> {
    let mut command = Command::new("sh");
    command.args(["-c", cmd]);
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

    let mut child = command
        .spawn()
        .map_err(|e| IoRunError::CommandError(e.to_string()))?;

    if let Some(input) = stdin_data {
        if let Some(mut pipe) = child.stdin.take() {
            pipe.write_all(input.as_bytes())
                .map_err(|e| IoRunError::CommandError(e.to_string()))?;
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
        Ok(Err(e)) => Err(IoRunError::CommandError(e.to_string())),
        Err(_timeout) => Err(IoRunError::Timeout(timeout_secs)),
    }
}

/// Dispatch an `ActionSpec` to the appropriate executor.
fn run_spec(spec: &ActionSpec) -> Result<CommandResult, IoRunError> {
    match spec {
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
    }
}

// ─── Error message extraction ─────────────────────────────────────────────────

/// Try to read an error string from an IoFail error closure.
fn extract_error_string(machine: &Machine<'_>, closure: &SynClosure) -> String {
    struct ExtractStr(SynClosure);
    impl Mutator for ExtractStr {
        type Input = ();
        type Output = Option<String>;
        fn run(&self, view: &MutatorHeapView, _: ()) -> Result<Option<String>, ExecutionError> {
            Ok(read_as_string(view, deref(view, self.0.clone())))
        }
    }
    machine
        .mutate(ExtractStr(closure.clone()), ())
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
                        "IoReturn missing value argument".to_string(),
                    )))
                });
            }

            Ok(DataConstructor::IoFail) => {
                let error_closure = args.into_iter().nth(1).ok_or_else(|| {
                    IoRunError::MachineError(Box::new(ExecutionError::Panic(
                        "IoFail missing error argument".to_string(),
                    )))
                })?;
                let msg = extract_error_string(machine, &error_closure);
                return Err(IoRunError::Fail(msg));
            }

            Ok(DataConstructor::IoAction) => {
                if !allow_io {
                    return Err(IoRunError::IoNotAllowed);
                }
                let world = args[0].clone();
                let spec_block = args[1].clone();

                // Stash world so it survives GC across the mutator calls below.
                machine.stash_push(world);

                // Read the action spec by static heap navigation.  The spec_block
                // is an unevaluated closure (Meta wrapping a LetRec block thunk).
                // `ReadSpecBlock` navigates the heap structure without machine
                // evaluation, using cycle-breaking in `dereference` to read
                // lambda-captured values from letrec bindings.
                let pool = machine.symbol_pool().clone();
                let spec = machine
                    .mutate(ReadSpecBlock { spec_block }, pool)
                    .map_err(IoRunError::from)?;

                // Execute the shell action
                let result = run_spec(&spec)?;

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
                // IMPORTANT: peek (not pop) so values remain GC-rooted
                // during BuildApply2 allocation which may trigger GC.
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

                // Now pop all three — mutator is done, values no longer needed
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
