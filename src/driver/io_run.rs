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
use crate::eval::{
    error::ExecutionError,
    intrinsics,
    machine::{env::SynClosure, env_builder::EnvBuilder, vm::Machine},
    memory::{
        alloc::ScopedAllocator,
        array::Array,
        mutator::{Mutator, MutatorHeapView},
        syntax::{HeapSyn, LambdaForm, Native, Ref, RefPtr, StgBuilder},
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
fn dereference(view: &MutatorHeapView<'_>, mut closure: SynClosure) -> SynClosure {
    loop {
        let code = view.scoped(closure.code());
        match &*code {
            HeapSyn::Atom {
                evaluand: Ref::L(i),
            } => {
                let env = view.scoped(closure.env());
                match env.get(view, *i) {
                    Some(inner) => closure = inner,
                    None => break,
                }
            }
            _ => break,
        }
    }
    closure
}

/// Strip a single `Meta` wrapper from a closure, returning the metadata
/// symbol name (if readable) and the body closure.
///
/// Returns `(None, closure)` if there is no metadata wrapper.
fn peel_meta(view: &MutatorHeapView<'_>, closure: SynClosure) -> (Option<String>, SynClosure) {
    let derefed = dereference(view, closure);
    let code = view.scoped(derefed.code());
    match &*code {
        HeapSyn::Meta { meta, body } => {
            // Read meta ref as a symbol name
            let sym_name = match meta {
                Ref::V(Native::Sym(id)) => Some(id.to_string()),
                Ref::L(i) => {
                    let env = view.scoped(derefed.env());
                    env.get(view, *i).and_then(|meta_c| {
                        let mc = dereference(view, meta_c);
                        let mc_code = view.scoped(mc.code());
                        match &*mc_code {
                            HeapSyn::Atom {
                                evaluand: Ref::V(Native::Sym(id)),
                            } => Some(id.to_string()),
                            HeapSyn::Cons { tag, args }
                                if *tag == DataConstructor::BoxedSymbol.tag() =>
                            {
                                let r = args.get(0)?;
                                match r {
                                    Ref::V(Native::Sym(id)) => Some(id.to_string()),
                                    _ => None,
                                }
                            }
                            _ => None,
                        }
                    })
                }
                _ => None,
            };

            // Resolve body Ref to a closure
            let body_closure = match body {
                Ref::L(i) => {
                    let env = view.scoped(derefed.env());
                    env.get(view, *i).unwrap_or_else(|| derefed.clone())
                }
                Ref::V(_) | Ref::G(_) => derefed.clone(),
            };

            (sym_name, body_closure)
        }
        _ => (None, derefed),
    }
}

/// Extract the cons-list closure from a `Block` constructor.
fn block_list(view: &MutatorHeapView<'_>, closure: SynClosure) -> Option<SynClosure> {
    let c = dereference(view, closure);
    let code = view.scoped(c.code());
    match &*code {
        HeapSyn::Cons { tag, args } if *tag == DataConstructor::Block.tag() => {
            let list_ref = args.get(0)?;
            resolve_ref(view, &c, list_ref).ok()
        }
        _ => None,
    }
}

/// Walk a cons-list of `BlockPair` nodes, extracting string key-value pairs.
fn collect_block_fields(
    view: &MutatorHeapView<'_>,
    list_closure: SynClosure,
) -> HashMap<String, String> {
    let mut fields = HashMap::new();
    let mut current = dereference(view, list_closure);

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
                    collect_pair(view, dereference(view, head), &mut fields);
                }
                match resolve_ref(view, &current, tail_ref) {
                    Ok(tail) => current = dereference(view, tail),
                    Err(_) => break,
                }
            }
            _ => break,
        }
    }

    fields
}

/// Read a single `BlockPair` node into the fields map.
fn collect_pair(
    view: &MutatorHeapView<'_>,
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
                Ref::V(Native::Sym(id)) => id.to_string(),
                _ => return,
            };

            if let Ok(val_closure) = resolve_ref(view, &pair, val_ref) {
                if let Some(val_str) = read_as_string(view, dereference(view, val_closure)) {
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
                        if let Some(s) = read_as_string(view, dereference(view, head)) {
                            parts.push(s);
                        }
                        cur = dereference(view, tail);
                    }
                    _ => break,
                }
            }
            Some(parts.join("\x00"))
        }
        HeapSyn::Meta { body, .. } => {
            let body_closure = resolve_ref(view, &closure, body.clone()).ok()?;
            read_as_string(view, dereference(view, body_closure))
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
    type Input = ();
    type Output = ActionSpec;

    fn run(&self, view: &MutatorHeapView, _: ()) -> Result<ActionSpec, ExecutionError> {
        // Strip metadata to get (tag_sym_id_str, body_block_closure)
        let (meta_sym, body_closure) = peel_meta(view, self.spec_block.clone());

        // Extract the cons-list from the block
        let list_closure = block_list(view, body_closure).ok_or_else(|| {
            ExecutionError::Panic("IoAction spec block is not a Block constructor".to_string())
        })?;

        // Walk the list to collect fields
        let fields = collect_block_fields(view, list_closure);

        // Identify action type from metadata symbol
        let tag_name = meta_sym.ok_or_else(|| {
            ExecutionError::Panic("IO action spec block has no metadata tag".to_string())
        })?;

        let timeout_secs = fields
            .get("timeout")
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(30);

        let stdin = fields.get("stdin").filter(|s| !s.is_empty()).cloned();

        // The symbol pool uses interned IDs as strings, so we cannot directly
        // compare "io-shell". Instead we rely on the SymbolId Display being the
        // interned text — this requires that the prelude interned the symbol
        // into the machine's pool before the spec block was constructed.
        //
        // For robustness, we match on both the raw ID string (as a fallback)
        // and the expected hyphenated names.
        let is_shell =
            tag_name == "io-shell" || fields.contains_key("cmd") && !fields.contains_key("args");
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
    type Input = ();
    type Output = SynClosure;

    fn run(&self, view: &MutatorHeapView, _: ()) -> Result<SynClosure, ExecutionError> {
        let mut pool = crate::eval::memory::symbol::SymbolPool::new();

        // Allocate the three value atoms
        let stdout_ref = view.str_ref(self.stdout.as_str())?;
        let stderr_ref = view.str_ref(self.stderr.as_str())?;
        let exit_code_ref = Ref::V(Native::Num(serde_json::Number::from(self.exit_code)));

        let stdout_atom = view
            .alloc(HeapSyn::Atom {
                evaluand: stdout_ref,
            })?
            .as_ptr();
        let stderr_atom = view
            .alloc(HeapSyn::Atom {
                evaluand: stderr_ref,
            })?
            .as_ptr();
        let exit_code_atom = view
            .alloc(HeapSyn::Atom {
                evaluand: exit_code_ref,
            })?
            .as_ptr();

        let stdout_c = SynClosure::new(stdout_atom, self.root_env);
        let stderr_c = SynClosure::new(stderr_atom, self.root_env);
        let exit_code_c = SynClosure::new(exit_code_atom, self.root_env);

        // Frame for values: [stdout=0, stderr=1, exit_code=2]
        let value_frame = view.from_closures(
            [stdout_c, stderr_c, exit_code_c].iter().cloned(),
            3,
            self.root_env,
            Smid::default(),
        )?;

        // Build BlockPair nodes in value_frame
        let stdout_sym = view.sym_ref(&mut pool, "stdout")?;
        let stderr_sym = view.sym_ref(&mut pool, "stderr")?;
        let exitcode_sym = view.sym_ref(&mut pool, "exit-code")?;

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

        // Build a frame of pairs: [pair0=0, pair1=1, pair2=2]
        let pair0_c = SynClosure::new(pair0_ptr, value_frame);
        let pair1_c = SynClosure::new(pair1_ptr, value_frame);
        let pair2_c = SynClosure::new(pair2_ptr, value_frame);
        let pair_frame = view.from_closures(
            [pair0_c, pair1_c, pair2_c].iter().cloned(),
            3,
            value_frame,
            Smid::default(),
        )?;

        // Build list via letrec over pair_frame (4 slots: nil, c2, c1, c0).
        //
        // pair_frame indices: pair0=0, pair1=1, pair2=2
        //
        // letrec frame (4 bindings) over pair_frame:
        //   letrec[0] = ListNil
        //   letrec[1] = ListCons(L(2+3), L(0))    pair2 = L(2+3=5)? No:
        //
        // In a letrec of 4 bindings over pair_frame (3 slots):
        //   L(0..3) = letrec bindings (self-referential)
        //   L(4..6) = pair_frame slots (pair0=L(4), pair1=L(5), pair2=L(6))
        //
        //   letrec[0] = ListNil
        //   letrec[1] = ListCons(L(6), L(0))   — pair2, nil
        //   letrec[2] = ListCons(L(5), L(1))   — pair1, c2
        //   letrec[3] = ListCons(L(4), L(2))   — pair0, c1
        // body = L(3)  — the outermost cons = c0

        let nil_syn = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::ListNil.tag(),
                args: Array::default(),
            })?
            .as_ptr();
        let c2_syn = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::ListCons.tag(),
                args: Array::from_slice(view, &[Ref::L(6), Ref::L(0)]),
            })?
            .as_ptr();
        let c1_syn = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::ListCons.tag(),
                args: Array::from_slice(view, &[Ref::L(5), Ref::L(1)]),
            })?
            .as_ptr();
        let c0_syn = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::ListCons.tag(),
                args: Array::from_slice(view, &[Ref::L(4), Ref::L(2)]),
            })?
            .as_ptr();

        let letrec_bindings = Array::from_slice(
            view,
            &[
                LambdaForm::value(nil_syn),
                LambdaForm::value(c2_syn),
                LambdaForm::value(c1_syn),
                LambdaForm::value(c0_syn),
            ],
        );
        let body_atom = view
            .alloc(HeapSyn::Atom {
                evaluand: Ref::L(3),
            })?
            .as_ptr();
        let list_letrec = view
            .alloc(HeapSyn::LetRec {
                bindings: letrec_bindings,
                body: body_atom,
            })?
            .as_ptr();

        let list_closure = SynClosure::new(list_letrec, pair_frame);

        // Frame holding the list: [list=0]
        let list_frame = view.from_closure(list_closure, self.root_env, Smid::default())?;

        // Block { list=L(0), no_index }
        let no_index = Ref::V(Native::Num(serde_json::Number::from(0)));
        let block_syn = view
            .alloc(HeapSyn::Cons {
                tag: DataConstructor::Block.tag(),
                args: Array::from_slice(view, &[Ref::L(0), no_index]),
            })?
            .as_ptr();

        Ok(SynClosure::new(block_syn, list_frame))
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

// ─── Mutator: apply continuation to result ───────────────────────────────────

struct ApplyCont {
    cont: SynClosure,
    result: SynClosure,
    root_env: RefPtr<EnvFrame>,
}

impl Mutator for ApplyCont {
    type Input = ();
    type Output = SynClosure;

    fn run(&self, view: &MutatorHeapView, _: ()) -> Result<SynClosure, ExecutionError> {
        // Frame: [cont=0, result=1]
        let env = view.from_closures(
            [self.cont.clone(), self.result.clone()].iter().cloned(),
            2,
            self.root_env,
            Smid::default(),
        )?;
        // App { callable: L(0), args: [L(1)] }
        let app_syn = view
            .alloc(HeapSyn::App {
                callable: Ref::L(0),
                args: Array::from_slice(view, &[Ref::L(1)]),
            })?
            .as_ptr();
        Ok(SynClosure::new(app_syn, env))
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
            Ok(read_as_string(view, dereference(view, self.0.clone())))
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

                // Read the spec from the heap
                let spec = machine
                    .mutate(ReadSpecBlock { spec_block }, ())
                    .map_err(IoRunError::from)?;

                // Execute the shell action
                let result = run_spec(&spec)?;

                // Build the result block closure
                let result_c = machine
                    .mutate(
                        BuildResultBlock {
                            stdout: result.stdout,
                            stderr: result.stderr,
                            exit_code: result.exit_code,
                            root_env: machine.root_env(),
                        },
                        (),
                    )
                    .map_err(IoRunError::from)?;

                // Wrap in IoReturn(world, result_block) and resume
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

                machine.resume(io_return_c);
                machine.run(None).map_err(IoRunError::from)?;
                // Loop back to inspect the new yield
            }

            Ok(DataConstructor::IoBind) => {
                // IoBind(world=0, action=1, continuation=2)
                let action = args[1].clone();
                let cont = args[2].clone();

                // Step 1: evaluate the action
                machine.resume(action);
                machine.run(None).map_err(IoRunError::from)?;

                if !machine.io_yielded() {
                    return Err(IoRunError::MachineError(Box::new(ExecutionError::Panic(
                        "IoBind action did not yield an IO constructor".to_string(),
                    ))));
                }

                // Step 2: recursively process the action result
                let action_result = io_run(machine, allow_io)?;

                // Step 3: apply continuation to result
                let apply_c = machine
                    .mutate(
                        ApplyCont {
                            cont,
                            result: action_result,
                            root_env: machine.root_env(),
                        },
                        (),
                    )
                    .map_err(IoRunError::from)?;

                machine.resume_for_render(apply_c);
                machine.run(None).map_err(IoRunError::from)?;
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
