//! IO monad interpret loop for eucalypt
//!
//! When the STG machine evaluates to an IO constructor (IoReturn, IoBind,
//! IoAction, IoFail) it yields rather than terminating.  This module
//! implements the driver loop that:
//!
//! 1. Inspects the yielded IO constructor
//! 2. Executes shell actions (`:io-shell`, `:io-exec`)
//! 3. Injects results back into the machine as STG blocks
//! 4. Applies bind continuations and resumes
//! 5. Renders the final `IoReturn` value using the normal render pipeline
//!
//! Entry point: `io_run(machine, opt)`.

use std::{
    io::Write as _,
    process::{Child, Command, Stdio},
    sync::mpsc,
    thread,
    time::Duration,
};

use serde_json::Number;

use crate::{
    common::sourcemap::Smid,
    eval::{
        error::ExecutionError,
        machine::{
            env::{EnvFrame, SynClosure},
            env_builder::EnvBuilder,
            vm::Machine,
        },
        memory::{
            alloc::ScopedAllocator,
            mutator::{Mutator, MutatorHeapView},
            syntax::{HeapSyn, LambdaForm, Native, Ref, RefPtr, StgBuilder},
        },
        stg::tags::DataConstructor,
    },
};

use super::options::EucalyptOptions;

// ─── Public error type ────────────────────────────────────────────────────────

/// Errors that can occur in the io-run loop
#[derive(Debug)]
pub enum IoRunError {
    IoFail(String),
    UnknownActionTag(String),
    Timeout(String),
    NotAllowed,
    Execution(Box<ExecutionError>),
    SpecBlockError(String),
}

impl std::fmt::Display for IoRunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IoRunError::IoFail(msg) => write!(f, "IO action failed: {msg}"),
            IoRunError::UnknownActionTag(tag) => write!(f, "unknown IO action tag: {tag}"),
            IoRunError::Timeout(cmd) => write!(f, "IO action timed out: {cmd}"),
            IoRunError::NotAllowed => {
                write!(f, "IO operations require the --allow-io (-I) flag")
            }
            IoRunError::Execution(e) => write!(f, "{e}"),
            IoRunError::SpecBlockError(msg) => write!(f, "invalid IO action spec block: {msg}"),
        }
    }
}

impl From<ExecutionError> for IoRunError {
    fn from(e: ExecutionError) -> Self {
        IoRunError::Execution(Box::new(e))
    }
}

// ─── Shell execution ─────────────────────────────────────────────────────────

struct ShellResult {
    stdout: String,
    stderr: String,
    exit_code: i32,
}

/// Spawn a child process, write stdin if provided, and wait with a timeout.
///
/// The timeout is implemented using a background thread that sends a signal on
/// a channel after `timeout_secs`.  The main thread uses `recv_timeout` on the
/// same channel to decide whether the child should be killed.
fn wait_with_timeout(
    mut child: Child,
    stdin_data: Option<&str>,
    timeout_secs: u64,
    label: &str,
) -> Result<ShellResult, IoRunError> {
    if let Some(input) = stdin_data {
        let mut stdin = child.stdin.take().expect("stdin must be piped");
        stdin
            .write_all(input.as_bytes())
            .map_err(|e| IoRunError::SpecBlockError(format!("stdin write failed: {e}")))?;
        drop(stdin); // signal EOF to child
    }

    // Spawn a thread to wait_with_output; use a channel with a timeout.
    let (tx, rx) = mpsc::channel::<Result<std::process::Output, std::io::Error>>();

    thread::spawn(move || {
        let result = child.wait_with_output();
        let _ = tx.send(result);
    });

    match rx.recv_timeout(Duration::from_secs(timeout_secs)) {
        Ok(Ok(output)) => {
            let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
            let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
            let exit_code = output.status.code().unwrap_or(-1);
            Ok(ShellResult {
                stdout,
                stderr,
                exit_code,
            })
        }
        Ok(Err(e)) => Err(IoRunError::SpecBlockError(format!("wait failed: {e}"))),
        Err(mpsc::RecvTimeoutError::Timeout) => Err(IoRunError::Timeout(label.to_string())),
        Err(mpsc::RecvTimeoutError::Disconnected) => Err(IoRunError::SpecBlockError(
            "child thread disconnected".to_string(),
        )),
    }
}

/// Execute a shell command via `/bin/sh -c` with an optional timeout.
fn execute_shell(
    cmd: &str,
    stdin_data: Option<&str>,
    timeout_secs: u64,
) -> Result<ShellResult, IoRunError> {
    let mut builder = Command::new("sh");
    builder.args(["-c", cmd]);
    if stdin_data.is_some() {
        builder.stdin(Stdio::piped());
    } else {
        builder.stdin(Stdio::null());
    }
    builder.stdout(Stdio::piped());
    builder.stderr(Stdio::piped());

    let child = builder
        .spawn()
        .map_err(|e| IoRunError::SpecBlockError(format!("failed to spawn shell: {e}")))?;

    wait_with_timeout(child, stdin_data, timeout_secs, cmd)
}

/// Execute a command directly (no shell) with an optional timeout.
fn execute_exec(
    cmd: &str,
    args: &[String],
    stdin_data: Option<&str>,
    timeout_secs: u64,
) -> Result<ShellResult, IoRunError> {
    let mut builder = Command::new(cmd);
    builder.args(args);
    if stdin_data.is_some() {
        builder.stdin(Stdio::piped());
    } else {
        builder.stdin(Stdio::null());
    }
    builder.stdout(Stdio::piped());
    builder.stderr(Stdio::piped());

    let child = builder
        .spawn()
        .map_err(|e| IoRunError::SpecBlockError(format!("failed to exec {cmd}: {e}")))?;

    wait_with_timeout(child, stdin_data, timeout_secs, cmd)
}

// ─── Spec block inspection ────────────────────────────────────────────────────

/// Parsed contents of an IoAction spec block
struct ActionSpec {
    tag: String,
    cmd: String,
    stdin: Option<String>,
    timeout_secs: u64,
    args: Vec<String>,
}

/// Input passed to the `SpecInspector` mutator
struct SpecInspectorInput {
    /// The forced (WHNF) spec block closure
    closure: SynClosure,
    /// Symbol pool for resolving sym IDs to strings
    sym_ids: Vec<String>,
}

/// A mutator that inspects a forced spec block and extracts `ActionSpec`.
///
/// Expects the closure's code to be:
///   `HeapSyn::Meta { meta: Sym(id), body: Ref → Block }`
struct SpecInspector;

impl Mutator for SpecInspector {
    type Input = SpecInspectorInput;
    type Output = ActionSpec;

    fn run(
        &self,
        view: &MutatorHeapView,
        input: SpecInspectorInput,
    ) -> Result<ActionSpec, ExecutionError> {
        let code = view.scoped(input.closure.code());
        let env = view.scoped(input.closure.env());

        let (meta_sym_id, block_closure) = match &*code {
            HeapSyn::Meta { meta, body } => {
                let sym_id = resolve_ref_to_sym_id(view, meta.clone(), &env)?;
                let block_cl = resolve_ref_to_closure(view, body.clone(), &env)?;
                (sym_id, block_cl)
            }
            other => {
                return Err(ExecutionError::NotValue(
                    Smid::default(),
                    format!("spec block must be Meta, got {}", syn_name(other)),
                ))
            }
        };

        let tag = input
            .sym_ids
            .get(meta_sym_id as usize)
            .cloned()
            .unwrap_or_else(|| format!("sym#{meta_sym_id}"));

        let pairs = collect_block_pairs(view, block_closure, &input.sym_ids)?;

        let mut cmd: Option<String> = None;
        let mut stdin: Option<String> = None;
        let mut timeout_secs: u64 = 30;
        let mut exec_args: Vec<String> = Vec::new();

        for (key, value) in pairs {
            match key.as_str() {
                "cmd" => {
                    if let FieldValue::Str(s) = value {
                        cmd = Some(s);
                    }
                }
                "stdin" => {
                    if let FieldValue::Str(s) = value {
                        stdin = Some(s);
                    }
                }
                "timeout" => {
                    if let FieldValue::Num(n) = value {
                        if let Some(u) = n.as_u64() {
                            timeout_secs = u;
                        } else if let Some(f) = n.as_f64() {
                            timeout_secs = f as u64;
                        }
                    }
                }
                "args" => {
                    if let FieldValue::StrList(v) = value {
                        exec_args = v;
                    }
                }
                _ => {}
            }
        }

        let cmd = cmd.ok_or_else(|| {
            ExecutionError::NotValue(
                Smid::default(),
                "spec block missing 'cmd' field".to_string(),
            )
        })?;

        Ok(ActionSpec {
            tag,
            cmd,
            stdin,
            timeout_secs,
            args: exec_args,
        })
    }
}

/// A field value extracted from a spec block
enum FieldValue {
    Str(String),
    Num(serde_json::Number),
    StrList(Vec<String>),
}

/// Collect key-value pairs from a Block closure.
///
/// Returns a `Vec<(String, FieldValue)>` using sym_ids to resolve keys.
fn collect_block_pairs(
    view: &MutatorHeapView,
    block_closure: SynClosure,
    sym_ids: &[String],
) -> Result<Vec<(String, FieldValue)>, ExecutionError> {
    let code = view.scoped(block_closure.code());
    let env = view.scoped(block_closure.env());

    let list_closure = match &*code {
        HeapSyn::Cons { tag, args } if *tag == DataConstructor::Block.tag() => {
            // args[0] = list ref
            let r = args.get(0).ok_or_else(|| {
                ExecutionError::NotValue(Smid::default(), "Block missing list".to_string())
            })?;
            resolve_ref_to_closure(view, r, &env)?
        }
        other => {
            return Err(ExecutionError::NotValue(
                Smid::default(),
                format!("expected Block cons, got {}", syn_name(other)),
            ))
        }
    };

    let mut result = Vec::new();
    walk_list(
        view,
        list_closure,
        &mut |pair_closure| -> Result<(), ExecutionError> {
            let pair_code = view.scoped(pair_closure.code());
            let pair_env = view.scoped(pair_closure.env());

            if let HeapSyn::Cons { tag, args } = &*pair_code {
                if *tag == DataConstructor::BlockPair.tag() {
                    let key_r = match args.get(0) {
                        Some(r) => r,
                        None => return Ok(()),
                    };
                    let val_r = match args.get(1) {
                        Some(r) => r,
                        None => return Ok(()),
                    };

                    let key_sym_id = resolve_ref_to_sym_id(view, key_r, &pair_env)?;
                    let key = sym_ids
                        .get(key_sym_id as usize)
                        .cloned()
                        .unwrap_or_else(|| format!("sym#{key_sym_id}"));

                    let val_closure = resolve_ref_to_closure(view, val_r, &pair_env)?;
                    let field_value = match coerce_closure_to_field_value(view, val_closure.clone())
                    {
                        Ok(v) => v,
                        Err(_) => {
                            // May be a list (for the `args` field in `:io-exec`)
                            match collect_str_list(view, val_closure) {
                                Ok(list) => FieldValue::StrList(list),
                                Err(_) => return Ok(()), // skip unresolvable values
                            }
                        }
                    };

                    result.push((key, field_value));
                }
            }
            Ok(())
        },
    )?;

    Ok(result)
}

/// Coerce a closure to a primitive FieldValue (Str or Num)
fn coerce_closure_to_field_value(
    view: &MutatorHeapView,
    closure: SynClosure,
) -> Result<FieldValue, ExecutionError> {
    match resolve_closure_to_native(view, closure)? {
        Native::Str(ptr) => {
            let hs = view.scoped(ptr);
            Ok(FieldValue::Str(hs.as_str().to_string()))
        }
        Native::Num(n) => Ok(FieldValue::Num(n)),
        _ => Err(ExecutionError::NotValue(
            Smid::default(),
            "not a str or num".to_string(),
        )),
    }
}

/// Walk a cons-list, calling `f` for each head closure.
fn walk_list(
    view: &MutatorHeapView,
    list_closure: SynClosure,
    f: &mut dyn FnMut(SynClosure) -> Result<(), ExecutionError>,
) -> Result<(), ExecutionError> {
    let mut current = list_closure;
    loop {
        let code = view.scoped(current.code());
        let env = view.scoped(current.env());
        match &*code {
            HeapSyn::Cons { tag, .. } if *tag == DataConstructor::ListNil.tag() => return Ok(()),
            HeapSyn::Cons { tag, args } if *tag == DataConstructor::ListCons.tag() => {
                let head_r = args.get(0).ok_or_else(|| {
                    ExecutionError::NotValue(Smid::default(), "ListCons missing head".to_string())
                })?;
                let tail_r = args.get(1).ok_or_else(|| {
                    ExecutionError::NotValue(Smid::default(), "ListCons missing tail".to_string())
                })?;
                let head = resolve_ref_to_closure(view, head_r, &env)?;
                let tail = resolve_ref_to_closure(view, tail_r, &env)?;
                f(head)?;
                current = tail;
            }
            HeapSyn::Atom { evaluand: r } => {
                // Follow atom chain
                current = resolve_ref_to_closure(view, r.clone(), &env)?;
            }
            _ => return Ok(()), // End of list or unexpected structure
        }
    }
}

/// Collect a cons-list of strings
fn collect_str_list(
    view: &MutatorHeapView,
    list_closure: SynClosure,
) -> Result<Vec<String>, ExecutionError> {
    let mut result = Vec::new();
    walk_list(view, list_closure, &mut |c| {
        if let Native::Str(ptr) = resolve_closure_to_native(view, c)? {
            let hs = view.scoped(ptr);
            result.push(hs.as_str().to_string());
        }
        Ok(())
    })?;
    Ok(result)
}

/// Resolve a `Ref` relative to an `EnvFrame`, giving back a `SynClosure`.
///
/// `Ref::V` refs are wrapped in an atom closure with an empty env frame.
fn resolve_ref_to_closure(
    view: &MutatorHeapView,
    r: Ref,
    env: &EnvFrame,
) -> Result<SynClosure, ExecutionError> {
    match r {
        Ref::V(_) => {
            let atom = view.alloc(HeapSyn::Atom { evaluand: r })?.as_ptr();
            let empty = view.alloc(EnvFrame::default())?.as_ptr();
            Ok(SynClosure::new(atom, empty))
        }
        Ref::L(i) => env
            .get(view, i)
            .ok_or(ExecutionError::BadEnvironmentIndex(i)),
        Ref::G(_) => Err(ExecutionError::NotValue(
            Smid::default(),
            "global ref in spec block inspection".to_string(),
        )),
    }
}

/// Resolve a `Ref` to a `SymbolId` (as u32).
fn resolve_ref_to_sym_id(
    view: &MutatorHeapView,
    r: Ref,
    env: &EnvFrame,
) -> Result<u32, ExecutionError> {
    let native = match r {
        Ref::V(n) => n,
        Ref::L(i) => {
            let c = env
                .get(view, i)
                .ok_or(ExecutionError::BadEnvironmentIndex(i))?;
            resolve_closure_to_native(view, c)?
        }
        Ref::G(_) => {
            return Err(ExecutionError::NotValue(
                Smid::default(),
                "global ref in sym resolution".to_string(),
            ))
        }
    };
    match native {
        Native::Sym(id) => Ok(id.as_u32()),
        _ => Err(ExecutionError::NotValue(
            Smid::default(),
            "expected symbol, got non-symbol native".to_string(),
        )),
    }
}

/// Walk atom and boxed-value chains to reach a `Native` leaf.
fn resolve_closure_to_native(
    view: &MutatorHeapView,
    mut closure: SynClosure,
) -> Result<Native, ExecutionError> {
    loop {
        let code = view.scoped(closure.code());
        let env = view.scoped(closure.env());
        match &*code {
            HeapSyn::Atom {
                evaluand: Ref::V(n),
            } => return Ok(n.clone()),
            HeapSyn::Atom { evaluand: r } => {
                closure = resolve_ref_to_closure(view, r.clone(), &env)?;
            }
            HeapSyn::Cons { tag, args } => {
                // Transparent unboxing
                let inner_r = args.get(0).ok_or_else(|| {
                    ExecutionError::NotValue(Smid::default(), "boxed cons empty".to_string())
                })?;
                match &inner_r {
                    Ref::V(Native::Str(ptr)) if *tag == DataConstructor::BoxedString.tag() => {
                        return Ok(Native::Str(*ptr));
                    }
                    Ref::V(Native::Sym(id)) if *tag == DataConstructor::BoxedSymbol.tag() => {
                        return Ok(Native::Sym(*id));
                    }
                    Ref::V(Native::Num(n)) if *tag == DataConstructor::BoxedNumber.tag() => {
                        return Ok(Native::Num(n.clone()));
                    }
                    _ => closure = resolve_ref_to_closure(view, inner_r, &env)?,
                }
            }
            other => {
                return Err(ExecutionError::NotValue(
                    Smid::default(),
                    format!("cannot resolve {} to native", syn_name(other)),
                ))
            }
        }
    }
}

fn syn_name(syn: &HeapSyn) -> &'static str {
    match syn {
        HeapSyn::Atom { .. } => "Atom",
        HeapSyn::App { .. } => "App",
        HeapSyn::Bif { .. } => "Bif",
        HeapSyn::Cons { .. } => "Cons",
        HeapSyn::Let { .. } => "Let",
        HeapSyn::LetRec { .. } => "LetRec",
        HeapSyn::Case { .. } => "Case",
        HeapSyn::Meta { .. } => "Meta",
        HeapSyn::DeMeta { .. } => "DeMeta",
        HeapSyn::Ann { .. } => "Ann",
        HeapSyn::BlackHole => "BlackHole",
    }
}

// ─── Result block injection ───────────────────────────────────────────────────

/// Input for `ResultBlockBuilder`
struct ResultBlockInput {
    stdout: String,
    stderr: String,
    exit_code: i32,
    /// Root env to use as parent of the constructed env frames
    root_env: RefPtr<EnvFrame>,
    /// Pre-allocated sym IDs for the field keys (stdout, stderr, exit-code)
    stdout_sym: u32,
    stderr_sym: u32,
    exit_code_sym: u32,
}

/// A mutator that builds `{stdout: Str, stderr: Str, exit-code: Num}` on the
/// heap and returns a `SynClosure` pointing to the block.
struct ResultBlockBuilder;

impl Mutator for ResultBlockBuilder {
    type Input = ResultBlockInput;
    type Output = SynClosure;

    fn run(
        &self,
        view: &MutatorHeapView,
        input: ResultBlockInput,
    ) -> Result<SynClosure, ExecutionError> {
        use crate::eval::memory::symbol::SymbolId;

        // Helper: intern a sym id back into a Ref::V(Native::Sym(...))
        // We already have the sym IDs pre-interned by the caller.
        fn sym_ref_from_id(id: u32) -> Ref {
            // Reconstruct SymbolId — it's repr(transparent) over u32
            // SAFETY: SymbolId is #[repr(transparent)] over u32
            let sym_id: SymbolId = unsafe { std::mem::transmute(id) };
            Ref::V(Native::Sym(sym_id))
        }

        let stdout_key = sym_ref_from_id(input.stdout_sym);
        let stderr_key = sym_ref_from_id(input.stderr_sym);
        let exit_key = sym_ref_from_id(input.exit_code_sym);

        // Allocate string values on heap
        let stdout_val = view.str_ref(input.stdout.as_str())?;
        let stderr_val = view.str_ref(input.stderr.as_str())?;
        let exit_val = Ref::V(Native::Num(Number::from(input.exit_code)));

        // Build the structure as a letrec:
        // bindings[0] = ListNil
        // bindings[1] = BlockPair(exit-code, exit_val)  -- Ref::V(exit_val) inline
        // bindings[2] = ListCons(L(1), L(0))            -- [exit_pair]
        // bindings[3] = BlockPair(stderr, stderr_val)
        // bindings[4] = ListCons(L(3), L(2))            -- [stderr_pair, exit_pair]
        // bindings[5] = BlockPair(stdout, stdout_val)
        // bindings[6] = ListCons(L(5), L(4))            -- [stdout, stderr, exit]
        // bindings[7] = Block(L(6), 0)
        // body = Atom(L(7))

        let nil = view.nil()?;
        let exit_pair = view.data(
            DataConstructor::BlockPair.tag(),
            view.array(&[exit_key, exit_val]),
        )?;
        let list1 = view.data(
            DataConstructor::ListCons.tag(),
            view.array(&[Ref::L(1), Ref::L(0)]),
        )?;
        let stderr_pair = view.data(
            DataConstructor::BlockPair.tag(),
            view.array(&[stderr_key, stderr_val]),
        )?;
        let list2 = view.data(
            DataConstructor::ListCons.tag(),
            view.array(&[Ref::L(3), Ref::L(2)]),
        )?;
        let stdout_pair = view.data(
            DataConstructor::BlockPair.tag(),
            view.array(&[stdout_key, stdout_val]),
        )?;
        let list3 = view.data(
            DataConstructor::ListCons.tag(),
            view.array(&[Ref::L(5), Ref::L(4)]),
        )?;
        let no_index = Ref::V(Native::Num(Number::from(0)));
        let block = view.data(
            DataConstructor::Block.tag(),
            view.array(&[Ref::L(6), no_index]),
        )?;
        let body = view.atom(Ref::L(7))?;

        let bindings = view.array(&[
            LambdaForm::value(nil.as_ptr()),
            LambdaForm::value(exit_pair.as_ptr()),
            LambdaForm::value(list1.as_ptr()),
            LambdaForm::value(stderr_pair.as_ptr()),
            LambdaForm::value(list2.as_ptr()),
            LambdaForm::value(stdout_pair.as_ptr()),
            LambdaForm::value(list3.as_ptr()),
            LambdaForm::value(block.as_ptr()),
        ]);

        let letrec_node = view.letrec(bindings, body)?;
        Ok(SynClosure::new(letrec_node.as_ptr(), input.root_env))
    }
}

// ─── IoReturn value rendering ─────────────────────────────────────────────────

/// Render the IoReturn value by resuming the machine with a RENDER call on
/// `value_closure`.
///
/// RENDER is a global intrinsic.  We build an `App { callable: G(render_idx),
/// args: [value_ref] }` closure and driver_resume the machine with it, then
/// run to termination.  The emitter captures output normally.
fn resume_with_render(machine: &mut Machine, value_closure: SynClosure) -> Result<(), IoRunError> {
    let render_global_idx = crate::eval::intrinsics::index("RENDER")
        .ok_or_else(|| IoRunError::SpecBlockError("RENDER intrinsic not found".to_string()))?;

    let render_closure = machine.mutate(
        BuildRenderCall,
        RenderCallInput {
            value_closure,
            render_global_idx,
        },
    )?;

    machine.driver_resume(render_closure);
    machine.run(None)?;
    Ok(())
}

struct RenderCallInput {
    value_closure: SynClosure,
    render_global_idx: usize,
}

struct BuildRenderCall;

impl Mutator for BuildRenderCall {
    type Input = RenderCallInput;
    type Output = SynClosure;

    fn run(
        &self,
        view: &MutatorHeapView,
        input: RenderCallInput,
    ) -> Result<SynClosure, ExecutionError> {
        // Build environment with value_closure at L(0)
        let empty = view.alloc(EnvFrame::default())?.as_ptr();
        let value_env = view.from_closure(input.value_closure, empty, Smid::default())?;

        // App { callable: G(render_global_idx), args: [L(0)] }
        let args = view.array(&[Ref::L(0)]);
        let app = view.app(Ref::G(input.render_global_idx), args)?;

        Ok(SynClosure::new(app.as_ptr(), value_env))
    }
}

// ─── IoBind continuation application ─────────────────────────────────────────

/// Apply a bind continuation to a result closure.
///
/// The continuation is a 1-argument function (arity 1).  We build a saturated
/// closure and resume the machine with it.
fn apply_continuation(
    machine: &mut Machine,
    cont_closure: SynClosure,
    result_closure: SynClosure,
) -> Result<(), IoRunError> {
    let saturated = machine.mutate(
        SaturateCont,
        SaturateContInput {
            cont_closure,
            result_closure,
        },
    )?;

    machine.driver_resume(saturated);
    machine.run(None)?;
    Ok(())
}

struct SaturateContInput {
    cont_closure: SynClosure,
    result_closure: SynClosure,
}

struct SaturateCont;

impl Mutator for SaturateCont {
    type Input = SaturateContInput;
    type Output = SynClosure;

    fn run(
        &self,
        view: &MutatorHeapView,
        input: SaturateContInput,
    ) -> Result<SynClosure, ExecutionError> {
        view.saturate(&input.cont_closure, &[input.result_closure])
    }
}

// ─── Main io-run loop ─────────────────────────────────────────────────────────

/// Run the IO interpret loop on a machine that has yielded on an IO constructor.
///
/// `opt` is used to check `--allow-io` before executing any `IoAction`.
///
/// Returns `Ok(())` if the loop completed normally (IoReturn rendered), or an
/// `IoRunError` for failure.  The machine emitter will have received all render
/// events for the final value.
pub fn io_run(machine: &mut Machine, opt: &EucalyptOptions) -> Result<(), IoRunError> {
    // Stack of pending continuations from IoBind nodes.
    // Each entry is a continuation closure waiting for a result.
    let mut pending_conts: Vec<SynClosure> = Vec::new();

    loop {
        if !machine.io_yielded() {
            // Machine terminated normally — this should not happen if we were
            // called correctly.  Treat as a completed IoReturn.
            return Ok(());
        }

        let tag = match machine.yielded_io_tag() {
            Some(t) => t,
            None => return Ok(()),
        };

        let args = match machine.yielded_io_args() {
            Some(a) => a,
            None => return Ok(()),
        };

        if tag == DataConstructor::IoReturn.tag() {
            // args[0] = world (ignored), args[1] = value
            let value_closure = args.get(1).cloned().unwrap_or_else(|| args[0].clone());

            if let Some(cont) = pending_conts.pop() {
                // There's a pending continuation: apply it to the value
                apply_continuation(machine, cont, value_closure)?;
                // Continue the loop — machine will yield again or terminate
                continue;
            } else {
                // No pending continuations: render the value
                resume_with_render(machine, value_closure)?;
                return Ok(());
            }
        } else if tag == DataConstructor::IoFail.tag() {
            // args[0] = world (ignored), args[1] = error message
            let err_closure = args.get(1).cloned().unwrap_or_else(|| args[0].clone());
            let err_msg = extract_error_message(machine, err_closure);
            return Err(IoRunError::IoFail(err_msg));
        } else if tag == DataConstructor::IoBind.tag() {
            // args[0] = world, args[1] = action, args[2] = continuation
            let action = args.get(1).cloned().ok_or_else(|| {
                IoRunError::SpecBlockError("IoBind missing action arg".to_string())
            })?;
            let cont = args.get(2).cloned().ok_or_else(|| {
                IoRunError::SpecBlockError("IoBind missing continuation arg".to_string())
            })?;

            // Push the continuation, then force the action
            pending_conts.push(cont);

            machine.driver_resume(action);
            machine.run(None).map_err(IoRunError::from)?;
            // Continue the loop to dispatch on whatever the action produced
            continue;
        } else if tag == DataConstructor::IoAction.tag() {
            // args[0] = world (ignored), args[1] = spec_block (unevaluated)
            let spec_closure = args.get(1).cloned().ok_or_else(|| {
                IoRunError::SpecBlockError("IoAction missing spec_block arg".to_string())
            })?;

            // Force the spec block to WHNF
            machine.driver_resume(spec_closure.clone());
            machine.run(None).map_err(IoRunError::from)?;

            // Inspect the forced spec block
            let forced_spec = machine.current_closure();
            let sym_ids = machine.symbol_pool_strings();

            let spec = machine
                .mutate(
                    SpecInspector,
                    SpecInspectorInput {
                        closure: forced_spec,
                        sym_ids,
                    },
                )
                .map_err(|e| IoRunError::SpecBlockError(format!("{e}")))?;

            // Execute the action (checking --allow-io first)
            let result = execute_action(&spec, opt)?;

            // Intern the field key symbols into the machine's symbol pool
            // and build the result block on the heap.
            let root_env = machine.root_env_ptr();
            let (stdout_sym, stderr_sym, exit_code_sym) = machine.intern_result_keys();

            let result_closure = machine.mutate(
                ResultBlockBuilder,
                ResultBlockInput {
                    stdout: result.stdout,
                    stderr: result.stderr,
                    exit_code: result.exit_code,
                    root_env,
                    stdout_sym,
                    stderr_sym,
                    exit_code_sym,
                },
            )?;

            // We now have a result.  If there's a pending continuation, apply it.
            if let Some(cont) = pending_conts.pop() {
                apply_continuation(machine, cont, result_closure)?;
                continue;
            } else {
                // Standalone IoAction (no bind) — wrap in IoReturn and render
                let io_return_closure = make_io_return(machine, result_closure)?;
                machine.driver_resume(io_return_closure);
                machine.run(None).map_err(IoRunError::from)?;
                continue;
            }
        } else {
            return Err(IoRunError::SpecBlockError(format!(
                "unexpected IO tag {tag}"
            )));
        }
    }
}

/// Execute an action spec, returning a `ShellResult`.
fn execute_action(spec: &ActionSpec, opt: &EucalyptOptions) -> Result<ShellResult, IoRunError> {
    match spec.tag.as_str() {
        "io-shell" => {
            if !opt.allow_io() {
                return Err(IoRunError::NotAllowed);
            }
            execute_shell(&spec.cmd, spec.stdin.as_deref(), spec.timeout_secs)
        }
        "io-exec" => {
            if !opt.allow_io() {
                return Err(IoRunError::NotAllowed);
            }
            execute_exec(
                &spec.cmd,
                &spec.args,
                spec.stdin.as_deref(),
                spec.timeout_secs,
            )
        }
        other => Err(IoRunError::UnknownActionTag(other.to_string())),
    }
}

/// Extract an error message from a value closure (best effort)
fn extract_error_message(machine: &mut Machine, closure: SynClosure) -> String {
    machine
        .mutate(ErrorMessageExtractor, closure)
        .unwrap_or_else(|_| "unknown IO error".to_string())
}

struct ErrorMessageExtractor;

impl Mutator for ErrorMessageExtractor {
    type Input = SynClosure;
    type Output = String;

    fn run(&self, view: &MutatorHeapView, closure: SynClosure) -> Result<String, ExecutionError> {
        match resolve_closure_to_native(view, closure) {
            Ok(Native::Str(ptr)) => {
                let hs = view.scoped(ptr);
                Ok(hs.as_str().to_string())
            }
            Ok(n) => Ok(format!("{n:?}")),
            Err(_) => Ok("unknown IO error".to_string()),
        }
    }
}

/// Build an `IoReturn(world, value)` closure — used when a standalone IoAction
/// needs to be wrapped for looping.
fn make_io_return(
    machine: &mut Machine,
    value_closure: SynClosure,
) -> Result<SynClosure, IoRunError> {
    machine
        .mutate(MakeIoReturn, value_closure)
        .map_err(IoRunError::from)
}

struct MakeIoReturn;

impl Mutator for MakeIoReturn {
    type Input = SynClosure;
    type Output = SynClosure;

    fn run(
        &self,
        view: &MutatorHeapView,
        value_closure: SynClosure,
    ) -> Result<SynClosure, ExecutionError> {
        // IoReturn(world=Unit, value)
        let unit = view.unit()?;
        let empty = view.alloc(EnvFrame::default())?.as_ptr();
        let unit_closure = SynClosure::new(unit.as_ptr(), empty);

        // Build env [world=unit, value]
        let env = view.from_closures(
            [unit_closure, value_closure].into_iter(),
            2,
            empty,
            Smid::default(),
        )?;

        // Cons { IoReturn, [L(0), L(1)] }  — world at 0, value at 1
        let io_return = view.data(
            DataConstructor::IoReturn.tag(),
            view.array(&[Ref::L(0), Ref::L(1)]),
        )?;

        Ok(SynClosure::new(io_return.as_ptr(), env))
    }
}
