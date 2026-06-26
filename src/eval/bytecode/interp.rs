//! Bytecode interpreter: flat opcode dispatch replacing HeapSyn tree-walk.
//!
//! The interpreter reuses the existing continuation stack, environment
//! frames, heap, and intrinsic machinery.  Only the dispatch path
//! changes: instead of dereferencing a `RefPtr<HeapSyn>` and matching
//! on an enum variant, we read a `u8` opcode from a contiguous
//! `Vec<u8>` buffer and match on that.
//!
//! Closures that point to bytecode use `HeapSyn::Atom { V(Num(offset)) }`
//! as their code pointer.  When the interpreter enters such a closure,
//! it dispatches the opcode at `offset`.  Non-bytecode closures (e.g.
//! those created by the runtime or by intrinsics that return HeapSyn
//! closures) fall through to the existing HeapSyn dispatch.

use serde_json::Number;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::{
            cont::Continuation,
            env::SynClosure,
            env_builder::EnvBuilder,
            intrinsic::StgIntrinsic,
            metrics::{Metrics, ThreadOccupation},
            vm::{interrupted, HeapNavigator, Machine, MachineBifContext, MachineState},
        },
        memory::{
            alloc::ScopedAllocator,
            array::Array,
            infotable::{InfoTable, InfoTagged},
            mutator::MutatorHeapView,
            syntax::{HeapSyn, LambdaForm, Native, Ref, RefPtr, StgBuilder},
        },
        stg::tags::DataConstructor,
    },
};

use super::{
    opcode,
    program::{read_u16, read_u32, read_u8, BytecodeProgram},
};

/// Bytecode stub sentinel base.  We use a large negative value
/// (below i32::MIN) that cannot plausibly be a user-program number.
/// Encoding: `sentinel = SENTINEL_BASE - offset`.
/// Decoding: `offset = SENTINEL_BASE - sentinel`.
const BYTECODE_SENTINEL_BASE: i64 = -0x4000_0000_0000;

/// Detect a bytecode stub: `HeapSyn::Atom { V(Num(n)) }` where `n`
/// is a very large negative integer (below the sentinel base).
fn bytecode_offset(code: &HeapSyn) -> Option<usize> {
    if let HeapSyn::Atom {
        evaluand: Ref::V(Native::Num(n)),
    } = code
    {
        n.as_i64().and_then(|i| {
            if i <= BYTECODE_SENTINEL_BASE {
                Some((BYTECODE_SENTINEL_BASE - i) as usize)
            } else {
                None
            }
        })
    } else {
        None
    }
}

/// Allocate a `HeapSyn::Atom { V(Num(offset)) }` stub on the heap.
///
/// These tiny heap objects serve as code pointers for bytecode closures.
/// They are never GC-scanned as code (the GC treats them as normal Atom
/// nodes) and are never entered directly — the interpreter intercepts
/// them before normal HeapSyn dispatch.
fn alloc_bytecode_stub(
    view: MutatorHeapView<'_>,
    offset: u32,
) -> Result<RefPtr<HeapSyn>, ExecutionError> {
    let sentinel = BYTECODE_SENTINEL_BASE - (offset as i64);
    let n = Number::from(sentinel);
    Ok(view
        .alloc(HeapSyn::Atom {
            evaluand: Ref::V(Native::Num(n)),
        })?
        .as_ptr())
}

/// Read an inline ref from the bytecode stream.
///
/// Uses the pre-converted `heap_refs` for constant pool lookups.
fn read_ref(code: &[u8], pc: &mut usize, program: &BytecodeProgram) -> Ref {
    let tag = read_u8(code, pc);
    match tag {
        opcode::REF_L => Ref::L(read_u16(code, pc) as usize),
        opcode::REF_G => Ref::G(read_u16(code, pc) as usize),
        opcode::REF_V => {
            let ci = read_u32(code, pc) as usize;
            program.heap_refs[ci].clone()
        }
        _ => unreachable!("bad ref tag: {tag:#x}"),
    }
}

/// Dispatch one bytecode instruction.
///
/// Called when the current closure's code is a bytecode stub.
/// Reads the opcode at `pc` and executes it, modifying `state`
/// (closure, stack, annotation, etc.) just as `handle_instruction`
/// would.
///
/// # Safety
///
/// The caller must ensure that `pc` is a valid offset into
/// `program.code`.
fn dispatch(
    state: &mut MachineState,
    program: &BytecodeProgram,
    pc: usize,
    view: MutatorHeapView<'_>,
    metrics: &mut Metrics,
    _intrinsics: &[&dyn StgIntrinsic],
) -> Result<(), ExecutionError> {
    let code = &program.code;
    let environment = state.closure.env();
    let remaining_arity = state.closure.arity();

    // Set annotation from the closure (same as HeapSyn path)
    let closure_ann = state.closure.annotation();
    if closure_ann.is_valid() {
        state.annotation = closure_ann;
    }

    if remaining_arity > 0 {
        return state.return_fun(view);
    }

    let mut pc = pc;
    let op = read_u8(code, &mut pc);

    match op {
        opcode::ATOM_L => {
            let i = read_u16(code, &mut pc) as usize;
            let nav = HeapNavigator::new(view, environment, state.globals, state.annotation);
            state.closure = nav.get(i)?;
            let is_thunk = state.closure.update();
            if is_thunk {
                let hole = view.alloc(HeapSyn::BlackHole)?;
                let black_hole = SynClosure::new(hole.as_ptr(), environment);
                let cont_env = view.scoped(environment);
                cont_env.update(&view, i, black_hole)?;
                state.push(
                    view,
                    Continuation::Update {
                        environment,
                        index: i,
                    },
                )?;
            }
        }

        opcode::ATOM_G => {
            let i = read_u16(code, &mut pc) as usize;
            let nav = HeapNavigator::new(view, environment, state.globals, state.annotation);
            state.closure = nav.global(i)?;
        }

        opcode::ATOM_V => {
            let ci = read_u32(code, &mut pc) as usize;
            let heap_ref = &program.heap_refs[ci];
            if let Ref::V(v) = heap_ref {
                // Allocate a proper HeapSyn::Atom with the actual value
                // so the closure code pointer holds the real value
                // (not the bytecode stub offset).
                let atom = view.alloc(HeapSyn::Atom {
                    evaluand: heap_ref.clone(),
                })?;
                state.closure = SynClosure::new(atom.as_ptr(), environment);
                state.return_native(view, v)?;
            }
        }

        opcode::APP => {
            let callable = read_ref(code, &mut pc, program);
            let nargs = read_u8(code, &mut pc) as usize;
            let mut refs = Vec::with_capacity(nargs);
            for _ in 0..nargs {
                refs.push(read_ref(code, &mut pc, program));
            }
            let array = view.create_arg_array(&refs, environment)?;
            state.push(
                view,
                Continuation::ApplyTo {
                    args: array,
                    annotation: state.annotation,
                },
            )?;
            // Handle thunk blackholing for local callable refs
            if let Ref::L(i) = &callable {
                let nav = HeapNavigator::new(view, environment, state.globals, state.annotation);
                let callee = nav.get(*i)?;
                let is_thunk = callee.update();
                if is_thunk {
                    let hole = view.alloc(HeapSyn::BlackHole)?;
                    let black_hole = SynClosure::new(hole.as_ptr(), environment);
                    let cont_env = view.scoped(environment);
                    cont_env.update(&view, *i, black_hole)?;
                    state.push(
                        view,
                        Continuation::Update {
                            environment,
                            index: *i,
                        },
                    )?;
                    state.closure = callee;
                } else {
                    let nav2 =
                        HeapNavigator::new(view, environment, state.globals, state.annotation);
                    state.closure = nav2.resolve_callable(&callable)?;
                }
            } else {
                let nav = HeapNavigator::new(view, environment, state.globals, state.annotation);
                state.closure = nav.resolve_callable(&callable)?;
            }
        }

        opcode::DIRECT_APP => {
            let smid_val = read_u32(code, &mut pc);
            let smid = Smid::from(smid_val);
            state.annotation = smid;
            let callable = read_ref(code, &mut pc, program);
            let nargs = read_u8(code, &mut pc) as usize;
            let mut refs = Vec::with_capacity(nargs);
            for _ in 0..nargs {
                refs.push(read_ref(code, &mut pc, program));
            }
            let array = view.create_arg_array(&refs, environment)?;
            let nav = HeapNavigator::new(view, environment, state.globals, state.annotation);
            let closure = nav.resolve_callable(&callable)?;
            if closure.arity() as usize == array.len() {
                state.closure = view.saturate_with_array(&closure, array)?;
            } else {
                state.push(
                    view,
                    Continuation::ApplyTo {
                        args: array,
                        annotation: state.annotation,
                    },
                )?;
                state.closure = closure;
            }
        }

        opcode::BIF => {
            let intrinsic_idx = read_u8(code, &mut pc);
            let nargs = read_u8(code, &mut pc) as usize;
            let mut refs = Vec::with_capacity(nargs);
            for _ in 0..nargs {
                refs.push(read_ref(code, &mut pc, program));
            }
            // Allocate a HeapSyn::Bif node on the heap so that
            // Machine::step()'s pending_bif handler can read args from it.
            let args_array = {
                let mut a = Array::with_capacity(&view, nargs);
                for r in &refs {
                    a.push(&view, r.clone());
                }
                a
            };
            let bif_node = view.alloc(HeapSyn::Bif {
                intrinsic: intrinsic_idx,
                args: args_array,
            })?;
            // Reconstruct the closure with the BIF code pointer so that
            // Machine::step()'s pending_bif handler can read the args
            // from it.
            state.closure = SynClosure::new(bif_node.as_ptr(), environment);
            state.pending_bif = Some(intrinsic_idx);
        }

        opcode::CASE => {
            let scr_off = read_u32(code, &mut pc);
            let nbranches = read_u8(code, &mut pc) as usize;

            // Build the branch table
            let mut branches: Vec<(u8, u32)> = Vec::with_capacity(nbranches);
            for _ in 0..nbranches {
                let tag = read_u8(code, &mut pc);
                let off = read_u32(code, &mut pc);
                branches.push((tag, off));
            }
            let has_fallback = read_u8(code, &mut pc) != 0;
            let fb_off = if has_fallback {
                Some(read_u32(code, &mut pc))
            } else {
                None
            };

            // Build indexed branch table (as HeapSyn uses) with bytecode stubs
            let min_tag = branches.iter().map(|(t, _)| *t).min().unwrap_or(0);
            let max_tag = branches.iter().map(|(t, _)| *t).max().unwrap_or(0);
            let table_size = if branches.is_empty() {
                0
            } else {
                (max_tag - min_tag + 1) as usize
            };

            let mut branch_table: Array<Option<RefPtr<HeapSyn>>> =
                Array::with_capacity(&view, table_size);
            for _ in 0..table_size {
                branch_table.push(&view, None);
            }
            for (tag, off) in &branches {
                let stub = alloc_bytecode_stub(view, *off)?;
                let index = (*tag - min_tag) as usize;
                // SAFETY: index within range by construction
                unsafe { branch_table.set_unchecked(index, Some(stub)) };
            }

            let fallback = match fb_off {
                Some(off) => Some(alloc_bytecode_stub(view, off)?),
                None => None,
            };

            state.push(
                view,
                Continuation::Branch {
                    min_tag,
                    branch_table,
                    fallback,
                    environment,
                    annotation: state.annotation,
                },
            )?;

            // Enter the scrutinee
            let scr_stub = alloc_bytecode_stub(view, scr_off)?;
            state.closure = SynClosure::new(scr_stub, environment);
        }

        opcode::CONS => {
            let tag = read_u8(code, &mut pc);
            let nargs = read_u8(code, &mut pc) as usize;
            let mut refs = Vec::with_capacity(nargs);
            for _ in 0..nargs {
                refs.push(read_ref(code, &mut pc, program));
            }
            // Allocate a proper HeapSyn::Cons so the closure code
            // reflects the actual data constructor (not the bytecode
            // stub).  This is needed because return_data does not
            // update the closure code when terminating.
            let args_arr = Array::from_slice(&view, &refs);
            let cons_node = view.alloc(HeapSyn::Cons {
                tag,
                args: args_arr,
            })?;
            state.closure = SynClosure::new(cons_node.as_ptr(), environment);
            state.return_data(view, tag, &refs)?;
        }

        opcode::LET => {
            let nbindings = read_u16(code, &mut pc) as usize;
            metrics.alloc(nbindings);
            let mut lambda_forms: Vec<LambdaForm> = Vec::with_capacity(nbindings);
            for _ in 0..nbindings {
                let kind = read_u8(code, &mut pc);
                let arity = read_u8(code, &mut pc);
                let smid_val = read_u32(code, &mut pc);
                let body_off = read_u32(code, &mut pc);
                let stub = alloc_bytecode_stub(view, body_off)?;
                let smid = Smid::from(smid_val);
                let lf = match kind {
                    opcode::FORM_LAMBDA => InfoTagged::new(arity, stub, smid),
                    opcode::FORM_THUNK => InfoTagged::thunk(stub),
                    opcode::FORM_VALUE => InfoTagged::value(stub),
                    _ => unreachable!("bad lambda form kind: {kind}"),
                };
                lambda_forms.push(lf);
            }
            let body_off = read_u32(code, &mut pc);

            // Create env frame using existing from_let machinery
            let binding_slice: Vec<LambdaForm> = lambda_forms;
            let new_env = view.from_let(&binding_slice, environment, state.annotation)?;
            let body_stub = alloc_bytecode_stub(view, body_off)?;
            state.closure = SynClosure::new(body_stub, new_env);
        }

        opcode::LETREC => {
            let nbindings = read_u16(code, &mut pc) as usize;
            metrics.alloc(nbindings);
            let mut lambda_forms: Vec<LambdaForm> = Vec::with_capacity(nbindings);
            for _ in 0..nbindings {
                let kind = read_u8(code, &mut pc);
                let arity = read_u8(code, &mut pc);
                let smid_val = read_u32(code, &mut pc);
                let body_off = read_u32(code, &mut pc);
                let stub = alloc_bytecode_stub(view, body_off)?;
                let smid = Smid::from(smid_val);
                let lf = match kind {
                    opcode::FORM_LAMBDA => InfoTagged::new(arity, stub, smid),
                    opcode::FORM_THUNK => InfoTagged::thunk(stub),
                    opcode::FORM_VALUE => InfoTagged::value(stub),
                    _ => unreachable!("bad lambda form kind: {kind}"),
                };
                lambda_forms.push(lf);
            }
            let body_off = read_u32(code, &mut pc);

            let binding_slice: Vec<LambdaForm> = lambda_forms;
            let new_env = view.from_letrec(&binding_slice, environment, state.annotation)?;
            let body_stub = alloc_bytecode_stub(view, body_off)?;
            state.closure = SynClosure::new(body_stub, new_env);
        }

        opcode::ANN => {
            let smid_val = read_u32(code, &mut pc);
            let body_off = read_u32(code, &mut pc);
            state.annotation = Smid::from(smid_val);
            let body_stub = alloc_bytecode_stub(view, body_off)?;
            state.closure = SynClosure::new(body_stub, environment);
        }

        opcode::META => {
            let meta = read_ref(code, &mut pc, program);
            let body = read_ref(code, &mut pc, program);
            state.return_meta(view, &meta, &body)?;
        }

        opcode::DEMETA => {
            let scr_off = read_u32(code, &mut pc);
            let handler_off = read_u32(code, &mut pc);
            let or_else_off = read_u32(code, &mut pc);

            let handler_stub = alloc_bytecode_stub(view, handler_off)?;
            let or_else_stub = alloc_bytecode_stub(view, or_else_off)?;

            state.push(
                view,
                Continuation::DeMeta {
                    handler: handler_stub,
                    or_else: or_else_stub,
                    environment,
                },
            )?;
            let scr_stub = alloc_bytecode_stub(view, scr_off)?;
            state.closure = SynClosure::new(scr_stub, environment);
        }

        opcode::SEQ => {
            let scr_off = read_u32(code, &mut pc);
            let body_off = read_u32(code, &mut pc);

            let body_stub = alloc_bytecode_stub(view, body_off)?;
            state.push(
                view,
                Continuation::SeqBind {
                    body: body_stub,
                    environment,
                    annotation: state.annotation,
                },
            )?;
            let scr_stub = alloc_bytecode_stub(view, scr_off)?;
            state.closure = SynClosure::new(scr_stub, environment);
        }

        opcode::LOOKUP_LIT => {
            let smid_val = read_u32(code, &mut pc);
            let smid = Smid::from(smid_val);
            state.annotation = smid;
            let key = read_ref(code, &mut pc, program);
            let obj = read_ref(code, &mut pc, program);
            let default = read_ref(code, &mut pc, program);

            // Resolve the key to a SymbolId
            let sym_id = match &key {
                Ref::V(Native::Sym(id)) => *id,
                _ => {
                    return Err(ExecutionError::NotValue(
                        state.annotation,
                        "non-symbol key in LookupLit".to_string(),
                    ))
                }
            };

            // Resolve obj to a closure
            let nav = HeapNavigator::new(view, environment, state.globals, state.annotation);
            let obj_closure = match &obj {
                Ref::L(i) => nav.get(*i)?,
                Ref::G(i) => nav.global(*i)?,
                Ref::V(_) => {
                    return Err(ExecutionError::NotCallable(
                        state.annotation,
                        "native value".to_string(),
                    ))
                }
            };

            // Fast path: obj already a Block
            let is_block = {
                let obj_code = view.scoped(obj_closure.code());
                matches!(&*obj_code, HeapSyn::Cons { tag, .. } if *tag == DataConstructor::Block.tag())
            };
            if is_block {
                if let Some(value) =
                    crate::eval::stg::block::lookup_lit_in_block(&nav, view, &obj_closure, sym_id)
                {
                    state.closure = value;
                } else {
                    state.closure = match &default {
                        Ref::L(i) => nav.get(*i)?,
                        Ref::G(i) => nav.global(*i)?,
                        Ref::V(_) => {
                            let atom = view.atom(default)?;
                            SynClosure::new(atom.as_ptr(), environment)
                        }
                    };
                }
                return Ok(());
            }

            // Slow path: force the obj
            let default_closure = match &default {
                Ref::L(i) => nav.get(*i)?,
                Ref::G(i) => nav.global(*i)?,
                Ref::V(_) => {
                    let atom = view.atom(default)?;
                    SynClosure::new(atom.as_ptr(), environment)
                }
            };

            state.push(
                view,
                Continuation::LookupLitForce {
                    key: sym_id,
                    smid,
                    default_closure,
                },
            )?;

            if let Ref::L(i) = &obj {
                if obj_closure.update() {
                    let hole = view.alloc(HeapSyn::BlackHole)?;
                    let black_hole = SynClosure::new(hole.as_ptr(), environment);
                    let cont_env = view.scoped(environment);
                    cont_env.update(&view, *i, black_hole)?;
                    state.push(
                        view,
                        Continuation::Update {
                            environment,
                            index: *i,
                        },
                    )?;
                }
            }

            state.closure = obj_closure;
        }

        opcode::BLACKHOLE => {
            return Err(ExecutionError::BlackHole(state.annotation));
        }

        _ => {
            panic!("unimplemented bytecode opcode: {op:#x} at pc={}", pc - 1);
        }
    }

    Ok(())
}

/// Patch the machine's root closure to use a bytecode stub.
///
/// `root_off` is the bytecode offset for the root program.
///
/// Globals are NOT patched — they stay as HeapSyn closures.  When
/// bytecode dispatch encounters a global reference (ATOM_G), it
/// retrieves the HeapSyn closure and the hybrid dispatcher falls
/// through to HeapSyn dispatch.  This keeps intrinsic wrappers
/// (RENDER_DOC, MERGE, LOOKUP, etc.) working correctly.
///
/// Must be called after `standard_machine()` and `prepare_constants()`.
pub fn patch_machine_for_bytecode(
    machine: &mut Machine<'_>,
    _program: &BytecodeProgram,
    root_off: u32,
    _global_forms: &[super::encode::GlobalForm],
) -> Result<(), ExecutionError> {
    let view = MutatorHeapView::new(&machine.core.heap);

    // Patch root closure only
    let root_stub = alloc_bytecode_stub(view, root_off)?;
    machine.state.closure = SynClosure::new(root_stub, machine.state.closure.env());

    Ok(())
}

/// Run the machine using bytecode dispatch until termination.
///
/// This mirrors `Machine::run()` but checks each tick whether the
/// current closure is a bytecode stub.  If so, dispatches via the
/// bytecode interpreter; otherwise, falls through to the existing
/// `handle_instruction` path.
pub fn bytecode_run(
    machine: &mut Machine<'_>,
    program: &BytecodeProgram,
) -> Result<Option<u8>, ExecutionError> {
    // Register crash diagnostics
    crate::eval::machine::crash::register_crash_diagnostics(&machine.core.crash_diagnostics);

    machine.core.clock.switch(ThreadOccupation::Mutator);

    let gc_check_freq: u32 = 500;
    let mut gc_countdown: u32 = gc_check_freq;

    while !machine.state.terminated {
        gc_countdown -= 1;
        if gc_countdown == 0 {
            gc_countdown = gc_check_freq;

            if interrupted() {
                return Err(ExecutionError::Interrupted);
            }

            if machine.core.heap.policy_requires_collection() {
                machine.collect_with_diagnostics();
                machine.core.clock.switch(ThreadOccupation::Mutator);
            }
        }

        bytecode_step(machine, program)?;
    }

    if machine.core.heap.policy_requires_collection() {
        machine.collect_with_diagnostics();
    }
    machine.core.clock.stop();

    Ok(machine.exit_code())
}

/// Execute one step using bytecode dispatch where possible.
fn bytecode_step(
    machine: &mut Machine<'_>,
    program: &BytecodeProgram,
) -> Result<(), ExecutionError> {
    machine.core.metrics.tick();

    let view = MutatorHeapView::new(&machine.core.heap);

    // Check if current closure points to bytecode
    let code: &HeapSyn = unsafe { &*machine.state.closure.code().as_ptr() };
    if let Some(pc) = bytecode_offset(code) {
        dispatch(
            &mut machine.state,
            program,
            pc,
            view,
            &mut machine.core.metrics,
            &machine.core.intrinsics,
        )
        .map_err(|e| {
            let nav_view = MutatorHeapView::new(&machine.core.heap);
            let nav = HeapNavigator::new(
                nav_view,
                machine.state.closure.env(),
                machine.state.globals,
                machine.state.annotation,
            );
            ExecutionError::Traced(
                Box::new(e),
                nav.env_trace(),
                machine
                    .state
                    .stack_trace(&MutatorHeapView::new(&machine.core.heap)),
            )
        })?;
    } else {
        // HeapSyn dispatch (for intrinsic wrappers, data constructors, etc.)
        let emitter: &mut dyn Emitter = if let Some(top) = machine.capture_emitters.last_mut() {
            top as &mut dyn Emitter
        } else {
            machine.emitter.as_mut()
        };
        machine
            .state
            .handle_instruction(
                view,
                emitter,
                &machine.core.intrinsics,
                &mut machine.core.metrics,
            )
            .map_err(|e| {
                let nav_view = MutatorHeapView::new(&machine.core.heap);
                ExecutionError::Traced(
                    Box::new(e),
                    HeapNavigator::new(
                        nav_view,
                        machine.state.closure.env(),
                        machine.state.globals,
                        machine.state.annotation,
                    )
                    .env_trace(),
                    machine
                        .state
                        .stack_trace(&MutatorHeapView::new(&machine.core.heap)),
                )
            })?;
    }

    // Handle pending BIF (same as Machine::step())
    if let Some(intrinsic_idx) = machine.state.pending_bif.take() {
        let bif = machine.core.intrinsics[intrinsic_idx as usize];
        // Build view from raw ptr so that &mut core remains available for ctx.
        let bif_view = unsafe { MutatorHeapView::from_raw_heap(&machine.core.heap as *const _) };
        // Re-derive args from the closure (same as Machine::step()).
        let args: &[Ref] = match unsafe { &*machine.state.closure.code().as_ptr() } {
            HeapSyn::Bif { args, .. } => args.as_slice(),
            _ => &[],
        };
        let bif_emitter: &mut dyn Emitter = if let Some(top) = machine.capture_emitters.last_mut() {
            top as &mut dyn Emitter
        } else {
            machine.emitter.as_mut()
        };
        let mut ctx = MachineBifContext {
            state: &mut machine.state,
            core: &mut machine.core,
        };
        let bif_result = bif.execute(&mut ctx, bif_view, bif_emitter, args);
        if bif_result.is_err() {
            let ann_view = unsafe { MutatorHeapView::from_raw_heap(&ctx.core.heap as *const _) };
            if let Ok(global_closure) = HeapNavigator::new(
                ann_view,
                ctx.state.closure.env(),
                ctx.state.globals,
                ctx.state.annotation,
            )
            .global(intrinsic_idx as usize)
            {
                let ann = global_closure.annotation();
                if ann.is_valid() {
                    ctx.state.annotation = ann;
                }
            }
        }
        // ctx borrows end here.
        bif_result.map_err(|e| {
            let view = MutatorHeapView::new(&machine.core.heap);
            ExecutionError::Traced(
                Box::new(e),
                HeapNavigator::new(
                    view,
                    machine.state.closure.env(),
                    machine.state.globals,
                    machine.state.annotation,
                )
                .env_trace(),
                machine
                    .state
                    .stack_trace(&MutatorHeapView::new(&machine.core.heap)),
            )
        })?;
    }

    // Handle capture lifecycle
    if let Some(format) = machine.state.pending_capture_start.take() {
        let mut capture = crate::eval::stg::render_to_string::OwnedCaptureEmitter::new(
            &format,
            machine.state.annotation,
        )?;
        capture.stream_start();
        machine.capture_emitters.push(capture);
    }

    if machine.state.capture_end_pending {
        machine.state.capture_end_pending = false;
        let mut capture = machine.capture_emitters.pop().ok_or_else(|| {
            ExecutionError::Panic(Smid::default(), "no active capture emitter".to_string())
        })?;
        capture.stream_end();
        let result_str = capture.into_string(machine.state.annotation)?;
        let view = MutatorHeapView::new(&machine.core.heap);
        let str_ref = view.str_ref(result_str)?;
        let atom = view.alloc(HeapSyn::Atom { evaluand: str_ref })?.as_ptr();
        machine.state.closure = SynClosure::new(atom, machine.state.root_env);
    }

    Ok(())
}
