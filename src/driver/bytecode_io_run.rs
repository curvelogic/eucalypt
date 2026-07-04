//! IO monad interpret loop for the bytecode engine (BV1; eu-lka7).
//!
//! A faithful mirror of the HeapSyn io-run driver (`driver::io_run`) over the
//! [`BytecodeMachine`] instead of the HeapSyn `Machine`. It interprets the IO
//! data constructors (`IoReturn`, `IoBind`, `IoAction`, `IoFail`) that the
//! bytecode machine yields on, drives the shell-execution side effects (via the
//! shared [`crate::driver::io_common`] executor), and re-enters the machine
//! with results.
//!
//! The control flow, GC-stash discipline, and world-injection / RENDER_DOC
//! render steps match the HeapSyn driver one-for-one. The differences are all
//! in the *representation*: values are [`BcValue`]s navigated / synthesised via
//! the `BytecodeMachine` io-run API rather than HeapSyn `SynClosure`s built by
//! heap mutators.
//!
//! ## Deviation from HeapSyn (deliberate)
//!
//! The HeapSyn driver extracts the action tag (`io-shell` / `io-exec` /
//! `io-fail`) from the spec block's metadata symbol via `peel_meta`. In the
//! bytecode engine a metadata-annotated value has its `Meta` wrapper **stripped
//! during forcing** (a `Meta` reaching the top of an empty continuation stack
//! flows straight to its body — `machine::return_meta`), so the tag symbol is
//! gone by the time the block is in WHNF. We therefore infer the tag from the
//! evaluated field set, which is unambiguous for the four prelude IO actions:
//! `io-exec` has an `args` field, `io-shell` has `cmd` (no `args`), and
//! `io-fail` has `message` (no `cmd`). This matches the HeapSyn driver's own
//! field-based fallback for parameterised (App-thunk) spec blocks.

use std::collections::HashMap;

use crate::driver::io_common::{run_spec, ActionSpec, IoRunError};
use crate::eval::bytecode::{BcValue, BytecodeMachine};
use crate::eval::error::ExecutionError;
use crate::eval::intrinsics;
use crate::eval::memory::infotable::InfoTable;
use crate::eval::stg::tags::DataConstructor;

// ─── Spec-block evaluation ────────────────────────────────────────────────────

/// Build an [`ActionSpec`] from an `IoAction` spec block by forcing it to WHNF
/// (a `Block`, metadata stripped) and evaluating each field. Mirrors
/// `io_run::evaluate_spec_block`.
fn evaluate_spec_block(
    machine: &mut BytecodeMachine<'_>,
    spec_block: BcValue,
) -> Result<ActionSpec, IoRunError> {
    // Recover the spec block's meta tag (`io-shell` / `io-exec` / `io-fail`)
    // statically, BEFORE forcing strips the `Meta` wrapper. This lets the
    // bytecode engine dispatch on the actual tag exactly as the HeapSyn driver
    // does, so a spec whose field set disagrees with its tag (e.g.
    // `{:io-shell cmd: ..., args: []}`) runs the SAME action on both engines
    // (eu-xqab). `None` for App-thunk specs (`io.shell-with` / `io.exec-with`)
    // whose `Meta` is not statically visible — the shared policy then falls
    // back to field inference, matching HeapSyn's `peel_meta` behaviour.
    let tag = machine.peel_io_spec_tag(spec_block.clone());

    // Statically peel the `Meta` wrapper (see `BytecodeMachine::peel_io_spec`):
    // an inline spec block is a `let`-bound `Meta` whose body ref is relative
    // to the let frame, so it cannot be forced naively — we resolve the block
    // body against the container frame first. App-thunk specs pass through
    // unchanged. Then force the (block) body to WHNF.
    let body = machine.peel_io_spec(spec_block);
    let block = machine
        .evaluate_to_whnf_for_io(body)
        .map_err(IoRunError::from)?;

    // Read the raw (unevaluated) field value closures from the block.
    let raw_fields = machine
        .block_field_values(&block)
        .map_err(IoRunError::from)?;

    // Stash all field values before the per-field WHNF loop: each
    // `evaluate_to_whnf_for_io` runs the machine and may evacuate, leaving any
    // `BcValue` copies held on the Rust stack dangling. The stash is a GC root
    // set that the collector traces and updates. Push in reverse so the forward
    // pop order matches the field order.
    for (_, v) in raw_fields.iter().rev() {
        machine.stash_push(v.clone());
    }

    let mut eval_fields: HashMap<String, Option<String>> = HashMap::new();
    for (key, _) in &raw_fields {
        let raw = machine.stash_pop();
        let whnf = force_field(machine, raw)?;

        // List-valued fields (e.g. `args`) need each element forced and
        // rendered, then joined with NUL as a separator.
        let value_str = if machine.value_is_list(&whnf) {
            let elements = machine
                .list_element_values(&whnf)
                .map_err(IoRunError::from)?;
            // Stash all element closures before the loop (same GC reason as the
            // fields above); push in reverse for forward pop order.
            for e in elements.iter().rev() {
                machine.stash_push(e.clone());
            }
            let mut parts: Vec<String> = Vec::new();
            for _ in 0..elements.len() {
                let elem = machine.stash_pop();
                let elem_whnf = force_field(machine, elem)?;
                if let Some(s) = machine.whnf_scalar_string(&elem_whnf) {
                    parts.push(s);
                }
            }
            if parts.is_empty() {
                None
            } else {
                Some(parts.join("\x00"))
            }
        } else {
            machine.whnf_scalar_string(&whnf)
        };
        eval_fields.insert(key.clone(), value_str);
    }

    // Hand the recovered tag and evaluated fields to the shared, representation-
    // agnostic policy so both engines apply identical tag dispatch, timeout /
    // stdin handling, NUL-split args, required-cmd errors, and io-fail wording.
    let call_smid = machine.annotation();
    crate::driver::io_common::action_spec_from_fields(tag.as_deref(), &eval_fields, call_smid)
}

/// Force a spec-block field to WHNF, peeling a boxed scalar whose inner field
/// is still an unevaluated thunk (e.g. a `BoxedString` wrapping a format-expr).
/// Mirrors the `peel_box_inner` re-evaluation in `io_run::evaluate_spec_block`.
fn force_field(machine: &mut BytecodeMachine<'_>, value: BcValue) -> Result<BcValue, IoRunError> {
    let whnf = machine
        .evaluate_to_whnf_for_io(value)
        .map_err(IoRunError::from)?;
    if let Some(inner) = machine.peel_box_inner(&whnf) {
        machine
            .evaluate_to_whnf_for_io(inner)
            .map_err(IoRunError::from)
    } else {
        Ok(whnf)
    }
}

// ─── IO monad interpret loop ──────────────────────────────────────────────────

/// Interpret the IO monad until an `IoReturn` or `IoFail` is reached.
///
/// The machine must be in the io-yielded state when this is called. Returns the
/// pure value from the final `IoReturn`. Mirrors `io_run::io_run`.
pub fn io_run(machine: &mut BytecodeMachine<'_>, allow_io: bool) -> Result<BcValue, IoRunError> {
    loop {
        if !machine.io_yielded() {
            return Err(panic(
                machine,
                "bytecode io_run called on a non-yielded machine",
            ));
        }

        let tag = machine
            .yielded_io_tag()
            .ok_or_else(|| panic(machine, "bytecode io_run: machine yield has no IO tag"))?;
        let args = machine.yielded_io_args().ok_or_else(|| {
            panic(
                machine,
                "bytecode io_run: could not resolve IO constructor args",
            )
        })?;

        match DataConstructor::try_from(tag) {
            Ok(DataConstructor::IoReturn) => {
                // IoReturn(world=0, value=1)
                return args
                    .into_iter()
                    .nth(1)
                    .ok_or_else(|| panic(machine, "IoReturn missing value argument"));
            }

            Ok(DataConstructor::IoFail) => {
                let error_value = args
                    .into_iter()
                    .nth(1)
                    .ok_or_else(|| panic(machine, "IoFail missing error argument"))?;
                let msg = machine
                    .evaluate_to_whnf_for_io(error_value)
                    .ok()
                    .and_then(|c| machine.whnf_scalar_string(&c))
                    .unwrap_or_else(|| "IO monad failure".to_string());
                return Err(IoRunError::Fail(msg));
            }

            Ok(DataConstructor::IoAction) => {
                if !allow_io {
                    return Err(IoRunError::IoNotAllowed(machine.annotation()));
                }
                let world = args[0].clone();
                let spec_block = args[1].clone();

                // Stash world so it survives GC across the spec evaluation and
                // shell execution below.
                machine.stash_push(world);

                let spec = evaluate_spec_block(machine, spec_block)?;

                // Capture the source annotation before the shell call so
                // timeout / command errors carry a source location.
                let ann = machine.annotation();
                let result = run_spec(&spec, ann)?;

                // Build the result block { stdout, stderr, exit-code }. World is
                // still stashed as a GC root.
                let result_c = machine
                    .build_io_result_block(&result.stdout, &result.stderr, result.exit_code)
                    .map_err(IoRunError::from)?;

                // Wrap in IoReturn(world, result_block) and resume. Pop world
                // (GC-updated) only now that all allocation is complete.
                machine.stash_push(result_c);
                let result_c = machine.stash_pop();
                let world = machine.stash_pop();
                let io_return = machine
                    .build_io_return(world, result_c)
                    .map_err(IoRunError::from)?;

                machine.stash_push(io_return);
                let io_return = machine.stash_peek(0);
                machine.resume(io_return);
                machine.run(None).map_err(IoRunError::from)?;
                machine.stash_pop();
                // Loop back to inspect the new yield.
            }

            Ok(DataConstructor::IoBind) => {
                // IoBind(world=0, cont=1, action=2)
                let world = args[0].clone();
                let cont = args[1].clone();
                let action = args[2].clone();

                // Stash `cont` and `world` across the recursive machine runs.
                machine.stash_push(cont);
                machine.stash_push(world);

                // Step 1: apply world to the action (a PAP waiting for world,
                // e.g. io.return(42) = λworld.IoReturn(world, 42)).
                let world_ref = machine.stash_peek(0);
                let action_with_world = machine
                    .build_apply1(action, world_ref)
                    .map_err(IoRunError::from)?;

                machine.stash_push(action_with_world);
                let action_with_world = machine.stash_peek(0);
                machine.resume(action_with_world);
                machine.run(None).map_err(IoRunError::from)?;
                machine.stash_pop(); // action_with_world

                if !machine.io_yielded() {
                    machine.stash_pop(); // world
                    machine.stash_pop(); // cont
                    return Err(panic(
                        machine,
                        "IoBind action did not yield an IO constructor",
                    ));
                }

                // Step 2: recursively process the action result. `cont` and
                // `world` remain stashed throughout.
                let action_result = io_run(machine, allow_io)?;

                // Step 3: apply the continuation to (result, world).
                machine.stash_push(action_result);
                let action_result = machine.stash_peek(0);
                let world = machine.stash_peek(1);
                let cont = machine.stash_peek(2);
                let cont_call = machine
                    .build_apply2(cont, action_result, world)
                    .map_err(IoRunError::from)?;
                machine.stash_pop(); // action_result
                machine.stash_pop(); // world
                machine.stash_pop(); // cont

                machine.stash_push(cont_call);
                let cont_call = machine.stash_peek(0);
                machine.resume(cont_call);
                machine.run(None).map_err(IoRunError::from)?;
                machine.stash_pop();
                // Loop back.
            }

            _ => {
                return Err(panic(
                    machine,
                    &format!("unexpected IO constructor tag: {tag}"),
                ));
            }
        }
    }
}

/// Build a `RENDER_DOC(value)` application thunk (the render entry point).
fn build_render_doc(machine: &BytecodeMachine<'_>, value: BcValue) -> Result<BcValue, IoRunError> {
    let idx = intrinsics::index("RENDER_DOC")
        .ok_or_else(|| panic(machine, "RENDER_DOC intrinsic not found in registry"))?;
    let render_global = machine.global_value(idx).map_err(IoRunError::from)?;
    machine
        .build_apply1(render_global, value)
        .map_err(IoRunError::from)
}

/// Apply the initial world token (unit) to an IO function and re-run so the
/// first IO constructor is produced and the machine yields. Returns `false` if
/// the closure is not a function (a plain document value) or if injection did
/// not result in an IO yield. Mirrors `io_run::inject_world_and_run`.
pub fn inject_world_and_run(machine: &mut BytecodeMachine<'_>) -> Result<bool, IoRunError> {
    let io_fn = machine.current();

    // Only inject world if the current value is a function (arity > 0). Plain
    // data values are rendered directly without world injection.
    let arity = match &io_fn {
        BcValue::Closure(c) => c.arity(),
        BcValue::Native(_) => 0,
    };
    if arity == 0 {
        return Ok(false);
    }

    let unit = machine.build_unit().map_err(IoRunError::from)?;
    let apply_c = machine
        .build_apply1(io_fn, unit)
        .map_err(IoRunError::from)?;
    machine.resume(apply_c);
    machine.run(None).map_err(IoRunError::from)?;

    Ok(machine.io_yielded())
}

/// Render the result of a headless evaluation that did not yield an IO
/// constructor: build `RENDER_DOC(value)` and re-run. Mirrors
/// `io_run::render_headless_result`.
pub fn render_headless_result(machine: &mut BytecodeMachine<'_>) -> Result<Option<u8>, IoRunError> {
    let value = machine.current();
    let render_c = build_render_doc(machine, value)?;
    machine.resume(render_c);
    machine.run(None).map_err(IoRunError::from)
}

/// Run the IO monad interpret loop and render the final pure value. The main
/// integration point called from `eval.rs`. Mirrors `io_run::io_run_and_render`.
pub fn io_run_and_render(
    machine: &mut BytecodeMachine<'_>,
    allow_io: bool,
) -> Result<Option<u8>, IoRunError> {
    let final_value = io_run(machine, allow_io)?;
    let render_c = build_render_doc(machine, final_value)?;
    machine.resume(render_c);
    machine.run(None).map_err(IoRunError::from)
}

/// Construct a boxed `Panic` `IoRunError` carrying the machine's annotation.
fn panic(machine: &BytecodeMachine<'_>, msg: &str) -> IoRunError {
    IoRunError::MachineError(Box::new(ExecutionError::Panic(
        machine.annotation(),
        msg.to_string(),
    )))
}
