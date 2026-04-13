//! The `__EXPECT` intrinsic — unified test expectation with stderr diagnostics.
//!
//! `__EXPECT(actual, expected_repr, pass)` is the core of the unified
//! expectation operators (`//=`, `//!`). On failure it emits a diagnostic
//! to stderr and returns `false`; on success it returns `true`.

use std::convert::TryInto;

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::intrinsic::{CallGlobal4, IntrinsicMachine, StgIntrinsic},
    memory::{
        mutator::MutatorHeapView,
        syntax::{HeapSyn, Ref},
    },
    stg::support::{machine_return_bool, str_arg},
};

use super::{debug::render_debug_repr, tags::DataConstructor};

/// Resolve a strict bool argument to a Rust bool.
///
/// Since `pass` is declared strict in the intrinsic catalogue, it
/// will have been evaluated to WHNF before `execute` runs, so we
/// only need to check the data constructor tag.
fn resolve_bool(machine: &dyn IntrinsicMachine, view: MutatorHeapView<'_>, r: &Ref) -> bool {
    let closure = match machine.nav(view).resolve(r) {
        Ok(c) => c,
        Err(_) => return false,
    };
    let code = view.scoped(closure.code());
    match &*code {
        HeapSyn::Cons { tag, .. } => {
            let dc: Result<DataConstructor, _> = (*tag).try_into();
            matches!(dc, Ok(DataConstructor::BoolTrue))
        }
        _ => false,
    }
}

/// `__EXPECT(actual, expected_repr, pass, success_value)` — unified
/// expectation / assertion BIF.
///
/// Behaviour:
/// - `pass` is `true` → return `success_value` unchanged.
/// - `pass` is `false` → emit a "EXPECT FAILED" diagnostic to stderr
///   with the expected representation and the actual value rendered
///   by `render_debug_repr`, then:
///   - In test mode: return `false`.
///   - In normal mode: raise `AssertionFailed` error.
///
/// The `actual` argument is lazy (not forced by the wrapper) so that
/// unforced thunks render as `<unevaluated>` in failure messages.
///
/// For boolean expectations (`//=`, `//!`, `//=?`), pass `true` as
/// `success_value`. For pass-through assertions (`//=>`, `//=?>`),
/// pass the actual value as `success_value`.
///
/// `expected_repr` and `pass` are strict. `success_value` is lazy
/// (only returned on success, never inspected on failure).
pub struct Expect;

impl StgIntrinsic for Expect {
    fn name(&self) -> &str {
        "EXPECT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args[0] = actual value (lazy — rendered only on failure)
        // args[1] = expected representation string (strict)
        // args[2] = pass boolean (strict)
        // args[3] = success return value (lazy — returned on success)
        let pass = resolve_bool(machine, view, &args[2]);

        if pass {
            // Return the success value (args[3]) as-is
            let closure = machine.nav(view).resolve(&args[3])?;
            machine.set_closure(closure)
        } else {
            let expected_repr = str_arg(machine, view, &args[1])?;
            let actual_repr = render_debug_repr(machine, view, &args[0]);
            eprintln!("EXPECT FAILED: expected {expected_repr}, got {actual_repr}");

            if machine.test_mode() {
                // Test mode: return false, continue execution
                machine_return_bool(machine, view, false)
            } else {
                // Normal mode: panic with assertion failure
                Err(ExecutionError::AssertionFailed(
                    machine.annotation(),
                    actual_repr,
                    expected_repr,
                ))
            }
        }
    }
}

impl CallGlobal4 for Expect {}
