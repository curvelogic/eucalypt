//! The `__EXPECT` intrinsic — unified test expectation with stderr diagnostics.
//!
//! `__EXPECT(actual, expected_repr, pass)` is the core of the unified
//! expectation operators (`//=`, `//!`). On failure it emits a diagnostic
//! to stderr and returns `false`; on success it returns `true`.

use std::convert::TryInto;

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::intrinsic::{CallGlobal3, IntrinsicMachine, StgIntrinsic},
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

/// `__EXPECT(actual, expected_repr, pass)` — unified expectation BIF.
///
/// Behaviour:
/// - `pass` is `true` → return `true` (success, no output).
/// - `pass` is `false` → emit a "EXPECT FAILED" diagnostic to stderr
///   with the expected representation and the actual value rendered
///   by `render_debug_repr`, then return `false`.
///
/// The `actual` argument is lazy (not forced by the wrapper) so that
/// unforced thunks render as `<unevaluated>` in failure messages
/// rather than triggering evaluation side-effects. The operator
/// implementations (`//=`, `//!`) already force `actual` via the
/// equality check passed as `pass`, so in practice `actual` is
/// usually already at WHNF when `__EXPECT` runs.
///
/// `expected_repr` and `pass` are strict.
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
        let pass = resolve_bool(machine, view, &args[2]);

        if pass {
            machine_return_bool(machine, view, true)
        } else {
            let expected_repr = str_arg(machine, view, &args[1])?;
            let actual_repr = render_debug_repr(machine, view, &args[0]);
            eprintln!("EXPECT FAILED: expected {expected_repr}, got {actual_repr}");
            machine_return_bool(machine, view, false)
        }
    }
}

impl CallGlobal3 for Expect {}
