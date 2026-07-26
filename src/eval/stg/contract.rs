//! The `CONTRACT_FAIL` intrinsic — raises a structural-contract violation.
//!
//! `lib/contract.eu`'s `ensure` renders each violation to a line and calls
//! `__CONTRACT_FAIL(headline, lines)`.
//!
//! The marshalling boundary between the eucalypt report and the Rust error
//! variant is deliberately **strings** (SV3 design §7.3): the report's
//! *presentation* then lives in eucalypt, where it is readable, testable by
//! harness test, and changeable without touching Rust, and the Rust side
//! stays a dumb carrier. Nothing structured is lost — the structured report
//! is exactly what `validate` returns, and a caller who wants structure
//! calls `validate` instead of `ensure`.

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
    },
};

use super::{
    force::SeqStrList,
    support::{call, str_arg, str_list_arg},
    syntax::{
        dsl::{force, lambda, local, lref, unbox_str},
        LambdaForm,
    },
};

/// `CONTRACT_FAIL(headline, lines)`
///
/// Always raises `ExecutionError::ContractViolation`.
pub struct ContractFail;

impl StgIntrinsic for ContractFail {
    fn name(&self) -> &str {
        "CONTRACT_FAIL"
    }

    /// Force and unbox both arguments before entering `execute`.
    ///
    /// `SeqStrList` walks and unboxes the list spine — the established
    /// pattern for an intrinsic taking a list of strings (`string::Join`).
    /// The headline is then **unboxed and forced**: a box is a non-strict
    /// data structure that can hold a thunk, so unboxing alone does not
    /// satisfy a strict intrinsic — see the note in `stg::wrap`, whose
    /// generated wrappers do exactly the same unbox-then-force pair. The
    /// headline is built by string interpolation, which is a suspended
    /// intrinsic call, so skipping that force fails on any computed value
    /// (and silently works for a string literal).
    ///
    /// Each `force`/`unbox` adds one environment layer, so with
    /// `[headline lines]` bound the innermost body sees
    /// `[forced] [unboxed] [seqlines] [headline lines]`.
    ///
    /// A plain `lambda`, deliberately, not `annotated_lambda`: an annotated
    /// wrapper overwrites the machine's transient annotation with a synthetic
    /// Smid, and the `ensure` call site in the user's own file is precisely
    /// what this error wants to blame.
    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            2, // [headline lines]
            force(
                SeqStrList.global(lref(1)),
                // [seqlines] [headline lines]
                unbox_str(
                    local(1),
                    // [unboxed] [seqlines] [headline lines]
                    force(
                        local(0),
                        // [forced] [unboxed] [seqlines] [headline lines]
                        call::bif::contract_fail(lref(0), lref(2)),
                    ),
                ),
            ),
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let headline = str_arg(machine, view, &args[0])?;
        let violations = str_list_arg(machine, view, args[1].clone())?;
        // Deliberately `Smid::default()` rather than `machine.annotation()`.
        //
        // The wrapper has just forced the headline, which is a string built
        // by interpolation inside `lib/contract.eu` and `lib/reflect.eu`, so
        // by the time `execute` runs the machine's transient annotation
        // points at a string literal in a library — a location the user
        // cannot act on.
        //
        // The `SourceMap`'s resource-file mechanism, which exists precisely
        // to keep such a location from becoming the primary label, is
        // **bypassed** rather than absent (eu-8a49h): `mark_resource_file`
        // fires only for `Locator::Resource`, and `{ import: "contract.eu" }`
        // yields `Locator::Fs` — only an explicit `resource:` prefix produces
        // `Locator::Resource` (`src/syntax/input.rs`). `read_fs_input` then
        // falls back to the baked-in resource *text* while the locator stays
        // `Fs`, so the file is never marked and `is_user_file` reports true
        // for it. Until eu-8a49h is fixed, the machine annotation would
        // therefore win `to_diagnostic`'s "prefer a user file" test and put
        // a library string literal in the primary label.
        //
        // Leaving the Smid empty hands the choice to `to_diagnostic`'s trace
        // search, which looks for the nearest user-file frame — the `ensure`
        // call site, which is what SV3 §9.1 asks this error to blame.
        //
        // **This is necessary but not yet sufficient, and §9.1 is therefore
        // not met in blob mode.** Emptying this Smid removes the *error's own*
        // location from contention, but the trace search still ranks a library
        // frame as a user frame for the same eu-8a49h reason, so whichever
        // library frame happens to sit at the head of the stack trace can win.
        // In source-prelude mode that frame is a properly-marked *prelude*
        // frame and blame lands correctly; in blob mode it is a
        // `lib/contract.eu` frame and blame moves into the library. eu-8a49h
        // is the fix; a primary-label assertion is deliberately absent from
        // `196_sv3_contract_violation.eu.expect` until it lands, because such
        // an assertion would fail today rather than gate anything.
        //
        // When eu-8a49h lands: add that assertion, confirm it passes in *both*
        // prelude modes, and only then consider whether `machine.annotation()`
        // can be restored here.
        Err(ExecutionError::ContractViolation(
            Smid::default(),
            Box::new((headline, violations)),
        ))
    }
}

impl CallGlobal2 for ContractFail {}
