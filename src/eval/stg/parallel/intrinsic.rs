//! The `__PARMAP` intrinsic — the sole parallel primitive.
//!
//! The reduction combinators (`par-sum`/`par-max`/`par-min`/`par-concat`) are
//! prelude wrappers that pipe `par-map` into the exact sequential prelude
//! reducer, so they need no dedicated intrinsic and are byte-for-byte identical
//! to their sequential forms by construction.

use std::convert::TryInto;

use crate::common::sourcemap::Smid;
use crate::eval::{
    error::ExecutionError,
    machine::intrinsic::{CallGlobal1, CallGlobal3, IntrinsicMachine, StgIntrinsic},
    memory::{mutator::MutatorHeapView, syntax::Ref},
    stg::{
        force::SeqSpine,
        support::sym_arg,
        syntax::{
            dsl::{app_bif, force, lambda, local, lref, unbox_sym},
            LambdaForm,
        },
    },
};

use crate::eval::emit::Emitter;

/// `__PARMAP(name, f, xs)` — parallel map, semantically identical to
/// `xs map(f)`, order-preserving, with a transparent sequential fallback.
///
/// `name` is the surface combinator the user wrote (`:par-map`, `:par-sum`,
/// …). Since the reductions are prelude wrappers over this one primitive, it
/// is the only way a boundary error can name the combinator actually called
/// rather than always saying `par-map`.
///
/// `name` (arg 0) and `xs` (arg 2) are strict, so the wrapper forces the
/// symbol and `xs`'s SPINE to WHNF before the intrinsic runs — the custom
/// `wrapper()` below, not the default arg-strictness wrapper `wrap()`
/// would generate, because `xs` needs its whole spine walked (via
/// [`SeqSpine`]), not just its head cons cell, for the reasons documented on
/// `SeqSpine` itself.
pub struct ParMap;

impl StgIntrinsic for ParMap {
    fn name(&self) -> &str {
        "PARMAP"
    }

    /// Hand-written in place of the default `wrap()`-generated wrapper
    /// (which only forces `xs` to WHNF at its head cons cell) so that
    /// `xs`'s entire spine is forced via the compile-time `force`/`Case`
    /// DSL — the same Update-continuation mechanism ordinary lazy variable
    /// forcing uses everywhere, and the only way to memoise a single-pass
    /// producer's binding correctly (see `SeqSpine`'s doc comment). This
    /// replaces `lib/prelude.eu`'s former `force-spine` helper, which did
    /// the same walk from ordinary (non-wrapper) compiled code — the
    /// mechanism is the same; only where it is woven in changes.
    ///
    /// Mirrors the shape `wrap()` would generate for `PARMAP`'s declared
    /// strict args `[0, 2]` (arg 0 is a symbol, needing unbox + force; arg 2
    /// is the list): unbox+force `name`, then force `xs`'s spine via
    /// `SeqSpine`, then call the intrinsic with the ORIGINAL `xs` reference
    /// (now fully forced in place by `SeqSpine`'s side effect, not a fresh
    /// value bound by the force) — `f` (arg 1) is untouched, per its
    /// non-strict declaration.
    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            3,
            unbox_sym(
                local(0), // name (arg 0); env after match: [unboxed-name-thunk, name, f, xs]
                force(
                    local(0), // force the unboxed symbol field
                    // env now: [forced-name, unboxed-name-thunk, name, f, xs]
                    force(
                        SeqSpine.global(lref(4)), // force xs's spine; xs = lref(4) here
                        // env now: [seqspine-result(discarded), forced-name,
                        //           unboxed-name-thunk, name, f, xs]
                        app_bif(
                            self.index().try_into().unwrap(),
                            vec![lref(1), lref(4), lref(5)],
                        ),
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
        // args = [name, f, xs]
        let combinator = sym_arg(machine, view, &args[0])?;
        super::par_map(machine, view, &args[1], &args[2], &combinator)
    }
}

impl CallGlobal3 for ParMap {}
