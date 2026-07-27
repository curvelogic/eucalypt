//! Helpers for constructing environments on the heap
//!
//! # Settled-slot pass-through (eu-wpswc)
//!
//! When an application's argument is a `Ref::L(i)`, the default is to wrap it
//! in a freshly allocated `Atom{Ref::L(i)}` alias closure over the caller's
//! frame.  The alias exists for exactly one reason: **memoisation**.  If slot
//! `i` holds an update thunk, the callee must force it *through the caller's
//! slot* so that the resulting WHNF is written back there and every other
//! reader of that slot shares the work.
//!
//! A slot whose closure can never change needs no such indirection, and
//! aliasing it is actively harmful.  Each recursive call re-wraps the
//! previous level's wrapper, and because alias closures are built with
//! `SynClosure::new` (`update() == false`) the `Atom{Ref::L}` handler in
//! `vm.rs` pushes no `Update` continuation for them — nothing ever collapses
//! the chain.  A function parameter threaded through `N` levels of recursion
//! therefore sits behind `O(N)` alias hops, and forcing every element of the
//! result costs `O(N²)`.  That is the `xs map(f) sum` quadratic.
//!
//! So: pass the closure through directly when the slot is **settled**, and
//! keep the alias when it is not.  A closure is settled when it is not an
//! update thunk *and* is one of exactly two immutable shapes:
//!
//! * **arity > 0** — a lambda or a PAP.  It is never forced, never updated
//!   and never a black hole (a black hole has arity 0), so a callee holding
//!   it by value applies it exactly as it would through an alias.  Applying
//!   the same PAP from two sites builds two independent saturated frames
//!   either way — the PAP closure itself is immutable and is not the sharing
//!   point for its supplied arguments; those live in the PAP's own env frame,
//!   which both applications continue to share by pointer.
//! * **an `Atom` node** — a pure indirection or an inline value, which is
//!   what an alias closure itself is.  Re-aliasing an alias is precisely how
//!   the chain grows, and passing it on instead keeps the chain at length
//!   one.  Nothing is duplicated: an alias onto a thunk elsewhere still
//!   navigates to *that* owning frame and updates *that* slot when forced,
//!   exactly as it does today.
//!
//! Everything else keeps its alias, so its representation is bit-identical
//! to before.  That deliberately includes evaluated data and `Value`-form
//! closures: they are immutable too, and passing them through is sound for
//! *evaluation*, but the IO-run driver's static spec-block navigator
//! (`driver::io_run::peel_meta` / `block_list_inner`, and the bytecode twin
//! `BytecodeMachine::block_field_values`) walks argument structure on the
//! assumption that it can chase an `Atom` indirection and record the
//! container frame it came from.  Handing it a bare `Cons` instead breaks
//! that walk — measured: every `io.*` harness test fails.  Widening the
//! predicate to all non-updateable closures is therefore blocked on making
//! that navigator independent of argument aliasing; see eu-wpswc's
//! follow-up.
//!
//! Black holes need no explicit exclusion under this predicate: a black hole
//! is `HeapSyn::BlackHole` with arity 0, so it matches neither shape.
//!
//! This subsumes what `create_arg_array_eager` does at self-recursive call
//! sites (eu-e3c3i, commit 6a902030) for the settled case, and is safe where
//! that unconditional eager resolution is not: `create_arg_array_eager`
//! copies update thunks too, which loses the caller's slot as the shared
//! memoisation point.  The `eager_args` path is left exactly as it was.
//!
//! The bytecode engine's two argument-array builders
//! (`bytecode::machine::make_arg_array` and `make_arg_array_pd`) implement
//! the identical predicate; the three must not drift.

use crate::{
    common::sourcemap::Smid,
    eval::{
        error::ExecutionError,
        memory::{
            alloc::{ScopedAllocator, ScopedPtr},
            array::Array,
            infotable::InfoTable,
            mutator::MutatorHeapView,
            syntax::{HeapSyn, LambdaForm, Ref, RefPtr, StgBuilder},
        },
    },
};

use super::env::{EnvFrame, SynClosure};

/// Whether an environment slot's closure can never change, and so may be
/// passed to a callee by value instead of behind an `Atom{Ref::L}` alias.
///
/// See the module doc comment for the full argument.  Must stay in lockstep
/// with `bytecode::machine::bc_is_settled`.
#[inline]
fn is_settled(view: MutatorHeapView<'_>, closure: &SynClosure) -> bool {
    if closure.update() {
        return false;
    }
    // A lambda or PAP: immutable, and never a black hole, so skip the load.
    if closure.arity() > 0 {
        return true;
    }
    // An alias/inline-value `Atom` node: a pure indirection.
    let code = ScopedPtr::from_non_null(&view, closure.code());
    matches!(&*code, HeapSyn::Atom { .. })
}

/// For building environments in the heap
/// All operations now return Result to handle allocation failures gracefully.
#[allow(clippy::wrong_self_convention)]
pub trait EnvBuilder {
    fn from_saturation(
        &self,
        args: Array<SynClosure>,
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError>;

    fn from_args(
        &self,
        args: &[Ref],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError>;

    fn from_closure(
        &self,
        closure: SynClosure,
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError>;

    fn from_closures<I: Iterator<Item = SynClosure>>(
        &self,
        closures: I,
        len: usize,
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError>;

    /// "Allocate" let bindings in a new env
    fn from_let(
        &self,
        bindings: &[LambdaForm],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError>;

    /// "Allocate" let bindings in a new env
    fn from_letrec(
        &self,
        bindings: &[LambdaForm],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError>;

    /// Create a saturated version of a closure ready for entry
    fn saturate(
        &self,
        closure: &SynClosure,
        args: &[SynClosure],
    ) -> Result<SynClosure, ExecutionError>;

    /// Create a saturated version of a closure, consuming an existing
    /// args array without copying it.  Use this when the caller
    /// already owns the exact `Array<SynClosure>` to avoid an
    /// unnecessary heap allocation.
    fn saturate_with_array(
        &self,
        closure: &SynClosure,
        args: Array<SynClosure>,
    ) -> Result<SynClosure, ExecutionError>;

    /// Create a new closure with extra partial arguments
    fn partially_apply(
        &self,
        closure: &SynClosure,
        args: &[SynClosure],
    ) -> Result<SynClosure, ExecutionError>;

    /// Create array of atom closures from refs
    fn create_arg_array(
        &self,
        args: &[Ref],
        environment: RefPtr<EnvFrame>,
    ) -> Result<Array<SynClosure>, ExecutionError>;

    /// Like `create_arg_array` but resolves `Ref::L` args eagerly by
    /// looking up the closure from the environment instead of creating
    /// a lazy `Atom{Ref::L}` indirection.  Used at self-recursive call
    /// sites to prevent O(n) indirection chain build-up.
    fn create_arg_array_eager(
        &self,
        args: &[Ref],
        environment: RefPtr<EnvFrame>,
    ) -> Result<Array<SynClosure>, ExecutionError>;
}

impl EnvBuilder for MutatorHeapView<'_> {
    /// Allocate an env frame for a set of bindings coming from an
    /// argument list
    fn from_saturation(
        &self,
        args: Array<SynClosure>,
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError> {
        Ok(self
            .alloc(EnvFrame::new(args, annotation, Some(next)))?
            .as_ptr())
    }

    /// From data constructor or lambda args
    fn from_args(
        &self,
        args: &[Ref],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError> {
        let mut array = Array::with_capacity(self, args.len());
        for r in args {
            let atom_ptr = self
                .alloc(HeapSyn::Atom {
                    evaluand: r.clone(),
                })?
                .as_ptr();
            array.push(self, SynClosure::new(atom_ptr, next))
        }

        self.from_saturation(array, next, annotation)
    }

    /// From single closure (creating a scope with a single item
    /// especially fallback clauses in case / demeta)
    fn from_closure(
        &self,
        closure: SynClosure,
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError> {
        let mut array = Array::with_capacity(self, 1);
        array.push(self, closure);

        self.from_saturation(array, next, annotation)
    }

    /// From closures
    fn from_closures<I: Iterator<Item = SynClosure>>(
        &self,
        closures: I,
        len: usize,
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError> {
        let mut array = Array::with_capacity(self, len);
        for c in closures {
            array.push(self, c)
        }

        self.from_saturation(array, next, annotation)
    }

    /// "Allocate" let bindings in a new env
    fn from_let(
        &self,
        bindings: &[LambdaForm],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError> {
        let closures = bindings.iter().map(|lf| SynClosure::close(lf, next));
        self.from_closures(closures, bindings.len(), next, annotation)
    }

    /// "Allocate" let bindings in a new env
    fn from_letrec(
        &self,
        bindings: &[LambdaForm],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError> {
        let mut array = Array::with_capacity(self, bindings.len());
        for _ in 0..bindings.len() {
            array.push(
                self,
                SynClosure::new(RefPtr::dangling(), RefPtr::dangling()),
            );
        }

        let frame = self
            .alloc(EnvFrame::new(array.clone(), annotation, Some(next)))?
            .as_ptr();

        for (i, pc) in bindings.iter().enumerate() {
            // SAFETY: We pre-allocated array with bindings.len() capacity and i < bindings.len()
            unsafe {
                array.set_unchecked(i, SynClosure::close(pc, frame));
            }
        }

        Ok(frame)
    }

    /// Create a new saturated closure ready for call
    fn saturate(
        &self,
        closure: &SynClosure,
        args: &[SynClosure],
    ) -> Result<SynClosure, ExecutionError> {
        let arg_array: Array<SynClosure> = Array::from_slice(self, args);
        Ok(SynClosure::new_annotated(
            closure.code(),
            self.from_saturation(arg_array, closure.env(), closure.annotation())?,
            closure.annotation(),
        ))
    }

    /// Create a new saturated closure, consuming the args array directly
    /// to avoid an extra heap allocation when the caller already owns it.
    fn saturate_with_array(
        &self,
        closure: &SynClosure,
        args: Array<SynClosure>,
    ) -> Result<SynClosure, ExecutionError> {
        Ok(SynClosure::new_annotated(
            closure.code(),
            self.from_saturation(args, closure.env(), closure.annotation())?,
            closure.annotation(),
        ))
    }

    /// Create a new closure with extra partial arguments available in
    /// an env frame
    fn partially_apply(
        &self,
        closure: &SynClosure,
        args: &[SynClosure],
    ) -> Result<SynClosure, ExecutionError> {
        let arity = closure.arity() - (args.len() as u8);
        let env = self.from_closures(
            std::iter::once(closure.clone()).chain(args.iter().cloned()),
            args.len() + 1,
            closure.env(),
            closure.annotation(),
        );
        let syn = pap_syn(*self, args.len(), arity.into())?;
        Ok(SynClosure::new_annotated_lambda(
            syn,
            arity,
            env?,
            closure.annotation(),
        ))
    }

    /// Create an array of argument closures from refs to build apply call
    fn create_arg_array(
        &self,
        args: &[Ref],
        environment: RefPtr<EnvFrame>,
    ) -> Result<Array<SynClosure>, ExecutionError> {
        // SAFETY: environment is a valid heap pointer kept alive by the
        // current mutator scope.  We only read through it.
        let env = unsafe { environment.as_ref() };
        let mut array = Array::with_capacity(self, args.len());
        for syn in args.iter() {
            // Settled-slot pass-through (eu-wpswc).  See the module doc
            // comment: a `Ref::L(i)` naming a slot whose closure can never
            // change is passed straight through instead of being wrapped in
            // a fresh alias, which is what stops per-iteration alias chains
            // forming on lazily-threaded function parameters.
            if let Ref::L(i) = syn {
                if let Some(c) = env.get(self, *i) {
                    if is_settled(*self, &c) {
                        array.push(self, c);
                        continue;
                    }
                }
            }
            array.push(
                self,
                SynClosure::new(self.atom(syn.clone())?.as_ptr(), environment),
            );
        }

        Ok(array)
    }

    fn create_arg_array_eager(
        &self,
        args: &[Ref],
        environment: RefPtr<EnvFrame>,
    ) -> Result<Array<SynClosure>, ExecutionError> {
        // SAFETY: environment is a valid heap pointer kept alive by the
        // current mutator scope.  We only read through it.
        let env = unsafe { environment.as_ref() };
        let mut array = Array::with_capacity(self, args.len());
        for syn in args.iter() {
            let closure = match syn {
                Ref::L(i) => env
                    .get(self, *i)
                    .ok_or(ExecutionError::BadEnvironmentIndex(*i))?,
                _ => SynClosure::new(self.atom(syn.clone())?.as_ptr(), environment),
            };
            array.push(self, closure);
        }
        Ok(array)
    }
}

/// Return the code of a closure which acts as the partial application
/// of f to xs where the top frame in its environment is f:xs and it
/// expects pending to be passed as arguments.
fn pap_syn(
    view: MutatorHeapView,
    supplied: usize,
    pending: usize,
) -> Result<RefPtr<HeapSyn>, ExecutionError> {
    let mut arg_array = Array::with_capacity(&view, supplied + pending);
    for i in 0..supplied {
        arg_array.push(&view, Ref::L(pending + i + 1));
    }
    for i in 0..pending {
        arg_array.push(&view, Ref::L(i));
    }
    Ok(view.app(Ref::L(pending), arg_array)?.as_ptr())
}
