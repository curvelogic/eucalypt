//! Helpers for constructing bytecode environment frames on the heap.
//!
//! The `BcClosure` analogue of `machine::env_builder::EnvBuilder`. The
//! env-frame layout and cactus stack are shared with the HeapSyn engine
//! (spec §3), so these builders only differ in the closure code type
//! (`CodeRef` rather than `RefPtr<HeapSyn>`).
//!
//! Phase 1 provides the **code-synthesis-free** builders — the ones that
//! wrap already-built `BcClosure`s into env frames and reuse a callee's
//! `CodeRef` with a fresh env (`saturate*`). The builders that need
//! runtime code synthesis — `create_arg_array`/`_eager` (a per-arg
//! `OP_ATOM`) and `partially_apply` (a pap trampoline) — plus
//! `from_let`/`from_letrec` (which `close` over encoded form headers) are
//! added in Phase 2, once the arg-atom / pap encoding strategy is fixed
//! (plan Task 2.x).

use crate::common::sourcemap::Smid;
use crate::eval::error::ExecutionError;
use crate::eval::machine::env::EnvironmentFrame;
use crate::eval::memory::{
    alloc::ScopedAllocator, array::Array, infotable::InfoTable, mutator::MutatorHeapView,
    syntax::RefPtr,
};

use super::BcClosure;

/// Environment frame type for the bytecode engine.
pub type BcEnvFrame = EnvironmentFrame<BcClosure>;

/// Builds bytecode environment frames and saturated closures.
#[allow(clippy::wrong_self_convention)]
pub trait BcEnvBuilder {
    /// Allocate an env frame from an already-built argument array.
    fn from_saturation(
        &self,
        args: Array<BcClosure>,
        next: RefPtr<BcEnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<BcEnvFrame>, ExecutionError>;

    /// Allocate a single-binding env frame (e.g. case/demeta fallbacks).
    fn from_closure(
        &self,
        closure: BcClosure,
        next: RefPtr<BcEnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<BcEnvFrame>, ExecutionError>;

    /// Allocate an env frame from an iterator of closures.
    fn from_closures<I: Iterator<Item = BcClosure>>(
        &self,
        closures: I,
        len: usize,
        next: RefPtr<BcEnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<BcEnvFrame>, ExecutionError>;

    /// Create a saturated closure ready for entry, copying `args`.
    fn saturate(
        &self,
        closure: &BcClosure,
        args: &[BcClosure],
    ) -> Result<BcClosure, ExecutionError>;

    /// Create a saturated closure, consuming an existing args array
    /// without copying it.
    fn saturate_with_array(
        &self,
        closure: &BcClosure,
        args: Array<BcClosure>,
    ) -> Result<BcClosure, ExecutionError>;
}

impl BcEnvBuilder for MutatorHeapView<'_> {
    fn from_saturation(
        &self,
        args: Array<BcClosure>,
        next: RefPtr<BcEnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<BcEnvFrame>, ExecutionError> {
        Ok(self
            .alloc(BcEnvFrame::new(args, annotation, Some(next)))?
            .as_ptr())
    }

    fn from_closure(
        &self,
        closure: BcClosure,
        next: RefPtr<BcEnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<BcEnvFrame>, ExecutionError> {
        let mut array = Array::with_capacity(self, 1);
        array.push(self, closure);
        self.from_saturation(array, next, annotation)
    }

    fn from_closures<I: Iterator<Item = BcClosure>>(
        &self,
        closures: I,
        len: usize,
        next: RefPtr<BcEnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<BcEnvFrame>, ExecutionError> {
        let mut array = Array::with_capacity(self, len);
        for c in closures {
            array.push(self, c);
        }
        self.from_saturation(array, next, annotation)
    }

    fn saturate(
        &self,
        closure: &BcClosure,
        args: &[BcClosure],
    ) -> Result<BcClosure, ExecutionError> {
        let arg_array: Array<BcClosure> = Array::from_slice(self, args);
        Ok(BcClosure::new_annotated(
            closure.code(),
            self.from_saturation(arg_array, closure.env(), closure.annotation())?,
            closure.annotation(),
        ))
    }

    fn saturate_with_array(
        &self,
        closure: &BcClosure,
        args: Array<BcClosure>,
    ) -> Result<BcClosure, ExecutionError> {
        Ok(BcClosure::new_annotated(
            closure.code(),
            self.from_saturation(args, closure.env(), closure.annotation())?,
            closure.annotation(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::memory::heap::Heap;

    #[test]
    fn saturate_reuses_code_with_new_env() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        // Root frame, and a callee env carrying a sentinel binding.
        let root_env = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let sentinel = BcClosure::new(7777u32, root_env);
        let callee_env = view
            .from_saturation(view.array(&[sentinel]), root_env, Smid::default())
            .unwrap();

        // Callee: arity 1, code offset 42, closed over callee_env.
        let callee = BcClosure::new_annotated_lambda(42u32, 1, callee_env, Smid::default());

        // Saturate with a single arg closure (code offset 55).
        let arg = BcClosure::new(55u32, root_env);
        let result = view
            .saturate_with_array(&callee, view.array(&[arg]))
            .unwrap();

        // Code is reused; the env is a fresh frame.
        assert_eq!(result.code(), 42);
        assert_ne!(result.env(), callee.env());

        // The fresh top frame binds the arg at index 0 …
        let frame = view.scoped(result.env());
        assert_eq!(frame.get(&view, 0).unwrap().code(), 55);
        // … and chains onto the callee env (its sentinel is reachable next).
        assert_eq!(frame.get(&view, 1).unwrap().code(), 7777);
    }
}
