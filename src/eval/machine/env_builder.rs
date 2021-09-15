//! Helpers for constructing environments on the heap

use crate::{
    common::sourcemap::Smid,
    eval::{
        error::ExecutionError,
        memory::{
            alloc::ScopedAllocator,
            array::Array,
            mutator::MutatorHeapView,
            syntax::{HeapSyn, LambdaForm, Ref, RefPtr, StgBuilder},
        },
    },
};

use super::env::{Closure, EnvFrame};

/// For building environments in the heap
pub trait EnvBuilder {
    fn from_saturation(
        &self,
        args: Array<RefPtr<Closure>>,
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> RefPtr<EnvFrame>;

    fn from_args(&self, args: &[Ref], next: RefPtr<EnvFrame>, annotation: Smid)
        -> RefPtr<EnvFrame>;

    fn from_closure(
        &self,
        closure: RefPtr<Closure>,
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> RefPtr<EnvFrame>;

    fn from_closures<I: Iterator<Item = RefPtr<Closure>>>(
        &self,
        closures: I,
        len: usize,
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> RefPtr<EnvFrame>;

    /// "Allocate" let bindings in a new env
    fn from_let(
        &self,
        bindings: &[LambdaForm],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> RefPtr<EnvFrame>;

    /// "Allocate" let bindings in a new env
    fn from_letrec(
        &self,
        bindings: &[LambdaForm],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> RefPtr<EnvFrame>;

    /// Create a new closure from code and environment
    fn new_closure(
        &self,
        code: RefPtr<HeapSyn>,
        env: RefPtr<EnvFrame>,
    ) -> Result<RefPtr<Closure>, ExecutionError>;

    /// Create a saturated version of a closure ready for entry
    fn saturate(
        &self,
        closure: &Closure,
        args: &[RefPtr<Closure>],
    ) -> Result<RefPtr<Closure>, ExecutionError>;

    /// Create a new closure with extra partial arguments
    fn partially_apply(
        &self,
        closure: &Closure,
        args: &[RefPtr<Closure>],
    ) -> Result<RefPtr<Closure>, ExecutionError>;

    /// Create array of atom closures from refs
    fn create_arg_array(
        &self,
        args: &[Ref],
        environment: RefPtr<EnvFrame>,
    ) -> Result<Array<RefPtr<Closure>>, ExecutionError>;
}

impl<'scope> EnvBuilder for MutatorHeapView<'scope> {
    /// Allocate an env frame for a set of bindings coming from an
    /// argument list
    fn from_saturation(
        &self,
        args: Array<RefPtr<Closure>>,
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> RefPtr<EnvFrame> {
        self.alloc(EnvFrame::new(args, annotation, Some(next.clone())))
            .expect("failed to allocate env frame from saturation")
            .as_ptr()
    }

    /// From data constructor or lambda args
    fn from_args(
        &self,
        args: &[Ref],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> RefPtr<EnvFrame> {
        let mut array = Array::with_capacity(self, args.len());
        for r in args {
            array.push(
                self,
                self.alloc(Closure::new(
                    self.alloc(HeapSyn::Atom {
                        evaluand: r.clone(),
                    })
                    .expect("allocation failure")
                    .as_ptr(),
                    next,
                ))
                .expect("allocation failure")
                .as_ptr(),
            )
        }

        self.from_saturation(array, next, annotation)
    }

    /// From single closure (creating a scope with a single item
    /// especially fallback clauses in case / demeta)
    fn from_closure(
        &self,
        closure: RefPtr<Closure>,
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> RefPtr<EnvFrame> {
        let mut array = Array::with_capacity(self, 1);
        array.push(self, closure);

        self.from_saturation(array, next, annotation)
    }

    /// From closures
    fn from_closures<I: Iterator<Item = RefPtr<Closure>>>(
        &self,
        closures: I,
        len: usize,
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> RefPtr<EnvFrame> {
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
    ) -> RefPtr<EnvFrame> {
        let closures = bindings.iter().map(|lf| {
            self.alloc(Closure::close(lf, next))
                .expect("allocation failure")
                .as_ptr()
        });
        self.from_closures(closures, bindings.len(), next, annotation)
    }

    /// "Allocate" let bindings in a new env
    fn from_letrec(
        &self,
        bindings: &[LambdaForm],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> RefPtr<EnvFrame> {
        let mut array = Array::with_capacity(self, bindings.len());
        for _ in 0..bindings.len() {
            array.push(self, RefPtr::dangling());
        }

        let frame = self
            .alloc(EnvFrame::new(array.clone(), annotation, Some(next)))
            .expect("allocation failure")
            .as_ptr();

        for (i, pc) in bindings.iter().enumerate() {
            array.set(
                i,
                self.alloc(Closure::close(pc, frame))
                    .expect("allocation failure")
                    .as_ptr(),
            )
        }

        frame
    }

    /// Create a new closure from code and environment
    fn new_closure(
        &self,
        code: RefPtr<HeapSyn>,
        env: RefPtr<EnvFrame>,
    ) -> Result<RefPtr<Closure>, ExecutionError> {
        Ok(self.alloc(Closure::new(code, env))?.as_ptr())
    }

    /// Create a new saturated closure ready for call
    fn saturate(
        &self,
        closure: &Closure,
        args: &[RefPtr<Closure>],
    ) -> Result<RefPtr<Closure>, ExecutionError> {
        let mut all_args: Array<RefPtr<Closure>> = Array::default();
        for c in closure.pap_args().iter() {
            all_args.push(self, *c);
        }
        for a in args {
            all_args.push(self, *a);
        }
        Ok(self
            .alloc(Closure::new_annotated(
                closure.code(),
                self.from_saturation(all_args, closure.env(), closure.annotation()),
                closure.annotation(),
            ))?
            .as_ptr())
    }

    /// Create a new closure with extra partial arguments
    fn partially_apply(
        &self,
        closure: &Closure,
        args: &[RefPtr<Closure>],
    ) -> Result<RefPtr<Closure>, ExecutionError> {
        let mut pap_args = Array::with_capacity(self, closure.pap_args().len() + args.len());
        for a in closure.pap_args().iter() {
            pap_args.push(self, *a)
        }
        for a in args {
            pap_args.push(self, *a)
        }
        Ok(self.alloc(closure.with_pap_args(pap_args))?.as_ptr())
    }

    /// Create an array of argument closures from refs to build apply call
    fn create_arg_array(
        &self,
        args: &[Ref],
        environment: RefPtr<EnvFrame>,
    ) -> Result<Array<RefPtr<Closure>>, ExecutionError> {
        let mut array = Array::with_capacity(self, args.len());
        for syn in args.iter() {
            array.push(
                self,
                self.alloc(Closure::new(self.atom(syn.clone())?.as_ptr(), environment))?
                    .as_ptr(),
            );
        }

        Ok(array)
    }
}
