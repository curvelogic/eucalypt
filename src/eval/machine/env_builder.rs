//! Helpers for constructing environments on the heap

use crate::{
    common::sourcemap::Smid,
    eval::{
        error::ExecutionError,
        memory::{
            alloc::ScopedAllocator,
            array::Array,
            infotable::InfoTable,
            mutator::MutatorHeapView,
            syntax::{HeapSyn, LambdaForm, Ref, RefPtr, StgBuilder},
        },
        stg::syntax::CaptureInstruction,
    },
};

use super::env::{CaptureEntry, EnvFrame, SynClosure};

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
        recipe: &[CaptureInstruction],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError>;

    /// "Allocate" let bindings in a new env
    fn from_letrec(
        &self,
        bindings: &[LambdaForm],
        recipe: &[CaptureInstruction],
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
            .alloc(EnvFrame::new(
                args,
                Array::default(),
                annotation,
                Some(next),
            ))?
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
        recipe: &[CaptureInstruction],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError> {
        let captures = resolve_captures(self, recipe, next);
        let closures = bindings.iter().map(|lf| SynClosure::close(lf, next));
        let mut array = Array::with_capacity(self, bindings.len());
        for c in closures {
            array.push(self, c)
        }

        Ok(self
            .alloc(EnvFrame::new(array, captures, annotation, Some(next)))?
            .as_ptr())
    }

    /// "Allocate" let bindings in a new env
    fn from_letrec(
        &self,
        bindings: &[LambdaForm],
        recipe: &[CaptureInstruction],
        next: RefPtr<EnvFrame>,
        annotation: Smid,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError> {
        let captures = resolve_captures(self, recipe, next);
        let mut array = Array::with_capacity(self, bindings.len());
        for _ in 0..bindings.len() {
            array.push(
                self,
                SynClosure::new(RefPtr::dangling(), RefPtr::dangling()),
            );
        }

        let frame = self
            .alloc(EnvFrame::new(
                array.clone(),
                captures,
                annotation,
                Some(next),
            ))?
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
        let mut array = Array::with_capacity(self, args.len());
        for syn in args.iter() {
            array.push(
                self,
                SynClosure::new(self.atom(syn.clone())?.as_ptr(), environment),
            );
        }

        Ok(array)
    }
}

/// Build a captures array from a capture recipe, reading from `enclosing`.
///
/// - `CaptureLocal(phys_idx)`: creates `CaptureEntry { frame: enclosing, index: phys_idx }`.
/// - `CopyCapture(cap_idx)`: copies `enclosing.captures[cap_idx]`.
///
/// Returns `Array::default()` if `recipe` is empty (backward compatible).
pub fn resolve_captures(
    view: &MutatorHeapView<'_>,
    recipe: &[CaptureInstruction],
    enclosing: RefPtr<EnvFrame>,
) -> Array<CaptureEntry> {
    if recipe.is_empty() {
        return Array::default();
    }
    let enc = (*view).scoped(enclosing);
    let mut caps = Array::with_capacity(view, recipe.len());
    for instr in recipe {
        let entry = match instr {
            CaptureInstruction::CaptureLocal(phys_idx) => CaptureEntry {
                frame: enclosing,
                index: *phys_idx,
            },
            CaptureInstruction::CopyCapture(cap_idx) => enc
                .get_capture_entry(*cap_idx as usize)
                .unwrap_or_else(|| panic!("CopyCapture index {} out of bounds", cap_idx)),
        };
        caps.push(view, entry);
    }
    caps
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

#[cfg(test)]
mod tests {
    use crate::{
        common::sourcemap::Smid,
        eval::{
            memory::{
                alloc::ScopedAllocator,
                heap::Heap,
                mutator::MutatorHeapView,
                syntax::{HeapSyn, Ref, RefPtr},
            },
            stg::syntax::CaptureInstruction,
        },
    };

    use super::{resolve_captures, CaptureEntry, EnvFrame, SynClosure};

    /// Create an atom closure pointing to `Ref::V(num)` and put it in a fresh
    /// env frame with a single slot, returning the frame pointer.
    fn make_frame_with_num(view: &MutatorHeapView<'_>, n: i64) -> RefPtr<EnvFrame> {
        let code = view
            .alloc(HeapSyn::Atom {
                evaluand: Ref::V(crate::eval::memory::syntax::Native::Num(
                    serde_json::Number::from(n),
                )),
            })
            .unwrap()
            .as_ptr();
        let dummy_env = view.alloc(EnvFrame::default()).unwrap().as_ptr();
        let closure = SynClosure::new(code, dummy_env);
        let mut arr = crate::eval::memory::array::Array::with_capacity(view, 1);
        arr.push(view, closure);
        let frame = view
            .alloc(EnvFrame::new(
                arr,
                crate::eval::memory::array::Array::default(),
                Smid::default(),
                None,
            ))
            .unwrap()
            .as_ptr();
        frame
    }

    #[test]
    fn test_capture_local_recipe() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        // Build a source frame with 2 locals (values 42 and 99).
        let code42 = view
            .alloc(HeapSyn::Atom {
                evaluand: Ref::V(crate::eval::memory::syntax::Native::Num(
                    serde_json::Number::from(42i64),
                )),
            })
            .unwrap()
            .as_ptr();
        let code99 = view
            .alloc(HeapSyn::Atom {
                evaluand: Ref::V(crate::eval::memory::syntax::Native::Num(
                    serde_json::Number::from(99i64),
                )),
            })
            .unwrap()
            .as_ptr();
        let dummy = view.alloc(EnvFrame::default()).unwrap().as_ptr();
        let c42 = SynClosure::new(code42, dummy);
        let c99 = SynClosure::new(code99, dummy);
        let mut bindings = crate::eval::memory::array::Array::with_capacity(&view, 2);
        bindings.push(&view, c42);
        bindings.push(&view, c99);
        let src_frame = view
            .alloc(EnvFrame::new(
                bindings,
                crate::eval::memory::array::Array::default(),
                Smid::default(),
                None,
            ))
            .unwrap()
            .as_ptr();

        // Recipe: capture local 1 (value 99), then local 0 (value 42).
        let recipe = vec![
            CaptureInstruction::CaptureLocal(1),
            CaptureInstruction::CaptureLocal(0),
        ];

        let caps = resolve_captures(&view, &recipe, src_frame);

        // Verify the captures array has 2 entries.
        assert_eq!(caps.len(), 2, "should have 2 capture entries");

        // Both entries should point to src_frame.
        let entry0 = caps.get(0).expect("capture entry 0");
        let entry1 = caps.get(1).expect("capture entry 1");
        assert_eq!(entry0.frame, src_frame);
        assert_eq!(entry0.index, 1);
        assert_eq!(entry1.frame, src_frame);
        assert_eq!(entry1.index, 0);
    }

    #[test]
    fn test_copy_capture_recipe() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        // Build a source frame with one local.
        let src_frame = make_frame_with_num(&view, 7);

        // Build a capture frame with one entry pointing to src_frame slot 0.
        let entry = CaptureEntry {
            frame: src_frame,
            index: 0,
        };
        let mut cap_arr = crate::eval::memory::array::Array::with_capacity(&view, 1);
        cap_arr.push(&view, entry);
        let cap_frame = view
            .alloc(EnvFrame::new(
                crate::eval::memory::array::Array::default(),
                cap_arr,
                Smid::default(),
                None,
            ))
            .unwrap()
            .as_ptr();

        // Recipe: copy capture 0 from cap_frame.
        let recipe = vec![CaptureInstruction::CopyCapture(0)];
        let caps = resolve_captures(&view, &recipe, cap_frame);

        assert_eq!(caps.len(), 1);
        let copied = caps.get(0).expect("copied entry");
        // Should have copied the original entry (pointing to src_frame slot 0).
        assert_eq!(copied.frame, src_frame);
        assert_eq!(copied.index, 0);
    }

    #[test]
    fn test_get_local_direct() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        let frame = make_frame_with_num(&view, 55);
        let env = view.scoped(frame);
        // get_local(0) should return the closure at physical index 0.
        let got = env.get_local(0);
        assert!(got.is_some(), "get_local(0) should succeed");
        // get_local(1) should return None (frame has only 1 slot).
        let none = env.get_local(1);
        assert!(none.is_none(), "get_local(1) should be None");
    }

    #[test]
    fn test_get_capture() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        // Build source frame with 1 slot.
        let src_frame = make_frame_with_num(&view, 123);

        // Build capture frame pointing to src_frame slot 0.
        let entry = CaptureEntry {
            frame: src_frame,
            index: 0,
        };
        let mut cap_arr = crate::eval::memory::array::Array::with_capacity(&view, 1);
        cap_arr.push(&view, entry);
        let cap_frame = view
            .alloc(EnvFrame::new(
                crate::eval::memory::array::Array::default(),
                cap_arr,
                Smid::default(),
                None,
            ))
            .unwrap()
            .as_ptr();

        let cap_env = view.scoped(cap_frame);
        // get_capture(0) should return the closure from src_frame slot 0.
        let got = cap_env.get_capture(&view, 0);
        assert!(got.is_some(), "get_capture(0) should succeed");
        // get_capture(1) should return None.
        let none = cap_env.get_capture(&view, 1);
        assert!(none.is_none(), "get_capture(1) should be None");
    }
}
