//! Bytecode value model (spec §3, revised 2026-07-02).
//!
//! A bytecode runtime value is either a code closure or a WHNF native:
//!
//! ```text
//! BcValue = Closure(BcClosure) | Native(Native)
//! BcClosure = { InfoTagged<CodeRef>, env: RefPtr<EnvironmentFrame<BcValue>> }
//! ```
//!
//! Unlike the HeapSyn engine — where a native value hides inside a
//! heap-allocated `Atom{V(native)}` *code* node — a bytecode native has no
//! code to live in (a `CodeRef` is an inert `u32` offset). So env slots,
//! continuations and the machine's current value range over `BcValue`, and
//! natives are carried directly.
//!
//! GC: a `BcClosure` scans only its env pointer (the code offset never
//! moves); a `BcValue::Native` marks only the heap-pointer natives
//! (`Str`/`Set`/`Vec`/`NdArray`). Code stays off the scan set (spec §7).

use crate::common::sourcemap::Smid;
use crate::eval::machine::env::EnvironmentFrame;
use crate::eval::memory::alloc::StgObject;
use crate::eval::memory::collect::{CollectorHeapView, CollectorScope, GcScannable, ScanPtr};
use crate::eval::memory::infotable::{InfoTable, InfoTagged};
use crate::eval::memory::syntax::{Native, RefPtr};

use super::CodeRef;

/// Environment frame for the bytecode engine: its slots are `BcValue`s.
pub type BcEnvFrame = EnvironmentFrame<BcValue>;

/// A code closure: static part (code offset + arity/update/annotation)
/// plus a pointer to its environment.
#[derive(Clone, Copy)]
pub struct BcClosure {
    info: InfoTagged<CodeRef>,
    env: RefPtr<BcEnvFrame>,
}

impl BcClosure {
    /// A non-callable closure of `code` over `env`.
    pub fn new(code: CodeRef, env: RefPtr<BcEnvFrame>) -> Self {
        BcClosure {
            info: InfoTagged::new(0, code, Smid::default()),
            env,
        }
    }

    /// A non-callable closure with a source annotation.
    pub fn new_annotated(code: CodeRef, env: RefPtr<BcEnvFrame>, annotation: Smid) -> Self {
        BcClosure {
            info: InfoTagged::new(0, code, annotation),
            env,
        }
    }

    /// A closure of the given arity (a lambda) with a source annotation.
    pub fn new_annotated_lambda(
        code: CodeRef,
        arity: u8,
        env: RefPtr<BcEnvFrame>,
        annotation: Smid,
    ) -> Self {
        BcClosure {
            info: InfoTagged::new(arity, code, annotation),
            env,
        }
    }

    /// Construct a closure from a pre-built static part.
    pub fn close(info: &InfoTagged<CodeRef>, env: RefPtr<BcEnvFrame>) -> Self {
        BcClosure { info: *info, env }
    }

    /// The closure's code offset.
    pub fn code(&self) -> CodeRef {
        self.info.body()
    }

    /// The closure's environment.
    pub fn env(&self) -> RefPtr<BcEnvFrame> {
        self.env
    }

    /// Redirect the environment pointer (GC fixup).
    pub fn set_env(&mut self, env: RefPtr<BcEnvFrame>) {
        self.env = env;
    }
}

impl InfoTable for BcClosure {
    fn arity(&self) -> u8 {
        self.info.arity()
    }
    fn update(&self) -> bool {
        self.info.update()
    }
    fn annotation(&self) -> Smid {
        self.info.annotation()
    }
}

impl StgObject for BcClosure {}

/// A bytecode runtime value: a code closure or a WHNF native primitive.
#[derive(Clone)]
pub enum BcValue {
    Closure(BcClosure),
    Native(Native),
}

impl StgObject for BcValue {}

impl BcValue {
    /// Borrow the closure, or `None` if this is a native.
    pub fn as_closure(&self) -> Option<&BcClosure> {
        match self {
            BcValue::Closure(c) => Some(c),
            BcValue::Native(_) => None,
        }
    }
}

/// Mark the heap pointers embedded in a native value (mirrors
/// `syntax::mark_ref_heap_pointers`). Only `Str`/`Set`/`Vec`/`NdArray`
/// carry GC-managed pointers; scalars and opaque handles are inert.
fn scan_native<'a>(
    native: &'a Native,
    scope: &'a dyn CollectorScope,
    marker: &mut CollectorHeapView<'a>,
    out: &mut Vec<ScanPtr<'a>>,
) {
    match native {
        Native::Str(ptr) if marker.mark(*ptr) => {
            // Push so HeapString::scan marks the backing bytes.
            out.push(ScanPtr::from_non_null(scope, *ptr));
        }
        Native::Set(ptr) => {
            marker.mark(*ptr);
        }
        Native::NdArray(ptr) => {
            marker.mark(*ptr);
        }
        Native::Vec(ptr) => {
            marker.mark(*ptr);
        }
        _ => {}
    }
}

/// Update forwarded heap pointers in a native value (mirrors
/// `syntax::update_ref_heap_pointers`).
fn update_native(native: &mut Native, heap: &CollectorHeapView<'_>) {
    match native {
        Native::Str(ptr) => {
            if let Some(new_ptr) = heap.forwarded_to(*ptr) {
                *ptr = new_ptr;
            }
        }
        Native::Set(ptr) => {
            if let Some(new_ptr) = heap.forwarded_to(*ptr) {
                *ptr = new_ptr;
            }
        }
        Native::NdArray(ptr) => {
            if let Some(new_ptr) = heap.forwarded_to(*ptr) {
                *ptr = new_ptr;
            }
        }
        Native::Vec(ptr) => {
            if let Some(new_ptr) = heap.forwarded_to(*ptr) {
                *ptr = new_ptr;
            }
        }
        _ => {}
    }
}

impl GcScannable for BcClosure {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        // The code field is a `u32` offset into the non-GC arena — inert.
        let env = self.env();
        if marker.mark(env) {
            out.push(ScanPtr::from_non_null(scope, env));
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        if let Some(new_env) = heap.forwarded_to(self.env()) {
            self.set_env(new_env);
        }
    }
}

impl GcScannable for BcValue {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        match self {
            BcValue::Closure(c) => c.scan(scope, marker, out),
            BcValue::Native(n) => scan_native(n, scope, marker, out),
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        match self {
            BcValue::Closure(c) => c.scan_and_update(heap),
            BcValue::Native(n) => update_native(n, heap),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::memory::alloc::ScopedAllocator;
    use crate::eval::memory::heap::Heap;
    use crate::eval::memory::mutator::MutatorHeapView;

    /// A `BcClosure`'s code field is plain inert data: constructing it
    /// with an arbitrary large offset and then redirecting its env leaves
    /// the code offset untouched. (Full `scan_and_update` under a live
    /// collector is exercised by the Phase 2/3 GC-stress differential runs.)
    #[test]
    fn bcclosure_code_field_is_inert() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        let env1 = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let env2 = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();

        let bogus_code: CodeRef = 0xDEAD_BEEF;
        let mut closure = BcClosure::new_annotated_lambda(bogus_code, 3, env1, Smid::from(42));

        assert_eq!(closure.code(), bogus_code);
        assert_eq!(closure.env(), env1);
        assert_eq!(closure.arity(), 3);

        closure.set_env(env2);
        assert_eq!(closure.env(), env2);
        assert_eq!(closure.code(), bogus_code);
    }

    #[test]
    fn bcvalue_native_carries_scalar() {
        let v = BcValue::Native(Native::Num(serde_json::Number::from(7)));
        assert!(v.as_closure().is_none());
        match v {
            BcValue::Native(Native::Num(n)) => assert_eq!(n.as_i64(), Some(7)),
            _ => panic!("expected native num"),
        }
    }
}
