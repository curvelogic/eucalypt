//! Bytecode closures: `BcClosure = Closing<CodeRef>` (spec §3).
//!
//! A bytecode closure is the generic `Closing<S>` instantiated with a
//! `CodeRef` (a `u32` bytecode offset) instead of `RefPtr<HeapSyn>`.
//! Because a `u32` offset never moves, the collector treats the code
//! field as inert: `scan`/`scan_and_update` touch only the environment
//! pointer, never `code()` — contrast `SynClosure`, which must both mark
//! and forward its `RefPtr<HeapSyn>` (`env.rs`).
//!
//! All constructors (`new`, `new_annotated`, `new_annotated_lambda`,
//! `close`) are inherited unchanged from the generic `impl<S: Copy>
//! Closing<S>`, so none are redefined here.

use crate::eval::machine::env::Closing;
use crate::eval::memory::collect::{CollectorHeapView, CollectorScope, GcScannable, ScanPtr};

use super::CodeRef;

/// A closure whose code is a bytecode offset rather than a heap pointer.
pub type BcClosure = Closing<CodeRef>;

impl GcScannable for BcClosure {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        // The code field is a `u32` offset into the non-GC bytecode
        // arena — never marked, never traced. Only the environment
        // frame is a heap object.
        let env = self.env();
        if marker.mark(env) {
            out.push(ScanPtr::from_non_null(scope, env));
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        // No `forwarded_to(code())` / `set_body()` — offsets don't move.
        if let Some(new_env) = heap.forwarded_to(self.env()) {
            self.set_env(new_env);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::machine::env::EnvironmentFrame;
    use crate::eval::memory::alloc::ScopedAllocator;
    use crate::eval::memory::heap::Heap;
    use crate::eval::memory::infotable::InfoTable;
    use crate::eval::memory::mutator::MutatorHeapView;

    /// A `BcClosure`'s code field is plain inert data: constructing it
    /// with an arbitrary large offset and then redirecting its env
    /// leaves the code offset untouched. (Full `scan_and_update` under a
    /// live collector is exercised by the Phase 2/3 GC-stress
    /// differential runs, which need a `CollectorHeapView` that cannot be
    /// built outside the `collect` module.)
    #[test]
    fn bcclosure_code_field_is_inert() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        let env1 = view
            .alloc(EnvironmentFrame::<BcClosure>::default())
            .unwrap()
            .as_ptr();
        let env2 = view
            .alloc(EnvironmentFrame::<BcClosure>::default())
            .unwrap()
            .as_ptr();

        let bogus_code: CodeRef = 0xDEAD_BEEF;
        let mut closure = BcClosure::new_annotated_lambda(
            bogus_code,
            3,
            env1,
            crate::common::sourcemap::Smid::from(42),
        );

        assert_eq!(closure.code(), bogus_code);
        assert_eq!(closure.env(), env1);
        assert_eq!(closure.arity(), 3);

        // Redirecting the env (as GC fixup does) never disturbs the code.
        closure.set_env(env2);
        assert_eq!(closure.env(), env2);
        assert_eq!(closure.code(), bogus_code);
    }
}
