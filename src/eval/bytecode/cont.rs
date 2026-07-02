//! Bytecode continuations (spec §6.2).
//!
//! A parallel to `machine::cont::Continuation`, with `CodeRef` bytecode
//! offsets in place of every `RefPtr<HeapSyn>`. The seven variants and
//! their runtime roles are unchanged; the GC impl mirrors the HeapSyn
//! one **minus** code-pointer scanning: `CodeRef` branch bodies,
//! fallbacks, handlers and seq bodies are inert `u32`s. Only heap values
//! — env frames, `ApplyTo`/`LookupLitForce` closures, and the `Array`
//! backings — are traced.

use crate::common::sourcemap::Smid;
use crate::eval::machine::env::EnvironmentFrame;
use crate::eval::memory::{
    alloc::StgObject,
    array::Array,
    collect::{CollectorHeapView, CollectorScope, GcScannable, OpaqueHeapBytes, ScanPtr},
    symbol::SymbolId,
    syntax::RefPtr,
};
use crate::eval::stg::tags::Tag;

use super::{BcClosure, CodeRef};

/// Environment frame type for the bytecode engine.
type BcEnvFrame = EnvironmentFrame<BcClosure>;

/// Continuations recorded on the bytecode machine's stack.
#[derive(Clone)]
pub enum BcContinuation {
    /// Expect a data constructor and match to a branch (or native to fallback).
    Branch {
        /// Lowest tag in the branch table.
        min_tag: Tag,
        /// Indexed branch table: entry at `[tag - min_tag]` holds the code
        /// offset for that tag, or `None` if no branch exists.
        branch_table: Array<Option<CodeRef>>,
        /// Fallback code offset for unmatched data or native.
        fallback: Option<CodeRef>,
        /// Environment of the case statement.
        environment: RefPtr<BcEnvFrame>,
        /// Source annotation at the point the case was pushed.
        annotation: Smid,
    },
    /// Update thunk in environment at index i.
    Update {
        environment: RefPtr<BcEnvFrame>,
        index: usize,
    },
    /// Once callable is evaluated, apply to args.
    ApplyTo {
        args: Array<BcClosure>,
        annotation: Smid,
    },
    /// Catch metadata and pass it (with body) to handler.
    DeMeta {
        /// Handler code offset (receives metadata and body as bound args).
        handler: CodeRef,
        /// or_else code offset (receives the body as a bound arg).
        or_else: CodeRef,
        /// Environment of handlers.
        environment: RefPtr<BcEnvFrame>,
    },
    /// Force-and-discard: once the scrutinee reaches WHNF, enter `body` in
    /// the original environment.
    SeqBind {
        /// Body code offset to enter after forcing the scrutinee.
        body: CodeRef,
        /// Environment for the body (same as the Seq's environment).
        environment: RefPtr<BcEnvFrame>,
        /// Source annotation at the seq site.
        annotation: Smid,
    },
    /// After forcing an unevaluated block object to WHNF, perform the
    /// deferred literal-key lookup.
    LookupLitForce {
        /// Pre-resolved interned symbol to look up.
        key: SymbolId,
        /// Source annotation at the lookup site.
        smid: Smid,
        /// Pre-resolved default closure (returned when key is absent).
        default_closure: BcClosure,
    },
    /// Marks the end of an emitter capture.
    CaptureEnd,
}

impl StgObject for BcContinuation {}

impl BcContinuation {
    /// O(1) tag dispatch for a `Branch` continuation: look up the branch
    /// code offset for `tag` by direct indexing (mirrors
    /// `cont::match_tag`). Returns `None` for any non-`Branch` variant.
    pub fn match_tag(&self, tag: Tag) -> Option<CodeRef> {
        match self {
            BcContinuation::Branch {
                min_tag,
                branch_table,
                ..
            } => {
                if tag < *min_tag {
                    return None;
                }
                let index = (tag - *min_tag) as usize;
                branch_table.get(index).flatten()
            }
            _ => None,
        }
    }
}

/// Mark and push the environment of a single `BcClosure` for GC scanning.
/// Unlike `cont::scan_syn_closure`, there is no code pointer to mark.
fn scan_bc_closure<'a>(
    closure: &'a BcClosure,
    scope: &'a dyn CollectorScope,
    marker: &mut CollectorHeapView<'a>,
    out: &mut Vec<ScanPtr<'a>>,
) {
    let env = closure.env();
    if marker.mark(env) {
        out.push(ScanPtr::from_non_null(scope, env));
    }
}

impl GcScannable for BcContinuation {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        match self {
            BcContinuation::Branch {
                branch_table,
                environment,
                ..
            } => {
                // The branch table is a heap `Array`, so its backing is
                // marked/evacuated — but its entries are inert `CodeRef`s,
                // so (unlike the HeapSyn continuation) no entry is traced.
                if marker.mark_array(branch_table) {
                    if let Some(backing_ptr) = branch_table.allocated_data() {
                        out.push(ScanPtr::from_non_null(
                            scope,
                            backing_ptr.cast::<OpaqueHeapBytes>(),
                        ));
                    }
                }
                // `fallback` is an inert `CodeRef` — not traced.
                if marker.mark(*environment) {
                    out.push(ScanPtr::from_non_null(scope, *environment));
                }
            }
            BcContinuation::Update { environment, .. } => {
                if marker.mark(*environment) {
                    out.push(ScanPtr::from_non_null(scope, *environment));
                }
            }
            BcContinuation::ApplyTo { args, .. } => {
                if marker.mark_array(args) {
                    if let Some(backing_ptr) = args.allocated_data() {
                        out.push(ScanPtr::from_non_null(
                            scope,
                            backing_ptr.cast::<OpaqueHeapBytes>(),
                        ));
                    }
                    for arg in args.iter() {
                        out.push(ScanPtr::new(scope, arg));
                    }
                }
            }
            BcContinuation::DeMeta { environment, .. } => {
                // `handler`/`or_else` are inert `CodeRef`s — not traced.
                if marker.mark(*environment) {
                    out.push(ScanPtr::from_non_null(scope, *environment));
                }
            }
            BcContinuation::SeqBind { environment, .. } => {
                // `body` is an inert `CodeRef` — not traced.
                if marker.mark(*environment) {
                    out.push(ScanPtr::from_non_null(scope, *environment));
                }
            }
            BcContinuation::LookupLitForce {
                default_closure, ..
            } => {
                scan_bc_closure(default_closure, scope, marker, out);
            }
            BcContinuation::CaptureEnd => {
                // No heap pointers to scan.
            }
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        match self {
            BcContinuation::Branch {
                branch_table,
                environment,
                ..
            } => {
                // Update the backing ptr if the branch_table was evacuated.
                // Entries are inert `CodeRef`s and need no per-entry fixup.
                if let Some(old_ptr) = branch_table.allocated_data() {
                    if let Some(new_ptr) = heap.forwarded_to(old_ptr) {
                        // SAFETY: new_ptr is a valid evacuated copy of the same
                        // backing allocation.
                        unsafe { branch_table.set_backing_ptr(new_ptr.cast()) };
                    }
                }
                if let Some(new) = heap.forwarded_to(*environment) {
                    *environment = new;
                }
            }
            BcContinuation::Update { environment, .. } => {
                if let Some(new) = heap.forwarded_to(*environment) {
                    *environment = new;
                }
            }
            BcContinuation::ApplyTo { args, .. } => {
                if let Some(old_ptr) = args.allocated_data() {
                    if let Some(new_ptr) = heap.forwarded_to(old_ptr) {
                        // SAFETY: new_ptr is a valid evacuated copy of the same
                        // backing allocation.
                        unsafe { args.set_backing_ptr(new_ptr.cast()) };
                    }
                }
                for closure in args.iter_mut() {
                    closure.scan_and_update(heap);
                }
            }
            BcContinuation::DeMeta { environment, .. } => {
                if let Some(new) = heap.forwarded_to(*environment) {
                    *environment = new;
                }
            }
            BcContinuation::SeqBind { environment, .. } => {
                if let Some(new) = heap.forwarded_to(*environment) {
                    *environment = new;
                }
            }
            BcContinuation::LookupLitForce {
                default_closure, ..
            } => {
                default_closure.scan_and_update(heap);
            }
            BcContinuation::CaptureEnd => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::memory::alloc::ScopedAllocator;
    use crate::eval::memory::heap::Heap;
    use crate::eval::memory::mutator::MutatorHeapView;

    #[test]
    fn branch_match_tag_indexes_table() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        let env = view
            .alloc(EnvironmentFrame::<BcClosure>::default())
            .unwrap()
            .as_ptr();
        let branch_table: Array<Option<CodeRef>> = view.array(&[Some(100u32), Some(200u32)]);

        let cont = BcContinuation::Branch {
            min_tag: 5,
            branch_table,
            fallback: Some(999),
            environment: env,
            annotation: Smid::default(),
        };

        assert_eq!(cont.match_tag(5), Some(100));
        assert_eq!(cont.match_tag(6), Some(200));
        assert_eq!(cont.match_tag(4), None); // below min_tag
        assert_eq!(cont.match_tag(99), None); // out of range
    }

    #[test]
    fn non_branch_match_tag_is_none() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let env = view
            .alloc(EnvironmentFrame::<BcClosure>::default())
            .unwrap()
            .as_ptr();
        let cont = BcContinuation::Update {
            environment: env,
            index: 0,
        };
        assert_eq!(cont.match_tag(0), None);
    }
}
