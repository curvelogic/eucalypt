//! Collector support
//!
//! In contrast to the mutator, a collector needs facilities for
//! tracing, marking and potentially, in future, moving.
//!

use std::{collections::VecDeque, ptr::NonNull};

use crate::eval::machine::metrics::{Clock, ThreadOccupation};

use super::{array::Array, heap::Heap, mark::flip_mark_state};

pub struct ScanPtr<'scope> {
    value: &'scope dyn GcScannable,
}

impl<'scope> ScanPtr<'scope> {
    pub fn new(
        _scope: &'scope dyn CollectorScope,
        value: &'scope dyn GcScannable,
    ) -> ScanPtr<'scope> {
        ScanPtr { value }
    }

    pub fn from_non_null<T: GcScannable + 'scope>(
        guard: &'scope dyn CollectorScope,
        ptr: NonNull<T>,
    ) -> Self {
        ScanPtr::new(guard, unsafe { &*ptr.as_ptr() })
    }

    pub fn get(&self) -> &'scope dyn GcScannable {
        self.value
    }
}

impl std::fmt::Debug for ScanPtr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.value)
    }
}

/// Anything that represents the collector scope (in contrast to MutatorScope)
pub trait CollectorScope {}

/// A heap object that scanned for references to other heap objects
pub trait GcScannable {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
    ) -> Vec<ScanPtr<'a>>;
}

impl<T: GcScannable> GcScannable for Vec<NonNull<T>> {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
    ) -> Vec<ScanPtr<'a>> {
        let mut grey = vec![];
        for p in self {
            if marker.mark(*p) {
                grey.push(ScanPtr::from_non_null(scope, *p))
            }
        }
        grey
    }
}

/// View of the heap available to the collector
pub struct CollectorHeapView<'guard> {
    heap: &'guard mut Heap,
}

impl CollectorHeapView<'_> {
    pub fn reset(&mut self) {
        self.heap.reset_region_marks();
    }

    /// Mark object if not already marked and return whether marked
    pub fn mark<T>(&mut self, obj: NonNull<T>) -> bool {
        debug_assert!(obj != NonNull::dangling());
        debug_assert!(obj.as_ptr() as usize != 0xffffffffffffffff);
        if obj != NonNull::dangling() && !self.heap.is_marked(obj) {
            self.heap.mark_object(obj);
            self.heap.mark_line(obj);
            true
        } else {
            false
        }
    }

    /// Mark object if not already marked and return whether marked
    pub fn mark_array<T: Clone>(&mut self, arr: &Array<T>) -> bool {
        if let Some(ptr) = arr.allocated_data() {
            debug_assert!(ptr != NonNull::dangling());
            debug_assert!(ptr.as_ptr() as usize != 0xffffffffffffffff);
            if !self.heap.is_marked(ptr) {
                self.heap.mark_object(ptr);
                self.heap.mark_lines_for_bytes(ptr);
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn is_marked<T>(&self, obj: NonNull<T>) -> bool {
        self.heap.is_marked(obj)
    }

    pub fn sweep(&mut self) {
        self.heap.sweep();
    }
}

pub struct Scope();
impl CollectorScope for Scope {}

pub fn collect(roots: &dyn GcScannable, heap: &mut Heap, clock: &mut Clock, dump_heap: bool) {
    if dump_heap {
        eprintln!("GC!");
    }

    clock.switch(ThreadOccupation::CollectorMark);

    let mut heap_view = CollectorHeapView { heap };

    // clear line maps
    heap_view.reset();

    let mut queue = VecDeque::default();

    let scope = Scope();

    // find and queue the roots
    queue.extend(roots.scan(&scope, &mut heap_view).drain(..));

    while let Some(scanptr) = queue.pop_front() {
        queue.extend(scanptr.get().scan(&scope, &mut heap_view).drain(..));
    }

    if dump_heap {
        eprintln!("Heap after mark:\n\n{:?}", &heap_view.heap)
    }

    clock.switch(ThreadOccupation::CollectorSweep);

    // sweep to region
    heap_view.sweep();

    if dump_heap {
        eprintln!("Heap after sweep:\n\n{:?}", &heap_view.heap)
    }

    // After collection, flip mark state ready for next collection
    flip_mark_state();
}

#[cfg(test)]
pub mod tests {
    use std::iter::repeat_with;

    use crate::{
        common::sourcemap::Smid,
        eval::memory::{
            mutator::MutatorHeapView,
            syntax::{LambdaForm, Ref, StgBuilder},
        },
    };

    use super::*;

    #[test]
    pub fn test_simple_collection() {
        use crate::eval::memory::mark::{mark_state, flip_mark_state};
        
        // Ensure clean mark state at test start
        if mark_state() != false {
            flip_mark_state();
        }
        
        let mut heap = Heap::new();
        let mut clock = Clock::default();

        clock.switch(ThreadOccupation::Mutator);

        let let_ptr = {
            let view = MutatorHeapView::new(&heap);

            // A bunch of garbage...

            let ids = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
            })
            .take(1024)
            .collect::<Vec<_>>();
            let idarray = view.array(ids.as_slice());

            view.let_(
                idarray,
                view.app(Ref::L(0), view.singleton(view.sym_ref("foo").unwrap()))
                    .unwrap(),
            )
            .unwrap()
            .as_ptr()
        };

        clock.switch(ThreadOccupation::Mutator);

        let bif_ptr = {
            let view = MutatorHeapView::new(&heap);

            let scoped_ptr = view.app_bif(13, view.array(&[])).unwrap();

            scoped_ptr.as_ptr()
        };

        {
            collect(&vec![let_ptr, bif_ptr], &mut heap, &mut clock, true);
        }

        let stats_a = heap.stats();

        {
            collect(&vec![bif_ptr], &mut heap, &mut clock, true);
        }

        let stats_b = heap.stats();

        clock.switch(ThreadOccupation::Mutator);

        let let_ptr2 = {
            let view = MutatorHeapView::new(&heap);

            // A bunch of garbage...

            let ids = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
            })
            .take(256)
            .collect::<Vec<_>>();
            let idarray = view.array(ids.as_slice());

            view.let_(
                idarray,
                view.app(Ref::L(0), view.singleton(view.sym_ref("foo").unwrap()))
                    .unwrap(),
            )
            .unwrap()
            .as_ptr()
        };

        {
            collect(&vec![let_ptr2], &mut heap, &mut clock, true);
        }

        let stats_c = heap.stats();
        eprintln!("Final stats: {:?}", &stats_c);

        // Original test logic: removing let_ptr root should cause more blocks to be recycled
        assert!(stats_a.recycled < stats_b.recycled, 
               "Second collection should recycle more blocks after removing let_ptr root. stats_a.recycled ({}) < stats_b.recycled ({})", 
               stats_a.recycled, stats_b.recycled);
               
        // Clean up mark state for other tests
        if mark_state() != false {
            flip_mark_state();
        }
    }

    #[test]
    pub fn test_mark_state_isolation() {
        use crate::eval::memory::mark::{mark_state, flip_mark_state};
        
        // Test should be isolated from other tests' mark state
        let initial_mark_state = mark_state();
        
        // Perform collection which flips mark state
        let mut heap = Heap::new();
        let mut clock = Clock::default();
        clock.switch(ThreadOccupation::Mutator);
        
        let empty_roots: Vec<NonNull<crate::eval::memory::syntax::HeapSyn>> = vec![];
        collect(&empty_roots, &mut heap, &mut clock, true);
        
        // Mark state should have flipped
        assert_ne!(mark_state(), initial_mark_state);
        
        // Reset mark state to ensure test isolation
        if mark_state() != false {
            flip_mark_state();
        }
        
        // Verify we're back to initial state
        assert_eq!(mark_state(), false);
    }

    #[test]
    pub fn test_simple_collection_with_isolation() {
        use crate::eval::memory::mark::{mark_state, flip_mark_state};
        
        // Ensure clean mark state at test start
        if mark_state() != false {
            flip_mark_state();
        }
        
        let mut heap = Heap::new();
        let mut clock = Clock::default();

        clock.switch(ThreadOccupation::Mutator);

        let let_ptr = {
            let view = MutatorHeapView::new(&heap);

            // A bunch of garbage...
            let ids = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
            })
            .take(1024)
            .collect::<Vec<_>>();
            let idarray = view.array(ids.as_slice());

            view.let_(
                idarray,
                view.app(Ref::L(0), view.singleton(view.sym_ref("foo").unwrap()))
                    .unwrap(),
            )
            .unwrap()
            .as_ptr()
        };

        clock.switch(ThreadOccupation::Mutator);

        let bif_ptr = {
            let view = MutatorHeapView::new(&heap);
            let scoped_ptr = view.app_bif(13, view.array(&[])).unwrap();
            scoped_ptr.as_ptr()
        };

        {
            collect(&vec![let_ptr, bif_ptr], &mut heap, &mut clock, true);
        }

        let stats_a = heap.stats();

        {
            collect(&vec![bif_ptr], &mut heap, &mut clock, true);
        }

        let stats_b = heap.stats();

        clock.switch(ThreadOccupation::Mutator);

        let let_ptr2 = {
            let view = MutatorHeapView::new(&heap);

            // A bunch of garbage...
            let ids = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
            })
            .take(256)
            .collect::<Vec<_>>();
            let idarray = view.array(ids.as_slice());

            view.let_(
                idarray,
                view.app(Ref::L(0), view.singleton(view.sym_ref("foo").unwrap()))
                    .unwrap(),
            )
            .unwrap()
            .as_ptr()
        };

        {
            collect(&vec![let_ptr2], &mut heap, &mut clock, true);
        }

        let stats_c = heap.stats();
        eprintln!("Final stats: {:?}", &stats_c);

        // Original test logic: removing let_ptr root should cause more blocks to be recycled  
        assert!(stats_a.recycled < stats_b.recycled, 
               "Second collection should recycle more blocks after removing let_ptr root. stats_a.recycled ({}) < stats_b.recycled ({})", 
               stats_a.recycled, stats_b.recycled);
               
        // Clean up mark state for other tests
        if mark_state() != false {
            flip_mark_state();
        }
    }
}
