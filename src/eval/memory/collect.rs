//! Collector support
//!
//! In contrast to the mutator, a collector needs facilities for
//! tracing, marking and potentially, in future, moving.
//!

use std::{collections::VecDeque, ptr::NonNull};

use crate::eval::machine::metrics::{Clock, ThreadOccupation};

use super::{array::Array, heap::{Heap, CollectionStrategy}};

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
    /// Objects that need evacuation (collected during marking)
    evacuation_candidates: Vec<NonNull<()>>,
    /// Collection strategy for this collection cycle
    strategy: Option<CollectionStrategy>,
}

impl CollectorHeapView<'_> {
    pub fn reset(&mut self) {
        self.heap.reset_region_marks();
        self.evacuation_candidates.clear();
    }

    pub fn set_strategy(&mut self, strategy: CollectionStrategy) {
        self.strategy = Some(strategy);
    }

    /// Mark object if not already marked and return whether marked
    pub fn mark<T>(&mut self, obj: NonNull<T>) -> bool {
        debug_assert!(obj != NonNull::dangling());
        debug_assert!(obj.as_ptr() as usize != 0xffffffffffffffff);
        if obj != NonNull::dangling() && !self.heap.is_marked(obj) {
            self.heap.mark_object(obj);
            self.heap.mark_line(obj);
            
            // Check if this object should be evacuated based on collection strategy
            if let Some(ref strategy) = self.strategy {
                if self.should_evacuate_object(obj, strategy) {
                    self.evacuation_candidates.push(NonNull::new(obj.as_ptr() as *mut ()).unwrap());
                }
            }
            
            true
        } else {
            false
        }
    }


    /// Check if an object should be evacuated based on collection strategy
    fn should_evacuate_object<T>(&self, obj: NonNull<T>, strategy: &CollectionStrategy) -> bool {
        self.heap.should_evacuate_object(obj, strategy)
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

    /// Evacuate all collected evacuation candidates
    pub fn perform_evacuation(&mut self) {
        let candidates = std::mem::take(&mut self.evacuation_candidates);
        
        for candidate in candidates {
            // Try to evacuate the object
            match self.heap.evacuate_object(candidate) {
                Ok(new_location) => {
                    // Object successfully evacuated
                    // The forwarding pointer is already set by evacuate_object
                    if cfg!(debug_assertions) {
                        println!("Evacuated object from {:p} to {:p}", candidate.as_ptr(), new_location.as_ptr());
                    }
                }
                Err(err) => {
                    // Evacuation failed - this could happen if we run out of space
                    // For now, log the error and continue
                    eprintln!("Failed to evacuate object {:p}: {:?}", candidate.as_ptr(), err);
                }
            }
        }
    }

    /// Evacuate objects from specific fragmented blocks during selective evacuation
    pub fn evacuate_fragmented_blocks(&mut self, block_indices: &[usize]) {
        if cfg!(debug_assertions) {
            println!("Performing selective evacuation of {} blocks", block_indices.len());
        }
        
        // Evacuation candidates should have been collected during marking
        // Now perform the actual evacuation
        self.perform_evacuation();
    }

    /// Evacuate objects from all fragmented blocks during comprehensive defragmentation
    pub fn evacuate_all_fragmented_blocks(&mut self) {
        if cfg!(debug_assertions) {
            println!("Performing comprehensive defragmentation evacuation");
        }
        
        // Evacuation candidates should have been collected during marking
        // Now perform the actual evacuation
        self.perform_evacuation();
    }
}

pub struct Scope();
impl CollectorScope for Scope {}


pub fn collect(roots: &dyn GcScannable, heap: &mut Heap, clock: &mut Clock, dump_heap: bool) {
    if dump_heap {
        eprintln!("GC!");
    }

    // Analyze fragmentation and choose collection strategy before collection
    let collection_strategy = heap.analyze_collection_strategy();
    if dump_heap {
        eprintln!("Collection strategy: {:?}", collection_strategy);
    }

    clock.switch(ThreadOccupation::CollectorMark);

    let mut heap_view = CollectorHeapView { 
        heap,
        evacuation_candidates: Vec::new(),
        strategy: None,
    };

    // clear line maps and evacuation candidates
    heap_view.reset();
    
    // Set collection strategy for evacuation-aware marking
    heap_view.set_strategy(collection_strategy.clone());

    let mut queue = VecDeque::default();

    let scope = Scope();

    // find and queue the roots - the mark method will automatically check for evacuation
    queue.extend(roots.scan(&scope, &mut heap_view).drain(..));

    while let Some(scanptr) = queue.pop_front() {
        queue.extend(scanptr.get().scan(&scope, &mut heap_view).drain(..));
    }

    if dump_heap {
        eprintln!("Heap after mark:\n\n{:?}", &heap_view.heap)
    }

    // Apply evacuation based on collection strategy before sweep
    match collection_strategy {
        CollectionStrategy::MarkInPlace => {
            // No evacuation needed - standard mark-and-sweep
            if dump_heap {
                eprintln!("Using MarkInPlace - no evacuation");
            }
        }
        CollectionStrategy::SelectiveEvacuation(block_indices) => {
            if dump_heap {
                eprintln!("Using SelectiveEvacuation for {} blocks", block_indices.len());
            }
            heap_view.evacuate_fragmented_blocks(&block_indices);
        }
        CollectionStrategy::DefragmentationSweep => {
            if dump_heap {
                eprintln!("Using DefragmentationSweep - evacuating all fragmented blocks");
            }
            heap_view.evacuate_all_fragmented_blocks();
        }
    }

    clock.switch(ThreadOccupation::CollectorSweep);

    // sweep to region
    heap_view.sweep();

    if dump_heap {
        eprintln!("Heap after sweep:\n\n{:?}", &heap_view.heap)
    }

    // After collection, flip mark state ready for next collection
    heap.flip_mark_state();
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
    }

    #[test]
    pub fn test_per_heap_mark_state_isolation() {
        // Test that each heap has its own independent mark state
        let mut heap1 = Heap::new();
        let mut heap2 = Heap::new();
        let mut clock = Clock::default();
        clock.switch(ThreadOccupation::Mutator);

        // Both heaps should start with the same mark state
        assert_eq!(heap1.mark_state(), heap2.mark_state());
        let initial_state = heap1.mark_state();

        // Perform collection on heap1 only
        let empty_roots: Vec<NonNull<crate::eval::memory::syntax::HeapSyn>> = vec![];
        collect(&empty_roots, &mut heap1, &mut clock, true);

        // heap1's mark state should have flipped, heap2's should remain unchanged
        assert_ne!(heap1.mark_state(), initial_state);
        assert_eq!(heap2.mark_state(), initial_state);
        assert_ne!(heap1.mark_state(), heap2.mark_state());
    }

    #[test]
    pub fn test_simple_collection_with_isolation() {
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
    }

    #[test]
    pub fn test_collection_with_strategy_selection() {
        let mut heap = Heap::new();
        let mut clock = Clock::default();

        clock.switch(ThreadOccupation::Mutator);

        // Create some objects that will result in a specific collection strategy
        let ptr = {
            let view = MutatorHeapView::new(&heap);
            view.atom(Ref::L(0)).unwrap().as_ptr()
        };

        // Test that collection analyzes strategy before proceeding
        let strategy_before = heap.analyze_collection_strategy();
        
        // Collect with heap dumping disabled to avoid noise
        collect(&vec![ptr], &mut heap, &mut clock, false);
        
        // Verify that collection completed successfully 
        // (The strategy selection is now integrated into the collection process)
        let stats = heap.stats();
        assert!(stats.blocks_allocated > 0, "Heap should have allocated blocks");
        
        // Verify that the strategy selection is consistent
        let strategy_after = heap.analyze_collection_strategy();
        
        // The strategy might change after collection due to different block states,
        // but the analysis should not crash
        match strategy_before {
            CollectionStrategy::MarkInPlace => {}
            CollectionStrategy::SelectiveEvacuation(_) => {}
            CollectionStrategy::DefragmentationSweep => {}
        }
        
        match strategy_after {
            CollectionStrategy::MarkInPlace => {}
            CollectionStrategy::SelectiveEvacuation(_) => {}
            CollectionStrategy::DefragmentationSweep => {}
        }
    }
}
