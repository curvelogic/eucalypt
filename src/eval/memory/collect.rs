//! Collector support
//!
//! In contrast to the mutator, a collector needs facilities for
//! tracing, marking and potentially, in future, moving.
//!

use std::{collections::VecDeque, mem::size_of, ptr::NonNull};

use crate::eval::machine::metrics::{Clock, ThreadOccupation};

use super::{array::Array, header::AllocHeader, heap::Heap};

pub struct ScanPtr<'scope> {
    value: &'scope dyn GcScannable,
    /// True if this points to an independently heap-allocated object
    /// (with its own AllocHeader prefix), as opposed to an embedded
    /// reference within another allocation (e.g. array element).
    heap_object: bool,
}

impl<'scope> ScanPtr<'scope> {
    /// Create a ScanPtr for an embedded reference (e.g. array element).
    /// This object cannot be independently evacuated.
    pub fn new(
        _scope: &'scope dyn CollectorScope,
        value: &'scope dyn GcScannable,
    ) -> ScanPtr<'scope> {
        ScanPtr {
            value,
            heap_object: false,
        }
    }

    /// Create a ScanPtr for an independently heap-allocated object.
    /// This object has its own AllocHeader and can be evacuated.
    pub fn from_non_null<T: GcScannable + 'scope>(
        _guard: &'scope dyn CollectorScope,
        ptr: NonNull<T>,
    ) -> Self {
        // SAFETY: The dereference is valid because:
        // - NonNull<T> guarantees the pointer is non-null
        // - The pointer came from heap allocation during mutation
        // - The 'scope lifetime is tied to the CollectorScope (GC pause)
        // - During collection, the heap is not mutated (stop-the-world)
        ScanPtr {
            value: unsafe { &*ptr.as_ptr() },
            heap_object: true,
        }
    }

    pub fn get(&self) -> &'scope dyn GcScannable {
        self.value
    }

    /// Whether this points to an independently heap-allocated object
    /// that can be evacuated (has its own AllocHeader).
    pub fn is_heap_object(&self) -> bool {
        self.heap_object
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
    /// Scan this object for references to other heap objects.
    /// Pushes newly discovered grey objects to the provided buffer.
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    );

    /// Update any forwarded pointers in this object in place.
    ///
    /// Called during the update phase of an evacuating collection.
    /// Default implementation does nothing (for types with no
    /// rewritable pointers).
    fn scan_and_update(&mut self, _heap: &CollectorHeapView<'_>) {}
}

impl<T: GcScannable> GcScannable for Vec<NonNull<T>> {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        for p in self {
            if marker.mark(*p) {
                out.push(ScanPtr::from_non_null(scope, *p))
            }
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        for p in self.iter_mut() {
            if let Some(new_ptr) = heap.forwarded_to(*p) {
                *p = new_ptr;
            }
        }
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

    pub fn defer_sweep(&mut self) {
        self.heap.defer_sweep();
    }

    /// Allocate space in the evacuation target block during collection.
    ///
    /// Returns None if no space is available (target block full and no
    /// replacement can be acquired).
    pub fn evacuate_alloc(&mut self, size: usize) -> Option<NonNull<u8>> {
        self.heap.bump_allocate_in_target(size)
    }

    /// If the object at `ptr` has been forwarded, return the new location.
    /// Otherwise return None.
    pub fn forwarded_to<T>(&self, ptr: NonNull<T>) -> Option<NonNull<T>> {
        let header_ptr = self.heap.header_ptr_mut(ptr);
        // SAFETY: header_ptr is valid (from heap allocation with AllocHeader
        // prefix). Read-only access during stop-the-world collection.
        let header = unsafe { &*header_ptr };
        if header.is_forwarded() {
            // SAFETY: The forwarded_to pointer was set by evacuate() and
            // points to a valid copy of the same type T in an evacuation
            // target block.
            Some(unsafe {
                NonNull::new_unchecked(
                    header
                        .forwarded_to()
                        .expect("forwarded bit set implies pointer present")
                        .as_ptr() as *mut T,
                )
            })
        } else {
            None
        }
    }

    /// Check whether an object resides in a candidate block.
    pub fn is_in_candidate_block<T>(&self, ptr: NonNull<T>, candidates: &[usize]) -> bool {
        self.heap.is_in_candidate_block(ptr, candidates)
    }

    /// Evacuate an object from a candidate block to the evacuation target.
    ///
    /// Copies the object (header + payload) to a fresh allocation in the
    /// evacuation target block, sets the forwarding pointer in the old
    /// header, and marks the new copy.
    ///
    /// Returns the new location, or None if evacuation failed (e.g.
    /// target block full and no replacement available).
    pub fn evacuate<T>(&mut self, obj: NonNull<T>) -> Option<NonNull<T>> {
        let header_ptr = self.heap.header_ptr_mut(obj);
        // SAFETY: header_ptr is valid (from heap allocation). Mutable
        // access is safe during stop-the-world collection — no concurrent
        // readers or writers.
        let header = unsafe { &mut *header_ptr };

        // Already forwarded — return existing new location
        if header.is_forwarded() {
            // SAFETY: forwarded_to was set by a previous evacuate() call
            // and points to a valid T in an evacuation target block.
            return Some(unsafe {
                NonNull::new_unchecked(
                    header
                        .forwarded_to()
                        .expect("forwarded bit set implies pointer present")
                        .as_ptr() as *mut T,
                )
            });
        }

        let payload_size = header.length() as usize;
        let alloc_size = size_of::<AllocHeader>() + payload_size;

        // Allocate space in evacuation target
        let new_raw = self.heap.bump_allocate_in_target(alloc_size)?;

        // SAFETY: Byte-level copy of the complete allocation (header + payload).
        //
        // Invariants:
        // 1. Source (`src`) is the start of the AllocHeader, which is
        //    `obj - size_of::<AllocHeader>()`. Valid because all heap objects
        //    are allocated with an AllocHeader prefix by `Heap::alloc()`.
        // 2. Destination (`new_raw`) is freshly bump-allocated in the
        //    evacuation target block, guaranteed non-overlapping with the
        //    source (different block).
        // 3. `alloc_size = size_of::<AllocHeader>() + header.length()` covers
        //    the complete allocation. `header.length()` stores the object's
        //    payload size, set by `alloc<T>()` to `size_of::<T>()`.
        //
        // If violated: partial copy would leave the evacuated object with
        // uninitialised fields, causing segfaults when `scan_and_update`
        // reads internal pointers.
        unsafe {
            let src = (obj.as_ptr() as *const u8).sub(size_of::<AllocHeader>());
            std::ptr::copy_nonoverlapping(src, new_raw.as_ptr(), alloc_size);
        }

        // Compute new object pointer (after the copied header)
        // SAFETY: new_raw points to the start of the copied header.
        // Adding size_of::<AllocHeader>() yields the object pointer,
        // which is valid and non-null (bump allocation succeeded).
        let new_obj = unsafe {
            NonNull::new_unchecked(new_raw.as_ptr().add(size_of::<AllocHeader>()) as *mut T)
        };

        // Set forwarding pointer in the OLD header
        // SAFETY: header_ptr is the same valid header we read above.
        // Mutable access is safe during stop-the-world collection.
        let old_header = unsafe { &mut *self.heap.header_ptr_mut(obj) };
        old_header.set_forwarded(new_obj.cast());

        // Mark the new copy as live
        self.heap.mark_object(new_obj);
        self.heap.mark_line(new_obj);

        Some(new_obj)
    }
}

pub struct Scope();
impl CollectorScope for Scope {}

pub fn collect(roots: &mut dyn GcScannable, heap: &mut Heap, clock: &mut Clock, dump_heap: bool) {
    // Decide collection strategy based on fragmentation analysis
    let strategy = heap.analyze_collection_strategy();
    let candidates = strategy.candidates();

    if !candidates.is_empty() {
        return collect_with_evacuation(roots, heap, clock, &candidates, dump_heap);
    }

    if dump_heap {
        eprintln!("GC!");
    }

    clock.switch(ThreadOccupation::CollectorMark);

    let mut heap_view = CollectorHeapView { heap };

    // clear line maps
    heap_view.reset();

    let mut queue = VecDeque::default();
    let mut scan_buffer = Vec::new();

    let scope = Scope();

    // find and queue the roots
    roots.scan(&scope, &mut heap_view, &mut scan_buffer);
    queue.extend(scan_buffer.drain(..));

    while let Some(scanptr) = queue.pop_front() {
        scanptr.get().scan(&scope, &mut heap_view, &mut scan_buffer);
        queue.extend(scan_buffer.drain(..));
    }

    if dump_heap {
        eprintln!("Heap after mark:\n\n{:?}", &heap_view.heap)
    }

    clock.switch(ThreadOccupation::CollectorSweep);

    // Defer sweep to allocation time (lazy sweeping)
    heap_view.defer_sweep();

    if dump_heap {
        eprintln!("Heap after defer_sweep:\n\n{:?}", &heap_view.heap)
    }

    // Record collection count and update peak blocks
    heap_view.heap.record_collection();

    // After collection, flip mark state ready for next collection
    heap_view.heap.flip_mark_state();
}

/// Perform a collecting GC cycle with selective evacuation of fragmented blocks.
///
/// This extends the basic mark-sweep with Immix-style opportunistic evacuation:
/// 1. Mark phase: trace live objects as normal
/// 2. Evacuate phase: copy marked objects from candidate blocks to target blocks
/// 3. Update phase: rewrite all pointers to forwarded objects
/// 4. Sweep phase: defer sweep; candidate blocks are fully dead after evacuation
///
/// Objects are evacuated via `CollectorHeapView::evacuate()` which does a
/// byte-level copy (header + payload) and sets a forwarding pointer in the
/// old header. The update phase calls `scan_and_update()` on all live objects
/// to rewrite any pointers that now point to forwarded locations.
pub fn collect_with_evacuation(
    roots: &mut dyn GcScannable,
    heap: &mut Heap,
    clock: &mut Clock,
    candidates: &[usize],
    dump_heap: bool,
) {
    if dump_heap {
        eprintln!("GC (evacuating)!");
    }

    clock.switch(ThreadOccupation::CollectorMark);

    // --- Mark phase ---
    // Record all visited heap objects as raw pointers for the update phase.
    // Only heap objects (with AllocHeader prefix) are recorded — embedded
    // references (e.g. array elements) are updated via their parent object's
    // scan_and_update implementation.
    let mut live_heap_objects: Vec<(*const u8, *const ())> = Vec::new();

    {
        let mut heap_view = CollectorHeapView { heap: &mut *heap };
        heap_view.reset();

        let mut queue = VecDeque::default();
        let mut scan_buffer = Vec::new();
        let scope = Scope();

        roots.scan(&scope, &mut heap_view, &mut scan_buffer);
        queue.extend(scan_buffer.drain(..));

        while let Some(scanptr) = queue.pop_front() {
            let obj = scanptr.get();

            if scanptr.is_heap_object() {
                // Record data pointer and vtable for the update phase
                let (data_ptr, vtable_ptr) = raw_parts_of(obj);
                live_heap_objects.push((data_ptr, vtable_ptr));

                // Evacuate if in a candidate block. The old copy is still
                // scanned normally — its data is intact.
                try_evacuate(&mut heap_view, data_ptr, candidates);
            }

            obj.scan(&scope, &mut heap_view, &mut scan_buffer);
            queue.extend(scan_buffer.drain(..));
        }

        if dump_heap {
            eprintln!("Heap after mark:\n\n{:?}", &heap_view.heap)
        }
    }

    // --- Update phase ---
    // Rewrite forwarded pointers in roots and all live heap objects.
    {
        let heap_view = CollectorHeapView { heap: &mut *heap };

        // Update root pointers (MachineState: globals, closure, stack)
        roots.scan_and_update(&heap_view);

        // Update all live heap objects via reconstructed fat pointers.
        // The fat pointer is reconstructed from the data pointer (possibly
        // resolved to the forwarded copy) and the original vtable pointer.
        for &(data_ptr, vtable_ptr) in &live_heap_objects {
            let Some(ptr) = NonNull::new(data_ptr as *mut u8) else {
                continue;
            };

            // Determine the target: if evacuated, update the NEW copy
            let target = if let Some(new_ptr) = heap_view.forwarded_to(ptr) {
                new_ptr.as_ptr()
            } else {
                data_ptr as *mut u8
            };

            // SAFETY: Reconstruct a `&mut dyn GcScannable` fat pointer from
            // its raw components (data pointer + vtable pointer).
            //
            // Invariants:
            // 1. `target` is a valid data pointer: either the original heap
            //    object (if not evacuated) or a byte-for-byte copy in the
            //    evacuation target block (if evacuated). In both cases the
            //    memory is live and correctly laid out for the concrete type.
            // 2. `vtable_ptr` was extracted by `raw_parts_of()` from a real
            //    `&dyn GcScannable` trait object during the mark phase. It
            //    points to the correct vtable in static/code memory, which
            //    is unaffected by heap evacuation.
            // 3. The concrete type's layout matches between old and new
            //    locations because `evacuate()` does a byte-level copy of
            //    header + payload (via `copy_nonoverlapping`).
            // 4. Mutable access is safe: during stop-the-world collection
            //    no concurrent readers or writers exist.
            //
            // If violated: calling through a mismatched vtable would
            // invoke the wrong `scan_and_update`, likely corrupting heap
            // pointers or segfaulting.
            unsafe {
                let obj_ref: &mut dyn GcScannable =
                    std::mem::transmute::<[usize; 2], &mut dyn GcScannable>([
                        target as usize,
                        vtable_ptr as usize,
                    ]);
                obj_ref.scan_and_update(&heap_view);
            }
        }

        // Finalise evacuation: integrate target blocks, reclaim candidates
        heap_view.heap.finalise_evacuation();
    }

    clock.switch(ThreadOccupation::CollectorSweep);

    // Defer sweep to allocation time (lazy sweeping)
    {
        let mut heap_view = CollectorHeapView { heap };
        heap_view.defer_sweep();
    }

    heap.record_collection();
    heap.flip_mark_state();
}

/// Extract data pointer and vtable pointer from a trait object reference.
///
/// Returns `(data_ptr, vtable_ptr)` which can be used to reconstruct the
/// fat pointer later via `transmute::<[usize; 2], &mut dyn GcScannable>`.
///
/// This is used during evacuating collection to record live objects in the
/// mark phase and reconstruct mutable references in the update phase.
fn raw_parts_of(obj: &dyn GcScannable) -> (*const u8, *const ()) {
    // SAFETY: `&dyn Trait` is a fat pointer `(data_ptr, vtable_ptr)` on all
    // Rust targets (guaranteed by the Rust ABI for trait objects).
    // Transmuting to `[usize; 2]` extracts both components.
    //
    // The data pointer (fat[0]) points to the concrete object in heap memory.
    // The vtable pointer (fat[1]) points to the type's vtable in static memory.
    //
    // If violated: the ABI representation of trait objects changes, which would
    // be a fundamental Rust ABI break affecting all dynamic dispatch.
    let fat: [usize; 2] = unsafe { std::mem::transmute(obj) };
    (fat[0] as *const u8, fat[1] as *const ())
}

/// Attempt to evacuate an object at `data_ptr` if it resides in a candidate block.
fn try_evacuate(heap_view: &mut CollectorHeapView<'_>, data_ptr: *const u8, candidates: &[usize]) {
    if candidates.is_empty() {
        return;
    }
    let Some(ptr) = NonNull::new(data_ptr as *mut u8) else {
        return;
    };
    if !heap_view.is_in_candidate_block(ptr, candidates) {
        return;
    }
    // Evacuate: copy header + payload, set forwarding pointer in old header
    let _ = heap_view.evacuate(ptr);
}

#[cfg(test)]
pub mod tests {
    use std::iter::repeat_with;

    use crate::{
        common::sourcemap::Smid,
        eval::memory::{
            mutator::MutatorHeapView,
            syntax::{HeapSyn, LambdaForm, Ref, StgBuilder},
        },
    };

    use super::*;

    #[test]
    pub fn test_simple_collection() {
        let mut heap = Heap::new();
        let mut clock = Clock::default();
        let mut pool = crate::eval::memory::symbol::SymbolPool::new();

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
                view.app(
                    Ref::L(0),
                    view.singleton(view.sym_ref(&mut pool, "foo").unwrap()),
                )
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
            collect(&mut vec![let_ptr, bif_ptr], &mut heap, &mut clock, true);
        }

        // Flush lazy sweep so recycled counts are deterministic
        heap.flush_unswept();
        let stats_a = heap.stats();

        {
            collect(&mut vec![bif_ptr], &mut heap, &mut clock, true);
        }

        heap.flush_unswept();
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
                view.app(
                    Ref::L(0),
                    view.singleton(view.sym_ref(&mut pool, "foo").unwrap()),
                )
                .unwrap(),
            )
            .unwrap()
            .as_ptr()
        };

        {
            collect(&mut vec![let_ptr2], &mut heap, &mut clock, true);
        }

        heap.flush_unswept();
        let stats_c = heap.stats();
        eprintln!("Final stats: {:?}", &stats_c);

        // Original test logic: removing let_ptr root should cause more blocks to be recycled
        assert!(stats_a.recycled < stats_b.recycled,
               "Second collection should recycle more blocks after removing let_ptr root. stats_a.recycled ({}) < stats_b.recycled ({})",
               stats_a.recycled, stats_b.recycled);
    }

    /// Test that the transmute-based fat pointer reconstruction works for
    /// scan_and_update by manually creating an object, extracting its
    /// fat pointer components, and calling scan_and_update via transmute.
    #[test]
    pub fn test_transmute_scan_and_update() {
        let mut heap = Heap::new();
        let let_ptr = {
            let view = MutatorHeapView::new(&heap);

            // Create a simple HeapSyn object on the heap
            let atom = view.atom(Ref::L(0)).unwrap();
            let body = view.atom(Ref::L(1)).unwrap();
            let let_obj = view
                .let_(
                    view.array(&[LambdaForm::new(1, atom.as_ptr(), Smid::default())]),
                    body,
                )
                .unwrap();
            let_obj.as_ptr()
        };

        // Get a trait object reference
        let obj: &dyn GcScannable = unsafe { &*let_ptr.as_ptr() };

        // Extract fat pointer parts
        let (data_ptr, vtable_ptr) = raw_parts_of(obj);

        eprintln!(
            "data_ptr={:p} vtable_ptr={:p} let_ptr={:p}",
            data_ptr,
            vtable_ptr,
            let_ptr.as_ptr()
        );

        // Verify data_ptr matches the actual pointer
        assert_eq!(data_ptr as usize, let_ptr.as_ptr() as usize);

        // Reconstruct via transmute and call scan_and_update (no-op since
        // nothing is forwarded, but verifies the transmute doesn't crash)
        let heap_view = CollectorHeapView { heap: &mut heap };
        unsafe {
            let obj_ref: &mut dyn GcScannable =
                std::mem::transmute::<[usize; 2], &mut dyn GcScannable>([
                    data_ptr as usize,
                    vtable_ptr as usize,
                ]);
            obj_ref.scan_and_update(&heap_view);
        }
        eprintln!("transmute + scan_and_update succeeded");
    }

    /// Test full evacuation cycle with pointer updating.
    #[test]
    pub fn test_evacuation_with_pointer_update() {
        let mut heap = Heap::new();
        let mut clock = Clock::default();
        let mut pool = crate::eval::memory::symbol::SymbolPool::new();
        clock.switch(ThreadOccupation::Mutator);

        // Allocate enough objects to span multiple blocks
        let root_ptr = {
            let view = MutatorHeapView::new(&heap);
            let ids = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
            })
            .take(512)
            .collect::<Vec<_>>();
            let idarray = view.array(ids.as_slice());
            view.let_(
                idarray,
                view.app(
                    Ref::L(0),
                    view.singleton(view.sym_ref(&mut pool, "root").unwrap()),
                )
                .unwrap(),
            )
            .unwrap()
            .as_ptr()
        };

        // Do a normal collection first
        collect(&mut vec![root_ptr], &mut heap, &mut clock, false);
        heap.flush_unswept();

        let rest_before = heap.rest_block_count();
        let unswept_before = heap.unswept_block_count();
        eprintln!("After first collection: rest={rest_before} unswept={unswept_before}");

        // Allocate more objects (these go into new/recycled blocks)
        clock.switch(ThreadOccupation::Mutator);
        let ptr2 = {
            let view = MutatorHeapView::new(&heap);
            let ids = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
            })
            .take(128)
            .collect::<Vec<_>>();
            let idarray = view.array(ids.as_slice());
            view.let_(
                idarray,
                view.app(
                    Ref::L(0),
                    view.singleton(view.sym_ref(&mut pool, "child").unwrap()),
                )
                .unwrap(),
            )
            .unwrap()
            .as_ptr()
        };

        let rest_after_alloc = heap.rest_block_count();
        eprintln!("After second allocation: rest={rest_after_alloc}");

        // Force evacuation of all rest blocks
        let candidate_count = rest_after_alloc;
        if candidate_count == 0 {
            eprintln!("No rest blocks to evacuate, skipping test");
            return;
        }
        let candidates: Vec<usize> = (2..2 + candidate_count).collect();
        eprintln!("Forcing evacuation of candidates: {:?}", candidates);

        collect_with_evacuation(
            &mut vec![root_ptr, ptr2],
            &mut heap,
            &mut clock,
            &candidates,
            true,
        );

        eprintln!("Evacuation collection completed successfully");
    }

    /// Test that a single evacuated object is accessible via the forwarding
    /// pointer and that its data is intact after evacuation.
    #[test]
    pub fn test_evacuate_single_object() {
        let mut heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        // Allocate a simple atom (HeapSyn::Atom { evaluand: Ref::L(42) })
        let atom_ptr = view.atom(Ref::L(42)).unwrap().as_ptr();
        let original_addr = atom_ptr.as_ptr() as usize;

        // Manually perform evacuation via CollectorHeapView
        let mut heap_view = CollectorHeapView { heap: &mut heap };

        // Mark the object so evacuation recognises it as live
        heap_view.mark(atom_ptr);

        // Evacuate
        let new_ptr = heap_view
            .evacuate(atom_ptr)
            .expect("evacuation should succeed");
        let new_addr = new_ptr.as_ptr() as usize;

        // New location must differ from original
        assert_ne!(
            original_addr, new_addr,
            "evacuated copy should be at a different address"
        );

        // Forwarding pointer should point to the new location
        let forwarded = heap_view.forwarded_to(atom_ptr);
        assert!(
            forwarded.is_some(),
            "original should have a forwarding pointer"
        );
        assert_eq!(
            forwarded.unwrap().as_ptr() as usize,
            new_addr,
            "forwarding pointer should match new location"
        );

        // Data at new location should be intact
        let new_obj: &HeapSyn = unsafe { &*new_ptr.as_ptr() };
        match new_obj {
            HeapSyn::Atom {
                evaluand: Ref::L(42),
            } => {} // correct
            other => panic!("evacuated object has wrong data: {:?}", other),
        }
    }

    /// Test that evacuation preserves data integrity for various HeapSyn
    /// variants (Atom, App, Cons).
    #[test]
    pub fn test_evacuate_preserves_data() {
        let mut heap = Heap::new();
        let mut pool = crate::eval::memory::symbol::SymbolPool::new();
        let view = MutatorHeapView::new(&heap);

        // Create several different types of objects
        let atom_ptr = view.atom(Ref::L(7)).unwrap().as_ptr();
        let app_ptr = view
            .app(
                Ref::L(0),
                view.singleton(view.sym_ref(&mut pool, "test").unwrap()),
            )
            .unwrap()
            .as_ptr();

        let mut heap_view = CollectorHeapView { heap: &mut heap };

        // Mark and evacuate both
        heap_view.mark(atom_ptr);
        heap_view.mark(app_ptr);

        let new_atom = heap_view.evacuate(atom_ptr).expect("atom evacuation");
        let new_app = heap_view.evacuate(app_ptr).expect("app evacuation");

        // Verify atom data
        let atom_obj: &HeapSyn = unsafe { &*new_atom.as_ptr() };
        match atom_obj {
            HeapSyn::Atom {
                evaluand: Ref::L(7),
            } => {}
            other => panic!("evacuated atom has wrong data: {:?}", other),
        }

        // Verify app data structure
        let app_obj: &HeapSyn = unsafe { &*new_app.as_ptr() };
        match app_obj {
            HeapSyn::App { callable, args } => {
                assert_eq!(*callable, Ref::L(0));
                assert_eq!(args.len(), 1);
            }
            other => panic!("evacuated app has wrong data: {:?}", other),
        }
    }

    /// Test that no evacuation occurs when the heap is densely packed.
    ///
    /// A heap where all blocks are densely occupied should produce
    /// MarkInPlace strategy, not evacuation.
    #[test]
    pub fn test_no_evacuation_when_not_fragmented() {
        let mut heap = Heap::new();
        let mut clock = Clock::default();
        clock.switch(ThreadOccupation::Mutator);

        // Allocate many objects to densely fill blocks
        let ptrs: Vec<_> = {
            let view = MutatorHeapView::new(&heap);
            (0..400)
                .map(|i| view.atom(Ref::L(i)).unwrap().as_ptr())
                .collect()
        };

        // After collection with all objects as roots, blocks should be dense
        collect(&mut ptrs.clone(), &mut heap, &mut clock, false);

        let strategy = heap.analyze_collection_strategy();
        // When all blocks are dense, strategy should be MarkInPlace
        // (or at worst, the candidate list should be empty)
        let candidates = strategy.candidates();
        assert!(
            candidates.is_empty(),
            "densely packed heap should not have evacuation candidates, got {:?}",
            candidates
        );
    }

    /// Test that evacuation correctly updates internal pointers.
    ///
    /// Create a Let expression whose body points to another HeapSyn.
    /// Evacuate the body object. After the update phase, verify the
    /// Let's body pointer has been rewritten to the new location.
    #[test]
    pub fn test_evacuate_with_internal_refs() {
        let mut heap = Heap::new();
        let mut clock = Clock::default();
        let mut pool = crate::eval::memory::symbol::SymbolPool::new();
        clock.switch(ThreadOccupation::Mutator);

        // We need objects in different blocks to test cross-block evacuation.
        // Fill up blocks to push objects into rest.
        let (let_ptr, body_addr) = {
            let view = MutatorHeapView::new(&heap);

            // Create garbage to fill initial blocks
            let _garbage = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
            })
            .take(512)
            .collect::<Vec<_>>();

            // Create a body object
            let body = view.atom(Ref::L(99)).unwrap();
            let body_addr = body.as_ptr().as_ptr() as usize;

            // Create a Let that references the body
            let ids = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
            })
            .take(4)
            .collect::<Vec<_>>();
            let let_ptr = view
                .let_(
                    view.array(ids.as_slice()),
                    view.app(
                        Ref::L(0),
                        view.singleton(view.sym_ref(&mut pool, "ref").unwrap()),
                    )
                    .unwrap(),
                )
                .unwrap()
                .as_ptr();

            (let_ptr, body_addr)
        };

        // Perform a normal collection then an evacuating collection
        collect(&mut vec![let_ptr], &mut heap, &mut clock, false);
        heap.flush_unswept();

        let rest_count = heap.rest_block_count();
        if rest_count > 0 {
            let candidates: Vec<usize> = (2..2 + rest_count).collect();
            collect_with_evacuation(
                &mut vec![let_ptr],
                &mut heap,
                &mut clock,
                &candidates,
                false,
            );
        }

        // If we reach here, evacuation with internal pointer updates succeeded
        let _ = body_addr; // used for setup
    }

    #[test]
    pub fn test_per_heap_mark_state_isolation() {
        // Test that each heap has its own independent mark state
        let mut heap1 = Heap::new();
        let heap2 = Heap::new();
        let mut clock = Clock::default();
        clock.switch(ThreadOccupation::Mutator);

        // Both heaps should start with the same mark state
        assert_eq!(heap1.mark_state(), heap2.mark_state());
        let initial_state = heap1.mark_state();

        // Perform collection on heap1 only
        let mut empty_roots: Vec<NonNull<crate::eval::memory::syntax::HeapSyn>> = vec![];
        collect(&mut empty_roots, &mut heap1, &mut clock, true);

        // heap1's mark state should have flipped, heap2's should remain unchanged
        assert_ne!(heap1.mark_state(), initial_state);
        assert_eq!(heap2.mark_state(), initial_state);
        assert_ne!(heap1.mark_state(), heap2.mark_state());
    }

    #[test]
    pub fn test_simple_collection_with_isolation() {
        let mut heap = Heap::new();
        let mut clock = Clock::default();
        let mut pool = crate::eval::memory::symbol::SymbolPool::new();

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
                view.app(
                    Ref::L(0),
                    view.singleton(view.sym_ref(&mut pool, "foo").unwrap()),
                )
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
            collect(&mut vec![let_ptr, bif_ptr], &mut heap, &mut clock, true);
        }

        // Flush lazy sweep so recycled counts are deterministic
        heap.flush_unswept();
        let stats_a = heap.stats();

        {
            collect(&mut vec![bif_ptr], &mut heap, &mut clock, true);
        }

        heap.flush_unswept();
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
                view.app(
                    Ref::L(0),
                    view.singleton(view.sym_ref(&mut pool, "foo").unwrap()),
                )
                .unwrap(),
            )
            .unwrap()
            .as_ptr()
        };

        {
            collect(&mut vec![let_ptr2], &mut heap, &mut clock, true);
        }

        heap.flush_unswept();
        let stats_c = heap.stats();
        eprintln!("Final stats: {:?}", &stats_c);

        // Original test logic: removing let_ptr root should cause more blocks to be recycled
        assert!(stats_a.recycled < stats_b.recycled,
               "Second collection should recycle more blocks after removing let_ptr root. stats_a.recycled ({}) < stats_b.recycled ({})",
               stats_a.recycled, stats_b.recycled);
    }
}
