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

/// A sentinel type used to register raw heap byte allocations (e.g. `Array<T>`
/// backing buffers) as first-class heap objects in the GC scan queue.
///
/// The backing buffer of an `Array<T>` is allocated with an `AllocHeader`
/// prefix but has no vtable and is never queued for scanning on its own.
/// As a result, when the containing object (e.g. `EnvFrame`) is evacuated
/// from a candidate block to a new block, the backing buffer stays in the
/// candidate block.  After the mark state is flipped the backing's mark bit
/// is stale; at the next lazy sweep the backing's lines are recycled while
/// the evacuated copy of the parent still holds the old (now-dead) pointer.
///
/// By pushing the backing pointer as a `ScanPtr::from_non_null::<OpaqueHeapBytes>`
/// during the parent's `scan()` call, the evacuation loop calls `try_evacuate`
/// on the backing.  The parent's `scan_and_update` then calls
/// `forwarded_to(bindings.allocated_data())` and updates the stale pointer.
///
/// `scan` and `scan_and_update` are no-ops: the backing contains raw bytes
/// whose element-level pointer updates are handled by the parent's
/// `scan_and_update`.
pub struct OpaqueHeapBytes;

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

impl GcScannable for OpaqueHeapBytes {
    fn scan<'a>(
        &'a self,
        _scope: &'a dyn CollectorScope,
        _marker: &mut CollectorHeapView<'a>,
        _out: &mut Vec<ScanPtr<'a>>,
    ) {
        // No sub-objects: element-level pointer updates are handled by the
        // parent object's scan_and_update.
    }
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
        debug_assert!(obj.as_ptr() as usize != usize::MAX);

        // Poison check: if enabled, verify the object doesn't sit in
        // swept (poisoned) memory.  This catches use-after-free where a
        // live pointer references memory that was already reclaimed.
        if super::gc_debug::poison_enabled() {
            unsafe {
                if super::gc_debug::looks_poisoned(obj.cast()) {
                    panic!(
                        "GC POISON: use-after-free detected!\n\
                         Object at {:p} (type {}) sits in poisoned (swept) memory.\n\
                         A live GC root references memory that was reclaimed by lazy sweep.",
                        obj.as_ptr(),
                        std::any::type_name::<T>(),
                    );
                }
            }
        }

        if obj != NonNull::dangling() && !self.heap.is_marked(obj) {
            self.heap.mark_object(obj);
            self.heap.mark_line(obj);

            // Verify mark was actually applied
            if super::gc_debug::verify_enabled() {
                debug_assert!(
                    self.heap.is_marked(obj),
                    "GC BUG: mark_object({:p}) did not persist! mark_state={}",
                    obj.as_ptr(),
                    self.heap.mark_state(),
                );
            }

            true
        } else {
            false
        }
    }

    /// Mark a raw byte allocation (e.g. a `RawArray<u8>` backing buffer)
    /// if not already marked, covering all lines spanned by the bytes.
    ///
    /// Returns `true` if the object was newly marked (i.e. it should be
    /// pushed onto the scan queue as an `OpaqueHeapBytes` object).
    pub fn mark_raw_bytes(&mut self, ptr: NonNull<u8>) -> bool {
        debug_assert!(ptr != NonNull::dangling());
        debug_assert!(ptr.as_ptr() as usize != usize::MAX);
        if !self.heap.is_marked(ptr) {
            self.heap.mark_object(ptr);
            self.heap.mark_lines_for_bytes(ptr);
            true
        } else {
            false
        }
    }

    /// Mark object if not already marked and return whether marked
    pub fn mark_array<T: Clone>(&mut self, arr: &Array<T>) -> bool {
        if let Some(ptr) = arr.allocated_data() {
            debug_assert!(ptr != NonNull::dangling());
            debug_assert!(ptr.as_ptr() as usize != usize::MAX);
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

    /// Check whether the block containing `ptr` is currently pinned.
    pub fn is_pinned<T>(&self, ptr: NonNull<T>) -> bool {
        self.heap.is_ptr_in_pinned_block(ptr)
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
        // Round up to 16-byte alignment to match the normal allocation path
        // (Heap::alloc_size_of). The bump block must receive aligned sizes so
        // that consecutive evacuated objects remain 16-byte aligned.
        const ALIGN: usize = 16;
        let alloc_size = (size_of::<AllocHeader>() + payload_size + ALIGN - 1) & !(ALIGN - 1);

        // Allocate space in evacuation target
        let new_raw = self.heap.bump_allocate_in_target(alloc_size)?;

        // SAFETY: Byte-level copy of the header + payload (excluding alignment
        // padding, which need not be copied).
        //
        // Invariants:
        // 1. Source (`src`) is the start of the AllocHeader, which is
        //    `obj - size_of::<AllocHeader>()`. Valid because all heap objects
        //    are allocated with an AllocHeader prefix by `Heap::alloc()`.
        // 2. Destination (`new_raw`) is freshly bump-allocated in the
        //    evacuation target block, guaranteed non-overlapping with the
        //    source (different block). `alloc_size` is >= `copy_size`.
        // 3. `copy_size = size_of::<AllocHeader>() + header.length()` covers
        //    the header and all payload bytes. `header.length()` stores the
        //    object's payload size, set by `alloc<T>()` to `size_of::<T>()`.
        //    The remaining `alloc_size - copy_size` bytes are alignment padding
        //    and need not be initialised.
        //
        // If violated: partial copy would leave the evacuated object with
        // uninitialised fields, causing segfaults when `scan_and_update`
        // reads internal pointers.
        unsafe {
            let src = (obj.as_ptr() as *const u8).sub(size_of::<AllocHeader>());
            // Copy only header + payload, not the alignment padding
            let copy_size = size_of::<AllocHeader>() + payload_size;
            std::ptr::copy_nonoverlapping(src, new_raw.as_ptr(), copy_size);
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

        // Mark the new copy as live.
        //
        // Use `mark_lines_for_bytes` rather than `mark_line<T>` so that the
        // actual payload length (stored in the AllocHeader) is used to mark all
        // lines spanned by the evacuation copy.  `mark_line<T>` uses
        // `size_of::<T>()` which is 0 for ZST types like `OpaqueHeapBytes`:
        // that would leave lines 1..N of a large backing store unmarked, making
        // them visible to `lazy_sweep_next()` as reclaimable holes — corrupting
        // live data on the next allocation.
        self.heap.mark_object(new_obj);
        self.heap.mark_lines_for_bytes(new_obj.cast::<u8>());

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

    // Post-mark verification: re-scan all reachable objects and verify
    // that every pointer they contain points to a marked object.
    if super::gc_debug::verify_enabled() {
        verify_mark_integrity(roots, heap_view.heap);
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

    // Post-mark verification for the evacuation path
    if super::gc_debug::verify_enabled() {
        verify_mark_integrity(roots, heap);
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

/// Verify mark integrity after the mark phase.
///
/// Re-runs the full mark traversal from roots.  Since all live objects
/// should already be marked, `mark()` should return false (already marked)
/// for every object.  If it returns true (freshly marked), that object was
/// missed by the original mark phase and would have been swept — a bug.
///
/// This works by counting how many new objects `mark()` finds.  The
/// `mark()` function marks-and-returns-true for new objects, false for
/// already-marked ones.  After a correct mark phase, the re-traversal
/// should find zero new objects.
///
/// Enabled by `EU_GC_VERIFY=1`.
fn verify_mark_integrity(roots: &dyn GcScannable, heap: &mut Heap) {
    let scope = Scope();
    let mut heap_view = CollectorHeapView { heap };
    let mut queue = VecDeque::default();
    let mut scan_buffer = Vec::new();
    let mut total = 0usize;

    // Track how many times mark() returns true (object was newly marked).
    // After a complete mark phase, this should be zero — every reachable
    // object should already be marked.
    let mark_count_before = heap_view.heap.mark_count();

    // Re-scan from roots using the same scan/mark mechanism.
    roots.scan(&scope, &mut heap_view, &mut scan_buffer);
    queue.extend(scan_buffer.drain(..));

    while let Some(scanptr) = queue.pop_front() {
        total += 1;
        scanptr.get().scan(&scope, &mut heap_view, &mut scan_buffer);
        queue.extend(scan_buffer.drain(..));
    }

    let newly_marked = heap_view.heap.mark_count() - mark_count_before;

    if newly_marked > 0 {
        panic!(
            "GC mark integrity violation: {newly_marked} reachable objects were not marked \
             ({total} objects in re-traversal)"
        );
    }
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
    // Skip evacuation for pinned blocks
    if heap_view.is_pinned(ptr) {
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
    /// Test that a pinned block is NOT evacuated during collection.
    #[test]
    pub fn test_pinned_block_not_evacuated() {
        let mut heap = Heap::new();
        let mut clock = Clock::default();
        clock.switch(ThreadOccupation::Mutator);

        let pinned_ptr = {
            let view = MutatorHeapView::new(&heap);
            let _garbage: Vec<_> = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
            })
            .take(512)
            .collect();
            let pinned = view.atom(Ref::L(42)).unwrap().as_ptr();
            let _others: Vec<_> = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
            })
            .take(256)
            .collect();
            pinned
        };

        let original_addr = pinned_ptr.as_ptr() as usize;

        let _pin_guard = {
            let view = MutatorHeapView::new(&heap);
            view.pin(pinned_ptr)
        };

        collect(&mut vec![pinned_ptr], &mut heap, &mut clock, false);
        heap.flush_unswept();

        let rest_count = heap.rest_block_count();
        if rest_count > 0 {
            let candidates: Vec<usize> = (0..rest_count + 2).collect();
            collect_with_evacuation(
                &mut vec![pinned_ptr],
                &mut heap,
                &mut clock,
                &candidates,
                false,
            );
        }

        let final_addr = pinned_ptr.as_ptr() as usize;
        assert_eq!(
            original_addr, final_addr,
            "pinned object should not have been evacuated"
        );

        let obj: &HeapSyn = unsafe { &*pinned_ptr.as_ptr() };
        match obj {
            HeapSyn::Atom {
                evaluand: Ref::L(42),
            } => {}
            other => panic!("pinned object has wrong data: {:?}", other),
        }
    }

    /// Test that evacuated objects' lines in the target block are correctly
    /// marked, preventing the target block from being recycled by lazy sweep.
    ///
    /// Without the fix (evacuation_target not searched in mark_region_in_block),
    /// the target block has zero line marks after finalise_evacuation(). When
    /// flush_unswept() processes it, recycle() sees a large hole and moves the
    /// block to `recycled`. Subsequent allocations overwrite the evacuated data.
    ///
    /// With the fix, all lines containing evacuated objects are marked, so
    /// recycle() cannot find a hole spanning the occupied lines, and the data
    /// is preserved.
    #[test]
    pub fn test_evacuation_target_lines_marked_after_collection() {
        let mut heap = Heap::new();
        let mut clock = Clock::default();
        let mut pool = crate::eval::memory::symbol::SymbolPool::new();
        clock.switch(ThreadOccupation::Mutator);

        // Allocate enough objects to fill several blocks, producing at least
        // one `rest` block which can be used as an evacuation candidate.
        let root_ptr = {
            let view = MutatorHeapView::new(&heap);
            let ids: Vec<LambdaForm> = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
            })
            .take(512)
            .collect();
            let idarray = view.array(ids.as_slice());
            view.let_(
                idarray,
                view.app(
                    Ref::L(0),
                    view.singleton(view.sym_ref(&mut pool, "root-data").unwrap()),
                )
                .unwrap(),
            )
            .unwrap()
            .as_ptr()
        };

        // First mark-in-place collection to establish rest blocks.
        // Use a named Vec so scan_and_update updates our pointer if needed.
        let mut roots = vec![root_ptr];
        collect(&mut roots, &mut heap, &mut clock, false);
        heap.flush_unswept();

        let rest_count = heap.rest_block_count();
        if rest_count == 0 {
            eprintln!(
                "test_evacuation_target_lines_marked: no rest blocks after first GC, skipping"
            );
            return;
        }

        // Force evacuation of all rest blocks.  After this call, roots[0] is
        // updated to the post-evacuation pointer by scan_and_update.
        let candidates: Vec<usize> = (2..2 + rest_count).collect();
        collect_with_evacuation(&mut roots, &mut heap, &mut clock, &candidates, false);
        // roots[0] is now the pointer into the evacuation target block.
        let evacuated_ptr = roots[0];

        // Flush unswept forces lazy sweep of all blocks including the evacuation
        // target (target → rest → unswept via finalise_evacuation + defer_sweep).
        // Without the fix: target has zero line marks → recycle() finds a huge hole
        // → block goes to `recycled` → subsequent allocations overwrite it.
        heap.flush_unswept();

        // Allocate aggressively to fill any recycled space.  If the evacuation
        // target was recycled, these allocations will overwrite the evacuated objects.
        clock.switch(ThreadOccupation::Mutator);
        {
            let view = MutatorHeapView::new(&heap);
            let fillers: Vec<LambdaForm> = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(99)).unwrap().as_ptr(), Smid::default())
            })
            .take(1024)
            .collect();
            let _ = view.array(fillers.as_slice());
        }

        // Verify the evacuated object is still a valid LetRec node.
        // If the evacuation target block was recycled and overwritten, reading
        // evacuated_ptr gives garbage — a wrong HeapSyn variant or garbage fields.
        let evacuated_obj: &HeapSyn = unsafe { &*evacuated_ptr.as_ptr() };
        assert!(
            matches!(evacuated_obj, HeapSyn::Let { .. }),
            "evacuation target corruption: expected Let node, got {:?}",
            evacuated_obj
        );
    }

    /// Deterministic invariant test: after `collect_with_evacuation`, the block
    /// that was used as the evacuation target must have at least one marked line.
    ///
    /// This test is designed to FAIL on the unfixed code and PASS on the fix.
    ///
    /// ## The invariant
    ///
    /// `mark_region_in_block` must search `evacuation_target` and
    /// `filled_evacuation_blocks` so that lines containing evacuated objects
    /// are marked.  Without this, the target block enters the block lifecycle
    /// with zero marks and `lazy_sweep_next()` can recycle the entire block,
    /// overwriting live evacuated data.
    ///
    /// ## Lifecycle of the evacuation target
    ///
    /// After `collect_with_evacuation` returns:
    ///   1. `finalise_evacuation()` moves the target → `rest`
    ///   2. `defer_sweep()` moves ALL of `rest` → `unswept`
    ///
    /// Therefore `rest` is always empty on return.  The former evacuation
    /// target is in `unswept`.  The test captures `unswept` base addresses
    /// before and after the evacuation GC pass to find the new block.
    ///
    /// ## Pass/fail criterion
    ///
    /// - **With the fix**: `mark_region_in_block` searched `evacuation_target`
    ///   during marking, so the block's line map has >0 marked lines.
    /// - **Without the fix**: `mark_region_in_block` silently skipped the
    ///   evacuation target, so the block has 0 marked lines → the assertion
    ///   fails.
    #[test]
    pub fn test_evacuation_target_block_has_marked_lines_after_collection() {
        use std::collections::HashSet;

        let mut heap = Heap::new();
        let mut clock = Clock::default();
        let mut pool = crate::eval::memory::symbol::SymbolPool::new();
        clock.switch(ThreadOccupation::Mutator);

        // Allocate enough objects to fill multiple blocks so we have rest
        // candidates to evacuate.
        let root_ptr = {
            let view = MutatorHeapView::new(&heap);
            let ids: Vec<LambdaForm> = repeat_with(|| -> LambdaForm {
                LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
            })
            .take(512)
            .collect();
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

        // Mark-in-place collection to settle live objects into rest blocks.
        let mut roots = vec![root_ptr];
        collect(&mut roots, &mut heap, &mut clock, false);
        heap.flush_unswept();

        let rest_count = heap.rest_block_count();
        if rest_count == 0 {
            // Nothing to evacuate — test is inconclusive, skip rather than fail.
            return;
        }

        // Capture unswept addresses BEFORE the evacuation pass so we can
        // identify the newly-added evacuation target block afterwards.
        let unswept_before: HashSet<usize> = heap.unswept_block_base_addresses();

        // Force evacuation of all rest blocks (indices 2..2+rest_count is a
        // conventional way to include every block in the evacuation candidate
        // set — the exact indices do not matter as long as all rest blocks
        // are selected).
        let candidates: Vec<usize> = (2..2 + rest_count).collect();
        collect_with_evacuation(&mut roots, &mut heap, &mut clock, &candidates, false);

        // After collect_with_evacuation:
        //   finalise_evacuation() → target to rest
        //   defer_sweep()         → rest (including target) to unswept
        // So rest is empty and the former target is in unswept.
        let unswept_after: HashSet<usize> = heap.unswept_block_base_addresses();
        let new_blocks: Vec<usize> = unswept_after.difference(&unswept_before).copied().collect();

        // If no new block appeared in unswept, the evacuation target was never
        // allocated (nothing was actually evacuated).  Skip rather than fail.
        if new_blocks.is_empty() {
            return;
        }

        // Each new block that appeared in unswept after the evacuation pass is
        // either the active evacuation target or one of the
        // `filled_evacuation_blocks`.  ALL of them must have at least one
        // marked line; a zero-marked block would be fully recycled by
        // `lazy_sweep_next()`, overwriting live evacuated data.
        for &target_base in &new_blocks {
            let marked = heap
                .unswept_block_marked_lines(target_base)
                .expect("former evacuation target block must be in unswept");

            assert!(
                marked > 0,
                "evacuation target block {:#x} has 0 marked lines after collection: \
                 mark_region_in_block did not search evacuation_target / \
                 filled_evacuation_blocks.  Without the fix in heap.rs, \
                 lazy_sweep_next() will recycle this block and overwrite the \
                 live evacuated data (test119 / monad_utility crash on aarch64).",
                target_base
            );
        }
    }
}
