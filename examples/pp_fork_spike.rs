//! PP fork-safety feasibility spike (eu-u9xj.6 §9 gate) — THROWAWAY PROTOTYPE
//!
//! Proves (or disproves) the mechanism in
//! docs/superpowers/specs/2026-07-25-pp-parallelism-design.md §4:
//! `fork()` from a process with a live managed-GC heap, evaluate in the
//! child's COW copy, pass a fully-forced value back through an
//! `mmap(MAP_SHARED | MAP_ANON)` arena, `waitpid` and read in the parent.
//!
//! Three phases, each its own fork:
//!
//!  A. Heap-level: live `Heap` with rooted data, child allocates hard and
//!     runs several full collections in the inherited COW heap, checksums
//!     rooted data, writes the checksum to the arena; parent does the same
//!     post-join in its own copy. Run under EU_GC_VERIFY=2 / EU_GC_POISON=1.
//!
//!  B. VM-level: a full `Machine` with the standard runtime globals loaded,
//!     parent evaluates to termination, forks; the child re-enters the
//!     machine thousands of times (load STG → run → native return),
//!     forcing machine-level GC against machine roots in the COW heap, and
//!     writes its result sum + collection count to the arena.
//!
//!  C. Cost: median fork+waitpid round-trip and ns/record for length-prefixed
//!     small-value writes into the shared arena (feeds spec §7 thresholds).
//!
//! The process mimics the real `eu` binary's thread shape before forking:
//! crash handler installed, a ctrl-c handler thread (ctrlc crate), and a
//! parked thread standing in for the join-blocked initial thread.
//!
//! THIS IS A SPIKE. Not wired into cargo test; not for merge to master.

#[cfg(not(unix))]
fn main() {
    eprintln!("pp_fork_spike is unix-only");
}

#[cfg(unix)]
fn main() {
    unix_spike::main()
}

#[cfg(unix)]
mod unix_spike {
    use std::cell::RefCell;
    use std::panic::{catch_unwind, AssertUnwindSafe};
    use std::ptr::NonNull;
    use std::rc::Rc;
    use std::time::Instant;

    use eucalypt::common::sourcemap::SourceMap;
    use eucalypt::eval::emit::NullEmitter;
    use eucalypt::eval::machine::env::{EnvFrame, SynClosure};
    use eucalypt::eval::machine::metrics::{Clock, ThreadOccupation};
    use eucalypt::eval::machine::standard_machine;
    use eucalypt::eval::machine::vm::Machine;
    use eucalypt::eval::memory::collect::collect;
    use eucalypt::eval::memory::gc_debug;
    use eucalypt::eval::memory::heap::Heap;
    use eucalypt::eval::memory::loader::load;
    use eucalypt::eval::memory::mutator::{Mutator, MutatorHeapView};
    use eucalypt::eval::memory::symbol::SymbolPool;
    use eucalypt::eval::memory::syntax::{HeapSyn, Native, Ref as HeapRef, RefPtr, StgBuilder};
    use eucalypt::eval::stg::syntax::{dsl, StgSyn};
    use eucalypt::eval::stg::{make_standard_runtime, StgSettings};

    // ---------------------------------------------------------------
    // Shared arena (spec §6a): anonymous MAP_SHARED, created pre-fork
    // ---------------------------------------------------------------

    struct Arena {
        base: *mut u8,
        size: usize,
    }

    impl Arena {
        fn new(size: usize) -> Arena {
            let base = unsafe {
                libc::mmap(
                    std::ptr::null_mut(),
                    size,
                    libc::PROT_READ | libc::PROT_WRITE,
                    libc::MAP_SHARED | libc::MAP_ANON,
                    -1,
                    0,
                )
            };
            assert!(
                base != libc::MAP_FAILED,
                "mmap(MAP_SHARED|MAP_ANON) failed: {}",
                std::io::Error::last_os_error()
            );
            Arena {
                base: base as *mut u8,
                size,
            }
        }

        fn write_u64(&self, offset: usize, v: u64) {
            assert!(offset + 8 <= self.size);
            unsafe {
                (self.base.add(offset) as *mut u64).write_unaligned(v);
            }
        }

        fn read_u64(&self, offset: usize) -> u64 {
            assert!(offset + 8 <= self.size);
            unsafe { (self.base.add(offset) as *const u64).read_unaligned() }
        }

        fn write_bytes(&self, offset: usize, bytes: &[u8]) {
            assert!(offset + 8 + bytes.len() <= self.size);
            self.write_u64(offset, bytes.len() as u64);
            unsafe {
                std::ptr::copy_nonoverlapping(
                    bytes.as_ptr(),
                    self.base.add(offset + 8),
                    bytes.len(),
                );
            }
        }

        fn read_bytes(&self, offset: usize) -> Vec<u8> {
            let len = self.read_u64(offset) as usize;
            assert!(offset + 8 + len <= self.size);
            let mut out = vec![0u8; len];
            unsafe {
                std::ptr::copy_nonoverlapping(self.base.add(offset + 8), out.as_mut_ptr(), len);
            }
            out
        }
    }

    impl Drop for Arena {
        fn drop(&mut self) {
            unsafe {
                libc::munmap(self.base as *mut libc::c_void, self.size);
            }
        }
    }

    // Arena slot layout (offsets in bytes)
    const SLOT_STATUS: usize = 0; // magic written by child on success
    const SLOT_A_CHECKSUM: usize = 8; // phase A: child checksum of rooted data
    const SLOT_A_COLLECTIONS: usize = 16; // phase A: child collection count
    const SLOT_B_SUM: usize = 24; // phase B: child result sum
    const SLOT_B_COLLECTIONS: usize = 32; // phase B: child machine collections
    const SLOT_B_TICKS: usize = 40; // phase B: child machine ticks
    const SLOT_STR: usize = 48; // length-prefixed string record
    const SLOT_BULK: usize = 4096; // phase C bulk record region

    const STATUS_OK: u64 = 0x50505f4f4b21_u64; // "PP_OK!"

    // ---------------------------------------------------------------
    // fork/wait helpers
    // ---------------------------------------------------------------

    /// Fork; run `child` in the child and `_exit` with its code
    /// (0 = success, 42 = panic); return the child's exit status to
    /// the parent after waitpid.
    fn fork_run(child: impl FnOnce()) -> i32 {
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0, "fork failed: {}", std::io::Error::last_os_error());
        if pid == 0 {
            // CHILD. Never unwind out, never run parent Drops/atexit:
            // always leave via _exit.
            let result = catch_unwind(AssertUnwindSafe(child));
            let code = match result {
                Ok(()) => 0,
                Err(_) => {
                    eprintln!("[child] PANIC during child phase");
                    42
                }
            };
            unsafe { libc::_exit(code) };
        }
        let mut status: libc::c_int = 0;
        let r = unsafe { libc::waitpid(pid, &mut status, 0) };
        assert_eq!(
            r,
            pid,
            "waitpid failed: {}",
            std::io::Error::last_os_error()
        );
        if libc::WIFEXITED(status) {
            libc::WEXITSTATUS(status)
        } else if libc::WIFSIGNALED(status) {
            eprintln!("[parent] child killed by signal {}", libc::WTERMSIG(status));
            -libc::WTERMSIG(status)
        } else {
            -999
        }
    }

    // ---------------------------------------------------------------
    // Phase A: heap-level COW fork
    // ---------------------------------------------------------------

    /// Allocate `n` HeapSyn number atoms starting at `start`, returning
    /// their pointers (roots).
    fn alloc_num_atoms(heap: &Heap, start: u64, n: u64) -> Vec<NonNull<HeapSyn>> {
        let view = MutatorHeapView::new(heap);
        (start..start + n)
            .map(|i| {
                view.atom(HeapRef::V(Native::Num(serde_json::Number::from(i))))
                    .expect("alloc failed")
                    .as_ptr()
            })
            .collect()
    }

    /// Sum the numeric payload of rooted atoms (follows pointers the
    /// collector may have updated).
    fn checksum(heap: &Heap, roots: &[NonNull<HeapSyn>]) -> u64 {
        let view = MutatorHeapView::new(heap);
        roots
            .iter()
            .map(|p| {
                let scoped = view.scoped(*p);
                match &*scoped {
                    HeapSyn::Atom {
                        evaluand: HeapRef::V(Native::Num(n)),
                    } => n.as_u64().expect("non-u64 payload"),
                    other => panic!("root no longer a num atom: {:?}", other),
                }
            })
            .sum()
    }

    fn phase_a(arena: &Arena, do_fork: bool) {
        eprintln!("== Phase A: heap-level COW fork (fork={do_fork}) ==");
        let mut heap = Heap::new();
        let mut clock = Clock::default();
        clock.switch(ThreadOccupation::Mutator);

        const N_ROOTS: u64 = 50_000;
        let mut roots = alloc_num_atoms(&heap, 0, N_ROOTS);
        let expected: u64 = (0..N_ROOTS).sum();

        // some garbage too
        let _garbage = alloc_num_atoms(&heap, 0, 50_000);

        // pre-fork collection: baseline must be green before we fork
        collect(&mut roots, &mut heap, &mut clock, false);
        assert_eq!(checksum(&heap, &roots), expected, "pre-fork checksum");
        let parent_collections_before = heap.stats().collections_count;
        eprintln!(
            "[parent] pre-fork: {} roots, checksum ok, {} collections, verify level {}",
            N_ROOTS,
            parent_collections_before,
            gc_debug::verify_level()
        );

        let mut child_work = || {
            // CHILD (or inline control): allocate hard in the (inherited COW)
            // heap, collect repeatedly (each collect runs EU_GC_VERIFY
            // checkpoints), then checksum the inherited roots.
            let mut child_roots = roots.clone();
            for round in 0..5 {
                let extra = alloc_num_atoms(&heap, 1_000_000, 100_000);
                // root the new data for one round as survivors
                child_roots.extend(extra.iter().take(10_000).copied());
                collect(&mut child_roots, &mut heap, &mut clock, false);
                child_roots.truncate(roots.len());
                let _ = round;
            }
            let sum = checksum(&heap, &child_roots);
            arena.write_u64(SLOT_A_CHECKSUM, sum);
            arena.write_u64(SLOT_A_COLLECTIONS, heap.stats().collections_count);
            arena.write_bytes(SLOT_STR, b"hello from the COW child heap");
            arena.write_u64(SLOT_STATUS, STATUS_OK);
        };
        if do_fork {
            let code = fork_run(child_work);
            assert_eq!(code, 0, "phase A child failed (exit {code})");
        } else {
            // control: identical workload, same process — distinguishes
            // fork-induced GC issues from artefacts of the workload itself
            child_work();
        }

        // parent reads results from the shared arena
        assert_eq!(arena.read_u64(SLOT_STATUS), STATUS_OK, "child status");
        assert_eq!(arena.read_u64(SLOT_A_CHECKSUM), expected, "child checksum");
        let child_collections = arena.read_u64(SLOT_A_COLLECTIONS);
        assert!(
            child_collections > parent_collections_before,
            "child ran no collections"
        );
        let msg = String::from_utf8(arena.read_bytes(SLOT_STR)).unwrap();
        assert_eq!(msg, "hello from the COW child heap");

        if do_fork {
            // parent's own heap must be untouched by child GC: collect + checksum
            // (skipped in the no-fork control, where the inline collects have
            // legitimately moved objects away from the parent root pointers)
            collect(&mut roots, &mut heap, &mut clock, false);
            assert_eq!(
                checksum(&heap, &roots),
                expected,
                "post-join parent checksum"
            );
        }
        eprintln!(
            "[parent] phase A OK: child ran {} collections (verify level {}), \
             arena round-trip ok{}",
            child_collections,
            gc_debug::verify_level(),
            if do_fork {
                ", parent heap intact post-join"
            } else {
                " (no-fork control)"
            }
        );
    }

    // ---------------------------------------------------------------
    // Phase B: full VM COW fork
    // ---------------------------------------------------------------

    /// Mutator that loads STG syntax into the machine heap as a closure.
    /// NB: fresh SymbolPool per load — safe only because the loaded
    /// syntax contains no symbols.
    struct LoadSyn {
        syntax: Rc<StgSyn>,
    }

    impl Mutator for LoadSyn {
        type Input = RefPtr<EnvFrame>;
        type Output = SynClosure;

        fn run(
            &self,
            view: &MutatorHeapView,
            input: Self::Input,
        ) -> Result<Self::Output, eucalypt::eval::error::ExecutionError> {
            let mut pool = SymbolPool::new();
            Ok(SynClosure::new(
                load(view, &mut pool, self.syntax.clone())?,
                input,
            ))
        }
    }

    /// `let id = \x -> x; b = ~box_num(i) in case id b of BoxedNum n -> n`
    /// — forces a thunk through the full step machinery and returns an
    /// unboxed native number.
    fn compute_syntax(i: u64) -> Rc<StgSyn> {
        use dsl::*;
        let_(
            vec![lambda(1, local(0)), thunk(box_num(i as i64))],
            unbox_num(app(lref(0), vec![lref(1)]), atom(lref(0))),
        )
    }

    /// Run one computation on the machine, returning the numeric result.
    ///
    /// Results are kept in 0..=255 so we can read them back through the
    /// public `run()` exit-code path (numbers 0-255 pass through as-is);
    /// `native_return()` is `#[cfg(test)]`-only.
    fn run_compute(machine: &mut Machine<'_>, i: u64) -> u64 {
        assert!(i <= 255);
        let closure = machine
            .mutate(
                LoadSyn {
                    syntax: compute_syntax(i),
                },
                machine.root_env(),
            )
            .expect("load failed");
        machine.resume_for_render(closure);
        let code = machine.run(None).expect("machine error");
        code.expect("machine did not terminate") as u64
    }

    fn phase_b(arena: &Arena, do_fork: bool) {
        eprintln!("== Phase B: full VM COW fork (fork={do_fork}) ==");
        let mut source_map = SourceMap::default();
        let rt = make_standard_runtime(&mut source_map);
        let settings = StgSettings {
            generate_annotations: false,
            trace_steps: false,
            // small limit so the in-run GC policy actually fires during
            // the child's iterations (limit is in heap blocks derived
            // from MiB — the workload allocates well past this); kept
            // small because EU_GC_VERIFY=2 makes every collection run a
            // full structural verification
            heap_limit_mib: Some(4),
            ..Default::default()
        };

        // machine with the full standard runtime globals loaded; run the
        // trivial initial program to termination → quiescent point.
        let mut machine = standard_machine(
            &settings,
            dsl::atom(dsl::num(1)),
            Box::new(NullEmitter),
            rt.as_ref(),
        )
        .expect("machine construction failed");
        machine.run(None).expect("initial run failed");

        // parent-side sanity pre-fork
        assert_eq!(run_compute(&mut machine, 41), 41);
        let parent_collections_before = machine.heap_stats().collections_count;
        eprintln!(
            "[parent] pre-fork: machine live, sanity compute ok, {} collections",
            parent_collections_before
        );

        // tunable: EU_GC_STRESS makes every collection evacuate + verify,
        // which is orders of magnitude slower — use PP_SPIKE_ITERS to keep
        // the stress run tractable while still forcing many collections
        let child_iters: u64 = std::env::var("PP_SPIKE_ITERS")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(20_000);
        // sum of (i % 200) + 42 for i in 0..child_iters (kept in 0..=255
        // for the exit-code read-back path)
        let expected_sum: u64 = (0..child_iters).map(|i| (i % 200) + 42).sum();

        let machine_cell = RefCell::new(machine);

        let child_work = || {
            // CHILD (or inline control): re-enter the inherited machine over
            // and over — every iteration loads fresh STG into the COW heap
            // and evaluates, machine-level GC fires against machine roots
            // when policy demands (with EU_GC_VERIFY checkpoints).
            let mut machine = machine_cell.borrow_mut();
            let mut sum = 0u64;
            for i in 0..child_iters {
                sum += run_compute(&mut machine, (i % 200) + 42);
            }
            arena.write_u64(SLOT_B_SUM, sum);
            arena.write_u64(SLOT_B_COLLECTIONS, machine.heap_stats().collections_count);
            arena.write_u64(SLOT_B_TICKS, machine.metrics().ticks());
            arena.write_u64(SLOT_STATUS, STATUS_OK + 1);
        };
        if do_fork {
            let code = fork_run(child_work);
            assert_eq!(code, 0, "phase B child failed (exit {code})");
        } else {
            child_work();
        }
        assert_eq!(arena.read_u64(SLOT_STATUS), STATUS_OK + 1, "child status");
        assert_eq!(arena.read_u64(SLOT_B_SUM), expected_sum, "child result sum");
        let child_collections = arena.read_u64(SLOT_B_COLLECTIONS);
        let child_ticks = arena.read_u64(SLOT_B_TICKS);

        // parent machine must still work post-join
        let mut machine = machine_cell.into_inner();
        for n in 0..5_000u64 {
            let i = n % 256;
            assert_eq!(run_compute(&mut machine, i), i);
        }
        eprintln!(
            "[parent] phase B OK ({}): child ran {} iters / {} ticks / {} machine GCs \
             (parent had {}), correct sum through arena, parent machine intact post-join",
            if do_fork { "fork" } else { "no-fork control" },
            child_iters,
            child_ticks,
            child_collections,
            parent_collections_before
        );
        if child_collections == parent_collections_before {
            eprintln!(
                "[parent] WARNING: no machine-level GC fired in child — \
                 increase child_iters for a conclusive GC-under-fork result"
            );
        }
    }

    // ---------------------------------------------------------------
    // Phase D: W concurrent workers over one inherited machine
    // (the actual par-sum shape from spec §4: fork W, each evaluates
    // its contiguous chunk in its own COW heap, writes one partial per
    // worker into its arena slot, parent joins and combines in order)
    // ---------------------------------------------------------------

    fn phase_d(arena: &Arena) {
        eprintln!("== Phase D: W=4 concurrent COW workers (par-sum shape) ==");
        let mut source_map = SourceMap::default();
        let rt = make_standard_runtime(&mut source_map);
        let settings = StgSettings {
            generate_annotations: false,
            trace_steps: false,
            heap_limit_mib: Some(4),
            ..Default::default()
        };
        let mut machine = standard_machine(
            &settings,
            dsl::atom(dsl::num(1)),
            Box::new(NullEmitter),
            rt.as_ref(),
        )
        .expect("machine construction failed");
        machine.run(None).expect("initial run failed");

        const W: u64 = 4;
        const CHUNK: u64 = 5_000;
        let expected: u64 = (0..W * CHUNK).map(|i| (i % 200) + 42).sum();

        let machine_cell = RefCell::new(machine);
        let t = Instant::now();
        let mut pids = Vec::new();
        for w in 0..W {
            let pid = unsafe { libc::fork() };
            assert!(pid >= 0, "fork failed");
            if pid == 0 {
                // WORKER w: contiguous chunk, local partial reduction,
                // one partial into slot w.
                let result = catch_unwind(AssertUnwindSafe(|| {
                    let mut machine = machine_cell.borrow_mut();
                    let mut partial = 0u64;
                    for i in w * CHUNK..(w + 1) * CHUNK {
                        partial += run_compute(&mut machine, (i % 200) + 42);
                    }
                    arena.write_u64(SLOT_BULK + (w as usize) * 16, partial);
                    arena.write_u64(
                        SLOT_BULK + (w as usize) * 16 + 8,
                        machine.heap_stats().collections_count,
                    );
                }));
                unsafe { libc::_exit(if result.is_ok() { 0 } else { 42 }) };
            }
            pids.push(pid);
        }
        for pid in &pids {
            let mut status: libc::c_int = 0;
            let r = unsafe { libc::waitpid(*pid, &mut status, 0) };
            assert_eq!(r, *pid, "waitpid failed");
            assert!(
                libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0,
                "worker failed: status {status}"
            );
        }
        let elapsed = t.elapsed();

        let mut total = 0u64;
        let mut collections = Vec::new();
        for w in 0..W as usize {
            total += arena.read_u64(SLOT_BULK + w * 16);
            collections.push(arena.read_u64(SLOT_BULK + w * 16 + 8));
        }
        assert_eq!(total, expected, "combined worker partials");

        // parent machine still healthy after joining all workers
        let mut machine = machine_cell.into_inner();
        for n in 0..1_000u64 {
            let i = n % 256;
            assert_eq!(run_compute(&mut machine, i), i);
        }
        eprintln!(
            "[parent] phase D OK: {} workers x {} items in {:?}, worker GC counts {:?}, \
             partials combine to expected, parent machine intact",
            W, CHUNK, elapsed, collections
        );
    }

    // ---------------------------------------------------------------
    // Phase C: costs
    // ---------------------------------------------------------------

    fn phase_c(arena: &Arena) {
        eprintln!("== Phase C: fork + serialisation cost ==");

        // fork+waitpid round trip with a trivial child
        let mut samples = Vec::with_capacity(20);
        for _ in 0..20 {
            let t = Instant::now();
            let code = fork_run(|| {});
            assert_eq!(code, 0);
            samples.push(t.elapsed());
        }
        samples.sort();
        let median = samples[samples.len() / 2];

        // serialisation throughput: 16-byte records into the shared arena
        const RECORDS: usize = 1_000_000;
        let region = SLOT_BULK;
        let t = Instant::now();
        for i in 0..RECORDS {
            let off = region + i * 16;
            arena.write_u64(off, i as u64);
            arena.write_u64(off + 8, (i as u64).wrapping_mul(31));
        }
        let write_elapsed = t.elapsed();
        let t = Instant::now();
        let mut acc = 0u64;
        for i in 0..RECORDS {
            let off = region + i * 16;
            acc = acc.wrapping_add(arena.read_u64(off) ^ arena.read_u64(off + 8));
        }
        let read_elapsed = t.elapsed();
        assert_ne!(acc, 0);

        eprintln!(
            "[parent] fork+waitpid round-trip median: {:?} (min {:?}, max {:?})",
            median,
            samples[0],
            samples[samples.len() - 1]
        );
        eprintln!(
            "[parent] arena write: {} x 16B records in {:?} ({:.1} ns/record); \
             read back in {:?} ({:.1} ns/record)",
            RECORDS,
            write_elapsed,
            write_elapsed.as_nanos() as f64 / RECORDS as f64,
            read_elapsed,
            read_elapsed.as_nanos() as f64 / RECORDS as f64,
        );
    }

    // ---------------------------------------------------------------

    pub fn main() {
        // Mimic the real `eu` binary's pre-fork thread/handler shape:
        // crash handler + ctrl-c handler thread + a parked thread standing
        // in for the join-blocked initial thread.
        eucalypt::eval::machine::crash::install_crash_handler();
        ctrlc::set_handler(|| {}).expect("ctrlc handler");
        std::thread::spawn(|| loop {
            std::thread::park();
        });

        eprintln!(
            "pp_fork_spike: EU_GC_VERIFY={} EU_GC_POISON={}",
            gc_debug::verify_level(),
            gc_debug::poison_enabled()
        );

        // --nofork: run the identical workloads inline (no fork) as a
        // control, to separate fork-induced GC issues from artefacts of
        // the workload / verification machinery itself.
        let do_fork = !std::env::args().any(|a| a == "--nofork");

        let arena = Arena::new(64 * 1024 * 1024); // spec §6a: over-provision virtual

        phase_a(&arena, do_fork);
        phase_b(&arena, do_fork);
        if do_fork {
            phase_d(&arena);
            phase_c(&arena);
        }

        eprintln!("pp_fork_spike: ALL PHASES PASSED");
    }
}
