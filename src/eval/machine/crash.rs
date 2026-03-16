//! Crash diagnostics for SIGSEGV / SIGBUS signal handling.
//!
//! Provides a fixed-size, signal-safe diagnostic snapshot that is
//! updated during VM execution and read from the signal handler.
//! All data structures avoid heap allocation so they can be safely
//! read from an async-signal context.

use std::cell::{Cell, UnsafeCell};
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};

// ---------------------------------------------------------------------------
// GC Event Ring Buffer
// ---------------------------------------------------------------------------

/// Ring buffer capacity — must be a power of 2.
const GC_EVENT_RING_SIZE: usize = 64;
const GC_EVENT_RING_MASK: usize = GC_EVENT_RING_SIZE - 1;

/// Kinds of GC events we track.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum GcEventKind {
    /// Placeholder for uninitialised slots.
    Empty = 0,
    /// Start of a GC collection cycle.
    CollectionStart = 1,
    /// End of a GC collection cycle.
    CollectionEnd = 2,
    /// A block was chosen for evacuation.
    EvacuateBlock = 3,
    /// A block was swept by the lazy sweeper.
    SweepBlock = 4,
    /// A new block was allocated from the OS.
    AllocBlock = 5,
    /// An emergency collection was triggered.
    EmergencyCollection = 6,
    /// Mark phase started.
    MarkStart = 7,
    /// Mark phase ended.
    MarkEnd = 8,
}

impl GcEventKind {
    #[cfg(not(target_arch = "wasm32"))]
    fn label(self) -> &'static [u8] {
        match self {
            Self::Empty => b"empty",
            Self::CollectionStart => b"gc-start",
            Self::CollectionEnd => b"gc-end",
            Self::EvacuateBlock => b"evacuate",
            Self::SweepBlock => b"sweep",
            Self::AllocBlock => b"alloc-block",
            Self::EmergencyCollection => b"emergency",
            Self::MarkStart => b"mark-start",
            Self::MarkEnd => b"mark-end",
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn from_u8(v: u8) -> Self {
        match v {
            1 => Self::CollectionStart,
            2 => Self::CollectionEnd,
            3 => Self::EvacuateBlock,
            4 => Self::SweepBlock,
            5 => Self::AllocBlock,
            6 => Self::EmergencyCollection,
            7 => Self::MarkStart,
            8 => Self::MarkEnd,
            _ => Self::Empty,
        }
    }
}

/// A single GC event record — fixed size, no allocations.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct GcEvent {
    /// Event kind (discriminant of [`GcEventKind`]).
    pub kind: u8,
    /// Event-specific detail (e.g. block count, bytes freed).
    pub detail: u32,
    /// VM tick at the time of the event.
    pub tick: u64,
}

impl GcEvent {
    const EMPTY: Self = GcEvent {
        kind: 0,
        detail: 0,
        tick: 0,
    };
}

/// Fixed-size, lock-free ring buffer for GC events.
///
/// Uses relaxed atomics for the write position — this is fine because
/// we only need a best-effort snapshot from the signal handler, and
/// the buffer is only written from the mutator thread.
pub struct GcEventRing {
    events: UnsafeCell<[GcEvent; GC_EVENT_RING_SIZE]>,
    pos: AtomicUsize,
}

impl Default for GcEventRing {
    fn default() -> Self {
        Self::new()
    }
}

impl GcEventRing {
    pub const fn new() -> Self {
        GcEventRing {
            events: UnsafeCell::new([GcEvent::EMPTY; GC_EVENT_RING_SIZE]),
            pos: AtomicUsize::new(0),
        }
    }

    /// Record a GC event. Called from the mutator thread only.
    ///
    /// # Safety
    ///
    /// Uses `UnsafeCell` for interior mutability. Safe because:
    /// (1) the ring is only ever written from the single mutator thread,
    /// (2) signal handler reads are best-effort snapshots that tolerate
    /// torn entries.
    pub fn push(&self, kind: GcEventKind, detail: u32, tick: u64) {
        let idx = self.pos.load(Ordering::Relaxed) & GC_EVENT_RING_MASK;
        // SAFETY: single-writer (mutator thread), readers (signal handler)
        // tolerate partially-written entries.
        unsafe {
            let events = &mut *self.events.get();
            events[idx] = GcEvent {
                kind: kind as u8,
                detail,
                tick,
            };
        }
        self.pos.fetch_add(1, Ordering::Relaxed);
    }

    /// Return events in chronological order (oldest first).
    ///
    /// # Safety
    ///
    /// Safe to call from a signal handler because it only reads
    /// fixed-size arrays and an atomic counter — no allocations.
    #[cfg(any(not(target_arch = "wasm32"), test))]
    fn iter_raw(&self) -> impl Iterator<Item = GcEvent> + '_ {
        let total = self.pos.load(Ordering::Relaxed);
        let count = total.min(GC_EVENT_RING_SIZE);
        let start = if total <= GC_EVENT_RING_SIZE {
            0
        } else {
            total & GC_EVENT_RING_MASK
        };
        (0..count).map(move |i| {
            // SAFETY: reading fixed-size array, tolerant of torn reads
            unsafe { (*self.events.get())[(start + i) & GC_EVENT_RING_MASK] }
        })
    }
}

// ---------------------------------------------------------------------------
// Crash Diagnostics Snapshot
// ---------------------------------------------------------------------------

/// VM and GC state snapshot for crash diagnostics.
///
/// This struct is owned by the `Machine` and updated at key points
/// during execution. A thread-local raw pointer allows the signal
/// handler to read it without any locking.
///
/// All fields use atomics so that partial writes from the mutator
/// are not torn reads in the signal handler (on platforms where
/// atomic loads are signal-safe, which includes x86_64 and aarch64).
pub struct CrashDiagnostics {
    // VM state
    pub vm_ticks: AtomicU64,
    pub vm_allocs: AtomicU64,
    pub vm_max_stack: AtomicUsize,
    pub vm_stack_depth: AtomicUsize,

    // GC state
    pub gc_collections: AtomicU64,
    pub gc_blocks: AtomicUsize,
    pub gc_peak_blocks: AtomicUsize,
    pub gc_lobs: AtomicUsize,
    pub gc_mark_state: AtomicUsize,

    // GC event history
    pub gc_event_ring: GcEventRing,
}

impl Default for CrashDiagnostics {
    fn default() -> Self {
        Self::new()
    }
}

impl CrashDiagnostics {
    pub const fn new() -> Self {
        CrashDiagnostics {
            vm_ticks: AtomicU64::new(0),
            vm_allocs: AtomicU64::new(0),
            vm_max_stack: AtomicUsize::new(0),
            vm_stack_depth: AtomicUsize::new(0),
            gc_collections: AtomicU64::new(0),
            gc_blocks: AtomicUsize::new(0),
            gc_peak_blocks: AtomicUsize::new(0),
            gc_lobs: AtomicUsize::new(0),
            gc_mark_state: AtomicUsize::new(0),
            gc_event_ring: GcEventRing::new(),
        }
    }

    /// Update VM counters from current metrics.
    pub fn update_vm(&self, ticks: u64, allocs: u64, max_stack: usize, stack_depth: usize) {
        self.vm_ticks.store(ticks, Ordering::Relaxed);
        self.vm_allocs.store(allocs, Ordering::Relaxed);
        self.vm_max_stack.store(max_stack, Ordering::Relaxed);
        self.vm_stack_depth.store(stack_depth, Ordering::Relaxed);
    }

    /// Update GC counters from current heap state.
    pub fn update_gc(
        &self,
        collections: u64,
        blocks: usize,
        peak_blocks: usize,
        lobs: usize,
        mark_state: bool,
    ) {
        self.gc_collections.store(collections, Ordering::Relaxed);
        self.gc_blocks.store(blocks, Ordering::Relaxed);
        self.gc_peak_blocks.store(peak_blocks, Ordering::Relaxed);
        self.gc_lobs.store(lobs, Ordering::Relaxed);
        self.gc_mark_state
            .store(mark_state as usize, Ordering::Relaxed);
    }

    /// Record a GC event in the ring buffer.
    pub fn record_gc_event(&self, kind: GcEventKind, detail: u32, tick: u64) {
        self.gc_event_ring.push(kind, detail, tick);
    }
}

// ---------------------------------------------------------------------------
// Thread-local crash state pointer
// ---------------------------------------------------------------------------

thread_local! {
    static CRASH_DIAG_PTR: Cell<*const CrashDiagnostics> = const { Cell::new(std::ptr::null()) };
}

/// Register the crash diagnostics for the current thread.
///
/// Called when the `Machine` is created. The pointer must remain
/// valid for the lifetime of the `Machine`.
pub fn register_crash_diagnostics(diag: *const CrashDiagnostics) {
    CRASH_DIAG_PTR.set(diag);
}

/// Unregister crash diagnostics for the current thread.
pub fn unregister_crash_diagnostics() {
    CRASH_DIAG_PTR.set(std::ptr::null());
}

// ---------------------------------------------------------------------------
// Signal handler (native platforms only)
// ---------------------------------------------------------------------------

/// Install the SIGSEGV and SIGBUS signal handlers.
///
/// This should be called once, early in `main()`, before any
/// evaluation begins. On WASM this is a no-op.
#[cfg(not(target_arch = "wasm32"))]
pub fn install_crash_handler() {
    unsafe {
        let mut action: libc::sigaction = std::mem::zeroed();
        action.sa_sigaction = crash_signal_handler as *const () as usize;
        action.sa_flags = libc::SA_SIGINFO;
        libc::sigemptyset(&mut action.sa_mask);

        libc::sigaction(libc::SIGSEGV, &action, std::ptr::null_mut());
        libc::sigaction(libc::SIGBUS, &action, std::ptr::null_mut());
    }
}

/// No-op on WASM — signals are not available.
#[cfg(target_arch = "wasm32")]
pub fn install_crash_handler() {}

/// Signal-safe integer formatting into a fixed buffer.
///
/// Returns the number of bytes written.
#[cfg(any(not(target_arch = "wasm32"), test))]
fn format_u64(mut val: u64, buf: &mut [u8]) -> usize {
    if val == 0 {
        if !buf.is_empty() {
            buf[0] = b'0';
            return 1;
        }
        return 0;
    }
    let mut tmp = [0u8; 20]; // max digits for u64
    let mut len = 0;
    while val > 0 {
        tmp[len] = b'0' + (val % 10) as u8;
        val /= 10;
        len += 1;
    }
    let write_len = len.min(buf.len());
    for i in 0..write_len {
        buf[i] = tmp[len - 1 - i];
    }
    write_len
}

/// Signal-safe hex formatting into a fixed buffer.
///
/// Returns the number of bytes written.
#[cfg(any(not(target_arch = "wasm32"), test))]
fn format_hex(mut val: u64, buf: &mut [u8]) -> usize {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    if val == 0 {
        if buf.len() >= 3 {
            buf[0] = b'0';
            buf[1] = b'x';
            buf[2] = b'0';
            return 3;
        }
        return 0;
    }
    let mut tmp = [0u8; 16];
    let mut len = 0;
    while val > 0 {
        tmp[len] = HEX[(val & 0xf) as usize];
        val >>= 4;
        len += 1;
    }
    if buf.len() < len + 2 {
        return 0;
    }
    buf[0] = b'0';
    buf[1] = b'x';
    for i in 0..len {
        buf[2 + i] = tmp[len - 1 - i];
    }
    len + 2
}

/// Write a byte slice to stderr (signal-safe).
///
/// # Safety
///
/// Uses `libc::write` which is async-signal-safe.
#[cfg(not(target_arch = "wasm32"))]
unsafe fn write_stderr(data: &[u8]) {
    libc::write(libc::STDERR_FILENO, data.as_ptr().cast(), data.len());
}

/// Write a labelled u64 value to stderr (signal-safe).
#[cfg(not(target_arch = "wasm32"))]
unsafe fn write_field(label: &[u8], value: u64) {
    let mut buf = [0u8; 20];
    let len = format_u64(value, &mut buf);
    write_stderr(b"  ");
    write_stderr(label);
    write_stderr(b": ");
    write_stderr(&buf[..len]);
    write_stderr(b"\n");
}

/// The actual signal handler.
///
/// # Safety
///
/// This is called from the OS signal delivery mechanism. It must
/// only use async-signal-safe operations (no heap allocation, no
/// locks, no stdio). We use `libc::write` for all output.
#[cfg(not(target_arch = "wasm32"))]
unsafe extern "C" fn crash_signal_handler(
    sig: libc::c_int,
    info: *mut libc::siginfo_t,
    _ctx: *mut libc::c_void,
) {
    // Banner
    write_stderr(b"\n=== EUCALYPT CRASH DIAGNOSTICS ===\n");

    // Signal info
    let sig_name = match sig {
        libc::SIGSEGV => b"SIGSEGV" as &[u8],
        libc::SIGBUS => b"SIGBUS" as &[u8],
        _ => b"UNKNOWN" as &[u8],
    };
    write_stderr(b"Signal: ");
    write_stderr(sig_name);
    write_stderr(b"\n");

    // Faulting address
    if !info.is_null() {
        let addr = (*info).si_addr() as u64;
        let mut buf = [0u8; 20];
        let len = format_hex(addr, &mut buf);
        write_stderr(b"Faulting address: ");
        write_stderr(&buf[..len]);
        write_stderr(b"\n");
    }

    // Read diagnostics from thread-local
    let diag_ptr = CRASH_DIAG_PTR.get();

    if diag_ptr.is_null() {
        write_stderr(b"(no VM diagnostics available - crash outside VM execution)\n");
    } else {
        let diag = &*diag_ptr;

        write_stderr(b"\nVM State:\n");
        write_field(b"ticks", diag.vm_ticks.load(Ordering::Relaxed));
        write_field(b"allocs", diag.vm_allocs.load(Ordering::Relaxed));
        write_field(
            b"max stack",
            diag.vm_max_stack.load(Ordering::Relaxed) as u64,
        );
        write_field(
            b"current stack depth",
            diag.vm_stack_depth.load(Ordering::Relaxed) as u64,
        );

        write_stderr(b"\nGC State:\n");
        write_field(b"collections", diag.gc_collections.load(Ordering::Relaxed));
        write_field(b"blocks", diag.gc_blocks.load(Ordering::Relaxed) as u64);
        write_field(
            b"peak blocks",
            diag.gc_peak_blocks.load(Ordering::Relaxed) as u64,
        );
        write_field(b"LOBs", diag.gc_lobs.load(Ordering::Relaxed) as u64);
        write_field(
            b"mark state",
            diag.gc_mark_state.load(Ordering::Relaxed) as u64,
        );

        // GC event ring
        write_stderr(b"\nRecent GC events (oldest first):\n");
        let mut event_count = 0u32;
        for event in diag.gc_event_ring.iter_raw() {
            let kind = GcEventKind::from_u8(event.kind);
            if kind == GcEventKind::Empty {
                continue;
            }
            write_stderr(b"  [tick=");
            let mut buf = [0u8; 20];
            let len = format_u64(event.tick, &mut buf);
            write_stderr(&buf[..len]);
            write_stderr(b"] ");
            write_stderr(kind.label());

            if event.detail > 0 {
                write_stderr(b" detail=");
                let len = format_u64(event.detail as u64, &mut buf);
                write_stderr(&buf[..len]);
            }
            write_stderr(b"\n");
            event_count += 1;
        }
        if event_count == 0 {
            write_stderr(b"  (none)\n");
        }
    }

    write_stderr(b"\n=== END CRASH DIAGNOSTICS ===\n");

    // Re-raise the signal with default handler to get the core dump / exit
    let mut action: libc::sigaction = std::mem::zeroed();
    action.sa_sigaction = libc::SIG_DFL;
    libc::sigemptyset(&mut action.sa_mask);
    libc::sigaction(sig, &action, std::ptr::null_mut());
    libc::raise(sig);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_u64_zero() {
        let mut buf = [0u8; 20];
        let len = format_u64(0, &mut buf);
        assert_eq!(&buf[..len], b"0");
    }

    #[test]
    fn format_u64_large() {
        let mut buf = [0u8; 20];
        let len = format_u64(123456789, &mut buf);
        assert_eq!(&buf[..len], b"123456789");
    }

    #[test]
    fn format_hex_zero() {
        let mut buf = [0u8; 20];
        let len = format_hex(0, &mut buf);
        assert_eq!(&buf[..len], b"0x0");
    }

    #[test]
    fn format_hex_value() {
        let mut buf = [0u8; 20];
        let len = format_hex(0xdeadbeef, &mut buf);
        assert_eq!(&buf[..len], b"0xdeadbeef");
    }

    #[test]
    fn gc_event_ring_push_and_iterate() {
        let ring = GcEventRing::new();
        ring.push(GcEventKind::CollectionStart, 0, 100);
        ring.push(GcEventKind::MarkStart, 5, 101);
        ring.push(GcEventKind::CollectionEnd, 3, 102);

        let events: Vec<_> = ring.iter_raw().collect();
        assert_eq!(events.len(), 3);
        assert_eq!(events[0].kind, GcEventKind::CollectionStart as u8);
        assert_eq!(events[0].tick, 100);
        assert_eq!(events[1].kind, GcEventKind::MarkStart as u8);
        assert_eq!(events[2].kind, GcEventKind::CollectionEnd as u8);
    }

    #[test]
    fn gc_event_ring_wraps() {
        let ring = GcEventRing::new();
        // Fill past capacity
        for i in 0..100u64 {
            ring.push(GcEventKind::SweepBlock, i as u32, i);
        }
        let events: Vec<_> = ring.iter_raw().collect();
        assert_eq!(events.len(), GC_EVENT_RING_SIZE);
        // Oldest should be tick 36 (100 - 64)
        assert_eq!(events[0].tick, 36);
        // Newest should be tick 99
        assert_eq!(events[GC_EVENT_RING_SIZE - 1].tick, 99);
    }

    #[test]
    fn crash_diagnostics_update() {
        let diag = CrashDiagnostics::new();
        diag.update_vm(1000, 500, 32, 16);
        diag.update_gc(5, 10, 12, 2, true);

        assert_eq!(diag.vm_ticks.load(Ordering::Relaxed), 1000);
        assert_eq!(diag.vm_allocs.load(Ordering::Relaxed), 500);
        assert_eq!(diag.gc_collections.load(Ordering::Relaxed), 5);
        assert_eq!(diag.gc_blocks.load(Ordering::Relaxed), 10);
    }
}
