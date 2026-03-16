//! GC debugging and diagnostic tools.
//!
//! Provides two opt-in diagnostic modes controlled by environment variables:
//!
//! - `EU_GC_POISON=1` — Fill swept (reclaimed) memory with a poison pattern
//!   (`0xDE`). Before marking, check if the object looks poisoned. This
//!   detects use-after-free: a live pointer leading to poisoned memory means
//!   the target was swept while still referenced.
//!
//! - `EU_GC_VERIFY=1` — After the mark phase (before `defer_sweep`), walk
//!   all marked objects and verify that every pointer they contain also
//!   points to a marked object.  Any unmarked-but-referenced object is
//!   about to be incorrectly swept.

use std::ptr::NonNull;
use std::sync::atomic::{AtomicBool, Ordering};

/// Poison byte pattern used to fill swept memory.
pub const POISON_BYTE: u8 = 0xDE;

/// Cached flags — checked once at process start.
static GC_POISON_ENABLED: AtomicBool = AtomicBool::new(false);
static GC_VERIFY_ENABLED: AtomicBool = AtomicBool::new(false);
static FLAGS_INITIALISED: AtomicBool = AtomicBool::new(false);

/// Initialise GC debug flags from environment variables.
///
/// Called lazily on first use.  Thread-safe (idempotent, relaxed atomics).
fn ensure_initialised() {
    if !FLAGS_INITIALISED.load(Ordering::Relaxed) {
        GC_POISON_ENABLED.store(std::env::var("EU_GC_POISON").is_ok(), Ordering::Relaxed);
        GC_VERIFY_ENABLED.store(std::env::var("EU_GC_VERIFY").is_ok(), Ordering::Relaxed);
        FLAGS_INITIALISED.store(true, Ordering::Relaxed);
    }
}

/// Returns `true` when swept-memory poisoning is enabled.
#[inline]
pub fn poison_enabled() -> bool {
    ensure_initialised();
    GC_POISON_ENABLED.load(Ordering::Relaxed)
}

/// Returns `true` when post-mark verification is enabled.
#[inline]
pub fn verify_enabled() -> bool {
    ensure_initialised();
    GC_VERIFY_ENABLED.load(Ordering::Relaxed)
}

/// Fill a memory region with the poison pattern.
///
/// # Safety
///
/// `ptr` must point to `len` bytes of writable memory.
pub unsafe fn poison_region(ptr: *mut u8, len: usize) {
    std::ptr::write_bytes(ptr, POISON_BYTE, len);
}

/// Check whether the first 8 bytes at `ptr` look poisoned.
///
/// Returns `true` if the memory is filled with `POISON_BYTE`,
/// indicating a use-after-free.
///
/// # Safety
///
/// `ptr` must point to at least 8 readable bytes.
pub unsafe fn looks_poisoned(ptr: NonNull<u8>) -> bool {
    let bytes = std::slice::from_raw_parts(ptr.as_ptr(), 8);
    bytes.iter().all(|&b| b == POISON_BYTE)
}
