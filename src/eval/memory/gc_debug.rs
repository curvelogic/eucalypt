//! GC debugging and diagnostic tools.
//!
//! Provides opt-in diagnostic modes controlled by environment variables:
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
//!
//! - `EU_GC_VERIFY=2` — Full multi-checkpoint structural verification of
//!   the heap during garbage collection. Includes header validation,
//!   pointer validity checks, line mark consistency, forwarding pointer
//!   lifecycle, and block list integrity. Level 2 implies level 1.

use std::ptr::NonNull;
use std::sync::atomic::{AtomicBool, AtomicU8, Ordering};

/// Poison byte pattern used to fill swept memory.
pub const POISON_BYTE: u8 = 0xDE;

/// Cached flags — checked once at process start.
static GC_POISON_ENABLED: AtomicBool = AtomicBool::new(false);
static GC_VERIFY_LEVEL: AtomicU8 = AtomicU8::new(0);
static FLAGS_INITIALISED: AtomicBool = AtomicBool::new(false);

/// Initialise GC debug flags from environment variables.
///
/// Called lazily on first use.  Thread-safe (idempotent, acquire/release
/// ordering ensures the flag stores are visible before `FLAGS_INITIALISED`).
fn ensure_initialised() {
    if !FLAGS_INITIALISED.load(Ordering::Acquire) {
        GC_POISON_ENABLED.store(std::env::var("EU_GC_POISON").is_ok(), Ordering::Relaxed);
        let level = std::env::var("EU_GC_VERIFY")
            .ok()
            .and_then(|v| v.parse::<u8>().ok())
            .unwrap_or_else(|| {
                // For backward compatibility: if the var is set but not
                // a valid number, treat it as level 1 (the old boolean
                // behaviour).
                if std::env::var("EU_GC_VERIFY").is_ok() {
                    1
                } else {
                    0
                }
            });
        GC_VERIFY_LEVEL.store(level, Ordering::Relaxed);
        FLAGS_INITIALISED.store(true, Ordering::Release);
    }
}

/// Returns `true` when swept-memory poisoning is enabled.
#[inline]
pub fn poison_enabled() -> bool {
    ensure_initialised();
    GC_POISON_ENABLED.load(Ordering::Relaxed)
}

/// Returns the GC verification level (0 = off, 1 = post-mark, 2 = full).
#[inline]
pub fn verify_level() -> u8 {
    ensure_initialised();
    GC_VERIFY_LEVEL.load(Ordering::Relaxed)
}

/// Returns `true` when post-mark verification is enabled (level >= 1).
///
/// Backward-compatible wrapper around `verify_level()`.
#[inline]
pub fn verify_enabled() -> bool {
    verify_level() >= 1
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
