//! Mark state
//!
//! Global setting of whether true or false indicates a mark. This is
//! flipped on every trace so as to avoid blanking out all marks.
//!

use std::sync::atomic::{AtomicBool, Ordering::SeqCst};

/// The boolean value that indicates marked (known live) objects.
pub static MARK_STATE: AtomicBool = AtomicBool::new(false);

/// Flip the boolean value that represents marked live objects.
pub fn flip_mark_state() {
    MARK_STATE.fetch_xor(true, SeqCst);
}

/// Current boolean value that represents marked live objects.
pub fn mark_state() -> bool {
    MARK_STATE.load(SeqCst)
}
