//! GC-root frames for handles an intrinsic must hold across `force()`.
//!
//! `force()` runs the machine and the machine collects mid-run. An
//! `AbiClosure` sitting on the Rust stack is invisible to the collector — it
//! is neither the current closure nor on the continuation stack — so
//! evacuation moves the object behind it and leaves the handle dangling. The
//! rule the rest of the tree follows is "no handle is held across a force"
//! (`src/eval/stg/list.rs`); where that is impossible, the machine's root set
//! is used instead (`src/driver/io_run.rs`).
//!
//! The parallel driver and value serialiser genuinely must accumulate handles
//! across forces — mapping N elements means holding N results — so they use
//! [`IntrinsicMachine::gc_root_push`] and read every handle back from the root
//! set after each force. [`with_roots`] scopes the pushes so the frame is
//! released on every exit path, errors included.

use crate::eval::{error::ExecutionError, machine::intrinsic::IntrinsicMachine};

/// Run `body` with a fresh root frame, truncating the machine's root set back
/// to its entry length afterwards — on the error path too.
///
/// Nesting is safe: `force()` pushes and pops its own entries, and inner
/// frames restore the length they found, so indices handed out by an outer
/// frame stay valid.
pub fn with_roots<T>(
    machine: &mut dyn IntrinsicMachine,
    body: impl FnOnce(&mut dyn IntrinsicMachine) -> Result<T, ExecutionError>,
) -> Result<T, ExecutionError> {
    let base = machine.gc_root_len();
    let result = body(&mut *machine);
    machine.gc_root_truncate(base);
    result
}
