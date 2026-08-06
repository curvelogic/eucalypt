//! Interrupt-flag plumbing shared with the signal handler.
//!
//! This module used to be the HeapSyn tree-walk machine (`MachineState`,
//! `Machine`, `HeapNavigator`, `MachineBifContext`) — the STG interpreter
//! that walked heap-allocated `HeapSyn` code directly. It was deleted by the
//! Phase 4 collapse (eu-oufc): the bytecode engine (`crate::eval::bytecode`)
//! is now the sole execution engine, and its own unit test suite in
//! `bytecode/machine.rs` covers the same VM-mechanics properties this
//! module's test suite used to (atom/case/let/apply/direct-apply/partial
//! application termination, target-annotation lookthrough, IO yield/resume,
//! and `evaluate_to_whnf`'s caller-state restoration on error).
//!
//! Only the SIGINT interrupt flag — checked by the bytecode engine's run
//! loop, not specific to either engine — remained live outside that deleted
//! machine and stayed in this module rather than moving, so
//! `crate::eval::machine::vm::set_interrupted` (called from the signal
//! handler in `bin/eu.rs`) keeps its path.

use std::sync::atomic::{AtomicBool, Ordering as AtomicOrdering};

/// Global flag set by the SIGINT handler.  Checked by the VM run loop
/// every 500 steps alongside the GC check.
static INTERRUPTED: AtomicBool = AtomicBool::new(false);

/// Check whether an interrupt has been requested.
pub fn interrupted() -> bool {
    INTERRUPTED.load(AtomicOrdering::Relaxed)
}

/// Set the interrupt flag (called from the signal handler).
pub fn set_interrupted() {
    INTERRUPTED.store(true, AtomicOrdering::Relaxed);
}
