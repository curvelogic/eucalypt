//! Native flat-bytecode execution engine (BV1). See
//! docs/superpowers/specs/2026-07-01-bv1-bytecode-vm-design.md.

/// A code reference: a byte offset into a `BytecodeProgram.code` buffer.
/// Unlike `RefPtr<HeapSyn>`, a `CodeRef` never moves, so it carries zero
/// GC cost — closures holding one need no `scan_and_update` code fixup.
pub type CodeRef = u32;

mod opcode;
pub use opcode::*;

mod program;
pub use program::*;

mod encode;
pub use encode::*;

mod closure;
pub use closure::*;

mod cont;
pub use cont::*;

mod env_builder;
pub use env_builder::*;

mod predecode;
pub use predecode::*;

mod machine;
pub use machine::*;

#[cfg(test)]
mod differential;

/// Whether the bytecode engine is selected for this run. Wired into
/// `driver/eval.rs` for pure programs.
///
/// As of BV1 (eu-enyv) the bytecode engine is the **default**. Set
/// `EU_HEAPSYN=1` to opt out and select the legacy HeapSyn machine, which is
/// retained as the performance baseline and differential-testing engine
/// (Phase 4 collapse is deferred pending an A/B perf study).
///
/// `EU_BYTECODE=1` is still accepted as a now-redundant explicit opt-in so
/// existing invocations keep working; `EU_HEAPSYN=1` takes precedence over it.
pub fn bytecode_enabled() -> bool {
    !heapsyn_enabled()
}

/// Whether the legacy HeapSyn engine has been explicitly requested via the
/// `EU_HEAPSYN=1` opt-out.
pub fn heapsyn_enabled() -> bool {
    std::env::var("EU_HEAPSYN").as_deref() == Ok("1")
}

/// Whether the pre-decoded execution IR (lever a, bead eu-2sa6.13) is selected
/// via `EU_PREDECODE=1`. When set, the bytecode engine decodes the whole
/// program once at load into a flat [`DecodedProgram`] of typed [`Instr`]
/// records plus off-heap side pools, and dispatches over the typed fields
/// instead of re-reading the byte stream every tick. Flag OFF ⇒ byte-identical
/// current behaviour (the byte path is untouched). Only meaningful when the
/// bytecode engine is selected (`EU_HEAPSYN=1` takes precedence).
pub fn predecode_enabled() -> bool {
    std::env::var("EU_PREDECODE").as_deref() == Ok("1")
}
