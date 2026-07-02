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

/// Whether the bytecode engine is selected for this run. Reads the
/// `EU_BYTECODE` env var; a CLI flag is added in Phase 2.
// Removed when wired in Phase 2; inert scaffolding for now.
#[allow(dead_code)]
pub fn bytecode_enabled() -> bool {
    std::env::var("EU_BYTECODE").as_deref() == Ok("1")
}
