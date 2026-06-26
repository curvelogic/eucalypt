//! Bytecode encoding and dispatch spike (BV0).
//!
//! This module implements a flat bytecode interpreter alongside the
//! existing `HeapSyn` tree-walk VM.  It encodes `StgSyn` into a
//! contiguous `Vec<u8>` opcode stream and dispatches via `match` on
//! `u8` opcodes — testing whether cache-friendly flat dispatch is
//! significantly faster than pointer-chasing through heap-allocated
//! `HeapSyn` nodes.
//!
//! All code in this module is experimental and does not modify the
//! production `HeapSyn` VM.

pub mod encode;
pub mod interp;
pub mod opcode;
pub mod program;
