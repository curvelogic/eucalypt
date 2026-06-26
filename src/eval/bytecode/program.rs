//! Bytecode program structure: flat opcode buffer + constant pool.

use crate::eval::{
    memory::{
        alloc::ScopedAllocator,
        mutator::MutatorHeapView,
        string::HeapString,
        symbol::SymbolPool,
        syntax::{Native, Ref},
    },
    stg::syntax::Native as StgNative,
};

/// A compiled bytecode program.
///
/// Code is a flat `Vec<u8>` of opcodes and inline operands.
/// Constants holds native values referenced by `ATOM_V` operands.
/// The `heap_refs` field is populated after machine initialisation to
/// hold pre-converted `Ref` values (with interned symbols and
/// heap-allocated strings).
#[derive(Debug, Default)]
pub struct BytecodeProgram {
    /// Flat opcode stream.
    pub code: Vec<u8>,
    /// Constant pool: STG-level native values (strings, symbols, numbers).
    pub constants: Vec<StgNative>,
    /// Pre-converted heap-level refs, one per constant.
    /// Populated by `prepare_constants()` after the machine heap and
    /// symbol pool are available.
    pub heap_refs: Vec<Ref>,
}

impl BytecodeProgram {
    /// Pre-convert all constant pool entries from `StgNative` to
    /// heap-level `Ref` values.
    ///
    /// Must be called after the machine's symbol pool is populated
    /// (i.e., after `standard_machine()` has loaded globals).
    pub fn prepare_constants(&mut self, view: MutatorHeapView<'_>, pool: &mut SymbolPool) {
        self.heap_refs = self
            .constants
            .iter()
            .map(|n| match n {
                StgNative::Num(num) => Ref::V(Native::Num(num.clone())),
                StgNative::Str(s) => {
                    let ptr = view
                        .alloc(HeapString::from_str(&view, s.as_str()))
                        .expect("alloc heap str in bytecode constant pool")
                        .as_ptr();
                    Ref::V(Native::Str(ptr))
                }
                StgNative::Sym(s) => {
                    let id = pool.intern(s.as_str());
                    Ref::V(Native::Sym(id))
                }
                StgNative::Zdt(dt) => Ref::V(Native::Zdt(*dt)),
            })
            .collect();
    }
}

// ── Read helpers ────────────────────────────────────────────────────

/// Read a `u8` from `code` at `*pc` and advance `*pc`.
#[inline(always)]
pub fn read_u8(code: &[u8], pc: &mut usize) -> u8 {
    let v = code[*pc];
    *pc += 1;
    v
}

/// Read a little-endian `u16` from `code` at `*pc` and advance `*pc`.
#[inline(always)]
pub fn read_u16(code: &[u8], pc: &mut usize) -> u16 {
    let v = u16::from_le_bytes([code[*pc], code[*pc + 1]]);
    *pc += 2;
    v
}

/// Read a little-endian `u32` from `code` at `*pc` and advance `*pc`.
#[inline(always)]
pub fn read_u32(code: &[u8], pc: &mut usize) -> u32 {
    let v = u32::from_le_bytes([code[*pc], code[*pc + 1], code[*pc + 2], code[*pc + 3]]);
    *pc += 4;
    v
}
