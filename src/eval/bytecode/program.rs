//! Bytecode program: the flat opcode buffer, constant pool, and global
//! entry table (spec §4.1).
//!
//! The `code` buffer lives outside the GC heap for the whole run — it is
//! never allocated per node, marked, evacuated or forwarded. Heap-backed
//! literals (strings, symbols) cannot live in the byte stream, so they are
//! hoisted once into the constant pool and rooted for the run (spec §4.4).

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

use super::CodeRef;

/// A compiled bytecode program: a flat opcode stream plus the constant
/// pool and per-global entry offsets it refers to.
#[derive(Debug, Default)]
pub struct BytecodeProgram {
    /// Flat opcode + inline-operand stream (non-GC).
    pub code: Vec<u8>,
    /// Constant pool: STG-level native values referenced by `REF_V`
    /// operands (index into this vector).
    pub constants: Vec<StgNative>,
    /// Global slot index -> bytecode entry offset (spec §5).
    pub global_entries: Vec<CodeRef>,
}

impl BytecodeProgram {
    /// Convert the constant pool from STG-level `Native` to heap-level
    /// `Ref` values, allocating/interning heap literals exactly once
    /// (spec §4.4). The returned vector is indexed by `REF_V` payloads
    /// and must be registered as a GC root set for the run.
    ///
    /// Must be called after the machine's heap and symbol pool exist.
    pub fn prepare_constants(&self, view: MutatorHeapView<'_>, pool: &mut SymbolPool) -> Vec<Ref> {
        self.constants
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
            .collect()
    }
}

// ── Little-endian read helpers ──────────────────────────────────────

/// Read a `u8` from `code` at `*pc` and advance `*pc` by 1.
#[inline(always)]
pub fn read_u8(code: &[u8], pc: &mut usize) -> u8 {
    let v = code[*pc];
    *pc += 1;
    v
}

/// Read a little-endian `u16` from `code` at `*pc` and advance `*pc` by 2.
#[inline(always)]
pub fn read_u16(code: &[u8], pc: &mut usize) -> u16 {
    let v = u16::from_le_bytes([code[*pc], code[*pc + 1]]);
    *pc += 2;
    v
}

/// Read a little-endian `u32` from `code` at `*pc` and advance `*pc` by 4.
#[inline(always)]
pub fn read_u32(code: &[u8], pc: &mut usize) -> u32 {
    let v = u32::from_le_bytes([code[*pc], code[*pc + 1], code[*pc + 2], code[*pc + 3]]);
    *pc += 4;
    v
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn readers_are_little_endian() {
        // 0xAA, then u16 0x2211, then u32 0x44332211.
        let code = [0xAA, 0x11, 0x22, 0x11, 0x22, 0x33, 0x44];
        let mut pc = 0usize;
        assert_eq!(read_u8(&code, &mut pc), 0xAA);
        assert_eq!(pc, 1);
        assert_eq!(read_u16(&code, &mut pc), 0x2211);
        assert_eq!(pc, 3);
        assert_eq!(read_u32(&code, &mut pc), 0x4433_2211);
        assert_eq!(pc, 7);
    }
}
