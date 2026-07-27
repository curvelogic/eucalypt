//! Value serialiser for the process-parallelism boundary (spec §6).
//!
//! Only the **serialisable-data subset** crosses the arena: numbers, strings,
//! symbols, booleans, null, lists of serialisable, and blocks of serialisable.
//! A function, IO action, or any opaque value (set, vector, PRNG, producer
//! handle, …) is a programmer error at the boundary — surfaced as
//! [`ExecutionError::NotSerialisable`] naming the value's kind and the
//! combinator, rather than silently mishandled.
//!
//! [`force_and_serialise`] forces a value, deep-forces it (driving the machine
//! to force nested thunks) and byte-encodes it; [`deserialise_value`] rebuilds
//! an equivalent WHNF value on the parent's heap through the neutral intrinsic
//! ABI, so both engines produce byte-identical results.
//!
//! Metadata travels with its value. Forcing a value *strips* any metadata
//! nothing consumes (`return_meta` in both engines), so a copying boundary
//! that ignored it would silently drop a `:suppress` or `:doc` annotation and
//! change what the value renders as — `map` preserves it, and `par-map`
//! promises to be identical to `map`. [`force_and_serialise`] recovers it
//! through [`IntrinsicMachine::take_stripped_meta`] and encodes it ahead of
//! the body; metadata that is not itself serialisable raises the boundary
//! error rather than being dropped.
//!
//! Every handle held across a `force()` lives in the machine's root set (see
//! [`super::roots`]) and is read back from there afterwards — the collector
//! updates the root set, not the copy on the Rust stack.
//!
//! Wire format (little-endian, self-describing):
//! ```text
//!   0x00 null            0x01 true            0x02 false
//!   0x03 number   1-byte kind (0=u64,1=i64,2=f64) + 8-byte LE payload
//!   0x04 symbol   u32 length + UTF-8 bytes
//!   0x05 string   u32 length + UTF-8 bytes
//!   0x06 zdt      u32 length + RFC 3339 UTF-8 bytes
//!   0x07 list     u32 count + `count` encoded values
//!   0x08 block    u32 count + `count` × (encoded key scalar, encoded value)
//!   0x09 meta     encoded metadata value + encoded body value
//! ```

use chrono::DateTime;
use serde_json::Number;

use crate::common::sourcemap::Smid;
use crate::eval::{
    error::ExecutionError,
    machine::intrinsic::{AbiClosure, IntrinsicMachine},
    memory::{
        mutator::MutatorHeapView,
        syntax::{Native, StgBuilder},
    },
    stg::parallel::roots::with_roots,
    stg::tags::DataConstructor,
};

const TAG_NULL: u8 = 0x00;
const TAG_TRUE: u8 = 0x01;
const TAG_FALSE: u8 = 0x02;
const TAG_NUM: u8 = 0x03;
const TAG_SYM: u8 = 0x04;
const TAG_STR: u8 = 0x05;
const TAG_ZDT: u8 = 0x06;
const TAG_LIST: u8 = 0x07;
const TAG_BLOCK: u8 = 0x08;
const TAG_META: u8 = 0x09;

const NUM_U64: u8 = 0;
const NUM_I64: u8 = 1;
const NUM_F64: u8 = 2;

fn not_serialisable(smid: Smid, combinator: &str, kind: &str) -> ExecutionError {
    ExecutionError::NotSerialisable(smid, Box::new((combinator.to_string(), kind.to_string())))
}

/// A corrupt/short byte stream from the arena — an internal invariant
/// violation (we produced the bytes ourselves), surfaced as a panic-class
/// execution error rather than a silent misread.
fn corrupt(smid: Smid, what: &str) -> ExecutionError {
    ExecutionError::Panic(smid, format!("par: corrupt arena record ({what})"))
}

// ── pure wire-format codec (unit-testable without a machine) ────────────

fn write_u32(out: &mut Vec<u8>, v: u32) {
    out.extend_from_slice(&v.to_le_bytes());
}

fn write_len_bytes(out: &mut Vec<u8>, bytes: &[u8]) {
    write_u32(out, bytes.len() as u32);
    out.extend_from_slice(bytes);
}

fn encode_number(out: &mut Vec<u8>, n: &Number) {
    out.push(TAG_NUM);
    if let Some(u) = n.as_u64() {
        out.push(NUM_U64);
        out.extend_from_slice(&u.to_le_bytes());
    } else if let Some(i) = n.as_i64() {
        out.push(NUM_I64);
        out.extend_from_slice(&i.to_le_bytes());
    } else {
        // serde_json guarantees one of u64/i64/f64 for a finite number.
        let f = n.as_f64().unwrap_or(f64::NAN);
        out.push(NUM_F64);
        out.extend_from_slice(&f.to_le_bytes());
    }
}

fn take<'a>(cur: &mut &'a [u8], n: usize, smid: Smid) -> Result<&'a [u8], ExecutionError> {
    if cur.len() < n {
        return Err(corrupt(smid, "short read"));
    }
    let (head, tail) = cur.split_at(n);
    *cur = tail;
    Ok(head)
}

fn read_u8(cur: &mut &[u8], smid: Smid) -> Result<u8, ExecutionError> {
    Ok(take(cur, 1, smid)?[0])
}

fn read_u32(cur: &mut &[u8], smid: Smid) -> Result<u32, ExecutionError> {
    let b = take(cur, 4, smid)?;
    Ok(u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
}

fn read_u64(cur: &mut &[u8], smid: Smid) -> Result<u64, ExecutionError> {
    let b = take(cur, 8, smid)?;
    Ok(u64::from_le_bytes(b.try_into().unwrap()))
}

fn read_len_bytes<'a>(cur: &mut &'a [u8], smid: Smid) -> Result<&'a [u8], ExecutionError> {
    let len = read_u32(cur, smid)? as usize;
    take(cur, len, smid)
}

fn read_str(cur: &mut &[u8], smid: Smid) -> Result<String, ExecutionError> {
    let bytes = read_len_bytes(cur, smid)?;
    std::str::from_utf8(bytes)
        .map(|s| s.to_string())
        .map_err(|_| corrupt(smid, "invalid utf-8"))
}

fn decode_number(cur: &mut &[u8], smid: Smid) -> Result<Number, ExecutionError> {
    match read_u8(cur, smid)? {
        NUM_U64 => Ok(Number::from(read_u64(cur, smid)?)),
        NUM_I64 => Ok(Number::from(read_u64(cur, smid)? as i64)),
        NUM_F64 => {
            let f = f64::from_bits(read_u64(cur, smid)?);
            Number::from_f64(f).ok_or_else(|| corrupt(smid, "non-finite float"))
        }
        other => Err(corrupt(smid, &format!("bad number kind {other}"))),
    }
}

// ── deep-force serialise ────────────────────────────────────────────────

/// Force `value`, then deep-force and byte-encode it into `out`.
///
/// This is the only entry point: forcing must happen *here* rather than in the
/// caller, because forcing is what strips metadata, and the stripped metadata
/// is only recoverable immediately afterwards (see the module header).
///
/// Errors with [`ExecutionError::NotSerialisable`] on a non-data value.
pub fn force_and_serialise(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    value: AbiClosure,
    combinator: &str,
    out: &mut Vec<u8>,
) -> Result<(), ExecutionError> {
    with_roots(machine, |machine| {
        let slot = machine.gc_root_push(value);
        force_slot_and_serialise(machine, view, slot, combinator, out)
    })
}

/// Force the handle in root slot `slot`, writing the forced value back into
/// the slot; encode any metadata the force stripped, then the value itself.
fn force_slot_and_serialise(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    slot: usize,
    combinator: &str,
    out: &mut Vec<u8>,
) -> Result<(), ExecutionError> {
    let handle = machine.gc_root_get(slot);
    let forced = machine.force(handle)?;
    machine.gc_root_set(slot, forced);
    if let Some(meta) = machine.take_stripped_meta() {
        out.push(TAG_META);
        // The metadata is itself an unforced value, and may in principle carry
        // metadata of its own, so it goes through the same path.
        with_roots(machine, |machine| {
            let meta_slot = machine.gc_root_push(meta);
            force_slot_and_serialise(machine, view, meta_slot, combinator, out)
        })?;
    }
    serialise_forced(machine, view, slot, combinator, out)
}

/// Byte-encode the already-forced value in root slot `slot`.
fn serialise_forced(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    slot: usize,
    combinator: &str,
    out: &mut Vec<u8>,
) -> Result<(), ExecutionError> {
    let smid = machine.annotation();
    let whnf = machine.gc_root_get(slot);
    match machine.data_tag(view, &whnf) {
        None => {
            // A bare native atom.
            let native = machine
                .value_native(view, &whnf)
                .ok_or_else(|| not_serialisable(smid, combinator, "function"))?;
            encode_scalar(machine, view, &native, combinator, out)
        }
        Some(tag) => match DataConstructor::try_from(tag) {
            Ok(DataConstructor::Unit) => {
                out.push(TAG_NULL);
                Ok(())
            }
            Ok(DataConstructor::BoolTrue) => {
                out.push(TAG_TRUE);
                Ok(())
            }
            Ok(DataConstructor::BoolFalse) => {
                out.push(TAG_FALSE);
                Ok(())
            }
            Ok(
                DataConstructor::BoxedNumber
                | DataConstructor::BoxedString
                | DataConstructor::BoxedSymbol
                | DataConstructor::BoxedZdt,
            ) => {
                // A box's payload (field 0) may still be an unevaluated thunk
                // even once the box constructor is at WHNF — force it before
                // reading the scalar native. Nothing else is held live across
                // that force.
                let field = machine
                    .data_field(view, &whnf, 0)
                    .ok_or_else(|| corrupt(smid, "boxed scalar payload"))?;
                let field = machine.force(field)?;
                let native = machine
                    .value_native(view, &field)
                    .ok_or_else(|| not_serialisable(smid, combinator, "opaque boxed value"))?;
                encode_scalar(machine, view, &native, combinator, out)
            }
            Ok(DataConstructor::ListNil) => {
                out.push(TAG_LIST);
                write_u32(out, 0);
                Ok(())
            }
            Ok(DataConstructor::ListCons) => serialise_list(machine, view, slot, combinator, out),
            Ok(DataConstructor::Block) => serialise_block(machine, view, slot, combinator, out),
            Ok(DataConstructor::BoxedTypeData) => {
                Err(not_serialisable(smid, combinator, "type-data value"))
            }
            Ok(
                DataConstructor::IoReturn
                | DataConstructor::IoBind
                | DataConstructor::IoAction
                | DataConstructor::IoFail,
            ) => Err(not_serialisable(smid, combinator, "IO action")),
            Ok(DataConstructor::Clause) => {
                Err(not_serialisable(smid, combinator, "conditional clause"))
            }
            Ok(DataConstructor::BlockPair | DataConstructor::BlockKvList) => Err(not_serialisable(
                smid,
                combinator,
                "bare block-internal value",
            )),
            Err(_) => Err(not_serialisable(smid, combinator, "unknown value")),
        },
    }
}

fn encode_scalar(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    native: &Native,
    combinator: &str,
    out: &mut Vec<u8>,
) -> Result<(), ExecutionError> {
    let smid = machine.annotation();
    match native {
        Native::Num(n) => {
            encode_number(out, n);
            Ok(())
        }
        Native::Str(ptr) => {
            out.push(TAG_STR);
            let scoped = view.scoped(*ptr);
            write_len_bytes(out, (*scoped).as_str().as_bytes());
            Ok(())
        }
        Native::Sym(id) => {
            out.push(TAG_SYM);
            let name = machine.symbol_pool().resolve(*id).to_string();
            write_len_bytes(out, name.as_bytes());
            Ok(())
        }
        Native::Zdt(dt) => {
            out.push(TAG_ZDT);
            write_len_bytes(out, dt.to_rfc3339().as_bytes());
            Ok(())
        }
        Native::Index(_)
        | Native::Set(_)
        | Native::NdArray(_)
        | Native::Vec(_)
        | Native::Prng(_)
        | Native::Producer(_) => Err(not_serialisable(smid, combinator, "opaque value")),
    }
}

/// Walk a `ListCons` spine, deep-forcing and serialising each element.
///
/// The spine cursor is held in the root set, not on the Rust stack: every
/// element serialisation forces, and a force can collect.
fn serialise_list(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    slot: usize,
    combinator: &str,
    out: &mut Vec<u8>,
) -> Result<(), ExecutionError> {
    let smid = machine.annotation();
    out.push(TAG_LIST);
    let count_pos = out.len();
    write_u32(out, 0); // placeholder, backpatched below
    let count = with_roots(machine, |machine| {
        let head_of_list = machine.gc_root_get(slot);
        let cur_slot = machine.gc_root_push(head_of_list);
        let mut count: u32 = 0;
        loop {
            let cur = machine.gc_root_get(cur_slot);
            match machine
                .data_tag(view, &cur)
                .and_then(|t| DataConstructor::try_from(t).ok())
            {
                Some(DataConstructor::ListNil) => break,
                Some(DataConstructor::ListCons) => {
                    let head = machine
                        .data_field(view, &cur, 0)
                        .ok_or_else(|| corrupt(smid, "list head"))?;
                    let tail = machine
                        .data_field(view, &cur, 1)
                        .ok_or_else(|| corrupt(smid, "list tail"))?;
                    // Root the tail before serialising the head — serialising
                    // forces, and an unrooted `tail` would not survive it.
                    machine.gc_root_set(cur_slot, tail);
                    force_and_serialise(machine, view, head, combinator, out)?;
                    count += 1;
                    let tail = machine.gc_root_get(cur_slot);
                    let tail = machine.force(tail)?;
                    machine.gc_root_set(cur_slot, tail);
                }
                _ => return Err(not_serialisable(smid, combinator, "improper list")),
            }
        }
        Ok(count)
    })?;
    out[count_pos..count_pos + 4].copy_from_slice(&count.to_le_bytes());
    Ok(())
}

/// Walk a `Block`'s key-value list, serialising each `(key, value)` pair.
fn serialise_block(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    slot: usize,
    combinator: &str,
    out: &mut Vec<u8>,
) -> Result<(), ExecutionError> {
    let smid = machine.annotation();
    out.push(TAG_BLOCK);
    let count_pos = out.len();
    write_u32(out, 0);
    let count = with_roots(machine, |machine| {
        // Field 0 is the key-value cons list.
        let block = machine.gc_root_get(slot);
        let kvlist = machine
            .data_field(view, &block, 0)
            .ok_or_else(|| corrupt(smid, "block kv list"))?;
        let cur_slot = machine.gc_root_push(kvlist);
        let cur = machine.gc_root_get(cur_slot);
        let cur = machine.force(cur)?;
        machine.gc_root_set(cur_slot, cur);
        let mut count: u32 = 0;
        loop {
            let cur = machine.gc_root_get(cur_slot);
            match machine
                .data_tag(view, &cur)
                .and_then(|t| DataConstructor::try_from(t).ok())
            {
                Some(DataConstructor::ListNil) => break,
                Some(DataConstructor::ListCons) => {
                    let pair = machine
                        .data_field(view, &cur, 0)
                        .ok_or_else(|| corrupt(smid, "block pair"))?;
                    let tail = machine
                        .data_field(view, &cur, 1)
                        .ok_or_else(|| corrupt(smid, "block kv tail"))?;
                    machine.gc_root_set(cur_slot, tail);
                    let pair = machine.force(pair)?;
                    let pair_slot = machine.gc_root_push(pair);
                    // Key: an unboxed symbol (or string) native in field 0.
                    // Read and encoded with no intervening force — a
                    // `Native::Str` carries a raw heap pointer.
                    let pair = machine.gc_root_get(pair_slot);
                    let key = machine.field_native(view, &pair, 0).ok_or_else(|| {
                        not_serialisable(smid, combinator, "non-scalar block key")
                    })?;
                    encode_scalar(machine, view, &key, combinator, out)?;
                    // Value: field 1, forced and recursively serialised — this
                    // is where a `:suppress`/`:doc` annotation is recovered.
                    let pair = machine.gc_root_get(pair_slot);
                    let value = machine
                        .data_field(view, &pair, 1)
                        .ok_or_else(|| corrupt(smid, "block value"))?;
                    force_and_serialise(machine, view, value, combinator, out)?;
                    machine.gc_root_truncate(pair_slot);
                    count += 1;
                    let tail = machine.gc_root_get(cur_slot);
                    let tail = machine.force(tail)?;
                    machine.gc_root_set(cur_slot, tail);
                }
                _ => return Err(not_serialisable(smid, combinator, "malformed block")),
            }
        }
        Ok(count)
    })?;
    out[count_pos..count_pos + 4].copy_from_slice(&count.to_le_bytes());
    Ok(())
}

// ── deserialise / rebuild ───────────────────────────────────────────────

/// Rebuild a WHNF value from `cur` (advancing it past the value's bytes)
/// through the neutral ABI, so the value lives on the reading engine's heap.
pub fn deserialise_value(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    cur: &mut &[u8],
) -> Result<AbiClosure, ExecutionError> {
    let smid = machine.annotation();
    match read_u8(cur, smid)? {
        TAG_NULL => machine.data_value(view, DataConstructor::Unit.tag(), &[]),
        TAG_TRUE => machine.data_value(view, DataConstructor::BoolTrue.tag(), &[]),
        TAG_FALSE => machine.data_value(view, DataConstructor::BoolFalse.tag(), &[]),
        TAG_NUM => {
            let n = decode_number(cur, smid)?;
            let field = machine.native_value(view, Native::Num(n))?;
            machine.data_value(view, DataConstructor::BoxedNumber.tag(), &[field])
        }
        TAG_STR => {
            let s = read_str(cur, smid)?;
            let ptr = view.str(s.as_str())?.as_ptr();
            let field = machine.native_value(view, Native::Str(ptr))?;
            machine.data_value(view, DataConstructor::BoxedString.tag(), &[field])
        }
        TAG_SYM => {
            let s = read_str(cur, smid)?;
            let id = machine.symbol_pool_mut().intern(s.as_str());
            let field = machine.native_value(view, Native::Sym(id))?;
            machine.data_value(view, DataConstructor::BoxedSymbol.tag(), &[field])
        }
        TAG_ZDT => {
            let s = read_str(cur, smid)?;
            let dt = DateTime::parse_from_rfc3339(&s).map_err(|_| corrupt(smid, "bad rfc3339"))?;
            let field = machine.native_value(view, Native::Zdt(dt))?;
            machine.data_value(view, DataConstructor::BoxedZdt.tag(), &[field])
        }
        TAG_LIST => {
            let count = read_u32(cur, smid)?;
            let mut items = Vec::with_capacity(count as usize);
            for _ in 0..count {
                items.push(deserialise_value(machine, view, cur)?);
            }
            build_list(machine, view, items)
        }
        TAG_BLOCK => {
            let count = read_u32(cur, smid)?;
            let mut pairs = Vec::with_capacity(count as usize);
            for _ in 0..count {
                // Key scalar → rebuild as an unboxed native, then value.
                let key = deserialise_key_native(machine, view, cur)?;
                let value = deserialise_value(machine, view, cur)?;
                let pair =
                    machine.data_value(view, DataConstructor::BlockPair.tag(), &[key, value])?;
                pairs.push(pair);
            }
            let kvlist = build_list(machine, view, pairs)?;
            // Block(kvlist, no_index) — the no-index sentinel is boxed zero.
            let no_index = machine.native_value(view, Native::Num(0.into()))?;
            machine.data_value(view, DataConstructor::Block.tag(), &[kvlist, no_index])
        }
        TAG_META => {
            // Metadata first, then the body it annotates — rebuilt as a `Meta`
            // value so the reader sees the annotation `map` would have
            // preserved. Neither call forces, so nothing needs rooting.
            let meta = deserialise_value(machine, view, cur)?;
            let body = deserialise_value(machine, view, cur)?;
            machine.meta_value(view, meta, body)
        }
        other => Err(corrupt(smid, &format!("unknown value tag {other}"))),
    }
}

/// Rebuild a block key (encoded as a scalar) as an **unboxed** native value
/// handle (block pairs hold an unboxed symbol/string, not a boxed cell).
fn deserialise_key_native(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    cur: &mut &[u8],
) -> Result<AbiClosure, ExecutionError> {
    let smid = machine.annotation();
    match read_u8(cur, smid)? {
        TAG_SYM => {
            let s = read_str(cur, smid)?;
            let id = machine.symbol_pool_mut().intern(s.as_str());
            machine.native_value(view, Native::Sym(id))
        }
        TAG_STR => {
            let s = read_str(cur, smid)?;
            let ptr = view.str(s.as_str())?.as_ptr();
            machine.native_value(view, Native::Str(ptr))
        }
        other => Err(corrupt(smid, &format!("bad block key tag {other}"))),
    }
}

/// Build a cons list value handle from `items` (ListNil tail, ListCons cells).
pub fn build_list(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    items: Vec<AbiClosure>,
) -> Result<AbiClosure, ExecutionError> {
    let mut acc = machine.data_value(view, DataConstructor::ListNil.tag(), &[])?;
    for item in items.into_iter().rev() {
        acc = machine.data_value(view, DataConstructor::ListCons.tag(), &[item, acc])?;
    }
    Ok(acc)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number_wire_round_trips_all_kinds() {
        for n in [
            Number::from(0u64),
            Number::from(u64::MAX),
            Number::from(-5i64),
            Number::from(i64::MIN),
            Number::from_f64(3.5).unwrap(),
            Number::from_f64(-2.25e10).unwrap(),
        ] {
            let mut out = Vec::new();
            encode_number(&mut out, &n);
            let mut cur = &out[..];
            assert_eq!(read_u8(&mut cur, Smid::default()).unwrap(), TAG_NUM);
            let decoded = decode_number(&mut cur, Smid::default()).unwrap();
            assert_eq!(decoded, n, "number {n} did not round-trip");
            assert!(cur.is_empty(), "trailing bytes after {n}");
        }
    }

    #[test]
    fn length_prefixed_bytes_round_trip() {
        let mut out = Vec::new();
        write_len_bytes(&mut out, b"hello world");
        write_len_bytes(&mut out, b"");
        let mut cur = &out[..];
        assert_eq!(read_str(&mut cur, Smid::default()).unwrap(), "hello world");
        assert_eq!(read_str(&mut cur, Smid::default()).unwrap(), "");
        assert!(cur.is_empty());
    }

    #[test]
    fn short_read_is_reported() {
        let bytes = [TAG_NUM, NUM_U64, 1, 2, 3]; // truncated 8-byte payload
        let mut cur = &bytes[1..];
        assert!(decode_number(&mut cur, Smid::default()).is_err());
    }
}
