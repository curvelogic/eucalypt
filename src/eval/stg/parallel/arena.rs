//! Anonymous `MAP_SHARED` mmap arena for the process-parallelism boundary
//! (spec §6a).
//!
//! The arena is created **before** the fork, so children inherit the mapping;
//! `MAP_SHARED` makes each worker's writes visible to the parent after join.
//! It is anonymous (`MAP_ANONYMOUS`) — no backing file, no named kernel
//! resource — so disposal is trivial and crash-safe: the parent `munmap`s on
//! `Drop`, and any worker's own mapping is reclaimed by the OS at `_exit`.
//!
//! Anonymous pages are demand-zero, so a generous virtual size costs address
//! space, not physical RAM: only the bytes actually written are faulted in.
//!
//! The arena is split into W equal **per-worker segments**. Worker `w` owns
//! segment `w` and writes its results length-prefixed, in index order. Because
//! each worker writes only its own disjoint segment there are no cross-process
//! atomics and no growth/remap. The parent reads segments in worker-index
//! order, which — since worker `w` holds the contiguous index chunk `w` — is
//! global index order, hence deterministic.
//!
//! Segment layout (byte offsets relative to the segment base):
//! ```text
//!   [0 ..  8)   record count (u64 LE), written by the writer on `finish`
//!   [8 ..  )    records, each `[u64 LE length][length bytes]`
//! ```

use std::io;

/// A worker segment overflowed its slice of the arena. Given the small
/// results the target workloads produce this is rare; it surfaces as a
/// boundary error rather than corrupting a neighbouring segment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArenaOverflow;

/// An anonymous shared-memory arena split into per-worker segments.
pub struct Arena {
    base: *mut u8,
    size: usize,
    segment_size: usize,
    n_workers: usize,
}

// The arena is a raw shared mapping handed to forked children; the `*mut u8`
// is not shared across threads within a process (only across processes via
// COW/`MAP_SHARED`), so the raw pointer's lack of `Send`/`Sync` is not a
// hazard for the single-threaded fork model. We do not auto-derive either.

const HEADER_LEN: usize = 8;
const LEN_PREFIX: usize = 8;

impl Arena {
    /// Create an anonymous `MAP_SHARED` arena of at least `size` bytes, split
    /// into `n_workers` equal segments. `size` is rounded up so each segment
    /// is a whole number of bytes.
    pub fn new(size: usize, n_workers: usize) -> io::Result<Arena> {
        assert!(n_workers >= 1, "arena needs at least one worker segment");
        let segment_size = size.div_ceil(n_workers).max(HEADER_LEN + LEN_PREFIX);
        let size = segment_size * n_workers;
        // SAFETY: standard anonymous mmap; we check MAP_FAILED below.
        let base = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_SHARED | libc::MAP_ANON,
                -1,
                0,
            )
        };
        if base == libc::MAP_FAILED {
            return Err(io::Error::last_os_error());
        }
        Ok(Arena {
            base: base as *mut u8,
            size,
            segment_size,
            n_workers,
        })
    }

    /// Byte offset of worker `w`'s segment base.
    fn segment_base(&self, w: usize) -> usize {
        w * self.segment_size
    }

    /// A writer over worker `w`'s segment (call from within that worker).
    pub fn writer(&self, w: usize) -> SegmentWriter<'_> {
        assert!(w < self.n_workers, "worker index out of range");
        let base = self.segment_base(w);
        SegmentWriter {
            arena: self,
            cursor: base + HEADER_LEN,
            end: base + self.segment_size,
            base,
            count: 0,
        }
    }

    /// A reader over worker `w`'s segment (call from the parent after join).
    pub fn reader(&self, w: usize) -> SegmentReader<'_> {
        assert!(w < self.n_workers, "worker index out of range");
        let base = self.segment_base(w);
        let count = self.read_u64(base);
        SegmentReader {
            arena: self,
            cursor: base + HEADER_LEN,
            remaining: count,
        }
    }

    fn write_u64(&self, offset: usize, v: u64) {
        debug_assert!(offset + 8 <= self.size);
        // SAFETY: offset bounds checked by callers/segment logic.
        unsafe {
            (self.base.add(offset) as *mut u64).write_unaligned(v.to_le());
        }
    }

    fn read_u64(&self, offset: usize) -> u64 {
        debug_assert!(offset + 8 <= self.size);
        // SAFETY: offset within the mapping.
        u64::from_le(unsafe { (self.base.add(offset) as *const u64).read_unaligned() })
    }

    fn write_bytes(&self, offset: usize, bytes: &[u8]) {
        debug_assert!(offset + bytes.len() <= self.size);
        // SAFETY: offset + len within the mapping (checked by the writer).
        unsafe {
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), self.base.add(offset), bytes.len());
        }
    }

    fn read_slice(&self, offset: usize, len: usize) -> &[u8] {
        debug_assert!(offset + len <= self.size);
        // SAFETY: offset + len within the mapping; lifetime tied to &self.
        unsafe { std::slice::from_raw_parts(self.base.add(offset), len) }
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        // SAFETY: we own the mapping created in `new`.
        unsafe {
            libc::munmap(self.base as *mut libc::c_void, self.size);
        }
    }
}

/// Appends length-prefixed records into one worker's segment.
pub struct SegmentWriter<'a> {
    arena: &'a Arena,
    base: usize,
    cursor: usize,
    end: usize,
    count: u64,
}

impl SegmentWriter<'_> {
    /// Append one length-prefixed record. Returns `Err(ArenaOverflow)` if the
    /// record would spill past this worker's segment.
    pub fn push(&mut self, bytes: &[u8]) -> Result<(), ArenaOverflow> {
        let needed = LEN_PREFIX + bytes.len();
        if self.cursor + needed > self.end {
            return Err(ArenaOverflow);
        }
        self.arena.write_u64(self.cursor, bytes.len() as u64);
        self.arena.write_bytes(self.cursor + LEN_PREFIX, bytes);
        self.cursor += needed;
        self.count += 1;
        Ok(())
    }

    /// Finalise the segment header (the record count). Must be called after
    /// the last `push`, before the parent reads.
    pub fn finish(self) {
        self.arena.write_u64(self.base, self.count);
    }
}

/// Reads length-prefixed records back from one worker's segment.
pub struct SegmentReader<'a> {
    arena: &'a Arena,
    cursor: usize,
    remaining: u64,
}

impl<'a> SegmentReader<'a> {
    /// The next record's bytes, or `None` once the segment is exhausted.
    pub fn next(&mut self) -> Option<&'a [u8]> {
        if self.remaining == 0 {
            return None;
        }
        let len = self.arena.read_u64(self.cursor) as usize;
        let slice = self.arena.read_slice(self.cursor + LEN_PREFIX, len);
        self.cursor += LEN_PREFIX + len;
        self.remaining -= 1;
        Some(slice)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_variable_length_records() {
        let arena = Arena::new(4096, 2).unwrap();
        {
            let mut w = arena.writer(0);
            w.push(b"alpha").unwrap();
            w.push(b"").unwrap();
            w.push(b"a longer record here").unwrap();
            w.finish();
        }
        let mut r = arena.reader(0);
        assert_eq!(r.next(), Some(&b"alpha"[..]));
        assert_eq!(r.next(), Some(&b""[..]));
        assert_eq!(r.next(), Some(&b"a longer record here"[..]));
        assert_eq!(r.next(), None);
        // segment 1 is empty (count 0)
        assert_eq!(arena.reader(1).next(), None);
    }

    #[test]
    fn overflow_is_reported_not_corrupting() {
        // A tiny arena: 64 bytes per segment.
        let arena = Arena::new(128, 2).unwrap();
        let mut w = arena.writer(0);
        // 8 header + 8 len + payload must stay within 64.
        assert!(w.push(&[0u8; 40]).is_ok());
        assert_eq!(w.push(&[0u8; 40]), Err(ArenaOverflow));
    }
}
