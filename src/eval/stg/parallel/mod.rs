//! Process-level parallelism (PP) — `par-map` plus a safe associative
//! reduction vocabulary (`par-sum`/`par-max`/`par-min`/`par-concat`).
//!
//! Each combinator is a pure performance advisory: it is byte-for-byte
//! identical to its sequential form. The mechanism (spec
//! `docs/superpowers/specs/2026-07-25-pp-parallelism-design.md`, gated by the
//! fork-safety spike `docs/superpowers/reports/2026-07-25-pp-fork-safety-spike.md`)
//! is Unix copy-on-write `fork()`: the parent forces the spine of `xs`, forks
//! W workers who inherit the entire heap COW (code and inputs for free), each
//! evaluates its contiguous index chunk in its own COW heap and writes
//! fully-forced results (or a worker-local partial) into an anonymous
//! `MAP_SHARED` mmap arena at index-addressed slots; the parent `waitpid`s and
//! reassembles deterministically in index order.
//!
//! Below a size threshold, with W <= 1, or on a non-Unix platform, the driver
//! falls back transparently to a sequential map — the identical result with no
//! fork.

#[cfg(unix)]
pub mod arena;
mod driver;
#[cfg(unix)]
pub mod fork;
pub mod serialise;

pub use driver::par_map;
