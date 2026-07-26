//! The engine-neutral parallel-map driver shared by the `par-*` intrinsics.
//!
//! `par-map` is the sole parallel primitive; the reduction combinators
//! (`par-sum`/`par-max`/`par-min`/`par-concat`) are prelude wrappers that pipe
//! `par-map` into the exact sequential prelude reducer, so they are
//! byte-for-byte identical to their sequential forms by construction.
//!
//! The driver runs entirely against the neutral [`IntrinsicMachine`] ABI, so it
//! behaves identically on the HeapSyn and bytecode engines. On unix, above a
//! size threshold and with more than one worker, it forks workers who each
//! deep-force and serialise their contiguous index chunk into a shared arena;
//! the parent deserialises in index order. Below threshold, with W ≤ 1, on a
//! non-unix platform, or if the fork path fails for any reason, it evaluates
//! sequentially — and, crucially, the sequential path shares the same
//! serialise/deserialise codec, so both paths produce the identical value and
//! the identical boundary error.

use crate::eval::{
    error::ExecutionError,
    machine::intrinsic::{AbiClosure, IntrinsicMachine},
    memory::{mutator::MutatorHeapView, syntax::Ref},
    stg::{parallel::serialise, tags::DataConstructor},
};

/// Default minimum element count before forking is even considered. Below
/// this, `par-map` runs sequentially (fork + arena overhead does not pay).
/// Overridable via `EU_PP_THRESHOLD` (chiefly so tests can force the fork
/// path on small inputs, and so the default can be tuned).
#[cfg(unix)]
const DEFAULT_THRESHOLD: usize = 1024;

/// Per-element arena byte budget used to size the (virtual, demand-zero)
/// mapping; a worker whose serialised result set overflows its segment simply
/// fails and the driver falls back to sequential.
#[cfg(unix)]
const PER_ELEM_CAP: usize = 4096;

/// Ceiling on the arena's virtual size.
#[cfg(unix)]
const ARENA_MAX: usize = 256 << 20;

#[cfg(unix)]
fn env_usize(name: &str, default: usize) -> usize {
    std::env::var(name)
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(default)
}

/// Whether `EU_PP_TRACE=1` asked us to report which path each `par-*` took.
///
/// The parallel path is invisible by construction — same value, same errors —
/// so without this there is no way for a user (or a test) to tell a fork from
/// the sequential fallback, and a silently-never-forking build looks exactly
/// like a working one.
#[cfg(unix)]
fn trace_enabled() -> bool {
    std::env::var("EU_PP_TRACE").as_deref() == Ok("1")
}

#[cfg(unix)]
fn trace(combinator: &str, n: usize, what: std::fmt::Arguments<'_>) {
    if trace_enabled() {
        eprintln!("{combinator}: {n} elements — {what}");
    }
}

/// Decide the worker count. Returns 1 to mean "run sequentially".
#[cfg(unix)]
fn decide_workers(n: usize) -> usize {
    let threshold = env_usize("EU_PP_THRESHOLD", DEFAULT_THRESHOLD);
    if n < threshold || n < 2 {
        return 1;
    }
    let default_w = std::thread::available_parallelism()
        .map(|p| p.get().saturating_sub(1))
        .unwrap_or(1);
    let w = env_usize("EU_PP_WORKERS", default_w);
    w.min(n).max(1)
}

/// `par-map(f, xs)` — the parallel map primitive. `xs` is already forced to
/// WHNF by the wrapper's strictness; `f` is a callable closure reference.
///
/// Sets the machine result to the mapped list in index order — identical to
/// `xs map(f)` for the finite, fully-consumed lists these workloads use.
pub fn par_map(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    f: &Ref,
    xs: &Ref,
    combinator: &str,
) -> Result<(), ExecutionError> {
    let f_closure = machine.resolve_callable_closure(view, f)?;
    let elements = collect_spine(machine, view, xs, combinator)?;

    #[cfg(unix)]
    {
        // Forking is only safe where a host has vouched for the process (spec
        // §2/§4): a child inherits every lock in whatever state the threads
        // that no longer exist left it in. The `eu` CLI opts in for
        // evaluation; the LSP server, the WASM API and the libtest harness do
        // not, and simply do not fork — the sequential path gives the
        // identical answer.
        let n = elements.len();
        let w = decide_workers(n);
        if w < 2 {
            trace(combinator, n, format_args!("sequential (below threshold)"));
        } else if !super::fork::process_is_fork_safe() {
            trace(
                combinator,
                n,
                format_args!("sequential (process is not a declared fork-safe host)"),
            );
        } else if try_parallel(machine, view, &f_closure, &elements, w, combinator)? {
            trace(combinator, n, format_args!("forked {w} workers"));
            return Ok(());
        } else {
            trace(
                combinator,
                n,
                format_args!("sequential (fork path declined)"),
            );
        }
    }

    sequential_map(machine, view, &f_closure, &elements, combinator)
}

/// Walk the (WHNF-headed) list `xs` into a vector of its element closures
/// (each left unforced — `f` is applied lazily then forced).
///
/// Tails are forced defensively, but the prelude wrapper has already walked the
/// spine via `force-spine`, and that matters: the machine memoises a thunk by
/// writing the result back into the environment slot naming it, and an
/// intrinsic holding a resolved closure has no slot to write to. Forcing a
/// spine *only* here would therefore leave every cell unmemoised — harmless for
/// a pure list, but for a streaming import it would mean the next reader
/// re-advances an already-consumed producer and sees a truncated list.
fn collect_spine(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    xs: &Ref,
    combinator: &str,
) -> Result<Vec<AbiClosure>, ExecutionError> {
    let smid = machine.annotation();
    let mut out = Vec::new();
    let mut cur = machine.resolve_closure(view, xs)?;
    loop {
        cur = machine.force(cur)?;
        match machine
            .data_tag(view, &cur)
            .and_then(|t| DataConstructor::try_from(t).ok())
        {
            Some(DataConstructor::ListNil) => break,
            Some(DataConstructor::ListCons) => {
                let head = machine.data_field(view, &cur, 0).ok_or_else(|| {
                    ExecutionError::Panic(smid, format!("{combinator}: bad list"))
                })?;
                out.push(head);
                cur = machine.data_field(view, &cur, 1).ok_or_else(|| {
                    ExecutionError::Panic(smid, format!("{combinator}: bad list"))
                })?;
            }
            _ => {
                return Err(ExecutionError::Panic(
                    smid,
                    format!("{combinator}: expected a list argument"),
                ))
            }
        }
    }
    Ok(out)
}

/// Map one element: apply `f`, deep-force, then round-trip through the
/// serialisation codec so the sequential path normalises values and raises the
/// boundary error identically to the parallel path.
fn map_element(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    f: &AbiClosure,
    elem: &AbiClosure,
    combinator: &str,
) -> Result<AbiClosure, ExecutionError> {
    let thunk = machine.apply1_thunk(view, f.clone(), elem.clone())?;
    let forced = machine.force(thunk)?;
    let mut buf = Vec::new();
    serialise::serialise_value(machine, view, &forced, combinator, &mut buf)?;
    let mut cur = &buf[..];
    serialise::deserialise_value(machine, view, &mut cur)
}

/// Sequential fallback (also the parallel path's oracle): map every element
/// through [`map_element`] and set the result list.
fn sequential_map(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    f: &AbiClosure,
    elements: &[AbiClosure],
    combinator: &str,
) -> Result<(), ExecutionError> {
    let mut results = Vec::with_capacity(elements.len());
    for elem in elements {
        results.push(map_element(machine, view, f, elem, combinator)?);
    }
    let list = serialise::build_list(machine, view, results)?;
    machine.set_result(list)
}

/// Contiguous even partition of `0..n` into `w` chunks; chunk `i` is
/// `[start, end)`. The first `n % w` chunks are one longer.
#[cfg(unix)]
fn chunk_bounds(n: usize, w: usize, i: usize) -> (usize, usize) {
    let base = n / w;
    let rem = n % w;
    let start = i * base + i.min(rem);
    let len = base + if i < rem { 1 } else { 0 };
    (start, start + len)
}

/// Attempt the COW-fork parallel path. Returns `Ok(true)` if it completed and
/// set the machine result, `Ok(false)` if the caller should fall back to the
/// sequential path (fork/serialisation issue). A genuine user error surfaces on
/// the sequential re-run, so this never returns `Err` for a worker fault.
#[cfg(unix)]
fn try_parallel(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    f: &AbiClosure,
    elements: &[AbiClosure],
    w: usize,
    combinator: &str,
) -> Result<bool, ExecutionError> {
    use super::arena::Arena;
    use super::fork::run_workers;

    let n = elements.len();
    let size = (n.saturating_mul(PER_ELEM_CAP)).clamp(w * (16 + 64), ARENA_MAX);
    let arena = match Arena::new(size, w) {
        Ok(a) => a,
        Err(_) => return Ok(false), // couldn't map — fall back
    };

    // Workers run in their own COW address space; mutating the machine there is
    // private to each child (spec §4). We serialise into disjoint segments.
    let worker = |wi: usize| -> Result<(), ExecutionError> {
        // Mark this child a PP worker so that any attempt to advance an impure
        // streaming producer fails here rather than stealing the parent's
        // shared fd offset (spike R2). Failing the worker costs only a
        // sequential re-run in the parent, which consumes the producer
        // correctly — so `par-*` stays a transparent advisory.
        crate::eval::stg::stream::enter_parallel_worker();
        let (start, end) = chunk_bounds(n, w, wi);
        let mut writer = arena.writer(wi);
        let mut buf = Vec::new();
        for elem in &elements[start..end] {
            let thunk = machine.apply1_thunk(view, f.clone(), elem.clone())?;
            let forced = machine.force(thunk)?;
            buf.clear();
            serialise::serialise_value(machine, view, &forced, combinator, &mut buf)?;
            writer
                .push(&buf)
                .map_err(|_| arena_overflow_error(machine, combinator))?;
        }
        writer.finish();
        Ok(())
    };

    // The worker closure holds `&mut machine`; `run_workers` consumes it and
    // invokes it only inside forked children (one call per child address
    // space), so the aliasing the borrow checker cannot see never occurs at
    // runtime. Moving it into `run_workers` releases the borrow on return, so
    // the parent can reuse `machine` for reassembly below.
    let join = run_workers(w, worker);

    if join.is_err() {
        return Ok(false); // any worker fault → sequential re-run (surfaces the real error)
    }

    // Parent reassembles in worker-index (= global index) order.
    let mut results = Vec::with_capacity(n);
    for wi in 0..w {
        for rec in arena.reader(wi) {
            let mut cur = rec;
            match serialise::deserialise_value(machine, view, &mut cur) {
                Ok(v) => results.push(v),
                Err(_) => return Ok(false), // corrupt read → sequential re-run
            }
        }
    }
    if results.len() != n {
        return Ok(false); // a worker wrote the wrong count → sequential re-run
    }
    let list = serialise::build_list(machine, view, results)?;
    machine.set_result(list)?;
    Ok(true)
}

#[cfg(unix)]
fn arena_overflow_error(machine: &dyn IntrinsicMachine, combinator: &str) -> ExecutionError {
    // Surfaced only inside a worker; it becomes a non-zero exit and the parent
    // falls back to sequential, so this message is diagnostic only.
    ExecutionError::Panic(
        machine.annotation(),
        format!("{combinator}: worker result set overflowed its arena segment"),
    )
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    #[test]
    fn chunk_bounds_partition_exactly() {
        for &(n, w) in &[(10usize, 3usize), (12, 4), (7, 4), (1, 1), (5, 5), (0, 3)] {
            let mut covered = Vec::new();
            let mut last_end = 0;
            for i in 0..w {
                let (s, e) = chunk_bounds(n, w, i);
                assert_eq!(s, last_end, "contiguous for n={n} w={w} i={i}");
                assert!(e >= s);
                for k in s..e {
                    covered.push(k);
                }
                last_end = e;
            }
            assert_eq!(last_end, n, "covers all of 0..{n} for w={w}");
            covered.sort_unstable();
            assert_eq!(covered, (0..n).collect::<Vec<_>>());
        }
    }
}
