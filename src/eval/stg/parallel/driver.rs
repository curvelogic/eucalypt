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
    stg::{parallel::serialise, stream::any_live_impure_producer, tags::DataConstructor},
};

/// Default minimum element count before forking is even considered. Below
/// this, `par-map` runs sequentially (fork + arena overhead does not pay).
/// Overridable via `EU_PP_THRESHOLD` (chiefly so tests can force the fork
/// path on small inputs, and so the default can be tuned).
const DEFAULT_THRESHOLD: usize = 1024;

/// Per-element arena byte budget used to size the (virtual, demand-zero)
/// mapping; a worker whose serialised result set overflows its segment simply
/// fails and the driver falls back to sequential.
#[cfg(unix)]
const PER_ELEM_CAP: usize = 4096;

/// Ceiling on the arena's virtual size.
#[cfg(unix)]
const ARENA_MAX: usize = 256 << 20;

fn env_usize(name: &str, default: usize) -> usize {
    std::env::var(name)
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(default)
}

/// Decide the worker count. Returns 1 to mean "run sequentially".
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
    let smid = machine.annotation();

    // Boundary check (spike R2 / spec §6): a live impure streaming producer
    // shares an fd offset across forked workers — refuse rather than fork.
    if any_live_impure_producer() {
        return Err(ExecutionError::NotSerialisable(
            smid,
            Box::new((combinator.to_string(), "value with a live streaming import".to_string())),
        ));
    }

    let f_closure = machine.resolve_callable_closure(view, f)?;
    let elements = collect_spine(machine, view, xs, combinator)?;
    let n = elements.len();

    #[cfg(unix)]
    {
        let w = decide_workers(n);
        if w > 1 && try_parallel(machine, view, &f_closure, &elements, w, combinator)? {
            return Ok(());
        }
    }

    sequential_map(machine, view, &f_closure, &elements, combinator)
}

/// Walk the (WHNF-headed) list `xs`, forcing each tail, into a vector of its
/// element closures (each left unforced — `f` is applied lazily then forced).
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
                let head = machine
                    .data_field(view, &cur, 0)
                    .ok_or_else(|| ExecutionError::Panic(smid, format!("{combinator}: bad list")))?;
                out.push(head);
                cur = machine
                    .data_field(view, &cur, 1)
                    .ok_or_else(|| ExecutionError::Panic(smid, format!("{combinator}: bad list")))?;
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
    machine.return_closure_list(view, results)
}

/// Contiguous even partition of `0..n` into `w` chunks; chunk `i` is
/// `[start, end)`. The first `n % w` chunks are one longer.
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
        let mut reader = arena.reader(wi);
        while let Some(rec) = reader.next() {
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
    machine.return_closure_list(view, results)?;
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

#[cfg(test)]
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
