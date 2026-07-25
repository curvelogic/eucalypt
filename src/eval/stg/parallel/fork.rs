//! Unix `fork`/`waitpid` worker coordination for process parallelism, with the
//! worker-hygiene rules the fork-safety spike mandated (§4 addendum, R3/R4):
//!
//! - A child leaves **only** via `libc::_exit` — never unwinding past the fork
//!   frame (so no parent `Drop`s/atexit run twice, no inherited stdio buffers
//!   are flushed by a child) and never touching stdout.
//! - A child resets `SIGINT` to `SIG_DFL` immediately after fork, so a
//!   foreground Ctrl-C terminates workers by default and the parent's handler
//!   drives cleanup.
//! - On the first worker failure the parent `SIGKILL`s and reaps the
//!   survivors, then reports the failure. The caller (the PP driver) treats any
//!   failure as a signal to fall back to a **sequential** re-evaluation in the
//!   parent — which reproduces the exact result or raises the exact
//!   user-facing error with a proper source location.

use std::panic::{catch_unwind, AssertUnwindSafe};

use crate::eval::error::ExecutionError;

/// A worker did not complete cleanly. The driver treats this opaquely and
/// falls back to sequential evaluation, so the variants exist only for
/// diagnostics/logging.
#[derive(Debug)]
pub enum ForkError {
    /// `fork()` itself failed.
    ForkFailed(std::io::Error),
    /// A worker exited non-zero or was killed by a signal.
    Worker { worker: usize },
    /// `waitpid` failed for a worker.
    WaitFailed { worker: usize },
}

impl std::fmt::Display for ForkError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ForkError::ForkFailed(e) => write!(f, "fork failed: {e}"),
            ForkError::Worker { worker } => write!(f, "parallel worker {worker} failed"),
            ForkError::WaitFailed { worker } => {
                write!(f, "waitpid failed for parallel worker {worker}")
            }
        }
    }
}

/// Reset SIGINT disposition to default in the just-forked child.
fn reset_sigint_to_default() {
    // SAFETY: async-signal-safe; called immediately after fork in the child.
    unsafe {
        libc::signal(libc::SIGINT, libc::SIG_DFL);
    }
}

fn exited_cleanly(status: libc::c_int) -> bool {
    libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0
}

/// Fork `n_workers` children; each runs `worker(w)` once inside a
/// `catch_unwind` guard and `_exit`s (0 on `Ok(())`, 42 otherwise). The parent
/// joins all; on the first failure it `SIGKILL`s the survivors and returns the
/// error.
///
/// `worker` runs in the child's copy-on-write address space, so its mutation of
/// any captured state (the machine, the heap) is private to that process — the
/// parent is unaffected.
pub fn run_workers<F>(n_workers: usize, mut worker: F) -> Result<(), ForkError>
where
    F: FnMut(usize) -> Result<(), ExecutionError>,
{
    let mut pids: Vec<libc::pid_t> = Vec::with_capacity(n_workers);
    for w in 0..n_workers {
        // SAFETY: single-threaded quiescent fork point (spec §2/§4).
        let pid = unsafe { libc::fork() };
        if pid < 0 {
            let err = std::io::Error::last_os_error();
            kill_and_reap(&pids);
            return Err(ForkError::ForkFailed(err));
        }
        if pid == 0 {
            // ── CHILD ──────────────────────────────────────────────────
            reset_sigint_to_default();
            let ok = matches!(catch_unwind(AssertUnwindSafe(|| worker(w))), Ok(Ok(())));
            // SAFETY: terminate without running parent Drops/atexit or
            // flushing inherited stdio buffers.
            unsafe { libc::_exit(if ok { 0 } else { 42 }) };
        }
        pids.push(pid);
    }
    join_all(&pids)
}

/// Join all workers; on the first failure, kill and reap the rest.
fn join_all(pids: &[libc::pid_t]) -> Result<(), ForkError> {
    let mut first_err: Option<ForkError> = None;
    let mut idx = 0;
    while idx < pids.len() {
        let pid = pids[idx];
        idx += 1;
        let mut status: libc::c_int = 0;
        // SAFETY: reaping our own child.
        let r = unsafe { libc::waitpid(pid, &mut status, 0) };
        let worker = idx - 1;
        if r != pid {
            if first_err.is_none() {
                first_err = Some(ForkError::WaitFailed { worker });
                kill_and_reap(&pids[idx..]);
            }
        } else if !exited_cleanly(status) && first_err.is_none() {
            first_err = Some(ForkError::Worker { worker });
            kill_and_reap(&pids[idx..]);
        }
    }
    match first_err {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

/// SIGKILL and reap a set of still-running workers (best effort).
fn kill_and_reap(pids: &[libc::pid_t]) {
    for &pid in pids {
        // SAFETY: signalling/reaping our own children.
        unsafe {
            libc::kill(pid, libc::SIGKILL);
            let mut status: libc::c_int = 0;
            libc::waitpid(pid, &mut status, 0);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::stg::parallel::arena::Arena;

    #[test]
    fn workers_write_disjoint_segments() {
        let arena = Arena::new(4096, 4).unwrap();
        let result = run_workers(4, |w| {
            let mut writer = arena.writer(w);
            writer.push(&(w as u64).to_le_bytes()).unwrap();
            writer.finish();
            Ok(())
        });
        assert!(result.is_ok(), "all workers should succeed");
        for w in 0..4usize {
            let mut r = arena.reader(w);
            let rec = r.next().expect("a record");
            assert_eq!(u64::from_le_bytes(rec.try_into().unwrap()), w as u64);
        }
    }

    #[test]
    fn worker_failure_is_reported() {
        let result = run_workers(4, |w| {
            if w == 2 {
                Err(ExecutionError::NotScalar(Default::default()))
            } else {
                Ok(())
            }
        });
        assert!(matches!(result, Err(ForkError::Worker { worker: 2 })));
    }

    #[test]
    fn worker_panic_is_contained() {
        let result = run_workers(2, |w| {
            if w == 1 {
                panic!("boom in a worker");
            }
            Ok(())
        });
        assert!(matches!(result, Err(ForkError::Worker { worker: 1 })));
    }
}
