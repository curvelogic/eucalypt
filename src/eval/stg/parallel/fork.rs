//! Unix `fork`/`waitpid` worker coordination for process parallelism, with the
//! worker-hygiene rules the fork-safety spike mandated (§4 addendum, R3/R4):
//!
//! - A child leaves **only** via `libc::_exit` — never unwinding past the fork
//!   frame (so no parent `Drop`s/atexit run twice, no inherited stdio buffers
//!   are flushed by a child) and never touching stdout.
//! - A child resets `SIGINT` — and the inherited `SIGSEGV`/`SIGBUS` crash
//!   diagnostics handler — to `SIG_DFL` immediately after fork, so a foreground
//!   Ctrl-C terminates workers by default, a faulting worker dies rather than
//!   running a non-async-signal-safe reporter in a forked child, and in both
//!   cases the parent drives cleanup.
//! - The driver forks only where a host has explicitly vouched for the
//!   process ([`declare_fork_safe_host`] / [`process_is_fork_safe`]);
//!   elsewhere `par-map` stays sequential.
//! - On the first worker failure the parent `SIGKILL`s and reaps the
//!   survivors, then reports the failure. The converse is not covered: if the
//!   *parent* is killed by something other than a process-group signal — a
//!   bare `kill -TERM` rather than a foreground Ctrl-C — the workers run to
//!   completion unattended before exiting. They are bounded by their own
//!   chunks and write only into the shared mapping, so they cannot corrupt
//!   anything, but they do keep burning cores until they finish. The caller (the PP driver) treats any
//!   failure as a signal to fall back to a **sequential** re-evaluation in the
//!   parent — which reproduces the exact result or raises the exact
//!   user-facing error with a proper source location.

use std::panic::{catch_unwind, AssertUnwindSafe};
use std::sync::atomic::{AtomicUsize, Ordering};

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

/// Reset inherited signal dispositions in the just-forked child.
///
/// - `SIGINT`: so a foreground Ctrl-C terminates workers by default and the
///   parent's handler drives cleanup.
/// - `SIGSEGV`/`SIGBUS`: the crash-diagnostics handler installs unconditionally
///   in `main()` (see `eval::machine::crash`) and is inherited by children. It
///   walks machine state and formats a report — work that is not
///   async-signal-safe and, run in a forked child, could hang or garble the
///   parent's terminal. A faulting worker should simply die; the parent sees a
///   non-zero wait status and re-runs sequentially, where a genuine fault is
///   reported once, properly, with the handler intact.
fn reset_signals_to_default() {
    // SAFETY: `signal` is async-signal-safe; called immediately after fork in
    // the child, before any other work.
    unsafe {
        libc::signal(libc::SIGINT, libc::SIG_DFL);
        libc::signal(libc::SIGSEGV, libc::SIG_DFL);
        libc::signal(libc::SIGBUS, libc::SIG_DFL);
    }
}

/// Silence the panic reporter in a just-forked worker.
///
/// `catch_unwind` contains a worker panic, but the *default hook has already
/// run* by the time it returns, writing `thread '<unnamed>' panicked at …` to
/// the stderr the child inherited from the parent. A worker fault is not a
/// user-facing event — the parent falls back to a sequential re-run which
/// either succeeds or raises the real error with a proper source location — so
/// a run that ultimately succeeds must not spray Rust panic text at the user.
/// The payload is still lost only in the sense that it was never wanted: the
/// non-zero exit is what the parent acts on.
fn silence_panic_output() {
    std::panic::set_hook(Box::new(|_| {}));
}

/// Number of threads in this process, where we can establish it cheaply.
/// `None` means "unknown".
#[cfg(target_os = "linux")]
fn thread_count() -> Option<usize> {
    // Each live thread has a directory under /proc/self/task.
    std::fs::read_dir("/proc/self/task")
        .ok()
        .map(|entries| entries.filter(|e| e.is_ok()).count())
        .filter(|n| *n > 0)
}

#[cfg(target_vendor = "apple")]
fn thread_count() -> Option<usize> {
    let mut info: libc::proc_taskinfo = unsafe { std::mem::zeroed() };
    let size = std::mem::size_of::<libc::proc_taskinfo>() as libc::c_int;
    // SAFETY: `info` is a correctly sized, zeroed `proc_taskinfo`; we only
    // read it when the call reports it filled the whole struct.
    let filled = unsafe {
        libc::proc_pidinfo(
            std::process::id() as libc::c_int,
            libc::PROC_PIDTASKINFO,
            0,
            &mut info as *mut _ as *mut libc::c_void,
            size,
        )
    };
    if filled == size && info.pti_threadnum > 0 {
        Some(info.pti_threadnum as usize)
    } else {
        None
    }
}

#[cfg(all(unix, not(target_os = "linux"), not(target_vendor = "apple")))]
fn thread_count() -> Option<usize> {
    None
}

/// Sentinel meaning "no host has vouched for this process".
const NOT_DECLARED: usize = usize::MAX;

/// Thread count at the moment a host declared itself fork-safe, or
/// [`NOT_DECLARED`].
static FORK_HOST_BASELINE: AtomicUsize = AtomicUsize::new(NOT_DECLARED);

/// Declare this process a safe host for the PP `fork()` — **opt in, and only
/// from a process that knows its own threading**.
///
/// Forking hands the child a copy of every lock in whatever state it happened
/// to be in, with only the forking thread alive to unlock it. Being literally
/// single-threaded is not the practical test (the `eu` CLI runs evaluation on a
/// large-stack worker while the original thread parks in `join` and the
/// Ctrl-C watcher parks in a signal wait — three threads, none holding a lock);
/// the test is whether every *other* thread is quiescent, and only the process
/// owner can answer that.
///
/// So the default is **do not fork**, and a host opts in. The `eu` CLI does,
/// for evaluation; the LSP server (`src/driver/lsp/`), the WASM API, embedders
/// and the libtest harness do not, and `par-map` is simply sequential there —
/// the identical result.
///
/// The thread count is recorded so [`process_is_fork_safe`] can withdraw the
/// declaration if threads appear afterwards.
pub fn declare_fork_safe_host() {
    FORK_HOST_BASELINE.store(thread_count().unwrap_or(0), Ordering::SeqCst);
}

/// Whether it is safe to `fork()` here (spec §2/§4): a host vouched for this
/// process *and* no thread has appeared since.
///
/// `EU_PP_ASSUME_SINGLE_THREADED=1` forces it on — for diagnostics; it does not
/// make an unsafe fork safe.
pub fn process_is_fork_safe() -> bool {
    if std::env::var("EU_PP_ASSUME_SINGLE_THREADED").as_deref() == Ok("1") {
        // The override bypasses the gate in *any* host, including ones that
        // deliberately never declared themselves (the LSP server, the WASM
        // API, an embedder). It exists to make the fork path reachable while
        // investigating, so it warns once rather than silently changing what
        // an unvouched-for process does.
        static WARNED: std::sync::Once = std::sync::Once::new();
        WARNED.call_once(|| {
            eprintln!(
                "warning: EU_PP_ASSUME_SINGLE_THREADED=1 bypasses the par-* fork-safety gate; \
                 this is a diagnostic override and does not make an unsafe fork safe"
            );
        });
        return true;
    }
    match FORK_HOST_BASELINE.load(Ordering::SeqCst) {
        NOT_DECLARED => false,
        baseline => match thread_count() {
            // No thread has appeared since the host vouched for the process.
            Some(now) => now <= baseline,
            // This platform gives us no count; the declaration is all we have.
            None => true,
        },
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
            reset_signals_to_default();
            silence_panic_output();
            let ok = matches!(catch_unwind(AssertUnwindSafe(|| worker(w))), Ok(Ok(())));
            // SAFETY: terminate without running parent Drops/atexit or
            // flushing inherited stdio buffers.
            unsafe { libc::_exit(if ok { 0 } else { 42 }) };
        }
        pids.push(pid);
    }
    join_all(&pids)
}

/// `waitpid` a single child, retrying on `EINTR`.
///
/// The `eu` CLI installs a Ctrl-C watcher, so a signal arriving during a long
/// `par-map` is expected rather than exotic. Treating the resulting `EINTR` as
/// a wait failure would abandon a live worker: it is neither killed nor
/// reaped, so it keeps burning a core to the end of its chunk and then becomes
/// a zombie, while the parent redundantly re-runs the whole map sequentially.
fn waitpid_eintr_safe(pid: libc::pid_t, status: &mut libc::c_int) -> libc::pid_t {
    loop {
        // SAFETY: reaping our own child.
        let r = unsafe { libc::waitpid(pid, status, 0) };
        if r < 0 && std::io::Error::last_os_error().raw_os_error() == Some(libc::EINTR) {
            continue;
        }
        return r;
    }
}

/// Join all workers; on the first failure, kill and reap the rest.
fn join_all(pids: &[libc::pid_t]) -> Result<(), ForkError> {
    let mut first_err: Option<ForkError> = None;
    let mut idx = 0;
    while idx < pids.len() {
        let pid = pids[idx];
        idx += 1;
        let mut status: libc::c_int = 0;
        let r = waitpid_eintr_safe(pid, &mut status);
        let worker = idx - 1;
        if r != pid {
            if first_err.is_none() {
                first_err = Some(ForkError::WaitFailed { worker });
                // The kill set includes `pid` itself: the wait failed, so this
                // worker's fate is unknown and it may still be running.
                kill_and_reap(&pids[worker..]);
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
        // SAFETY: signalling our own child.
        unsafe {
            libc::kill(pid, libc::SIGKILL);
        }
        let mut status: libc::c_int = 0;
        waitpid_eintr_safe(pid, &mut status);
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
