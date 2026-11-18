//! Helpers shared by integration tests that spawn the `eu` binary.

use std::io::Read;
use std::process::{Child, Command, Output, Stdio};
use std::sync::mpsc;
use std::time::{Duration, Instant};

/// How long the drain may run on after the child exits, even past the
/// deadline, so output the child wrote just before exiting is collected
/// rather than cut short. A grandchild holding the pipes open still
/// costs at most this much beyond the deadline.
const DRAIN_GRACE: Duration = Duration::from_secs(2);

/// Stream a pipe's bytes to `tx` in chunks until EOF or error.
fn drain<R: Read + Send + 'static>(pipe: Option<R>, tx: mpsc::Sender<Vec<u8>>) {
    std::thread::spawn(move || {
        if let Some(p) = pipe {
            forward(p, &tx);
        }
    });
}

/// Read `pipe` to EOF, sending each chunk to `tx`. A read interrupted by
/// a signal (`ErrorKind::Interrupted`) is retried, not taken for EOF; any
/// other error ends the stream.
fn forward<R: Read>(mut pipe: R, tx: &mpsc::Sender<Vec<u8>>) {
    let mut chunk = [0u8; 8192];
    loop {
        match pipe.read(&mut chunk) {
            Ok(0) => break,
            Ok(n) => {
                if tx.send(chunk[..n].to_vec()).is_err() {
                    break;
                }
            }
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(_) => break,
        }
    }
}

/// When collection stops: at the deadline, but never sooner than
/// [`DRAIN_GRACE`] after `exited` (when the child was seen to exit).
fn drain_until(end: Instant, exited: Instant) -> Instant {
    end.max(exited + DRAIN_GRACE)
}

/// Collect chunks from `rx` into `buf` until the sender side closes
/// (EOF) or `until` passes. Returns whether EOF was reached.
fn collect(rx: &mpsc::Receiver<Vec<u8>>, buf: &mut Vec<u8>, until: Instant) -> bool {
    loop {
        let left = until.saturating_duration_since(Instant::now());
        match rx.recv_timeout(left) {
            Ok(chunk) => buf.extend_from_slice(&chunk),
            Err(mpsc::RecvTimeoutError::Disconnected) => return true,
            Err(mpsc::RecvTimeoutError::Timeout) => return false,
        }
    }
}

fn kill_and_reap(child: &mut Child) {
    let _ = child.kill();
    let _ = child.wait();
}

/// Run `cmd`, killing it and returning `None` if it doesn't complete
/// within `deadline`.
///
/// Some fixtures are not designed to terminate under plain evaluation
/// (e.g. `092_self_assign_arg_pos_ok.eu`, a deliberate infinite lazy
/// fixpoint used only to verify the checker doesn't flag it), so a
/// bounded wait is required rather than `Command::output()`'s
/// indefinite block.
///
/// Both pipes are drained while waiting, so a chatty child cannot block
/// on a full pipe. The drain is also bounded by `deadline`: a
/// grandchild that inherits the pipes and outlives the child keeps them
/// open, so EOF may never arrive. Past the deadline (or [`DRAIN_GRACE`]
/// after the child's exit, if that is later, so a child exiting just
/// before the deadline still has its output collected) the helper stops
/// waiting for it and returns whatever output it has collected (the
/// blocked reader threads are left detached until the grandchild
/// exits). Only the direct child is killed on timeout.
pub fn run_with_deadline(mut cmd: Command, deadline: Duration) -> Option<Output> {
    use wait_timeout::ChildExt;

    let end = Instant::now() + deadline;
    let mut child = cmd
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn child");

    let (out_tx, out_rx) = mpsc::channel();
    let (err_tx, err_rx) = mpsc::channel();
    drain(child.stdout.take(), out_tx);
    drain(child.stderr.take(), err_tx);

    let status = match child.wait_timeout(deadline).expect("wait_timeout failed") {
        Some(status) => status,
        None => {
            kill_and_reap(&mut child);
            return None;
        }
    };
    let until = drain_until(end, Instant::now());
    let mut stdout = Vec::new();
    let mut stderr = Vec::new();
    collect(&out_rx, &mut stdout, until);
    collect(&err_rx, &mut stderr, until);
    Some(Output {
        status,
        stdout,
        stderr,
    })
}

// These run in each integration-test crate that includes this module; they
// are cheap.
#[cfg(test)]
mod tests {
    use super::*;
    use std::io;

    /// A reader that is interrupted by a signal once, then yields `hi`.
    struct InterruptedOnce(u8);

    impl Read for InterruptedOnce {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            self.0 += 1;
            match self.0 {
                1 => Err(io::ErrorKind::Interrupted.into()),
                2 => {
                    buf[..2].copy_from_slice(b"hi");
                    Ok(2)
                }
                _ => Ok(0),
            }
        }
    }

    #[test]
    fn forward_retries_an_interrupted_read() {
        let (tx, rx) = mpsc::channel();
        forward(InterruptedOnce(0), &tx);
        drop(tx);
        let got: Vec<u8> = rx.iter().flatten().collect();
        assert_eq!(got, b"hi");
    }

    #[test]
    fn drain_runs_on_past_a_deadline_the_child_exited_near() {
        let now = Instant::now();
        // The child exited just before the deadline: the grace applies.
        assert_eq!(drain_until(now, now), now + DRAIN_GRACE);
        // It exited long before: the deadline stands.
        let later = now + DRAIN_GRACE * 5;
        assert_eq!(drain_until(later, now), later);
    }
}
