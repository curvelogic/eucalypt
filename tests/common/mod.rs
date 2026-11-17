//! Helpers shared by integration tests that spawn the `eu` binary.

use std::io::Read;
use std::process::{Child, Command, Output, Stdio};
use std::sync::mpsc;
use std::time::{Duration, Instant};

/// Stream a pipe's bytes to `tx` in chunks until EOF or error.
fn drain<R: Read + Send + 'static>(pipe: Option<R>, tx: mpsc::Sender<Vec<u8>>) {
    std::thread::spawn(move || {
        let Some(mut p) = pipe else { return };
        let mut chunk = [0u8; 8192];
        while let Ok(n) = p.read(&mut chunk) {
            if n == 0 || tx.send(chunk[..n].to_vec()).is_err() {
                break;
            }
        }
    });
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
/// open, so EOF may never arrive. Past the deadline the helper stops
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
    let mut stdout = Vec::new();
    let mut stderr = Vec::new();
    collect(&out_rx, &mut stdout, end);
    collect(&err_rx, &mut stderr, end);
    Some(Output {
        status,
        stdout,
        stderr,
    })
}
