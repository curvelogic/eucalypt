//! Representation-agnostic IO action execution.
//!
//! The shell / exec side of the IO monad driver: the action spec extracted
//! from an `IoAction` spec block, its execution (`sh -c` / direct binary with
//! stdin + timeout), the command result, and the driver error type. These are
//! shared verbatim by both the HeapSyn io-run driver (`driver::io_run`) and
//! the bytecode io-run driver (`driver::bytecode_io_run`) — only the heap
//! navigation that *builds* an `ActionSpec` and consumes a `CommandResult`
//! differs between the two engines.

use std::{
    io::Write as IoWrite,
    process::{Command, Stdio},
    sync::mpsc,
    thread,
    time::Duration,
};

use crate::common::sourcemap::Smid;
use crate::eval::error::ExecutionError;

// ─── Public error type ────────────────────────────────────────────────────────

/// Error from running the IO monad interpret loop.
#[derive(Debug, thiserror::Error)]
pub enum IoRunError {
    #[error("io.fail: {0}")]
    Fail(String),
    #[error("IO operations are not permitted; use the --allow-io (-I) flag to enable")]
    IoNotAllowed(Smid),
    #[error("io.shell-with: command timed out after {0} seconds")]
    Timeout(Smid, u64),
    #[error("io.shell-with: command execution error: {0}")]
    CommandError(Smid, String),
    /// Boxed to keep the error variant size small (ExecutionError is large).
    #[error("STG machine error: {0}")]
    MachineError(Box<ExecutionError>),
}

impl From<ExecutionError> for IoRunError {
    fn from(e: ExecutionError) -> Self {
        IoRunError::MachineError(Box::new(e))
    }
}

// ─── Command result ───────────────────────────────────────────────────────────

/// The outcome of a shell command execution.
#[derive(Debug)]
pub struct CommandResult {
    pub stdout: String,
    pub stderr: String,
    pub exit_code: i64,
}

// ─── Action spec (extracted from an IoAction spec block) ─────────────────────

/// A parsed IO action, ready to execute. Built by each engine's driver from
/// the evaluated spec block, then run by [`run_spec`].
#[derive(Debug)]
pub enum ActionSpec {
    Shell {
        cmd: String,
        stdin: Option<String>,
        timeout_secs: u64,
    },
    Exec {
        cmd: String,
        args: Vec<String>,
        stdin: Option<String>,
        timeout_secs: u64,
    },
}

// ─── Shell execution ──────────────────────────────────────────────────────────

/// Execute a shell command via the platform shell.
///
/// On Unix, uses `sh -c`. On Windows, uses `pwsh -NoProfile -Command`
/// (PowerShell Core). Shell command strings are inherently
/// platform-specific.
fn execute_shell(
    cmd: &str,
    stdin_data: Option<&str>,
    timeout_secs: u64,
    call_smid: Smid,
) -> Result<CommandResult, IoRunError> {
    let command = if cfg!(windows) {
        let mut c = Command::new("pwsh");
        c.args(["-NoProfile", "-Command", cmd]);
        c
    } else {
        let mut c = Command::new("sh");
        c.args(["-c", cmd]);
        c
    };
    run_command(command, stdin_data, timeout_secs, call_smid)
}

/// Execute a binary directly.
fn execute_exec(
    cmd: &str,
    args: &[String],
    stdin_data: Option<&str>,
    timeout_secs: u64,
    call_smid: Smid,
) -> Result<CommandResult, IoRunError> {
    let mut command = Command::new(cmd);
    command.args(args);
    run_command(command, stdin_data, timeout_secs, call_smid)
}

/// Core helper: spawn the command, optionally write stdin, and wait with timeout.
fn run_command(
    mut command: Command,
    stdin_data: Option<&str>,
    timeout_secs: u64,
    call_smid: Smid,
) -> Result<CommandResult, IoRunError> {
    command
        .stdin(if stdin_data.is_some() {
            Stdio::piped()
        } else {
            Stdio::null()
        })
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = match command.spawn() {
        Ok(child) => child,
        Err(e) => {
            // Spawn failure (e.g. binary not found) returns a result block
            // rather than a hard error, matching shell conventions.
            let exit_code = match e.kind() {
                std::io::ErrorKind::NotFound => 127,
                std::io::ErrorKind::PermissionDenied => 126,
                _ => -1,
            };
            return Ok(CommandResult {
                stdout: String::new(),
                stderr: e.to_string(),
                exit_code,
            });
        }
    };

    if let Some(input) = stdin_data {
        if let Some(mut pipe) = child.stdin.take() {
            pipe.write_all(input.as_bytes())
                .map_err(|e| IoRunError::CommandError(call_smid, e.to_string()))?;
            // Drop `pipe` to close stdin and signal EOF to the child
        }
    }

    // Use a worker thread to implement timeout, since `wait_timeout` is not a
    // project dependency.
    let (tx, rx) = mpsc::channel();
    thread::spawn(move || {
        let _ = tx.send(child.wait_with_output());
    });

    match rx.recv_timeout(Duration::from_secs(timeout_secs)) {
        Ok(Ok(output)) => {
            let exit_code = output.status.code().unwrap_or(-1) as i64;
            let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
            let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
            Ok(CommandResult {
                stdout,
                stderr,
                exit_code,
            })
        }
        Ok(Err(e)) => Err(IoRunError::CommandError(call_smid, e.to_string())),
        Err(_timeout) => Err(IoRunError::Timeout(call_smid, timeout_secs)),
    }
}

/// Whether IO tracing is enabled (cached from `EU_IO_TRACE` env var).
static IO_TRACE: std::sync::LazyLock<bool> =
    std::sync::LazyLock::new(|| std::env::var("EU_IO_TRACE").is_ok());

/// Dispatch an `ActionSpec` to the appropriate executor.
pub fn run_spec(spec: &ActionSpec, call_smid: Smid) -> Result<CommandResult, IoRunError> {
    if *IO_TRACE {
        match spec {
            ActionSpec::Shell { cmd, stdin, .. } => {
                eprintln!(
                    "IO TRACE: shell {cmd:?}{}",
                    if stdin.is_some() { " (with stdin)" } else { "" }
                );
            }
            ActionSpec::Exec {
                cmd, args, stdin, ..
            } => {
                eprintln!(
                    "IO TRACE: exec {cmd:?} {args:?}{}",
                    if stdin.is_some() { " (with stdin)" } else { "" }
                );
            }
        }
    }
    let result = match spec {
        ActionSpec::Shell {
            cmd,
            stdin,
            timeout_secs,
        } => execute_shell(cmd, stdin.as_deref(), *timeout_secs, call_smid),
        ActionSpec::Exec {
            cmd,
            args,
            stdin,
            timeout_secs,
        } => execute_exec(cmd, args, stdin.as_deref(), *timeout_secs, call_smid),
    };
    if *IO_TRACE {
        match &result {
            Ok(r) => eprintln!("IO TRACE: → exit {}", r.exit_code),
            Err(e) => eprintln!("IO TRACE: → error: {e}"),
        }
    }
    result
}
