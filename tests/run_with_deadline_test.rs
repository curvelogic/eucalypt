//! Tests for the shared `run_with_deadline` helper (eu-oaz72.60).
//!
//! No `.eu` program can reach this layer, so it is gated by these unit
//! tests rather than a harness test.

mod common;
use common::run_with_deadline;
use std::process::Command;
use std::time::{Duration, Instant};

#[cfg(unix)]
fn sh(script: &str) -> Command {
    let mut c = Command::new("sh");
    c.args(["-c", script]);
    c
}

#[cfg(windows)]
fn sh(script: &str) -> Command {
    let mut c = Command::new("cmd");
    c.args(["/C", script]);
    c
}

/// A command printing `hi` whose output pipes are inherited by a
/// long-lived grandchild that outlives the command itself.
fn leaves_grandchild() -> Command {
    #[cfg(unix)]
    return sh("sleep 30 & echo hi");
    #[cfg(windows)]
    return sh("start /B ping -n 30 127.0.0.1 & echo hi");
}

#[cfg(unix)]
fn print_then_sleep() -> Command {
    sh("echo hi; sleep 30")
}

#[cfg(windows)]
fn print_then_sleep() -> Command {
    sh("echo hi & ping -n 30 127.0.0.1")
}

#[test]
fn returns_output_of_a_normal_child() {
    let out = run_with_deadline(sh("echo hi"), Duration::from_secs(20)).expect("timed out");
    assert!(out.status.success());
    assert!(String::from_utf8_lossy(&out.stdout).contains("hi"));
}

#[test]
fn drains_more_than_a_pipe_buffer() {
    #[cfg(unix)]
    let cmd = sh("head -c 300000 /dev/zero");
    #[cfg(windows)]
    let cmd = sh("for /L %i in (1,1,20000) do @echo xxxxxxxxxxxxxxxxxxxx");
    let out = run_with_deadline(cmd, Duration::from_secs(30)).expect("timed out");
    assert!(out.stdout.len() >= 200_000);
}

#[test]
fn child_running_past_deadline_returns_none_at_deadline() {
    let start = Instant::now();
    let out = run_with_deadline(print_then_sleep(), Duration::from_secs(1));
    assert!(out.is_none());
    assert!(start.elapsed() < Duration::from_secs(15));
}

#[test]
fn grandchild_holding_pipes_returns_at_deadline() {
    let start = Instant::now();
    let out = run_with_deadline(leaves_grandchild(), Duration::from_secs(2))
        .expect("the child itself exited well before the deadline");
    let elapsed = start.elapsed();
    assert!(
        elapsed < Duration::from_secs(15),
        "blocked on grandchild-held pipes for {elapsed:?}"
    );
    // The child exits at once, so only a grandchild still holding the pipes
    // keeps the drain from seeing EOF until the deadline: without this the
    // test would pass on a platform where no grandchild was spawned.
    assert!(
        elapsed >= Duration::from_millis(1500),
        "returned after {elapsed:?}: no grandchild held the pipes open"
    );
    assert!(out.status.success());
    assert!(String::from_utf8_lossy(&out.stdout).contains("hi"));
}
