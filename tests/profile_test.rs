//! `eu run --profile` attributes ticks, allocations and bytes to source
//! functions (eu-oaz72.37).
//!
//! Drives the real binary: the profile is assembled across the machine's
//! dispatch loops, the driver and the source map, and only an end-to-end run
//! shows that a known hot function is the one charged.

use std::process::Command;

/// `hot` loops 20,000 times in user code; `cold` does one addition. The
/// profile must charge the loop's ticks to `hot`, not to `cold` or to an
/// unresolved location.
const PROGRAM: &str = "\
hot(n, acc): if(n = 0, acc, hot(n - 1, acc + n * 2))
cold(n): n + 1
result: { a: hot(20000, 0) b: cold(4) }
";

struct Row {
    ticks: u64,
    allocs: u64,
    bytes: u64,
    location: String,
}

/// Parse the profile table from stderr: the header's tick total and the
/// data rows.
fn parse_profile(stderr: &str) -> (u64, Vec<Row>) {
    let mut lines = stderr.lines().skip_while(|l| !l.starts_with("Profile: "));
    let header = lines.next().expect("profile header on stderr");
    let total: u64 = header
        .trim_start_matches("Profile: ")
        .split_whitespace()
        .next()
        .and_then(|n| n.parse().ok())
        .expect("tick total in header");
    let rows = lines
        .skip(1) // column titles
        .take_while(|l| !l.trim().is_empty() && !l.starts_with('═'))
        .map(|l| {
            let cols: Vec<&str> = l.split_whitespace().collect();
            // The location follows the last percentage column.
            let location = l
                .rsplit_once("%  ")
                .map(|(_, loc)| loc.to_string())
                .expect("location column");
            Row {
                ticks: cols[0].parse().expect("ticks"),
                allocs: cols[2].parse().expect("allocs"),
                bytes: cols[4].parse().expect("bytes"),
                location,
            }
        })
        .collect();
    (total, rows)
}

fn ticks_from_statistics(stderr: &str) -> u64 {
    stderr
        .lines()
        .find(|l| l.trim_start().starts_with("Ticks"))
        .and_then(|l| l.rsplit(':').next())
        .map(|n| n.trim().replace(',', ""))
        .and_then(|n| n.parse().ok())
        .expect("Ticks line in -S output")
}

#[test]
fn profile_charges_the_hot_function() {
    let dir = tempfile::tempdir().expect("temp dir");
    let program = dir.path().join("prog.eu");
    std::fs::write(&program, PROGRAM).expect("write program");
    let folded = dir.path().join("prog.folded");

    let output = Command::new(env!("CARGO_BIN_EXE_eu"))
        .arg("run")
        .arg("--profile")
        .arg("--profile-top")
        .arg("1000")
        .arg("--profile-folded")
        .arg(&folded)
        .arg("-S")
        .arg(&program)
        .output()
        .expect("run eu");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "eu failed: {stderr}");
    assert!(
        String::from_utf8_lossy(&output.stdout).contains("a: 400020000"),
        "unexpected output"
    );

    let (total, rows) = parse_profile(&stderr);

    // Every tick is charged exactly once: the table's total is the -S total,
    // and the rows add up to it.
    assert_eq!(total, ticks_from_statistics(&stderr), "{stderr}");
    assert_eq!(rows.iter().map(|r| r.ticks).sum::<u64>(), total);

    let charged = |name: &str| -> (u64, u64, u64) {
        rows.iter()
            .filter(|r| r.location.starts_with(&format!("{name} at prog.eu:")))
            .fold((0, 0, 0), |(t, a, b), r| {
                (t + r.ticks, a + r.allocs, b + r.bytes)
            })
    };
    let (hot_ticks, _hot_allocs, hot_bytes) = charged("hot");
    let (cold_ticks, _, _) = charged("cold");

    // The loop body runs 20,000 times, at several ticks per iteration, in
    // `hot`'s own code.
    assert!(
        hot_ticks > 100_000,
        "hot charged only {hot_ticks} ticks:\n{stderr}"
    );
    assert!(hot_bytes > 0, "hot charged no bytes:\n{stderr}");
    assert!(
        cold_ticks < 1_000,
        "cold charged {cold_ticks} ticks:\n{stderr}"
    );
    // `hot` is the costliest user-file location.
    let top_user = rows
        .iter()
        .find(|r| r.location.contains(" at prog.eu:"))
        .expect("a user-file row");
    assert!(
        top_user.location.starts_with("hot at prog.eu:"),
        "top user row is {}:\n{stderr}",
        top_user.location
    );

    // The folded stacks name the hot function too.
    let folded = std::fs::read_to_string(&folded).expect("folded stacks written");
    assert!(
        folded.lines().any(|l| l.contains("hot at prog.eu:")),
        "folded stacks do not mention hot:\n{folded}"
    );
    assert!(folded.lines().all(|l| l
        .rsplit_once(' ')
        .is_some_and(|(_, n)| n.parse::<u64>().is_ok())));
}

#[test]
fn profile_is_off_by_default() {
    let dir = tempfile::tempdir().expect("temp dir");
    let program = dir.path().join("prog.eu");
    std::fs::write(&program, PROGRAM).expect("write program");
    let output = Command::new(env!("CARGO_BIN_EXE_eu"))
        .arg("run")
        .arg(&program)
        .output()
        .expect("run eu");
    assert!(output.status.success());
    assert!(!String::from_utf8_lossy(&output.stderr).contains("Profile: "));
}
