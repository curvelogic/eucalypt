//! `cargo xtask engine-ab` — run the canonical engine A/B suite under the
//! measurement protocol and append rows to the results ledger.
//!
//! This is the operational half of `docs/superpowers/engine-ab/PROTOCOL.md`
//! (bead eu-2sa6.6). It runs each bench in the canonical suite interleaved on
//! both engines (bytecode default, HeapSyn via `EU_HEAPSYN=1`), reports wall
//! medians with spread and the HeapSyn-side deterministic metrics
//! (ticks/allocs/GC), and appends one JSONL row per bench to
//! `docs/superpowers/engine-ab/results.jsonl`.
//!
//! Both engines run the SAME binary at the SAME filesystem path, selected by
//! the `EU_HEAPSYN` environment variable — the strictest realisation of the
//! protocol's "same path / same blob" fairness rule (no binary-path-length or
//! blob-fairness skew is possible when it is literally one binary).
//!
//! Usage:
//!   cargo xtask engine-ab [--runs N] [--eu PATH] [--dry-run]
//!   cargo xtask engine-ab --check
//!
//! `--check` reads the last two rows per bench and flags regressions
//! (ratio worsened >15% vs the previous run) and per-class threshold
//! crossings; it appends nothing and exits 1 if any regression is found.

use std::{
    path::{Path, PathBuf},
    process::Command,
    time::Instant,
};

use anyhow::{bail, Context, Result};

const LEDGER: &str = "docs/superpowers/engine-ab/results.jsonl";
const HEAP_LIMIT: &str = "12288";

/// One bench in the canonical suite.
struct Bench {
    /// File stem (also the ledger `bench` field).
    id: &'static str,
    /// Path relative to the workspace root.
    file: &'static str,
    /// The `-t` target inside the file.
    target: &'static str,
    /// Workload class (review C taxonomy).
    class: &'static str,
    /// Needs `--allow-io`.
    requires_io: bool,
}

/// The canonical suite (review C section 3; eu-2sa6.6).
const SUITE: &[Bench] = &[
    Bench {
        id: "015_block_merge",
        file: "tests/harness/bench/015_block_merge.eu",
        target: "bench-block-merge",
        class: "D",
        requires_io: false,
    },
    Bench {
        id: "016_import_export_yaml",
        file: "tests/harness/bench/016_import_export_yaml.eu",
        target: "bench-import-yaml",
        class: "I",
        requires_io: false,
    },
    Bench {
        id: "017_import_export_toml",
        file: "tests/harness/bench/017_import_export_toml.eu",
        target: "bench-import-toml",
        class: "I",
        requires_io: false,
    },
    Bench {
        id: "018_string_scale",
        file: "tests/harness/bench/018_string_scale.eu",
        target: "bench-string-scale",
        class: "G",
        requires_io: false,
    },
    Bench {
        id: "019_list_scale",
        file: "tests/harness/bench/019_list_scale.eu",
        target: "bench-list-scale",
        class: "H",
        requires_io: false,
    },
    Bench {
        id: "020_lookup_curve",
        file: "tests/harness/bench/020_lookup_curve.eu",
        target: "bench-lookup-curve",
        class: "E",
        requires_io: false,
    },
    Bench {
        id: "021_io_loop",
        file: "tests/harness/bench/021_io_loop.eu",
        target: "bench-io-loop",
        class: "L",
        requires_io: true,
    },
    Bench {
        id: "022_hof_fold",
        file: "tests/harness/bench/022_hof_fold.eu",
        target: "bench-hof-fold",
        class: "C",
        requires_io: false,
    },
];

/// Per-class "bytecode wins" threshold (review C section 3 / review section 6).
/// A bc/hs ratio above this is out of band for the class. Class E (the static
/// large-block lookup tripwire) is intentionally asymmetric — its ratio is the
/// finding, not a target — so a crossing there is reported as WATCH, never a
/// hard regression.
fn class_threshold(class: &str) -> f64 {
    match class {
        "A" => 1.00,
        "B" => 1.15,
        "C" => 1.00,
        "D" => 1.05,
        "E" => 1.10,
        "F" => 1.00,
        "G" => 1.05,
        "H" => 1.00,
        "I" => 1.05,
        "L" => 1.05,
        _ => 1.10,
    }
}

/// The regression band from review A P3 — anything inside ±15% is noise.
const REGRESSION_BAND: f64 = 0.15;

pub fn run(args: &mut dyn Iterator<Item = String>) -> Result<()> {
    let root = workspace_root()?;
    let mut runs: usize = 5; // protocol: >=5 multi-second (the whole suite is)
    let mut eu = root.join("target/release/eu");
    let mut check = false;
    let mut dry_run = false;

    let mut it = args.peekable();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--check" => check = true,
            "--dry-run" => dry_run = true,
            "--runs" => {
                runs = it
                    .next()
                    .context("--runs needs a value")?
                    .parse()
                    .context("--runs value")?;
            }
            "--eu" => {
                eu = PathBuf::from(it.next().context("--eu needs a path")?);
            }
            other => bail!("unknown engine-ab arg: {other}"),
        }
    }

    if check {
        return cmd_check(&root);
    }

    if !eu.exists() {
        bail!(
            "eu binary not found at {} — build it first (cargo build --release)",
            eu.display()
        );
    }

    let commit = git_short_commit(&root).unwrap_or_else(|| "unknown".to_string());
    let host = host_string();
    let date = today();
    let prelude_config = if root.join("lib/prelude.blob").exists() {
        "blob"
    } else {
        "source"
    };

    println!(
        "engine-ab: {} benches, {runs} interleaved runs each",
        SUITE.len()
    );
    println!("  eu     = {}", eu.display());
    println!("  commit = {commit}   host = {host}   prelude = {prelude_config}");
    println!("  ticks/allocs/gc read from the HeapSyn (-S) pass\n");

    println!(
        "{:<26} {:>3} {:>9} {:>9} {:>7}  {:>14} {:>12} {:>4}",
        "bench (class)", "n", "bc_med", "hs_med", "ratio", "hs_ticks", "hs_allocs", "gc"
    );

    let mut rows: Vec<String> = Vec::new();
    let suite_start = Instant::now();

    for b in SUITE {
        let file = root.join(b.file);
        // Interleaved bc/hs wall timings.
        let mut bc = Vec::with_capacity(runs);
        let mut hs = Vec::with_capacity(runs);
        for _ in 0..runs {
            bc.push(time_run(&eu, &file, b, false, false)?);
            hs.push(time_run(&eu, &file, b, true, false)?);
        }
        // Separate HeapSyn -S pass for deterministic metrics (kept out of the
        // wall medians so `-S` reporting overhead never skews the ratio).
        let (ticks, allocs, gc) = stats_run(&eu, &file, b)?;

        let bc_med = median(&mut bc);
        let hs_med = median(&mut hs);
        let ratio = bc_med / hs_med;
        let bc_spread = spread(&bc);
        let hs_spread = spread(&hs);

        println!(
            "{:<26} {:>3} {:>8.3}s {:>8.3}s {:>7.3}  {:>14} {:>12} {:>4}",
            format!("{} ({})", short_id(b.id), b.class),
            runs,
            bc_med,
            hs_med,
            ratio,
            ticks,
            allocs,
            gc
        );
        println!(
            "{:<26}     spread bc [{:.3}..{:.3}] hs [{:.3}..{:.3}]",
            "", bc_spread.0, bc_spread.1, hs_spread.0, hs_spread.1
        );

        rows.push(row_json(
            &date,
            &commit,
            b,
            bc_med,
            hs_med,
            ratio,
            ticks,
            allocs,
            gc,
            &host,
            runs,
            prelude_config,
        ));
    }

    println!("\nsuite wall: {:.1}s", suite_start.elapsed().as_secs_f64());

    if dry_run {
        println!("\n--dry-run: not appending to {LEDGER}");
        return Ok(());
    }

    append_rows(&root, &rows)?;
    println!("\nappended {} rows to {LEDGER}", rows.len());
    Ok(())
}

/// Run one invocation and return its wall time in seconds.
fn time_run(eu: &Path, file: &Path, b: &Bench, heapsyn: bool, stats: bool) -> Result<f64> {
    let mut cmd = base_cmd(eu, file, b, heapsyn, stats);
    let start = Instant::now();
    let out = cmd.output().with_context(|| format!("run {}", b.id))?;
    let secs = start.elapsed().as_secs_f64();
    if !out.status.success() {
        bail!(
            "{} ({}) failed on {} engine:\n{}",
            b.id,
            b.target,
            if heapsyn { "HeapSyn" } else { "bytecode" },
            String::from_utf8_lossy(&out.stderr)
        );
    }
    Ok(secs)
}

/// Run one HeapSyn `-S` pass and parse ticks / allocs / GC collections.
fn stats_run(eu: &Path, file: &Path, b: &Bench) -> Result<(u64, u64, u64)> {
    let out = base_cmd(eu, file, b, true, true)
        .output()
        .with_context(|| format!("stats run {}", b.id))?;
    let text = String::from_utf8_lossy(&out.stderr);
    let ticks = parse_stat(&text, "Ticks").unwrap_or(0);
    let allocs = parse_stat(&text, "Allocs").unwrap_or(0);
    let gc = parse_stat(&text, "Collections").unwrap_or(0);
    Ok((ticks, allocs, gc))
}

fn base_cmd(eu: &Path, file: &Path, b: &Bench, heapsyn: bool, stats: bool) -> Command {
    let mut cmd = Command::new(eu);
    if heapsyn {
        cmd.env("EU_HEAPSYN", "1");
    }
    if stats {
        cmd.arg("-S");
    }
    cmd.arg("--heap-limit-mib").arg(HEAP_LIMIT);
    if b.requires_io {
        cmd.arg("--allow-io");
    }
    cmd.arg("-t").arg(b.target).arg(file);
    cmd
}

/// Parse a `Label          :     12,345,678` stat line.
fn parse_stat(text: &str, label: &str) -> Option<u64> {
    for line in text.lines() {
        let t = line.trim_start();
        if t.starts_with(label) {
            if let Some((_, rhs)) = line.split_once(':') {
                let digits: String = rhs.chars().filter(|c| c.is_ascii_digit()).collect();
                if !digits.is_empty() {
                    return digits.parse().ok();
                }
            }
        }
    }
    None
}

fn median(xs: &mut [f64]) -> f64 {
    xs.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let n = xs.len();
    if n == 0 {
        0.0
    } else if n % 2 == 1 {
        xs[n / 2]
    } else {
        (xs[n / 2 - 1] + xs[n / 2]) / 2.0
    }
}

fn spread(xs: &[f64]) -> (f64, f64) {
    let mut min = f64::MAX;
    let mut max = f64::MIN;
    for &x in xs {
        min = min.min(x);
        max = max.max(x);
    }
    (min, max)
}

#[allow(clippy::too_many_arguments)]
fn row_json(
    date: &str,
    commit: &str,
    b: &Bench,
    bc: f64,
    hs: f64,
    ratio: f64,
    ticks: u64,
    allocs: u64,
    gc: u64,
    host: &str,
    runs: usize,
    prelude_config: &str,
) -> String {
    let v = serde_json::json!({
        "date": date,
        "commit": commit,
        "bench": b.id,
        "class": b.class,
        "bc_wall_med": round3(bc),
        "hs_wall_med": round3(hs),
        "ratio": round3(ratio),
        "hs_ticks": ticks,
        "hs_allocs": allocs,
        "gc": gc,
        "host": host,
        "runs": runs,
        "prelude_config": prelude_config,
    });
    v.to_string()
}

fn round3(x: f64) -> f64 {
    (x * 1000.0).round() / 1000.0
}

fn append_rows(root: &Path, rows: &[String]) -> Result<()> {
    use std::io::Write;
    let path = root.join(LEDGER);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).ok();
    }
    let mut f = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
        .with_context(|| format!("open {}", path.display()))?;
    for r in rows {
        writeln!(f, "{r}")?;
    }
    Ok(())
}

// ── --check ────────────────────────────────────────────────────────────────

fn cmd_check(root: &Path) -> Result<()> {
    let path = root.join(LEDGER);
    let text =
        std::fs::read_to_string(&path).with_context(|| format!("read {}", path.display()))?;

    // Collect rows per bench in file order (append-only ⇒ chronological).
    let mut per_bench: std::collections::BTreeMap<String, Vec<serde_json::Value>> =
        Default::default();
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let v: serde_json::Value =
            serde_json::from_str(line).with_context(|| format!("parse ledger row: {line}"))?;
        if let Some(bench) = v.get("bench").and_then(|b| b.as_str()) {
            per_bench.entry(bench.to_string()).or_default().push(v);
        }
    }

    println!(
        "{:<26} {:>3} {:>8} {:>8} {:>8} {:>8}  {}",
        "bench", "cls", "prev", "last", "delta%", "thresh", "status"
    );

    let mut regressed = false;
    for (bench, rows) in &per_bench {
        let last = rows.last().unwrap();
        let class = last.get("class").and_then(|c| c.as_str()).unwrap_or("?");
        let last_ratio = last.get("ratio").and_then(|r| r.as_f64()).unwrap_or(0.0);
        let thresh = class_threshold(class);

        let (prev_ratio, delta) = if rows.len() >= 2 {
            let p = rows[rows.len() - 2]
                .get("ratio")
                .and_then(|r| r.as_f64())
                .unwrap_or(last_ratio);
            (p, (last_ratio - p) / p)
        } else {
            (f64::NAN, 0.0)
        };

        // Regression = ratio worsened by more than the noise band.
        let is_regression = rows.len() >= 2 && delta > REGRESSION_BAND;
        // Watch = out of the class band (informational; class E is a tripwire).
        let over_threshold = last_ratio > thresh;

        let status = if is_regression {
            regressed = true;
            "REGRESSED"
        } else if over_threshold && class == "E" {
            "WATCH (tripwire)"
        } else if over_threshold {
            "WATCH (over band)"
        } else {
            "ok"
        };

        let prev_s = if prev_ratio.is_nan() {
            "  --  ".to_string()
        } else {
            format!("{prev_ratio:>8.3}")
        };
        let delta_s = if prev_ratio.is_nan() {
            "  --  ".to_string()
        } else {
            format!("{:>+7.1}%", delta * 100.0)
        };
        println!(
            "{:<26} {:>3} {} {:>8.3} {} {:>8.3}  {}",
            short_id(bench),
            class,
            prev_s,
            last_ratio,
            delta_s,
            thresh,
            status
        );
    }

    if regressed {
        println!("\nengine-ab --check: REGRESSION(S) found (ratio worsened >15% vs previous).");
        std::process::exit(1);
    }
    println!("\nengine-ab --check: no regressions (>15% worsening) vs previous run.");
    Ok(())
}

// ── helpers ────────────────────────────────────────────────────────────────

fn short_id(id: &str) -> &str {
    // Trim the leading NNN_ index for a tidier table.
    id.split_once('_').map(|(_, s)| s).unwrap_or(id)
}

fn workspace_root() -> Result<PathBuf> {
    let cwd = std::env::current_dir().context("current_dir")?;
    if cwd.join("Cargo.toml").exists() {
        return Ok(cwd);
    }
    bail!("could not find workspace root (no Cargo.toml in {cwd:?})")
}

fn git_short_commit(root: &Path) -> Option<String> {
    let out = Command::new("git")
        .current_dir(root)
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    Some(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

fn host_string() -> String {
    let out = Command::new("uname").arg("-srm").output().ok();
    match out {
        Some(o) if o.status.success() => {
            String::from_utf8_lossy(&o.stdout).trim().replace(' ', "-")
        }
        _ => format!("{}-{}", std::env::consts::OS, std::env::consts::ARCH),
    }
}

fn today() -> String {
    // Cheap ISO date without a chrono dependency: shell out to `date`.
    let out = Command::new("date").arg("+%Y-%m-%d").output().ok();
    match out {
        Some(o) if o.status.success() => String::from_utf8_lossy(&o.stdout).trim().to_string(),
        _ => "unknown".to_string(),
    }
}
