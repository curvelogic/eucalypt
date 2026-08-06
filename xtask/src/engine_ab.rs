//! `cargo xtask engine-ab` — historically ran the canonical engine A/B suite
//! (bytecode vs HeapSyn) under the measurement protocol
//! (`docs/superpowers/engine-ab/PROTOCOL.md`, bead eu-2sa6.6) and appended
//! rows to the results ledger.
//!
//! **Phase 4 collapse (eu-oufc):** the HeapSyn engine and its `EU_HEAPSYN`
//! selector were deleted from the driver, so a live bc/hs A/B run is no
//! longer possible — `run()` below refuses with a pointer to the follow-up
//! bead rather than silently measuring bytecode twice under an "hs" label.
//! `--check` still works: it only reads the historical ledger
//! (`docs/superpowers/engine-ab/results.jsonl`), which this change does not
//! touch, and flags regressions (ratio worsened >15% vs the previous run in
//! the same (bench, prelude_config, dispatch) lineage — eu-lhai/eu-hxu6) and
//! per-class threshold crossings.
//!
//! Usage:
//!   cargo xtask engine-ab --check
//!
//! A post-collapse redesign of the live-run half (e.g. a
//! predecoded-vs-byte-dispatch A/B, per eu-1hcw) is tracked by a follow-up
//! bead rather than attempted here.

use std::path::{Path, PathBuf};

use anyhow::{bail, Context, Result};

const LEDGER: &str = "docs/superpowers/engine-ab/results.jsonl";

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
    let mut check = false;

    for a in args {
        match a.as_str() {
            "--check" => check = true,
            other => bail!(
                "engine-ab: the HeapSyn engine was deleted (eu-oufc Phase 4 collapse) — \
                 only --check (read the historical ledger) is supported now; \
                 got {other}. See the eu-hn3j0 follow-up bead for this harness's \
                 post-collapse redesign."
            ),
        }
    }

    if check {
        return cmd_check(&root);
    }

    // Phase 4 collapse (eu-oufc): the HeapSyn engine and its EU_HEAPSYN
    // selector were deleted from the driver, so a live interleaved bc/hs A/B
    // pass — this command's whole former premise — is no longer possible.
    // Refuse clearly rather than silently measuring bytecode twice under an
    // "hs" label, which would corrupt the ledger with mislabelled rows.
    // History in results.jsonl is untouched; `--check` above still reads it.
    bail!(
        "engine-ab: the HeapSyn engine was deleted (eu-oufc Phase 4 collapse) — \
         a live bc/hs A/B run is no longer possible. Use --check to inspect \
         historical ledger rows; see the eu-hn3j0 follow-up bead for this \
         harness's post-collapse redesign."
    );
}

// ── --check ────────────────────────────────────────────────────────────────

/// A (bench, prelude_config, dispatch) key identifying one comparable
/// history within the ledger. Two rows are only ever compared against each
/// other by `cmd_check` when their lineage keys match — see eu-lhai/eu-hxu6.
type LineageKey = (String, String, String);

/// Parse ledger JSONL text into rows grouped by lineage, in file order
/// (append-only ⇒ chronological within a lineage). Grouping by bench alone
/// would compare a blob row against a source row (eu-lhai) or a predecoded
/// row against a byte-dispatch row (eu-hxu6) whenever the ledger interleaves
/// configs — either reads a config change as an engine regression. A row
/// with no `dispatch` field predates eu-hxu6 and is treated as "predecoded"
/// (that was the only bytecode path measured before this field existed).
fn group_by_lineage(
    text: &str,
) -> Result<std::collections::BTreeMap<LineageKey, Vec<serde_json::Value>>> {
    let mut per_lineage: std::collections::BTreeMap<LineageKey, Vec<serde_json::Value>> =
        Default::default();
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let v: serde_json::Value =
            serde_json::from_str(line).with_context(|| format!("parse ledger row: {line}"))?;
        if let Some(bench) = v.get("bench").and_then(|b| b.as_str()) {
            let config = v
                .get("prelude_config")
                .and_then(|c| c.as_str())
                .unwrap_or("unknown")
                .to_string();
            let dispatch = v
                .get("dispatch")
                .and_then(|d| d.as_str())
                .unwrap_or("predecoded")
                .to_string();
            per_lineage
                .entry((bench.to_string(), config, dispatch))
                .or_default()
                .push(v);
        }
    }
    Ok(per_lineage)
}

/// The --check verdict for one lineage: (last_ratio, prev_ratio [NaN if
/// none], delta, is_regression, status label).
struct LineageVerdict {
    class: String,
    last_ratio: f64,
    prev_ratio: f64,
    delta: f64,
    thresh: f64,
    is_regression: bool,
    status: &'static str,
}

/// Compute the regression/watch verdict for one lineage's row history.
/// Pure function of the rows (no I/O) so it is directly unit-testable.
fn evaluate_lineage(rows: &[serde_json::Value]) -> LineageVerdict {
    let last = rows.last().expect("lineage groups are never empty");
    let class = last
        .get("class")
        .and_then(|c| c.as_str())
        .unwrap_or("?")
        .to_string();
    let last_ratio = last.get("ratio").and_then(|r| r.as_f64()).unwrap_or(0.0);
    let thresh = class_threshold(&class);

    let (prev_ratio, delta) = if rows.len() >= 2 {
        let p = rows[rows.len() - 2]
            .get("ratio")
            .and_then(|r| r.as_f64())
            .unwrap_or(last_ratio);
        (p, (last_ratio - p) / p)
    } else {
        (f64::NAN, 0.0)
    };

    // Regression = ratio worsened by more than the noise band, WITHIN this
    // lineage only.
    let is_regression = rows.len() >= 2 && delta > REGRESSION_BAND;
    // Watch = out of the class band (informational; class E is a tripwire).
    let over_threshold = last_ratio > thresh;

    let status = if is_regression {
        "REGRESSED"
    } else if over_threshold && class == "E" {
        "WATCH (tripwire)"
    } else if over_threshold {
        "WATCH (over band)"
    } else {
        "ok"
    };

    LineageVerdict {
        class,
        last_ratio,
        prev_ratio,
        delta,
        thresh,
        is_regression,
        status,
    }
}

fn cmd_check(root: &Path) -> Result<()> {
    let path = root.join(LEDGER);
    let text =
        std::fs::read_to_string(&path).with_context(|| format!("read {}", path.display()))?;

    let per_lineage = group_by_lineage(&text)?;

    println!(
        "{:<26} {:>8} {:>10} {:>3} {:>8} {:>8} {:>8} {:>8}  status",
        "bench", "config", "dispatch", "cls", "prev", "last", "delta%", "thresh"
    );

    let mut regressed = false;
    for ((bench, config, dispatch), rows) in &per_lineage {
        let v = evaluate_lineage(rows);
        let (class, last_ratio, prev_ratio, delta, thresh, is_regression, status) = (
            v.class.as_str(),
            v.last_ratio,
            v.prev_ratio,
            v.delta,
            v.thresh,
            v.is_regression,
            v.status,
        );
        if is_regression {
            regressed = true;
        }

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
            "{:<26} {:>8} {:>10} {:>3} {} {:>8.3} {} {:>8.3}  {}",
            short_id(bench),
            config,
            dispatch,
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

// ── tests (eu-lhai / eu-hxu6) ────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn row(bench: &str, config: &str, ratio: f64, class: &str) -> String {
        serde_json::json!({
            "date": "2026-08-01", "commit": "deadbeef", "bench": bench, "class": class,
            "bc_wall_med": 1.0, "hs_wall_med": 1.0, "ratio": ratio, "hs_ticks": 1,
            "hs_allocs": 1, "gc": 0, "host": "test", "runs": 5,
            "prelude_config": config,
        })
        .to_string()
    }

    fn row_with_dispatch(bench: &str, config: &str, dispatch: &str, ratio: f64) -> String {
        serde_json::json!({
            "date": "2026-08-01", "commit": "deadbeef", "bench": bench, "class": "C",
            "bc_wall_med": 1.0, "hs_wall_med": 1.0, "ratio": ratio, "hs_ticks": 1,
            "hs_allocs": 1, "gc": 0, "host": "test", "runs": 5,
            "prelude_config": config, "dispatch": dispatch,
        })
        .to_string()
    }

    /// eu-lhai: a blob row sandwiched between two source rows must never be
    /// compared against the source lineage — each config gets its own
    /// two-row (prev, last) history, so mixing configs does NOT synthesise a
    /// spurious large delta.
    #[test]
    fn check_does_not_compare_across_prelude_config() {
        let text = [
            row("b", "source", 1.00, "C"), // source lineage, 1 row: no prev
            row("b", "blob", 0.80, "C"),   // blob lineage, row 1
            row("b", "source", 0.95, "C"), // source lineage, row 2: prev=1.00
            row("b", "blob", 0.82, "C"),   // blob lineage, row 2: prev=0.80
        ]
        .join("\n");

        let groups = group_by_lineage(&text).unwrap();
        assert_eq!(groups.len(), 2, "blob and source must be separate lineages");

        let blob = groups
            .get(&(
                "b".to_string(),
                "blob".to_string(),
                "predecoded".to_string(),
            ))
            .unwrap();
        let v = evaluate_lineage(blob);
        assert_eq!(blob.len(), 2);
        assert!((v.prev_ratio - 0.80).abs() < 1e-9);
        assert!((v.last_ratio - 0.82).abs() < 1e-9);
        assert!(
            !v.is_regression,
            "0.80 -> 0.82 is well within the noise band"
        );

        let source = groups
            .get(&(
                "b".to_string(),
                "source".to_string(),
                "predecoded".to_string(),
            ))
            .unwrap();
        assert_eq!(source.len(), 2);
        let v2 = evaluate_lineage(source);
        assert!((v2.prev_ratio - 1.00).abs() < 1e-9);
        assert!((v2.last_ratio - 0.95).abs() < 1e-9);
    }

    /// Before the fix, comparing the LAST row overall (source, 0.95) against
    /// the PREVIOUS row overall (blob, 0.80) would read a config swap as a
    /// +18.75% ratio worsening — inside REGRESSION_BAND here but easily
    /// exceeding it with realistic blob/source deltas (eu-lhai's actual
    /// false positives). Confirm that cross-config pairing is impossible by
    /// construction: no lineage's row list ever mixes configs.
    #[test]
    fn lineages_never_mix_config_or_dispatch() {
        let text = [
            row("x", "blob", 0.5, "C"),
            row("x", "source", 0.9, "C"),
            row("x", "blob", 0.6, "C"),
        ]
        .join("\n");
        let groups = group_by_lineage(&text).unwrap();
        for (_, rows) in groups {
            let configs: std::collections::HashSet<_> = rows
                .iter()
                .map(|r| r["prelude_config"].as_str().unwrap())
                .collect();
            assert_eq!(configs.len(), 1, "a lineage must be a single config");
        }
    }

    /// eu-hxu6: rows carrying an explicit `dispatch` are their own lineage,
    /// distinct from `predecoded`-defaulted legacy rows and from each other.
    #[test]
    fn check_separates_dispatch_lineages() {
        let text = [
            row("x", "blob", 0.90, "C"), // legacy row, no `dispatch` field
            row_with_dispatch("x", "blob", "predecoded", 0.88),
            row_with_dispatch("x", "blob", "byte", 1.40),
        ]
        .join("\n");
        let groups = group_by_lineage(&text).unwrap();
        assert_eq!(
            groups.len(),
            2,
            "legacy (implicit predecoded) and explicit predecoded rows share one lineage; byte is a separate lineage"
        );
        let predecoded = groups
            .get(&(
                "x".to_string(),
                "blob".to_string(),
                "predecoded".to_string(),
            ))
            .unwrap();
        assert_eq!(predecoded.len(), 2, "legacy row + explicit predecoded row");
        let byte = groups
            .get(&("x".to_string(), "blob".to_string(), "byte".to_string()))
            .unwrap();
        assert_eq!(byte.len(), 1);
    }

    /// A genuine same-lineage regression is still caught.
    #[test]
    fn check_flags_genuine_same_lineage_regression() {
        let text = [row("y", "blob", 0.80, "C"), row("y", "blob", 1.20, "C")].join("\n");
        let groups = group_by_lineage(&text).unwrap();
        let rows = groups
            .get(&(
                "y".to_string(),
                "blob".to_string(),
                "predecoded".to_string(),
            ))
            .unwrap();
        let v = evaluate_lineage(rows);
        assert!(v.is_regression, "0.80 -> 1.20 is a 50% worsening");
        assert_eq!(v.status, "REGRESSED");
    }

    #[test]
    fn class_e_over_threshold_is_watch_not_regression() {
        let text = [
            row("lookup", "blob", 11.0, "E"),
            row("lookup", "blob", 12.0, "E"),
        ]
        .join("\n");
        let groups = group_by_lineage(&text).unwrap();
        let rows = groups
            .get(&(
                "lookup".to_string(),
                "blob".to_string(),
                "predecoded".to_string(),
            ))
            .unwrap();
        let v = evaluate_lineage(rows);
        assert!(
            !v.is_regression,
            "11.0 -> 12.0 is a 9% delta, inside the noise band"
        );
        assert_eq!(v.status, "WATCH (tripwire)");
    }
}
