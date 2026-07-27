//! `cargo xtask diag-snapshot` — the golden diagnostic snapshot CLI
//! (design spec §5.3, eu-1tkk.7.4).
//!
//! Three jobs:
//!
//! * **`--bless`** — regenerate the checked-in goldens under
//!   `tests/diagnostics/snapshots/` after a deliberate diagnostics change.
//! * **`--capture`** — record the corpus with an arbitrary `eu` binary into a
//!   standalone bundle file. With `--from-ref` it builds that binary from any
//!   git ref first, which is how a baseline is taken *retroactively* from a
//!   release tag.
//! * **`--compare`** — tabulate two captures into a reviewable before/after
//!   report.
//!
//! # Why blessing goes through `prelude-compile`
//!
//! A golden records both the blob-prelude and the `--source-prelude`
//! rendering. Only a binary that embeds a verified prelude blob can produce
//! the former, and the well-known trap is that restoring a `lib/prelude.blob`
//! with an old mtime does not re-trigger `build.rs`, so `cfg(prelude_blob_ok)`
//! stays unset and the blob half is silently captured as a second source-mode
//! run. `--bless` therefore regenerates the blob itself immediately before
//! building, which makes that state unreachable on the authoring path.

use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{bail, Context, Result};

#[path = "../../tests/diagnostics/snapshot_engine.rs"]
mod engine;

use engine::{Delta, Mode};

pub fn run(args: &mut impl Iterator<Item = String>) -> Result<()> {
    let root = crate::workspace_root()?;
    let argv: Vec<String> = args.collect();
    let mut i = 0;
    let mut bless = false;
    let mut capture = false;
    let mut compare: Vec<PathBuf> = Vec::new();
    let mut from_ref: Option<String> = None;
    let mut binary: Option<PathBuf> = None;
    let mut label: Option<String> = None;
    let mut out: Option<PathBuf> = None;
    let mut mode = Mode::Source;
    // LOCAL MEASUREMENT-ONLY FLAG (integration verification, not for merge):
    // opt in to capturing the blob half with an externally-supplied binary.
    // `cmd_capture` refuses to do this by default because it cannot know
    // whether an arbitrary binary embeds a verified blob; the caller here has
    // just built it and run `prelude-compile`, so it can.
    let mut with_blob = false;

    while i < argv.len() {
        match argv[i].as_str() {
            "--bless" => bless = true,
            "--capture" => capture = true,
            "--compare" => {
                compare.push(next(&argv, &mut i, "--compare <before>")?);
                compare.push(next(&argv, &mut i, "--compare <before> <after>")?);
            }
            "--with-blob" => with_blob = true,
            "--from-ref" => from_ref = Some(next_str(&argv, &mut i, "--from-ref <git ref>")?),
            "--binary" => binary = Some(next(&argv, &mut i, "--binary <path>")?),
            "--label" => label = Some(next_str(&argv, &mut i, "--label <name>")?),
            "--out" => out = Some(next(&argv, &mut i, "--out <path>")?),
            "--mode" => {
                mode = match next_str(&argv, &mut i, "--mode blob|source")?.as_str() {
                    "blob" => Mode::Blob,
                    "source" => Mode::Source,
                    other => bail!("--mode must be `blob` or `source`, got `{other}`"),
                }
            }
            "--help" | "-h" => {
                usage();
                return Ok(());
            }
            other => bail!("unknown diag-snapshot argument: {other}"),
        }
        i += 1;
    }

    if !compare.is_empty() {
        return cmd_compare(&compare[0], &compare[1], mode, out.as_deref());
    }
    if bless {
        return cmd_bless(&root);
    }
    if capture || from_ref.is_some() || binary.is_some() {
        return cmd_capture(&root, from_ref, binary, label, out, with_blob);
    }
    usage();
    bail!("nothing to do — pass --bless, --capture or --compare");
}

fn usage() {
    eprintln!("Usage: cargo xtask diag-snapshot <mode>");
    eprintln!();
    eprintln!("  --bless");
    eprintln!("        Regenerate lib/prelude.blob, rebuild, and rewrite the checked-in");
    eprintln!("        goldens under tests/diagnostics/snapshots/ plus the divergence");
    eprintln!("        inventory. Review the resulting diff — that is the point of it.");
    eprintln!();
    eprintln!("  --capture [--from-ref <ref> | --binary <path>] [--label <name>] --out <file>");
    eprintln!("        Capture the corpus into a standalone bundle. --from-ref builds `eu`");
    eprintln!("        from any git ref first, so a baseline can be taken retroactively");
    eprintln!("        from a release tag.");
    eprintln!();
    eprintln!("  --compare <before> <after> [--mode blob|source] [--out <report.md>]");
    eprintln!("        Tabulate two captures (bundle file or snapshot directory) into a");
    eprintln!("        before/after report.");
}

fn next(argv: &[String], i: &mut usize, what: &str) -> Result<PathBuf> {
    Ok(PathBuf::from(next_str(argv, i, what)?))
}

fn next_str(argv: &[String], i: &mut usize, what: &str) -> Result<String> {
    *i += 1;
    argv.get(*i)
        .cloned()
        .with_context(|| format!("missing argument for {what}"))
}

// ── --bless ───────────────────────────────────────────────────────────────────

fn cmd_bless(root: &Path) -> Result<()> {
    eprintln!("[1/3] regenerating lib/prelude.blob so the blob half is captured for real");
    crate::cmd_prelude_compile()?;

    eprintln!("[2/3] rebuilding the test binary against the fresh blob");
    // `prelude-compile` has just rewritten lib/prelude.blob with a current
    // mtime, so build.rs re-runs and cfg(prelude_blob_ok) is set.
    run_cargo(root, &["build", "--tests"])?;

    eprintln!("[3/3] capturing the corpus");
    let status = Command::new(cargo())
        .current_dir(root)
        .args([
            "test",
            "--test",
            "diagnostics_snapshots",
            "--",
            "--test-threads",
            "1",
            "--nocapture",
        ])
        .env("EU_DIAG_SNAPSHOT_BLESS", "1")
        .status()
        .context("cargo test --test diagnostics_snapshots")?;
    if !status.success() {
        bail!("blessing failed: {status}");
    }
    eprintln!(
        "\nBlessed. `git diff {} tests/diagnostics/DIVERGENCE.md` is the review artefact.",
        engine::SNAPSHOT_DIR
    );
    Ok(())
}

// ── --capture ─────────────────────────────────────────────────────────────────

fn cmd_capture(
    root: &Path,
    from_ref: Option<String>,
    binary: Option<PathBuf>,
    label: Option<String>,
    out: Option<PathBuf>,
    with_blob: bool,
) -> Result<()> {
    let out = out.context("--capture requires --out <file>")?;

    // Building from a ref produces a binary whose *own* prelude is embedded at
    // its own compile time, so running it from this checkout measures that
    // ref's diagnostics against this checkout's corpus — exactly the
    // comparison a release claim needs.
    let (binary, label, _worktree) = match (from_ref, binary) {
        (Some(git_ref), None) => {
            let wt = build_ref(root, &git_ref)?;
            let bin = wt.join("target/release/eu");
            (bin, label.unwrap_or(git_ref), Some(wt))
        }
        (None, Some(bin)) => {
            let label = label.unwrap_or_else(|| bin.display().to_string());
            (bin, label, None)
        }
        (Some(_), Some(_)) => bail!("--from-ref and --binary are mutually exclusive"),
        (None, None) => bail!("--capture requires --from-ref <ref> or --binary <path>"),
    };

    if !binary.exists() {
        bail!("no such binary: {}", binary.display());
    }

    let fixtures = engine::discover(root);
    eprintln!(
        "capturing {} fixtures with {} (label: {label})",
        fixtures.len(),
        binary.display()
    );
    // An externally-supplied binary is captured source-prelude only: whether it
    // embeds a blob is not knowable from here, and recording a second
    // source-mode run as if it were the blob mode would be a lie in the
    // historical record.
    let snapshots = engine::capture(&binary, root, &fixtures, with_blob, |i, n, id| {
        if i == 1 || i == n || i % 25 == 0 {
            eprintln!("  [{i}/{n}] {id}");
        }
    });
    if let Some(parent) = out.parent() {
        std::fs::create_dir_all(parent).ok();
    }
    std::fs::write(&out, engine::bundle(&label, &snapshots))
        .with_context(|| format!("writing {}", out.display()))?;
    eprintln!("wrote {}", out.display());
    Ok(())
}

/// Build `eu` at `git_ref` in a worktree under `target/`, returning its path.
///
/// A separate worktree, never a checkout or a stash, of the current one:
/// `refs/stash` lives in the common git directory and is shared by every
/// worktree of the repository, so stashing to move between refs can restore
/// another worktree's work into the tree a baseline is captured from. A
/// contaminated baseline is worse than no baseline — it would make the
/// before/after comparison quietly wrong.
///
/// The worktree is kept between runs (it lives under `target/`, where build
/// caches belong) so a repeat capture does not pay for a full release build.
/// Remove it with `git worktree remove target/diag-baseline/<ref>` when done.
fn build_ref(root: &Path, git_ref: &str) -> Result<PathBuf> {
    let wt = root.join("target/diag-baseline").join(git_ref);
    if !wt.exists() {
        // `cargo clean` deletes anything under `target/`, including a worktree
        // created by an earlier run, while leaving its registration behind.
        // `git worktree add` then fails with "already exists". Prune first so
        // that a clean build tree is not a confusing hard error.
        let _ = Command::new("git")
            .current_dir(root)
            .args(["worktree", "prune"])
            .status();
        let status = Command::new("git")
            .current_dir(root)
            .args(["worktree", "add", "--detach"])
            .arg(&wt)
            .arg(git_ref)
            .status()
            .context("git worktree add")?;
        if !status.success() {
            bail!("could not create a worktree at {git_ref}");
        }
    }
    run_cargo(&wt, &["build", "--release"])?;
    Ok(wt)
}

fn cargo() -> String {
    std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string())
}

fn run_cargo(dir: &Path, args: &[&str]) -> Result<()> {
    let status = Command::new(cargo())
        .current_dir(dir)
        .args(args)
        .status()
        .with_context(|| format!("cargo {}", args.join(" ")))?;
    if !status.success() {
        bail!("cargo {} failed in {}", args.join(" "), dir.display());
    }
    Ok(())
}

// ── --compare ─────────────────────────────────────────────────────────────────

fn cmd_compare(before: &Path, after: &Path, mode: Mode, out: Option<&Path>) -> Result<()> {
    let b =
        engine::load_capture(before).with_context(|| format!("loading {}", before.display()))?;
    let a = engine::load_capture(after).with_context(|| format!("loading {}", after.display()))?;
    let report = render_report(before, after, mode, &b, &a);
    match out {
        Some(path) => {
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent).ok();
            }
            std::fs::write(path, &report).with_context(|| format!("writing {}", path.display()))?;
            eprintln!("wrote {}", path.display());
        }
        None => print!("{report}"),
    }
    Ok(())
}

/// Render the before/after report.
///
/// The report deliberately separates **what changed** (a text diff, which the
/// reviewer must read) from **objective fact deltas** (which can be
/// tabulated). It makes no claim that a change is an improvement: several
/// deltas across a release boundary are new *features* — an error code where
/// there was none because the structured model did not exist yet — not better
/// diagnostics. Only the invariant columns support a directional reading, and
/// even those are stated as counts, not verdicts.
fn render_report(
    before_path: &Path,
    after_path: &Path,
    mode: Mode,
    before: &std::collections::BTreeMap<String, String>,
    after: &std::collections::BTreeMap<String, String>,
) -> String {
    let deltas = engine::compare(before, after, mode);
    let mut s = String::new();
    s.push_str("# Diagnostic snapshot comparison\n\n");
    s.push_str(&format!("- **before**: `{}`\n", before_path.display()));
    s.push_str(&format!("- **after**: `{}`\n", after_path.display()));
    s.push_str(&format!("- **prelude mode compared**: {}\n", mode.label()));
    s.push_str(&format!(
        "- **fixtures**: {} before, {} after\n\n",
        before.len(),
        after.len()
    ));

    let changed: Vec<_> = deltas
        .iter()
        .filter_map(|(id, d)| match d {
            Delta::Changed { before, after } => Some((id, before, after)),
            _ => None,
        })
        .collect();
    let added: Vec<&String> = deltas
        .iter()
        .filter(|(_, d)| matches!(d, Delta::Added))
        .map(|(id, _)| id)
        .collect();
    let removed: Vec<&String> = deltas
        .iter()
        .filter(|(_, d)| matches!(d, Delta::Removed))
        .map(|(id, _)| id)
        .collect();
    let same = deltas.len() - changed.len() - added.len() - removed.len();

    s.push_str("## Summary\n\n");
    s.push_str("| outcome | fixtures |\n|---|---|\n");
    s.push_str(&format!("| unchanged | {same} |\n"));
    s.push_str(&format!("| changed | {} |\n", changed.len()));
    s.push_str(&format!("| only in *after* | {} |\n", added.len()));
    s.push_str(&format!("| only in *before* | {} |\n\n", removed.len()));

    // Aggregate the objective properties over the fixtures present in both, so
    // the totals are comparable.
    let agg = |pick: fn(&engine::Facts) -> usize| -> (usize, usize) {
        let mut b = 0;
        let mut a = 0;
        for (_, before, after) in &changed {
            b += pick(before);
            a += pick(after);
        }
        (b, a)
    };
    s.push_str("## Objective properties, over the changed fixtures only\n\n");
    s.push_str("| property | before | after |\n|---|---|---|\n");
    for (name, pick) in [
        (
            "errors rendered",
            (|f: &engine::Facts| f.errors) as fn(&engine::Facts) -> usize,
        ),
        ("warnings rendered", |f| f.warnings),
        ("help lines", |f| f.help_lines),
        ("note lines", |f| f.note_lines),
        ("secondary labels", |f| f.secondary_labels),
        ("trace frames", |f| f.trace_frames),
        ("trace frames in user files", |f| f.trace_user_frames),
        ("trace frames in the prelude", |f| f.trace_prelude_frames),
        ("prelude source lines excerpted", |f| {
            f.prelude_excerpt_lines
        }),
    ] {
        let (b, a) = agg(pick);
        s.push_str(&format!("| {name} | {b} | {a} |\n"));
    }
    let count = |sel: fn(&engine::Facts) -> bool| -> (usize, usize) {
        let mut b = 0;
        let mut a = 0;
        for (_, before, after) in &changed {
            if sel(before) {
                b += 1;
            }
            if sel(after) {
                a += 1;
            }
        }
        (b, a)
    };
    for (name, sel) in [
        (
            "fixtures with a stable error code",
            (|f: &engine::Facts| f.code.is_some()) as fn(&engine::Facts) -> bool,
        ),
        ("fixtures with a primary label in a user file", |f| {
            matches!(f.primary, engine::Primary::User(_))
        }),
        ("fixtures with a primary label in the prelude", |f| {
            matches!(f.primary, engine::Primary::Prelude)
        }),
        ("fixtures with no primary label at all", |f| {
            matches!(f.primary, engine::Primary::None)
        }),
        ("fixtures ending in a Rust panic", |f| f.rust_panic),
    ] {
        let (b, a) = count(sel);
        s.push_str(&format!("| {name} | {b} | {a} |\n"));
    }
    s.push('\n');
    s.push_str(
        "> These are counts, not verdicts. A change is not automatically an improvement:\n\
         > across a release boundary some deltas are new *features* (a stable error code\n\
         > where the structured diagnostic model did not exist yet) and some are new\n\
         > *fixtures* the older binary never saw. The per-fixture diffs below are the\n\
         > evidence; this table is only an index into them.\n\n",
    );

    if !changed.is_empty() {
        s.push_str("## Changed fixtures\n\n");
        s.push_str(
            "| fixture | primary before → after | code before → after | trace before → after |\n",
        );
        s.push_str("|---|---|---|---|\n");
        for (id, b, a) in &changed {
            s.push_str(&format!(
                "| `{id}` | {} → {} | {} → {} | {} → {} |\n",
                b.primary.render(),
                a.primary.render(),
                b.code.as_deref().unwrap_or("-"),
                a.code.as_deref().unwrap_or("-"),
                b.trace_frames,
                a.trace_frames,
            ));
        }
        s.push('\n');
    }
    if !added.is_empty() {
        s.push_str("## Only in *after*\n\n");
        for id in &added {
            s.push_str(&format!("- `{id}`\n"));
        }
        s.push('\n');
    }
    if !removed.is_empty() {
        s.push_str("## Only in *before*\n\n");
        for id in &removed {
            s.push_str(&format!("- `{id}`\n"));
        }
        s.push('\n');
    }

    s.push_str("## Full renderings, side by side\n\n");
    for (id, _, _) in &changed {
        let bs = engine::parse_sections(before.get(*id).map(String::as_str).unwrap_or(""));
        let as_ = engine::parse_sections(after.get(*id).map(String::as_str).unwrap_or(""));
        let bt = engine::section_for(&bs, mode)
            .map(|s| s.stderr.clone())
            .unwrap_or_default();
        let at = engine::section_for(&as_, mode)
            .map(|s| s.stderr.clone())
            .unwrap_or_default();
        s.push_str(&format!("### `{id}`\n\n**before**\n\n```text\n"));
        s.push_str(if bt.is_empty() { "<no output>\n" } else { &bt });
        s.push_str("```\n\n**after**\n\n```text\n");
        s.push_str(if at.is_empty() { "<no output>\n" } else { &at });
        s.push_str("```\n\n");
    }
    s
}
