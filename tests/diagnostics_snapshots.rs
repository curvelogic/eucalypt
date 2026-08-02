//! Golden diagnostic snapshots — the regression gate (design spec §5.3,
//! eu-1tkk.7.4).
//!
//! Runs every corpus fixture through the `eu` binary this test run built and
//! compares the full rendered stderr, plus a block of objectively-derived
//! facts, against the checked-in golden under `tests/diagnostics/snapshots/`.
//!
//! # Relationship to the other diagnostics gates
//!
//! * `tests/diagnostics_invariants.rs` asserts five *objective invariants*
//!   (primary in a user file, no panic, in-region, user-anchored trace, trace
//!   within budget). It can only ever say "not obviously broken".
//! * These snapshots lock the *whole presentation*. They cannot say whether a
//!   diagnostic is good — only whether it changed. Every change is therefore a
//!   review decision, which is the point: the diff is what a reviewer reads.
//! * `tests/harness/errors/*.expect` assert a regex against stderr. A snapshot
//!   of the same fixture is strictly more informative; the sidecars remain the
//!   thing that gates `eu test`.
//!
//! # Blessing
//!
//! ```text
//! cargo xtask diag-snapshot --bless
//! ```
//!
//! which regenerates the prelude blob first, so the blob half of every golden
//! is captured from a binary that really embeds one. See
//! `tests/diagnostics/SNAPSHOTS.md`.
//!
//! # Why the two halves are gated differently
//!
//! A golden records both prelude modes. The `--source-prelude` half is
//! reproducible in any build and is checked unconditionally. The blob half can
//! only be reproduced by a binary that embeds a verified prelude blob, so
//! [`blob_prelude_snapshots_match`] is `#[cfg(prelude_blob_ok)]` and runs in
//! the `diagnostics-blob-mode` CI job, which generates the blob first. That
//! job also asserts the test is present in the binary's test list, so the
//! well-known failure mode — a stale-mtime blob leaving `prelude_blob_ok`
//! unset, compiling the test out, and the job passing with zero tests — is
//! caught rather than reported as green.

#[path = "diagnostics/snapshot_engine.rs"]
mod engine;

use engine::{Mode, Snapshot};
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn eu_binary() -> PathBuf {
    // `EU_DIAG_SNAPSHOT_BIN` lets a capture run against an arbitrary binary —
    // an older release tag, say — which is how a retroactive baseline is taken.
    match std::env::var("EU_DIAG_SNAPSHOT_BIN") {
        Ok(p) if !p.is_empty() => PathBuf::from(p),
        _ => PathBuf::from(env!("CARGO_BIN_EXE_eu")),
    }
}

fn snapshot_dir() -> PathBuf {
    match std::env::var("EU_DIAG_SNAPSHOT_OUT") {
        Ok(p) if !p.is_empty() => PathBuf::from(p),
        _ => repo_root().join(engine::SNAPSHOT_DIR),
    }
}

fn blessing() -> bool {
    std::env::var("EU_DIAG_SNAPSHOT_BLESS").as_deref() == Ok("1")
}

/// Whether this test binary's `eu` embeds a verified prelude blob.
///
/// Note this describes `CARGO_BIN_EXE_eu`, so it is only meaningful when the
/// capture is running against that binary; an externally-supplied
/// `EU_DIAG_SNAPSHOT_BIN` is treated as source-only unless the caller opts in
/// with `EU_DIAG_SNAPSHOT_WITH_BLOB=1`.
const OWN_BINARY_HAS_BLOB: bool = cfg!(prelude_blob_ok);

fn capture_blob_mode() -> bool {
    match std::env::var("EU_DIAG_SNAPSHOT_WITH_BLOB").as_deref() {
        Ok("1") => true,
        Ok("0") => false,
        _ => OWN_BINARY_HAS_BLOB && std::env::var("EU_DIAG_SNAPSHOT_BIN").is_err(),
    }
}

/// The corpus sweep is the expensive part (two `eu` processes per fixture), and
/// every test in this file wants the same one. Run it once per test binary and
/// share it: `--test-threads` is then free to schedule the tests however it
/// likes without multiplying the cost.
static CAPTURE: std::sync::OnceLock<Vec<Snapshot>> = std::sync::OnceLock::new();

fn capture_all() -> &'static [Snapshot] {
    CAPTURE.get_or_init(|| {
        let root = repo_root();
        let fixtures = engine::discover(&root);
        assert!(
            fixtures.len() > 150,
            "corpus discovery found only {} fixtures — expected the full \
             tests/harness/errors + provocation + many-declaration corpus. Is the \
             working directory right?",
            fixtures.len()
        );
        eprintln!(
            "capturing {} fixtures ({}) with {}",
            fixtures.len(),
            if capture_blob_mode() {
                "blob + source prelude"
            } else {
                "source prelude only"
            },
            eu_binary().display()
        );
        engine::capture(
            &eu_binary(),
            &root,
            &fixtures,
            capture_blob_mode(),
            |i, n, id| {
                if i == 1 || i == n || i % 50 == 0 {
                    eprintln!("  [{i}/{n}] {id}");
                }
            },
        )
    })
}

/// Windows is out of scope for this gate.
///
/// The goldens embed repo-relative fixture paths and verbatim source excerpts,
/// and neither Windows path rendering nor a CRLF checkout of the corpus has
/// been verified against them. Rather than let an unverified platform decide
/// whether master is red, the gate announces itself as skipped there and runs
/// on Linux and macOS, which is where diagnostics rendering is exercised.
/// Tracked by eu-1tkk.7.22.
fn skip_on_windows(what: &str) -> bool {
    if cfg!(windows) {
        eprintln!("SKIPPED on Windows: {what} (see eu-1tkk.7.22)");
        return true;
    }
    false
}

/// Compare one mode's half of every golden against a fresh capture, or rewrite
/// the goldens when blessing.
fn check_mode(mode: Mode) {
    if skip_on_windows("diagnostic snapshot gate") {
        return;
    }
    let dir = snapshot_dir();
    let snapshots = capture_all();

    if blessing() {
        std::fs::create_dir_all(&dir).expect("create snapshot dir");
        let written = engine::write_capture(&dir, snapshots).expect("write snapshots");
        let pruned = engine::prune_orphans(&dir, snapshots).expect("prune orphans");
        std::fs::write(
            repo_root().join(engine::DIVERGENCE_DOC),
            engine::render_divergence_doc(snapshots),
        )
        .expect("write divergence inventory");
        eprintln!(
            "blessed {} snapshot(s), pruned {} orphan(s)",
            written.len(),
            pruned.len()
        );
        return;
    }

    let mut missing = Vec::new();
    let mut mismatched = Vec::new();
    for snap in snapshots {
        let path = engine::snapshot_path(&dir, &snap.id);
        let Ok(doc) = std::fs::read_to_string(&path) else {
            missing.push(snap.id.clone());
            continue;
        };
        let golden = engine::parse_sections(&doc);
        let actual = engine::parse_sections(&snap.render());
        let (Some(want), Some(got)) = (
            engine::section_for(&golden, mode),
            engine::section_for(&actual, mode),
        ) else {
            mismatched.push(format!(
                "{}: no `{}` section (golden has {:?}, capture has {:?})",
                snap.id,
                mode.label(),
                golden.keys().collect::<Vec<_>>(),
                actual.keys().collect::<Vec<_>>()
            ));
            continue;
        };
        if want != got {
            mismatched.push(render_mismatch(&snap.id, &path, mode, want, got));
        }
    }

    let mut problems = Vec::new();
    if !missing.is_empty() {
        problems.push(format!(
            "{} fixture(s) have no golden snapshot:\n  {}",
            missing.len(),
            missing.join("\n  ")
        ));
    }
    problems.extend(mismatched);
    assert!(
        problems.is_empty(),
        "\n{}\n\n\
         {} fixture(s) differ from their golden snapshot.\n\
         If the change is intended, review each diff above and then run:\n\
         \n    cargo xtask diag-snapshot --bless\n\n\
         The diff — not the fact that a snapshot changed — is the thing to review.\n",
        problems.join("\n\n"),
        problems.len()
    );
}

fn render_mismatch(
    id: &str,
    path: &Path,
    mode: Mode,
    want: &engine::Section,
    got: &engine::Section,
) -> String {
    let mut s = format!("── {id} ({}) ──\n{}\n", mode.label(), path.display());
    if want.exit != got.exit {
        s.push_str(&format!(
            "  exit: golden {} → actual {}\n",
            want.exit, got.exit
        ));
    }
    if want.facts != got.facts {
        s.push_str(&format!(
            "  facts: golden {:?}\n         actual {:?}\n",
            want.facts, got.facts
        ));
    }
    if want.stderr != got.stderr {
        s.push_str("  stderr:\n");
        for line in unified_ish(&want.stderr, &got.stderr) {
            s.push_str(&format!("    {line}\n"));
        }
    }
    s
}

/// Minimal line-level diff, adequate for the short diagnostics in this corpus
/// and free of any dependency the shared engine could not also use.
fn unified_ish(before: &str, after: &str) -> Vec<String> {
    let b: Vec<&str> = before.lines().collect();
    let a: Vec<&str> = after.lines().collect();
    let mut out = Vec::new();
    let common_prefix = b.iter().zip(&a).take_while(|(x, y)| x == y).count();
    let common_suffix = b[common_prefix..]
        .iter()
        .rev()
        .zip(a[common_prefix..].iter().rev())
        .take_while(|(x, y)| x == y)
        .count();
    for line in &b[common_prefix..b.len() - common_suffix] {
        out.push(format!("- {line}"));
    }
    for line in &a[common_prefix..a.len() - common_suffix] {
        out.push(format!("+ {line}"));
    }
    if out.is_empty() {
        out.push("<differs only in trailing whitespace>".to_string());
    }
    out
}

/// The always-on gate: the `--source-prelude` half of every golden.
///
/// Reproducible in any build, blob or not, so this runs in the main test
/// suite and is the snapshot regression gate proper.
#[test]
fn source_prelude_snapshots_match() {
    check_mode(Mode::Source);
}

/// The blob half of every golden, plus the `prelude-modes:` classification and
/// the checked-in divergence inventory.
///
/// Only a binary embedding a verified prelude blob can reproduce this, so it
/// is gated. See the module docs for how the CI job proves it actually ran.
#[cfg(prelude_blob_ok)]
#[test]
fn blob_prelude_snapshots_match() {
    check_mode(Mode::Blob);
}

/// The divergence inventory must describe the divergence that actually exists.
///
/// A fixture that starts rendering differently under the blob than under
/// `--source-prelude` is a bug of the eu-7x0r / eu-9wq0s family; this asserts
/// nobody can introduce one without the inventory changing in the same commit.
#[cfg(prelude_blob_ok)]
#[test]
fn divergence_inventory_is_current() {
    if blessing() || skip_on_windows("blob/source divergence inventory") {
        return;
    }
    let snapshots = capture_all();
    let expected = engine::render_divergence_doc(snapshots);
    let path = repo_root().join(engine::DIVERGENCE_DOC);
    let actual = std::fs::read_to_string(&path).unwrap_or_default();
    assert_eq!(
        actual,
        expected,
        "\n{} is out of date.\n\
         A fixture's blob-prelude and source-prelude diagnostics now differ (or stopped \n\
         differing). Both are of interest: a new row is a regression of the eu-7x0r class, \n\
         a removed row is a fix worth recording. Re-run `cargo xtask diag-snapshot --bless`.\n",
        path.display()
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Normalisation self-tests
//
// The normaliser is the part of this harness that can silently destroy signal:
// suppress too much and a real regression stops showing up in the diff. These
// pin each rule against synthetic input, including the prelude-source
// suppression path, which today's corpus does not exercise (no diagnostic
// currently excerpts prelude source — the point is to notice if one starts).
// ─────────────────────────────────────────────────────────────────────────────

mod normalisation {
    use super::engine::{facts, normalise, Primary};
    use std::path::Path;

    fn norm(s: &str) -> String {
        normalise(s, Path::new("/tmp/some/checkout"))
    }

    #[test]
    fn strips_ansi_colour() {
        let raw = "\u{1b}[0m\u{1b}[1m\u{1b}[38;5;9merror\u{1b}[0m: boom\n";
        assert_eq!(norm(raw), "error: boom\n");
    }

    #[test]
    fn suppresses_prelude_line_numbers_but_keeps_the_combinator_name() {
        let raw = "  = while evaluating (outermost first):\n    - in 'nth' at [prelude]:1391:3\n";
        let out = norm(raw);
        assert!(
            out.contains("in 'nth' at [prelude]"),
            "combinator name must survive: {out}"
        );
        assert!(
            !out.contains("1391"),
            "prelude line number must not survive: {out}"
        );
    }

    #[test]
    fn rewrites_a_real_prelude_path_to_the_marker() {
        let raw = "  ┌─ /home/ci/eucalypt/lib/prelude.eu:1391:3\n";
        assert_eq!(norm(raw), "  ┌─ [prelude]\n");
    }

    #[test]
    fn replaces_the_checkout_path() {
        let raw = "error: could not read /tmp/some/checkout/foo.eu\n";
        assert_eq!(norm(raw), "error: could not read <root>/foo.eu\n");
    }

    #[test]
    fn drops_thread_ids_and_panic_line_numbers() {
        let raw = "thread '<unnamed>' (20493446) panicked at src/export/yaml.rs:46:29:\n";
        assert_eq!(
            norm(raw),
            "thread '<unnamed>' panicked at src/export/yaml.rs:LINE:COL:\n"
        );
    }

    /// The rustc `ignore-directory-in-diagnostics-source-blocks` analogue: a
    /// codespan block anchored in the prelude keeps its header and loses its
    /// body, and the number of lines lost is retained as a fact.
    #[test]
    fn suppresses_prelude_source_excerpts_but_counts_them() {
        let raw = concat!(
            "error: boom\n",
            "  ┌─ [prelude]:1391:3\n",
            "  │\n",
            "1391 │ nth(n, l): l drop(n) head\n",
            "  │            ^^^^\n",
            "  │\n",
            "  = while evaluating (outermost first):\n",
        );
        let out = norm(raw);
        assert!(
            !out.contains("nth(n, l)"),
            "prelude source text must not reach the snapshot: {out}"
        );
        assert!(
            out.contains("<4 prelude source line(s) suppressed>"),
            "the suppressed count must be visible: {out}"
        );
        assert_eq!(facts(&out).prelude_excerpt_lines, 4);
        assert_eq!(facts(&out).primary, Primary::Prelude);
    }

    /// User source excerpts are the substance of a diagnostic and must survive
    /// untouched — a normaliser that ate them would make the goldens useless.
    #[test]
    fn keeps_user_source_excerpts_verbatim() {
        let raw = concat!(
            "error: boom\n",
            "  ┌─ tests/harness/errors/030.eu:2:6\n",
            "  │\n",
            "2 │ x: 1 + \"hello\"\n",
            "  │      ^\n",
        );
        assert_eq!(norm(raw), raw);
    }

    #[test]
    fn is_idempotent() {
        let raw = concat!(
            "\u{1b}[31merror[EU-EVAL-TYPE]\u{1b}[0m: nope\n",
            "  ┌─ a.eu:1:1\n",
            "  = while evaluating (outermost first):\n",
            "    - result at a.eu:1:1\n",
            "    - in 'nth' (prelude)\n",
        );
        let once = norm(raw);
        assert_eq!(norm(&once), once);
    }

    #[test]
    fn extracts_facts_from_a_human_rendering() {
        let raw = concat!(
            "error[EU-EVAL-TYPE]: type mismatch: expected number, found string \"hello\"\n",
            "  help: use 'num'\n",
            "  ┌─ tests/harness/errors/030.eu:2:6\n",
            "  │\n",
            "2 │ x: 1 + \"hello\"\n",
            "  │      ^\n",
            "  │\n",
            "  = to convert a string to a number, use 'num'\n",
            "  = while evaluating (outermost first):\n",
            "    - x at 030.eu:2:6\n",
            "    - in 'nth' (prelude)\n",
        );
        let f = facts(&norm(raw));
        assert_eq!(f.errors, 1);
        assert_eq!(f.warnings, 0);
        assert_eq!(f.code.as_deref(), Some("EU-EVAL-TYPE"));
        assert_eq!(
            f.primary,
            Primary::User("tests/harness/errors/030.eu:2:6".to_string())
        );
        assert_eq!(f.help_lines, 1);
        assert_eq!(f.note_lines, 1);
        assert_eq!(f.trace_frames, 2);
        assert_eq!(f.trace_user_frames, 1);
        assert_eq!(f.trace_prelude_frames, 1);
        assert!(!f.rust_panic);
    }

    /// A bracketed *message* is not an error code.
    ///
    /// `error: array shape mismatch: shape [2, 2] requires 4 elements` has no
    /// stable code; reading the first `[...]` on the line reported `2, 2` as
    /// one, which would have shown up in a release comparison as a fixture
    /// gaining and keeping a nonsense code.
    #[test]
    fn a_bracketed_message_is_not_mistaken_for_an_error_code() {
        let raw =
            "error: array shape mismatch: shape [2, 2] requires 4 elements but 3 were provided\n";
        let f = facts(&norm(raw));
        assert_eq!(f.code, None);
        assert_eq!(f.errors, 1);

        let coded = "error[EU-EVAL-TYPE]: type mismatch: expected number, found list [1, 2]\n";
        assert_eq!(facts(&norm(coded)).code.as_deref(), Some("EU-EVAL-TYPE"));
    }

    /// Only an anchored severity word opens a diagnostic: a message body that
    /// mentions "warning" or "error" must not inflate the counts.
    #[test]
    fn only_anchored_severity_words_count_as_diagnostics() {
        let raw = concat!(
            "error: something failed\n",
            "  = the error: reported above is advisory\n",
            "warning: heads up\n",
        );
        let f = facts(&norm(raw));
        assert_eq!(f.errors, 1);
        assert_eq!(f.warnings, 1);
    }

    #[test]
    fn a_rust_panic_is_recorded_as_such() {
        let raw = concat!(
            "thread '<unnamed>' (204) panicked at src/export/yaml.rs:46:29:\n",
            "number 9999999999999999999 is too large to represent as a YAML integer (max i64)\n",
        );
        assert!(facts(&norm(raw)).rust_panic);
    }
}

/// Round-tripping a rendered snapshot back through the parser must be lossless
/// for every field the gate compares — otherwise a real difference could be
/// dropped on the floor and the gate would pass while output changed.
#[test]
fn snapshot_documents_round_trip() {
    use engine::{Facts, Primary, Section};
    let section = Section {
        exit: "1".to_string(),
        facts: Facts {
            errors: 1,
            warnings: 2,
            code: Some("EU-EVAL-TYPE".to_string()),
            primary: Primary::User("a.eu:1:2".to_string()),
            help_lines: 3,
            note_lines: 4,
            secondary_labels: 5,
            trace_frames: 6,
            trace_user_frames: 7,
            trace_prelude_frames: 8,
            prelude_excerpt_lines: 9,
            rust_panic: true,
        },
        stderr: "error: boom\n  and more\n".to_string(),
    };
    let snap = Snapshot {
        id: "errors/x".to_string(),
        argv: "run a.eu".to_string(),
        blob: Some(section.clone()),
        source: Section {
            exit: "2".to_string(),
            ..section.clone()
        },
    };
    let doc = snap.render();
    assert_eq!(
        engine::parse_prelude_modes(&doc).as_deref(),
        Some("divergent")
    );
    let parsed = engine::parse_sections(&doc);
    assert_eq!(parsed.get("blob prelude"), Some(&section));
    assert_eq!(
        parsed.get("--source-prelude").map(|s| s.exit.as_str()),
        Some("2")
    );

    // An empty rendering must survive the `<no output>` placeholder.
    let quiet = Snapshot {
        id: "errors/quiet".to_string(),
        argv: "run q.eu".to_string(),
        blob: None,
        source: Section {
            exit: "0".to_string(),
            facts: Facts::default(),
            stderr: String::new(),
        },
    };
    let doc = quiet.render();
    assert_eq!(
        engine::parse_prelude_modes(&doc).as_deref(),
        Some("source-only")
    );
    assert_eq!(
        engine::parse_sections(&doc)
            .get("--source-prelude")
            .map(|s| s.stderr.as_str()),
        Some("")
    );
}

/// A bundle must split back into exactly the documents it was built from —
/// the baseline captures are stored as bundles, so a lossy round trip would
/// corrupt the historical record.
#[test]
fn bundles_round_trip() {
    use engine::{Facts, Section};
    let make = |id: &str, exit: &str| Snapshot {
        id: id.to_string(),
        argv: format!("run {id}.eu"),
        blob: None,
        source: Section {
            exit: exit.to_string(),
            facts: Facts::default(),
            stderr: format!("error: {id}\n"),
        },
    };
    let snaps = vec![make("errors/a", "1"), make("many-decls/b", "2")];
    let text = engine::bundle("test", &snaps);
    let split = engine::split_bundle(&text);
    assert_eq!(split.len(), 2);
    for snap in &snaps {
        let doc = split.get(&snap.id).unwrap_or_else(|| panic!("{}", snap.id));
        let want = engine::parse_sections(&snap.render());
        assert_eq!(engine::parse_sections(doc), want, "{}", snap.id);
    }
}
