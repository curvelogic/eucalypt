//! Golden-snapshot engine for eucalypt diagnostics (design spec §5.3, eu-1tkk.7.4).
//!
//! This module is `#[path]`-included by BOTH
//! [`tests/diagnostics_snapshots.rs`] (the CI gate) and
//! [`xtask/src/diag_snapshot.rs`] (the `--bless` / capture / compare CLI), so
//! that a snapshot written by one is byte-identical to a snapshot checked by
//! the other. Consequently it **must depend on nothing but `std`** — the two
//! crates do not share a dependency set.
//!
//! # What a snapshot is
//!
//! One `.snap` file per corpus fixture, holding the *full rendered stderr* of
//! `eu` for that fixture plus a small block of objectively-derived facts
//! (exit code, error code, whether the primary label is in a user file, how
//! many trace frames and how many of those are the user's). It is strictly
//! more informative than the `.expect` regex sidecars in
//! `tests/harness/errors/`, which assert only that some substring appears.
//!
//! # Why both prelude modes
//!
//! Every fixture is run twice: once in the binary's default mode (blob, when
//! the binary embeds a pre-compiled prelude) and once under
//! `--source-prelude`. Divergence between the two has repeatedly hidden real
//! bugs (eu-7x0r, eu-9wq0s), so the snapshot records both renderings side by
//! side rather than averaging them away: a fixture whose two modes agree gets
//! one `=== both prelude modes ===` section, and a fixture whose modes
//! disagree gets two clearly-labelled sections and a `prelude-modes:
//! divergent` header line.
//!
//! # Normalisation ("prelude-source suppression")
//!
//! Snapshots are only useful if a diff means something changed in the
//! *diagnostic*. [`normalise`] removes everything else: ANSI colour,
//! absolute paths, thread ids, `rustc` panic line/columns, and — the analogue
//! of rustc's `ignore-directory-in-diagnostics-source-blocks` — prelude
//! line/column numbers and prelude source excerpts, which churn on every edit
//! to `lib/prelude.eu` without saying anything about diagnostic quality. The
//! *count* of suppressed prelude excerpt lines is kept as a fact, so
//! re-introducing prelude source into a diagnostic is still visible as a
//! change; only the churny text itself is dropped.

#![allow(dead_code)] // each includer uses a different subset

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

/// Bumped whenever the `.snap` file layout changes, so a stale golden fails
/// loudly with a format-version mismatch rather than a confusing text diff.
pub const FORMAT_VERSION: &str = "1";

/// Wall-clock ceiling for a single fixture run. Generous: the largest
/// many-declaration fixture compiles ~2000 bindings under the source prelude.
pub const FIXTURE_TIMEOUT: Duration = Duration::from_secs(120);

/// Managed-heap ceiling passed to every `eu` invocation, per the repository's
/// process-safety rule.
pub const HEAP_LIMIT_MIB: &str = "2048";

/// Corpus roots, as (directory relative to the repo root, snapshot subdirectory).
///
/// * `tests/harness/errors` — the 179-fixture error corpus. Its `.expect`
///   sidecars assert regexes; the snapshots capture what the user actually sees.
/// * `tests/diagnostics/corpus` — the provocation corpus behind the objective
///   invariant gate (eu-1tkk.7.2).
/// * `tests/diagnostics/snapshot-corpus` — many-declaration fixtures written
///   for this harness; see that directory's README for why they exist.
pub const CORPUS_ROOTS: &[(&str, &str)] = &[
    ("tests/harness/errors", "errors"),
    ("tests/diagnostics/corpus", "provocations"),
    ("tests/diagnostics/snapshot-corpus", "many-decls"),
];

/// Where the checked-in goldens live, relative to the repo root.
pub const SNAPSHOT_DIR: &str = "tests/diagnostics/snapshots";

/// Per-fixture extra-argument manifest, relative to the repo root.
pub const ARGS_MANIFEST: &str = "tests/diagnostics/snapshot-args.txt";

/// The divergence inventory, relative to the repo root.
pub const DIVERGENCE_DOC: &str = "tests/diagnostics/DIVERGENCE.md";

/// Names under `lib/` whose source is suppressed from snapshots.
const PRELUDE_SOURCES: &[&str] = &[
    "prelude.eu",
    "test.eu",
    "lens.eu",
    "state.eu",
    "reflect.eu",
    "markup.eu",
];

// ─────────────────────────────────────────────────────────────────────────────
// Corpus
// ─────────────────────────────────────────────────────────────────────────────

/// One corpus entry: a fixture and the exact argument vector used to run it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fixture {
    /// Stable identifier, e.g. `errors/030_type_mismatch_str`. Doubles as the
    /// snapshot path (`<SNAPSHOT_DIR>/<id>.snap`).
    pub id: String,
    /// Fixture path relative to the repo root.
    pub path: String,
    /// Arguments after the `eu` program name, excluding the prelude-mode flag.
    pub args: Vec<String>,
}

/// Load the per-fixture extra-argument manifest.
///
/// Format: blank lines and `#` comments are ignored; every other line is
/// `<fixture id> = <extra args>`. Only a handful of fixtures need this — the
/// ones whose diagnostic is only reachable with a `-t`/`-x`/`-L` argument the
/// plain harness cannot express.
pub fn load_arg_overrides(repo_root: &Path) -> BTreeMap<String, Vec<String>> {
    let mut out = BTreeMap::new();
    let Ok(text) = std::fs::read_to_string(repo_root.join(ARGS_MANIFEST)) else {
        return out;
    };
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let Some((id, args)) = line.split_once('=') else {
            continue;
        };
        out.insert(
            id.trim().to_string(),
            args.split_whitespace().map(str::to_string).collect(),
        );
    }
    out
}

/// Discover every corpus fixture, in a stable (sorted) order.
pub fn discover(repo_root: &Path) -> Vec<Fixture> {
    let overrides = load_arg_overrides(repo_root);
    let mut fixtures = Vec::new();
    for (dir, label) in CORPUS_ROOTS {
        let abs = repo_root.join(dir);
        let Ok(entries) = std::fs::read_dir(&abs) else {
            continue;
        };
        let mut stems: Vec<String> = entries
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("eu"))
            .filter_map(|p| p.file_stem().and_then(|s| s.to_str()).map(str::to_string))
            .collect();
        stems.sort();
        for stem in stems {
            let id = format!("{label}/{stem}");
            let path = format!("{dir}/{stem}.eu");
            // Baseline invocation: `run -L <corpus dir> --heap-limit-mib N <file>`.
            // `-L` mirrors what `tests/harness_test.rs` does for error tests so
            // that fixtures importing a sibling file resolve it.
            let mut args = vec![
                "run".to_string(),
                "-L".to_string(),
                (*dir).to_string(),
                "--heap-limit-mib".to_string(),
                HEAP_LIMIT_MIB.to_string(),
                path.clone(),
            ];
            args.extend(overrides.get(&id).cloned().unwrap_or_default());
            fixtures.push(Fixture { id, path, args });
        }
    }
    fixtures
}

// ─────────────────────────────────────────────────────────────────────────────
// Running
// ─────────────────────────────────────────────────────────────────────────────

/// Which prelude the run used.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    /// The binary's default — the pre-compiled blob when it embeds one, which
    /// is what a released binary does.
    Blob,
    /// Forced `--source-prelude`, available in every build.
    Source,
}

impl Mode {
    pub fn flag(self) -> Option<&'static str> {
        match self {
            Mode::Blob => None,
            Mode::Source => Some("--source-prelude"),
        }
    }

    pub fn label(self) -> &'static str {
        match self {
            Mode::Blob => "blob prelude",
            Mode::Source => "--source-prelude",
        }
    }
}

/// The outcome of one fixture run, already normalised.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Run {
    /// Process exit code, or `None` when killed by a signal or the timeout.
    pub exit: Option<i32>,
    /// True when the run hit [`FIXTURE_TIMEOUT`].
    pub timed_out: bool,
    /// Normalised stderr.
    pub stderr: String,
}

/// Run one fixture with `binary` from `cwd`, in `mode`, and normalise the result.
///
/// `stdout` is discarded: a diagnostic snapshot is about what the user is
/// *told*, and several fixtures emit hundreds of lines of successful YAML
/// before failing. Discarding it also leaves a single pipe to drain, so the
/// timeout below cannot deadlock on a full pipe buffer.
pub fn run_fixture(binary: &Path, cwd: &Path, fixture: &Fixture, mode: Mode) -> Run {
    let mut cmd = Command::new(binary);
    cmd.current_dir(cwd);
    // Force deterministic, colourless rendering. `NO_COLOR` is honoured by
    // termcolor's `ColorChoice::Auto`, which is what every eu renderer uses;
    // `TERM=dumb` is belt and braces for any renderer that sniffs the terminal.
    cmd.env("NO_COLOR", "1");
    cmd.env("TERM", "dumb");
    // A stray EU_* debug variable in the environment would silently change the
    // rendering, so clear the ones that do.
    for var in [
        "EU_ERROR_TRACE_DUMP",
        "EU_STACK_DIAG",
        "EU_IO_TRACE",
        "EU_GC_VERIFY",
        "EU_GC_POISON",
        "EU_GC_STRESS",
    ] {
        cmd.env_remove(var);
    }
    cmd.arg("run");
    if let Some(flag) = mode.flag() {
        cmd.arg(flag);
    }
    // `args[0]` is the `run` subcommand, already emitted above.
    cmd.args(&fixture.args[1..]);
    cmd.stdin(Stdio::null());
    cmd.stdout(Stdio::null());
    cmd.stderr(Stdio::piped());

    let mut child = match cmd.spawn() {
        Ok(c) => c,
        Err(e) => {
            return Run {
                exit: None,
                timed_out: false,
                stderr: format!("<failed to spawn {}: {e}>\n", binary.display()),
            }
        }
    };

    let mut pipe = child.stderr.take().expect("stderr piped");
    let reader = std::thread::spawn(move || {
        use std::io::Read;
        let mut buf = Vec::new();
        let _ = pipe.read_to_end(&mut buf);
        buf
    });

    let deadline = Instant::now() + FIXTURE_TIMEOUT;
    let mut timed_out = false;
    let status = loop {
        match child.try_wait() {
            Ok(Some(status)) => break Some(status),
            Ok(None) => {
                if Instant::now() >= deadline {
                    let _ = child.kill();
                    let _ = child.wait();
                    timed_out = true;
                    break None;
                }
                std::thread::sleep(Duration::from_millis(10));
            }
            Err(_) => break None,
        }
    };

    let raw = String::from_utf8_lossy(&reader.join().unwrap_or_default()).into_owned();
    Run {
        exit: status.and_then(|s| s.code()),
        timed_out,
        stderr: normalise(&raw, cwd),
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Normalisation
// ─────────────────────────────────────────────────────────────────────────────

/// Strip ANSI CSI/OSC escape sequences.
fn strip_ansi(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '\u{1b}' {
            out.push(c);
            continue;
        }
        match chars.peek() {
            // CSI: ESC [ params... final-byte in @..~
            Some('[') => {
                chars.next();
                for c in chars.by_ref() {
                    if ('\u{40}'..='\u{7e}').contains(&c) {
                        break;
                    }
                }
            }
            // OSC: ESC ] ... BEL or ESC \
            Some(']') => {
                chars.next();
                while let Some(c) = chars.next() {
                    if c == '\u{7}' {
                        break;
                    }
                    if c == '\u{1b}' && chars.peek() == Some(&'\\') {
                        chars.next();
                        break;
                    }
                }
            }
            // Any other two-character escape.
            Some(_) => {
                chars.next();
            }
            None => {}
        }
    }
    out
}

/// True when `s` at `i` begins `:<digits>:<digits>`; returns the end offset.
fn line_col_suffix(s: &str, i: usize) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut j = i;
    for _ in 0..2 {
        if bytes.get(j) != Some(&b':') {
            return None;
        }
        j += 1;
        let start = j;
        while bytes.get(j).is_some_and(u8::is_ascii_digit) {
            j += 1;
        }
        if j == start {
            return None;
        }
    }
    Some(j)
}

/// Replace `[prelude]:LINE:COL` with `[prelude]`.
///
/// Prelude line numbers move on every edit to `lib/prelude.eu`, which would
/// churn snapshots for a change that says nothing about diagnostic quality.
/// The combinator *name* (`in 'nth' at ...`) is the part that carries meaning
/// and is retained.
fn suppress_prelude_positions(s: &str) -> String {
    const MARK: &str = "[prelude]";
    let mut out = String::with_capacity(s.len());
    let mut i = 0;
    while let Some(rel) = s[i..].find(MARK) {
        let at = i + rel;
        out.push_str(&s[i..at]);
        out.push_str(MARK);
        i = at + MARK.len();
        if let Some(end) = line_col_suffix(s, i) {
            i = end;
        }
    }
    out.push_str(&s[i..]);
    out
}

/// Rewrite any reference to a `lib/` prelude-family source file — with or
/// without a `:line:col` suffix — to the bare `[prelude]` marker.
///
/// The runtime `SourceMap` already labels prelude locations `[prelude]`, so on
/// today's output this rewrites nothing. It is retained because a real
/// filesystem path reaching a diagnostic (via `-L`, a custom lib path, or a
/// future loader change) would otherwise embed a machine-specific absolute
/// path in a golden file.
fn suppress_prelude_paths(s: &str) -> String {
    let mut out = s.to_string();
    for name in PRELUDE_SOURCES {
        let needle = format!("lib/{name}");
        while let Some(at) = out.find(&needle) {
            // Walk back over the directory prefix of the path.
            let mut start = at;
            let bytes = out.as_bytes();
            while start > 0 {
                let c = bytes[start - 1];
                if c == b' ' || c == b'\t' || c == b'\n' || c == b'\'' || c == b'"' {
                    break;
                }
                start -= 1;
            }
            let mut end = at + needle.len();
            if let Some(e) = line_col_suffix(&out, end) {
                end = e;
            }
            out.replace_range(start..end, "[prelude]");
        }
    }
    out
}

/// Drop the thread id from a Rust panic banner and generalise the panic's own
/// source position.
///
/// `thread '<unnamed>' (20493446) panicked at src/export/yaml.rs:46:29:`
/// becomes
/// `thread '<unnamed>' panicked at src/export/yaml.rs:LINE:COL:`
///
/// The thread id differs on every run; the `.rs` line moves whenever that Rust
/// file is edited. Which *file* panicked is the signal and is kept.
fn normalise_rust_panic(line: &str) -> String {
    let Some(panicked) = line.find(" panicked at ") else {
        return line.to_string();
    };
    let head = &line[..panicked];
    let tail = &line[panicked + " panicked at ".len()..];

    // Drop a trailing ` (12345)` thread id from the head.
    let head = match (head.rfind(" ("), head.ends_with(')')) {
        (Some(open), true)
            if head[open + 2..head.len() - 1]
                .chars()
                .all(|c| c.is_ascii_digit()) =>
        {
            &head[..open]
        }
        _ => head,
    };

    // Generalise `path:LINE:COL` in the tail (which ends with a `:`).
    let tail_body = tail.strip_suffix(':').unwrap_or(tail);
    let generalised = match rsplit_line_col(tail_body) {
        Some(path) => format!("{path}:LINE:COL"),
        None => tail_body.to_string(),
    };
    format!("{head} panicked at {generalised}:")
}

/// Split a trailing `:LINE:COL` off `s`, returning the path part.
fn rsplit_line_col(s: &str) -> Option<&str> {
    let (rest, col) = s.rsplit_once(':')?;
    let (path, line) = rest.rsplit_once(':')?;
    if col.is_empty()
        || line.is_empty()
        || !col.bytes().all(|b| b.is_ascii_digit())
        || !line.bytes().all(|b| b.is_ascii_digit())
    {
        return None;
    }
    Some(path)
}

/// True for a codespan gutter line that carries an excerpt of source text,
/// e.g. `1391 │ nth(n, l): ...` or `     │        ^^^`.
fn is_gutter_line(line: &str) -> bool {
    let t = line.trim_start();
    t.starts_with('│') || t.starts_with('·') || {
        // `<digits> │ ...`
        let digits: String = t.chars().take_while(|c| c.is_ascii_digit()).collect();
        !digits.is_empty() && t[digits.len()..].trim_start().starts_with('│')
    }
}

/// Replace the source-excerpt body of any codespan block whose header points
/// into the prelude, returning the rewritten text and the number of excerpt
/// lines suppressed.
///
/// This is the direct analogue of rustc's
/// `ignore-directory-in-diagnostics-source-blocks`: showing the user prelude
/// source they did not write and cannot change is noise, and it churns the
/// golden on every prelude edit. The suppressed *count* is retained as a fact
/// so that a regression which starts excerpting library internals still shows
/// up as a snapshot change.
fn suppress_prelude_excerpts(s: &str) -> (String, usize) {
    let mut out: Vec<String> = Vec::new();
    let mut suppressed = 0usize;
    let mut lines = s.lines().peekable();
    while let Some(line) = lines.next() {
        let is_prelude_header = line.trim_start().starts_with("┌─") && line.contains("[prelude]");
        out.push(line.to_string());
        if !is_prelude_header {
            continue;
        }
        let mut count = 0usize;
        while lines.peek().is_some_and(|l| is_gutter_line(l)) {
            lines.next();
            count += 1;
        }
        if count > 0 {
            let indent: String = line.chars().take_while(|c| c.is_whitespace()).collect();
            out.push(format!(
                "{indent}<{count} prelude source line(s) suppressed>"
            ));
            suppressed += count;
        }
    }
    let mut text = out.join("\n");
    if !text.is_empty() {
        text.push('\n');
    }
    (text, suppressed)
}

/// Count how many prelude source lines [`normalise`] suppressed from `raw`.
pub fn prelude_excerpt_lines(raw_normalised: &str) -> usize {
    raw_normalised
        .lines()
        .filter_map(|l| {
            let t = l.trim();
            let rest = t
                .strip_prefix('<')?
                .strip_suffix(" prelude source line(s) suppressed>")?;
            rest.parse::<usize>().ok()
        })
        .sum()
}

/// Turn raw `eu` stderr into snapshot-stable text.
///
/// Everything removed here is either machine-specific (absolute paths, thread
/// ids), or churns without carrying diagnostic meaning (prelude line numbers,
/// prelude source excerpts, `rustc` panic line numbers). Anything a user would
/// read as part of the diagnostic is preserved verbatim.
pub fn normalise(raw: &str, cwd: &Path) -> String {
    let mut text = strip_ansi(raw);
    // Absolute paths: fixtures are always invoked with repo-relative paths, so
    // this only fires if something echoes a resolved path back.
    if let Some(root) = cwd.to_str() {
        if !root.is_empty() {
            text = text.replace(root, "<root>");
        }
    }
    text = suppress_prelude_paths(&text);
    text = suppress_prelude_positions(&text);
    let (text, _) = suppress_prelude_excerpts(&text);

    let mut lines: Vec<String> = text
        .lines()
        .map(|l| {
            let l = if l.contains(" panicked at ") {
                normalise_rust_panic(l)
            } else {
                l.to_string()
            };
            l.trim_end().to_string()
        })
        .collect();
    while lines.last().is_some_and(|l| l.is_empty()) {
        lines.pop();
    }
    if lines.is_empty() {
        return String::new();
    }
    let mut out = lines.join("\n");
    out.push('\n');
    out
}

// ─────────────────────────────────────────────────────────────────────────────
// Facts
// ─────────────────────────────────────────────────────────────────────────────

/// Objective, mechanically-derived properties of a rendered diagnostic.
///
/// Deliberately derived from the *human* rendering rather than from
/// `--error-format json`: the same extraction then works against an older
/// `eu` binary that predates the structured diagnostic model (eu-1tkk.7.1),
/// which is what makes a retroactive baseline capture from a release tag
/// possible at all.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Facts {
    pub errors: usize,
    pub warnings: usize,
    /// Stable error code of the final diagnostic, e.g. `EU-EVAL-TYPE`.
    pub code: Option<String>,
    /// Primary label location of the final diagnostic.
    pub primary: Primary,
    pub help_lines: usize,
    pub note_lines: usize,
    pub secondary_labels: usize,
    pub trace_frames: usize,
    pub trace_user_frames: usize,
    pub trace_prelude_frames: usize,
    pub prelude_excerpt_lines: usize,
    pub rust_panic: bool,
}

/// Where the final diagnostic's primary label points.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Primary {
    /// No primary label was rendered at all.
    #[default]
    None,
    /// A location in a file the user wrote.
    User(String),
    /// A location inside the prelude — invariant (i) of the objective gate
    /// says this must never happen.
    Prelude,
}

impl Primary {
    pub fn render(&self) -> String {
        match self {
            Primary::None => "none".to_string(),
            Primary::Prelude => "prelude".to_string(),
            Primary::User(loc) => format!("user {loc}"),
        }
    }

    pub fn parse(s: &str) -> Primary {
        match s.trim() {
            "none" => Primary::None,
            "prelude" => Primary::Prelude,
            other => match other.strip_prefix("user ") {
                Some(loc) => Primary::User(loc.to_string()),
                None => Primary::None,
            },
        }
    }
}

/// True when `line` opens a diagnostic of the given severity: `error: …`,
/// `error[CODE]: …`, `warning: …` or `warning[CODE]: …`.
///
/// Deliberately anchored rather than a substring test, so that a *message*
/// which happens to begin with the word (or to contain brackets, as the array
/// shape errors do — "shape [2, 2] requires 4 elements") is not miscounted.
fn is_diagnostic_header(line: &str, severity: &str) -> bool {
    let Some(rest) = line.strip_prefix(severity) else {
        return false;
    };
    rest.starts_with(':') || (rest.starts_with('[') && diagnostic_code(line).is_some())
}

/// Extract the stable error code from a diagnostic header, i.e. the `CODE` in
/// `error[CODE]: message`.
///
/// The bracket must open immediately after the severity word and close
/// immediately before the colon; anything else is message text. Codes are
/// upper-case ASCII, digits and hyphens (see `docs/reference/error-codes.md`).
fn diagnostic_code(line: &str) -> Option<String> {
    let rest = line
        .strip_prefix("error[")
        .or_else(|| line.strip_prefix("warning["))?;
    let close = rest.find(']')?;
    if !rest[close + 1..].starts_with(':') {
        return None;
    }
    let code = &rest[..close];
    if code.is_empty()
        || !code
            .chars()
            .all(|c| c.is_ascii_uppercase() || c.is_ascii_digit() || c == '-')
    {
        return None;
    }
    Some(code.to_string())
}

/// Derive [`Facts`] from already-[`normalise`]d stderr.
pub fn facts(stderr: &str) -> Facts {
    let mut f = Facts {
        prelude_excerpt_lines: prelude_excerpt_lines(stderr),
        ..Default::default()
    };
    let lines: Vec<&str> = stderr.lines().collect();

    let mut last_diagnostic: Option<usize> = None;
    for (i, line) in lines.iter().enumerate() {
        if line.contains(" panicked at ") {
            f.rust_panic = true;
        }
        if is_diagnostic_header(line, "error") {
            f.errors += 1;
            last_diagnostic = Some(i);
        } else if is_diagnostic_header(line, "warning") {
            f.warnings += 1;
            last_diagnostic = Some(i);
        }
        let t = line.trim_start();
        if t.starts_with("help:") {
            f.help_lines += 1;
        }
        if t.starts_with("= ") && t != "= while evaluating (outermost first):" {
            f.note_lines += 1;
        }
        if t.contains("called from here") {
            f.secondary_labels += 1;
        }
    }

    if let Some(start) = last_diagnostic {
        f.code = diagnostic_code(lines[start]);
        for line in &lines[start..] {
            let t = line.trim_start();
            if let Some(loc) = t.strip_prefix("┌─ ") {
                let loc = loc.trim();
                f.primary = if loc.starts_with("[prelude]") {
                    Primary::Prelude
                } else {
                    Primary::User(loc.to_string())
                };
                break;
            }
        }
    }

    // Trace frames: the `- <name> at <loc>` / `- <name> (<resource>)` lines
    // under a `= while evaluating (outermost first):` note.
    let mut in_trace = false;
    for line in &lines {
        let t = line.trim_start();
        if t == "= while evaluating (outermost first):" {
            in_trace = true;
            continue;
        }
        if !in_trace {
            continue;
        }
        if let Some(frame) = t.strip_prefix("- ") {
            f.trace_frames += 1;
            // A resource (bundled-library) frame carries no `at file:line:col`
            // any more (eu-1tkk.7.36) — it is named `(prelude)` etc. instead.
            // `[prelude]` is retained too in case a codespan header (rather
            // than a trace note) ever legitimately excerpts prelude source.
            if frame.contains("[prelude]") || frame.contains("(prelude)") {
                f.trace_prelude_frames += 1;
            } else {
                f.trace_user_frames += 1;
            }
        } else if !t.is_empty() {
            in_trace = false;
        }
    }

    f
}

// ─────────────────────────────────────────────────────────────────────────────
// Snapshot documents
// ─────────────────────────────────────────────────────────────────────────────

/// One mode's rendering plus its derived facts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Section {
    pub exit: String,
    pub facts: Facts,
    pub stderr: String,
}

impl Section {
    pub fn from_run(run: &Run) -> Section {
        Section {
            exit: match (run.timed_out, run.exit) {
                (true, _) => "timeout".to_string(),
                (false, Some(c)) => c.to_string(),
                (false, None) => "signal".to_string(),
            },
            facts: facts(&run.stderr),
            stderr: run.stderr.clone(),
        }
    }
}

/// A complete snapshot document for one fixture.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Snapshot {
    pub id: String,
    pub argv: String,
    /// `None` when the capture was source-only (no blob-capable binary).
    pub blob: Option<Section>,
    pub source: Section,
}

impl Snapshot {
    pub fn divergent(&self) -> bool {
        match &self.blob {
            None => false,
            Some(b) => b != &self.source,
        }
    }

    /// Render the `.snap` document.
    pub fn render(&self) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "# eucalypt diagnostic snapshot v{FORMAT_VERSION}\n"
        ));
        s.push_str(&format!("fixture: {}\n", self.id));
        s.push_str(&format!("argv: {}\n", self.argv));
        match &self.blob {
            None => {
                s.push_str("prelude-modes: source-only\n");
                push_section(&mut s, "--source-prelude", &self.source);
            }
            Some(blob) if blob == &self.source => {
                s.push_str("prelude-modes: identical\n");
                push_section(&mut s, "both prelude modes", &self.source);
            }
            Some(blob) => {
                s.push_str("prelude-modes: divergent\n");
                push_section(&mut s, "blob prelude", blob);
                push_section(&mut s, "--source-prelude", &self.source);
            }
        }
        s
    }
}

fn push_section(out: &mut String, label: &str, section: &Section) {
    let f = &section.facts;
    out.push_str(&format!("\n=== {label} ===\n"));
    out.push_str(&format!("exit: {}\n", section.exit));
    out.push_str(&format!("errors: {}\n", f.errors));
    out.push_str(&format!("warnings: {}\n", f.warnings));
    out.push_str(&format!("code: {}\n", f.code.as_deref().unwrap_or("-")));
    out.push_str(&format!("primary: {}\n", f.primary.render()));
    out.push_str(&format!("help-lines: {}\n", f.help_lines));
    out.push_str(&format!("note-lines: {}\n", f.note_lines));
    out.push_str(&format!("secondary-labels: {}\n", f.secondary_labels));
    out.push_str(&format!("trace-frames: {}\n", f.trace_frames));
    out.push_str(&format!("trace-user-frames: {}\n", f.trace_user_frames));
    out.push_str(&format!(
        "trace-prelude-frames: {}\n",
        f.trace_prelude_frames
    ));
    out.push_str(&format!(
        "prelude-excerpt-lines: {}\n",
        f.prelude_excerpt_lines
    ));
    out.push_str(&format!(
        "rust-panic: {}\n",
        if f.rust_panic { "yes" } else { "no" }
    ));
    out.push_str("--- stderr ---\n");
    if section.stderr.is_empty() {
        out.push_str("<no output>\n");
    } else {
        out.push_str(&section.stderr);
    }
}

/// Parse a rendered `.snap` document back into sections keyed by label.
///
/// Used by the CI gate to compare only the half of the golden that the current
/// build can reproduce, and by `--compare` to tabulate fact deltas.
pub fn parse_sections(doc: &str) -> BTreeMap<String, Section> {
    let mut out = BTreeMap::new();
    let mut label: Option<String> = None;
    let mut exit = String::new();
    let mut kv: BTreeMap<String, String> = BTreeMap::new();
    let mut body: Vec<String> = Vec::new();
    let mut in_body = false;

    let flush = |out: &mut BTreeMap<String, Section>,
                 label: &Option<String>,
                 exit: &str,
                 kv: &BTreeMap<String, String>,
                 body: &[String]| {
        let Some(label) = label else { return };
        let num = |k: &str| kv.get(k).and_then(|v| v.parse().ok()).unwrap_or(0);
        // Sections are separated by a blank line in the rendered document, so
        // the trailing blank belongs to the separator, not to the body. Not
        // trimming it here would make render → parse lossy and let a real
        // whitespace change slip past the gate.
        let mut end = body.len();
        while end > 0 && body[end - 1].is_empty() {
            end -= 1;
        }
        let body = &body[..end];
        let mut text = body.join("\n");
        if text == "<no output>" {
            text = String::new();
        } else if !text.is_empty() {
            text.push('\n');
        }
        out.insert(
            label.clone(),
            Section {
                exit: exit.to_string(),
                facts: Facts {
                    errors: num("errors"),
                    warnings: num("warnings"),
                    code: kv
                        .get("code")
                        .filter(|c| c.as_str() != "-")
                        .map(|c| c.to_string()),
                    primary: Primary::parse(
                        kv.get("primary").map(String::as_str).unwrap_or("none"),
                    ),
                    help_lines: num("help-lines"),
                    note_lines: num("note-lines"),
                    secondary_labels: num("secondary-labels"),
                    trace_frames: num("trace-frames"),
                    trace_user_frames: num("trace-user-frames"),
                    trace_prelude_frames: num("trace-prelude-frames"),
                    prelude_excerpt_lines: num("prelude-excerpt-lines"),
                    rust_panic: kv.get("rust-panic").map(String::as_str) == Some("yes"),
                },
                stderr: text,
            },
        );
    };

    for line in doc.lines() {
        if let Some(rest) = line.strip_prefix("=== ") {
            flush(&mut out, &label, &exit, &kv, &body);
            label = rest.strip_suffix(" ===").map(str::to_string);
            exit.clear();
            kv.clear();
            body.clear();
            in_body = false;
            continue;
        }
        if line == "--- stderr ---" {
            in_body = true;
            continue;
        }
        if in_body {
            body.push(line.to_string());
        } else if let Some((k, v)) = line.split_once(": ") {
            if k == "exit" {
                exit = v.to_string();
            } else {
                kv.insert(k.to_string(), v.to_string());
            }
        }
    }
    flush(&mut out, &label, &exit, &kv, &body);
    out
}

/// Read the `prelude-modes:` header of a rendered document.
pub fn parse_prelude_modes(doc: &str) -> Option<String> {
    doc.lines()
        .find_map(|l| l.strip_prefix("prelude-modes: "))
        .map(str::to_string)
}

/// Look up the section a given mode should be checked against.
///
/// A snapshot whose two modes agree stores a single `both prelude modes`
/// section, so either mode checks against it.
pub fn section_for(sections: &BTreeMap<String, Section>, mode: Mode) -> Option<&Section> {
    sections
        .get(mode.label())
        .or_else(|| sections.get("both prelude modes"))
}

// ─────────────────────────────────────────────────────────────────────────────
// Capture
// ─────────────────────────────────────────────────────────────────────────────

/// How many fixtures to run concurrently.
///
/// Every fixture is an independent `eu` process reading one file and writing
/// one pipe, so concurrency changes nothing about the output — it just turns a
/// four-minute serial sweep into a sub-minute one. Capped so a many-core CI
/// runner does not start hundreds of managed heaps at once.
fn worker_count() -> usize {
    std::thread::available_parallelism()
        .map(|n| n.get().clamp(1, 8))
        .unwrap_or(1)
}

/// Capture the whole corpus with `binary`, running from `cwd`.
///
/// `with_blob` selects whether the default (blob) mode is captured at all: a
/// binary built without an embedded prelude blob would produce a "blob" run
/// that is really a second source-prelude run, silently recording `identical`
/// for fixtures that genuinely diverge in a released build.
///
/// Results come back in corpus order regardless of scheduling, so a capture is
/// reproducible.
pub fn capture(
    binary: &Path,
    cwd: &Path,
    fixtures: &[Fixture],
    with_blob: bool,
    progress: impl FnMut(usize, usize, &str) + Send,
) -> Vec<Snapshot> {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Mutex;

    let total = fixtures.len();
    let next = AtomicUsize::new(0);
    let done = AtomicUsize::new(0);
    let results: Vec<Mutex<Option<Snapshot>>> = (0..total).map(|_| Mutex::new(None)).collect();
    let progress = Mutex::new(progress);

    std::thread::scope(|scope| {
        for _ in 0..worker_count().min(total.max(1)) {
            scope.spawn(|| loop {
                let i = next.fetch_add(1, Ordering::SeqCst);
                if i >= total {
                    return;
                }
                let fixture = &fixtures[i];
                let source = Section::from_run(&run_fixture(binary, cwd, fixture, Mode::Source));
                let blob = with_blob
                    .then(|| Section::from_run(&run_fixture(binary, cwd, fixture, Mode::Blob)));
                *results[i].lock().unwrap() = Some(Snapshot {
                    id: fixture.id.clone(),
                    argv: fixture.args.join(" "),
                    blob,
                    source,
                });
                let n = done.fetch_add(1, Ordering::SeqCst) + 1;
                (progress.lock().unwrap())(n, total, &fixture.id);
            });
        }
    });

    results
        .into_iter()
        .map(|m| m.into_inner().unwrap().expect("every slot filled"))
        .collect()
}

/// Path of a fixture's golden, under `dir`.
pub fn snapshot_path(dir: &Path, id: &str) -> PathBuf {
    dir.join(format!("{id}.snap"))
}

/// Write a capture out as one `.snap` per fixture, plus the divergence
/// inventory. Returns the paths written.
pub fn write_capture(dir: &Path, snapshots: &[Snapshot]) -> std::io::Result<Vec<PathBuf>> {
    let mut written = Vec::new();
    for snap in snapshots {
        let path = snapshot_path(dir, &snap.id);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&path, snap.render())?;
        written.push(path);
    }
    Ok(written)
}

/// Remove `.snap` files under `dir` that no longer correspond to a fixture.
pub fn prune_orphans(dir: &Path, snapshots: &[Snapshot]) -> std::io::Result<Vec<PathBuf>> {
    let keep: std::collections::BTreeSet<PathBuf> = snapshots
        .iter()
        .map(|s| snapshot_path(dir, &s.id))
        .collect();
    let mut removed = Vec::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(d) = stack.pop() {
        let Ok(entries) = std::fs::read_dir(&d) else {
            continue;
        };
        for entry in entries.flatten() {
            let p = entry.path();
            if p.is_dir() {
                stack.push(p);
            } else if p.extension().and_then(|e| e.to_str()) == Some("snap") && !keep.contains(&p) {
                std::fs::remove_file(&p)?;
                removed.push(p);
            }
        }
    }
    Ok(removed)
}

/// Render the divergence inventory: every fixture whose blob-prelude and
/// source-prelude diagnostics differ.
///
/// Checked in and regenerated by `--bless`, so a newly-introduced divergence
/// shows up as a one-line change in a single reviewable file rather than being
/// buried in one of two hundred `.snap` diffs.
pub fn render_divergence_doc(snapshots: &[Snapshot]) -> String {
    let divergent: Vec<&Snapshot> = snapshots.iter().filter(|s| s.divergent()).collect();
    let mut s = String::new();
    s.push_str("# Blob-prelude vs source-prelude diagnostic divergence\n\n");
    s.push_str(
        "Generated by `cargo xtask diag-snapshot --bless`; do not edit by hand.\n\n\
         Every fixture listed here renders a *different diagnostic* depending on whether\n\
         `eu` uses the pre-compiled prelude blob (what a released binary does) or\n\
         `--source-prelude`. Each one is a bug in the same family as eu-7x0r and\n\
         eu-9wq0s: the same program, the same mistake, two different answers.\n\n\
         The full text of both renderings is in the fixture's `.snap` file under\n\
         `tests/diagnostics/snapshots/`.\n\n",
    );
    s.push_str(&format!(
        "**{} of {} fixtures diverge.**\n\n",
        divergent.len(),
        snapshots.len()
    ));
    if divergent.is_empty() {
        s.push_str("No divergence. Any new entry here is a regression.\n");
        return s;
    }
    s.push_str("| fixture | blob primary | source primary | blob trace | source trace |\n");
    s.push_str("|---|---|---|---|---|\n");
    for snap in &divergent {
        let b = snap.blob.as_ref().expect("divergent implies blob captured");
        s.push_str(&format!(
            "| `{}` | {} | {} | {} ({} user) | {} ({} user) |\n",
            snap.id,
            b.facts.primary.render(),
            snap.source.facts.primary.render(),
            b.facts.trace_frames,
            b.facts.trace_user_frames,
            snap.source.facts.trace_frames,
            snap.source.facts.trace_user_frames,
        ));
    }
    s.push_str(&engine_scope_note());
    s
}

/// Static scope caveat, appended after the (per-run-computed) divergence
/// table above: which engine that table's figures actually cover.
///
/// The blob-vs-source table only ever exercises the **default** engine
/// (bytecode, pre-decoded dispatch) — `eu_binary()` in
/// `tests/diagnostics_snapshots.rs` spawns the release `eu` this test binary
/// was built alongside, with no `EU_HEAPSYN`/`EU_PREDECODE` override. The two
/// non-default dispatch paths are a separate, already-tracked issue
/// (eu-l51r7): asserted nowhere in this corpus, and they diverge from these
/// same goldens on a further, disjoint set of fixtures. Recorded by hand
/// (not computed from a live run of every dispatch path) because capturing
/// that would need three more full corpus sweeps; update the counts here
/// whenever eu-l51r7's own inventory moves.
fn engine_scope_note() -> String {
    "\n## Engine scope\n\n\
     The table above is for the **default engine** (bytecode, pre-decoded \
     dispatch) only — the shipped default, and the only dispatch path this \
     gate exercises with a real blob. It says nothing about the two \
     non-default paths.\n\n\
     `EU_HEAPSYN=1` and `EU_PREDECODE=0` are not asserted against these \
     goldens at all, and each diverges from them on its own set of \
     fixtures — a separate, already-known issue, not part of the count \
     above. As of eu-l51r7's most recent count: 6 fixtures diverge under \
     `EU_HEAPSYN=1`, and the same 6 diverge under `EU_PREDECODE=0`. In every \
     case the difference is one `while evaluating (outermost first):` note \
     frame missing, duplicating the primary label's own file:line:col — never a wrong \
     location or a missing primary. See eu-l51r7 for the full inventory; the \
     owner ruled this does not block 0.14 (P2, characterised and pinned \
     per-engine rather than fixed).\n"
        .to_string()
}

// ─────────────────────────────────────────────────────────────────────────────
// Bundles and comparison
// ─────────────────────────────────────────────────────────────────────────────

/// Concatenate a capture into a single self-describing text document.
///
/// Baselines are stored as bundles rather than directories so that comparing
/// two points in history is one `diff` of two files that reads top-to-bottom,
/// instead of a recursive directory diff.
pub fn bundle(label: &str, snapshots: &[Snapshot]) -> String {
    let mut s = String::new();
    s.push_str(&format!(
        "# eucalypt diagnostic snapshot bundle v{FORMAT_VERSION}\n"
    ));
    s.push_str(&format!("capture: {label}\n"));
    s.push_str(&format!("fixtures: {}\n", snapshots.len()));
    s.push_str(
        "#\n# One entry per corpus fixture, sorted by id. See\n\
         # tests/diagnostics/SNAPSHOTS.md for the format and the normalisation rules.\n",
    );
    let mut sorted: Vec<&Snapshot> = snapshots.iter().collect();
    sorted.sort_by(|a, b| a.id.cmp(&b.id));
    for snap in sorted {
        s.push_str("\n════════════════════════════════════════════════════════════════════\n");
        s.push_str(&snap.render());
    }
    s
}

/// Split a bundle back into per-fixture documents keyed by fixture id.
pub fn split_bundle(text: &str) -> BTreeMap<String, String> {
    let mut out = BTreeMap::new();
    let mut current: Option<String> = None;
    let mut buf: Vec<&str> = Vec::new();
    for line in text.lines() {
        if line.starts_with('═') {
            if let Some(id) = current.take() {
                out.insert(id, buf.join("\n"));
            }
            buf.clear();
            continue;
        }
        if let Some(id) = line.strip_prefix("fixture: ") {
            if current.is_none() {
                current = Some(id.to_string());
            }
        }
        if current.is_some() {
            buf.push(line);
        }
    }
    if let Some(id) = current.take() {
        out.insert(id, buf.join("\n"));
    }
    out
}

/// Load a capture as per-fixture documents from either a bundle file or a
/// directory of `.snap` files.
pub fn load_capture(path: &Path) -> std::io::Result<BTreeMap<String, String>> {
    if path.is_file() {
        return Ok(split_bundle(&std::fs::read_to_string(path)?));
    }
    let mut out = BTreeMap::new();
    let mut stack = vec![path.to_path_buf()];
    while let Some(d) = stack.pop() {
        for entry in std::fs::read_dir(&d)?.flatten() {
            let p = entry.path();
            if p.is_dir() {
                stack.push(p);
            } else if p.extension().and_then(|e| e.to_str()) == Some("snap") {
                let doc = std::fs::read_to_string(&p)?;
                if let Some(id) = doc.lines().find_map(|l| l.strip_prefix("fixture: ")) {
                    out.insert(id.to_string(), doc);
                }
            }
        }
    }
    Ok(out)
}

/// A per-fixture verdict from comparing two captures.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Delta {
    /// Present in both, byte-identical.
    Same,
    /// Present in both, text differs.
    Changed {
        before: Box<Facts>,
        after: Box<Facts>,
    },
    /// Only in the "after" capture — usually a fixture added since.
    Added,
    /// Only in the "before" capture — usually a fixture removed since.
    Removed,
}

/// Compare two captures fixture by fixture.
///
/// `mode` selects which prelude mode's section to compare; a snapshot whose
/// modes agree stores a single shared section and is used for either.
pub fn compare(
    before: &BTreeMap<String, String>,
    after: &BTreeMap<String, String>,
    mode: Mode,
) -> Vec<(String, Delta)> {
    let mut ids: Vec<&String> = before.keys().chain(after.keys()).collect();
    ids.sort();
    ids.dedup();
    let mut out = Vec::new();
    for id in ids {
        let delta = match (before.get(id), after.get(id)) {
            (None, Some(_)) => Delta::Added,
            (Some(_), None) => Delta::Removed,
            (Some(b), Some(a)) => {
                let bs = parse_sections(b);
                let as_ = parse_sections(a);
                match (section_for(&bs, mode), section_for(&as_, mode)) {
                    (Some(b), Some(a)) if b == a => Delta::Same,
                    (Some(b), Some(a)) => Delta::Changed {
                        before: Box::new(b.facts.clone()),
                        after: Box::new(a.facts.clone()),
                    },
                    _ => Delta::Same,
                }
            }
            (None, None) => unreachable!(),
        };
        out.push((id.clone(), delta));
    }
    out
}
