//! Analyse core unit into test plan

use crate::core::error::CoreError;
use crate::core::expr::*;
use crate::core::metadata::{ReadMetadata, TestHeaderMetadata};
use crate::core::target::Target;
use crate::core::unit::TranslationUnit;
use regex::Regex;
use std::path::{Path, PathBuf};

/// A key under which we will find a single-argument validation
/// function to process the test results
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TestExpectation {
    /// Documentation for the test
    doc: String,
    /// Key for lookup of validation function
    key: String,
}

impl TestExpectation {
    /// Test documentation
    pub fn doc(&self) -> &String {
        &self.doc
    }

    /// Key for lookup of validation function
    pub fn path(&self) -> &str {
        &self.key
    }
}

/// Expected exit code and/or stderr pattern from an `.expect` sidecar
///
/// Sidecar files sit alongside error test `.eu` files and specify
/// what constitutes a correct error: the exit code and/or a regex
/// pattern that must match somewhere in stderr output.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ErrorExpectation {
    /// Expected exit code (exact equality)
    exit: Option<i32>,
    /// Regex pattern to match against stderr
    stderr_pattern: Option<String>,
    /// Regex patterns that must *not* match stderr (`stderr-absent:`, may
    /// repeat) — used to gate that a hint is not shown without evidence
    /// (eu-oaz72.28)
    stderr_absent: Vec<String>,
    /// The primary label's location (`location:`), as `LINE:COL` in the
    /// fixture itself or `FILE:LINE:COL` — checked against the first `┌─`
    /// line of the first `error` diagnostic, so a location that appears
    /// only in a trace note or a warning cannot satisfy it (eu-ziq8o)
    location: Option<String>,
    /// Why this fixture pins no location (`no-location:`). An error sidecar
    /// must give either this or `location:`; see
    /// [`ErrorExpectation::validate_location`]
    no_location: Option<String>,
}

impl ErrorExpectation {
    /// Primary-label location pinned by the sidecar, if any
    pub fn location(&self) -> Option<&str> {
        self.location.as_deref()
    }

    /// Reason given for pinning no location, if any
    pub fn no_location(&self) -> Option<&str> {
        self.no_location.as_deref()
    }

    /// Check the primary label's location against `location:`.
    ///
    /// An error sidecar must pin where its error is reported, or say why
    /// it does not: a sidecar asserting only message text lets a
    /// wrong-location regression ship with a green test (eu-ziq8o —
    /// `083_array_arith_mismatch` named the wrong declaration in every
    /// release that shipped it). So a sidecar with neither `location:`
    /// nor `no-location:` fails here.
    ///
    /// The primary label is the first `┌─` line after the first line
    /// that opens an `error` diagnostic. Anchoring there, rather than
    /// searching stderr for the location anywhere, keeps a type warning
    /// printed before the error, or the `stack trace:` / `while
    /// evaluating` notes after it, from satisfying the check. `fixture`
    /// is the file name (`LINE:COL` locations are in it); ANSI colour
    /// codes are ignored.
    pub fn validate_location(&self, fixture: &str, actual_stderr: &str) -> Result<(), String> {
        if self.no_location.is_some() {
            return Ok(());
        }
        let Some(location) = &self.location else {
            return Err(
                "sidecar pins no source location: add `location: LINE:COL` (the primary \
                 label's position) or `no-location: <reason>` (eu-ziq8o)"
                    .to_string(),
            );
        };
        let wanted = if location.matches(':').count() >= 2 {
            location.clone()
        } else {
            format!("{fixture}:{location}")
        };
        let actual = primary_label_location(actual_stderr).ok_or_else(|| {
            format!(
                "expected the primary label at {wanted}, but stderr has no `┌─` label \
                 under an `error` diagnostic\nactual stderr:\n{actual_stderr}"
            )
        })?;
        let matches = actual == wanted
            || actual.ends_with(&format!("/{wanted}"))
            || actual.ends_with(&format!("\\{wanted}"));
        if matches {
            Ok(())
        } else {
            Err(format!(
                "primary label is at {actual}, expected {wanted}\nactual stderr:\n{actual_stderr}"
            ))
        }
    }

    /// Expected exit code
    pub fn exit(&self) -> Option<i32> {
        self.exit
    }

    /// Stderr regex pattern
    pub fn stderr_pattern(&self) -> Option<&str> {
        self.stderr_pattern.as_deref()
    }

    /// Validate actual exit code and stderr against expectations.
    /// Returns `Ok(())` on success, or `Err(reason)` on failure.
    pub fn validate(&self, actual_exit: i32, actual_stderr: &str) -> Result<(), String> {
        if let Some(expected_exit) = self.exit {
            if actual_exit != expected_exit {
                return Err(format!(
                    "exit code mismatch: expected {expected_exit}, got {actual_exit}"
                ));
            }
        }
        if let Some(ref pattern) = self.stderr_pattern {
            let re = Regex::new(pattern)
                .map_err(|e| format!("invalid stderr regex pattern '{pattern}': {e}"))?;
            if !re.is_match(actual_stderr) {
                return Err(format!(
                    "stderr did not match pattern '{pattern}'\nactual stderr:\n{actual_stderr}"
                ));
            }
        }
        for pattern in &self.stderr_absent {
            let re = Regex::new(pattern)
                .map_err(|e| format!("invalid stderr-absent regex pattern '{pattern}': {e}"))?;
            if re.is_match(actual_stderr) {
                return Err(format!(
                    "stderr matched absent pattern '{pattern}'\nactual stderr:\n{actual_stderr}"
                ));
            }
        }
        Ok(())
    }

    /// Parse an `.expect` sidecar file from its contents.
    ///
    /// The format is simple key-value YAML:
    /// ```yaml
    /// exit: 1
    /// stderr: "pattern"
    /// stderr-absent: "pattern that must not appear"
    /// ```
    ///
    /// `stderr-absent:` may be given more than once.
    pub fn parse(content: &str) -> Result<Self, String> {
        let mut exit = None;
        let mut stderr_pattern = None;
        let mut stderr_absent = vec![];
        let mut location = None;
        let mut no_location = None;

        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            if let Some(value) = line.strip_prefix("exit:") {
                let value = value.trim();
                exit = Some(
                    value
                        .parse::<i32>()
                        .map_err(|e| format!("invalid exit code '{value}': {e}"))?,
                );
            } else if let Some(value) = line.strip_prefix("stderr:") {
                let value = value.trim();
                // Strip surrounding quotes if present
                let value = value
                    .strip_prefix('"')
                    .and_then(|v| v.strip_suffix('"'))
                    .unwrap_or(value);
                stderr_pattern = Some(value.to_string());
            } else if let Some(value) = line.strip_prefix("location:") {
                let value = unquote(value.trim());
                let mut parts = value.rsplitn(3, ':');
                let col = parts.next().unwrap_or("");
                let line_no = parts.next().unwrap_or("");
                if col.parse::<u32>().is_err() || line_no.parse::<u32>().is_err() {
                    return Err(format!(
                        "invalid location '{value}': expected LINE:COL or FILE:LINE:COL"
                    ));
                }
                location = Some(value.to_string());
            } else if let Some(value) = line.strip_prefix("no-location:") {
                let value = unquote(value.trim());
                if value.is_empty() {
                    return Err("no-location: needs a reason".to_string());
                }
                no_location = Some(value.to_string());
            } else if let Some(value) = line.strip_prefix("stderr-absent:") {
                let value = value.trim();
                let value = value
                    .strip_prefix('"')
                    .and_then(|v| v.strip_suffix('"'))
                    .unwrap_or(value);
                stderr_absent.push(value.to_string());
            }
        }

        if exit.is_none() && stderr_pattern.is_none() {
            return Err("sidecar must specify at least one of 'exit' or 'stderr'".to_string());
        }

        if location.is_some() && no_location.is_some() {
            return Err("sidecar gives both 'location' and 'no-location'".to_string());
        }

        Ok(ErrorExpectation {
            exit,
            stderr_pattern,
            stderr_absent,
            location,
            no_location,
        })
    }

    /// Try to load an `.expect` sidecar for the given test file path.
    /// Returns `None` if no sidecar exists.
    pub fn load(test_file: &Path) -> Result<Option<Self>, String> {
        let mut sidecar_path = test_file.to_path_buf().into_os_string();
        sidecar_path.push(".expect");
        let sidecar_path = PathBuf::from(sidecar_path);

        if !sidecar_path.exists() {
            return Ok(None);
        }

        let content = std::fs::read_to_string(&sidecar_path)
            .map_err(|e| format!("failed to read sidecar '{}': {e}", sidecar_path.display()))?;

        Self::parse(&content).map(Some)
    }
}

/// Strip surrounding double quotes from a sidecar value, if present.
fn unquote(value: &str) -> &str {
    value
        .strip_prefix('"')
        .and_then(|v| v.strip_suffix('"'))
        .unwrap_or(value)
}

/// The location text of the primary label in rendered diagnostics: the
/// first `┌─` line after the first line opening an `error` diagnostic,
/// with ANSI colour codes removed.
fn primary_label_location(stderr: &str) -> Option<String> {
    let ansi = Regex::new("\x1b\\[[0-9;]*m").expect("valid ANSI regex");
    let plain = ansi.replace_all(stderr, "");
    plain
        .lines()
        .skip_while(|l| !l.trim_start().starts_with("error"))
        .find_map(|l| l.split_once("┌─").map(|(_, loc)| loc.trim().to_string()))
}

/// Plan for the tests to run against an eu file
///
/// A test plan consists of combinations of targets and formats to run
/// and a set of expectations to be validated against each run.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TestPlan {
    /// Run ID string for disambiguating repeated runs
    run_id: String,
    /// Test filename
    file: PathBuf,
    /// Test title
    title: String,
    /// Test targets to run (and their validations) and formats for each
    targets: Vec<(Target, Vec<String>)>,
    /// Error test expectations from `.expect` sidecar (if any)
    error_expectation: Option<ErrorExpectation>,
    /// Whether this test requires `--allow-io` to run
    requires_io: bool,
    /// Targets declared in the file whose bodies carry checks but which
    /// the tester does not run, so their checks would never gate
    unrun_checked_targets: Vec<Target>,
}

impl TestPlan {
    /// The filename of the test
    pub fn file(&self) -> &Path {
        &self.file
    }

    /// A test title
    pub fn title(&self) -> &String {
        &self.title
    }

    /// Test targets to run
    pub fn targets(&self) -> &Vec<(Target, Vec<String>)> {
        &self.targets
    }

    /// Error test expectations from `.expect` sidecar
    pub fn error_expectation(&self) -> Option<&ErrorExpectation> {
        self.error_expectation.as_ref()
    }

    /// Whether this is an error test with sidecar expectations
    pub fn is_error_test(&self) -> bool {
        self.error_expectation.is_some()
    }

    /// Whether this test requires `--allow-io` to run.
    ///
    /// When true, `eu test` skips the test rather than failing if
    /// `--allow-io` is not set.
    pub fn requires_io(&self) -> bool {
        self.requires_io
    }

    /// Targets declared in the file under test whose bodies carry checks
    /// (a `RESULT` binding or an inline assertion) but which the tester
    /// does not run: not named `test…` or `bench-…`, not `main`, and
    /// with no `verify`. Their checks would never gate, so the tester
    /// fails the file.
    pub fn unrun_checked_targets(&self) -> &[Target] {
        &self.unrun_checked_targets
    }

    /// Whether this plan contains any benchmark targets
    pub fn has_benchmarks(&self) -> bool {
        self.targets.iter().any(|(t, _)| t.is_benchmark())
    }

    /// The run this plan belongs to
    pub fn run_id(&self) -> &str {
        &self.run_id
    }

    pub fn test_directory(&self) -> &Path {
        self.file().parent().unwrap()
    }

    pub fn result_directory(&self) -> PathBuf {
        let mut directory = PathBuf::from(self.file().parent().unwrap());
        directory.push(".result");
        directory.push(&self.run_id);
        directory
    }

    pub fn evidence_file_name(&self) -> PathBuf {
        self.result_directory()
            .join(self.sidecar_name("evidence.yaml"))
    }

    pub fn result_file_name(&self) -> PathBuf {
        self.result_directory()
            .join(self.sidecar_name("report.yaml"))
    }

    /// Name of a result file for this test: the test file's full name
    /// with `suffix` appended, so `x.eu` and `x.yaml` in one directory
    /// get distinct files (`x.eu.report.yaml`, `x.yaml.report.yaml`).
    fn sidecar_name(&self, suffix: &str) -> String {
        let name = self
            .file()
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "untitled".to_string());
        format!("{name}.{suffix}")
    }

    pub fn prepare_directory(&self) -> Result<(), std::io::Error> {
        std::fs::create_dir_all(self.result_directory())
    }

    /// Analyse a translation unit to determine test plan
    pub fn analyse(
        run_id: &str,
        filename: &Path,
        unit: &TranslationUnit,
    ) -> Result<Self, CoreError> {
        // parse head metadata
        let header = if let Expr::Meta(_, _, m) = &*unit.expr.inner {
            m.clone().read_metadata()?
        } else {
            TestHeaderMetadata::default()
        };

        // by default use yaml format
        let mut formats = header.formats;
        if formats.is_empty() {
            formats.push("yaml".to_string());
        }

        // Auto-discover test and benchmark targets from the top-level file only.
        //
        // `test-*` and `bench-*` targets are filtered to `own_targets` — targets
        // declared in the file under test itself.  This prevents targets from
        // transitively imported files from being run as part of the importing
        // file's test suite, whilst still allowing them to be invoked explicitly
        // via `eu -t target-name file.eu` (they remain in `unit.targets`).
        //
        // The `:main` target and any explicitly validated targets are drawn from
        // the full `targets` set so they work even when declared in imported files.
        let mut targets: Vec<(Target, Vec<String>)> = vec![];
        let mut unrun_checked_targets: Vec<Target> = vec![];
        for t in &unit.targets {
            let include = if t.name().starts_with("test") || t.is_benchmark() {
                // Only auto-run test/bench targets from the file itself.
                unit.own_targets.contains(t)
            } else {
                // :main and validated targets are always included regardless of origin.
                t.name() == "main" || !t.validations().is_empty()
            };

            if include {
                if let Some(f) = t.format() {
                    targets.push((t.clone(), vec![f.to_string()]))
                } else {
                    targets.push((t.clone(), formats.clone()))
                }
            } else if t.has_checks() && unit.own_targets.contains(t) {
                unrun_checked_targets.push(t.clone());
            }
        }
        unrun_checked_targets.sort_by(|a, b| a.name().cmp(b.name()));

        // default to empty target if there are none
        if targets.is_empty() {
            targets.push((Target::default(), formats));
        }

        Ok(TestPlan {
            run_id: run_id.to_string(),
            file: filename.to_path_buf(),
            title: header
                .title
                .or_else(|| {
                    filename
                        .to_path_buf()
                        .file_stem()
                        .map(|os| os.to_string_lossy().into_owned())
                })
                .unwrap_or_else(|| "untitled".to_string()),
            targets,
            error_expectation: None,
            requires_io: header.requires_io,
            unrun_checked_targets,
        })
    }

    /// Create a test plan for an error test file.
    ///
    /// Error tests use a single default target and validate against an
    /// `.expect` sidecar rather than in-file RESULT assertions.
    ///
    /// Pass `target_name = Some("name")` to evaluate a specific named target
    /// rather than the whole document.  Useful when the error is triggered only
    /// when evaluating a particular target (e.g. an IO action).
    pub fn for_error_test(
        run_id: &str,
        filename: &Path,
        expectation: ErrorExpectation,
        target_name: Option<&str>,
    ) -> Self {
        let title = filename
            .file_stem()
            .map(|os| os.to_string_lossy().into_owned())
            .unwrap_or_else(|| "untitled".to_string());

        let target = match target_name {
            Some(name) if !name.is_empty() => Target::new(
                name.to_string(),
                String::new(),
                None,
                vec![name.to_string()],
                vec![],
            ),
            _ => Target::default(),
        };

        TestPlan {
            run_id: run_id.to_string(),
            file: filename.to_path_buf(),
            title,
            targets: vec![(target, vec!["yaml".to_string()])],
            error_expectation: Some(expectation),
            requires_io: false,
            unrun_checked_targets: vec![],
        }
    }

    /// Create a test plan for an unvalidated error test (no sidecar).
    pub fn for_unvalidated_error_test(run_id: &str, filename: &Path) -> Self {
        let title = filename
            .file_stem()
            .map(|os| os.to_string_lossy().into_owned())
            .unwrap_or_else(|| "untitled".to_string());

        TestPlan {
            run_id: run_id.to_string(),
            file: filename.to_path_buf(),
            title,
            targets: vec![(Target::default(), vec!["yaml".to_string()])],
            error_expectation: None,
            requires_io: false,
            unrun_checked_targets: vec![],
        }
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
pub mod tests {

    use super::*;
    use crate::driver::source::SourceLoader;
    use crate::syntax::input::*;
    use std::collections::HashSet;
    use std::iter::FromIterator;

    #[test]
    fn test_parse_expect_sidecar_both_fields() {
        let content = "exit: 1\nstderr: \"division by zero\"\n";
        let exp = ErrorExpectation::parse(content).expect("should parse");
        assert_eq!(exp.exit(), Some(1));
        assert_eq!(exp.stderr_pattern(), Some("division by zero"));
    }

    #[test]
    fn test_parse_expect_sidecar_exit_only() {
        let content = "exit: 2\n";
        let exp = ErrorExpectation::parse(content).expect("should parse");
        assert_eq!(exp.exit(), Some(2));
        assert_eq!(exp.stderr_pattern(), None);
    }

    #[test]
    fn test_parse_expect_sidecar_stderr_absent() {
        let content = "exit: 1\nstderr-absent: \"arrow\"\nstderr-absent: \"and\"\n";
        let exp = ErrorExpectation::parse(content).unwrap();
        assert!(exp.validate(1, "unresolved variable 'g'").is_ok());
        assert!(exp.validate(1, "note: no arrow functions").is_err());
        assert!(exp.validate(1, "'and' and 'or'").is_err());
    }

    #[test]
    fn test_parse_expect_sidecar_stderr_only() {
        let content = "stderr: \"unterminated string\"\n";
        let exp = ErrorExpectation::parse(content).expect("should parse");
        assert_eq!(exp.exit(), None);
        assert_eq!(exp.stderr_pattern(), Some("unterminated string"));
    }

    #[test]
    fn test_parse_expect_sidecar_unquoted_stderr() {
        let content = "stderr: unterminated string\n";
        let exp = ErrorExpectation::parse(content).expect("should parse");
        assert_eq!(exp.stderr_pattern(), Some("unterminated string"));
    }

    #[test]
    fn test_parse_expect_sidecar_empty_fails() {
        let content = "# just a comment\n";
        assert!(ErrorExpectation::parse(content).is_err());
    }

    #[test]
    fn test_parse_expect_sidecar_with_comments() {
        let content =
            "# Check for division error\nexit: 1\n# stderr pattern\nstderr: \"division\"\n";
        let exp = ErrorExpectation::parse(content).expect("should parse");
        assert_eq!(exp.exit(), Some(1));
        assert_eq!(exp.stderr_pattern(), Some("division"));
    }

    /// Rendered stderr with a warning before the error and the same
    /// location repeated in a trace note after it.
    const LABELLED_STDERR: &str = "warning: type mismatch calling 'f'\n  \
        ┌─ tests/harness/errors/100_x.eu:2:3\n\n\
        \x1b[1merror\x1b[0m: boom\n  \x1b[34m┌─\x1b[0m tests/harness/errors/100_x.eu:4:9\n  \
        = while evaluating:\n    - f at 100_x.eu:7:1\n";

    #[test]
    fn test_location_matches_primary_label() {
        let exp = ErrorExpectation::parse("exit: 1\nlocation: 4:9\n").unwrap();
        assert_eq!(exp.location(), Some("4:9"));
        assert!(exp.validate_location("100_x.eu", LABELLED_STDERR).is_ok());
        let exp = ErrorExpectation::parse("exit: 1\nlocation: \"100_x.eu:4:9\"\n").unwrap();
        assert!(exp.validate_location("other.eu", LABELLED_STDERR).is_ok());
    }

    #[test]
    fn test_location_not_satisfied_by_warning_or_note() {
        // 2:3 is the warning's label, 7:1 only a trace note.
        for loc in ["2:3", "7:1", "4:1", "14:9"] {
            let exp = ErrorExpectation::parse(&format!("exit: 1\nlocation: {loc}\n")).unwrap();
            assert!(
                exp.validate_location("100_x.eu", LABELLED_STDERR).is_err(),
                "{loc} must not match"
            );
        }
    }

    #[test]
    fn test_location_required_unless_opted_out() {
        let exp = ErrorExpectation::parse("exit: 1\n").unwrap();
        let err = exp
            .validate_location("100_x.eu", LABELLED_STDERR)
            .unwrap_err();
        assert!(err.contains("pins no source location"), "{err}");
        let exp = ErrorExpectation::parse("exit: 1\nno-location: CLI error\n").unwrap();
        assert_eq!(exp.no_location(), Some("CLI error"));
        assert!(exp.validate_location("100_x.eu", "eu: bad flag").is_ok());
    }

    #[test]
    fn test_location_sidecar_errors() {
        assert!(ErrorExpectation::parse("exit: 1\nlocation: 4\n").is_err());
        assert!(ErrorExpectation::parse("exit: 1\nlocation: a:b\n").is_err());
        assert!(ErrorExpectation::parse("exit: 1\nno-location:\n").is_err());
        assert!(ErrorExpectation::parse("exit: 1\nlocation: 1:2\nno-location: x\n").is_err());
    }

    #[test]
    fn test_location_without_error_label_fails() {
        let exp = ErrorExpectation::parse("exit: 1\nlocation: 2:3\n").unwrap();
        // A label under a warning only is not a primary error label.
        let stderr = "warning: w\n  ┌─ 100_x.eu:2:3\n";
        assert!(exp.validate_location("100_x.eu", stderr).is_err());
    }

    #[test]
    fn test_validate_pass() {
        let exp = ErrorExpectation {
            exit: Some(1),
            stderr_pattern: Some("division by zero".to_string()),
            stderr_absent: vec![],
            location: None,
            no_location: None,
        };
        assert!(exp.validate(1, "error: division by zero at line 5").is_ok());
    }

    #[test]
    fn test_validate_exit_mismatch() {
        let exp = ErrorExpectation {
            exit: Some(1),
            stderr_pattern: None,
            stderr_absent: vec![],
            location: None,
            no_location: None,
        };
        let result = exp.validate(0, "");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("exit code mismatch"));
    }

    #[test]
    fn test_validate_stderr_mismatch() {
        let exp = ErrorExpectation {
            exit: None,
            stderr_pattern: Some("expected pattern".to_string()),
            stderr_absent: vec![],
            location: None,
            no_location: None,
        };
        let result = exp.validate(1, "something else entirely");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("stderr did not match"));
    }

    #[test]
    fn test_validate_regex_pattern() {
        let exp = ErrorExpectation {
            exit: None,
            stderr_pattern: Some(r"line \d+".to_string()),
            stderr_absent: vec![],
            location: None,
            no_location: None,
        };
        assert!(exp.validate(1, "error at line 42").is_ok());
        assert!(exp.validate(1, "error at line XY").is_err());
    }

    #[test]
    pub fn test_bench_targets_included() {
        let plan = source_to_test_plan(
            "
` { target: :bench-sort }
sort-benchmark: {
  data: [3, 1, 2]
  result: data
}

` { target: :test-basic }
basic: { RESULT: :PASS }
",
        );
        let names: Vec<_> = plan
            .targets()
            .iter()
            .map(|(t, _)| t.name().clone())
            .collect();
        assert!(names.contains(&"bench-sort".to_string()));
        assert!(names.contains(&"test-basic".to_string()));
        assert!(plan.has_benchmarks());
    }

    #[test]
    pub fn test_bench_target_is_benchmark() {
        let plan = source_to_test_plan(
            "
` { target: :bench-fib }
fib: 42
",
        );
        let bench_targets: Vec<_> = plan
            .targets()
            .iter()
            .filter(|(t, _)| t.is_benchmark())
            .collect();
        assert_eq!(bench_targets.len(), 1);
        assert_eq!(bench_targets[0].0.name(), "bench-fib");
    }

    #[test]
    pub fn test_no_bench_targets() {
        let plan = source_to_test_plan("RESULT: :PASS");
        assert!(!plan.has_benchmarks());
    }

    fn unrun_checked_names(plan: &TestPlan) -> Vec<String> {
        plan.unrun_checked_targets()
            .iter()
            .map(|t| t.name().clone())
            .collect()
    }

    /// A target with checks that the tester does not run is reported;
    /// targets the tester runs, and targets without checks, are not.
    #[test]
    pub fn test_unrun_checked_targets() {
        let plan = source_to_test_plan(
            "
` { target: :semantics }
semantics: { a: 1 = 1 RESULT: a then(:PASS, :FAIL) }

` { target: :asserted }
asserted: { a: 1 //=> 1 }

` { target: :predicate }
predicate: { a: 1 //=?> pos? }

` { target: :test-named }
named: { RESULT: :PASS }

` { target: :verified verify: [:v] }
verified: { RESULT: :PASS }

` { target: :fixture }
fixture: 1

v(context): :PASS
",
        );
        assert_eq!(
            unrun_checked_names(&plan),
            vec![
                "asserted".to_string(),
                "predicate".to_string(),
                "semantics".to_string()
            ]
        );
    }

    /// A metadata pass-through (`//`) is not an assertion.
    #[test]
    pub fn test_metadata_operator_is_not_a_check() {
        let plan = source_to_test_plan(
            "
` { target: :tagged }
tagged: 1 // { a: 1 }
",
        );
        assert!(unrun_checked_names(&plan).is_empty());
    }

    pub fn source_to_test_plan(text: &str) -> TestPlan {
        let sample_input = Input::from(Locator::Literal(text.to_string()));
        let mut loader = SourceLoader::new(vec![]);
        loader.load(&sample_input).unwrap();
        loader.translate(&sample_input).unwrap();
        loader.merge_units(&[sample_input]).unwrap();
        let run_id = format!("{}", chrono::offset::Utc::now().timestamp_millis());
        TestPlan::analyse(&run_id, &PathBuf::from("test"), loader.core()).unwrap()
    }

    #[test]
    pub fn test_simple() {
        let plan = source_to_test_plan("RESULT: :PASS");
        assert_eq!(plan.title(), "test");
    }

    #[test]
    pub fn test_title() {
        let plan = source_to_test_plan(" { title: \"A test\" } RESULT: :PASS");
        assert_eq!(plan.title(), "A test");
    }

    #[test]
    pub fn test_targets() {
        let plan = source_to_test_plan(
            "
` { target: :test-addition verify: [:validate-something] }
addition: passes(2 + 2 = 4)

` { target: :test-subtraction }
subtraction: passes(4 - 1 = 2)

` :main
result: :pass
",
        );
        assert_eq!(
            plan.targets()
                .iter()
                .map(|(t, _)| t.name())
                .collect::<HashSet<_>>(),
            HashSet::from_iter(
                [
                    "test-addition".to_string(),
                    "test-subtraction".to_string(),
                    "main".to_string()
                ]
                .iter()
            )
        );
        assert_eq!(
            plan.targets()
                .iter()
                .flat_map(|t| t.0.validations())
                .collect::<Vec<&String>>(),
            vec![&"validate-something".to_string()]
        );
    }

    /// Build a [`TestPlan`] and the merged [`TranslationUnit`] from a file on
    /// the filesystem, loading transitive imports as the real driver does.
    fn file_to_plan_and_unit(
        file_path: &std::path::Path,
    ) -> (TestPlan, crate::core::unit::TranslationUnit) {
        let loc = Locator::Fs(file_path.to_path_buf());
        let input = Input::from(loc);
        let lib_path = vec![file_path.parent().unwrap().to_path_buf()];
        let mut loader = SourceLoader::new(lib_path);
        loader.load(&input).unwrap();
        loader.translate(&input).unwrap();
        loader.merge_units(&[input]).unwrap();
        let run_id = format!("{}", chrono::offset::Utc::now().timestamp_millis());
        let plan = TestPlan::analyse(&run_id, file_path, loader.core()).unwrap();
        let unit = loader.core().clone();
        (plan, unit)
    }

    /// Targets from imported files must NOT be auto-discovered by the test
    /// runner (`test-*` / `bench-*` prefix), but they MUST remain available
    /// in the merged target set so `eu -t target-name file.eu` still works.
    #[test]
    fn test_imported_targets_excluded_from_plan() {
        use std::io::Write;

        // Create a temporary directory to hold two source files.
        let dir = std::env::temp_dir().join("eu_test_target_scoping");
        std::fs::create_dir_all(&dir).expect("create temp dir");

        // The imported library defines its own test target and a named
        // (non-test) target.  The test target must NOT appear in the
        // importing file's auto-discovered plan; the named target must
        // still be reachable via `-t`.
        let lib_path = dir.join("lib.eu");
        {
            let mut f = std::fs::File::create(&lib_path).expect("create lib.eu");
            writeln!(
                f,
                "` {{ target: :test-in-lib }}\nlib-test-result: :PASS\n\n\
                 ` {{ target: :lib-output }}\nlib-data: 42"
            )
            .unwrap();
        }

        // The top-level file imports the library and declares its own target.
        let main_path = dir.join("main.eu");
        {
            let mut f = std::fs::File::create(&main_path).expect("create main.eu");
            writeln!(
                f,
                "{{ import: \"lib.eu\" }}\n\n` {{ target: :test-in-main }}\nown-result: :PASS"
            )
            .unwrap();
        }

        let (plan, unit) = file_to_plan_and_unit(&main_path);

        // ── Test plan auto-discovery ──────────────────────────────────────────
        let plan_target_names: HashSet<_> = plan
            .targets()
            .iter()
            .map(|(t, _)| t.name().clone())
            .collect();

        // The top-level file's own test target must be auto-discovered.
        assert!(
            plan_target_names.contains("test-in-main"),
            "own test target should be auto-discovered; plan targets: {:?}",
            plan_target_names
        );

        // The imported file's test target must NOT be auto-discovered.
        assert!(
            !plan_target_names.contains("test-in-lib"),
            "imported test target should NOT be auto-discovered; plan targets: {:?}",
            plan_target_names
        );

        // ── Full target set (for `-t` invocation) ────────────────────────────
        let all_target_names: HashSet<_> = unit.targets.iter().map(|t| t.name().clone()).collect();

        // The imported non-test target must still be in the merged set.
        assert!(
            all_target_names.contains("lib-output"),
            "imported non-test target should remain in unit.targets; all targets: {:?}",
            all_target_names
        );

        // The imported test target must also remain in the merged set.
        assert!(
            all_target_names.contains("test-in-lib"),
            "imported test target should remain in unit.targets for -t invocation; all targets: {:?}",
            all_target_names
        );

        // Clean up.
        let _ = std::fs::remove_dir_all(&dir);
    }
}
