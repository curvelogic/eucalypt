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
}

impl ErrorExpectation {
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
        Ok(())
    }

    /// Parse an `.expect` sidecar file from its contents.
    ///
    /// The format is simple key-value YAML:
    /// ```yaml
    /// exit: 1
    /// stderr: "pattern"
    /// ```
    pub fn parse(content: &str) -> Result<Self, String> {
        let mut exit = None;
        let mut stderr_pattern = None;

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
            }
        }

        if exit.is_none() && stderr_pattern.is_none() {
            return Err("sidecar must specify at least one of 'exit' or 'stderr'".to_string());
        }

        Ok(ErrorExpectation {
            exit,
            stderr_pattern,
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

    /// Whether this plan contains any benchmark targets
    pub fn has_benchmarks(&self) -> bool {
        self.targets.iter().any(|(t, _)| t.is_benchmark())
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
        let mut name = self.result_directory();
        name.push(self.file().file_name().unwrap());
        name.set_extension("evidence.yaml");
        name
    }

    pub fn result_file_name(&self) -> PathBuf {
        let mut name = self.result_directory();
        name.push(self.file().file_name().unwrap());
        name.set_extension("report.yaml");
        name
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

        // run all targets beginning with test- or bench-, plus main and validated targets
        let mut targets: Vec<(Target, Vec<String>)> = vec![];
        for t in &unit.targets {
            if t.name().starts_with("test")
                || t.is_benchmark()
                || t.name() == "main"
                || !t.validations().is_empty()
            {
                if let Some(f) = t.format() {
                    targets.push((t.clone(), vec![f.to_string()]))
                } else {
                    targets.push((t.clone(), formats.clone()))
                }
            }
        }

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
        })
    }

    /// Create a test plan for an error test file.
    ///
    /// Error tests use a single default target and validate against an
    /// `.expect` sidecar rather than in-file RESULT assertions.
    pub fn for_error_test(run_id: &str, filename: &Path, expectation: ErrorExpectation) -> Self {
        let title = filename
            .file_stem()
            .map(|os| os.to_string_lossy().into_owned())
            .unwrap_or_else(|| "untitled".to_string());

        TestPlan {
            run_id: run_id.to_string(),
            file: filename.to_path_buf(),
            title,
            targets: vec![(Target::default(), vec!["yaml".to_string()])],
            error_expectation: Some(expectation),
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
        }
    }
}

#[cfg(test)]
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

    #[test]
    fn test_validate_pass() {
        let exp = ErrorExpectation {
            exit: Some(1),
            stderr_pattern: Some("division by zero".to_string()),
        };
        assert!(exp.validate(1, "error: division by zero at line 5").is_ok());
    }

    #[test]
    fn test_validate_exit_mismatch() {
        let exp = ErrorExpectation {
            exit: Some(1),
            stderr_pattern: None,
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
}
