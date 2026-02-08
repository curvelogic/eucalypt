//! Running test scripts
use crate::core::analyse::testplan::{ErrorExpectation, TestPlan};
use crate::core::target::Target;
use crate::driver::error::EucalyptError;
use crate::driver::eval;
use crate::driver::options::EucalyptOptions;
use crate::driver::prepare;
use crate::driver::source::SourceLoader;
use crate::syntax::input::{Input, Locator};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::{fmt, fs};
use uuid::Uuid;
use webbrowser;

use super::statistics::{Statistics, Timings};

/// Main entry point for `eu -T` - analyse for test plan, execute and report
pub fn test(opt: &EucalyptOptions) -> Result<i32, EucalyptError> {
    let cwd = Input::from(Locator::Fs(PathBuf::from(".")));
    let input = opt.explicit_inputs().last().unwrap_or(&cwd);
    let path = resolve_input(opt, input)?;
    let run_id = Uuid::new_v4().hyphenated().to_string();

    let plans = if path.is_dir() {
        directory_plans(opt, &run_id, &path)?
    } else {
        vec![load_plan(opt, &run_id, &path)?]
    };

    run_plans(opt, plans.as_slice())
}

/// Entry point for error tests — creates a plan from the `.expect`
/// sidecar (if present) and runs through the standard pipeline.
///
/// Error tests bypass the normal metadata analysis since the test
/// file itself may have deliberate parse errors. If no `.expect`
/// sidecar exists, the test is treated as unvalidated and passes
/// without running.
pub fn error_test(opt: &EucalyptOptions) -> Result<i32, EucalyptError> {
    let cwd = Input::from(Locator::Fs(PathBuf::from(".")));
    let input = opt.explicit_inputs().last().unwrap_or(&cwd);
    let path = resolve_input(opt, input)?;
    let run_id = Uuid::new_v4().hyphenated().to_string();

    let expectation = match ErrorExpectation::load(&path) {
        Ok(Some(exp)) => exp,
        Ok(None) => {
            // No sidecar — unvalidated error test, skip gracefully
            return Ok(0);
        }
        Err(e) => {
            return Err(EucalyptError::FileCouldNotBeRead(format!(
                "failed to load .expect sidecar: {e}"
            )));
        }
    };

    let plan = TestPlan::for_error_test(&run_id, &path, expectation);
    run_plans(opt, &[plan])
}

/// Resolve the one and only input to determine if we are running a
/// single test or a suite.
pub fn resolve_input(opt: &EucalyptOptions, input: &Input) -> Result<PathBuf, EucalyptError> {
    if let Locator::Fs(path) = input.locator() {
        for libdir in opt.lib_path() {
            let mut filename = libdir.to_path_buf();
            filename.push(path);
            filename = filename.canonicalize()?;
            if filename.exists() {
                return Ok(filename);
            }
        }
        Err(EucalyptError::FileCouldNotBeRead(
            path.to_string_lossy().to_string(),
        ))
    } else {
        Err(EucalyptError::FileCouldNotBeRead(format!("{input}")))
    }
}

/// Load the test subject file to analyse and extra a test plan
fn load_plan(
    opt: &EucalyptOptions,
    run_id: &str,
    filename: &Path,
) -> Result<TestPlan, EucalyptError> {
    let mut timings = Timings::default();

    let mut analysis_lib_path = opt.lib_path().to_vec();
    analysis_lib_path.push(
        filename
            .parent()
            .expect("test file should have a parent directory")
            .to_path_buf(),
    );

    let mut loader = SourceLoader::new(analysis_lib_path);

    // desugar to parse out targets and docstrings
    prepare::prepare(opt, &mut loader, &mut timings)?;

    // analyse into test plan
    let test_plan = TestPlan::analyse(run_id, filename, loader.core())?;
    Ok(test_plan)
}

/// Run the specified plans and generate report
pub fn run_plans(opt: &EucalyptOptions, tests: &[TestPlan]) -> Result<i32, EucalyptError> {
    let tester = InProcessTester {};
    let mut exit: i32 = 0;

    for test in tests {
        let mut lib_path = opt.lib_path().to_vec();
        lib_path.push(test.test_directory().to_path_buf());

        let input = Input::from_str(&test.file().to_string_lossy())?;
        let test_opts = opt
            .clone()
            .with_explicit_inputs(vec![input])
            .with_lib_path(lib_path.clone());

        test.prepare_directory()?;

        print!("{}...", test.title());

        let execution_opts = test_opts.clone().without_test_flag();
        tester.run(test, &execution_opts)?;
        tester.validate(test)?;
        let summary = tester.summary(test)?.to_string();
        print!("{summary}");

        if !summary.starts_with("PASS") {
            exit = 1;
        }
    }

    let report = tester.report(tests)?;
    println!("Report generated at {}", report.to_string_lossy());

    if opt.open_browser() {
        let _ = report.to_str().and_then(|s| webbrowser::open(s).ok());
    }

    Ok(exit)
}

/// Discover all tests in a directory and run, setting the directory in
/// the library path for easy resolution of imports
fn directory_plans(
    opt: &EucalyptOptions,
    run_id: &str,
    dir: &Path,
) -> Result<Vec<TestPlan>, EucalyptError> {
    println!("Gathering tests in {}", dir.display());
    let mut tests: Vec<_> = fs::read_dir(dir)?
        .filter_map(Result::ok)
        .filter(|f| {
            matches!(
                f.path().extension().and_then(|s| s.to_str()),
                Some("eu") | Some("yaml") | Some("json") | Some("toml") | Some("edn")
            )
        })
        .map(|f| f.path())
        .collect();

    tests.sort();

    tests
        .iter()
        .map(|t| {
            load_plan(
                &opt.clone()
                    .with_explicit_inputs(vec![Input::from(Locator::from(t.as_path()))])
                    .build(),
                run_id,
                t,
            )
        })
        .collect()
}

/// A test result
#[derive(Debug)]
pub struct TestResult<'t> {
    /// Target used
    pub target: &'t Target,
    /// Format used
    pub format: String,
    /// Exit code (zero success - None incomplete)
    pub exit_code: Option<u8>,
    /// Captured stdout
    pub stdout: String,
    /// Captured stderr
    pub stderr: String,
    /// Execution statistics
    pub statistics: Statistics,
}

impl fmt::Display for TestResult<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Target: {}", self.target.name())?;
        writeln!(f, "Format: {}", self.format)?;
        writeln!(
            f,
            "Exit: {}",
            match self.exit_code {
                Some(n) => format!("{n}"),
                None => "incomplete".to_string(),
            }
        )?;
        writeln!(f, "--STDOUT--\n{}", self.stdout)?;
        writeln!(f, "--STDERR--\n{}", self.stderr)
    }
}

/// A tester runs all the planned tests, gathers output and reports
pub trait Tester {
    /// Run all the tests in the plan and create an evidence.yaml output
    fn run(&self, plan: &TestPlan, opt: &EucalyptOptions) -> Result<(), EucalyptError>;

    /// Process the evidence to create a result.yaml output
    fn validate(&self, plan: &TestPlan) -> Result<(), EucalyptError>;

    /// Read the result summary (PASS/FAIL) from the result.yaml output
    fn summary(&self, plan: &TestPlan) -> Result<String, EucalyptError>;

    /// Aggregate result.yamls and generate output report
    fn report(&self, plan: &[TestPlan]) -> Result<PathBuf, EucalyptError>;
}

pub struct InProcessTester {}

/// Quote a string for embedding in eu source code.
///
/// Escapes double quotes and curly braces (which would otherwise
/// trigger string interpolation).
fn quote<T: AsRef<str>>(text: T) -> String {
    let escaped = text
        .as_ref()
        .replace('{', "{{")
        .replace('}', "}}")
        .replace('\"', "\\\"");
    format!("\"{escaped}\"")
}

/// Convert stats to a core expression for output
fn stats_to_eu(stats: &Statistics) -> String {
    format!(
        "{{ ticks: {} allocs: {} max-stack: {} }}",
        stats.ticks(),
        stats.allocs(),
        stats.max_stack()
    )
}

impl InProcessTester {
    /// Takes plan and test output and creates an evidence.yaml
    ///
    /// The evidence does not as yet constitute a pass or a fail as
    /// assertions might have been specified which validate the
    /// outputs recorded in evidence.yaml - e.g. the stderr or stdout.
    fn create_evidence_yaml(
        &self,
        plan: &TestPlan,
        results: Vec<TestResult<'_>>,
    ) -> Result<(), EucalyptError> {
        let mut inputs = vec![];
        let mut evidence_script = Vec::new();

        writeln!(&mut evidence_script, "title: {}", quote(plan.title()))?;
        writeln!(
            &mut evidence_script,
            "filename: {}",
            quote(plan.file().to_string_lossy())
        )?;
        writeln!(&mut evidence_script, "tests: {{")?;
        for result in results {
            let target_name = if result.target.name().is_empty() {
                "default"
            } else {
                result.target.name()
            };

            let format = result.format;
            // The tests output parsed as the output format end
            // embedded for testing expectations against
            let output = Input::new(
                Locator::Literal(result.stdout.clone()),
                Some(format!("{}-{}-result", &target_name, &format)),
                format.clone(),
            );
            // Literal stdout
            let stdout = Input::new(
                Locator::Literal(result.stdout.clone()),
                Some(format!("{}-{}-stdout-text", &target_name, &format)),
                "text",
            );
            // Literal stderr
            let stderr = Input::new(
                Locator::Literal(result.stderr),
                Some(format!("{}-{}-stderr-text", &target_name, &format)),
                "text",
            );

            let mut expectations: Vec<String> = result
                .target
                .validations()
                .iter()
                .map(|v| format!("{{ name: \"{v}\" function-key: \"{v}\" }}"))
                .collect();

            if expectations.is_empty() {
                if result.target.is_benchmark() {
                    // Bench targets without explicit validations are
                    // report-only: use an expectation that always passes
                    // so they don't affect the overall pass/fail summary.
                    expectations
                        .push("{ name: \"bench\" function-key: \"bench-expectation\"}".to_string());
                } else {
                    expectations.push(
                        "{ name: \"default\" function-key: \"default-expectation\"}".to_string(),
                    );
                }
            }

            let benchmark_field = if result.target.is_benchmark() {
                "\n    benchmark: true"
            } else {
                ""
            };

            let test_template = format!(
                r#"
  '{}-{}': {{
    exit: {}
    stdout: '{}'
    stderr: '{}'
    result: {}
    expectations: [{}]
    stats: {}{benchmark_field}
  }}"#,
                &target_name,
                &format,
                result.exit_code.unwrap_or(1),
                stdout.name().as_ref().expect("stdout temp file has a name"),
                stderr.name().as_ref().expect("stderr temp file has a name"),
                if result.exit_code.is_some() {
                    format!(
                        "'{}'",
                        output.name().as_ref().expect("output temp file has a name")
                    )
                } else {
                    "{ RESULT: :FAIL }".to_string()
                },
                expectations.as_slice().join(","),
                stats_to_eu(&result.statistics)
            );

            // Gather all the outputs together
            inputs.push(stdout);
            inputs.push(stderr);
            if result.exit_code.is_some() {
                inputs.push(output);
            }

            writeln!(&mut evidence_script, "{test_template}")?;
        }
        writeln!(&mut evidence_script, "}}\n")?;

        let evidence_script_text = std::str::from_utf8(&evidence_script)
            .expect("evidence script should be valid UTF-8")
            .to_string();
        let evidence_input = Input::new(Locator::Literal(evidence_script_text), None, "eu");
        inputs.push(evidence_input);

        let evidence_file = plan.evidence_file_name();
        let evidence_opts = EucalyptOptions::default()
            .with_explicit_inputs(inputs)
            .with_output(evidence_file)
            .build();
        let mut loader = SourceLoader::new(vec![]);
        prepare::prepare(&evidence_opts, &mut loader, &mut Timings::default())?;
        eval::run(&evidence_opts, loader)?;

        Ok(())
    }
}

impl InProcessTester {
    /// Validate an error test by checking exit code and stderr against
    /// the `.expect` sidecar expectations.
    ///
    /// Reads the evidence.yaml to extract actual exit code and stderr,
    /// then writes a result YAML with overall PASS/FAIL.
    fn validate_error_test(&self, plan: &TestPlan) -> Result<(), EucalyptError> {
        let expectation = plan
            .error_expectation()
            .expect("validate_error_test called on non-error test");

        // Read the evidence YAML to get actual exit code and stderr
        let evidence_text = fs::read_to_string(plan.evidence_file_name())?;
        let (actual_exit, actual_stderr) = Self::extract_error_evidence(&evidence_text);

        let (overall, reason) = match expectation.validate(actual_exit, &actual_stderr) {
            Ok(()) => ("PASS".to_string(), String::new()),
            Err(reason) => ("FAIL".to_string(), reason),
        };

        // Write a result YAML compatible with the summary reader and
        // report generator. The report generator (lib/test.eu) expects
        // each test entry to have stdout, stderr, validation, and stats
        // keys.
        let escaped_reason = reason.replace('"', "\\\"").replace('\n', "\\n");
        let escaped_stderr = actual_stderr.replace('"', "\\\"").replace('\n', "\\n");
        let result_yaml = format!(
            r#"overall: {overall}
title: {title}
tests:
  error-test:
    exit: {actual_exit}
    stdout: []
    stderr:
      - "{escaped_stderr}"
    stats:
      ticks: 0
      allocs: 0
      max-stack: 0
    validation:
      - name: error-expectation
        result: {overall}
        reason: "{escaped_reason}"
"#,
            title = plan.title(),
        );
        fs::write(plan.result_file_name(), result_yaml)?;

        Ok(())
    }

    /// Print benchmark stats to stderr for a benchmark target.
    fn report_bench_stats(target_name: &str, format: &str, stats: &Statistics) {
        eprintln!("\nBENCH {target_name} ({format})");
        eprintln!("  ticks:     {:>10}", stats.ticks());
        eprintln!("  allocs:    {:>10}", stats.allocs());
        eprintln!("  max-stack: {:>10}", stats.max_stack());
    }

    /// Extract exit code and stderr from evidence YAML text.
    ///
    /// The evidence format has a `tests` block with entries like:
    /// ```yaml
    /// tests:
    ///   default-yaml:
    ///     exit: 1
    ///     stderr: ...
    /// ```
    /// We extract the first test entry's exit and stderr values.
    fn extract_error_evidence(evidence_text: &str) -> (i32, String) {
        let mut exit_code = 1i32;
        let mut stderr_lines = Vec::new();
        let mut in_stderr = false;

        for line in evidence_text.lines() {
            let trimmed = line.trim();

            // Look for "exit: N" lines
            if let Some(val) = trimmed.strip_prefix("exit:") {
                if let Ok(code) = val.trim().parse::<i32>() {
                    exit_code = code;
                }
            }

            // stderr in evidence.yaml is stored as a list of strings
            if trimmed == "stderr:" {
                in_stderr = true;
                continue;
            }

            if in_stderr {
                if let Some(item) = trimmed.strip_prefix("- ") {
                    // Strip surrounding quotes
                    let item = item
                        .strip_prefix('"')
                        .and_then(|v| v.strip_suffix('"'))
                        .or_else(|| item.strip_prefix('\'').and_then(|v| v.strip_suffix('\'')))
                        .unwrap_or(item);
                    stderr_lines.push(item.to_string());
                } else if !trimmed.starts_with('-') {
                    in_stderr = false;
                }
            }
        }

        (exit_code, stderr_lines.join("\n"))
    }
}

impl Tester for InProcessTester {
    /// Invoke the engine for each target / format / invocation and
    /// capture outputs in evidence.yaml
    fn run(&self, plan: &TestPlan, opt: &EucalyptOptions) -> Result<(), EucalyptError> {
        let mut results = vec![];

        for (t, fs) in plan.targets() {
            for f in fs {
                let test_opts = opt
                    .clone()
                    .for_test(t.name().to_string(), f.to_string())
                    .build();

                let mut loader = SourceLoader::new(test_opts.lib_path().to_vec());
                let mut statistics = Statistics::default();

                // For error tests, catch preparation failures gracefully
                // since the error itself is what we're testing
                let prep_result =
                    prepare::prepare(&test_opts, &mut loader, statistics.timings_mut());

                if let Err(e) = prep_result {
                    if plan.is_error_test() {
                        results.push(TestResult {
                            target: t,
                            format: f.to_string(),
                            exit_code: Some(1),
                            stdout: String::new(),
                            stderr: format!("{e}"),
                            statistics,
                        });
                        continue;
                    }
                    return Err(e);
                }

                let mut outbuf = Vec::new();
                let mut errbuf = Vec::new();

                let exit_code = {
                    let out = Box::new(&mut outbuf);
                    let err = Box::new(&mut errbuf);
                    let mut executor = eval::Executor::from(loader);
                    executor.capture_output(out, err);
                    executor.execute(&test_opts, &mut statistics, f.clone())
                };

                let result = TestResult {
                    target: t,
                    format: f.to_string(),
                    exit_code: exit_code.unwrap_or(None),
                    stdout: std::str::from_utf8(outbuf.as_slice())
                        .expect("stdout should be valid UTF-8")
                        .to_string(),
                    stderr: std::str::from_utf8(errbuf.as_slice())
                        .expect("stderr should be valid UTF-8")
                        .to_string(),
                    statistics,
                };

                if t.is_benchmark() {
                    Self::report_bench_stats(t.name(), f, &result.statistics);
                }

                results.push(result);
            }
        }

        self.create_evidence_yaml(plan, results)
    }

    /// Validates the evidence.yaml which was written during the run stage
    /// and writes the results file.
    fn validate(&self, plan: &TestPlan) -> Result<(), EucalyptError> {
        if plan.is_error_test() {
            return self.validate_error_test(plan);
        }

        // Adding test subject back in, we may need to set the lib
        // path for imports again
        let lib_path = vec![plan
            .file()
            .parent()
            .expect("test plan file should have a parent directory")
            .to_path_buf()];

        let validate_opts = EucalyptOptions::default()
            .with_explicit_inputs(vec![
                Input::new(
                    Locator::Fs(plan.evidence_file_name()),
                    Some("evidence".to_string()),
                    "yaml",
                ),
                // Re-import test subject so validation expressions
                // can reference it. Error tests are handled separately
                // (validate_error_test) so parse/compile failures here
                // propagate as test failures, which is correct.
                Input::from(Locator::Fs(plan.file().to_path_buf())).with_name("subject"),
                Input::from(Locator::Resource("test".to_string())),
            ])
            .with_lib_path(lib_path.clone())
            .with_export_type("yaml".to_string())
            .with_target(Some("verify".to_string()))
            .with_output(plan.result_file_name())
            .build();
        let mut check_loader = SourceLoader::new(lib_path);

        prepare::prepare(&validate_opts, &mut check_loader, &mut Timings::default())?;
        eval::run(&validate_opts, check_loader)?;
        Ok(())
    }

    /// Read the result YAML to return a PASS / FAIL summary for
    /// stdout reporting
    fn summary(&self, plan: &TestPlan) -> Result<String, EucalyptError> {
        // Then finally, one more to check for success
        let report_input = Input::from(Locator::Fs(plan.result_file_name()));
        let check_opts = EucalyptOptions::default()
            .with_explicit_inputs(vec![report_input])
            .with_export_type("text".to_string())
            .to_evaluate("overall")
            .build();
        let mut check_loader = SourceLoader::new(vec![]);
        prepare::prepare(&check_opts, &mut check_loader, &mut Timings::default())?;

        let mut outbuf = Vec::new();
        let mut errbuf = Vec::new();

        {
            let out = Box::new(&mut outbuf);
            let err = Box::new(&mut errbuf);
            let mut executor = eval::Executor::from(check_loader);
            let mut stats = Statistics::default();
            executor.capture_output(out, err);
            executor
                .execute(&check_opts, &mut stats, "text".to_string())
                .map_err(|e| EucalyptError::Execution(Box::new(e)))?;
        }
        let output = std::str::from_utf8(outbuf.as_slice())
            .expect("validation output should be valid UTF-8")
            .to_string();
        Ok(output)
    }

    /// Generate a combined HTML test report for all specified tests
    ///
    /// Return path to generated report
    fn report(&self, plans: &[TestPlan]) -> Result<PathBuf, EucalyptError> {
        let test_dir = plans
            .first()
            .expect("report requires at least one test plan")
            .result_directory();

        // first combine all the result.yamls into one
        let inputs: Vec<Input> = plans
            .iter()
            .map(|p| Input::from(Locator::from(p.result_file_name().as_path())))
            .collect();
        let mut result_file = test_dir.clone();
        result_file.push("results.yaml");
        let result_opts = EucalyptOptions::default()
            .with_explicit_inputs(inputs)
            .with_collect_as(Some("results".to_string()))
            .with_output(result_file.clone())
            .build();
        let mut loader = SourceLoader::new(vec![]);
        prepare::prepare(&result_opts, &mut loader, &mut Timings::default())?;
        eval::run(&result_opts, loader)?;

        // then process using test.eu:generate-report
        let mut report_file = test_dir;
        report_file.push("report.html");
        let report_opts = EucalyptOptions::default()
            .with_explicit_inputs(vec![
                Input::new(Locator::Fs(result_file), None, "yaml"),
                Input::from(Locator::Resource("test".to_string())),
            ])
            .with_target(Some("generate-report".to_string()))
            .with_output(report_file.clone())
            .build();

        let mut loader = SourceLoader::new(vec![]);
        prepare::prepare(&report_opts, &mut loader, &mut Timings::default())?;
        eval::run(&report_opts, loader)?;

        Ok(report_file)
    }
}
