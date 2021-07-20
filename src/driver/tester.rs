//! Running test scripts
use crate::core::analyse::testplan::TestPlan;
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

use super::statistics::{Statistics, Timings};

/// Main entry point for `eu -T` - analyse for test plan, execute and report
pub fn test(opt: &EucalyptOptions) -> Result<i32, EucalyptError> {
    let cwd = Input::from(Locator::Fs(PathBuf::from(".")));
    let input = opt.explicit_inputs().last().unwrap_or(&cwd);
    let path = resolve_input(opt, input)?;
    let run_id = Uuid::new_v4().to_hyphenated().to_string();

    let plans = if path.is_dir() {
        directory_plans(&opt, &run_id, &path)?
    } else {
        vec![load_plan(&opt, &run_id, &path)?]
    };

    run_plans(&opt, plans.as_slice())
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
        Err(EucalyptError::FileCouldNotBeRead(format!("{}", input)))
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
    analysis_lib_path.push(filename.parent().unwrap().to_path_buf());

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
        tester.run(&test, &execution_opts)?;
        let ret = tester.validate(&test)?;
        print!("{}", tester.summary(&test)?);

        if ret > 0 {
            exit = 1;
        }
    }

    let report = tester.report(tests)?;
    println!("Report generated at {}", report.to_string_lossy());

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
                Some("eu") | Some("yaml") | Some("json")
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
pub struct TestResult {
    /// Target used
    pub target: String,
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

impl fmt::Display for TestResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Target: {}", self.target)?;
        writeln!(f, "Format: {}", self.format)?;
        writeln!(
            f,
            "Exit: {}",
            match self.exit_code {
                Some(n) => format!("{}", n),
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
    fn validate(&self, plan: &TestPlan) -> Result<i32, EucalyptError>;

    /// Read the result summary (PASS/FAIL) from the result.yaml output
    fn summary(&self, plan: &TestPlan) -> Result<String, EucalyptError>;

    /// Aggregate result.yamls and generate output report
    fn report(&self, plan: &[TestPlan]) -> Result<PathBuf, EucalyptError>;
}

pub struct InProcessTester {}

/// TODO: eu escaping
fn quote<T: AsRef<str>>(text: T) -> String {
    format!("\"{}\"", text.as_ref().replace("\"", "\\\""))
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
        results: Vec<TestResult>,
    ) -> Result<(), EucalyptError> {
        let mut inputs = vec![];
        let mut validator = Vec::new();

        writeln!(&mut validator, "title: {}", quote(plan.title()))?;
        writeln!(
            &mut validator,
            "filename: {}",
            quote(plan.file().to_string_lossy())
        )?;
        writeln!(&mut validator, "tests: {{")?;
        for result in results {
            let target = if result.target.is_empty() {
                "default".to_string()
            } else {
                result.target
            };
            let format = result.format;
            // The tests output parsed as the output format end
            // embedded for testing expectations against
            let output = Input::new(
                Locator::Literal(result.stdout.clone()),
                Some(format!("{}-{}-result", &target, &format)),
                format.clone(),
            );
            // Literal stdout
            let stdout = Input::new(
                Locator::Literal(result.stdout.clone()),
                Some(format!("{}-{}-stdout-text", &target, &format)),
                "text".to_string(),
            );
            // Literal stderr
            let stderr = Input::new(
                Locator::Literal(result.stderr),
                Some(format!("{}-{}-stderr-text", &target, &format)),
                "text".to_string(),
            );

            let expectations: Vec<String> = plan
                .expectations()
                .iter()
                .map(|e| {
                    let path = e.path().as_slice().join(".");
                    format!("{{ name: \"{}\" value: \"{}\" }}", e.doc(), path)
                })
                .collect();

            let test_template = format!(
                r#"
  {}-{}: {{
    exit: {}
    stdout: {}
    stderr: {}
    result: {}
    expectations: [{}]
    stats: {}
  }}"#,
                &target,
                &format,
                result.exit_code.unwrap_or(1),
                stdout.name().as_ref().unwrap(),
                stderr.name().as_ref().unwrap(),
                if result.exit_code.is_some() {
                    output.name().as_ref().unwrap()
                } else {
                    "{ RESULT: :FAIL }"
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

            writeln!(&mut validator, "{}", test_template)?;
        }
        writeln!(&mut validator, "}}\n")?;

        let validator_text = std::str::from_utf8(&validator).unwrap().to_string();
        let validator_input = Input::new(Locator::Literal(validator_text), None, "eu".to_string());
        inputs.push(validator_input);

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

                prepare::prepare(&test_opts, &mut loader, statistics.timings_mut())?;

                let mut outbuf = Vec::new();
                let mut errbuf = Vec::new();

                let exit_code = {
                    let out = Box::new(&mut outbuf);
                    let err = Box::new(&mut errbuf);
                    let mut executor = eval::Executor::from(loader);
                    executor.capture_output(out, err);
                    executor.execute(&test_opts, &mut statistics, f.clone())
                };

                results.push(TestResult {
                    target: t.name().to_string(),
                    format: f.to_string(),
                    exit_code: exit_code.unwrap_or(None),
                    stdout: std::str::from_utf8(outbuf.as_slice()).unwrap().to_string(),
                    stderr: std::str::from_utf8(errbuf.as_slice()).unwrap().to_string(),
                    statistics,
                });
            }
        }

        self.create_evidence_yaml(plan, results)
    }

    /// Validates the evidence.yaml which was written during the run stage
    ///
    fn validate(&self, plan: &TestPlan) -> Result<i32, EucalyptError> {
        let validate_opts = EucalyptOptions::default()
            .with_explicit_inputs(vec![
                Input::new(
                    Locator::Fs(plan.evidence_file_name()),
                    Some("evidence".to_string()),
                    "yaml",
                ),
                Input::from(Locator::Resource("verify".to_string())),
            ])
            .with_export_type("yaml".to_string())
            .with_output(plan.result_file_name())
            .build();
        let mut check_loader = SourceLoader::new(vec![]);

        prepare::prepare(&validate_opts, &mut check_loader, &mut Timings::default())?;
        eval::run(&validate_opts, check_loader)?;

        Ok(0)
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
            executor.execute(&check_opts, &mut stats, "text".to_string())?;
        }
        let output = std::str::from_utf8(outbuf.as_slice()).unwrap().to_string();
        Ok(output)
    }

    /// Generate a combined HTML test report for all specified tests
    ///
    /// Return path to generated report
    fn report(&self, plans: &[TestPlan]) -> Result<PathBuf, EucalyptError> {
        let test_dir = plans.first().unwrap().result_directory();

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

        // then process using report.eu
        let mut report_file = test_dir;
        report_file.push("report.html");
        let report_opts = EucalyptOptions::default()
            .with_explicit_inputs(vec![
                Input::new(Locator::Fs(result_file), None, "yaml"),
                Input::from(Locator::Resource("report".to_string())),
            ])
            .with_output(report_file.clone())
            .build();

        let mut loader = SourceLoader::new(vec![]);
        prepare::prepare(&report_opts, &mut loader, &mut Timings::default())?;
        eval::run(&report_opts, loader)?;

        Ok(report_file)
    }
}
