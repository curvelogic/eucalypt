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

use super::statistics::{Statistics, Timings};

pub fn test(opt: &EucalyptOptions) -> Result<i32, EucalyptError> {
    let cwd = Input::from(Locator::Fs(PathBuf::from(".")));
    let input = opt.inputs().last().unwrap_or(&cwd);
    let path = resolve_input(opt, input)?;

    if path.is_dir() {
        run_suite(&opt, &path)
    } else {
        run_test(&opt, &path)
    }
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

/// Run tests for a single file
pub fn run_test(opt: &EucalyptOptions, filename: &Path) -> Result<i32, EucalyptError> {
    print!("{}...", filename.file_stem().unwrap().to_string_lossy());

    let mut timings = Timings::default();

    let test_plan = {
        let mut loader = SourceLoader::new(opt.lib_path().to_vec());

        // desugar to parse out targets and docstrings
        prepare::prepare(opt, &mut loader, &mut timings)?;

        // analyse into test plan
        TestPlan::analyse(filename, loader.core())?
    };

    test_plan.prepare_directory()?;

    // TODO: select tester implementation
    let tester = InProcessTester {};

    let execution_opts = opt.clone().without_test_flag();
    let results = tester.run(&test_plan, &execution_opts)?;

    tester.validate(&test_plan, results)?;

    tester.report(&test_plan)
}

/// Discover all tests in a directory and run
pub fn run_suite(opt: &EucalyptOptions, dir: &Path) -> Result<i32, EucalyptError> {
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

    let mut exit: i32 = 0;

    for test in tests {
        let input = Input::from_str(&test.to_string_lossy())?;
        let test_opts = opt.clone().with_inputs(vec![input]);
        if run_test(&test_opts, &test)? > 0 {
            exit = 1;
        }
    }

    Ok(exit)
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
    /// Run each of the tests, returning results
    fn run(&self, plan: &TestPlan, opt: &EucalyptOptions)
        -> Result<Vec<TestResult>, EucalyptError>;

    /// Process plan and results to generate a report
    fn validate(&self, plan: &TestPlan, results: Vec<TestResult>) -> Result<(), EucalyptError>;

    /// Summarise the report to stdout and return an exit code
    fn report(&self, plan: &TestPlan) -> Result<i32, EucalyptError>;
}

pub struct InProcessTester {}

impl Tester for InProcessTester {
    /// Invoke the engine many times for each target / format
    /// combination and capture stdout / stderr for processing
    fn run(
        &self,
        plan: &TestPlan,
        opt: &EucalyptOptions,
    ) -> Result<Vec<TestResult>, EucalyptError> {
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
                });
            }
        }

        Ok(results)
    }

    /// Another invocation of eucalypt to gather the test output and
    /// validate that expectations have passed.
    fn validate(&self, plan: &TestPlan, results: Vec<TestResult>) -> Result<(), EucalyptError> {
        // prepare named inputs to the report for each test result
        // each must be provided as source and as the format
        // and index them
        let mut inputs = vec![];
        let mut validator = Vec::new();

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
                    format!(
                        "{{ name: \"{}\" doc: \"{}\" value: result __LOOKUPOR(:{}, :FAIL) }}",
                        path,
                        e.doc(),
                        path
                    )
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
                expectations.as_slice().join(",")
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

        // An invocation to run the validation and generate yaml report
        let report_file = plan.report_file_name();
        let report_opts = EucalyptOptions::default()
            .with_inputs(inputs)
            .with_output(report_file)
            .build();
        let mut loader = SourceLoader::new(vec![]);
        prepare::prepare(&report_opts, &mut loader, &mut Timings::default())?;
        eval::run(&report_opts, loader)?;

        Ok(())
    }

    /// Process the report file to establish a headline pass / file
    /// and report to stdout and return an exit code
    fn report(&self, plan: &TestPlan) -> Result<i32, EucalyptError> {
        let script = r#"tests.default-yaml.result __LOOKUPOR(:RESULT, :FAIL)"#;

        // Then finally, one more to check for success
        let report_input = Input::from(Locator::Fs(plan.report_file_name()));
        let check_opts = EucalyptOptions::default()
            .with_inputs(vec![report_input])
            .with_export_type("text".to_string())
            .to_evaluate(script)
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

        print!("{}", output);

        if output.trim().eq_ignore_ascii_case("pass") {
            Ok(0)
        } else {
            Ok(1)
        }
    }
}
