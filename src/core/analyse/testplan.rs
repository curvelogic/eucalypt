//! Analyse core unit into test plan

use crate::core::error::CoreError;
use crate::core::expr::*;
use crate::core::metadata::{ReadMetadata, TestHeaderMetadata};
use crate::core::target::Target;
use crate::core::unit::TranslationUnit;
use std::path::{Path, PathBuf};

/// A key under which we will expect :pass result and fail if they are
/// not found
#[derive(Debug, PartialEq, Clone)]
pub struct TestExpectation {
    /// Documentation for the test
    doc: String,
    /// Key path under which pass must be found
    path: Vec<String>,
}

impl TestExpectation {
    /// Test documentation
    pub fn doc(&self) -> &String {
        &self.doc
    }

    /// Path of test expectation
    pub fn path(&self) -> &Vec<String> {
        &self.path
    }
}

/// Plan for the tests to run against an eu file
///
/// A test plan consists of combinations of targets and formats to run
/// and a set of expectations to be validated against each run.
#[derive(Debug, PartialEq, Clone)]
pub struct TestPlan {
    /// Test filename
    file: PathBuf,
    /// Test title
    title: String,
    /// Header documentation
    doc: String,
    /// Test targets to run and formats for each
    targets: Vec<(Target, Vec<String>)>,
    /// Expectations
    expectations: Vec<TestExpectation>,
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

    /// Test documentation
    pub fn doc(&self) -> &String {
        &self.doc
    }

    /// Test targets to run
    pub fn targets(&self) -> &Vec<(Target, Vec<String>)> {
        &self.targets
    }

    /// Expectations to validate
    pub fn expectations(&self) -> &Vec<TestExpectation> {
        &self.expectations
    }

    pub fn result_directory(&self) -> PathBuf {
        let mut directory = PathBuf::from(self.file().parent().unwrap());
        directory.push(".result");
        directory
    }

    pub fn report_file_name(&self) -> PathBuf {
        let mut name = self.result_directory();
        name.push(self.file().file_name().unwrap());
        name.set_extension("report.yaml");
        name
    }

    pub fn prepare_directory(&self) -> Result<(), std::io::Error> {
        std::fs::create_dir_all(self.result_directory())
    }

    /// Analyse a translation unit to determine test plan
    pub fn analyse(filename: &Path, unit: &TranslationUnit) -> Result<Self, CoreError> {
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

        // run all targets begining with test
        let mut targets: Vec<(Target, Vec<String>)> = vec![];
        for t in &unit.targets {
            if t.name().starts_with("test") || t.name() == "main" {
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

        // For now, only one expectation, RESULT, but will allow
        // metadata to select
        let expectations = vec![TestExpectation {
            doc: "RESULT".to_string(),
            path: vec!["RESULT".to_string()],
        }];

        Ok(TestPlan {
            file: filename.to_path_buf(),
            title: header.title.unwrap_or_else(|| "".to_string()),
            doc: header.doc.unwrap_or_else(|| "".to_string()),
            targets,
            expectations,
        })
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::driver::source::SourceLoader;
    use crate::syntax::input::*;
    use std::collections::HashSet;
    use std::iter::FromIterator;

    pub fn source_to_test_plan(text: &str) -> TestPlan {
        let sample_input = Input::from(Locator::Literal(text.to_string()));
        let mut loader = SourceLoader::new(vec![]);
        loader.load(&sample_input).unwrap();
        loader.translate(&sample_input).unwrap();
        loader.merge_units(&[sample_input]).unwrap();
        TestPlan::analyse(&PathBuf::from("test"), loader.core()).unwrap()
    }

    #[test]
    pub fn test_simple() {
        let plan = source_to_test_plan("RESULT: :PASS");
        assert_eq!(plan.title(), "");
        assert_eq!(plan.doc(), "");
    }

    #[test]
    pub fn test_title() {
        let plan = source_to_test_plan(" { title: \"A test\" } RESULT: :PASS");
        assert_eq!(plan.title(), "A test");
    }

    #[test]
    pub fn test_doc() {
        let plan = source_to_test_plan(" { doc: \"This tests something.\" } RESULT: :PASS");
        assert_eq!(plan.doc(), "This tests something.");
    }

    // #[test] TODO: work out how this should work
    pub fn test_expectations() {
        let plan = source_to_test_plan(
            "
` \"Test sums\"
addition: passes(2 + 2 = 4)

` \"Test that might fail\"
subtraction: passes(4 - 1 = 2)
",
        );
        assert_eq!(
            plan.expectations(),
            &[
                TestExpectation {
                    doc: "Test sums".to_string(),
                    path: vec!["addition".to_string()]
                },
                TestExpectation {
                    doc: "Test that might fail".to_string(),
                    path: vec!["subtraction".to_string()]
                },
            ]
        );
    }

    #[test]
    pub fn test_targets() {
        let plan = source_to_test_plan(
            "
` { target: :test-addition }
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
    }
}
