//! Analyse core unit into test plan

use crate::core::error::CoreError;
use crate::core::expr::*;
use crate::core::metadata::{ReadMetadata, TestHeaderMetadata};
use crate::core::target::Target;
use crate::core::unit::TranslationUnit;
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

        // run all targets begining with test
        let mut targets: Vec<(Target, Vec<String>)> = vec![];
        for t in &unit.targets {
            if t.name().starts_with("test") || t.name() == "main" || !t.validations().is_empty() {
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
