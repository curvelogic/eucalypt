use crate::core::cook;
use crate::core::desugar::{Content, Desugarer};
use crate::core::error::CoreError;
use crate::core::expr::RcExpr;
use crate::core::inline::reduce;
use crate::core::inline::tag;
use crate::core::simplify::compress;
use crate::core::simplify::prune;
use crate::core::unit::TranslationUnit;
use crate::core::verify::content;
use crate::driver::error::EucalyptError;
use crate::driver::resources::Resources;
use crate::import::{csv, text, xml, yaml};
use crate::syntax::ast::*;
use crate::syntax::import::ImportGraph;
use crate::syntax::input::Input;
use crate::syntax::input::Locator;
use crate::syntax::parser;
use crate::{common::sourcemap::*, import::toml};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::emit;
use codespan_reporting::term::termcolor::{ColorChoice, NoColor, StandardStream};
use codespan_reporting::{diagnostic::Diagnostic, files::Files};
use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;
use std::{collections::HashMap, path::Path};

use super::io::create_io_pseudoblock;

/// A loader for source code that stores the bytes for error reporting
/// and manages the evolution of code through parse, translation to
/// core and manipulation as the final core expression syntax which
/// will be compiled / interpreted.
pub struct SourceLoader {
    /// Baked in resources (like the prelude)
    resources: Resources,
    /// Map Locators to usizes to key other stores
    locators: HashMap<Locator, usize>,
    /// Access to source file text
    files: SimpleFiles<String, String>,
    /// Parsed ASTs  pending core translation
    asts: HashMap<usize, Expression>,
    /// Parsed core exprs pending core translation (YAML, JSON etc.)
    cores: HashMap<Input, RcExpr>,
    /// Import analysis
    imports: ImportGraph,
    /// Core Units pending merge
    units: HashMap<Input, TranslationUnit>,
    /// Units once merged
    core: TranslationUnit,
    /// SourceMap for references to source locations across all files
    source_map: SourceMap,
    /// Paths to search for FS resources
    lib_path: Vec<PathBuf>,
}

impl Default for SourceLoader {
    fn default() -> Self {
        SourceLoader {
            resources: Resources::default(),
            locators: HashMap::new(),
            files: SimpleFiles::new(),
            asts: HashMap::new(),
            cores: HashMap::new(),
            imports: ImportGraph::default(),
            units: HashMap::new(),
            core: TranslationUnit::default(),
            source_map: SourceMap::new(),
            lib_path: Vec::new(),
        }
    }
}

impl SourceLoader {
    /// Create a fresh blank source loader
    pub fn new(lib_path: Vec<PathBuf>) -> Self {
        SourceLoader {
            resources: Resources::default(),
            locators: HashMap::new(),
            files: SimpleFiles::new(),
            asts: HashMap::new(),
            cores: HashMap::new(),
            imports: ImportGraph::default(),
            units: HashMap::new(),
            core: TranslationUnit::default(),
            source_map: SourceMap::new(),
            lib_path,
        }
    }

    /// Return refs to all parsed ASTs by locator,
    pub fn asts(&self) -> Vec<(&Locator, &Expression)> {
        self.locators
            .iter()
            .filter_map(|(k, v)| self.asts.get(v).map(|value| (k, value)))
            .collect()
    }

    /// Retrieve the parsed AST for the specified locator
    pub fn ast(&self, locator: &Locator) -> Option<&Expression> {
        self.locators.get(locator).and_then(|id| self.asts.get(id))
    }

    /// Load an input (and transitive imports)
    pub fn load(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        match input.format() {
            "yaml" | "json" => self.load_yaml(input), // direct to core
            "toml" => self.load_toml(input),
            "text" => self.load_text(input),
            "csv" => self.load_csv(input),
            "xml" => self.load_xml(input),
            "core" => self.load_core(input), // load pseudoblock
            _ => self.load_tree(input),      // to ASTs needing desugaring
        }
    }

    /// Load and parse import-graph of eucalypt files starting with the one
    /// specified by locator
    fn load_tree(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let locator = input.locator();
        let file_id = self.load_eucalypt(locator)?;
        let ast = self.asts.get(&file_id).unwrap();
        let inputs = self.imports.analyse_ast(input.clone(), ast)?;
        self.imports.check_for_cycles()?;

        for input in inputs {
            self.load(&input)?;
        }
        Ok(file_id)
    }

    /// Load and parse the source from a source specified by locator
    pub fn load_eucalypt(&mut self, locator: &Locator) -> Result<usize, EucalyptError> {
        // Load the text
        let id = self.load_source(locator)?;

        // We may still have an AST for this content (or we may have
        // consumed it in translation - it is rare enough we don't
        // mind the rework).
        if self.asts.contains_key(&id) {
            // we're done
            return Ok(id);
        }

        let ast = if matches!(locator, Locator::Cli(_)) {
            parser::parse_expression(&self.files, id)?
        } else {
            Expression::Block(Box::new(parser::parse_unit(&self.files, id)?))
        };
        self.asts.insert(id, ast);
        Ok(id)
    }

    /// Load YAML
    ///
    /// YAML is a special case because of available eu embeddings.
    fn load_yaml(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let file_id = self.load_source(input.locator())?;
        // the copy is necessary as yaml needs a mutable files db for
        // adding embeddings so cannot have immutable ref into it at
        // the same time
        let text = self.files.source(file_id)?.to_string();
        let core = yaml::read_yaml(&mut self.files, &mut self.source_map, file_id, &text)?;
        self.imports.add_leaf(input.clone())?;
        self.cores.insert(input.clone(), core);
        Ok(file_id)
    }

    /// Load TOML
    fn load_toml(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let file_id = self.load_source(input.locator())?;
        let text = self.files.source(file_id)?;
        let core = toml::read_toml(&mut self.source_map, file_id, text)?;
        self.imports.add_leaf(input.clone())?;
        self.cores.insert(input.clone(), core);
        Ok(file_id)
    }

    /// Load text as list of lines
    fn load_text(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let file_id = self.load_source(input.locator())?;
        let text = self.files.source(file_id)?;
        let core = text::read_text(&mut self.source_map, file_id, text)?;
        self.imports.add_leaf(input.clone())?;
        self.cores.insert(input.clone(), core);
        Ok(file_id)
    }

    /// Load text as list of lines
    fn load_csv(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let file_id = self.load_source(input.locator())?;
        let text = self.files.source(file_id)?;
        let core = csv::read_csv(&mut self.source_map, file_id, text)?;
        self.imports.add_leaf(input.clone())?;
        self.cores.insert(input.clone(), core);
        Ok(file_id)
    }

    fn load_xml(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let file_id = self.load_source(input.locator())?;
        let text = self.files.source(file_id)?;
        let core = xml::read_xml(&mut self.source_map, file_id, text)?;
        self.imports.add_leaf(input.clone())?;
        self.cores.insert(input.clone(), core);
        Ok(file_id)
    }

    /// Load __io pseudoblock as core
    fn load_core(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let file_id = self.load_source(input.locator())?;
        let core = create_io_pseudoblock();
        self.imports.add_leaf(input.clone())?;
        self.cores.insert(input.clone(), core);
        Ok(file_id)
    }

    /// Load the text from the specified location into our file store
    fn load_source(&mut self, locator: &Locator) -> Result<usize, EucalyptError> {
        if let Some(file_id) = self.locators.get(locator) {
            return Ok(*file_id);
        }

        match self.locators.get(locator) {
            Some(id) => Ok(*id),
            None => {
                // read text
                let source = match locator {
                    Locator::Fs(path) => self.read_fs_input(path)?,
                    Locator::Cli(text) => text.to_string(),
                    Locator::Literal(text) => text.to_string(),
                    Locator::Resource(name) => self
                        .resources
                        .get(name)
                        .ok_or_else(|| EucalyptError::UnknownResource(name.clone()))?
                        .clone(),
                    Locator::StdIn => self.read_stdin()?,
                    Locator::Pseudo(_) => "(no source)".to_string(),
                    _ => unimplemented!(),
                };

                // store text and map locator to fileid
                let id = self.files.add(locator.to_string(), source);
                self.locators.insert(locator.clone(), id);
                Ok(id)
            }
        }
    }

    /// Desugar the interrelated tree of ASTs identified by `input`
    /// into a core expression.
    pub fn translate(&mut self, input: &Input) -> Result<&TranslationUnit, EucalyptError> {
        // If we already have a translation, we're done.
        if self.units.contains_key(&input) {
            return Ok(self.units.get(&input).unwrap());
        }

        // Retrieve the ASTs and Core exprs contributing to the unit
        let inputs = self.imports.unit_inputs(input)?;
        let mut desugarables: HashMap<Input, Content> = HashMap::new();

        for input in inputs {
            if let Some(file_id) = self.locators.get(input.locator()) {
                if let Some(ast) = self.asts.get(file_id) {
                    desugarables.insert(input.clone(), Content::new(*file_id, ast));
                }

                if let Some(core) = self.cores.get(input) {
                    desugarables.insert(input.clone(), Content::new(*file_id, core));
                }
            }
        }

        // Desugar all the content starting from the input specified
        let mut desugarer = Desugarer::new(&desugarables, &mut self.source_map);
        let unit = desugarer.translate_unit(input)?;
        self.units.insert(input.clone(), unit);
        Ok(&self.units[input])
    }

    /// Set the body of the core expression to the specified target
    /// and return the target's format if specified
    pub fn retarget(&mut self, target: &str) -> Result<(), EucalyptError> {
        self.core = self.core.retarget(target)?;
        Ok(())
    }

    /// Cook the translated core to organise soup into proper
    /// application tree and to handle expression anaphora
    pub fn cook(&mut self) -> Result<(), EucalyptError> {
        self.core.expr = cook::cook(self.core.expr.clone())?;
        Ok(())
    }

    /// Run inliner
    pub fn inline(&mut self) -> Result<(), EucalyptError> {
        self.core.expr = tag::tag_combinators(&self.core.expr)?;
        self.core.expr = reduce::inline_pass(&self.core.expr)?;
        Ok(())
    }

    /// Prune definitions of any unused bindings prior to run
    pub fn eliminate(&mut self) -> Result<(), EucalyptError> {
        self.core.expr = prune::prune(&self.core.expr);
        self.core.expr = compress::compress(&self.core.expr)?;
        Ok(())
    }

    /// Final verification of core expression prior to run
    pub fn verify(&mut self) -> Result<Vec<CoreError>, EucalyptError> {
        content::verify(self.core.expr.clone()).map_err(EucalyptError::from)
    }

    /// The core syntax translation units currently in play
    pub fn units(&self) -> Vec<&TranslationUnit> {
        self.units.values().collect()
    }

    /// Merge all units into a single core expression
    pub fn merge_units(&mut self, inputs: &[Input]) -> Result<(), EucalyptError> {
        let units = inputs
            .iter()
            .map(|i| self.units.get(&i).expect("merging unknown unit"));
        self.core = TranslationUnit::merge(units.cloned())?;
        Ok(())
    }

    /// Retrieve the merged, complete, core expression
    pub fn core(&self) -> &TranslationUnit {
        &self.core
    }

    /// Declare source manipulation complete and take ownership of
    /// reporting resources
    pub fn complete(self) -> (SimpleFiles<String, String>, SourceMap, RcExpr) {
        (self.files, self.source_map, self.core.expr)
    }

    /// Read text from the filesystem, using lib-path to resolve the
    /// filesnames.
    fn read_fs_input(&mut self, path: &Path) -> Result<String, EucalyptError> {
        for libdir in &self.lib_path {
            let mut filename = libdir.to_path_buf();
            filename.push(path);
            if let Ok(text) = fs::read_to_string(filename) {
                return Ok(text);
            }
        }

        // lastly - absolute files are ok with empty lib path
        if let Ok(text) = fs::read_to_string(path) {
            return Ok(text);
        }

        Err(EucalyptError::FileCouldNotBeRead(
            path.to_string_lossy().to_string(),
        ))
    }

    /// Read source from stdin
    fn read_stdin(&mut self) -> Result<String, EucalyptError> {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        Ok(buffer)
    }

    /// Access to source map for creating diagnostics
    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    /// Print a diagnostic to stderr
    pub fn diagnose_to_stderr(&self, diag: &Diagnostic<usize>) {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = codespan_reporting::term::Config::default();
        emit(&mut writer.lock(), &config, &self.files, diag).unwrap();
    }

    pub fn diagnose_to_string(&self, diag: &Diagnostic<usize>) -> String {
        let mut s: Vec<u8> = Vec::new();
        {
            let mut writer = NoColor::new(&mut s);
            let config = codespan_reporting::term::Config::default();
            emit(&mut writer, &config, &self.files, diag).unwrap();
        }
        String::from_utf8_lossy(&s).into_owned()
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use std::fs;
    use std::path::Path;

    fn eu_paths() -> Vec<PathBuf> {
        let mut paths = vec![];

        if let Ok(entries) = fs::read_dir("./harness/test") {
            for f in entries.flatten() {
                let path = f.path();
                if path.extension().and_then(|s| s.to_str()) == Some("eu") {
                    let name = path.file_stem().and_then(|s| s.to_str()).unwrap();

                    // TODO: arbitrary size integers, text imports....
                    if name == "x013_arith_overflow" {
                        continue;
                    }
                    paths.push(f.path())
                }
            }
        } else {
            panic!("Failed to read test directory");
        }

        paths
    }

    #[test]
    fn test_parse_harness_tests() {
        let mut loader = SourceLoader::new(vec![
            std::env::current_dir().unwrap(),
            Path::new("./harness").to_path_buf(),
        ]);

        for f in eu_paths() {
            match loader.load_eucalypt(&Locator::Fs(f.clone())) {
                Err(e) => {
                    let diag = loader.diagnose_to_string(&e.to_diagnostic(loader.source_map()));
                    panic!("Failed to parse {:?}.\n{}", &f, diag);
                }
                Ok(_) => {}
            }
        }
    }

    #[test]
    fn test_desugar_tests() {
        let mut loader = SourceLoader::new(vec![Path::new("./harness/test").to_path_buf()]);

        for f in eu_paths() {
            let loc = Locator::Fs(f.clone());

            match loader.load_tree(&Input::from(loc.clone())) {
                Err(e) => {
                    let diag = loader.diagnose_to_string(&e.to_diagnostic(loader.source_map()));
                    panic!("Failed to parse {:?}.\n{}", &f, diag);
                }
                Ok(_) => {}
            }

            match loader.translate(&Input::from(loc.clone())) {
                Err(e) => {
                    let diag = loader.diagnose_to_string(&e.to_diagnostic(loader.source_map()));
                    panic!("Failed to desugar {:?}.\n{}", &f, diag);
                }
                Ok(_) => {}
            };
        }
    }

    #[test]
    fn test_load_prelude() {
        let mut loader = SourceLoader::default();
        let prelude = Locator::Resource("prelude".to_string());
        let _id = loader.load_tree(&Input::from(prelude.clone())).unwrap();
        let _ast = loader.ast(&prelude);
    }
}
