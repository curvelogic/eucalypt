use crate::common::sourcemap::*;
use crate::core::cook;
use crate::core::desugar::desugarable::Desugarable;
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
use crate::import::read_to_core;
use crate::syntax::rowan::ast::{Soup, Unit};

/// Enum to hold either a Unit (for files) or Soup (for CLI expressions)
#[derive(Debug)]
pub enum ParsedAst {
    Unit(Unit),
    Soup(Soup),
}

impl Desugarable for ParsedAst {
    fn desugar(
        &self,
        desugarer: &mut crate::core::desugar::desugarer::Desugarer,
    ) -> Result<crate::core::expr::RcExpr, crate::core::error::CoreError> {
        match self {
            ParsedAst::Unit(unit) => unit.desugar(desugarer),
            ParsedAst::Soup(soup) => soup.desugar(desugarer),
        }
    }
}
use crate::syntax::import::ImportGraph;
use crate::syntax::input::Input;
use crate::syntax::input::Locator;
use crate::syntax::parser;
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::emit;
use codespan_reporting::term::termcolor::{ColorChoice, NoColor, StandardStream};
use std::io::{self, Read};
use std::path::PathBuf;
use std::{collections::HashMap, path::Path};
use std::{fs, iter};

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
    asts: HashMap<usize, ParsedAst>,
    /// Parsed core exprs pending core translation (YAML, JSON etc.)
    cores: HashMap<Input, RcExpr>,
    /// Import analysis
    imports: ImportGraph,
    /// Core Units pending merge
    translation_units: HashMap<Input, TranslationUnit>,
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
            translation_units: HashMap::new(),
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
            translation_units: HashMap::new(),
            core: TranslationUnit::default(),
            source_map: SourceMap::new(),
            lib_path,
        }
    }

    /// Return refs to all parsed ASTs by locator,
    pub fn asts(&self) -> Vec<(&Locator, &ParsedAst)> {
        self.locators
            .iter()
            .filter_map(|(k, v)| self.asts.get(v).map(|value| (k, value)))
            .collect()
    }

    /// Retrieve the parsed AST for the specified locator
    pub fn ast(&self, locator: &Locator) -> Option<&ParsedAst> {
        self.locators.get(locator).and_then(|id| self.asts.get(id))
    }

    /// Load an input (and transitive imports)
    pub fn load(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let fmt = input.format();

        if fmt == "eu" {
            self.load_tree(input)
        } else if fmt == "core" {
            self.load_core(input)
        } else {
            self.load_simple(input)
        }
    }

    /// Loads from a data format that does not support imports
    fn load_simple(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let file_id = self.load_source(input.locator())?;
        let core = read_to_core(
            input.format(),
            &mut self.files,
            &mut self.source_map,
            file_id,
        )
        .map_err(|e| EucalyptError::Source(Box::new(e)))?;
        self.imports.add_leaf(input.clone())?;
        self.cores.insert(input.clone(), core);
        Ok(file_id)
    }

    /// Load and parse import-graph of eucalypt files starting with the one
    /// specified by locator
    fn load_tree(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let locator = input.locator();
        let file_id = self.load_eucalypt(locator)?;

        // Implement import analysis for Rowan AST
        let ast = self.asts.get(&file_id).unwrap();
        let inputs = self.imports.analyse_rowan_ast(input.clone(), ast)?;
        self.imports.check_for_cycles()?;

        for import_input in inputs {
            self.load(&import_input)?;
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
            let soup = parser::parse_expression(&self.files, id)?;
            ParsedAst::Soup(soup)
        } else {
            let unit = parser::parse_unit(&self.files, id)?;
            ParsedAst::Unit(unit)
        };
        self.asts.insert(id, ast);
        Ok(id)
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
        if self.translation_units.contains_key(input) {
            return Ok(self.translation_units.get(input).unwrap());
        }

        // Check if this is a core input (already desugared)
        if let Some(core_expr) = self.cores.get(input) {
            // Apply name if specified (same as desugarer does)
            let expr = if let Some(name) = input.name() {
                core_expr.apply_name(Smid::default(), name)
            } else {
                core_expr.clone()
            };
            
            // Create a translation unit directly from the core expression
            let unit = TranslationUnit {
                expr,
                targets: std::collections::HashSet::new(),
                docs: Vec::new(),
            };
            self.translation_units.insert(input.clone(), unit);
            return Ok(&self.translation_units[input]);
        }

        // Retrieve the ASTs contributing to the unit
        let inputs = self.imports.unit_inputs(input)?;
        let mut desugarables: HashMap<Input, Content> = HashMap::new();

        for input in inputs {
            if let Some(file_id) = self.locators.get(input.locator()) {
                if let Some(ast) = self.asts.get(file_id) {
                    desugarables.insert(input.clone(), Content::new(*file_id, ast));
                } else if let Some(core_expr) = self.cores.get(input) {
                    // Include core expressions (from non-eucalypt file formats) in desugarables
                    desugarables.insert(input.clone(), Content::new(*file_id, core_expr));
                }
            }
        }

        // Desugar all the content starting from the input specified
        let mut desugarer = Desugarer::new(&desugarables, &mut self.source_map);
        let unit = desugarer.translate_unit(input)?;
        self.translation_units.insert(input.clone(), unit);
        Ok(&self.translation_units[input])
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
        self.translation_units.values().collect()
    }

    /// Merge all units into a single core expression
    pub fn merge_units(&mut self, inputs: &[Input]) -> Result<(), EucalyptError> {
        let units = inputs
            .iter()
            .map(|i| self.translation_units.get(i).expect("merging unknown unit"));
        self.core = TranslationUnit::merge(units.cloned())?;
        Ok(())
    }

    /// Collect all explicit inputs into a named collection then merge
    pub fn collect_and_merge_units(
        &mut self,
        collection_name: &str,
        name_inputs: bool,
        prologue_inputs: &[Input],
        explicit_inputs: &[Input],
        epilogue_inputs: &[Input],
    ) -> Result<(), EucalyptError> {
        let prologue_units = prologue_inputs
            .iter()
            .map(|i| self.translation_units.get(i).expect("merging unknown unit"));
        let explicit_units = explicit_inputs
            .iter()
            .map(|i| self.translation_units.get(i).expect("merging unknown unit"));
        let epilogue_units = epilogue_inputs
            .iter()
            .map(|i| self.translation_units.get(i).expect("merging unknown unit"));

        let mut collection_unit = if name_inputs {
            let names: Vec<String> = explicit_inputs
                .iter()
                .map(|i| i.name().clone().unwrap_or(format!("{}", i.locator())))
                .collect();
            TranslationUnit::blockify(&names, explicit_units)?
        } else {
            TranslationUnit::listify(explicit_units)?
        };

        collection_unit.expr = collection_unit
            .expr
            .apply_name(Smid::default(), collection_name);

        self.core = TranslationUnit::merge(
            prologue_units
                .chain(iter::once(&collection_unit))
                .chain(epilogue_units)
                .cloned(),
        )?;

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
            if let Err(e) = loader.load_eucalypt(&Locator::Fs(f.clone())) {
                let diag = loader.diagnose_to_string(&e.to_diagnostic(loader.source_map()));
                panic!("Failed to parse {:?}.\n{}", &f, diag);
            }
        }
    }

    #[test]
    fn test_desugar_tests() {
        let mut loader = SourceLoader::new(vec![Path::new("./harness/test").to_path_buf()]);

        for f in eu_paths() {
            let loc = Locator::Fs(f.clone());

            if let Err(e) = loader.load_tree(&Input::from(loc.clone())) {
                let diag = loader.diagnose_to_string(&e.to_diagnostic(loader.source_map()));
                panic!("Failed to parse {:?}.\n{}", &f, diag);
            }

            if let Err(e) = loader.translate(&Input::from(loc.clone())) {
                let diag = loader.diagnose_to_string(&e.to_diagnostic(loader.source_map()));
                panic!("Failed to desugar {:?}.\n{}", &f, diag);
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
