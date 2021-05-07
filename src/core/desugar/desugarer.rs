//! Desugarer maintains state during desugar passes.
use super::desugarable::{Content, Desugarable};
use crate::{
    common::{
        environment::DefaultingEnvironment,
        sourcemap::{Smid, SourceMap},
    },
    core::{
        doc::DeclarationDocumentation, error::CoreError, expr::*, target::*, unit::TranslationUnit,
    },
    syntax::{ast::*, input::*},
};
use codespan::Span;
use moniker::FreeVar;
use std::collections::{HashMap, HashSet};

/// State kept during desugaring pass
pub struct Desugarer<'smap> {
    /// While in the scope of anaphoric string pattern, collect all the
    /// anaphora for processing at the boundary the pattern
    pending_string_anaphora: HashMap<Anaphor<Smid, i32>, FreeVar<String>>,
    /// Targets discovered
    targets: HashSet<Target>,
    /// Doc strings discovered (path, doc)
    docs: Vec<DeclarationDocumentation>,
    /// Stack of names
    stack: Vec<String>,
    /// All parsed content for translation
    contents: &'smap HashMap<Input, Content<'smap>>,
    /// SourceMap
    source_map: &'smap mut SourceMap,
    /// Track variable names so we mint appropriate free vars
    env: DefaultingEnvironment<String, FreeVar<String>>,
    /// Current usize
    file: Vec<usize>,
}

impl<'smap> Desugarer<'smap> {
    /// Construct a Desugarer from a SourceMap and the desugarable contents
    #[allow(clippy::redundant_closure)]
    pub fn new(
        contents: &'smap HashMap<Input, Content<'smap>>,
        source_map: &'smap mut SourceMap,
    ) -> Self {
        Desugarer {
            pending_string_anaphora: HashMap::new(),
            targets: HashSet::new(),
            docs: Vec::new(),
            stack: vec![],
            contents,
            source_map,
            env: DefaultingEnvironment::new(|k| FreeVar::fresh_named(k)),
            file: vec![],
        }
    }

    /// Desugar content at locator (and imports) to create a new
    /// translation unit.
    pub fn translate_unit(&mut self, input: &Input) -> Result<TranslationUnit, CoreError> {
        if let Some(source) = self.contents.get(input) {
            self.file.push(source.file_id());
            let mut expr = source.content().desugar(self)?;

            // if we have only a single name, varify - (a lone name
            // will not have been contained in any context which will
            // varify it) HACK:
            if expr.inner.is_name() {
                expr = self.varify(expr);
            }

            if let Some(name) = input.name() {
                expr = expr.apply_name(Smid::default(), name);
            };

            let unit = TranslationUnit {
                expr,
                targets: self.targets.clone(),
                docs: self.docs.clone(),
            };
            self.file.pop();
            Ok(unit)
        } else {
            Err(CoreError::NoParsedAstFor(input.clone()))
        }
    }

    /// Used during translation to switch context and desugar an
    /// import.
    pub fn translate_import(&mut self, smid: Smid, import: Input) -> Result<RcExpr, CoreError> {
        if let Some(source) = self.contents.get(&import) {
            self.file.push(source.file_id());
            let mut expr = source.content().desugar(self)?;
            if let Some(name) = import.name() {
                expr = expr.apply_name(smid, name);
            }

            self.file.pop();
            Ok(expr)
        } else {
            Err(CoreError::NoParsedAstFor(import.clone()))
        }
    }

    /// Translate a chunk of parsed AST that has no imports
    pub fn translate_simple(
        &mut self,
        file_id: usize,
        ast: &impl Desugarable,
    ) -> Result<RcExpr, CoreError> {
        self.file.push(file_id);
        ast.desugar(self)
    }

    /// Reference to the name to free_var representation environment
    pub fn env(&self) -> &DefaultingEnvironment<String, FreeVar<String>> {
        &self.env
    }

    /// Mutable reference to the name to free_var representation environment
    pub fn env_mut(&mut self) -> &mut DefaultingEnvironment<String, FreeVar<String>> {
        &mut self.env
    }

    /// Record source position and mint a SMID for it
    pub fn new_smid(&mut self, span: Span) -> Smid {
        self.source_map.add(*self.file.last().unwrap(), span)
    }

    /// Record an annotated source position and mint a SMID for it
    pub fn new_annotated_smid<T: AsRef<str>>(&mut self, span: Span, annotation: T) -> Smid {
        self.source_map
            .add_annotated(*self.file.last().unwrap(), span, annotation)
    }

    /// True iff we are in the main module, not an import
    pub fn in_base_file(&self) -> bool {
        self.file.len() == 1
    }

    /// True if there are string anaphora pending
    pub fn has_pending_string_anaphora(&self) -> bool {
        !self.pending_string_anaphora.is_empty()
    }

    /// Clear any pending string anaphora
    pub fn clear_pending_string_anaphora(&mut self) {
        self.pending_string_anaphora.clear();
    }

    /// Reference to the pending string anaphora for calculating binders
    pub fn pending_string_anaphora(&self) -> &HashMap<Anaphor<Smid, i32>, FreeVar<String>> {
        &self.pending_string_anaphora
    }

    /// Add a pending string anaphor
    pub fn add_pending_string_anaphor(&mut self, anaphor: Anaphor<Smid, i32>) -> FreeVar<String> {
        let var = free(&format!("{}", anaphor));
        self.pending_string_anaphora.insert(anaphor, var.clone());
        var
    }

    /// Return the appropriate var to use for `name` in this scope
    pub fn var(&mut self, name: &str) -> moniker::FreeVar<String> {
        self.env.encounter(name.to_string());
        self.env.get(&name.to_string()).unwrap().clone()
    }

    /// Used in contexts where we are desugaring something "initial"...
    /// i.e. something which is not somewhere in op soup but at the start
    /// of an expression, where names cannot be names but must be
    /// variables.
    ///
    /// The default desugaring leaves names as names to leave soup
    /// desugaring in control of lookup subtleties.
    pub fn varify(&mut self, expr: RcExpr) -> RcExpr {
        match &*expr.inner {
            Expr::Name(smid, name) => {
                let var = self.var(&name);
                RcExpr::from(Expr::Var(*smid, moniker::Var::Free(var)))
            }
            _ => expr,
        }
    }

    /// Push a name component on entering a block, to keep track of
    /// locations for defining targets.
    pub fn push(&mut self, name: &str) {
        self.stack.push(String::from(name))
    }

    /// Pop a name component
    pub fn pop(&mut self) {
        self.stack.pop();
    }

    /// Record a target discovered in the desugaring process
    pub fn record_target(&mut self, target: String, doc: String, fmt: Option<String>) {
        let tgt = Target::new(
            target,
            doc,
            fmt,
            self.stack.iter().map(String::clone).collect(),
        );
        self.targets.insert(tgt);
    }

    /// Record doc strings
    pub fn record_doc(&mut self, doc: String, decl: &DeclarationComponents) {
        let path = self.stack.iter().map(String::clone).collect();

        self.docs.push(DeclarationDocumentation {
            name: decl.name.name().to_string(),
            args: decl.args.iter().map(|a| a.name().to_string()).collect(),
            path,
            doc,
        });
    }
}
