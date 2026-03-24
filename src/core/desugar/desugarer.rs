//! Desugarer maintains state during desugar passes.
use super::desugarable::{Content, Desugarable};
use crate::{
    common::{
        environment::DefaultingEnvironment,
        sourcemap::{Smid, SourceMap},
    },
    core::{
        binding::Var, doc::DeclarationDocumentation, error::CoreError, expr::*, target::*,
        unit::TranslationUnit,
    },
    syntax::input::*,
};
use codespan::Span;
use std::collections::{HashMap, HashSet};

/// A monad specification registered for a bracket pair or used inline in block metadata.
///
/// Describes how monadic blocks are desugared:
/// - `Explicit` names the bind and return functions directly.
/// - `Namespace` refers to a block in scope that must have `bind` and `return` members.
#[derive(Debug, Clone)]
pub enum MonadSpec {
    /// Explicit bind/return function names (e.g. from `{ :monad bind: f return: r }`).
    Explicit {
        /// Name of the monadic bind function (e.g. `"list-bind"`)
        bind_name: String,
        /// Name of the monadic return function (e.g. `"list-return"`)
        return_name: String,
    },
    /// Namespace reference: the monad is a block in scope with `bind` and `return` members.
    /// Emits `name.bind(value, lambda)` and `name.return(expr)` calls.
    Namespace(String),
}

/// State kept during desugaring pass
pub struct Desugarer<'smap> {
    /// While in the scope of anaphoric string pattern, collect all the
    /// anaphora for processing at the boundary the pattern
    pending_string_anaphora: HashMap<Anaphor<Smid, i32>, String>,
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
    /// Track variable names so we mint appropriate names
    env: DefaultingEnvironment<String, String>,
    /// Current usize
    file: Vec<usize>,
    /// Registry mapping bracket pair names (e.g. `"⟦⟧"`) to their monad spec.
    ///
    /// Populated when a bracket pair declaration with `bind` and `return` metadata
    /// is desugared.  Consulted when desugaring `⟦ { ... } ⟧` block expressions.
    monad_registry: HashMap<String, MonadSpec>,
    /// Registry mapping namespace names (e.g. `"io"`) to their monad spec.
    ///
    /// Populated when a top-level declaration carries `monad: true` metadata.
    /// Consulted when desugaring `{ :ns ... }` blocks without an explicit
    /// `.expr` return — only registered namespaces trigger implicit return.
    monad_namespace_registry: HashMap<String, MonadSpec>,
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
            env: DefaultingEnvironment::new(|k: &String| k.clone()),
            file: vec![],
            monad_registry: HashMap::new(),
            monad_namespace_registry: HashMap::new(),
        }
    }

    /// Register a monad spec for a bracket pair.
    ///
    /// Called when a bracket pair declaration with `bind` and `return` metadata
    /// is processed so that subsequent monadic block usages can look up the spec.
    pub fn register_monad_spec(&mut self, pair_name: String, spec: MonadSpec) {
        self.monad_registry.insert(pair_name, spec);
    }

    /// Look up the monad spec for a bracket pair, if one has been registered.
    pub fn monad_spec(&self, pair_name: &str) -> Option<&MonadSpec> {
        self.monad_registry.get(pair_name)
    }

    /// Return `true` if the desugarer is currently at the top level of a unit
    /// (i.e., the name stack is empty — no outer declaration has been pushed).
    pub fn is_top_level(&self) -> bool {
        self.stack.is_empty()
    }

    /// Register a monad spec for a namespace name.
    ///
    /// Called when a top-level declaration carrying `monad: true` metadata is
    /// processed so that `{ :ns ... }` blocks without `.expr` can use implicit
    /// return semantics.
    pub fn register_monad_namespace(&mut self, name: String, spec: MonadSpec) {
        self.monad_namespace_registry.insert(name, spec);
    }

    /// Look up the monad spec for a namespace name, if one has been registered.
    pub fn monad_namespace_spec(&self, name: &str) -> Option<&MonadSpec> {
        self.monad_namespace_registry.get(name)
    }

    /// Seed the monad namespace registry with entries from a previous
    /// translation unit (e.g. the prelude).
    pub fn seed_monad_namespace_registry(&mut self, registry: &HashMap<String, MonadSpec>) {
        self.monad_namespace_registry.extend(registry.clone());
    }

    /// Drain the monad namespace registry for persistence across units.
    pub fn drain_monad_namespace_registry(&mut self) -> HashMap<String, MonadSpec> {
        std::mem::take(&mut self.monad_namespace_registry)
    }

    /// Desugar content at locator (and imports) to create a new
    /// translation unit.
    pub fn translate_unit(&mut self, input: &Input) -> Result<TranslationUnit, CoreError> {
        if let Some(source) = self.contents.get(input) {
            self.file.push(source.file_id());
            let mut expr = source.content().desugar(self)?;

            // A lone name at the top level won't have passed through
            // any desugaring context that varifies it, so handle it
            // explicitly here.
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
            Err(CoreError::NoParsedAstFor(Box::new(input.clone())))
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
            Err(CoreError::NoParsedAstFor(Box::new(import.clone())))
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

    /// Reference to the name to variable name environment
    pub fn env(&self) -> &DefaultingEnvironment<String, String> {
        &self.env
    }

    /// Mutable reference to the name to variable name environment
    pub fn env_mut(&mut self) -> &mut DefaultingEnvironment<String, String> {
        &mut self.env
    }

    /// Record source position and mint a SMID for it.
    ///
    /// If we are inside a declaration (stack is non-empty), the SMID
    /// is automatically annotated with the current declaration name so
    /// that stack traces show function names instead of source snippets.
    pub fn new_smid(&mut self, span: Span) -> Smid {
        let file = *self.file.last().unwrap();
        if let Some(name) = self.stack.last() {
            self.source_map.add_annotated(file, span, name.clone())
        } else {
            self.source_map.add(file, span)
        }
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
    pub fn pending_string_anaphora(&self) -> &HashMap<Anaphor<Smid, i32>, String> {
        &self.pending_string_anaphora
    }

    /// Add a pending string anaphor
    pub fn add_pending_string_anaphor(&mut self, anaphor: Anaphor<Smid, i32>) -> String {
        let name = free(&format!("{anaphor}"));
        self.pending_string_anaphora.insert(anaphor, name.clone());
        name
    }

    /// Return the appropriate var name to use for `name` in this scope
    pub fn var(&mut self, name: &str) -> String {
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
                let var_name = self.var(name);
                RcExpr::from(Expr::Var(*smid, Var::Free(var_name)))
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
    pub fn record_target(
        &mut self,
        target: String,
        doc: String,
        fmt: Option<String>,
        validations: Vec<String>,
    ) {
        let tgt = Target::new(
            target,
            doc,
            fmt,
            self.stack.iter().map(String::clone).collect(),
            validations,
        );
        self.targets.insert(tgt);
    }

    /// Record doc strings discovered during desugaring
    pub fn record_doc(&mut self, doc: String, name: &str, args: Vec<String>) {
        self.docs.push(DeclarationDocumentation {
            name: name.to_string(),
            args,
            path: self.stack.iter().map(String::clone).collect(),
            doc,
        });
    }
}
