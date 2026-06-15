//! In-process LSP test harness.
//!
//! Provides `LspTestSession` — a lightweight wrapper around the LSP
//! handler functions that simulates an editing session without any
//! JSON-RPC transport.  Used for integration tests that exercise LSP
//! features against realistic editing scenarios.

use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{mpsc, Arc};

use crate::core::typecheck::types::Type;

use lsp_types::{
    CodeAction, CompletionItem, CompletionResponse, DocumentHighlight, GotoDefinitionResponse,
    Hover, InlayHint, Position, Range, TextDocumentContentChangeEvent, Url,
};

use super::actions;
use super::completion;
use super::highlight;
use super::hover;
use super::inlay_hints;
use super::navigation;
use super::query::{FileId, PipelineCacheEntry, QueryStore};
use super::symbol_table::{self, SymbolSource, SymbolTable};
use super::{apply_content_change, PipelineError, PipelineResult, TypeEnv};
use crate::syntax::rowan::parse_unit;

/// An in-process LSP editing session for testing.
///
/// Simulates opening, editing, and querying a single document without
/// the JSON-RPC transport layer. Mirrors the real server architecture
/// with a background pipeline for semantic features.
pub struct LspTestSession {
    uri: Url,
    content: String,
    prelude_table: SymbolTable,
    pipeline_rx: mpsc::Receiver<PipelineResult>,
    pipeline_tx: mpsc::Sender<PipelineResult>,
    cancel: Arc<AtomicBool>,
    /// Query store tracking parse results, pipeline results, and the
    /// import graph.
    queries: QueryStore,
    /// Last pipeline error, if any.
    last_pipeline_error: Option<PipelineError>,
}

impl LspTestSession {
    /// Create a new session with the prelude loaded.
    ///
    /// Uses the same temp-file prelude as the real server so that
    /// go-to-definition on prelude names returns a file: URI.
    pub fn new() -> Self {
        let uri = Url::parse("file:///test.eu").expect("test URI");
        let prelude_uri = super::ServerState::write_prelude_to_temp()
            .unwrap_or_else(|| Url::parse("resource:prelude").expect("prelude URI"));
        let (pipeline_tx, pipeline_rx) = mpsc::channel();
        Self {
            uri,
            content: String::new(),
            prelude_table: symbol_table::prelude_symbols(&prelude_uri),
            pipeline_rx,
            pipeline_tx,
            cancel: Arc::new(AtomicBool::new(false)),
            queries: QueryStore::new(),
            last_pipeline_error: None,
        }
    }

    /// Set the document URI (for import resolution against real files).
    pub fn set_uri(&mut self, uri: &str) {
        self.uri = Url::parse(uri).expect("valid URI");
    }

    /// Open (or replace) the document content and trigger a background
    /// pipeline run if the green node changed.
    pub fn open(&mut self, content: &str) {
        self.content = content.to_string();
        self.maybe_spawn_pipeline();
    }

    /// Replace the entire document content (alias for `open`) and
    /// trigger a background pipeline run if the green node changed.
    pub fn change(&mut self, content: &str) {
        self.content = content.to_string();
        self.maybe_spawn_pipeline();
    }

    /// Apply an incremental edit to the document using an LSP-style range
    /// (line, UTF-16 character offsets) and replacement text, then trigger
    /// a pipeline run if the green node changed.
    pub fn apply_edit(
        &mut self,
        start_line: u32,
        start_char: u32,
        end_line: u32,
        end_char: u32,
        new_text: &str,
    ) {
        let change = TextDocumentContentChangeEvent {
            range: Some(Range {
                start: Position::new(start_line, start_char),
                end: Position::new(end_line, end_char),
            }),
            range_length: None,
            text: new_text.to_string(),
        };
        self.content = apply_content_change(&self.content, &change);
        self.maybe_spawn_pipeline();
    }

    /// Block until the background pipeline completes and apply the
    /// result. Used in tests that need deterministic semantic data.
    pub fn wait_for_pipeline(&mut self) {
        match self
            .pipeline_rx
            .recv_timeout(std::time::Duration::from_secs(30))
        {
            Ok(result) => match result.result {
                Ok(entry) => {
                    let file_id = FileId::from_url(&result.uri);
                    // Update import graph in the query store.
                    let import_ids: Vec<FileId> = entry
                        .imports
                        .iter()
                        .map(|imp| FileId::from_url(&imp.uri))
                        .collect();
                    self.queries.set_imports(&file_id, import_ids);
                    // Compute combined input hash and store pipeline result.
                    let input_hash = self.queries.compute_pipeline_input_hash(&file_id);
                    self.queries
                        .store_pipeline_result(file_id, entry, input_hash);
                    self.last_pipeline_error = None;
                }
                Err(err) => {
                    eprintln!("pipeline error in test: {err}");
                    self.last_pipeline_error = Some(err);
                }
            },
            Err(e) => {
                eprintln!("pipeline recv timeout/disconnect: {e}");
            }
        }
    }

    /// Convenience: open + wait_for_pipeline.
    pub fn run_pipeline(&mut self, content: &str) {
        self.open(content);
        self.wait_for_pipeline();
    }

    /// Request completion at the given line and character offset.
    pub fn complete(&self, line: u32, character: u32) -> Vec<CompletionItem> {
        let parse = parse_unit(&self.content);
        let root = parse.syntax_node();
        let table = self.build_symbol_table();
        let type_env = self.type_env();

        let response = completion::completions(
            &self.content,
            &root,
            &Position::new(line, character),
            &table,
            type_env,
        );
        match response {
            CompletionResponse::Array(items) => items,
            CompletionResponse::List(list) => list.items,
        }
    }

    /// Request hover information at the given position.
    pub fn hover(&self, line: u32, character: u32) -> Option<Hover> {
        let parse = parse_unit(&self.content);
        let root = parse.syntax_node();
        let table = self.build_symbol_table();
        let type_env = self.type_env();

        hover::hover(
            &self.content,
            &root,
            &Position::new(line, character),
            &table,
            type_env,
        )
    }

    /// Request inlay hints for the full document.
    pub fn inlay_hints(&self) -> Vec<InlayHint> {
        let parse = parse_unit(&self.content);
        let root = parse.syntax_node();
        let table = self.build_symbol_table();
        let type_env = self.type_env();
        let lambda_params = self.lambda_params();

        let range = Range::new(Position::new(0, 0), Position::new(u32::MAX, 0));
        inlay_hints::inlay_hints(
            &self.content,
            &root,
            &range,
            &table,
            type_env,
            lambda_params,
        )
    }

    /// Get the completion labels as strings (convenience for assertions).
    pub fn complete_labels(&self, line: u32, character: u32) -> Vec<String> {
        self.complete(line, character)
            .into_iter()
            .map(|item| item.label)
            .collect()
    }

    /// Get hover content as a plain string (convenience for assertions).
    pub fn hover_text(&self, line: u32, character: u32) -> Option<String> {
        self.hover(line, character).map(|h| match h.contents {
            lsp_types::HoverContents::Markup(m) => m.value,
            lsp_types::HoverContents::Scalar(s) => match s {
                lsp_types::MarkedString::String(s) => s,
                lsp_types::MarkedString::LanguageString(ls) => ls.value,
            },
            lsp_types::HoverContents::Array(arr) => arr
                .into_iter()
                .map(|s| match s {
                    lsp_types::MarkedString::String(s) => s,
                    lsp_types::MarkedString::LanguageString(ls) => ls.value,
                })
                .collect::<Vec<_>>()
                .join("\n"),
        })
    }

    /// Compute go-to-definition at the given cursor position.
    pub fn goto_definition(&self, line: u32, character: u32) -> Option<GotoDefinitionResponse> {
        let parse = parse_unit(&self.content);
        let root = parse.syntax_node();
        let position = Position { line, character };

        // Check for alias reference in a type: string first (§A7).
        let alias_idx = super::alias_index::build_alias_index(&self.content, &root, &self.uri);
        if let Some(result) = super::alias_index::goto_definition_for_alias(
            &self.content,
            &root,
            &position,
            &alias_idx,
            &self.uri,
        ) {
            return Some(result);
        }

        let table = self.build_symbol_table();
        navigation::goto_definition(&self.content, &root, &position, &table)
    }

    /// Go-to-definition for a type alias reference inside a `type:` string (§A7).
    pub fn goto_definition_for_type_alias(
        &self,
        line: u32,
        character: u32,
    ) -> Option<GotoDefinitionResponse> {
        let parse = parse_unit(&self.content);
        let root = parse.syntax_node();
        let position = Position { line, character };
        let alias_idx = super::alias_index::build_alias_index(&self.content, &root, &self.uri);
        super::alias_index::goto_definition_for_alias(
            &self.content,
            &root,
            &position,
            &alias_idx,
            &self.uri,
        )
    }

    /// Hover for a type alias reference inside a `type:` string (§A7).
    pub fn hover_for_type_alias(&self, line: u32, character: u32) -> Option<Hover> {
        let parse = parse_unit(&self.content);
        let root = parse.syntax_node();
        let position = Position { line, character };
        let alias_idx = super::alias_index::build_alias_index(&self.content, &root, &self.uri);
        let cached = self.cached_pipeline();
        let alias_types = cached.as_ref().map(|c| &c.alias_types);
        super::alias_index::hover_for_alias(
            &self.content,
            &root,
            &position,
            &alias_idx,
            alias_types,
        )
    }

    /// Rename a type alias at the given cursor position (§A7).
    pub fn rename_type_alias(
        &self,
        line: u32,
        character: u32,
        new_name: &str,
    ) -> Option<lsp_types::WorkspaceEdit> {
        let parse = parse_unit(&self.content);
        let root = parse.syntax_node();
        let position = Position { line, character };
        let alias_idx = super::alias_index::build_alias_index(&self.content, &root, &self.uri);
        super::alias_index::rename_alias(
            &self.content,
            &root,
            &position,
            new_name,
            &alias_idx,
            &self.uri,
        )
    }

    /// Compute document highlights at the given cursor position.
    pub fn document_highlights(&self, line: u32, character: u32) -> Vec<DocumentHighlight> {
        let parse = parse_unit(&self.content);
        let root = parse.syntax_node();
        let position = Position { line, character };
        highlight::document_highlights(&self.content, &root, &position)
    }

    /// Compute code actions for the given range.
    pub fn code_actions(
        &self,
        start_line: u32,
        start_char: u32,
        end_line: u32,
        end_char: u32,
    ) -> Vec<CodeAction> {
        let parse = parse_unit(&self.content);
        let root = parse.syntax_node();
        let table = self.build_symbol_table();
        let range = Range::new(
            Position::new(start_line, start_char),
            Position::new(end_line, end_char),
        );
        actions::code_actions(
            &self.content,
            &root,
            &range,
            &self.uri,
            &table,
            &[],
            &crate::common::sourcemap::SourceMap::new(),
        )
    }

    /// Compute code actions for the full document.
    pub fn all_code_actions(&self) -> Vec<CodeAction> {
        self.code_actions(0, 0, 1000, 0)
    }

    /// Get code action titles (convenience for assertions).
    pub fn code_action_titles(
        &self,
        start_line: u32,
        start_char: u32,
        end_line: u32,
        end_char: u32,
    ) -> Vec<String> {
        self.code_actions(start_line, start_char, end_line, end_char)
            .into_iter()
            .map(|a| a.title)
            .collect()
    }

    /// Exercise all LSP operations at every character position in the
    /// document.  Panics if any operation panics.  Used for stability
    /// testing — the return value is not meaningful.
    pub fn exercise_all(&self) {
        let len = self.content.len() as u32;
        for col in 0..=len {
            let _ = self.complete(0, col);
            let _ = self.hover(0, col);
            let _ = self.document_highlights(0, col);
        }
        let _ = self.inlay_hints();
    }

    /// Return the current document content.
    pub fn content(&self) -> &str {
        &self.content
    }

    /// Lambda param names and types in the cached pipeline (for debugging).
    pub fn lambda_param_names(&self) -> Vec<(String, String)> {
        self.cached_pipeline().map_or(vec![], |c| {
            c.lambda_params
                .iter()
                .map(|((_line, _col, name), v)| (name.clone(), format!("{v}")))
                .collect()
        })
    }

    fn lambda_params(&self) -> Option<&HashMap<(u32, u32, String), Type>> {
        // Safety: Rc is not Send but this session is single-threaded.
        // We return a reference with the same lifetime as self.
        let file_id = FileId::from_url(&self.uri);
        self.queries
            .get_pipeline_result(&file_id)
            .map(|c| &c.lambda_params)
    }

    /// Return the last pipeline error, if any.
    pub fn last_pipeline_error(&self) -> Option<&PipelineError> {
        self.last_pipeline_error.as_ref()
    }

    /// Check if a cached pipeline result is available.
    pub fn has_cached(&self) -> bool {
        let file_id = FileId::from_url(&self.uri);
        self.queries.get_pipeline_result(&file_id).is_some()
    }

    /// Count of imported files in the cached pipeline.
    pub fn import_count(&self) -> usize {
        self.cached_pipeline().map_or(0, |c| c.imports.len())
    }

    /// Check whether a pipeline result is pending (has been spawned
    /// but not yet received).
    pub fn has_pending_pipeline(&self) -> bool {
        let file_id = FileId::from_url(&self.uri);
        !self.cancel.load(std::sync::atomic::Ordering::SeqCst)
            && self.queries.last_green(&file_id).is_some()
    }

    /// Try to receive a pipeline result without blocking.
    /// Returns true if a result was received and applied.
    pub fn try_recv_pipeline(&mut self) -> bool {
        match self.pipeline_rx.try_recv() {
            Ok(result) => {
                match result.result {
                    Ok(entry) => {
                        let file_id = FileId::from_url(&result.uri);
                        // Update import graph in the query store.
                        let import_ids: Vec<FileId> = entry
                            .imports
                            .iter()
                            .map(|imp| FileId::from_url(&imp.uri))
                            .collect();
                        self.queries.set_imports(&file_id, import_ids);
                        let input_hash = self.queries.compute_pipeline_input_hash(&file_id);
                        self.queries
                            .store_pipeline_result(file_id, entry, input_hash);
                    }
                    Err(err) => {
                        eprintln!("pipeline error in test: {err}");
                    }
                }
                true
            }
            Err(_) => false,
        }
    }

    fn type_env(&self) -> Option<&TypeEnv> {
        let file_id = FileId::from_url(&self.uri);
        self.queries
            .get_pipeline_result(&file_id)
            .filter(|c| c.uri == self.uri)
            .map(|c| &c.type_env)
    }

    fn build_symbol_table(&self) -> SymbolTable {
        let parse = parse_unit(&self.content);
        let unit = parse.tree();

        let mut table = SymbolTable::new();
        table.add_from_unit(&unit, &self.content, &self.uri, SymbolSource::Local);

        for sym in self.prelude_table.all_symbols() {
            table.add(sym.clone());
        }

        // Add symbols from imported files resolved by the pipeline.
        let cached = self.cached_pipeline();
        if let Some(c) = cached.as_ref().filter(|c| c.uri == self.uri) {
            for import in &c.imports {
                let import_parse = parse_unit(&import.source);
                let import_unit = import_parse.tree();
                table.add_from_unit(
                    &import_unit,
                    &import.source,
                    &import.uri,
                    SymbolSource::Import,
                );
            }
        }

        table
    }

    /// Return the last cached pipeline result for the session's URI, if any.
    fn cached_pipeline(&self) -> Option<Rc<PipelineCacheEntry>> {
        let file_id = FileId::from_url(&self.uri);
        self.queries.get_pipeline_result(&file_id).cloned()
    }

    /// Parse and check whether the green node changed; if so, cancel
    /// any previous pipeline run and spawn a new one.
    fn maybe_spawn_pipeline(&mut self) {
        let parse = parse_unit(&self.content);
        let new_green = parse.syntax_node().green().into_owned();

        let file_id = FileId::from_url(&self.uri);
        let changed = match self.queries.last_green(&file_id) {
            Some(prev) => *prev != new_green,
            None => true,
        };

        if !changed {
            return;
        }

        // Update the query store: set file text (bumps revision) and store the parse result.
        self.queries
            .set_file_text(file_id.clone(), std::sync::Arc::new(self.content.clone()));
        self.queries
            .store_parse(file_id, new_green, parse.errors().clone());

        // Cancel any in-flight run.
        self.cancel.store(true, Ordering::SeqCst);
        let cancel = Arc::new(AtomicBool::new(false));
        self.cancel = cancel.clone();

        let path: Option<PathBuf> = self.uri.to_file_path().ok();
        let text = self.content.clone();
        let tx = self.pipeline_tx.clone();
        let uri = self.uri.clone();

        super::spawn_pipeline(uri, text, path, tx, cancel);
    }

    // ── Query store accessors for tests ─────────────────────────────────────

    /// Return the files that the primary document imports, as recorded by
    /// the query store after the last completed pipeline run.
    pub fn recorded_imports(&self) -> Vec<Url> {
        let file_id = FileId::from_url(&self.uri);
        self.queries
            .imports_of(&file_id)
            .map(|f| f.uri.clone())
            .collect()
    }

    /// Return whether `importer_uri` is recorded as importing `dependency_uri`
    /// in the query store.
    pub fn is_recorded_importer(&self, importer_uri: &Url, dependency_uri: &Url) -> bool {
        let dep_id = FileId::from_url(dependency_uri);
        self.queries
            .importers_of(&dep_id)
            .any(|f| &f.uri == importer_uri)
    }

    /// Return the current revision of the query store.
    pub fn query_revision(&self) -> super::query::Revision {
        self.queries.revision()
    }

    /// Return whether the query store has a parse result for the primary document.
    pub fn has_parse_cache(&self) -> bool {
        let file_id = FileId::from_url(&self.uri);
        self.queries.last_green(&file_id).is_some()
    }

    /// Return whether the parse result for the primary document is still current
    /// (i.e. file text hash matches the stored parse hash).
    pub fn parse_is_current(&self) -> bool {
        let file_id = FileId::from_url(&self.uri);
        self.queries.get_parse(&file_id).is_some()
    }

    /// Return whether the pipeline result for the primary document is still
    /// current (i.e. computed from the current file text).
    pub fn pipeline_result_is_current(&self) -> bool {
        let file_id = FileId::from_url(&self.uri);
        self.queries.pipeline_result_is_current(&file_id)
    }

    /// Return the per-stage hashes for the last completed pipeline run, if any.
    pub fn stage_hashes(&self) -> Option<super::query::PipelineStageHashes> {
        self.cached_pipeline().map(|c| c.stage_hashes.clone())
    }
}

impl Default for LspTestSession {
    fn default() -> Self {
        Self::new()
    }
}
