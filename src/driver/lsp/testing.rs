//! In-process LSP test harness.
//!
//! Provides `LspTestSession` — a lightweight wrapper around the LSP
//! handler functions that simulates an editing session without any
//! JSON-RPC transport.  Used for integration tests that exercise LSP
//! features against realistic editing scenarios.

use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{mpsc, Arc};

use lsp_types::{
    CompletionItem, CompletionResponse, Hover, InlayHint, Position, Range,
    TextDocumentContentChangeEvent, Url,
};

use super::completion;
use super::hover;
use super::inlay_hints;
use super::symbol_table::{self, SymbolSource, SymbolTable};
use super::{apply_content_change, CachedPipeline, PipelineResult, TypeEnv};
use crate::syntax::rowan::{parse_unit, ParseError};

/// An in-process LSP editing session for testing.
///
/// Simulates opening, editing, and querying a single document without
/// the JSON-RPC transport layer. Mirrors the real server architecture
/// with a background pipeline for semantic features.
pub struct LspTestSession {
    uri: Url,
    content: String,
    prelude_table: SymbolTable,
    cached: Option<CachedPipeline>,
    pipeline_rx: mpsc::Receiver<PipelineResult>,
    pipeline_tx: mpsc::Sender<PipelineResult>,
    cancel: Arc<AtomicBool>,
    last_green: Option<rowan::GreenNode>,
    /// Parse errors from the most recent parse, cached to avoid re-parsing.
    last_parse_errors: Vec<ParseError>,
}

impl LspTestSession {
    /// Create a new session with the prelude loaded.
    pub fn new() -> Self {
        let uri = Url::parse("file:///test.eu").expect("test URI");
        let prelude_uri = Url::parse("resource:prelude").expect("prelude URI");
        let (pipeline_tx, pipeline_rx) = mpsc::channel();
        Self {
            uri,
            content: String::new(),
            prelude_table: symbol_table::prelude_symbols(&prelude_uri),
            cached: None,
            pipeline_rx,
            pipeline_tx,
            cancel: Arc::new(AtomicBool::new(false)),
            last_green: None,
            last_parse_errors: Vec::new(),
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
                Ok(cached) => {
                    self.cached = Some(cached);
                }
                Err(err) => {
                    eprintln!("pipeline error in test: {err}");
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

        let range = Range::new(Position::new(0, 0), Position::new(u32::MAX, 0));
        inlay_hints::inlay_hints(&self.content, &root, &range, &table, type_env)
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

    /// Exercise all LSP operations at every character position in the
    /// document.  Panics if any operation panics.  Used for stability
    /// testing — the return value is not meaningful.
    pub fn exercise_all(&self) {
        let len = self.content.len() as u32;
        for col in 0..=len {
            let _ = self.complete(0, col);
            let _ = self.hover(0, col);
        }
        let _ = self.inlay_hints();
    }

    /// Return the current document content.
    pub fn content(&self) -> &str {
        &self.content
    }

    /// Check if a cached pipeline result is available.
    pub fn has_cached(&self) -> bool {
        self.cached.is_some()
    }

    /// Count of imported files in the cached pipeline.
    pub fn import_count(&self) -> usize {
        self.cached.as_ref().map_or(0, |c| c.imports.len())
    }

    /// Check whether a pipeline result is pending (has been spawned
    /// but not yet received).
    pub fn has_pending_pipeline(&self) -> bool {
        // Try a non-blocking recv — if there's a result, put it back
        // by storing it. This is a peek operation.
        // Actually, we can't peek with mpsc. Instead, check if the
        // cancel flag is still false (meaning a pipeline is running).
        !self.cancel.load(std::sync::atomic::Ordering::SeqCst) && self.last_green.is_some()
    }

    /// Try to receive a pipeline result without blocking.
    /// Returns true if a result was received and applied.
    pub fn try_recv_pipeline(&mut self) -> bool {
        match self.pipeline_rx.try_recv() {
            Ok(result) => {
                match result.result {
                    Ok(cached) => {
                        self.cached = Some(cached);
                    }
                    Err(err) => {
                        eprintln!("pipeline error in test: {err}");
                        self.cached = None;
                    }
                }
                true
            }
            Err(_) => false,
        }
    }

    fn type_env(&self) -> Option<&TypeEnv> {
        self.cached
            .as_ref()
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
        if let Some(cached) = self.cached.as_ref().filter(|c| c.uri == self.uri) {
            for import in &cached.imports {
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

    /// Parse and check whether the green node changed; if so, cancel
    /// any previous pipeline run and spawn a new one.
    fn maybe_spawn_pipeline(&mut self) {
        let parse = parse_unit(&self.content);
        let new_green = parse.syntax_node().green().into_owned();

        let changed = match &self.last_green {
            Some(prev) => *prev != new_green,
            None => true,
        };

        if !changed {
            return;
        }
        self.last_green = Some(new_green);
        self.last_parse_errors = parse.errors().clone();

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
}

impl Default for LspTestSession {
    fn default() -> Self {
        Self::new()
    }
}
