//! LSP server implementation for eucalypt.
//!
//! Provides a Language Server Protocol server communicating via stdio,
//! built on the `lsp-server` crate. This module implements the main
//! event loop handling initialize/shutdown and dispatching requests
//! and notifications.

mod actions;
pub mod alias_index;
mod completion;
mod diagnostics;
mod folding;
mod formatting;
mod highlight;
mod hover;
mod inlay_hints;
mod navigation;
pub mod query;
mod references;
mod rename;
mod selection;
mod semantic;
pub mod symbol_table;
mod symbols;
pub mod testing;

use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{mpsc, Arc};
use std::time::Duration;

use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
    },
    request::{
        CodeActionRequest, Completion, DocumentHighlightRequest, DocumentSymbolRequest,
        FoldingRangeRequest, Formatting, GotoDefinition, HoverRequest, InlayHintRequest,
        RangeFormatting, References, Rename, SelectionRangeRequest, SemanticTokensFullRequest,
    },
    CompletionOptions, CompletionResponse, DocumentSymbolResponse, FoldingRangeProviderCapability,
    GotoDefinitionResponse, Hover, HoverProviderCapability, InitializeParams, Location,
    PublishDiagnosticsParams, SelectionRangeProviderCapability, SemanticTokensFullOptions,
    SemanticTokensOptions, SemanticTokensServerCapabilities, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkspaceEdit,
};

use crate::common::sourcemap::SourceMap;
use crate::core::typecheck::types::Type;

use self::query::{
    hash_combine, hash_many, hash_str, ContentHash, FileId, ImportedFile, PipelineCacheEntry,
    PipelineStageHashes, QueryStore, TypeEnv,
};
use self::symbol_table::{SymbolSource, SymbolTable};

/// A pipeline error with a message and optional source location.
pub struct PipelineError {
    /// The error message.
    pub message: String,
    /// Source location of the error, if available.
    pub range: Option<lsp_types::Range>,
}

impl std::fmt::Display for PipelineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

/// Result sent from the background pipeline thread.
struct PipelineResult {
    uri: Url,
    result: Result<PipelineCacheEntry, PipelineError>,
}

/// Run the LSP server on stdio.
///
/// This is the entry point called from the `eu lsp` subcommand.
/// It establishes a connection over stdin/stdout, performs the
/// LSP initialize handshake, then enters the main event loop
/// until shutdown is requested.
pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    eprintln!("eucalypt language server starting");

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = server_capabilities();
    let capabilities_json = serde_json::to_value(server_capabilities)?;

    let init_params = connection.initialize(capabilities_json)?;
    let _init_params: InitializeParams = serde_json::from_value(init_params)?;

    eprintln!("eucalypt language server initialised");

    main_loop(&connection)?;

    io_threads.join()?;

    eprintln!("eucalypt language server shut down");
    Ok(())
}

/// Build the server capabilities advertised during initialisation.
fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::INCREMENTAL,
        )),
        document_symbol_provider: Some(lsp_types::OneOf::Left(true)),
        folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
        definition_provider: Some(lsp_types::OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string()]),
            ..CompletionOptions::default()
        }),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: semantic::legend(),
                full: Some(SemanticTokensFullOptions::Bool(true)),
                range: None,
                ..SemanticTokensOptions::default()
            },
        )),
        document_formatting_provider: Some(lsp_types::OneOf::Left(true)),
        document_range_formatting_provider: Some(lsp_types::OneOf::Left(true)),
        references_provider: Some(lsp_types::OneOf::Left(true)),
        code_action_provider: Some(lsp_types::CodeActionProviderCapability::Simple(true)),
        inlay_hint_provider: Some(lsp_types::OneOf::Left(true)),
        document_highlight_provider: Some(lsp_types::OneOf::Left(true)),
        rename_provider: Some(lsp_types::OneOf::Right(lsp_types::RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: lsp_types::WorkDoneProgressOptions::default(),
        })),
        ..ServerCapabilities::default()
    }
}

/// In-memory store of open document contents, keyed by URI.
///
/// Needed because LSP requests (e.g. documentSymbol) reference
/// documents by URI but don't include the text.
struct DocumentStore {
    documents: HashMap<Url, String>,
}

impl DocumentStore {
    fn new() -> Self {
        Self {
            documents: HashMap::new(),
        }
    }

    fn open(&mut self, uri: Url, text: String) {
        self.documents.insert(uri, text);
    }

    fn change(&mut self, uri: Url, text: String) {
        self.documents.insert(uri, text);
    }

    fn close(&mut self, uri: &Url) {
        self.documents.remove(uri);
    }

    fn get(&self, uri: &Url) -> Option<&str> {
        self.documents.get(uri).map(|s| s.as_str())
    }
}

/// Convert an LSP `Position` (line, UTF-16 character offset) to a byte offset
/// within `text`.  Clamps to the end of file if the position is out of range.
fn lsp_position_to_byte_offset(text: &str, pos: lsp_types::Position) -> usize {
    let mut current_line = 0u32;
    let mut line_start = 0usize;

    // Walk characters to find the start of the target line.
    for (byte_idx, ch) in text.char_indices() {
        if current_line == pos.line {
            break;
        }
        if ch == '\n' {
            current_line += 1;
            line_start = byte_idx + 1; // '\n' is always 1 byte
        }
    }

    // If the requested line is beyond the end of file, clamp.
    if current_line < pos.line {
        return text.len();
    }

    // Walk UTF-16 code units within the target line.
    let line_text = &text[line_start..];
    let mut utf16_count = 0u32;
    let mut byte_offset = 0usize;

    for ch in line_text.chars() {
        if utf16_count >= pos.character {
            break;
        }
        utf16_count += ch.len_utf16() as u32;
        byte_offset += ch.len_utf8();
    }

    (line_start + byte_offset).min(text.len())
}

/// Apply a single LSP `TextDocumentContentChangeEvent` to `text`, returning
/// the updated document string.
///
/// If the change has no `range` it is a full-document replacement (the LSP
/// server may send this even in incremental mode as a fallback).  Otherwise
/// only the specified range is replaced.
pub fn apply_content_change(
    text: &str,
    change: &lsp_types::TextDocumentContentChangeEvent,
) -> String {
    if let Some(range) = change.range {
        let start = lsp_position_to_byte_offset(text, range.start);
        let end = lsp_position_to_byte_offset(text, range.end);
        // Guard against a malformed range where end < start.
        let (start, end) = if end < start {
            (end, start)
        } else {
            (start, end)
        };
        let mut result = String::with_capacity(text.len() - (end - start) + change.text.len());
        result.push_str(&text[..start]);
        result.push_str(&change.text);
        result.push_str(&text[end..]);
        result
    } else {
        // Full-document replacement.
        change.text.clone()
    }
}

/// Server state holding the document store, cached prelude symbols, and
/// the cached pipeline result from the last successful background run.
struct ServerState {
    store: DocumentStore,
    prelude_table: SymbolTable,

    /// Incremental query store: tracks file texts, parse results, pipeline
    /// results, per-stage hashes, and the import graph for cross-file
    /// invalidation.
    ///
    /// Replaces both the former `cached: HashMap<Url, CachedPipeline>` and
    /// the parse-specific `last_green`/`last_parse_errors` fields.
    queries: QueryStore,

    /// Channel for receiving pipeline results from background threads.
    pipeline_rx: mpsc::Receiver<PipelineResult>,

    /// Sender cloned to each background thread for delivering results.
    pipeline_tx: mpsc::Sender<PipelineResult>,

    /// Cancel flags per document URI for in-flight pipeline runs.
    cancel: HashMap<Url, Arc<AtomicBool>>,
}

impl ServerState {
    fn new() -> Self {
        // Write the embedded prelude to a temp file so go-to-definition
        // can navigate to real source locations instead of the non-existent
        // resource:prelude URI.
        let prelude_uri = Self::write_prelude_to_temp().unwrap_or_else(|| {
            Url::parse("resource:prelude")
                .unwrap_or_else(|_| Url::parse("file:///prelude.eu").expect("fallback URI"))
        });
        let (pipeline_tx, pipeline_rx) = mpsc::channel();
        Self {
            store: DocumentStore::new(),
            prelude_table: symbol_table::prelude_symbols(&prelude_uri),
            queries: QueryStore::new(),
            pipeline_rx,
            pipeline_tx,
            cancel: HashMap::new(),
        }
    }

    /// Write the embedded prelude source to a temp file and return its
    /// file URI.  Returns `None` if the write fails.
    fn write_prelude_to_temp() -> Option<Url> {
        let prelude_source = crate::driver::resources::Resources::default()
            .get("prelude")?
            .clone();
        let dir = std::env::temp_dir().join("eucalypt-lsp");
        std::fs::create_dir_all(&dir).ok()?;
        let path = dir.join("prelude.eu");
        std::fs::write(&path, &prelude_source).ok()?;
        Url::from_file_path(&path).ok()
    }

    /// Look up the type env for a URI from the cached pipeline result.
    fn type_env_for(&self, uri: &Url) -> Option<&TypeEnv> {
        let file_id = FileId::from_url(uri);
        self.queries
            .get_pipeline_result(&file_id)
            .map(|c| &c.type_env)
    }

    /// Look up lambda parameter types from the cached pipeline.
    fn lambda_params_for(&self, uri: &Url) -> Option<&HashMap<(u32, u32, String), Type>> {
        let file_id = FileId::from_url(uri);
        self.queries
            .get_pipeline_result(&file_id)
            .map(|c| &c.lambda_params)
    }

    /// Look up the cached pipeline entry for a URI.
    ///
    /// Returns the last successful result regardless of whether the file
    /// has been edited since (stale results remain useful for hover/completion
    /// while a new pipeline run is in progress).
    fn cached_pipeline(&self, uri: &Url) -> Option<Rc<PipelineCacheEntry>> {
        let file_id = FileId::from_url(uri);
        self.queries.get_pipeline_result(&file_id).cloned()
    }

    /// Build a symbol table for a document from AST, prelude, and
    /// cached pipeline type environment.
    ///
    /// Import resolution is handled by the background pipeline;
    /// symbols for imported names come from the cached type_env.
    fn build_symbol_table(&self, uri: &Url, text: &str) -> SymbolTable {
        let parse = crate::syntax::rowan::parse_unit(text);
        let unit = parse.tree();

        let mut table = SymbolTable::new();

        // Add local file symbols
        table.add_from_unit(&unit, text, uri, SymbolSource::Local);

        // Add prelude symbols
        for sym in self.prelude_table.all_symbols() {
            table.add(sym.clone());
        }

        // Add symbols from imported files resolved by the pipeline.
        if let Some(cached) = self.cached_pipeline(uri) {
            for import in &cached.imports {
                let import_parse = crate::syntax::rowan::parse_unit(&import.source);
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

    /// Apply a pipeline result: store in QueryStore and publish type diagnostics.
    fn apply_pipeline_result(
        &mut self,
        connection: &Connection,
        result: PipelineResult,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let uri = result.uri.clone();
        match result.result {
            Ok(entry) => {
                // Publish type diagnostics from the new pipeline result.
                let text = self.store.get(&uri).unwrap_or_default().to_string();
                let type_diags = diagnostics::diagnostics_from_type_warnings(
                    &text,
                    &entry.warnings,
                    &entry.source_map,
                );
                // Use cached parse errors from the query store rather than
                // re-parsing the document.
                let file_id = FileId::from_url(&uri);
                let empty = vec![];
                let parse_errors = self.queries.last_parse_errors(&file_id).unwrap_or(&empty);
                let mut diags = diagnostics::diagnostics_from_parse_errors(&text, parse_errors);
                diags.extend(type_diags);

                let params = PublishDiagnosticsParams {
                    uri: uri.clone(),
                    diagnostics: diags,
                    version: None,
                };
                let notif = lsp_server::Notification::new(
                    lsp_types::notification::PublishDiagnostics::METHOD.to_string(),
                    params,
                );
                connection.sender.send(Message::Notification(notif))?;

                // Update the import graph in the query store so that future
                // edits to imported files trigger re-checking of this file.
                let import_file_ids: Vec<FileId> = entry
                    .imports
                    .iter()
                    .map(|imp| FileId::from_url(&imp.uri))
                    .collect();
                self.queries.set_imports(&file_id, import_file_ids);

                // Compute the combined input hash for this pipeline run
                // (file text hash combined with all import hashes).
                let file_text_hash = self
                    .queries
                    .file_text_hash(&file_id)
                    .unwrap_or(entry.stage_hashes.file_text);
                let import_hashes: Vec<ContentHash> = self
                    .queries
                    .imports_of(&file_id)
                    .filter_map(|imp| self.queries.file_text_hash(imp))
                    .collect();
                let input_hash = hash_many(
                    &std::iter::once(file_text_hash)
                        .chain(import_hashes)
                        .collect::<Vec<_>>(),
                );

                // Store the pipeline result in the query store.
                self.queries
                    .store_pipeline_result(file_id, entry, input_hash);

                // Ask the client to re-request inlay hints now that
                // pipeline-resolved types (lambda_params) are available.
                // Also try semanticTokens/refresh as some clients respond to that.
                for (id_suffix, method) in [
                    ("inlay", "workspace/inlayHint/refresh"),
                    ("semantic", "workspace/semanticTokens/refresh"),
                ] {
                    let refresh = lsp_server::Request::new(
                        lsp_server::RequestId::from(format!("{id_suffix}-refresh")),
                        method.to_string(),
                        serde_json::Value::Null,
                    );
                    let _ = connection.sender.send(Message::Request(refresh));
                }
            }
            Err(err) => {
                eprintln!("pipeline error for {}: {}", uri, err.message);
                // Note: we intentionally do NOT remove the existing cached pipeline
                // result on error — stale type information is still useful for
                // hover/completion while the user is in the middle of an edit.
                let file_id = FileId::from_url(&uri);
                let text = self.store.get(&uri).unwrap_or_default().to_string();
                let empty = vec![];
                let parse_errors = self.queries.last_parse_errors(&file_id).unwrap_or(&empty);
                let mut diags = diagnostics::diagnostics_from_parse_errors(&text, parse_errors);
                // Surface the pipeline error as a diagnostic so the user
                // can see why type checking and import resolution failed.
                diags.push(lsp_types::Diagnostic {
                    range: err.range.unwrap_or_default(),
                    severity: Some(lsp_types::DiagnosticSeverity::WARNING),
                    source: Some("eucalypt-pipeline".to_string()),
                    message: err.message,
                    ..Default::default()
                });
                let params = PublishDiagnosticsParams {
                    uri: uri.clone(),
                    diagnostics: diags,
                    version: None,
                };
                let notif = lsp_server::Notification::new(
                    lsp_types::notification::PublishDiagnostics::METHOD.to_string(),
                    params,
                );
                connection.sender.send(Message::Notification(notif))?;
            }
        }
        Ok(())
    }
}

/// The main event loop, processing messages until shutdown.
fn main_loop(connection: &Connection) -> Result<(), Box<dyn std::error::Error>> {
    let mut state = ServerState::new();

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                handle_request(connection, &state, req)?;
            }
            Message::Notification(notif) => {
                handle_notification(connection, &mut state, notif)?;
            }
            Message::Response(_resp) => {
                // Responses to server-initiated requests (e.g.
                // workspace/inlayHint/refresh) — nothing to do.
            }
        }

        // Check for pipeline results from the background thread.
        while let Ok(result) = state.pipeline_rx.try_recv() {
            state.apply_pipeline_result(connection, result)?;
        }
    }
    Ok(())
}

/// Handle an incoming LSP request.
///
/// Dispatches known request types. Unrecognised requests receive a
/// method-not-found error response.
fn handle_request(
    connection: &Connection,
    state: &ServerState,
    req: Request,
) -> Result<(), Box<dyn std::error::Error>> {
    use lsp_types::request::Request as _;

    if req.method == DocumentSymbolRequest::METHOD {
        let (id, params): (_, lsp_types::DocumentSymbolParams) =
            req.extract(DocumentSymbolRequest::METHOD)?;
        let result = on_document_symbol(&state.store, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == FoldingRangeRequest::METHOD {
        let (id, params): (_, lsp_types::FoldingRangeParams) =
            req.extract(FoldingRangeRequest::METHOD)?;
        let result = on_folding_range(&state.store, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == SelectionRangeRequest::METHOD {
        let (id, params): (_, lsp_types::SelectionRangeParams) =
            req.extract(SelectionRangeRequest::METHOD)?;
        let result = on_selection_range(&state.store, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == GotoDefinition::METHOD {
        let (id, params): (_, lsp_types::GotoDefinitionParams) =
            req.extract(GotoDefinition::METHOD)?;
        let result = on_goto_definition(state, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == HoverRequest::METHOD {
        let (id, params): (_, lsp_types::HoverParams) = req.extract(HoverRequest::METHOD)?;
        let result = on_hover(state, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == Completion::METHOD {
        let (id, params): (_, lsp_types::CompletionParams) = req.extract(Completion::METHOD)?;
        let result = on_completion(state, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == SemanticTokensFullRequest::METHOD {
        let (id, params): (_, lsp_types::SemanticTokensParams) =
            req.extract(SemanticTokensFullRequest::METHOD)?;
        let result = on_semantic_tokens_full(state, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == Formatting::METHOD {
        let (id, params): (_, lsp_types::DocumentFormattingParams) =
            req.extract(Formatting::METHOD)?;
        let result = on_formatting(&state.store, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == RangeFormatting::METHOD {
        let (id, params): (_, lsp_types::DocumentRangeFormattingParams) =
            req.extract(RangeFormatting::METHOD)?;
        let result = on_range_formatting(&state.store, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == References::METHOD {
        let (id, params): (_, lsp_types::ReferenceParams) = req.extract(References::METHOD)?;
        let result = on_references(state, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == CodeActionRequest::METHOD {
        let (id, params): (_, lsp_types::CodeActionParams) =
            req.extract(CodeActionRequest::METHOD)?;
        let result = on_code_action(state, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == InlayHintRequest::METHOD {
        let (id, params): (_, lsp_types::InlayHintParams) =
            req.extract(InlayHintRequest::METHOD)?;
        let result = on_inlay_hint(state, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == DocumentHighlightRequest::METHOD {
        let (id, params): (_, lsp_types::DocumentHighlightParams) =
            req.extract(DocumentHighlightRequest::METHOD)?;
        let result = on_document_highlight(state, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    if req.method == Rename::METHOD {
        let (id, params): (_, lsp_types::RenameParams) = req.extract(Rename::METHOD)?;
        let result = on_rename(state, params);
        let resp = Response::new_ok(id, result);
        connection.sender.send(Message::Response(resp))?;
        return Ok(());
    }

    {
        use lsp_types::request::{PrepareRenameRequest, Request as _};
        if req.method == PrepareRenameRequest::METHOD {
            let (id, params): (_, lsp_types::TextDocumentPositionParams) =
                req.extract(PrepareRenameRequest::METHOD)?;
            let result = on_prepare_rename(state, params);
            let resp = Response::new_ok(id, result);
            connection.sender.send(Message::Response(resp))?;
            return Ok(());
        }
    }

    eprintln!("unhandled request: {}", req.method);
    let resp = Response::new_err(
        req.id,
        lsp_server::ErrorCode::MethodNotFound as i32,
        format!("unhandled request: {}", req.method),
    );
    connection.sender.send(Message::Response(resp))?;
    Ok(())
}

/// Handle an incoming LSP notification.
///
/// Dispatches known notification types to their handlers. Unknown
/// notifications are logged and ignored per the LSP specification.
fn handle_notification(
    connection: &Connection,
    state: &mut ServerState,
    notif: Notification,
) -> Result<(), Box<dyn std::error::Error>> {
    match notif.method.as_str() {
        DidOpenTextDocument::METHOD => {
            let params: lsp_types::DidOpenTextDocumentParams =
                serde_json::from_value(notif.params)?;
            on_document_open(connection, state, params)?;
        }
        DidChangeTextDocument::METHOD => {
            let params: lsp_types::DidChangeTextDocumentParams =
                serde_json::from_value(notif.params)?;
            on_document_change(connection, state, params)?;
        }
        DidCloseTextDocument::METHOD => {
            let params: lsp_types::DidCloseTextDocumentParams =
                serde_json::from_value(notif.params)?;
            on_document_close(state, params);
        }
        _ => {
            eprintln!("unhandled notification: {}", notif.method);
        }
    }
    Ok(())
}

/// Handle textDocument/didOpen — store the text, parse, and publish diagnostics.
fn on_document_open(
    connection: &Connection,
    state: &mut ServerState,
    params: lsp_types::DidOpenTextDocumentParams,
) -> Result<(), Box<dyn std::error::Error>> {
    let uri = params.text_document.uri;
    let text = params.text_document.text;
    state.store.open(uri.clone(), text.clone());
    on_document_changed(connection, state, uri, &text)?;
    Ok(())
}

/// Handle textDocument/didChange — apply incremental edits, re-parse, and
/// publish diagnostics.
///
/// We use incremental document sync, so each change event may contain a
/// `range` that describes only the modified region.  Multiple changes are
/// applied in order.  A change without a `range` is treated as a full
/// document replacement (the LSP spec permits this as a fallback).
fn on_document_change(
    connection: &Connection,
    state: &mut ServerState,
    params: lsp_types::DidChangeTextDocumentParams,
) -> Result<(), Box<dyn std::error::Error>> {
    let uri = params.text_document.uri;
    let mut text = state.store.get(&uri).unwrap_or_default().to_string();
    for change in &params.content_changes {
        text = apply_content_change(&text, change);
    }
    state.store.change(uri.clone(), text.clone());
    on_document_changed(connection, state, uri, &text)?;
    Ok(())
}

/// Handle textDocument/didClose — clean up per-document caches.
fn on_document_close(state: &mut ServerState, params: lsp_types::DidCloseTextDocumentParams) {
    let uri = params.text_document.uri;
    let file_id = FileId::from_url(&uri);
    state.store.close(&uri);
    // `queries.remove_file` cleans up parse cache, pipeline result, and import graph.
    state.queries.remove_file(&file_id);
    if let Some(cancel) = state.cancel.remove(&uri) {
        cancel.store(true, Ordering::SeqCst);
    }
}

/// Common handler for document open/change: parse, detect changes,
/// publish parse diagnostics, and spawn a background pipeline run.
///
/// ## Cross-file invalidation
///
/// After updating the file text in the query store, we look up all files that
/// import the changed file and trigger a pipeline re-run for each of them.
/// This ensures that edits to a library file propagate to its importers even
/// though the importer's own text hasn't changed.
fn on_document_changed(
    connection: &Connection,
    state: &mut ServerState,
    uri: Url,
    text: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let parse = crate::syntax::rowan::parse_unit(text);
    let new_green = parse.syntax_node().green().into_owned();

    // Check if the green node actually changed.
    //
    // Rowan's GreenNode equality compares both tree structure AND token text,
    // so changing a string literal (e.g. `"hello"` → `"world"`) is detected
    // as a change and triggers a pipeline re-run.  This is intentional:
    //
    // - Import paths are string literals (`{ import: "foo.eu" }`), and
    //   changing the path must re-run the pipeline to load the new file.
    // - Eucalypt string values participate in type-checking and can affect
    //   pipeline output semantically.
    //
    // A structural-only comparison (ignoring token text) would incorrectly
    // suppress re-runs for such changes.  The current approach is correct.
    let file_id = FileId::from_url(&uri);
    let changed = match state.queries.last_green(&file_id) {
        Some(prev_green) => *prev_green != new_green,
        None => true,
    };

    if !changed {
        return Ok(());
    }

    // Update the query store: set file text (bumps revision), store parse result.
    state
        .queries
        .set_file_text(file_id.clone(), std::sync::Arc::new(text.to_string()));
    state
        .queries
        .store_parse(file_id.clone(), new_green, parse.errors().clone());

    // Publish parse-error diagnostics immediately.
    let diags = diagnostics::diagnostics_from_parse_errors(text, parse.errors());
    let params = PublishDiagnosticsParams {
        uri: uri.clone(),
        diagnostics: diags,
        version: None,
    };
    let notif = lsp_server::Notification::new(
        lsp_types::notification::PublishDiagnostics::METHOD.to_string(),
        params,
    );
    connection.sender.send(Message::Notification(notif))?;

    // Cancel any in-flight pipeline run for this document.
    if let Some(prev_cancel) = state.cancel.get(&uri) {
        prev_cancel.store(true, Ordering::SeqCst);
    }
    let cancel = Arc::new(AtomicBool::new(false));
    state.cancel.insert(uri.clone(), cancel.clone());

    let path = uri.to_file_path().ok();
    let text_owned = text.to_string();
    let tx = state.pipeline_tx.clone();
    let uri_clone = uri.clone();

    spawn_pipeline(uri_clone, text_owned, path, tx, cancel);

    // Cross-file invalidation: trigger a pipeline re-run for all files that
    // import the changed file.  The import graph was populated when those
    // files' pipelines previously completed.
    let importers: Vec<FileId> = state.queries.importers_of(&file_id).cloned().collect();
    for importer_id in importers {
        let importer_uri = importer_id.uri.clone();
        // Skip if the importer is not open in the editor.
        let importer_text = match state.store.get(&importer_uri) {
            Some(t) => t.to_string(),
            None => continue,
        };
        // Cancel any in-flight run for the importer.
        if let Some(prev_cancel) = state.cancel.get(&importer_uri) {
            prev_cancel.store(true, Ordering::SeqCst);
        }
        let importer_cancel = Arc::new(AtomicBool::new(false));
        state
            .cancel
            .insert(importer_uri.clone(), importer_cancel.clone());
        let importer_path = importer_uri.to_file_path().ok();
        spawn_pipeline(
            importer_uri,
            importer_text,
            importer_path,
            state.pipeline_tx.clone(),
            importer_cancel,
        );
    }

    Ok(())
}

/// Handle textDocument/documentSymbol — return symbols from the syntax tree.
fn on_document_symbol(
    store: &DocumentStore,
    params: lsp_types::DocumentSymbolParams,
) -> Option<DocumentSymbolResponse> {
    let uri = &params.text_document.uri;
    let text = store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let unit = parse.tree();
    let syms = symbols::document_symbols(text, &unit);
    Some(DocumentSymbolResponse::Nested(syms))
}

/// Handle textDocument/foldingRange — return folding ranges from the syntax tree.
fn on_folding_range(
    store: &DocumentStore,
    params: lsp_types::FoldingRangeParams,
) -> Option<Vec<lsp_types::FoldingRange>> {
    let uri = &params.text_document.uri;
    let text = store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let unit = parse.tree();
    Some(folding::folding_ranges(text, &unit))
}

/// Handle textDocument/selectionRange — return selection ranges for positions.
fn on_selection_range(
    store: &DocumentStore,
    params: lsp_types::SelectionRangeParams,
) -> Option<Vec<lsp_types::SelectionRange>> {
    let uri = &params.text_document.uri;
    let text = store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    Some(selection::selection_ranges(text, &root, &params.positions))
}

/// Handle textDocument/definition — go to the definition of the symbol at cursor.
fn on_goto_definition(
    state: &ServerState,
    params: lsp_types::GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let text = state.store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    let position = &params.text_document_position_params.position;

    // Check for alias reference inside a `type:` string first (§A7).
    let alias_idx = alias_index::build_alias_index(text, &root, uri);
    if let Some(result) =
        alias_index::goto_definition_for_alias(text, &root, position, &alias_idx, uri)
    {
        return Some(result);
    }

    let table = state.build_symbol_table(uri, text);
    navigation::goto_definition(text, &root, position, &table)
}

/// Handle textDocument/hover — return hover information for the symbol at cursor.
fn on_hover(state: &ServerState, params: lsp_types::HoverParams) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let text = state.store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    let position = &params.text_document_position_params.position;

    // Check for alias reference inside a `type:` string first (§A7).
    let alias_idx = alias_index::build_alias_index(text, &root, uri);
    let cached = state.cached_pipeline(uri);
    let alias_types = cached.as_ref().map(|c| &c.alias_types);
    if let Some(h) = alias_index::hover_for_alias(text, &root, position, &alias_idx, alias_types) {
        return Some(h);
    }

    let table = state.build_symbol_table(uri, text);
    let type_env = state.type_env_for(uri);
    hover::hover(text, &root, position, &table, type_env)
}

/// Handle textDocument/references — find all references to the symbol at cursor.
fn on_references(state: &ServerState, params: lsp_types::ReferenceParams) -> Option<Vec<Location>> {
    let uri = &params.text_document_position.text_document.uri;
    let text = state.store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    references::find_references(
        text,
        &root,
        &params.text_document_position.position,
        uri,
        params.context.include_declaration,
    )
}

/// Handle textDocument/completion — return completion items at the cursor position.
fn on_completion(
    state: &ServerState,
    params: lsp_types::CompletionParams,
) -> Option<CompletionResponse> {
    let uri = &params.text_document_position.text_document.uri;
    let text = state.store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    let table = state.build_symbol_table(uri, text);
    let type_env = state.type_env_for(uri);
    Some(completion::completions(
        text,
        &root,
        &params.text_document_position.position,
        &table,
        type_env,
    ))
}

/// Handle textDocument/semanticTokens/full — return semantic tokens for the document.
fn on_semantic_tokens_full(
    state: &ServerState,
    params: lsp_types::SemanticTokensParams,
) -> Option<lsp_types::SemanticTokensResult> {
    let uri = &params.text_document.uri;
    let text = state.store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    let table = state.build_symbol_table(uri, text);
    let tokens = semantic::semantic_tokens_full(text, &root, &table);
    Some(lsp_types::SemanticTokensResult::Tokens(tokens))
}

/// Handle textDocument/formatting — format the entire document.
fn on_formatting(
    store: &DocumentStore,
    params: lsp_types::DocumentFormattingParams,
) -> Option<Vec<lsp_types::TextEdit>> {
    let uri = &params.text_document.uri;
    let text = store.get(uri)?;
    let edits = formatting::format_document(text, &params.options);
    Some(edits)
}

/// Handle textDocument/rangeFormatting — format a range within the document.
fn on_range_formatting(
    store: &DocumentStore,
    params: lsp_types::DocumentRangeFormattingParams,
) -> Option<Vec<lsp_types::TextEdit>> {
    let uri = &params.text_document.uri;
    let text = store.get(uri)?;
    let edits = formatting::format_range(text, &params.range, &params.options);
    Some(edits)
}

/// Handle textDocument/codeAction — return code actions for the given range.
fn on_code_action(
    state: &ServerState,
    params: lsp_types::CodeActionParams,
) -> Option<lsp_types::CodeActionResponse> {
    let uri = &params.text_document.uri;
    let text = state.store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    let table = state.build_symbol_table(uri, text);

    let cached = state.cached_pipeline(uri);
    let empty_warnings = vec![];
    let empty_source_map = SourceMap::new();
    let (warnings, source_map) = cached
        .as_ref()
        .map(|c| (c.warnings.as_slice(), &c.source_map))
        .unwrap_or((&empty_warnings, &empty_source_map));

    let code_actions = actions::code_actions(
        text,
        &root,
        &params.range,
        uri,
        &table,
        warnings,
        source_map,
    );
    let response: Vec<lsp_types::CodeActionOrCommand> = code_actions
        .into_iter()
        .map(lsp_types::CodeActionOrCommand::CodeAction)
        .collect();
    Some(response)
}

/// Handle textDocument/inlayHint — return inlay hints for the visible range.
fn on_inlay_hint(
    state: &ServerState,
    params: lsp_types::InlayHintParams,
) -> Option<Vec<lsp_types::InlayHint>> {
    let uri = &params.text_document.uri;
    let text = state.store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    let table = state.build_symbol_table(uri, text);
    let type_env = state.type_env_for(uri);
    let lambda_params = state.lambda_params_for(uri);
    let hints =
        inlay_hints::inlay_hints(text, &root, &params.range, &table, type_env, lambda_params);
    Some(hints)
}

/// Handle textDocument/documentHighlight — highlight matching bracket pairs.
fn on_document_highlight(
    state: &ServerState,
    params: lsp_types::DocumentHighlightParams,
) -> Option<Vec<lsp_types::DocumentHighlight>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let text = state.store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    let position = &params.text_document_position_params.position;
    let highlights = highlight::document_highlights(text, &root, position);
    if highlights.is_empty() {
        None
    } else {
        Some(highlights)
    }
}

/// Handle textDocument/rename — rename the symbol at cursor across the file.
fn on_rename(state: &ServerState, params: lsp_types::RenameParams) -> Option<WorkspaceEdit> {
    let uri = &params.text_document_position.text_document.uri;
    let text = state.store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    let position = &params.text_document_position.position;

    // If cursor is on an alias reference inside a type: string, rename the alias (§A7).
    let alias_idx = alias_index::build_alias_index(text, &root, uri);
    if let Some(edit) =
        alias_index::rename_alias(text, &root, position, &params.new_name, &alias_idx, uri)
    {
        return Some(edit);
    }

    rename::rename(text, &root, position, &params.new_name, uri)
}

/// Handle textDocument/prepareRename — validate the rename target.
fn on_prepare_rename(
    state: &ServerState,
    params: lsp_types::TextDocumentPositionParams,
) -> Option<lsp_types::PrepareRenameResponse> {
    let uri = &params.text_document.uri;
    let text = state.store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    rename::prepare_rename(text, &root, &params.position)
}

/// Extract binding names from the core expression's nested Lets
/// and insert them into the type env with `Type::Any` as a placeholder.
///
/// This is needed because the checker's scope stack is empty after
/// checking (all scopes are pushed then popped). The Let bindings
/// still provide the names of all resolved symbols (local + imported).
/// We recurse into the body of each Let to capture nested scopes
/// (prelude, imports, user bindings).
fn extract_top_level_bindings(expr: &crate::core::expr::RcExpr, type_env: &mut TypeEnv) {
    use crate::core::expr::Expr;

    match &*expr.inner {
        Expr::Let(_, scope, _) => {
            for b in &scope.pattern {
                type_env.entry(b.name.clone()).or_insert(Type::Any);
            }
            // Recurse into the body to capture nested Let scopes.
            extract_top_level_bindings(&scope.body, type_env);
        }
        Expr::Meta(_, inner, _) => {
            extract_top_level_bindings(inner, type_env);
        }
        _ => {}
    }
}

/// Spawn a background thread that runs the full SourceLoader pipeline
/// after a debounce delay. Results are sent via the mpsc channel.
fn spawn_pipeline(
    uri: Url,
    text: String,
    path: Option<PathBuf>,
    tx: mpsc::Sender<PipelineResult>,
    cancel: Arc<AtomicBool>,
) {
    std::thread::spawn(move || {
        // Debounce: wait 300ms before starting the pipeline.
        std::thread::sleep(Duration::from_millis(300));
        if cancel.load(Ordering::SeqCst) {
            return;
        }

        let result = run_pipeline(&uri, &text, path.as_deref());
        if cancel.load(Ordering::SeqCst) {
            return;
        }

        let _ = tx.send(PipelineResult { uri, result });
    });
}

/// Run the full SourceLoader pipeline on the given document text.
///
/// Uses `Locator::Buffer` for the primary document so that unsaved
/// editor content is type-checked without requiring a save to disk.
/// Returns extracted data (type env, warnings, source map) since
/// `SourceLoader` is not `Send`.
/// Convert a `EucalyptError` into a `PipelineError` with source location
/// extracted from the loader's source map and file store.
fn make_pipeline_error(
    loader: &crate::driver::source::SourceLoader,
    e: &crate::driver::error::EucalyptError,
) -> PipelineError {
    use codespan_reporting::files::Files;
    let diag = e.to_diagnostic(loader.source_map());
    let range = diag.labels.first().and_then(|label| {
        let files = loader.files();
        let start = files.location(label.file_id, label.range.start).ok()?;
        let end = files.location(label.file_id, label.range.end).ok()?;
        Some(lsp_types::Range {
            start: lsp_types::Position {
                line: (start.line_number - 1) as u32,
                character: (start.column_number - 1) as u32,
            },
            end: lsp_types::Position {
                line: (end.line_number - 1) as u32,
                character: (end.column_number - 1) as u32,
            },
        })
    });
    PipelineError {
        message: format!("{e}"),
        range,
    }
}

fn run_pipeline(
    uri: &Url,
    text: &str,
    path: Option<&std::path::Path>,
) -> Result<PipelineCacheEntry, PipelineError> {
    use crate::core::typecheck::check::type_check_full;
    use crate::driver::source::SourceLoader;
    use crate::syntax::input::{Input, Locator};

    let prelude = Input::new(Locator::Resource("prelude".to_string()), None, "eu");

    // Use Locator::Buffer when we have a path, otherwise fall back to
    // Locator::Literal (no source map location, but still runs).
    let doc_input = if let Some(p) = path {
        Input::new(
            Locator::Buffer {
                path: p.to_path_buf(),
                text: text.to_string(),
            },
            None,
            "eu",
        )
    } else {
        Input::new(Locator::Literal(text.to_string()), None, "eu")
    };

    let inputs = vec![prelude, doc_input];
    let mut loader = SourceLoader::new(vec![]);

    // Per-stage content hashes.
    //
    // Conservative proxy strategy: each stage's output hash equals its
    // input hash (the pipeline is deterministic, so same input → same output).
    // The type-check hash is derived from the actual output for finer
    // granularity.  More precise structural hashing of RcExpr trees is a
    // future optimisation.
    let file_text_hash = hash_str(text);

    for input in &inputs {
        if let Err(e) = loader.load(input) {
            return Err(make_pipeline_error(&loader, &e));
        }
    }
    for input in &inputs {
        if let Err(e) = loader.translate(input) {
            return Err(make_pipeline_error(&loader, &e));
        }
    }

    // Desugar hash: proxy — same file text → same desugared output.
    let desugar_hash = file_text_hash;

    if let Err(e) = loader.merge_units(&inputs) {
        return Err(make_pipeline_error(&loader, &e));
    }

    // Merge hash: proxy — same desugared inputs → same merged output.
    let merge_hash = desugar_hash;

    if let Err(e) = loader.cook() {
        return Err(make_pipeline_error(&loader, &e));
    }

    // Cook hash: proxy — same merge input → same cooked output.
    let cook_hash = merge_hash;

    // Extract all binding names BEFORE dead-code elimination so that
    // completion can offer imported names that aren't yet referenced.
    let mut type_env = TypeEnv::new();
    extract_top_level_bindings(&loader.core().expr, &mut type_env);

    // Extract type aliases BEFORE dead-code elimination.  Dead-code
    // elimination (prune) removes unreferenced bindings, which may include
    // declarations that carry `types:` metadata used only for annotations.
    // We walk all Meta nodes in the cooked expression to collect aliases that
    // would otherwise be lost before the type checker runs on the pruned expr.
    let alias_types_pre = crate::core::typecheck::check::extract_aliases(&loader.core().expr);

    if let Err(e) = loader.eliminate() {
        return Err(make_pipeline_error(&loader, &e));
    }

    // Eliminate hash: proxy — same cook input → same eliminated output.
    let eliminate_hash = cook_hash;
    // Inline hash: proxy (inline pass runs next; no separate call here).
    let inline_hash = eliminate_hash;

    let core_expr = loader.core().expr.clone();
    let result = type_check_full(&core_expr);

    // Type-check hash: derived from the actual output (warnings + binding names)
    // so that a refactoring that preserves types produces the same hash even
    // when the source text differs.
    let type_check_hash = {
        let mut h: ContentHash = 0;
        // Hash warnings in sorted order for stability.
        let mut w_strs: Vec<String> = result.warnings.iter().map(|w| format!("{:?}", w)).collect();
        w_strs.sort();
        for s in &w_strs {
            h = hash_combine(h, hash_str(s));
        }
        // Hash binding names (sorted for stability).
        let mut names: Vec<&String> = type_env.keys().collect();
        names.sort();
        for n in &names {
            h = hash_combine(h, hash_str(n));
        }
        h
    };

    // Diagnostics hash: combine parse errors (none at this stage — parse errors
    // are tracked separately) with type-check hash.
    let diagnostics_hash = type_check_hash;

    // Merge any types from the checker (currently empty because the
    // scope stack is popped, but future checker changes may fix this).
    for (name, ty) in result.types {
        type_env.insert(name, ty);
    }

    // Extract imported file sources before consuming the loader.
    // Skip the prelude (resource locator) and the primary document
    // (buffer/literal locator) — we only want actual filesystem imports.
    // Determine the document's directory for resolving relative imports.
    let doc_dir = path.and_then(|p| p.parent());

    let imports: Vec<ImportedFile> = loader
        .loaded_locators()
        .iter()
        .filter_map(|loc| {
            if let Locator::Fs(fs_path) = loc {
                let source = loader.source_text(loc)?;
                // Resolve relative paths against the document's directory.
                let abs_path = if fs_path.is_relative() {
                    doc_dir?.join(fs_path)
                } else {
                    fs_path.clone()
                };
                let import_uri = Url::from_file_path(&abs_path).ok()?;
                Some(ImportedFile {
                    uri: import_uri,
                    source: source.to_string(),
                })
            } else {
                None
            }
        })
        .collect();

    // Flatten the Smid-keyed lambda_params into (line, col, name) keys
    // using the source map for position resolution.
    let mut lambda_params = HashMap::new();
    {
        use codespan_reporting::files::Files;
        let files = loader.files();
        let sm = loader.source_map();
        for (smid, params) in &result.lambda_params {
            if let Some(info) = sm.source_info_for_smid(*smid) {
                if let (Some(file_id), Some(span)) = (info.file, info.span) {
                    if let Ok(loc) = files.location(file_id, span.start().into()) {
                        let line = (loc.line_number - 1) as u32;
                        let col = (loc.column_number - 1) as u32;
                        for (name, ty) in params {
                            lambda_params.insert((line, col, name.clone()), ty.clone());
                        }
                    }
                }
            }
        }
    }

    let (_, source_map, _) = loader.complete();

    // Merge pre-elimination aliases with post-elimination aliases.
    // Post-elimination aliases (from the type-check pass) take priority
    // since they are more precisely resolved against the inferred types.
    let mut alias_types = alias_types_pre;
    alias_types.extend(result.aliases);

    let stage_hashes = PipelineStageHashes {
        file_text: file_text_hash,
        desugar: desugar_hash,
        merge: merge_hash,
        cook: cook_hash,
        eliminate: eliminate_hash,
        inline: inline_hash,
        type_check: type_check_hash,
        diagnostics: diagnostics_hash,
    };

    Ok(PipelineCacheEntry {
        uri: uri.clone(),
        type_env,
        lambda_params,
        warnings: result.warnings,
        source_map,
        imports,
        alias_types,
        stage_hashes,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn server_capabilities_has_text_sync() {
        let caps = server_capabilities();
        assert!(caps.text_document_sync.is_some());
        match caps.text_document_sync.unwrap() {
            TextDocumentSyncCapability::Kind(kind) => {
                assert_eq!(kind, TextDocumentSyncKind::INCREMENTAL);
            }
            _ => panic!("expected TextDocumentSyncKind"),
        }
    }

    #[test]
    fn server_capabilities_has_definition() {
        let caps = server_capabilities();
        assert!(caps.definition_provider.is_some());
    }

    #[test]
    fn server_capabilities_has_hover() {
        let caps = server_capabilities();
        assert!(caps.hover_provider.is_some());
    }

    #[test]
    fn server_capabilities_has_semantic_tokens() {
        let caps = server_capabilities();
        assert!(caps.semantic_tokens_provider.is_some());
    }

    #[test]
    fn server_capabilities_has_formatting() {
        let caps = server_capabilities();
        assert!(caps.document_formatting_provider.is_some());
    }

    #[test]
    fn server_capabilities_has_range_formatting() {
        let caps = server_capabilities();
        assert!(caps.document_range_formatting_provider.is_some());
    }

    #[test]
    fn server_capabilities_has_references() {
        let caps = server_capabilities();
        assert!(caps.references_provider.is_some());
    }

    #[test]
    fn server_capabilities_has_inlay_hints() {
        let caps = server_capabilities();
        assert!(caps.inlay_hint_provider.is_some());
    }

    #[test]
    fn server_capabilities_has_code_actions() {
        let caps = server_capabilities();
        assert!(caps.code_action_provider.is_some());
    }

    #[test]
    fn server_capabilities_has_completion() {
        let caps = server_capabilities();
        assert!(caps.completion_provider.is_some());
        let opts = caps.completion_provider.unwrap();
        assert!(
            opts.trigger_characters
                .as_ref()
                .is_some_and(|tc| tc.contains(&".".to_string())),
            "dot should be a trigger character"
        );
    }
}
