//! LSP server implementation for eucalypt.
//!
//! Provides a Language Server Protocol server communicating via stdio,
//! built on the `lsp-server` crate. This module implements the main
//! event loop handling initialize/shutdown and dispatching requests
//! and notifications.

mod actions;
mod completion;
pub mod context;
mod diagnostics;
mod folding;
mod formatting;
mod hover;
mod inlay_hints;
mod navigation;
mod references;
mod rename;
mod selection;
mod semantic;
pub mod symbol_table;
mod symbols;

use std::collections::HashMap;

use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    notification::{DidChangeTextDocument, DidOpenTextDocument, Notification as _},
    request::{
        CodeActionRequest, Completion, DocumentSymbolRequest, FoldingRangeRequest, Formatting,
        GotoDefinition, HoverRequest, InlayHintRequest, RangeFormatting, References, Rename,
        SelectionRangeRequest, SemanticTokensFullRequest,
    },
    CompletionOptions, CompletionResponse, DocumentSymbolResponse, FoldingRangeProviderCapability,
    GotoDefinitionResponse, Hover, HoverProviderCapability, InitializeParams, Location,
    PublishDiagnosticsParams, SelectionRangeProviderCapability, SemanticTokensFullOptions,
    SemanticTokensOptions, SemanticTokensServerCapabilities, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkspaceEdit,
};

use self::symbol_table::{SymbolSource, SymbolTable};

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
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
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
        code_action_provider: None,
        inlay_hint_provider: Some(lsp_types::OneOf::Left(true)),
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

    fn get(&self, uri: &Url) -> Option<&str> {
        self.documents.get(uri).map(|s| s.as_str())
    }
}

/// Server state holding the document store and cached prelude symbols.
struct ServerState {
    store: DocumentStore,
    prelude_table: SymbolTable,
}

impl ServerState {
    fn new() -> Self {
        let prelude_uri = Url::parse("resource:prelude")
            .unwrap_or_else(|_| Url::parse("file:///prelude.eu").expect("fallback URI"));
        Self {
            store: DocumentStore::new(),
            prelude_table: symbol_table::prelude_symbols(&prelude_uri),
        }
    }

    /// Build a symbol table for a document, including prelude symbols.
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

        table
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
                // We don't send requests to the client yet,
                // so responses are unexpected.
            }
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
    publish_diagnostics(connection, uri, &text)?;
    Ok(())
}

/// Handle textDocument/didChange — update stored text, re-parse, and publish diagnostics.
///
/// We use full document sync, so the first content change contains the
/// entire document.
fn on_document_change(
    connection: &Connection,
    state: &mut ServerState,
    params: lsp_types::DidChangeTextDocumentParams,
) -> Result<(), Box<dyn std::error::Error>> {
    let uri = params.text_document.uri;
    if let Some(change) = params.content_changes.into_iter().next() {
        state.store.change(uri.clone(), change.text.clone());
        publish_diagnostics(connection, uri, &change.text)?;
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
    let table = state.build_symbol_table(uri, text);
    navigation::goto_definition(
        text,
        &root,
        &params.text_document_position_params.position,
        &table,
    )
}

/// Handle textDocument/hover — return hover information for the symbol at cursor.
fn on_hover(state: &ServerState, params: lsp_types::HoverParams) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let text = state.store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    let table = state.build_symbol_table(uri, text);
    hover::hover(
        text,
        &root,
        &params.text_document_position_params.position,
        &table,
    )
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
    Some(completion::completions(
        text,
        &root,
        &params.text_document_position.position,
        &table,
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
    let code_actions = actions::code_actions(text, &root, &params.range, uri, &table);
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
    let hints = inlay_hints::inlay_hints(text, &root, &params.range, &table);
    Some(hints)
}

/// Handle textDocument/rename — rename the symbol at cursor across the file.
fn on_rename(state: &ServerState, params: lsp_types::RenameParams) -> Option<WorkspaceEdit> {
    let uri = &params.text_document_position.text_document.uri;
    let text = state.store.get(uri)?;
    let parse = crate::syntax::rowan::parse_unit(text);
    let root = parse.syntax_node();
    rename::rename(
        text,
        &root,
        &params.text_document_position.position,
        &params.new_name,
        uri,
    )
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

/// Parse the document and publish diagnostics to the client.
fn publish_diagnostics(
    connection: &Connection,
    uri: Url,
    text: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let parse = crate::syntax::rowan::parse_unit(text);
    let diags = diagnostics::diagnostics_from_parse_errors(text, parse.errors());

    let params = PublishDiagnosticsParams {
        uri,
        diagnostics: diags,
        version: None,
    };

    let notif = lsp_server::Notification::new(
        lsp_types::notification::PublishDiagnostics::METHOD.to_string(),
        params,
    );
    connection.sender.send(Message::Notification(notif))?;
    Ok(())
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
                assert_eq!(kind, TextDocumentSyncKind::FULL);
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
    fn server_capabilities_has_no_code_actions() {
        let caps = server_capabilities();
        assert!(caps.code_action_provider.is_none());
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
