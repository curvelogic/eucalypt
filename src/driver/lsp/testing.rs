//! In-process LSP test harness.
//!
//! Provides `LspTestSession` — a lightweight wrapper around the LSP
//! handler functions that simulates an editing session without any
//! JSON-RPC transport.  Used for integration tests that exercise LSP
//! features against realistic editing scenarios.

use lsp_types::{CompletionItem, CompletionResponse, Hover, InlayHint, Position, Range, Url};

use super::completion;
use super::hover;
use super::inlay_hints;
use super::symbol_table::{self, SymbolSource, SymbolTable};
use crate::syntax::rowan::parse_unit;

/// An in-process LSP editing session for testing.
///
/// Simulates opening, editing, and querying a single document without
/// the JSON-RPC transport layer.
pub struct LspTestSession {
    uri: Url,
    content: String,
    prelude_table: SymbolTable,
}

impl LspTestSession {
    /// Create a new session with the prelude loaded.
    pub fn new() -> Self {
        let uri = Url::parse("file:///test.eu").expect("test URI");
        let prelude_uri = Url::parse("resource:prelude").expect("prelude URI");
        Self {
            uri,
            content: String::new(),
            prelude_table: symbol_table::prelude_symbols(&prelude_uri),
        }
    }

    /// Open (or replace) the document content.
    pub fn open(&mut self, content: &str) {
        self.content = content.to_string();
    }

    /// Replace the entire document content (alias for `open`).
    pub fn change(&mut self, content: &str) {
        self.content = content.to_string();
    }

    /// Request completion at the given line and character offset.
    pub fn complete(&self, line: u32, character: u32) -> Vec<CompletionItem> {
        let parse = parse_unit(&self.content);
        let root = parse.syntax_node();
        let table = self.build_symbol_table();

        let response = completion::completions(
            &self.content,
            &root,
            &Position::new(line, character),
            &table,
            None,
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

        hover::hover(
            &self.content,
            &root,
            &Position::new(line, character),
            &table,
            None,
        )
    }

    /// Request inlay hints for the full document.
    pub fn inlay_hints(&self) -> Vec<InlayHint> {
        let parse = parse_unit(&self.content);
        let root = parse.syntax_node();
        let table = self.build_symbol_table();

        let range = Range::new(Position::new(0, 0), Position::new(u32::MAX, 0));
        inlay_hints::inlay_hints(&self.content, &root, &range, &table, None)
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

    fn build_symbol_table(&self) -> SymbolTable {
        let parse = parse_unit(&self.content);
        let unit = parse.tree();

        let mut table = SymbolTable::new();
        table.add_from_unit(&unit, &self.content, &self.uri, SymbolSource::Local);

        for sym in self.prelude_table.all_symbols() {
            table.add(sym.clone());
        }

        table
    }
}

impl Default for LspTestSession {
    fn default() -> Self {
        Self::new()
    }
}
