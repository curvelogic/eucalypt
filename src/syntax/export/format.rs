//! Source code formatter for Eucalypt
//!
//! Provides formatting capabilities with two modes:
//! - Conservative: Fix violations while preserving good formatting
//! - Full reformat: Generate canonical output from scratch

use crate::syntax::rowan::ast::{self as rowan_ast, AstToken, HasSoup};
use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode, SyntaxToken};
use pretty::RcDoc;
use rowan::ast::AstNode;

/// Configuration for the formatter
#[derive(Debug, Clone)]
pub struct FormatterConfig {
    /// Target line width for breaking
    pub line_width: usize,
    /// Indent size in spaces
    pub indent_size: usize,
    /// Use full reformatting mode instead of conservative
    pub reformat: bool,
}

impl Default for FormatterConfig {
    fn default() -> Self {
        Self {
            line_width: 80,
            indent_size: 2,
            reformat: false,
        }
    }
}

impl FormatterConfig {
    pub fn new(line_width: usize, indent_size: usize, reformat: bool) -> Self {
        Self {
            line_width,
            indent_size,
            reformat,
        }
    }
}

/// The main formatter struct
pub struct Formatter {
    config: FormatterConfig,
}

impl Formatter {
    pub fn new(config: FormatterConfig) -> Self {
        Self { config }
    }

    /// Format a Unit (top-level file)
    pub fn format_unit(&self, unit: &rowan_ast::Unit) -> String {
        let doc = self.format_unit_doc(unit);
        self.render_doc(doc)
    }

    /// Format a Soup (expression)
    pub fn format_soup(&self, soup: &rowan_ast::Soup) -> String {
        let doc = self.format_soup_doc(soup);
        self.render_doc(doc)
    }

    /// Render a document to string
    fn render_doc(&self, doc: RcDoc<'static, ()>) -> String {
        let mut w = Vec::new();
        doc.render(self.config.line_width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    /// Format a Unit to a Doc
    fn format_unit_doc(&self, unit: &rowan_ast::Unit) -> RcDoc<'static, ()> {
        if self.config.reformat {
            self.reformat_unit(unit)
        } else {
            self.conservative_format_unit(unit)
        }
    }

    /// Format a Soup to a Doc
    fn format_soup_doc(&self, soup: &rowan_ast::Soup) -> RcDoc<'static, ()> {
        if self.config.reformat {
            self.reformat_soup(soup)
        } else {
            self.conservative_format_soup(soup)
        }
    }

    // ========================================================================
    // Full Reformatting Mode
    // ========================================================================

    fn reformat_unit(&self, unit: &rowan_ast::Unit) -> RcDoc<'static, ()> {
        let mut docs = Vec::new();

        // Handle metadata if present
        if let Some(meta) = unit.meta() {
            docs.push(self.reformat_block_metadata(&meta));
        }

        // Format each declaration
        for decl in unit.declarations() {
            docs.push(self.reformat_declaration(&decl));
        }

        // Join declarations with blank lines
        RcDoc::intersperse(docs, RcDoc::hardline().append(RcDoc::hardline()))
    }

    fn reformat_declaration(&self, decl: &rowan_ast::Declaration) -> RcDoc<'static, ()> {
        let mut doc = RcDoc::nil();

        // Handle metadata
        if let Some(meta) = decl.meta() {
            doc = doc
                .append(self.reformat_decl_metadata(&meta))
                .append(RcDoc::hardline());
        }

        // Format head
        if let Some(head) = decl.head() {
            doc = doc.append(self.reformat_decl_head(&head));
        }

        // Colon with proper spacing
        doc = doc.append(RcDoc::text(":"));

        // Format body
        if let Some(body) = decl.body() {
            if let Some(soup) = body.soup() {
                // Check if body is a block - blocks go on same line
                let is_block = soup
                    .singleton()
                    .is_some_and(|e| matches!(e, rowan_ast::Element::Block(_)));

                if is_block {
                    doc = doc.append(RcDoc::space()).append(self.reformat_soup(&soup));
                } else {
                    // For non-blocks, space then content
                    doc = doc.append(RcDoc::space()).append(self.reformat_soup(&soup));
                }
            }
        }

        doc
    }

    fn reformat_decl_head(&self, head: &rowan_ast::DeclarationHead) -> RcDoc<'static, ()> {
        match head.classify_declaration() {
            rowan_ast::DeclarationKind::Property(id) => RcDoc::text(id.syntax().text().to_string()),
            rowan_ast::DeclarationKind::Function(name, args) => {
                let name_doc = RcDoc::text(name.syntax().text().to_string());
                let args_doc = self.reformat_apply_tuple(&args);
                name_doc.append(args_doc)
            }
            rowan_ast::DeclarationKind::Nullary(paren, _) => self.reformat_paren_expr(&paren),
            rowan_ast::DeclarationKind::Prefix(paren, _, _) => self.reformat_paren_expr(&paren),
            rowan_ast::DeclarationKind::Postfix(paren, _, _) => self.reformat_paren_expr(&paren),
            rowan_ast::DeclarationKind::Binary(paren, _, _, _) => self.reformat_paren_expr(&paren),
            rowan_ast::DeclarationKind::MalformedHead(_) => {
                // Preserve original for malformed heads
                RcDoc::text(head.syntax().text().to_string())
            }
        }
    }

    fn reformat_soup(&self, soup: &rowan_ast::Soup) -> RcDoc<'static, ()> {
        let elements: Vec<_> = soup.elements().collect();
        if elements.is_empty() {
            return RcDoc::nil();
        }

        let mut docs = Vec::new();

        for (i, elem) in elements.iter().enumerate() {
            // Check spacing between elements
            if i > 0 {
                // ApplyTuple is a function call - no space before it (tight binding)
                let is_apply_tuple = matches!(elem, rowan_ast::Element::ApplyTuple(_));

                // Check if this is a dot-connected expression (lookup)
                // In eucalypt, dots are likely represented as operator identifiers
                let is_dot_op = match elem {
                    rowan_ast::Element::Name(n) => n.syntax().text() == ".",
                    _ => false,
                };

                let prev_was_dot = match &elements[i - 1] {
                    rowan_ast::Element::Name(n) => n.syntax().text() == ".",
                    _ => false,
                };

                // Add space unless:
                // - This is an ApplyTuple (function call, no space before)
                // - This or previous element is a dot operator
                if !is_apply_tuple && !is_dot_op && !prev_was_dot {
                    docs.push(RcDoc::space());
                }
            }

            docs.push(self.reformat_element(elem));
        }

        RcDoc::concat(docs)
    }

    fn reformat_element(&self, elem: &rowan_ast::Element) -> RcDoc<'static, ()> {
        match elem {
            rowan_ast::Element::Lit(lit) => self.reformat_literal(lit),
            rowan_ast::Element::Block(block) => self.reformat_block(block),
            rowan_ast::Element::List(list) => self.reformat_list(list),
            rowan_ast::Element::ParenExpr(pe) => self.reformat_paren_expr(pe),
            rowan_ast::Element::Name(name) => self.reformat_name(name),
            rowan_ast::Element::StringPattern(sp) => self.reformat_string_pattern(sp),
            rowan_ast::Element::ApplyTuple(at) => self.reformat_apply_tuple(at),
        }
    }

    fn reformat_literal(&self, lit: &rowan_ast::Literal) -> RcDoc<'static, ()> {
        // Literals are preserved as-is
        RcDoc::text(lit.syntax().text().to_string())
    }

    fn reformat_name(&self, name: &rowan_ast::Name) -> RcDoc<'static, ()> {
        RcDoc::text(name.syntax().text().to_string())
    }

    fn reformat_block(&self, block: &rowan_ast::Block) -> RcDoc<'static, ()> {
        let decls: Vec<_> = block.declarations().collect();

        if decls.is_empty() {
            return RcDoc::text("{ }");
        }

        // Check if block can be single line (short enough, no nested blocks)
        let single_line_len = self.estimate_block_single_line_len(block);
        let can_single_line = single_line_len <= self.config.line_width - 10
            && decls.len() <= 3
            && !self.has_nested_block(block);

        if can_single_line {
            // Single line: { a: 1, b: 2 }
            let mut docs = vec![RcDoc::text("{ ")];
            for (i, decl) in decls.iter().enumerate() {
                if i > 0 {
                    docs.push(RcDoc::text(", "));
                }
                docs.push(self.reformat_declaration_inline(decl));
            }
            docs.push(RcDoc::text(" }"));
            RcDoc::concat(docs)
        } else {
            // Multi-line block
            let indent = self.config.indent_size;
            let mut inner_docs = Vec::new();

            for decl in &decls {
                inner_docs.push(self.reformat_declaration(decl));
            }

            let inner = RcDoc::intersperse(inner_docs, RcDoc::hardline());

            RcDoc::text("{")
                .append(RcDoc::hardline())
                .append(inner.nest(indent as isize))
                .append(RcDoc::hardline())
                .append(RcDoc::text("}"))
        }
    }

    fn reformat_declaration_inline(&self, decl: &rowan_ast::Declaration) -> RcDoc<'static, ()> {
        let mut doc = RcDoc::nil();

        if let Some(head) = decl.head() {
            doc = doc.append(self.reformat_decl_head(&head));
        }

        doc = doc.append(RcDoc::text(": "));

        if let Some(body) = decl.body() {
            if let Some(soup) = body.soup() {
                doc = doc.append(self.reformat_soup(&soup));
            }
        }

        doc
    }

    fn reformat_list(&self, list: &rowan_ast::List) -> RcDoc<'static, ()> {
        let items: Vec<_> = list.items().collect();

        if items.is_empty() {
            return RcDoc::text("[]");
        }

        // Estimate single-line length
        let single_line_len: usize = items
            .iter()
            .map(|s| usize::from(s.syntax().text().len()) + 2)
            .sum();

        if single_line_len <= self.config.line_width - 10 {
            // Single line: [a, b, c]
            let docs: Vec<_> = items.iter().map(|s| self.reformat_soup(s)).collect();
            RcDoc::text("[")
                .append(RcDoc::intersperse(docs, RcDoc::text(", ")))
                .append(RcDoc::text("]"))
        } else {
            // Multi-line
            let indent = self.config.indent_size;
            let docs: Vec<_> = items.iter().map(|s| self.reformat_soup(s)).collect();

            RcDoc::text("[")
                .append(RcDoc::hardline())
                .append(
                    RcDoc::intersperse(docs, RcDoc::text(",").append(RcDoc::hardline()))
                        .nest(indent as isize),
                )
                .append(RcDoc::hardline())
                .append(RcDoc::text("]"))
        }
    }

    fn reformat_paren_expr(&self, pe: &rowan_ast::ParenExpr) -> RcDoc<'static, ()> {
        let inner = pe
            .soup()
            .map(|s| self.reformat_soup(&s))
            .unwrap_or_else(RcDoc::nil);
        RcDoc::text("(").append(inner).append(RcDoc::text(")"))
    }

    fn reformat_apply_tuple(&self, at: &rowan_ast::ApplyTuple) -> RcDoc<'static, ()> {
        let items: Vec<_> = at.items().collect();

        if items.is_empty() {
            return RcDoc::text("()");
        }

        let docs: Vec<_> = items.iter().map(|s| self.reformat_soup(s)).collect();
        RcDoc::text("(")
            .append(RcDoc::intersperse(docs, RcDoc::text(", ")))
            .append(RcDoc::text(")"))
    }

    fn reformat_string_pattern(&self, sp: &rowan_ast::StringPattern) -> RcDoc<'static, ()> {
        // String patterns are preserved as-is
        RcDoc::text(sp.syntax().text().to_string())
    }

    fn reformat_block_metadata(&self, meta: &rowan_ast::BlockMetadata) -> RcDoc<'static, ()> {
        if let Some(soup) = meta.soup() {
            RcDoc::text("` ").append(self.reformat_soup(&soup))
        } else {
            RcDoc::nil()
        }
    }

    fn reformat_decl_metadata(&self, meta: &rowan_ast::DeclarationMetadata) -> RcDoc<'static, ()> {
        if let Some(soup) = meta.soup() {
            RcDoc::text("` ").append(self.reformat_soup(&soup))
        } else {
            RcDoc::nil()
        }
    }

    // ========================================================================
    // Conservative Mode
    // ========================================================================

    fn conservative_format_unit(&self, unit: &rowan_ast::Unit) -> RcDoc<'static, ()> {
        // In conservative mode, we preserve structure but fix violations
        self.fix_whitespace_violations(unit.syntax())
    }

    fn conservative_format_soup(&self, soup: &rowan_ast::Soup) -> RcDoc<'static, ()> {
        self.fix_whitespace_violations(soup.syntax())
    }

    /// Fix whitespace violations in a syntax node
    fn fix_whitespace_violations(&self, node: &SyntaxNode) -> RcDoc<'static, ()> {
        let mut result = String::new();
        self.process_node_conservative(node, &mut result);
        RcDoc::text(result)
    }

    fn process_node_conservative(&self, node: &SyntaxNode, result: &mut String) {
        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    self.process_node_conservative(&n, result);
                }
                rowan::NodeOrToken::Token(t) => {
                    self.process_token_conservative(&t, result);
                }
            }
        }
    }

    fn process_token_conservative(&self, token: &SyntaxToken, result: &mut String) {
        let text = token.text();
        let kind = token.kind();

        match kind {
            SyntaxKind::WHITESPACE => {
                // Convert tabs to spaces and normalize excessive whitespace
                let normalized = self.normalize_whitespace(text);
                result.push_str(&normalized);
            }
            SyntaxKind::COLON => {
                // Colon should have no space before, one space after
                // (The space after is handled by following whitespace)
                result.push_str(text);
            }
            SyntaxKind::COMMA => {
                // Comma should have no space before, one space after
                result.push_str(text);
            }
            _ => {
                result.push_str(text);
            }
        }
    }

    /// Normalize whitespace: convert tabs to spaces, normalize runs
    fn normalize_whitespace(&self, ws: &str) -> String {
        let mut result = String::new();
        let mut col = 0;
        let mut has_newline = false;

        for c in ws.chars() {
            match c {
                '\n' => {
                    result.push('\n');
                    has_newline = true;
                    col = 0;
                }
                '\t' => {
                    // Convert tab to spaces (align to indent_size boundaries)
                    let spaces = self.config.indent_size - (col % self.config.indent_size);
                    for _ in 0..spaces {
                        result.push(' ');
                    }
                    col += spaces;
                }
                ' ' => {
                    result.push(' ');
                    col += 1;
                }
                '\r' => {
                    // Ignore carriage returns
                }
                _ => {
                    result.push(c);
                    col += 1;
                }
            }
        }

        // Don't collapse all whitespace to single space on newlines
        if !has_newline && !result.contains('\n') {
            // For inline whitespace, normalize excessive spaces (5+) to single
            let space_count = result.chars().filter(|c| *c == ' ').count();
            if space_count >= 5 {
                result = " ".to_string();
            }
        }

        result
    }

    // ========================================================================
    // Helper Methods
    // ========================================================================

    fn estimate_block_single_line_len(&self, block: &rowan_ast::Block) -> usize {
        let mut len = 4; // "{ " and " }"
        for decl in block.declarations() {
            len += usize::from(decl.syntax().text().len()) + 2; // ", " separator
        }
        len
    }

    fn has_nested_block(&self, block: &rowan_ast::Block) -> bool {
        for decl in block.declarations() {
            if let Some(body) = decl.body() {
                if let Some(soup) = body.soup() {
                    for elem in soup.elements() {
                        if matches!(elem, rowan_ast::Element::Block(_)) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }
}

/// Format a source file
pub fn format_source(source: &str, config: &FormatterConfig) -> Result<String, String> {
    use crate::syntax::rowan::parse_unit;

    let parsed = parse_unit(source);
    if !parsed.errors().is_empty() {
        return Err(format!("Parse errors: {:?}", parsed.errors()));
    }

    let unit = parsed.tree();
    let formatter = Formatter::new(config.clone());
    Ok(formatter.format_unit(&unit))
}

/// Check if a source file needs formatting
pub fn needs_formatting(source: &str, config: &FormatterConfig) -> Result<bool, String> {
    let formatted = format_source(source, config)?;
    Ok(formatted != source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_whitespace_tabs() {
        let config = FormatterConfig::default();
        let formatter = Formatter::new(config);

        let result = formatter.normalize_whitespace("\t");
        assert_eq!(result, "  ");

        let result = formatter.normalize_whitespace(" \t");
        assert_eq!(result, "  ");
    }

    #[test]
    fn test_normalize_whitespace_excessive() {
        let config = FormatterConfig::default();
        let formatter = Formatter::new(config);

        let result = formatter.normalize_whitespace("     ");
        assert_eq!(result, " ");
    }
}
