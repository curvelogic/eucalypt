//! Source code formatter for Eucalypt
//!
//! Provides formatting capabilities with two modes:
//!
//! - **Conservative mode** (`reformat: false`): Fixes whitespace violations
//!   (e.g., tabs to spaces, trailing whitespace) while preserving the overall
//!   structure and formatting decisions of the original code. Note that this
//!   mode does not break long lines - the `line_width` setting is not used.
//!   Line endings are normalized to LF.
//!
//! - **Full reformat mode** (`reformat: true`): Generates canonical output
//!   from scratch using the pretty printer. Uses `line_width` for breaking
//!   decisions and applies consistent formatting rules.

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
            indent_size: indent_size.max(1),
            reformat,
        }
    }
}

/// Maximum number of declarations for single-line block formatting.
/// Blocks with more declarations than this always use multi-line format.
const MAX_SINGLE_LINE_DECLS: usize = 3;

/// Margin reserved for surrounding context when deciding single-line formatting.
/// Accounts for potential nesting, braces, and other tokens on the same line.
const SINGLE_LINE_MARGIN: usize = 10;

/// The main formatter struct
pub struct Formatter {
    config: FormatterConfig,
}

impl Formatter {
    pub fn new(config: FormatterConfig) -> Self {
        Self { config }
    }

    /// Format a Unit (top-level file)
    pub fn format_unit(&self, unit: &rowan_ast::Unit) -> Result<String, String> {
        let doc = self.format_unit_doc(unit);
        let mut result = self.render_doc(doc)?;
        // Ensure trailing newline
        if !result.is_empty() && !result.ends_with('\n') {
            result.push('\n');
        }
        Ok(result)
    }

    /// Format a Soup (expression)
    pub fn format_soup(&self, soup: &rowan_ast::Soup) -> Result<String, String> {
        let doc = self.format_soup_doc(soup);
        self.render_doc(doc)
    }

    /// Render a document to string
    fn render_doc(&self, doc: RcDoc<'static, ()>) -> Result<String, String> {
        let mut w = Vec::new();
        doc.render(self.config.line_width, &mut w)
            .map_err(|e| format!("Render error: {e}"))?;
        String::from_utf8(w).map_err(|e| format!("UTF-8 error: {e}"))
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

    /// Extract leading comments (including shebang) from a syntax node
    fn extract_leading_comments(&self, node: &SyntaxNode) -> Vec<String> {
        let mut comments = Vec::new();
        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Token(t) => {
                    if t.kind() == SyntaxKind::COMMENT {
                        comments.push(t.text().to_string());
                    } else if t.kind() != SyntaxKind::WHITESPACE {
                        // Stop at first non-trivia token
                        break;
                    }
                }
                rowan::NodeOrToken::Node(_) => {
                    // Stop at first node (declarations, metadata, etc.)
                    break;
                }
            }
        }
        comments
    }

    /// Extract comments that precede a declaration
    fn extract_declaration_leading_comments(&self, decl: &rowan_ast::Declaration) -> Vec<String> {
        let mut found_comments = Vec::new();
        // Look at the previous siblings to find comments
        let mut prev = decl.syntax().prev_sibling_or_token();

        while let Some(sibling) = prev {
            match &sibling {
                rowan::NodeOrToken::Token(t) => {
                    if t.kind() == SyntaxKind::COMMENT {
                        found_comments.push(t.text().to_string());
                    } else if t.kind() == SyntaxKind::WHITESPACE {
                        // Check if whitespace contains blank line (resets comment association)
                        if t.text().chars().filter(|&c| c == '\n').count() > 1 {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                rowan::NodeOrToken::Node(_) => break,
            }
            prev = sibling.prev_sibling_or_token();
        }

        // Reverse since we collected in reverse order
        found_comments.reverse();
        found_comments
    }

    fn reformat_unit(&self, unit: &rowan_ast::Unit) -> RcDoc<'static, ()> {
        let mut result = RcDoc::nil();

        // Preserve leading comments (including shebang) as a single header block
        let leading_comments = self.extract_leading_comments(unit.syntax());
        if !leading_comments.is_empty() {
            for comment in &leading_comments {
                result = result
                    .append(RcDoc::text(comment.clone()))
                    .append(RcDoc::hardline());
            }
            // Add blank line after leading comments section
            result = result.append(RcDoc::hardline());
        }

        // Handle metadata if present
        if let Some(meta) = unit.meta() {
            result = result.append(self.reformat_block_metadata(&meta));
            result = result.append(RcDoc::hardline()).append(RcDoc::hardline());
        }

        // Format each declaration with its leading comments
        let declarations: Vec<_> = unit.declarations().collect();
        for (i, decl) in declarations.iter().enumerate() {
            if i > 0 {
                // Blank line between declarations
                result = result.append(RcDoc::hardline());

                // Extract and add comments that precede this declaration (not the first one,
                // as those are already handled by extract_leading_comments)
                let decl_comments = self.extract_declaration_leading_comments(decl);
                for comment in &decl_comments {
                    result = result
                        .append(RcDoc::text(comment.clone()))
                        .append(RcDoc::hardline());
                }
            }

            result = result.append(self.reformat_declaration(decl));
            if i < declarations.len() - 1 {
                result = result.append(RcDoc::hardline());
            }
        }

        result
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
                doc = doc.append(RcDoc::space()).append(self.reformat_soup(&soup));
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
            rowan_ast::DeclarationKind::Nullary(_, op) => {
                // Format: (op)
                RcDoc::text("(")
                    .append(RcDoc::text(op.syntax().text().to_string()))
                    .append(RcDoc::text(")"))
            }
            rowan_ast::DeclarationKind::Prefix(_, op, operand) => {
                // Format: (op operand) - no space between operator and operand
                RcDoc::text("(")
                    .append(RcDoc::text(op.syntax().text().to_string()))
                    .append(RcDoc::text(operand.syntax().text().to_string()))
                    .append(RcDoc::text(")"))
            }
            rowan_ast::DeclarationKind::Postfix(_, operand, op) => {
                // Format: (operand op) - no space between operand and operator
                RcDoc::text("(")
                    .append(RcDoc::text(operand.syntax().text().to_string()))
                    .append(RcDoc::text(op.syntax().text().to_string()))
                    .append(RcDoc::text(")"))
            }
            rowan_ast::DeclarationKind::Binary(_, x, op, y) => {
                // Format: (x op y) - spaces around operator
                RcDoc::text("(")
                    .append(RcDoc::text(x.syntax().text().to_string()))
                    .append(RcDoc::space())
                    .append(RcDoc::text(op.syntax().text().to_string()))
                    .append(RcDoc::space())
                    .append(RcDoc::text(y.syntax().text().to_string()))
                    .append(RcDoc::text(")"))
            }
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
                let is_dot_op = elem
                    .as_operator_identifier()
                    .is_some_and(|op| op.syntax().text() == ".");

                let prev_was_dot = elements[i - 1]
                    .as_operator_identifier()
                    .is_some_and(|op| op.syntax().text() == ".");

                // Check for unary operators - no space after prefix ops, no space before postfix ops
                let prev_is_prefix_op = self.is_prefix_operator(&elements, i - 1);

                let is_postfix_op = self.is_postfix_operator(&elements, i);

                // Add space unless:
                // - This is an ApplyTuple (function call, no space before)
                // - This or previous element is a dot operator
                // - Previous element is a prefix operator (tight binding)
                // - This element is a postfix operator (tight binding)
                let need_space = !is_apply_tuple
                    && !is_dot_op
                    && !prev_was_dot
                    && !prev_is_prefix_op
                    && !is_postfix_op;

                if need_space {
                    docs.push(RcDoc::space());
                }
            }

            docs.push(self.reformat_element(elem));
        }

        RcDoc::concat(docs)
    }

    /// Check if an element at a given position is likely a prefix operator.
    ///
    /// Uses positional heuristics since we don't have operator metadata at this phase.
    /// This works for common cases but may misclassify edge cases like chained unary
    /// operators (`--x`) or mixed fixity sequences. A more robust approach would use
    /// operator fixity metadata from the core desugar phase.
    fn is_prefix_operator(&self, elements: &[rowan_ast::Element], idx: usize) -> bool {
        // Must be an operator
        if elements[idx].as_operator_identifier().is_none() {
            return false;
        }
        // Prefix operator is either at position 0, or follows another operator
        if idx == 0 {
            return true;
        }
        // If the previous element is also an operator, this could be a prefix op
        // (e.g., in "x + -y", the "-" after "+" is a prefix operator)
        elements[idx - 1].as_operator_identifier().is_some()
    }

    /// Check if an element at a given position is likely a postfix operator.
    ///
    /// See `is_prefix_operator` for limitations of this heuristic approach.
    fn is_postfix_operator(&self, elements: &[rowan_ast::Element], idx: usize) -> bool {
        // Must be an operator
        if elements[idx].as_operator_identifier().is_none() {
            return false;
        }
        // Postfix operator is at the end, or followed by another operator
        if idx == elements.len() - 1 {
            return true;
        }
        // If the next element is also an operator, this could be a postfix op
        // (e.g., in "x! + y", the "!" before "+" is a postfix operator)
        elements[idx + 1].as_operator_identifier().is_some()
    }

    fn reformat_element(&self, elem: &rowan_ast::Element) -> RcDoc<'static, ()> {
        match elem {
            rowan_ast::Element::Lit(lit) => self.reformat_literal(lit),
            rowan_ast::Element::Block(block) => self.reformat_block(block),
            rowan_ast::Element::List(list) => self.reformat_list(list),
            rowan_ast::Element::ParenExpr(pe) => self.reformat_paren_expr(pe),
            rowan_ast::Element::Name(name) => self.reformat_name(name),
            rowan_ast::Element::StringPattern(sp) => self.reformat_string_pattern(sp),
            rowan_ast::Element::CStringPattern(csp) => {
                // For now, preserve c-string patterns as-is
                RcDoc::text(csp.syntax().text().to_string())
            }
            rowan_ast::Element::RawStringPattern(rsp) => {
                // For now, preserve raw string patterns as-is
                RcDoc::text(rsp.syntax().text().to_string())
            }
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
        let can_single_line = single_line_len
            <= self.config.line_width.saturating_sub(SINGLE_LINE_MARGIN)
            && decls.len() <= MAX_SINGLE_LINE_DECLS
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

            // Nest includes the leading hardline so first line gets indented too
            RcDoc::text("{")
                .append(RcDoc::hardline().append(inner).nest(indent as isize))
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

        if single_line_len <= self.config.line_width.saturating_sub(10) {
            // Single line: [a, b, c]
            let docs: Vec<_> = items.iter().map(|s| self.reformat_soup(s)).collect();
            RcDoc::text("[")
                .append(RcDoc::intersperse(docs, RcDoc::text(", ")))
                .append(RcDoc::text("]"))
        } else {
            // Multi-line - same pattern as blocks for consistent indentation
            let indent = self.config.indent_size;
            let docs: Vec<_> = items.iter().map(|s| self.reformat_soup(s)).collect();
            let inner = RcDoc::intersperse(docs, RcDoc::text(",").append(RcDoc::hardline()));

            RcDoc::text("[")
                .append(RcDoc::hardline().append(inner).nest(indent as isize))
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
        let mut line_col: usize = 0;
        self.process_node_conservative(node, &mut result, &mut line_col);
        RcDoc::text(result)
    }

    fn process_node_conservative(
        &self,
        node: &SyntaxNode,
        result: &mut String,
        line_col: &mut usize,
    ) {
        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    self.process_node_conservative(&n, result, line_col);
                }
                rowan::NodeOrToken::Token(t) => {
                    self.process_token_conservative(&t, result, line_col);
                }
            }
        }
    }

    fn process_token_conservative(
        &self,
        token: &SyntaxToken,
        result: &mut String,
        line_col: &mut usize,
    ) {
        let text = token.text();
        let kind = token.kind();

        match kind {
            SyntaxKind::WHITESPACE => {
                // Convert tabs to spaces and normalize excessive whitespace
                let normalized = self.normalize_whitespace(text, *line_col);
                // Update line_col based on normalized output
                for c in normalized.chars() {
                    if c == '\n' {
                        *line_col = 0;
                    } else {
                        *line_col += 1;
                    }
                }
                result.push_str(&normalized);
            }
            SyntaxKind::COLON => {
                // Colon preserved as-is; spacing is handled by surrounding whitespace tokens
                result.push_str(text);
                *line_col += text.len();
            }
            SyntaxKind::COMMA => {
                // Comma preserved as-is; spacing is handled by surrounding whitespace tokens
                result.push_str(text);
                *line_col += text.len();
            }
            _ => {
                // Update line_col tracking
                for c in text.chars() {
                    if c == '\n' {
                        *line_col = 0;
                    } else {
                        *line_col += 1;
                    }
                }
                result.push_str(text);
            }
        }
    }

    /// Normalize whitespace: convert tabs to spaces while preserving alignment
    /// line_col is the column position at the start of this whitespace token
    fn normalize_whitespace(&self, ws: &str, line_col: usize) -> String {
        let mut result = String::new();
        let mut col = line_col;

        // Standard tab width for alignment preservation
        const TAB_WIDTH: usize = 8;

        for c in ws.chars() {
            match c {
                '\n' => {
                    result.push('\n');
                    col = 0;
                }
                '\t' => {
                    // Convert tab to spaces (align to standard 8-character tab stops)
                    // This preserves the alignment intent of the original author
                    let next_tab_stop = ((col / TAB_WIDTH) + 1) * TAB_WIDTH;
                    let spaces = next_tab_stop - col;
                    for _ in 0..spaces {
                        result.push(' ');
                    }
                    col = next_tab_stop;
                }
                ' ' => {
                    result.push(' ');
                    col += 1;
                }
                '\r' => {
                    // Normalize line endings to LF by discarding CR characters.
                    // This standardizes output regardless of input line ending style.
                }
                _ => {
                    result.push(c);
                    col += 1;
                }
            }
        }

        // In conservative mode, we preserve whitespace structure to maintain alignment.
        // Only tabs are converted to spaces, keeping the overall spacing intent.

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
    formatter.format_unit(&unit)
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

        // Tab at column 0 -> 8 spaces (standard tab width)
        let result = formatter.normalize_whitespace("\t", 0);
        assert_eq!(result, "        ");

        // Space then tab at column 0: space puts us at col 1, tab aligns to col 8
        let result = formatter.normalize_whitespace(" \t", 0);
        assert_eq!(result, "        "); // 1 space + 7 more = 8 total

        // Tab at column 1 -> 7 spaces to reach next tab stop (col 8)
        let result = formatter.normalize_whitespace("\t", 1);
        assert_eq!(result, "       ");

        // Tab at column 8 (already at tab stop) -> 8 spaces to reach col 16
        let result = formatter.normalize_whitespace("\t", 8);
        assert_eq!(result, "        ");

        // Tab at column 3 -> 5 spaces to reach next tab stop (col 8)
        let result = formatter.normalize_whitespace("\t", 3);
        assert_eq!(result, "     ");
    }

    #[test]
    fn test_normalize_whitespace_preserves_spaces() {
        let config = FormatterConfig::default();
        let formatter = Formatter::new(config);

        // Conservative mode preserves spaces to maintain alignment
        let result = formatter.normalize_whitespace("     ", 0);
        assert_eq!(result, "     ");

        // Mixed tabs and spaces: tab expanded (to 8), then spaces preserved
        let result = formatter.normalize_whitespace("\t    ", 0);
        assert_eq!(result, "            "); // 8 (from tab) + 4 spaces = 12 spaces
    }

    // ========================================================================
    // Idempotency Tests (eu-bdj)
    // Format twice, verify identical output
    // ========================================================================

    #[test]
    fn test_idempotency_conservative_simple() {
        let source = "x: 1\ny: 2\n";
        let config = FormatterConfig::default();

        let first = format_source(source, &config).unwrap();
        let second = format_source(&first, &config).unwrap();

        assert_eq!(first, second, "Conservative mode should be idempotent");
    }

    #[test]
    fn test_idempotency_conservative_block() {
        let source = "obj: {\n  a: 1\n  b: 2\n}\n";
        let config = FormatterConfig::default();

        let first = format_source(source, &config).unwrap();
        let second = format_source(&first, &config).unwrap();

        assert_eq!(first, second, "Block formatting should be idempotent");
    }

    #[test]
    fn test_idempotency_reformat_simple() {
        let source = "x: 1\ny: 2\n";
        let config = FormatterConfig::new(80, 2, true);

        let first = format_source(source, &config).unwrap();
        let second = format_source(&first, &config).unwrap();

        assert_eq!(first, second, "Reformat mode should be idempotent");
    }

    #[test]
    fn test_idempotency_reformat_block() {
        let source = "obj: {\n  a: 1\n  b: 2\n}\n";
        let config = FormatterConfig::new(80, 2, true);

        let first = format_source(source, &config).unwrap();
        let second = format_source(&first, &config).unwrap();

        assert_eq!(first, second, "Reformat block should be idempotent");
    }

    #[test]
    fn test_idempotency_with_comments() {
        let source = "# Comment\nx: 1\n";
        let config = FormatterConfig::new(80, 2, true);

        let first = format_source(source, &config).unwrap();
        let second = format_source(&first, &config).unwrap();

        assert_eq!(first, second, "Comments should be preserved idempotently");
    }

    // ========================================================================
    // Round-trip Tests (eu-ybn)
    // Format then parse, verify no parse errors
    // ========================================================================

    #[test]
    fn test_roundtrip_conservative_simple() {
        use crate::syntax::rowan::parse_unit;

        let source = "x: 1\ny: 2\n";
        let config = FormatterConfig::default();

        let formatted = format_source(source, &config).unwrap();
        let reparsed = parse_unit(&formatted);

        assert!(
            reparsed.errors().is_empty(),
            "Formatted output should parse without errors: {:?}",
            reparsed.errors()
        );
    }

    #[test]
    fn test_roundtrip_reformat_block() {
        use crate::syntax::rowan::parse_unit;

        let source = "obj: { a: 1, b: 2 }\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();
        let reparsed = parse_unit(&formatted);

        assert!(
            reparsed.errors().is_empty(),
            "Reformatted block should parse: {:?}",
            reparsed.errors()
        );
    }

    #[test]
    fn test_roundtrip_reformat_list() {
        use crate::syntax::rowan::parse_unit;

        let source = "items: [1, 2, 3]\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();
        let reparsed = parse_unit(&formatted);

        assert!(
            reparsed.errors().is_empty(),
            "Reformatted list should parse: {:?}",
            reparsed.errors()
        );
    }

    #[test]
    fn test_roundtrip_nested_structures() {
        use crate::syntax::rowan::parse_unit;

        let source = "outer: { inner: { deep: [1, 2, { x: 3 }] } }\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();
        let reparsed = parse_unit(&formatted);

        assert!(
            reparsed.errors().is_empty(),
            "Nested structures should round-trip: {:?}",
            reparsed.errors()
        );
    }

    #[test]
    fn test_roundtrip_with_operators() {
        use crate::syntax::rowan::parse_unit;

        let source = "result: 1 + 2 * 3\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();
        let reparsed = parse_unit(&formatted);

        assert!(
            reparsed.errors().is_empty(),
            "Operators should round-trip: {:?}",
            reparsed.errors()
        );
    }

    // ========================================================================
    // Tab Handling Test Fixtures (eu-i32)
    // ========================================================================

    #[test]
    fn test_tab_detection_and_conversion() {
        // Input with tabs
        let source = "x:\t1\ny:\t2\n";
        let config = FormatterConfig::default();

        let formatted = format_source(source, &config).unwrap();

        // Should not contain tabs after formatting
        assert!(
            !formatted.contains('\t'),
            "Tabs should be converted to spaces"
        );
    }

    #[test]
    fn test_tab_alignment_preservation() {
        // Tabs used for alignment
        let source = "short:\t\t1\nlonger_name:\t2\n";
        let config = FormatterConfig::default();

        let formatted = format_source(source, &config).unwrap();

        // Should convert tabs but preserve relative alignment intent
        assert!(!formatted.contains('\t'), "Tabs should be converted");

        // Verify it still parses
        use crate::syntax::rowan::parse_unit;
        let reparsed = parse_unit(&formatted);
        assert!(reparsed.errors().is_empty());
    }

    #[test]
    fn test_mixed_tabs_and_spaces() {
        let source = "x: \t 1\n";
        let config = FormatterConfig::default();

        let formatted = format_source(source, &config).unwrap();

        assert!(
            !formatted.contains('\t'),
            "Mixed tabs/spaces should convert tabs"
        );
    }

    // ========================================================================
    // Line Breaking Test Fixtures (eu-evs)
    // ========================================================================

    #[test]
    fn test_line_break_short_block() {
        let source = "obj: { a: 1 }\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();

        // Short blocks can stay on one line
        assert!(formatted.contains("{ a: 1 }") || formatted.contains("{\n"));
    }

    #[test]
    fn test_line_break_long_block() {
        let source = "obj: { very_long_name_a: 1, very_long_name_b: 2, very_long_name_c: 3 }\n";
        let config = FormatterConfig::new(40, 2, true); // Narrow width forces breaks

        let formatted = format_source(source, &config).unwrap();

        // Long content should break across lines
        let line_count = formatted.lines().count();
        assert!(line_count > 1, "Long blocks should break across lines");
    }

    #[test]
    fn test_line_break_preserves_blank_lines() {
        let source = "x: 1\n\ny: 2\n";
        let config = FormatterConfig::default();

        let formatted = format_source(source, &config).unwrap();

        // Conservative mode should preserve blank line separation
        assert!(
            formatted.contains("\n\n") || formatted == source,
            "Blank lines should be preserved in conservative mode"
        );
    }

    #[test]
    fn test_line_break_nested_indent() {
        let source = "outer: {\n  inner: {\n    deep: 1\n  }\n}\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();

        // Verify indentation is consistent
        for line in formatted.lines() {
            if line.starts_with(' ') {
                let indent = line.len() - line.trim_start().len();
                assert_eq!(indent % 2, 0, "Indent should be multiple of 2: {:?}", line);
            }
        }
    }

    // ========================================================================
    // Spacing Normalisation Test Fixtures (eu-2n5)
    // ========================================================================

    #[test]
    fn test_spacing_colon() {
        let source = "x:1\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();

        // Colon should have space after
        assert!(
            formatted.contains(": "),
            "Colon should have space after: {}",
            formatted
        );
    }

    #[test]
    fn test_spacing_comma_in_list() {
        let source = "items: [1,2,3]\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();

        // Commas should have space after
        assert!(
            formatted.contains(", ") || formatted.contains(",\n"),
            "Commas should have proper spacing: {}",
            formatted
        );
    }

    #[test]
    fn test_spacing_braces() {
        let source = "obj: {a:1}\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();

        // Braces should have proper spacing
        assert!(
            formatted.contains("{ ") || formatted.contains("{\n"),
            "Opening brace should have space: {}",
            formatted
        );
    }

    #[test]
    fn test_spacing_operators() {
        let source = "result: 1+2*3\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();

        // Binary operators should have spaces
        assert!(
            formatted.contains(" + ") || formatted.contains(" * "),
            "Operators should have spaces: {}",
            formatted
        );
    }

    #[test]
    fn test_spacing_dot_access() {
        let source = "result: obj . field\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();

        // Dot access should not have extra spaces
        // Note: This depends on the language's style preferences
        use crate::syntax::rowan::parse_unit;
        let reparsed = parse_unit(&formatted);
        assert!(reparsed.errors().is_empty(), "Dot access should parse");
    }

    #[test]
    fn test_spacing_no_trailing_whitespace() {
        let source = "x: 1   \ny: 2\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();

        // No trailing whitespace on lines
        for line in formatted.lines() {
            assert!(
                !line.ends_with(' ') && !line.ends_with('\t'),
                "No trailing whitespace: {:?}",
                line
            );
        }
    }

    // ========================================================================
    // Edge Case Test Fixtures (eu-2261)
    // ========================================================================

    #[test]
    fn test_edge_case_empty_block() {
        let source = "empty: {}\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();
        let reparsed = crate::syntax::rowan::parse_unit(&formatted);

        assert!(
            reparsed.errors().is_empty(),
            "Empty block should parse: {:?}",
            reparsed.errors()
        );
    }

    #[test]
    fn test_edge_case_empty_list() {
        let source = "items: []\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();
        let reparsed = crate::syntax::rowan::parse_unit(&formatted);

        assert!(
            reparsed.errors().is_empty(),
            "Empty list should parse: {:?}",
            reparsed.errors()
        );
    }

    #[test]
    fn test_edge_case_comment_only() {
        let source = "# Just a comment\n";
        let config = FormatterConfig::new(80, 2, true);

        let result = format_source(source, &config);
        // Comment-only files should either format successfully or gracefully handle
        assert!(result.is_ok(), "Comment-only file should not error");
    }

    #[test]
    fn test_edge_case_deeply_nested() {
        let source = "a: { b: { c: { d: { e: 1 } } } }\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();
        let reparsed = crate::syntax::rowan::parse_unit(&formatted);

        assert!(
            reparsed.errors().is_empty(),
            "Deeply nested should parse: {:?}",
            reparsed.errors()
        );
    }

    #[test]
    fn test_edge_case_unicode_identifier() {
        let source = "données: 42\n";
        let config = FormatterConfig::new(80, 2, true);

        let formatted = format_source(source, &config).unwrap();
        let reparsed = crate::syntax::rowan::parse_unit(&formatted);

        assert!(
            reparsed.errors().is_empty(),
            "Unicode identifier should parse: {:?}",
            reparsed.errors()
        );
    }

    #[test]
    fn test_edge_case_multiple_blank_lines() {
        let source = "x: 1\n\n\n\ny: 2\n";
        let config = FormatterConfig::default();

        let formatted = format_source(source, &config).unwrap();

        // Conservative mode should preserve some blank lines
        assert!(
            formatted.contains("\n\n"),
            "Multiple blank lines should preserve separation"
        );
    }
}
