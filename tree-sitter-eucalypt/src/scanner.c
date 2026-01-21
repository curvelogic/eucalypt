/**
 * External scanner for Eucalypt tree-sitter grammar
 *
 * This scanner detects declaration boundaries by looking ahead for
 * patterns like `identifier:` or `(op_pattern):` that signal the start
 * of a new declaration.
 */

#include "tree_sitter/parser.h"
#include <wctype.h>
#include <stdio.h>

enum TokenType {
    DECLARATION_END,
};

// Check if character is an identifier start
static bool is_identifier_start(int32_t c) {
    if (c == 0x2022) return true;  // •
    if (c == '$' || c == '?' || c == '_') return true;
    return iswalpha(c);
}

// Check if character is an identifier continuation
static bool is_identifier_cont(int32_t c) {
    if (is_identifier_start(c)) return true;
    if (c == '!' || c == '*' || c == '-') return true;
    return iswdigit(c);
}

// Check if character is an operator character
static bool is_operator_char(int32_t c) {
    // Common ASCII operators
    if (c == '.' || c == '!' || c == '@' || c == '%' || c == '^' ||
        c == '&' || c == '*' || c == '|' || c == '>' || c == '<' ||
        c == '/' || c == '+' || c == '=' || c == '-' || c == '~' ||
        c == ';') {
        return true;
    }
    // Common Unicode operators
    if (c >= 0x2190 && c <= 0x21FF) return true;  // Arrows
    if (c >= 0x2200 && c <= 0x22FF) return true;  // Mathematical operators
    if (c >= 0x27F0 && c <= 0x27FF) return true;  // Supplemental arrows
    if (c == 0x2227 || c == 0x2228 || c == 0x2218) return true;  // ∧ ∨ ∘
    return false;
}

// Skip whitespace, return true if any was skipped
static bool skip_whitespace(TSLexer *lexer) {
    bool skipped = false;
    while (iswspace(lexer->lookahead)) {
        lexer->advance(lexer, true);
        skipped = true;
    }
    return skipped;
}

// Skip an identifier
static bool skip_identifier(TSLexer *lexer) {
    if (!is_identifier_start(lexer->lookahead)) return false;
    while (is_identifier_cont(lexer->lookahead)) {
        lexer->advance(lexer, true);
    }
    return true;
}

// Skip an operator
static bool skip_operator(TSLexer *lexer) {
    if (!is_operator_char(lexer->lookahead)) return false;
    while (is_operator_char(lexer->lookahead)) {
        lexer->advance(lexer, true);
    }
    return true;
}

// Check if we're at the start of a declaration
// Patterns: `identifier:`, `'quoted':`, `identifier(...):`, `(op_pattern):`
static bool at_declaration_start(TSLexer *lexer) {
    // Skip leading whitespace
    skip_whitespace(lexer);

    // Check for metadata (backtick)
    if (lexer->lookahead == '`') {
        // Metadata can precede a declaration
        return true;
    }

    // Check for identifier
    if (is_identifier_start(lexer->lookahead)) {
        skip_identifier(lexer);
        skip_whitespace(lexer);

        // identifier: or identifier(...):
        if (lexer->lookahead == ':') return true;
        if (lexer->lookahead == '(') {
            // Could be function declaration f(x):
            // Skip to matching paren
            int depth = 1;
            lexer->advance(lexer, true);
            while (depth > 0 && !lexer->eof(lexer)) {
                if (lexer->lookahead == '(') depth++;
                else if (lexer->lookahead == ')') depth--;
                lexer->advance(lexer, true);
            }
            skip_whitespace(lexer);
            if (lexer->lookahead == ':') return true;
        }
        return false;
    }

    // Check for quoted identifier
    if (lexer->lookahead == '\'') {
        lexer->advance(lexer, true);
        while (lexer->lookahead != '\'' && !lexer->eof(lexer)) {
            lexer->advance(lexer, true);
        }
        if (lexer->lookahead == '\'') {
            lexer->advance(lexer, true);
            skip_whitespace(lexer);
            if (lexer->lookahead == ':') return true;
            if (lexer->lookahead == '(') {
                // quoted function decl
                int depth = 1;
                lexer->advance(lexer, true);
                while (depth > 0 && !lexer->eof(lexer)) {
                    if (lexer->lookahead == '(') depth++;
                    else if (lexer->lookahead == ')') depth--;
                    lexer->advance(lexer, true);
                }
                skip_whitespace(lexer);
                if (lexer->lookahead == ':') return true;
            }
        }
        return false;
    }

    // Check for operator declaration (x op y): or (op x): or (x op):
    if (lexer->lookahead == '(') {
        lexer->advance(lexer, true);
        skip_whitespace(lexer);

        // Skip the content - look for pattern of identifiers and operators
        bool has_identifier = false;
        bool has_operator = false;

        while (lexer->lookahead != ')' && !lexer->eof(lexer)) {
            skip_whitespace(lexer);
            if (is_identifier_start(lexer->lookahead)) {
                skip_identifier(lexer);
                has_identifier = true;
            } else if (is_operator_char(lexer->lookahead)) {
                skip_operator(lexer);
                has_operator = true;
            } else {
                break;
            }
        }

        if (lexer->lookahead == ')') {
            lexer->advance(lexer, true);
            skip_whitespace(lexer);
            // An operator declaration has both identifier and operator
            if (has_identifier && has_operator && lexer->lookahead == ':') {
                return true;
            }
        }
        return false;
    }

    return false;
}

void *tree_sitter_eucalypt_external_scanner_create(void) {
    return NULL;
}

void tree_sitter_eucalypt_external_scanner_destroy(void *payload) {
}

unsigned tree_sitter_eucalypt_external_scanner_serialize(void *payload, char *buffer) {
    return 0;
}

void tree_sitter_eucalypt_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
}

bool tree_sitter_eucalypt_external_scanner_scan(
    void *payload,
    TSLexer *lexer,
    const bool *valid_symbols
) {
    if (valid_symbols[DECLARATION_END]) {
        // Mark current position
        lexer->mark_end(lexer);

        // Look ahead to see if we're at a declaration start
        if (at_declaration_start(lexer)) {
            lexer->result_symbol = DECLARATION_END;
            return true;
        }
    }
    return false;
}
