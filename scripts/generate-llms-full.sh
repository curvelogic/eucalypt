#!/usr/bin/env bash
# Generate llms-full.txt by concatenating all user-facing documentation
# into a single markdown file for AI agents and coding assistants.
#
# Usage: ./scripts/generate-llms-full.sh [output-path]
# Default output: doc/llms-full.txt

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DOC_DIR="$(cd "$SCRIPT_DIR/../doc" && pwd)"
OUTPUT="${1:-$DOC_DIR/llms-full.txt}"

{
    echo "# Eucalypt - Complete Documentation"
    echo ""
    echo "> This file contains the complete eucalypt documentation concatenated"
    echo "> into a single file for use by AI agents and coding assistants."
    echo "> Generated from the eucalypt documentation source."
    echo ""

    # Welcome / Getting Started
    for f in welcome/what-is-eucalypt.md welcome/quick-start.md welcome/by-example.md; do
        if [ -f "$DOC_DIR/$f" ]; then
            echo "---"
            echo ""
            cat "$DOC_DIR/$f"
            echo ""
        fi
    done

    # Guide chapters (in SUMMARY.md order)
    for f in \
        guide/blocks-and-declarations.md \
        guide/expressions-and-pipelines.md \
        guide/lists-and-transformations.md \
        guide/string-interpolation.md \
        guide/functions-and-combinators.md \
        guide/operators.md \
        guide/anaphora.md \
        guide/block-manipulation.md \
        guide/imports-and-modules.md \
        guide/working-with-data.md \
        guide/command-line.md \
        guide/yaml-embedding.md \
        guide/testing.md \
        guide/date-time-random.md \
        guide/advanced-topics.md; do
        if [ -f "$DOC_DIR/$f" ]; then
            echo "---"
            echo ""
            cat "$DOC_DIR/$f"
            echo ""
        fi
    done

    # Reference
    for f in \
        reference/syntax.md \
        reference/operators-and-identifiers.md \
        reference/prelude/index.md \
        reference/prelude/lists.md \
        reference/prelude/blocks.md \
        reference/prelude/strings.md \
        reference/prelude/numbers.md \
        reference/prelude/booleans.md \
        reference/prelude/combinators.md \
        reference/prelude/calendar.md \
        reference/prelude/sets.md \
        reference/prelude/random.md \
        reference/prelude/metadata.md \
        reference/prelude/io.md \
        reference/cli.md \
        reference/import-formats.md \
        reference/export-formats.md \
        reference/error-messages.md; do
        if [ -f "$DOC_DIR/$f" ]; then
            echo "---"
            echo ""
            cat "$DOC_DIR/$f"
            echo ""
        fi
    done

    # Understanding
    for f in \
        understanding/philosophy.md \
        understanding/lazy-evaluation.md; do
        if [ -f "$DOC_DIR/$f" ]; then
            echo "---"
            echo ""
            cat "$DOC_DIR/$f"
            echo ""
        fi
    done

    # FAQ
    if [ -f "$DOC_DIR/faq.md" ]; then
        echo "---"
        echo ""
        cat "$DOC_DIR/faq.md"
        echo ""
    fi

    # Appendices
    for f in \
        appendices/cheat-sheet.md \
        appendices/syntax-gotchas.md; do
        if [ -f "$DOC_DIR/$f" ]; then
            echo "---"
            echo ""
            cat "$DOC_DIR/$f"
            echo ""
        fi
    done

} > "$OUTPUT"

echo "Generated $OUTPUT ($(wc -l < "$OUTPUT") lines)"
