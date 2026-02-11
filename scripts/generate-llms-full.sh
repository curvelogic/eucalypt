#!/usr/bin/env bash
# Generate llms-full.txt by concatenating all user-facing documentation
# into a single markdown file for AI agents and coding assistants.
#
# Reads doc/SUMMARY.md to discover all documentation pages and
# concatenates them in order with --- separators.
#
# Usage: ./scripts/generate-llms-full.sh [output-path]
# Default output: doc/llms-full.txt

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DOC_DIR="$(cd "$SCRIPT_DIR/../doc" && pwd)"
OUTPUT="${1:-$DOC_DIR/llms-full.txt}"

# Extract all .md file paths from SUMMARY.md (relative to doc/)
# Matches patterns like [Title](path/to/file.md)
files=$(sed -n 's/.*](\([^)]*\.md\)).*/\1/p' "$DOC_DIR/SUMMARY.md")

{
    echo "# Eucalypt - Complete Documentation"
    echo ""
    echo "> This file contains the complete eucalypt documentation concatenated"
    echo "> into a single file for use by AI agents and coding assistants."
    echo "> Generated from the eucalypt documentation source."
    echo ""

    first=true
    for f in $files; do
        if [ ! -f "$DOC_DIR/$f" ]; then
            echo "WARNING: $DOC_DIR/$f not found, skipping" >&2
            continue
        fi

        if [ "$first" = true ]; then
            first=false
        else
            echo ""
            echo "---"
            echo ""
        fi

        cat "$DOC_DIR/$f"
    done

} > "$OUTPUT"

echo "Generated $OUTPUT ($(wc -l < "$OUTPUT") lines)"
