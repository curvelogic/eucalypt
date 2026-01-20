# Change: Add JSONL (JSON Lines) Format Support

## Why
JSON Lines is a common format for log files and data pipelines where each line contains a separate JSON object. Eucalypt currently cannot import this format, limiting its use for log processing workflows.

## What Changes
- Add `jsonl` as a supported import format
- Parse each line as a JSON object, return as a list
- Support `.jsonl` file extension auto-detection

## Impact
- Affected specs: `import-formats` (new capability)
- Affected code:
  - `src/import/jsonl.rs` - New module for JSONL parsing
  - `src/import/mod.rs` - Register jsonl format
  - `src/syntax/input.rs` - Add .jsonl extension mapping

## Success Criteria
- `eu jsonl@file.jsonl` imports JSONL file as list
- `eu jsonl@-` reads JSONL from stdin
- Auto-detects `.jsonl` extension
- Each line becomes a list element
- Empty lines are skipped

## Scope
Static file parsing only. Streaming/incremental processing is a separate feature (eu-n50) that builds on this.
