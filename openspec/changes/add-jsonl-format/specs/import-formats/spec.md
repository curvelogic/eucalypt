## ADDED Requirements

### Requirement: JSONL Format Import
The system SHALL support importing JSON Lines format files where each line contains a valid JSON value.

#### Scenario: Import JSONL file
- **WHEN** running `eu jsonl@file.jsonl`
- **THEN** the file SHALL be parsed as JSON Lines format

#### Scenario: Import JSONL from stdin
- **WHEN** running `eu jsonl@-` with JSONL data on stdin
- **THEN** the data SHALL be parsed as JSON Lines format

#### Scenario: Auto-detect .jsonl extension
- **WHEN** running `eu file.jsonl` (without format prefix)
- **THEN** the format SHALL be auto-detected as `jsonl`

### Requirement: JSONL List Structure
The system SHALL return JSONL data as a list where each element corresponds to one line of the input.

#### Scenario: Multiple JSON objects
- **WHEN** importing:
  ```
  {"name": "Alice"}
  {"name": "Bob"}
  ```
- **THEN** the result SHALL be a list `[{name: "Alice"}, {name: "Bob"}]`

#### Scenario: Mixed JSON types
- **WHEN** importing lines containing objects, arrays, and primitives
- **THEN** each line SHALL be parsed according to its JSON type

#### Scenario: Single line
- **WHEN** importing a JSONL file with one line
- **THEN** the result SHALL be a list with one element

### Requirement: Empty Line Handling
The system SHALL skip empty lines in JSONL input.

#### Scenario: Empty lines between records
- **WHEN** importing:
  ```
  {"a": 1}

  {"b": 2}
  ```
- **THEN** the result SHALL be `[{a: 1}, {b: 2}]` (empty line skipped)

#### Scenario: Trailing newline
- **WHEN** importing JSONL with a trailing newline
- **THEN** no empty element SHALL be added to the result

### Requirement: JSONL Error Handling
The system SHALL report errors for invalid JSON on any line.

#### Scenario: Invalid JSON on a line
- **WHEN** importing JSONL with malformed JSON on line N
- **THEN** an error SHALL be reported indicating the line number

#### Scenario: Whitespace-only lines
- **WHEN** importing JSONL with whitespace-only lines
- **THEN** those lines SHALL be treated as empty and skipped
