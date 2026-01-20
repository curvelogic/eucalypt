## ADDED Requirements

### Requirement: Content Preservation
The formatter SHALL preserve the semantic content of string literals, comments, section headers, and metadata during formatting.

#### Scenario: String literals preserved
- **WHEN** formatting source containing string literals
- **THEN** the string content MUST be identical in the output

#### Scenario: Comments preserved
- **WHEN** formatting source containing comments
- **THEN** comments MUST appear in the output in their relative positions

#### Scenario: Metadata preserved
- **WHEN** formatting source containing backtick metadata annotations
- **THEN** metadata MUST be preserved and associated with the correct declarations

### Requirement: Dot Spacing Normalisation
The formatter SHALL normalise spacing around dot operators, removing spaces around dots except within decimal numbers.

#### Scenario: Spaces removed around dots
- **WHEN** formatting `a . b`
- **THEN** the output SHALL be `a.b`

#### Scenario: Decimal numbers preserved
- **WHEN** formatting `3.14`
- **THEN** the output SHALL be `3.14` (unchanged)

#### Scenario: Method chaining normalised
- **WHEN** formatting `foo . bar . baz`
- **THEN** the output SHALL be `foo.bar.baz`

### Requirement: Colon Spacing Normalisation
The formatter SHALL normalise spacing around colons in declarations, ensuring no space before the colon and one space after.

#### Scenario: Space before colon removed
- **WHEN** formatting `name : value`
- **THEN** the output SHALL be `name: value`

#### Scenario: Missing space after colon added
- **WHEN** formatting `name:value`
- **THEN** the output SHALL be `name: value`

#### Scenario: Function declaration normalised
- **WHEN** formatting `f(x) : body`
- **THEN** the output SHALL be `f(x): body`

### Requirement: List Comma Spacing
The formatter SHALL ensure consistent spacing after commas in lists, with one space after each comma.

#### Scenario: Spaces added after commas
- **WHEN** formatting `[a,b,c]`
- **THEN** the output SHALL be `[a, b, c]`

#### Scenario: Existing correct spacing preserved
- **WHEN** formatting `[a, b, c]`
- **THEN** the output SHALL be `[a, b, c]` (unchanged)

### Requirement: Block Brace Spacing
The formatter SHALL ensure consistent spacing inside braces for single-line blocks.

#### Scenario: Spaces added inside braces
- **WHEN** formatting `{a:1}`
- **THEN** the output SHALL be `{ a: 1 }`

#### Scenario: Inline block with multiple declarations
- **WHEN** formatting `{a:1,b:2}`
- **THEN** the output SHALL be `{ a: 1, b: 2 }`

### Requirement: Line Breaking
The formatter SHALL break long lines at logical boundaries when they exceed the configured line width.

#### Scenario: Long list broken across lines
- **WHEN** formatting a list that exceeds line width
- **THEN** the list SHOULD be broken with elements on separate lines, properly indented

#### Scenario: Nested block line breaking
- **WHEN** formatting a block that exceeds line width
- **THEN** declarations SHOULD be placed on separate lines with proper indentation

#### Scenario: Short content kept on one line
- **WHEN** formatting content that fits within line width
- **THEN** the content SHALL remain on a single line

### Requirement: Tab Handling
The formatter SHALL convert tab characters to spaces, attempting to preserve apparent alignment intent.

#### Scenario: Tab converted to spaces preserving alignment
- **WHEN** formatting source containing tabs that appear to represent alignment (at 2 or 4 char boundaries)
- **THEN** tabs SHALL be converted to the equivalent number of spaces

#### Scenario: Unaligned tabs trigger reformatting
- **WHEN** formatting source containing tabs that cannot be legitimised as alignment
- **THEN** the affected node subtree SHALL be fully reformatted

### Requirement: Alignment Preservation
The formatter in conservative mode SHALL preserve intentional alignment patterns in properly formatted code.

#### Scenario: Columnar alignment preserved
- **WHEN** formatting declarations with intentional columnar alignment using spaces
- **THEN** the alignment pattern SHALL be preserved

#### Scenario: Excess whitespace normalised
- **WHEN** formatting single-line content with excessive whitespace (5+ spaces)
- **THEN** the excess whitespace SHALL be normalised to single spaces

### Requirement: Blank Line Preservation
The formatter SHALL preserve blank lines in multi-line blocks to maintain logical grouping.

#### Scenario: Blank lines between declaration groups
- **WHEN** formatting a block with blank lines separating declaration groups
- **THEN** the blank lines SHALL be preserved

### Requirement: Semantic Preservation
The formatter SHALL preserve semantic distinctions in the source code.

#### Scenario: Function call vs catenation preserved
- **WHEN** formatting `f(x)` (function call with apply tuple)
- **THEN** the output SHALL be `f(x)` (no space before parenthesis)

#### Scenario: Catenation spacing preserved
- **WHEN** formatting `f (x)` (catenation with parenthesised expression)
- **THEN** the output SHALL be `f (x)` (space preserved)

### Requirement: Well-Formatted Code Detection
The formatter in conservative mode SHALL detect code that is already well-formatted and avoid unnecessary changes.

#### Scenario: Well-formatted code unchanged
- **WHEN** formatting code that already conforms to formatting rules
- **THEN** the output SHALL be identical to the input

### Requirement: Indentation Consistency
The formatter SHALL ensure indentation uses consistent multiples of the configured indent size.

#### Scenario: Nested block indentation
- **WHEN** formatting nested blocks
- **THEN** each nesting level SHALL increase indentation by the configured indent size

#### Scenario: Mixed indentation normalised
- **WHEN** formatting code with inconsistent indentation levels
- **THEN** indentation SHALL be normalised to consistent multiples

### Requirement: CLI Integration
The formatter SHALL be accessible via the `eu fmt` command with appropriate options.

#### Scenario: Format file to stdout
- **WHEN** running `eu fmt file.eu`
- **THEN** formatted output SHALL be written to stdout

#### Scenario: Format file in place
- **WHEN** running `eu fmt --write file.eu`
- **THEN** the file SHALL be modified in place with formatted content

#### Scenario: Check mode for CI
- **WHEN** running `eu fmt --check file.eu` on unformatted code
- **THEN** the command SHALL exit with non-zero status

#### Scenario: Configurable line width
- **WHEN** running `eu fmt --width 100 file.eu`
- **THEN** line breaking SHALL use 100 characters as the target width

### Requirement: Formatting Modes
The formatter SHALL support two distinct formatting modes: conservative and full reformatting.

#### Scenario: Conservative mode default
- **WHEN** running `eu fmt file.eu` without mode flags
- **THEN** conservative mode SHALL be used, preserving good formatting

#### Scenario: Full reformatting mode
- **WHEN** running `eu fmt --reformat file.eu`
- **THEN** full reformatting SHALL be applied, generating canonical output
