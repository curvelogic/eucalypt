## ADDED Requirements

### Requirement: Argument Capture
The system SHALL capture command line arguments appearing after a `--` separator and make them available to eucalypt programs.

#### Scenario: Arguments after double-dash
- **WHEN** running `eu script.eu -- arg1 arg2 arg3`
- **THEN** the arguments `["arg1", "arg2", "arg3"]` SHALL be captured

#### Scenario: No double-dash separator
- **WHEN** running `eu script.eu` without `--`
- **THEN** an empty argument list `[]` SHALL be available

#### Scenario: Double-dash with no arguments
- **WHEN** running `eu script.eu --`
- **THEN** an empty argument list `[]` SHALL be available

### Requirement: __ARGS Pseudo-Input
The system SHALL provide a `__ARGS` pseudo-input containing the captured arguments as a list of strings.

#### Scenario: Access arguments as list
- **WHEN** eucalypt code references `__ARGS`
- **THEN** it SHALL evaluate to a list of string values

#### Scenario: List operations on arguments
- **WHEN** eucalypt code uses `__ARGS head` or `__ARGS map(f)`
- **THEN** standard list operations SHALL work on the arguments

#### Scenario: Empty arguments
- **WHEN** no arguments were provided after `--`
- **THEN** `__ARGS` SHALL evaluate to an empty list `[]`

### Requirement: Argument Preservation
The system SHALL preserve argument values exactly as provided, including special characters and whitespace.

#### Scenario: Arguments with spaces
- **WHEN** running `eu script.eu -- "hello world" foo`
- **THEN** `__ARGS` SHALL be `["hello world", "foo"]`

#### Scenario: Arguments with special characters
- **WHEN** running `eu script.eu -- --flag=value -x`
- **THEN** `__ARGS` SHALL be `["--flag=value", "-x"]`

### Requirement: Subcommand Compatibility
The `__ARGS` pseudo-input SHALL be available regardless of which subcommand is used.

#### Scenario: Args with run subcommand
- **WHEN** running `eu run script.eu -- args`
- **THEN** `__ARGS` SHALL be available

#### Scenario: Args with test subcommand
- **WHEN** running `eu test script.eu -- args`
- **THEN** `__ARGS` SHALL be available
