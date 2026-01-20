## ADDED Requirements

### Requirement: Tight Head Operator
The system SHALL provide a `↑` unary prefix operator that returns the first element of a list with higher precedence than property access.

#### Scenario: Basic head operation
- **WHEN** evaluating `↑[1, 2, 3]`
- **THEN** the result SHALL be `1`

#### Scenario: Head of named list
- **WHEN** evaluating `↑items` where `items` is `[{name: "a"}, {name: "b"}]`
- **THEN** the result SHALL be `{name: "a"}`

#### Scenario: Empty list
- **WHEN** evaluating `↑[]`
- **THEN** the result SHALL be `null` (same as `head` behaviour)

### Requirement: Tight Binding Precedence
The `↑` operator SHALL have higher precedence than the dot (`.`) operator, allowing property access to chain without parentheses.

#### Scenario: Property access on head
- **WHEN** evaluating `↑items.name` where `items` is `[{name: "first"}, {name: "second"}]`
- **THEN** the expression SHALL parse as `(↑items).name`
- **AND** the result SHALL be `"first"`

#### Scenario: Chained property access
- **WHEN** evaluating `↑data.users.email`
- **THEN** the expression SHALL parse as `((↑data).users).email`

#### Scenario: Multiple tight operators
- **WHEN** evaluating `↑↑nested` where `nested` is `[[1, 2], [3, 4]]`
- **THEN** the result SHALL be `1` (head of head)

### Requirement: Equivalence to Head Function
The `↑` operator SHALL be semantically equivalent to the existing `head` function.

#### Scenario: Same result as head
- **WHEN** evaluating both `↑xs` and `xs head` for any list `xs`
- **THEN** the results SHALL be identical

#### Scenario: Head function remains available
- **WHEN** using `items head`
- **THEN** the expression SHALL continue to work as before
