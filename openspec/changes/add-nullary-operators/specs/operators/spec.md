## ADDED Requirements

### Requirement: Nullary Operator Declaration
The system SHALL support declaring nullary (0-arity) operators using parenthesised operator syntax with no parameters.

#### Scenario: Declare nullary operator
- **WHEN** declaring `(∅): []`
- **THEN** the operator `∅` SHALL be defined with arity 0 and body `[]`

#### Scenario: Declare nullary operator with metadata
- **WHEN** declaring with metadata:
  ```
  ` { doc: "Empty set" }
  (∅): []
  ```
- **THEN** the operator `∅` SHALL be defined with associated metadata

### Requirement: Nullary Operator Evaluation
The system SHALL evaluate nullary operators as their body expression without requiring any operands.

#### Scenario: Use nullary operator in expression
- **WHEN** evaluating `∅`
- **THEN** the result SHALL be the operator's body expression (`[]`)

#### Scenario: Nullary operator in larger expression
- **WHEN** evaluating `[1, 2] ++ ∅`
- **THEN** `∅` SHALL evaluate to its body before the `++` operator is applied

### Requirement: Nullary Operator Precedence
Nullary operators SHALL be treated as immediate values in operator precedence, evaluated before any operand consumption by adjacent operators.

#### Scenario: Nullary with unary prefix
- **WHEN** evaluating `¬ ∅` where `¬` is unary prefix and `∅` is nullary
- **THEN** `∅` SHALL evaluate first, then `¬` SHALL be applied to the result

#### Scenario: Nullary with binary operator
- **WHEN** evaluating `∅ ++ xs` where `++` is binary
- **THEN** `∅` SHALL evaluate to its body as the left operand of `++`

#### Scenario: Multiple nullary operators
- **WHEN** evaluating `∅ ++ ∅`
- **THEN** both `∅` occurrences SHALL evaluate independently to their bodies

### Requirement: Nullary Operator Coexistence
Nullary operators SHALL coexist with unary and binary operators of the same symbol, distinguished by declaration syntax.

#### Scenario: Same symbol different arities
- **WHEN** declaring both `(∅): []` and `(∅ x): negate(x)`
- **THEN** both operators SHALL be valid with arity determining which is used based on context

#### Scenario: Nullary vs unary disambiguation
- **WHEN** `∅` appears with no adjacent value and both nullary and unary `∅` exist
- **THEN** the nullary form SHALL be preferred
