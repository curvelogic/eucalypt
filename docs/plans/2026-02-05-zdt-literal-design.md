# ZDT Literal — Design

## Problem

Creating ZDT (zoned datetime) values requires verbose function calls:

```eu
meeting: cal.parse("2023-01-15T10:30:00Z")
birthday: cal.parse("1990-06-15")
```

Users want cleaner syntax for datetime literals.

## Solution

A prefixed string literal `t"..."` for ZDT values, consistent with existing
`c"..."` (c-strings) and `r"..."` (raw strings).

```eu
meeting: t"2023-01-15T10:30:00Z"
birthday: t"1990-06-15"
deadline: t"2023-12-31T23:59:59+05:00"
```

## Supported Formats

Matching YAML timestamp flexibility for familiarity:

| Format | Example | Result |
|--------|---------|--------|
| ISO8601 with Z | `t"2023-01-15T10:30:00Z"` | 2023-01-15T10:30:00Z |
| ISO8601 with offset | `t"2023-01-15T10:30:00+05:00"` | 2023-01-15T10:30:00+05:00 |
| ISO8601 no timezone | `t"2023-01-15T10:30:00"` | 2023-01-15T10:30:00Z (UTC) |
| Space separator | `t"2023-01-15 10:30:00"` | 2023-01-15T10:30:00Z (UTC) |
| Fractional seconds | `t"2023-01-15T10:30:00.123Z"` | 2023-01-15T10:30:00.123Z |
| Date only | `t"2023-01-15"` | 2023-01-15T00:00:00Z (midnight UTC) |

## Defaults

- **Missing timezone** → UTC
- **Date only** → midnight (00:00:00) UTC

These match the existing YAML timestamp import behaviour.

## Compile-Time Validation

Invalid dates are rejected at parse time:

```eu
bad1: t"2023-02-30"        # error: February has 28/29 days
bad2: t"2023-13-01"        # error: month must be 1-12
bad3: t"2023-01-32"        # error: day out of range
bad4: t"2023-01-15T25:00"  # error: hour must be 0-23
bad5: t"not-a-date"        # error: invalid timestamp format

leap: t"2024-02-29"        # OK - 2024 is a leap year
```

Error messages include source location and specific reason:

```
error: invalid ZDT literal
  --> file.eu:3:10
   |
 3 | deadline: t"2023-02-30"
   |           ^^^^^^^^^^^^^ February 2023 has only 28 days
```

## Implementation

### Lexer (`src/syntax/rowan/lex.rs`)

Add `T_STRING` token type alongside existing `C_STRING` and `R_STRING`. When
the lexer sees `t"`, it:

1. Extracts the string content
2. Validates it as a timestamp using chrono
3. Emits `T_STRING` token (or error if invalid)

No interpolation support needed — just `T_STRING`, not `T_STRING_PATTERN`.

### Syntax kinds (`src/syntax/rowan/kind.rs`)

```rust
T_STRING,  // t"2023-01-15T10:30:00Z"
```

### AST and code generation

A `t"..."` literal desugars to a `ZDT` native value during compilation — the
same internal representation used by `cal.parse(...)` and YAML timestamp
imports. The chrono `DateTime<FixedOffset>` is computed at compile time and
embedded directly. No runtime parsing occurs.

## Testing

### Harness tests

- Valid formats: All supported YAML-style formats
- Timezone defaulting: Literals without timezone produce UTC
- Date-only: Produces midnight UTC
- Leap years: `t"2024-02-29"` valid, `t"2023-02-29"` invalid
- Edge cases: Month boundaries, year boundaries, maximum values
- Equivalence: `t"2023-01-15T10:30:00Z"` equals `cal.parse("2023-01-15T10:30:00Z")`
- Timezone comparison: `t"2023-01-15T10:00:00Z"` equals `t"2023-01-15T10:00:00+00:00"`
  (chrono compares absolute instants)
- Cross-timezone ordering: `t"2023-01-15T08:00:00Z"` < `t"2023-01-15T12:00:00+02:00"`
  (08:00 UTC is before 10:00 UTC)

### Error tests

Using `.expect` sidecar mechanism:

- Invalid day: `t"2023-02-30"`
- Invalid month: `t"2023-13-01"`
- Invalid hour: `t"2023-01-15T25:00:00Z"`
- Malformed: `t"not-a-date"`
- Empty: `t""`

## Out of Scope

- Interpolation in ZDT literals (not needed)
- Duration literals (separate future feature)
- Period literals (separate future feature)
- Time-only literals (separate future feature)
