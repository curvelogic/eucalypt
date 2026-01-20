# Change: Add Tight Head Operator (↑)

## Why
Accessing a property on the first element of a list requires awkward parentheses: `(items head).name`. A tight-binding prefix operator would allow the natural syntax `↑items.name`.

## What Changes
- Add `↑` as a unary prefix operator in the prelude
- Set precedence higher than dot (>90) so it binds tighter than property access
- Implement as equivalent to `head` function

## Impact
- Affected specs: `operators` (extend existing)
- Affected code:
  - `lib/prelude.eu` - Add operator definition

## Success Criteria
- `↑items` evaluates to `items head`
- `↑items.name` parses as `(↑items).name` without explicit parentheses
- `↑[1,2,3]` returns `1`
- Existing `head` function remains available

## Scope
Prelude-only change. No lexer or parser modifications needed - `↑` (U+2191) is already recognised as an operator symbol.
