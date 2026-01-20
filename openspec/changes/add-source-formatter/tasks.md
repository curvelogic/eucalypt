## 1. Core Infrastructure
- [ ] 1.1 Define `FormatterConfig` struct (line width, indent size, mode)
- [ ] 1.2 Create `Formatter` struct with configuration and mode state
- [ ] 1.3 Implement whitespace analysis utilities for Rowan nodes

## 2. ToSourceDoc Implementation
- [ ] 2.1 Implement `ToSourceDoc` for `Literal` (numbers, strings, symbols)
- [ ] 2.2 Implement `ToSourceDoc` for `Name` and `Identifier`
- [ ] 2.3 Implement `ToSourceDoc` for `List` with proper comma spacing
- [ ] 2.4 Implement `ToSourceDoc` for `Block` with brace spacing and line breaking
- [ ] 2.5 Implement `ToSourceDoc` for `Declaration` and `DeclarationHead`
- [ ] 2.6 Implement `ToSourceDoc` for `Soup` (expression sequences)
- [ ] 2.7 Implement `ToSourceDoc` for `StringPattern` (interpolation)
- [ ] 2.8 Implement `ToSourceDoc` for `ApplyTuple` (function arguments)
- [ ] 2.9 Implement `ToSourceDoc` for `ParenExpr`
- [ ] 2.10 Implement `ToSourceDoc` for `Unit` (top-level)

## 3. Spacing Rules
- [ ] 3.1 Implement dot spacing normalisation (`a . b` -> `a.b`)
- [ ] 3.2 Implement colon spacing normalisation (`name : value` -> `name: value`)
- [ ] 3.3 Implement comma spacing normalisation (`[a,b]` -> `[a, b]`)
- [ ] 3.4 Implement brace spacing normalisation (`{a:1}` -> `{ a: 1 }`)

## 4. Tab Handling
- [ ] 4.1 Implement tab detection in source
- [ ] 4.2 Implement tab-to-space conversion with alignment preservation
- [ ] 4.3 Implement mode switching when tabs cannot be legitimised

## 5. Conservative Mode
- [ ] 5.1 Implement violation detection (spacing, tabs, excess whitespace)
- [ ] 5.2 Implement selective reformatting at violation sites
- [ ] 5.3 Implement alignment preservation for intentional patterns
- [ ] 5.4 Implement blank line preservation

## 6. CLI Integration
- [ ] 6.1 Add `fmt` subcommand to CLI
- [ ] 6.2 Implement `--width` option for line width
- [ ] 6.3 Implement `--write` option for in-place modification
- [ ] 6.4 Implement `--check` option for CI integration
- [ ] 6.5 Add stdin/stdout support

## 7. Testing
- [ ] 7.1 Create test fixtures for spacing normalisation
- [ ] 7.2 Create test fixtures for line breaking
- [ ] 7.3 Create test fixtures for tab handling
- [ ] 7.4 Create round-trip tests (format then parse, verify AST equivalence)
- [ ] 7.5 Create idempotency tests (format twice, verify identical output)

## 8. Documentation
- [ ] 8.1 Add `eu fmt --help` documentation
- [ ] 8.2 Update harness tests or examples showing formatted style

## Dependencies
- Tasks 2.x depend on 1.x (infrastructure first)
- Tasks 3.x can be done in parallel with 2.x
- Tasks 4.x and 5.x depend on 2.x completion
- Task 6.x can start after 2.1-2.4 are complete
- Task 7.x should be developed alongside implementation
