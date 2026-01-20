## 1. Core Expression Extension
- [ ] 1.1 Add `Nullary` variant to `Fixity` enum in `src/core/expr.rs`
- [ ] 1.2 Implement `arity()` returning 0 for `Nullary`
- [ ] 1.3 Add appropriate `BindSide` classification for nullary operators

## 2. Desugaring
- [ ] 2.1 Handle `DeclarationKind::Nullary` in desugaring phase
- [ ] 2.2 Generate operator metadata with fixity `Nullary` for nullary declarations
- [ ] 2.3 Wire nullary operator body expression through to core

## 3. Cooking Phase
- [ ] 3.1 Update `Distributor` in `fixity.rs` to handle nullary operator metadata
- [ ] 3.2 Update `fill_gaps` in `fill.rs` to classify nullary operators correctly
- [ ] 3.3 Update shunting yard in `shunt.rs` to handle arity 0
- [ ] 3.4 Implement `apply_zero` or equivalent for nullary operator application

## 4. Testing
- [ ] 4.1 Add harness test for basic nullary operator declaration and use
- [ ] 4.2 Add test for nullary operators mixed with unary/binary operators
- [ ] 4.3 Add test for multiple nullary operators in same expression
- [ ] 4.4 Verify existing operator tests still pass

## Dependencies
- Tasks 2.x depend on 1.x (core types first)
- Tasks 3.x depend on 2.x (desugaring must produce nullary metadata)
- Tasks 4.x should run throughout to verify changes
