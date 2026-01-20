## 1. Implementation
- [ ] 1.1 Add `.jsonl` extension mapping in `Locator::ext_to_format()`
- [ ] 1.2 Create `src/import/jsonl.rs` with `read_jsonl()` function
- [ ] 1.3 Register `jsonl` format in `src/import/mod.rs`

## 2. Testing
- [ ] 2.1 Create test JSONL file in `harness/test/aux/`
- [ ] 2.2 Add harness test for basic JSONL import
- [ ] 2.3 Add test for JSONL with various JSON types (objects, arrays, primitives)
- [ ] 2.4 Add test for empty lines handling

## 3. Documentation
- [ ] 3.1 Update input format documentation

## Dependencies
- Tasks 1.x can be done in parallel
- Tasks 2.x depend on 1.x completion
- Task 3.x depends on 2.x
