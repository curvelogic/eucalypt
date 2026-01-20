## 1. CLI Argument Capture
- [ ] 1.1 Add `args: Vec<String>` field to `EucalyptOptions`
- [ ] 1.2 Update CLI parsing to capture arguments after `--` separator
- [ ] 1.3 Pass captured args through to options processing

## 2. Args Pseudo-Input
- [ ] 2.1 Create `create_args_pseudoblock(args: Vec<String>)` in `io.rs`
- [ ] 2.2 Add `__ARGS` pseudo-input injection in `process_defaults()`
- [ ] 2.3 Handle args pseudo-input in `source.rs::load_core()`

## 3. Testing
- [ ] 3.1 Add harness test for basic `__ARGS` access
- [ ] 3.2 Add test for empty args (no `--` provided)
- [ ] 3.3 Add test for args with special characters and spaces
- [ ] 3.4 Verify existing tests still pass

## 4. Documentation
- [ ] 4.1 Update command-line.md with `--` argument passing
- [ ] 4.2 Add example showing `__ARGS` usage

## Dependencies
- Tasks 2.x depend on 1.x (args must be captured first)
- Tasks 3.x depend on 2.x (implementation must exist)
- Tasks 4.x depend on 3.x (tests should pass before documenting)
