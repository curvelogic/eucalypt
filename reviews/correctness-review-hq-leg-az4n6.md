# Correctness Review

## Summary

This review covers the source code formatter feature (`eu fmt`) introduced in commit `0d529a9`. The implementation adds formatting capabilities with two modes: conservative (fix violations while preserving structure) and full reformat. Overall the code is well-structured, but there are several correctness issues ranging from a critical bug where `process_defaults()` is called twice causing duplicate inputs, to potential panics from unchecked division and integer underflow.

## Critical Issues

### P0-1: Double call to `process_defaults()` causes duplicate inputs

**File:** `src/bin/eu.rs:15-17` and `src/driver/options.rs:696`

**Description:** `process_defaults()` is called twice:
1. Inside `EucalyptOptions::from_args()` at options.rs:696 (via unwrap)
2. Again in `main()` at eu.rs:15

Each call to `process_defaults()` prepends inputs to the prologue:
- Line 809: Inserts current working directory to lib_path
- Lines 817-825: Prepends Eufile and dotfile
- Lines 829-835: Prepends prelude
- Lines 838-847: Prepends build-meta and io pseudo-inputs
- Lines 850-854: Adds CLI evaluand

**Impact:** All prologue inputs are duplicated, causing files to be processed twice and potentially causing incorrect program behavior.

**Suggested fix:** Remove the redundant call in eu.rs:
```rust
// Remove these lines from main():
// if opt.process_defaults().is_err() {
//     process::exit(1);
// }
```

Or make `process_defaults()` idempotent by tracking whether it has already run.

## Major Issues

### P1-1: Division by zero when indent_size is 0

**File:** `src/syntax/export/format.rs:440`

**Description:** The tab expansion calculation:
```rust
let spaces = self.config.indent_size - (col % self.config.indent_size);
```

If `indent_size` is 0, this causes a division by zero panic.

**Impact:** Program crash if user provides `--indent 0`.

**Suggested fix:** Validate indent_size in `FormatterConfig::new()`:
```rust
pub fn new(line_width: usize, indent_size: usize, reformat: bool) -> Self {
    Self {
        line_width: line_width.max(1),
        indent_size: indent_size.max(1),  // Prevent division by zero
        reformat,
    }
}
```

### P1-2: Integer underflow in single-line length check

**File:** `src/syntax/export/format.rs:238` and `src/syntax/export/format.rs:303`

**Description:** The single-line decision uses:
```rust
let can_single_line = single_line_len <= self.config.line_width - 10
```

If `line_width` is less than 10, this underflows to a very large number (usize wraps around), causing ALL blocks/lists to be formatted on single lines regardless of actual length.

**Impact:** Incorrect formatting output with narrow line widths.

**Suggested fix:** Use saturating subtraction:
```rust
let can_single_line = single_line_len <= self.config.line_width.saturating_sub(10)
```

## Minor Issues

### P2-1: Unwrap calls that could be Result propagation

**File:** `src/syntax/export/format.rs:68-69`

**Description:**
```rust
doc.render(self.config.line_width, &mut w).unwrap();
String::from_utf8(w).unwrap()
```

While these are unlikely to fail in practice (Vec<u8> writes don't fail, and input is valid UTF-8), using unwrap in library code is less robust than propagating errors.

**Impact:** Low - would only panic in unusual circumstances.

**Suggested fix:** Consider returning `Result<String, String>` from `render_doc`.

### P2-2: Redundant match pattern creates temporary Vec

**File:** `src/driver/options.rs:290`

**Description:**
```rust
_ => &Vec::new(),
```

This wildcard pattern catches `Some(Commands::Version)`. Creating a temporary `Vec::new()` and returning a reference to it is unusual (the borrow checker allows it because the reference is used immediately), but it would be cleaner to handle `Version` explicitly.

**Impact:** Minor inefficiency, reduced code clarity.

**Suggested fix:** Add explicit pattern:
```rust
Some(Commands::Version) => &Vec::new(),
```

Or use a static empty slice:
```rust
Some(Commands::Version) | None => &[],
```

Wait, that won't work due to type mismatch. Better approach:
```rust
// Just use cli.files for Version since it won't have any
Some(Commands::Version) => &cli.files,
```

### P2-3: Silent error on process_defaults failure

**File:** `src/bin/eu.rs:15-17`

**Description:**
```rust
if opt.process_defaults().is_err() {
    process::exit(1);
}
```

The error is discarded without printing a message to the user.

**Impact:** User confusion when the program exits with code 1 but no error message. (Note: This becomes moot if P0-1 is fixed by removing this call.)

## Observations

1. **Format mode stdin handling is correct:** The `format_stdin` function correctly ignores `--write` flag (you can't write back to stdin) and always outputs to stdout.

2. **Exit code semantics are correct:** The format command properly returns exit code 1 if ANY file needs formatting in check mode, and preserves non-zero exit codes across multiple files.

3. **Tab expansion logic is correct:** The tab-to-spaces conversion properly aligns to indent boundaries.

4. **Whitespace normalization preserves indentation:** The `normalize_whitespace` function only collapses excessive inline whitespace (5+ spaces without newlines), preserving leading indentation on new lines.

5. **Conservative mode is appropriately conservative:** It only normalizes whitespace tokens without restructuring the AST.
