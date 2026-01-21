# Security Review

## Summary

This security review covers the source code formatter feature (`eu fmt`) added in commit 0d529a9. The feature provides code formatting capabilities for Eucalypt source files with two modes: conservative (whitespace normalization) and full reformat.

The implementation is straightforward and presents **no significant security vulnerabilities**. This is a CLI tool that operates on local files specified by the user, with no network operations, no shell command execution from user input, and no dynamic code evaluation.

## Critical Issues
(P0 - Must fix before merge)

None identified.

## Major Issues
(P1 - Should fix before merge)

None identified.

## Minor Issues
(P2 - Nice to fix)

None identified.

## Observations
(Non-blocking notes and suggestions)

### Files Reviewed

1. **src/syntax/export/format.rs** - Core formatter implementation
2. **src/driver/format.rs** - CLI command handler
3. **src/driver/options.rs** - CLI argument parsing
4. **src/syntax/input.rs** - Input/Locator types

### Security Analysis by Category

**Input Validation**
- CLI arguments parsed via clap v4, which provides type-safe argument handling
- Numeric parameters (`width`, `indent`) are typed as `usize`, preventing injection
- File paths are accepted without sanitization, which is appropriate for a local CLI tool where users have filesystem access

**Command Injection**
- No shell commands constructed from user input
- No `Command::new()` calls in the formatter code
- No dynamic process spawning based on user data

**Path Traversal**
- File operations in `src/driver/format.rs:67` (`fs::read_to_string`) and `src/driver/format.rs:84` (`fs::write`) operate on user-specified paths
- No path canonicalization or traversal protection
- **This is acceptable** for a CLI formatter tool - users intentionally specify which files to format, similar to tools like `rustfmt` or `prettier`

**Injection Vulnerabilities (SQL, XSS, LDAP)**
- Not applicable - no database, web interface, or LDAP integration

**Sensitive Data Exposure**
- No logging of file contents
- Error messages include file paths (appropriate for CLI diagnostics)
- No credentials or secrets in the codebase

**SSRF (Server-Side Request Forgery)**
- No URL fetching or network requests in the formatter
- The `Locator::Url` type exists in the codebase but is not used for network operations in this feature

**Cryptographic Usage**
- No cryptographic operations in the formatter

**Deserialization**
- Parser operates on text using the existing Rowan-based AST
- No unsafe deserialization of arbitrary data formats

### Code Quality Notes

The implementation follows Rust idioms with proper error handling:
- `Result` types used consistently
- No `unwrap()` calls on user input (the `unwrap()` calls in `render_doc` are on internally-generated UTF-8 buffers which cannot fail)
- Clean separation between pure formatting logic and I/O

### Defense in Depth

The formatter operates within a sandboxed scope:
- Only reads/writes files explicitly specified by the user
- No privilege escalation possible
- No persistent state that could be poisoned

## Conclusion

The source code formatter is a low-risk feature with no security concerns identified. The implementation is clean and follows security best practices for a CLI tool of this nature.

---
*Review conducted for leg hq-leg-ikkyi*
