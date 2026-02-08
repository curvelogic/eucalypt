# Quick Wins Design

Date: 2026-02-03

## Overview

Five small, well-defined features that follow established patterns in
the codebase. Each is independent and can be implemented in any order.

1. **Catch `{name: name}` recursion** (eu-dlr) — static + runtime cycle detection
2. **min/max for ZDTs** (eu-u1m) — polymorphic comparison operators
3. **Version assertions** (eu-nbc) — `eu.requires(">=0.2.0")`
4. **Base64 encoding** (eu-dyx) — encode/decode intrinsics
5. **Hash built-ins** (eu-dd5) — SHA-256 intrinsic

---

## 1. Catch `{name: name}` Recursion (eu-dlr)

### Problem

The pattern `{name: name}` is a common mistake for developers coming
from other languages. Eucalypt currently enters an infinite loop
instead of providing a helpful error message. The thunk enters itself,
pushes an Update continuation, and re-enters endlessly.

### Approach: Two layers of protection

#### Layer 1: Static check in cook phase

After soup processing / varification, walk each `Let` binding's body.
If the body is a bare `Var::Free(fv)` where `fv.pretty_name` matches
the binding's own name (and there is no intervening scope that shadows
it), emit a compile error: "binding 'name' refers to itself".

Also detect simple mutual cycles like `{a: b, b: a}` within the same
let block by building a reference graph among the block's bindings and
checking for cycles.

**Location**: New function in `src/core/cook/mod.rs` or a small new
module `src/core/verify/self_ref.rs`. Runs after varification, before
inline/simplify.

**Performance**: Gate with before/after timing using the existing
`Timings` infrastructure. The cook phase already has a timing entry.
If the check is measurable, scope it to only check
`LetType::DefaultBlockLet` bindings (blocks, where the mistake
actually occurs) rather than all let expressions.

#### Layer 2: Runtime BlackHole

When entering a thunk (the code path in `vm.rs` that pushes
`Continuation::Update`), overwrite the binding in the environment
with a BlackHole sentinel before evaluation. If evaluation re-enters
the same binding, it hits the BlackHole and raises
`ExecutionError::BlackHole`. When the Update continuation fires
successfully, the BlackHole is replaced with the computed value
(which Update already does).

The `ExecutionError::BlackHole` variant already exists but is
currently unused. This catches any cycle that the static check
misses (indirect cycles through multiple levels of indirection).

**Important**: This does not affect benign recursion. Functions are
compiled as lambda forms (already in WHNF), not thunks. They never
get an Update continuation, so the BlackHole overwrite never happens.

### Files changed

- `src/core/cook/mod.rs` or `src/core/verify/self_ref.rs` — static
  cycle detection
- `src/eval/machine/vm.rs` — BlackHole overwrite at thunk entry
- `src/eval/memory/syntax.rs` — BlackHole sentinel value
- `src/eval/machine/error.rs` — improve BlackHole error message with
  binding context

### Testing

- New harness error test for `{name: name}` — compile error, not hang
- New harness error test for `{a: b, b: a}` — compile error
- New harness error test for indirect runtime cycle — BlackHole error
- Existing recursive programs (fibonacci etc.) continue to work
- Before/after timing of cook phase to verify no performance
  regression

---

## 2. min/max for ZDTs (eu-u1m)

### Problem

The prelude `min` and `max` functions use `<` and `>` operators, which
only work on numbers. ZDT values (`DateTime<FixedOffset>`) implement
`PartialOrd` and `Ord` via chrono but the comparison intrinsics do not
handle them.

### Approach: Make comparison operators polymorphic

Extend `Lt`, `Gt`, `Lte`, `Gte` in `arith.rs` to handle `Native::Zdt`
and `Native::Str`/`Native::Sym` values, following the same pattern as
`EQ` in `eq.rs` which already handles multiple types polymorphically:

```rust
fn execute(&self, ..., args: &[Ref]) -> Result<(), ExecutionError> {
    let x = machine.nav(view).resolve_native(&args[0])?;
    let y = machine.nav(view).resolve_native(&args[1])?;
    match (x, y) {
        (Native::Num(nx), Native::Num(ny)) => {
            // existing numeric comparison
        }
        (Native::Zdt(dx), Native::Zdt(dy)) => {
            machine_return_bool(machine, view, dx < dy)
        }
        (Native::Str(sx), Native::Str(sy)) => {
            machine_return_bool(machine, view, sx < sy)
        }
        (Native::Sym(sx), Native::Sym(sy)) => {
            // compare by resolved string, not by SymbolId
            machine_return_bool(machine, view, pool.resolve(sx) < pool.resolve(sy))
        }
        _ => Err(ExecutionError::TypeMismatch(...))
    }
}
```

This means `min`, `max`, and `sort-by(key, lt)` in the prelude
automatically work for ZDTs and strings with no prelude changes.

**Note**: Symbol comparison uses the resolved string (lexicographic),
not the interned ID (which reflects insertion order). This ensures
`sort-keys` produces alphabetical output regardless of interning order.

### Files changed

- `src/eval/stg/arith.rs` — extend `Lt`, `Gt`, `Lte`, `Gte` to
  handle `Native::Zdt`, `Native::Str`, and `Native::Sym`

### Testing

- New harness test: ZDT comparison operators return correct booleans
- New harness test: string comparison operators return correct booleans
- New harness test: symbol comparison with lexicographic ordering
- New harness test: `min` / `max` with ZDT values
- New harness test: `min` / `max` with string values
- Existing numeric comparison tests unchanged

---

## 3. Version Assertions (eu-nbc)

### Problem

Scripts should be able to declare which eucalypt version they require,
with fail-fast behaviour if requirements are not met.

### Approach: `REQUIRES` intrinsic with semver crate

Add `semver` to `Cargo.toml` dependencies. New intrinsic `REQUIRES`
that:

1. Takes one string argument (version constraint, e.g. `">=0.2.0"`)
2. Parses the constraint with `semver::VersionReq::parse()`
3. Parses the current eucalypt version with `semver::Version::parse()`
   (stripping the `.dev` suffix from e.g. `0.2.0.dev` → `0.2.0`)
4. If the constraint matches, returns unit (no-op)
5. If it does not match, returns an error: "eucalypt version 0.2.0
   does not satisfy requirement >=0.3.0"
6. If either string fails to parse, returns a clear parse error

### Files changed

- `Cargo.toml` — add `semver` dependency
- `src/eval/stg/version.rs` (new) — `Requires` intrinsic
  implementation
- `src/eval/stg/mod.rs` — register module and intrinsic
- `src/eval/intrinsics.rs` — catalogue entry
- `lib/prelude.eu` — expose as `eu.requires: __REQUIRES`

### Usage

```eu
eu.requires(">=0.2.0")
result: do-stuff
```

### Testing

- Harness test: `eu.requires(">=0.0.1")` — passes
- Harness error test: `eu.requires(">=99.0.0")` — fails with clear
  message
- Harness test: `eu.requires("^0.2")` — verify caret ranges work

---

## 4. Base64 Encoding Functions (eu-dyx)

### Problem

No built-in base64 encode/decode capability.

### Approach: Two new intrinsics using base64 crate

Add `base64` crate to `Cargo.toml`. Two intrinsics:

- `BASE64_ENCODE` — takes a string, returns its base64 encoding
  (standard alphabet, with padding)
- `BASE64_DECODE` — takes a base64 string, returns the decoded
  string. Errors if the input is not valid base64 or the decoded
  bytes are not valid UTF-8.

Both follow the existing string intrinsic pattern: `str_arg()` to
extract the argument, `machine_return_str()` to return the result.

### Files changed

- `Cargo.toml` — add `base64` dependency
- `src/eval/stg/encoding.rs` (new) — `Base64Encode` and
  `Base64Decode` intrinsic implementations
- `src/eval/stg/mod.rs` — register module and intrinsics
- `src/eval/intrinsics.rs` — two catalogue entries
- `lib/prelude.eu` — expose as `str.base64-encode: __BASE64_ENCODE`
  and `str.base64-decode: __BASE64_DECODE`

### Usage

```eu
str.base64-encode("hello world")       # => "aGVsbG8gd29ybGQ="
str.base64-decode("aGVsbG8gd29ybGQ=")  # => "hello world"
```

### Testing

- Harness test: encode then decode roundtrip
- Harness test: known test vectors
- Harness error test: invalid base64 input to decode

---

## 5. Hash Built-ins (eu-dd5)

### Problem

No built-in cryptographic hash capability.

### Approach: SHA-256 intrinsic using sha2 crate

Add `sha2` crate to `Cargo.toml`. One intrinsic:

- `SHA256` — takes a string, returns its SHA-256 hash as a lowercase
  hex string

Implemented in the same `encoding.rs` module as base64 (both are
encoding/hashing operations). If other hash algorithms are needed
later, they follow the same pattern in the same module.

### Files changed

- `Cargo.toml` — add `sha2` dependency
- `src/eval/stg/encoding.rs` — add `Sha256Hash` intrinsic
- `src/eval/stg/mod.rs` — register intrinsic
- `src/eval/intrinsics.rs` — catalogue entry
- `lib/prelude.eu` — expose as `str.sha256: __SHA256`

### Usage

```eu
str.sha256("hello world")
# => "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
```

### Testing

- Harness test: known SHA-256 test vector for "hello world"
- Harness test: empty string hash (known value)
- Harness test: verify output is lowercase hex, 64 characters

---

## Beads

Existing beads to update with design references:

- **eu-dlr** — Catch `{name: name}` recursion (section 1)
- **eu-u1m** — min/max for ZDTs (section 2)
- **eu-nbc** — Version assertions (section 3)
- **eu-dyx** — Base64 encoding functions (section 4)
- **eu-dd5** — Hash built-ins (section 5)

All five are independent and can be implemented in any order.
