# Structured Argument Parsing Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> **MANDATORY**: Before writing ANY eucalypt (.eu) code, read: `docs/reference/agent-reference.md`, `docs/appendices/syntax-gotchas.md`, `docs/appendices/cheat-sheet.md`

**Goal:** Add `parse-args(defaults, args)` to the prelude that parses command-line arguments against a defaults block with metadata, supporting long/short flags, flag toggles, type coercion, positional args, and auto-generated help.

**Architecture:** Mostly pure prelude using block introspection (`elements`, `meta`, `lookup-or`, `has`, `kv-block`, `sym`), string matching (`str.starts-with?`, `str.split-on`), and list folding. Requires new type predicate BIFs (`number?`, `string?`, `bool?`, `symbol?`) for type coercion. The defaults block doubles as the option schema — types inferred from default values, metadata defines short flags and documentation.

**Tech Stack:** Rust (new BIFs for type predicates), eucalypt prelude

---

## Task 0: Type predicate intrinsics (prerequisite)

Add `number?`, `string?`, `bool?`, `symbol?` predicates following the existing `block?`/`list?` pattern. These are generally useful beyond parse-args.

**Files:**
- Modify: `src/eval/stg/arith.rs` or create `src/eval/stg/type_check.rs` (BIF implementations)
- Modify: `src/eval/intrinsics.rs` (register intrinsics)
- Modify: `src/eval/stg/mod.rs` (register in runtime)
- Modify: `lib/prelude.eu` (add prelude wrappers)
- Test: `tests/harness/126_parse_args.eu` (or a separate type predicate test)
- Test: `tests/harness_test.rs`

- [ ] **Step 1: Implement type predicate BIFs**

Follow the `IsList` pattern in `src/eval/stg/list.rs:113-141`. Each predicate resolves the arg to native and checks the variant:

```rust
pub struct IsNumber;

impl StgIntrinsic for IsNumber {
    fn name(&self) -> &str { "ISNUMBER" }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let nav = machine.nav(view);
        let is_num = matches!(nav.resolve_native(&args[0]), Ok(Native::Num(_)));
        machine_return_bool(machine, view, is_num)
    }
}

impl CallGlobal1 for IsNumber {}
```

Similarly for `IsString` (checks `Native::Str`), `IsSymbol` (checks `Native::Sym`), `IsBool` (checks `BoolTrue`/`BoolFalse` data constructors — use the same tag-matching pattern as `IsList`).

- [ ] **Step 2: Register intrinsics**

Add to `src/eval/intrinsics.rs` at next available indices:

```rust
Intrinsic { name: "ISNUMBER", ty: function(vec![unk()]).unwrap(), strict: vec![0] },
Intrinsic { name: "ISSTRING", ty: function(vec![unk()]).unwrap(), strict: vec![0] },
Intrinsic { name: "ISSYMBOL", ty: function(vec![unk()]).unwrap(), strict: vec![0] },
Intrinsic { name: "ISBOOL", ty: function(vec![unk()]).unwrap(), strict: vec![0] },
```

Register in `src/eval/stg/mod.rs` `register_intrinsics()`.

- [ ] **Step 3: Add prelude wrappers**

In `lib/prelude.eu`, near `block?` and `list?`:

```eu
` "`number?(v)` - true if and only if `v` is a number."
number?: __ISNUMBER

` "`string?(v)` - true if and only if `v` is a string."
string?: __ISSTRING

` "`symbol?(v)` - true if and only if `v` is a symbol."
symbol?: __ISSYMBOL

` "`bool?(v)` - true if and only if `v` is a boolean."
bool?: __ISBOOL
```

- [ ] **Step 4: Write test**

Add type predicate tests to the harness (can be part of 126 or a separate file):

```eu
` { target: :test-type-predicates }
test-type-predicates: {
  n: 42 number? //= true
  s: "hello" string? //= true
  y: :foo symbol? //= true
  b: true bool? //= true
  nn: "hello" number? //= false
  ns: 42 string? //= false
  bl: [] block? //= false
  lb: {} list? //= false
  RESULT: [n, s, y, b, nn, ns, bl, lb] all-true? then(:PASS, :FAIL)
}
```

- [ ] **Step 5: Build and test**

```bash
cargo test test_harness_126
```

- [ ] **Step 6: Commit**

```bash
git commit -m "feat: add number?, string?, symbol?, bool? type predicates"
```

---

## Task 1: Core `parse-args` function

**Files:**
- Modify: `lib/prelude.eu` (add parse-args namespace/functions)
- Test: `tests/harness/126_parse_args.eu`
- Test: `tests/harness_test.rs` (add test entry)

- [ ] **Step 1: Read the documentation**

Read `docs/reference/agent-reference.md`, `docs/appendices/syntax-gotchas.md`, `docs/appendices/cheat-sheet.md` before writing any eucalypt code.

Key patterns needed:
- Block introspection: `elements`, `keys`, `meta`, `has`, `lookup-or`
- String operations: `str.starts-with?`, `str.split-on`, `str.contains?`, `str.replace`
- List operations: `foldl`, `head`, `tail`, `cons`, `nil?`, `map`, `filter`
- Block construction: `block`, `merge`, `zip-kv`
- Metadata access: `meta(v)` returns the metadata block of a value

- [ ] **Step 2: Write the harness test first**

Create `tests/harness/126_parse_args.eu`:

```eu
{ title: "126 parse-args" }

# Test defaults block
` :suppress
test-defaults: {
  ` { short: :v doc: "Enable verbose output" flag: true }
  verbose: false

  ` { short: :o doc: "Output file path" }
  output: "default.txt"

  ` { doc: "Repeat count" }
  count: 1
}

# Basic long option
` { target: :test-long-option }
test-long-option: {
  result: ["--output", "foo.txt"] parse-args(test-defaults)
  RESULT: if(result.output = "foo.txt", :PASS, :FAIL)
}

# Short option
` { target: :test-short-option }
test-short-option: {
  result: ["-o", "bar.txt"] parse-args(test-defaults)
  RESULT: if(result.output = "bar.txt", :PASS, :FAIL)
}

# Flag toggle
` { target: :test-flag }
test-flag: {
  result: ["--verbose"] parse-args(test-defaults)
  RESULT: if(result.verbose = true, :PASS, :FAIL)
}

# Short flag
` { target: :test-short-flag }
test-short-flag: {
  result: ["-v"] parse-args(test-defaults)
  RESULT: if(result.verbose = true, :PASS, :FAIL)
}

# Equals syntax
` { target: :test-equals-syntax }
test-equals-syntax: {
  result: ["--output=baz.txt"] parse-args(test-defaults)
  RESULT: if(result.output = "baz.txt", :PASS, :FAIL)
}

# Numeric coercion
` { target: :test-numeric }
test-numeric: {
  result: ["--count", "5"] parse-args(test-defaults)
  RESULT: if(result.count = 5, :PASS, :FAIL)
}

# Positional args
` { target: :test-positional }
test-positional: {
  result: ["--verbose", "file1.txt", "file2.txt"] parse-args(test-defaults)
  pass: (result.args = ["file1.txt", "file2.txt"]) && result.verbose
  RESULT: pass then(:PASS, :FAIL)
}

# Mixed flags and positional
` { target: :test-mixed }
test-mixed: {
  result: ["-v", "-o", "out.json", "--count", "3", "input.eu"] parse-args(test-defaults)
  pass: result.verbose && (result.output = "out.json") && (result.count = 3) && (result.args = ["input.eu"])
  RESULT: pass then(:PASS, :FAIL)
}

# Defaults preserved
` { target: :test-defaults-preserved }
test-defaults-preserved: {
  result: [] parse-args(test-defaults)
  pass: (result.verbose = false) && (result.output = "default.txt") && (result.count = 1) && (result.args = [])
  RESULT: pass then(:PASS, :FAIL)
}

# Combined short flags: -vo out.txt (v is flag, o takes arg)
` { target: :test-combined-short }
test-combined-short: {
  result: ["-vo", "out.txt"] parse-args(test-defaults)
  pass: result.verbose && (result.output = "out.txt")
  RESULT: pass then(:PASS, :FAIL)
}
```

Add to `tests/harness_test.rs`:

```rust
harness_test!(test_harness_126, "126_parse_args");
```

- [ ] **Step 3: Run test to verify it fails**

```bash
cargo test test_harness_126
```

Expected: FAIL (parse-args not defined yet)

- [ ] **Step 4: Implement `parse-args` in prelude**

Add to `lib/prelude.eu` after the io section (near the top, since it relates to io.args usage):

```eu
##
## Structured argument parsing
##

# Build lookup table: { "v": :verbose, "o": :output, ... }
# from defaults block metadata
` :suppress
build-short-lookup(defaults): {
  pairs: defaults elements filter(meta ; has(:short)) map(short-entry)
  short-entry([k, v]): [(meta(v) lookup(:short)) str.of, k]
}.pairs block

# Check if a value's default is numeric
` :suppress
is-numeric-default(defaults, key): {
  val: defaults lookup-or(key, "")
  # Check if default is a number by testing equality with itself plus zero
  # (strings would fail the addition)
  result: (val + 0 = val)
}.result

# Check if option is a flag (boolean toggle, no value arg)
` :suppress
is-flag(defaults, key): {
  val-meta: defaults elements filter(key = head ; key) head tail meta
  result: val-meta lookup-or(:flag, false)
}.result

# Coerce string value to match the type of the default
` :suppress
coerce-value(defaults, key, str-val): {
  default-val: defaults lookup-or(key, str-val)
  result: if(is-numeric-default(defaults, key),
             str-val parse-as(:json),
             str-val)
}.result

# Resolve a short flag character to its long key name
` :suppress
resolve-short(short-lookup, ch): short-lookup lookup-or(ch sym, null)

# Process a single --key=value or --key value argument
# Returns updated state: { opts: {...}, args: [...], pending: null }
` :suppress
process-long-arg(defaults, state, arg): {
  # Strip -- prefix
  name: arg str.replace("^--", "")
  # Check for = syntax
  has-eq: name str.contains?("=")
  key-str: if(has-eq, name str.split-on("=") head, name)
  key: key-str sym

  result: if(has-eq, {
    # --key=value
    val: name str.split-on("=") tail head
    opts: state.opts merge({ [key]: coerce-value(defaults, key, val) } block)
    args: state.args
    pending: null
  }, if(is-flag(defaults, key), {
    # --flag (boolean toggle)
    opts: state.opts merge({ [key]: true } block)
    args: state.args
    pending: null
  }, {
    # --key (next arg is value)
    opts: state.opts
    args: state.args
    pending: key
  }))
}.result

# Main fold function: process one arg at a time
` :suppress
process-arg(defaults, short-lookup, state, arg): {
  result: if(state.pending ✓, {
    # Previous arg was --key, this arg is its value
    key: state.pending
    opts: state.opts merge({ [key]: coerce-value(defaults, key, arg) } block)
    args: state.args
    pending: null
  },
  if(arg str.starts-with?("--"),
    if(arg = "--help", panic(generate-help(defaults)),
      process-long-arg(defaults, state, arg)),
  if(arg str.starts-with?("-"),
    process-short-args(defaults, short-lookup, state, arg str.replace("^-", "") letters),
  {
    # Positional arg
    opts: state.opts
    args: state.args ++ [arg]
    pending: null
  })))
}.result

# Process short flag characters (may be combined: -vn3)
` :suppress
process-short-args(defaults, short-lookup, state, chars): {
  result: foldl(process-short-char(defaults, short-lookup), state, chars)
}.result

` :suppress
process-short-char(defaults, short-lookup, state, ch): {
  key: resolve-short(short-lookup, ch)
  result: if(key null?,
    panic("unknown short option: -{ch}"),
    if(state.pending ✓, {
      # Previous short flag needs a value — but we got another flag
      # This means the pending was actually a flag
      opts: state.opts
      args: state.args
      pending: key
    },
    if(is-flag(defaults, key), {
      opts: state.opts merge({ [key]: true } block)
      args: state.args
      pending: null
    }, {
      opts: state.opts
      args: state.args
      pending: key
    })))
}.result

# Generate help text from defaults block
` :suppress
generate-help(defaults): {
  lines: defaults elements map(help-line)
  help-line([k, v]): {
    m: meta(v)
    short: m lookup-or(:short, null)
    doc: m lookup-or(:doc, "")
    short-str: if(short ✓, "  -{short str.of}, ", "      ")
    flag-str: if(m lookup-or(:flag, false), " (flag)", "")
    line: "{short-str}--{k str.of}{flag-str}  {doc}  [default: {v}]"
  }.line
  result: ["Options:", ""] ++ lines join-on(c"\n")
}.result

# Public API
` "`parse-args(defaults, args)` - parse command-line arguments against a defaults block.

  Each field in defaults defines an option with its default value.
  Field metadata can specify:
  - short: symbol for short flag (e.g. short: :v)
  - doc: description string for help
  - flag: true to mark as a boolean toggle (no value argument)

  Returns the defaults block overridden with parsed values,
  plus an args key containing positional arguments as a list.

  Panics on unknown options. --help or -h prints auto-generated help."
parse-args(defaults, args): {
  short-lookup: build-short-lookup(defaults)
  init: { opts: defaults, args: [], pending: null }
  final: foldl(process-arg(defaults, short-lookup), init, args)
  result: if(final.pending ✓,
    panic("option --{final.pending str.of} requires a value"),
    final.opts { args: final.args })
}.result
```

**IMPORTANT NOTES FOR IMPLEMENTOR:**

1. The code above is a sketch. Many patterns may need adjustment:
   - `{ [key]: value } block` for dynamic key construction — verify this works. The pattern is to create a list `[[key, value]]` then call `block`.
   - `sym` conversion from strings for key lookup — check `str.of` and sym construction.
   - `meta(v)` on block element values — verify metadata is accessible this way. May need to access via `elements` which returns `[key, value]` pairs where the value retains its metadata.

2. Read `docs/appendices/syntax-gotchas.md` — especially catenation precedence traps.

3. The `generate-help` function uses string interpolation `{...}` — ensure the expressions inside braces are valid.

4. `✓` is the non-null check operator.

5. `parse-as(:json, str)` is used for number coercion — `"42" parse-as(:json)` gives `42`.

6. Dynamic block construction: to create `{verbose: true}` from a runtime key, use `[[key, true]] block` where key is a symbol.

- [ ] **Step 5: Run tests**

```bash
cargo test test_harness_126
```

Fix any issues. The prelude code above is a sketch and will almost certainly need debugging. Key areas to watch:
- Dynamic block construction with runtime keys
- Metadata access on block element values
- String-to-symbol conversion for key lookup
- The combined short flags logic (-vo where v is flag, o takes next arg)

- [ ] **Step 6: Run full test suite**

```bash
cargo test
```

Verify no existing tests break.

- [ ] **Step 7: Commit**

```bash
git add lib/prelude.eu tests/harness/126_parse_args.eu tests/harness_test.rs
git commit -m "feat: add parse-args for structured command-line argument parsing"
```

---

## Task 2: Documentation

**Files:**
- Create: `docs/reference/prelude/args.md`
- Modify: `docs/reference/agent-reference.md` (add parse-args reference)

- [ ] **Step 1: Create args reference doc**

Create `docs/reference/prelude/args.md` with:
- Function signature and description
- Metadata keys (short, doc, flag)
- Type coercion rules
- Help generation
- Complete example
- Pipeline usage: `io.args parse-args(defaults)`

- [ ] **Step 2: Update agent reference**

Add `parse-args` to the prelude function table in `docs/reference/agent-reference.md`.

- [ ] **Step 3: Commit**

```bash
git commit -m "docs: add parse-args reference documentation"
```

---

## Task 3: Error test for unknown option

**Files:**
- Create: `tests/harness/errors/074_unknown_arg.eu`
- Create: `tests/harness/errors/074_unknown_arg.eu.expect`
- Modify: `tests/harness_test.rs`

- [ ] **Step 1: Create error test**

`tests/harness/errors/074_unknown_arg.eu`:
```eu
main: ["--unknown-flag"] parse-args({})
```

`tests/harness/errors/074_unknown_arg.eu.expect`:
```yaml
exit: 1
stderr: "unknown option"
```

Add to `tests/harness_test.rs`:
```rust
harness_test!(test_error_074, "errors/074_unknown_arg");
```

- [ ] **Step 2: Run test**

```bash
cargo test test_error_074
```

- [ ] **Step 3: Commit**

```bash
git commit -m "test: add error test for unknown parse-args option"
```

---

## Implementation Notes

### Dynamic block construction pattern

Creating a block with a runtime key requires:
```eu
# From a [key, value] pair where key is a symbol:
[[key, value]] block

# From a string key name:
[[:my-key, value]] block
# or
[[key-str sym, value]] block
```

Verify this pattern works before relying on it in the fold logic.

### Type coercion strategy

- Default is string → value stays as string
- Default is number → `value parse-as(:json)` (parses "42" as 42, "3.14" as 3.14)
- Default is bool (non-flag) → `value = "true"` gives true, else false
- Flag (flag: true in metadata) → no value arg, presence sets to true

### Short flag expansion

`-vo out.txt` should expand as:
1. `v` is a flag → set verbose=true
2. `o` needs a value → set pending=:output
3. Next arg `out.txt` fills pending → output="out.txt"

But `-von3` is ambiguous — is `n3` a short flag `n` with value `3`, or short flags `n` and `3`? The implementation should process character by character: if the char maps to a non-flag option, everything remaining in the string becomes its value. This is more complex than the sketch above — the implementor should handle this case.
