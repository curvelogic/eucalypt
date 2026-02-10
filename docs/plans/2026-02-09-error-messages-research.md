# Error Messages and Diagnostics: Research Report

## 1. Current State

### 1.1 Error Handling Architecture

Eucalypt has a layered error type hierarchy:

- **`EucalyptError`** (`src/driver/error.rs`) — top-level enum wrapping all
  error categories, each variant delegating to `to_diagnostic()` on the inner
  error type.
- **`ParserError` / `SyntaxError`** (`src/syntax/error.rs`) — parse-time errors
  from LALRPOP and the Rowan parser. These carry file IDs and byte spans for
  source location.
- **`CoreError`** (`src/core/error.rs`) — errors from desugaring, operator
  resolution, and variable binding verification. These carry `Smid` (Source Map
  ID) references.
- **`SourceError`** (`src/import/error.rs`) — errors from parsing foreign
  formats (YAML, JSON, TOML, XML, EDN).
- **`ImportError`** (`src/syntax/import.rs`) — import graph errors (cycles,
  missing inputs).
- **`CompileError`** (`src/eval/stg/compiler.rs`) — errors from STG
  compilation (free variables, bad expressions).
- **`ExecutionError`** (`src/eval/error.rs`) — runtime errors from the VM with
  30+ variants covering type mismatches, arithmetic errors, lookup failures,
  black holes, panics, and more.

All error types use `thiserror` for `Display` formatting. Diagnostic rendering
uses `codespan-reporting` (v0.11.1) which produces Rust-compiler-style output
with source snippets and coloured labels.

### 1.2 Source Location Tracking

Source locations are tracked via `Smid` (Source Map ID) — a `NonZeroU32` index
into a `SourceMap` (in `src/common/sourcemap.rs`). Each `Smid` maps to a
`SourceInfo` containing an optional file ID, byte span, and text annotation.

Compile-time errors (`CoreError`, `CompileError`) generally have valid `Smid`
values, so diagnostics can point to source locations. Runtime errors
(`ExecutionError`) frequently have `Smid::default()` (invalid/absent), meaning
**many runtime errors lack source location information entirely**.

### 1.3 Stack Traces

The VM (`src/eval/machine/vm.rs`) wraps execution errors in
`ExecutionError::Traced(error, env_trace, stack_trace)` where:

- **env_trace**: annotations from the environment chain (often intrinsic names
  like `PANIC`, `SATURATED`)
- **stack_trace**: annotations from continuation stack entries

These are rendered as notes on the diagnostic. The stack trace is deduped and
limited to 64 entries.

### 1.4 Actual Error Messages Users See Today

Below are actual error messages captured by running `eu run` on deliberately
broken programs. ANSI colour codes have been stripped for readability.

#### Type Mismatch (passing number where string expected)

```
$ echo 'x: 1 + "hello"' > test.eu && eu run test.eu

error: no branch for data tag 5
 = environment trace:
 = stack trace:
   - SATURATED
   - AND
```

**Problems**: The message "no branch for data tag 5" is meaningless to users.
Tag 5 is `BoxedString` internally. No source location. No indication of what
operation failed or what types were involved.

#### Undefined Variable

```
$ echo 'x: nonesuch' > test.eu && eu run test.eu

error: unresolved free variable reference [3700]
  ┌─ /tmp/eu_undef.eu:1:4
  │
1 │ x: nonesuch
  │    ^^^^^^^^
```

**Assessment**: This is actually one of the better error messages — it points
to the exact source location. The `[3700]` Smid reference is noise, however.

#### Division by Zero

```
error: out of range error operating on numbers (1, 0)
 = environment trace:
 = stack trace:
   - EMITx
```

**Problems**: Does not say "division by zero". Stack trace shows only internal
intrinsic names (`EMITx`). No source location.

#### Parse Error (unterminated string)

```
error: input was not correctly formed: Parse errors: UnclosedDoubleQuote { range: 3..13 }
  ┌─ /tmp/eu_parse.eu:1:1
  │
1 │ a: "unending
  │ ^
```

**Problems**: Raw Rust debug format (`UnclosedDoubleQuote { range: 3..13 }`)
leaks into the message. Source pointer hits the wrong location (`a` instead of
the string literal).

#### Panic

```
error: panic: something went wrong
 = environment trace:
   - PANIC
 = stack trace:
   - SATURATED
   - AND
```

**Assessment**: The user message is fine, but the trace shows only intrinsic
machinery (`PANIC`, `SATURATED`, `AND`). Users cannot relate these to their
source code.

#### Too Many Arguments

```
error: call of not callable
 = environment trace:
 = stack trace:
   - SATURATED
   - AND
```

**Problems**: "call of not callable" does not explain that a 1-argument
function was called with 2 arguments. No source location. No mention of what
was called or expected arity.

#### Argument Type Mismatch (number to string function)

```
$ echo 'x: str.letters(99)' > test.eu && eu run test.eu

error: no branch for data tag 3
 = environment trace:
 = stack trace:
   - SATURATED
   - AND
```

**Problems**: Identical shape to the type mismatch above. "data tag 3" is
`BoxedNumber`. No indication that `letters` expects a string but received a
number.

#### Self-Reference / Black Hole

```
error: infinite loop detected: binding refers to itself
 = environment trace:
 = stack trace:
   - SATURATED
   - AND
```

**Assessment**: The message text is clear enough. Missing source location.
Stack trace is unhelpful.

#### Missing File

```
error: path /tmp/nonexistent.eu could not be read
```

**Assessment**: Clear and useful. No source snippet needed.

#### Empty Expression

```
error: input was not correctly formed: Parse errors: EmptyExpression { range: 12..12 }
  ┌─ /tmp/eu_empty_val.eu:1:1
  │
1 │ a: 1
  │ ^
```

**Problems**: Raw debug format again. Points to line 1 rather than the empty
expression on line 3.

#### Key Not Found (lookup on block)

```
$ echo 'x: str.upper(42)' > test.eu && eu run test.eu

error: panic: Key not found: upper
 = environment trace:
   - PANIC
 = stack trace:
   - SATURATED
   - AND
```

**Problems**: This is reported as a panic rather than a structured lookup
failure. The prelude's `str` block does not have `upper`; this error arises
because `42` is a number, not a string, so the dispatch block does not match.
The real issue is a type mismatch, but the error says "Key not found: upper".


## 2. Key Problems

### 2.1 Unintelligible Data Tag Numbers

**Severity: Critical**

`NoBranchForDataTag(u8)` shows raw numeric tags (3, 5, 7, etc.) that
correspond to internal `DataConstructor` variants. Users never see or know
about these tag values. Error messages like "no branch for data tag 5" are
completely opaque.

Tags and their human meanings:
| Tag | DataConstructor | What the user would understand |
|-----|----------------|-------------------------------|
| 0 | Unit | null |
| 1 | BoolTrue | true |
| 2 | BoolFalse | false |
| 3 | BoxedNumber | a number |
| 4 | BoxedSymbol | a symbol |
| 5 | BoxedString | a string |
| 6 | ListNil | an empty list |
| 7 | ListCons | a list |
| 8 | Block | a block |
| 9 | BlockPair | a key-value pair |
| 10 | BlockKvList | a key-value list |
| 11 | BoxedZdt | a datetime |

### 2.2 Missing Source Locations for Runtime Errors

**Severity: Critical**

Most `ExecutionError` variants use `Smid::default()` (invalid), meaning no
source location is available. The `HasSmid` implementation returns defaults for
many variants. When the source map cannot resolve a Smid, the diagnostic has
no source pointer.

Root cause: intrinsic implementations in `src/eval/stg/support.rs` consistently
pass `Smid::default()` when constructing errors (e.g., `NotEvaluatedNumber`,
`NotEvaluatedString`, `NotEvaluatedZdt`). The VM itself (`vm.rs`) also
creates errors like `NotCallable(Smid::default())`.

### 2.3 Stack Traces Show Internal Machinery

**Severity: High**

Stack traces contain only STG-level intrinsic names like `SATURATED`, `AND`,
`PANIC`, `EMITx`. Users do not know what `SATURATED` means (it is the
saturation check wrapper). They cannot connect these to their source code.

The traces lack:
- Source file and line numbers for each frame
- The eucalypt-level names (e.g., `str.letters` rather than `LETTERS`)
- A clear connection between trace entries and the user's programme

### 2.4 Raw Debug Formatting Leaks

**Severity: Medium**

Parse errors include Rust debug representations:
- `UnclosedDoubleQuote { range: 3..13 }` instead of "unterminated string
  literal"
- `EmptyExpression { range: 12..12 }` instead of "empty expression"

These come from the Rowan parser error types being formatted via `Debug` rather
than having proper `Display` implementations.

### 2.5 Generic / Misleading Error Messages

**Severity: High**

Several errors use vague or misleading descriptions:

- **"call of not callable"** — does not explain what was called, what it
  actually is, or what the expected arity was.
- **"out of range error operating on numbers (1, 0)"** — does not say
  "division by zero".
- **"cannot return function into case table without default"** — internal
  compiler jargon.
- **"expected branch continuation"** — pure implementation detail.
- **"binding missing from environment"** — internal VM state description.
- **"no branch for native"** — meaningless to users.

### 2.6 Panics in Intrinsic Support Code

**Severity: Medium**

`src/eval/stg/support.rs` contains 9 `panic!()` calls in iterators
(`DataIterator`, `StrListIterator`) that would crash the process rather than
producing a diagnostic. These cover cases like "Non-list data after force" and
"bad cons cell". While these should be internal invariant violations, a crashed
process with a Rust backtrace is far worse than a graceful error.

### 2.7 Error Test Coverage Gaps

**Severity: Medium**

The `harness/test/errors/` directory has 28 test files but only 19 have
`.expect` sidecar files. Several error conditions have no test coverage at all:
- No test for `NoBranchForDataTag` with human-readable output
- No test for arithmetic type mismatches
- No test for stack overflow / deep recursion
- No test for allocation failures

### 2.8 Smid References in Error Messages

**Severity: Low**

The compile error `"unresolved free variable reference [3700]"` includes the
raw Smid value `[3700]`, which is an implementation detail. When a source
location is available (as it is for free variable errors), the Smid adds no
value.


## 3. Modern Best Practices

### 3.1 Rust Compiler (rustc)

Considered the gold standard for compiler error messages:

- **Show, don't tell**: Labels on source code rather than prose descriptions.
- **"What" and "why"**: Each error explains what went wrong and why.
- **Suggestions**: Machine-applicable fix suggestions where possible.
- **Error codes**: E0308-style codes linking to detailed explanations.
- **Multiple labels**: Primary and secondary labels connecting related
  locations (e.g., showing where a type was inferred and where it conflicted).
- **Colour and structure**: Errors are visually distinct, accessible without
  colour, and work across terminal types.
- **Continuous improvement**: Hundreds of contributors have refined messages
  over 10+ years.

### 3.2 Elm Compiler

Famous for the friendliness of its error messages:

- **Conversational tone**: Uses "I" and "we" to create a dialogue.
- **One error at a time**: Shows the most relevant error first rather than
  flooding the user.
- **Concrete suggestions**: "Maybe you meant X?" with specific alternatives.
- **Minimal source context**: Points to the smallest possible location.
- **No jargon**: Hides compiler internals completely.
- **Present tense**: "I see..." rather than "found...".

### 3.3 GHC / Haskell

Recent improvements (2024-2025) relevant to eucalypt:

- **Structured error codes**: `[GHC-12345]` codes that link to the Haskell
  Error Index (errors.haskell.org) with detailed explanations and examples.
- **User-written qualification**: Error messages display names as the user
  wrote them, not as the compiler represents them internally.
- **Error annotation plugins**: Extensible error enrichment system.
- **HLS integration**: Structured errors enable IDE tools to provide
  context-aware fixes.

### 3.4 General Best Practices

From the research, the most important principles for eucalypt are:

1. **Never expose internal representations** — no tag numbers, no Smid values,
   no intrinsic names like `SATURATED`.
2. **Always provide a source location** if one exists anywhere in the call
   chain.
3. **Explain the mismatch** — "expected X, found Y" with both sides.
4. **Name things as the user knows them** — `str.letters` not `LETTERS`,
   "string" not "data tag 5", "block" not "Record".
5. **Suggest fixes** where the error is unambiguous.
6. **Keep format stable** for tooling integration (error codes, structured
   output).


## 4. Library Options

### 4.1 Keep codespan-reporting (Current)

**Pros**:
- Already integrated and working.
- Produces the correct general format (source snippets, labels, notes).
- Stable, maintained library.
- Simple `Diagnostic<FileId>` API.

**Cons**:
- Less visually sophisticated than newer alternatives.
- No built-in support for suggestions / fix hints.
- The underlying quality issues are in eucalypt's error generation, not in
  `codespan-reporting`'s rendering.

### 4.2 Switch to Ariadne

**Pros**:
- More visually appealing output with Unicode box-drawing.
- Rich label system with colours and priorities.
- Multi-file span support.
- Active development.

**Cons**:
- Requires rewriting all diagnostic construction code.
- Different API philosophy (builder pattern vs declarative).
- Not clear the visual improvements justify the migration effort.

### 4.3 Switch to Miette

**Pros**:
- Most ergonomic API via derive macros on error types.
- Built-in support for suggestions, help text, related errors.
- Fancy terminal output with the `"fancy"` feature.
- Good integration with `thiserror`.

**Cons**:
- Pulls in more dependencies (especially the `"fancy"` feature).
- `GraphicalReportHandler` may not match current output style.
- Most of miette's power is in its protocol/trait system, which would
  require rethinking the error type hierarchy.

### 4.4 Recommendation

**Stay with codespan-reporting.** The rendering library is not the bottleneck.
The problems are upstream: poor error message text, missing source locations,
exposed internal representations, and unhelpful stack traces. Fixing these
will yield 90% of the improvement without any library migration. A library
switch can be revisited once the error content is right.


## 5. Recommendations

### Priority 1: Quick Wins (Low effort, high impact)

#### P1.1: Human-readable data tag names in `NoBranchForDataTag`

Replace the raw `u8` tag with a human-readable type name. Implement
`Display` for `DataConstructor` and use `TryFrom<Tag>` to convert. Change
the error message from "no branch for data tag 5" to something like:

```
error: type mismatch — expected a number, but found a string
```

Or at minimum:

```
error: unexpected value type: string (expected a different type here)
```

**Files**: `src/eval/error.rs`, `src/eval/stg/tags.rs`, `src/eval/machine/vm.rs`

#### P1.2: Recognise common arithmetic error patterns

Map `NumericRangeError` with specific operand patterns to user-friendly
messages. When the divisor is 0, say "division by zero" rather than "out of
range error operating on numbers (1, 0)".

**Files**: `src/eval/error.rs` (custom `Display` or match in `to_diagnostic`)

#### P1.3: Clean up parse error formatting

Implement proper `Display` for Rowan parser error types so that
`UnclosedDoubleQuote` displays as "unterminated string literal" and
`EmptyExpression` displays as "empty expression where a value was expected".

**Files**: Rowan parser error types, `src/syntax/error.rs`

#### P1.4: Remove Smid values from user-facing messages

Strip `[3700]`-style Smid references from the `Display` implementation of
`CompileError::FreeVar`. The source location pointer already provides this
information.

**Files**: `src/eval/stg/compiler.rs`

#### P1.5: Improve "call of not callable" message

Where possible, include the actual type encountered and the expected arity.
At minimum, reword to "tried to call a value that is not a function".

**Files**: `src/eval/error.rs`, `src/eval/machine/vm.rs`

### Priority 2: Medium Effort Improvements

#### P2.1: Propagate source locations through intrinsic errors

Pass the annotation `Smid` from the machine state into intrinsic error
constructors instead of `Smid::default()`. The `MachineState` already tracks
`annotation: Smid` — this needs to be threaded into the `IntrinsicMachine`
trait or passed to `ExecutionError` constructors.

**Impact**: All runtime errors would gain source locations.
**Files**: `src/eval/stg/support.rs`, `src/eval/machine/intrinsic.rs`,
`src/eval/machine/vm.rs`

#### P2.2: Map intrinsic names to user-facing names in stack traces

Create a mapping from internal intrinsic names to eucalypt-level names:
- `LETTERS` → `str.letters`
- `ADD` → `+` or `arithmetic addition`
- `LOOKUP` → `block lookup`
- `SATURATED` / `AND` → omit from trace (internal machinery)

Filter out purely internal frames and only show frames the user can relate
to their source code.

**Files**: `src/eval/intrinsics.rs` (add display name field),
`src/common/sourcemap.rs` (trace formatting)

#### P2.3: Source-level stack traces

Enhance the stack trace to show source file:line references where Smid
values resolve to file locations. Currently traces show only annotation
text. With valid Smids, each frame could show:

```
stack trace:
  - /path/to/file.eu:5:3 (in definition of 'x')
  - /path/to/file.eu:3:7 (in definition of 'helper')
```

**Files**: `src/common/sourcemap.rs` (`format_trace`), `src/driver/eval.rs`

#### P2.4: Convert panics in support.rs to proper errors

Replace the 9 `panic!()` calls in `DataIterator` and `StrListIterator`
with `ExecutionError` results. This requires changing the iterator
interface to return `Result` items.

**Files**: `src/eval/stg/support.rs`

#### P2.5: Type mismatch error with expected vs. actual

For intrinsic type checking, when `num_arg` or `str_arg` fails because the
value is of the wrong type, detect the actual type and produce:

```
error: type mismatch in str.letters: expected Str, found Num
```

The intrinsic type signatures are already available in the `INTRINSICS`
catalogue (`src/eval/intrinsics.rs`).

**Files**: `src/eval/stg/support.rs`, `src/eval/error.rs`

### Priority 3: Longer-Term Architectural Work

#### P3.1: Error codes and documentation

Assign stable error codes (e.g., `[EU-0001]`) to each error variant.
Create a documentation page (similar to Haskell Error Index or Rust E-codes)
that explains each error with examples. This enables:
- Stable references in documentation and Stack Overflow
- `eu explain EU-0001` command
- IDE integration

**Effort**: Moderate, but mostly documentation rather than code.

#### P3.2: Contextual "did you mean?" suggestions

For lookup failures (`Key not found: upper`), check for similar keys in the
block using edit distance and suggest the closest match:

```
error: key 'upper' not found in block
help: similar keys exist: 'upper-case', 'to-upper'
```

For free variable errors, check for similar names in scope.

**Files**: `src/eval/stg/block.rs`, `src/core/error.rs`

#### P3.3: Multi-label diagnostics

For type mismatches, show both the definition site (where the type was
determined) and the use site (where the mismatch occurred):

```
error: type mismatch in call to str.letters
  ┌─ example.eu:1:16
  │
1 │ x: str.letters(99)
  │                ^^ this is Num
  │
note: str.letters expects Str
```

**Files**: diagnostic construction in `src/eval/error.rs`,
`src/common/sourcemap.rs`

#### P3.4: Error recovery and multiple error reporting

Currently, eucalypt stops at the first error. For parse errors, the Rowan
parser could potentially report multiple errors. For compile-time errors
(free variables, redeclared names), accumulating and reporting all errors at
once would save the user from fix-one-rerun cycles.

**Files**: `src/driver/prepare.rs`, `src/core/verify/`

#### P3.5: Structured error output for tooling

Offer `--error-format=json` to emit errors as structured JSON, enabling
IDE integration, CI pipelines, and the LSP server to consume errors
programmatically.

**Files**: `src/driver/eval.rs`, `src/driver/options.rs`


## 6. Quick Wins vs Long Term

### Can Be Done Now (1-2 days each)

| Item | Impact | Effort |
|------|--------|--------|
| P1.1: Human-readable tag names | Critical | Small |
| P1.2: "Division by zero" message | High | Trivial |
| P1.3: Clean parse error formatting | High | Small |
| P1.4: Remove Smid from messages | Low | Trivial |
| P1.5: Better "not callable" message | Medium | Small |

### Medium Term (3-5 days each)

| Item | Impact | Effort |
|------|--------|--------|
| P2.1: Propagate Smid to intrinsics | Critical | Medium |
| P2.2: User-facing intrinsic names | High | Medium |
| P2.3: Source-level stack traces | High | Medium |
| P2.4: Convert panics to errors | Medium | Small |
| P2.5: Expected vs actual types | High | Medium |

### Long Term (1-2 weeks each)

| Item | Impact | Effort |
|------|--------|--------|
| P3.1: Error codes + docs | Medium | Large |
| P3.2: "Did you mean?" | Medium | Medium |
| P3.3: Multi-label diagnostics | High | Large |
| P3.4: Multiple error reporting | Medium | Large |
| P3.5: Structured JSON output | Medium | Medium |


## 7. Relationship to Child Beads

The three child beads map directly to specific recommendations:

- **eu-w4s0 (type mismatches)** → P1.1 (data tag names), P2.5 (expected vs
  actual types), P3.3 (multi-label diagnostics)
- **eu-tk4r (stack traces)** → P2.2 (user-facing names), P2.3 (source-level
  traces), P2.1 (Smid propagation)
- **eu-l7e1 (data tag names)** → P1.1 (human-readable tag names), directly
  implementing `Display` for `DataConstructor`


## 8. Summary

The most impactful improvements are:

1. **Translate data tag numbers to human-readable type names** — eliminates
   the single most confusing class of error messages.
2. **Propagate source locations into runtime errors** — gives users something
   to point at when debugging.
3. **Filter and translate stack trace entries** — makes traces useful rather
   than mystifying.
4. **Clean up raw debug formatting** — removes Rust implementation details
   from user-facing output.

These four changes would transform the error experience from "mostly useless
for runtime errors" to "helpful and actionable" without requiring any library
migration or major architectural changes.
