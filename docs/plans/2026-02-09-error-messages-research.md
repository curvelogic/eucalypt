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


## Deep Investigation: Trace Machinery

### Test Cases and Verbatim Output

Four test programs were created to exercise user-defined function errors.

**Test A** — User function calling intrinsic incorrectly:
```eucalypt
f(x): x + "hello"
main: f(42)
```
Output:
```
error: no branch for data tag 5
 = environment trace:
 = stack trace:
   - SATURATED
   - AND
```

**Test B** — Nested user function calls leading to error:
```eucalypt
inner(x): x / 0
middle(x): inner(x + 1)
outer(x): middle(x * 2)
main: outer(5)
```
Output:
```
error: out of range error operating on numbers (11, 0)
 = environment trace:
 = stack trace:
   - EMITx
```

**Test C** — Higher-order function passing:
```eucalypt
apply(f, x): f(x)
bad(x): x + "oops"
main: apply(bad, 42)
```
Output:
```
error: no branch for data tag 5
 = environment trace:
 = stack trace:
   - SATURATED
   - AND
```

**Test D** — Recursive function with base-case error:
```eucalypt
f(x): if(x = 0, x / 0, f(x - 1))
main: f(5)
```
Output:
```
error: out of range error operating on numbers (0, 0)
 = environment trace:
 = stack trace:
   - EMITx
```

### Key Observation: No User Function Names Appear

Across all four test cases:

- The **environment trace is always empty** — zero entries in every case.
- The **stack trace contains only intrinsic names** — `EMITx`, `SATURATED`,
  `AND` — all belonging to the rendering pipeline.
- No user-defined function name (`f`, `inner`, `middle`, `outer`, `apply`,
  `bad`) appears anywhere in any trace.

Instrumented debugging confirmed the raw Smid vectors: `env_trace` yields `[]`
(empty) and `stack_trace` yields Smids that resolve exclusively to intrinsic
globals.

### Root Cause Analysis

There are three independent root causes, all of which must be addressed for
useful traces.

#### Root Cause 1: Lazy evaluation defers errors to the rendering pipeline

Eucalypt uses lazy evaluation. Computations like `x / 0` do not execute
immediately; they are packaged as thunks. The error only surfaces when the
rendering pipeline (EMITx, SATURATED, AND) forces the result for output.

By the time the error is thrown, the machine's continuation stack and current
environment reflect the **rendering pipeline's context**, not the user
function call chain. The `inner`, `middle`, and `outer` function calls have
long since returned (yielding lazy thunks), and their stack frames no longer
exist.

**Code location**: Error wrapping at `src/eval/machine/vm.rs:862-870`.

#### Root Cause 2: `self.annotation` is volatile and gets overwritten on every closure entry

The VM has a single `annotation: Smid` field on `MachineState` (vm.rs:154)
that tracks the "current" source annotation. It is used to stamp environment
frames when they are created by `Let`/`LetRec` nodes (vm.rs:281, 287).

The `Ann` node (vm.rs:290-292) correctly sets `self.annotation` to the user
function's Smid. However, at the **start of every instruction execution**
(vm.rs:211), the annotation is unconditionally overwritten:

```rust
self.annotation = self.closure.annotation();
```

User-defined lambda forms are created via `dsl::lambda()` which passes
`Smid::default()` (src/eval/stg/syntax.rs:437). In contrast, intrinsic
wrappers use `annotated_lambda()` which carries a valid Smid.

The effect is:

1. Lambda is entered. `self.annotation` is set to `Smid::default()` (from the
   lambda form).
2. Lambda body starts with `Ann { smid, body }`. `self.annotation` is set to
   the correct user Smid.
3. The first sub-expression forces an argument (enters another closure).
   `self.annotation` is overwritten to that closure's annotation
   (`Smid::default()` for thunks/values).
4. All environment frames created thereafter carry `Smid::default()`.

**Code locations**:
- `dsl::lambda` uses `Smid::default()`: `src/eval/stg/syntax.rs:436-438`
- Annotation overwrite on entry: `src/eval/machine/vm.rs:211`
- `Ann` node handling: `src/eval/machine/vm.rs:290-292`
- `InfoTagged::thunk` and `::value` use `Smid::default()`:
  `src/eval/memory/infotable.rs:97-110`

#### Root Cause 3: `Continuation::ApplyTo` has no environment and is skipped in stack traces

The `stack_trace_iter` function (vm.rs:593-617) collects Smids from
continuations. It handles `Branch`, `Update`, and `DeMeta` continuations
(which have environment fields), but **skips `ApplyTo` entirely** (line 607:
`_ => return None`).

Function applications push `ApplyTo` continuations (vm.rs:272). Since user
function calls are function applications, they never contribute to the stack
trace. The only continuations that DO contribute are Cases (Branch), thunk
updates (Update), and metadata destructuring (DeMeta) — all internal
machinery.

Furthermore, `ApplyTo` (cont.rs:50) does not carry an `environment` field at
all — it only stores the argument closures. So even if the stack trace code
attempted to include it, there would be no environment frame to extract an
annotation from.

**Code locations**:
- `ApplyTo` definition: `src/eval/machine/cont.rs:50`
- Stack trace skips `ApplyTo`: `src/eval/machine/vm.rs:607`
- App pushes `ApplyTo`: `src/eval/machine/vm.rs:272`

### Supplementary Findings

#### Annotations are correctly stored in the source map

The desugarer creates annotated Smids for user-defined lambdas with the
function name:

```rust
// src/core/desugar/rowan_ast.rs:928-932
expr = core::lam(
    desugarer.new_annotated_smid(components.span, &components.name),
    components.arg_vars,
    expr.clone(),
);
```

This calls `SourceMap::add_annotated()` which stores both the byte span AND
the annotation string (the function name). So the source map has the
information needed to produce useful traces — it simply never receives the
right Smids.

#### The optimiser preserves Ann nodes

The `AllocationPruner` (src/eval/stg/optimiser.rs:296-299) correctly
reconstructs `Ann` nodes during its traversal:

```rust
StgSyn::Ann { smid, body } => Rc::new(StgSyn::Ann {
    smid: *smid,
    body: self.apply(body.clone()),
}),
```

So `Ann` nodes are not stripped during optimisation.

#### Intrinsic wrappers correctly appear because they use `annotated_lambda`

Intrinsic global wrappers are compiled with `annotated_lambda()` which stores
the annotation Smid on the `LambdaForm` itself (via `InfoTagged`). When these
closures are entered, `self.annotation = self.closure.annotation()` picks up
the intrinsic's Smid (e.g., "EMITx", "SATURATED"). This is why intrinsic
names DO appear in traces — the annotation mechanism works correctly for them.

The discrepancy is that user lambdas use `dsl::lambda()` (which passes
`Smid::default()`) and rely on the `Ann` body wrapper, while intrinsic
wrappers use `annotated_lambda()` and store the annotation on the closure
itself.

#### `format_trace` would work correctly if given valid Smids

The `format_trace` function (src/common/sourcemap.rs:206-229) first checks
for an annotation string, then falls back to extracting the source text from
the byte span. User function Smids have both annotation strings and spans.
The rendering logic is fine — it just never receives user function Smids.

### Assessment: Fixing vs Replacing the Machinery

The trace machinery does not need to be replaced wholesale. The core
components — source map, annotation storage, `format_trace`, `Ann` nodes —
are sound. What needs fixing are the specific gaps in the pipeline:

1. **The `Ann` node's annotation is immediately overwritten.** This is the
   most fundamental issue. The current design assumes `self.annotation` will
   persist across sub-expression evaluation, but it does not.

2. **`ApplyTo` carries no annotation information.** Function call
   continuations are invisible to the trace machinery.

3. **Lazy evaluation disconnects errors from their call sites.** This is the
   hardest problem and is inherent to the evaluation model.

### Concrete Recommendations

#### R1: Store annotation on the `LambdaForm`, not (only) in the body

Change `ProtoLambda::take_lambda_form` (src/eval/stg/compiler.rs:723-753) to
use `annotated_lambda` instead of `lambda`, passing the Smid directly to the
lambda form. This mirrors what intrinsic wrappers already do:

```rust
Ok(dsl::annotated_lambda(
    args.try_into().or(Err(CompileError::MaxLambdaArgs))?,
    body,
    self.annotation,
))
```

This ensures that when a user function closure is entered, `self.annotation`
is set to the function's Smid, and it persists on the closure itself
(surviving re-entry). The `Ann` body wrapper becomes redundant but harmless.

**Impact**: Environment frames created inside user function bodies would carry
the function's annotation. The env_trace would show user function names.

#### R2: Add annotation support to `Continuation::ApplyTo`

Extend `ApplyTo` to carry the current `self.annotation` Smid:

```rust
ApplyTo { args: Array<SynClosure>, annotation: Smid },
```

Set this when pushing the continuation (vm.rs:272):

```rust
self.push(view, Continuation::ApplyTo {
    args: array,
    annotation: self.annotation,
})?;
```

Update `stack_trace_iter` to include `ApplyTo` annotations:

```rust
Continuation::ApplyTo { annotation, .. } => *annotation,
```

**Impact**: Function call sites would appear in stack traces, including user
function calls.

**Note**: This increases the size of the `Continuation` enum and adds a field
to GC scanning. The overhead is one `Smid` (4 bytes) per `ApplyTo`
continuation.

#### R3: Consider a "source trace" independent of lazy evaluation

For lazily-evaluated values, the error arises far from the call site. The
fundamental fix would require the thunk to carry provenance information — a
chain of Smids recording where the thunk was created and by which function.
This is similar to how GHC's `-fprof-auto` inserts cost centres.

A lighter-weight approach: when wrapping an error in `Traced`, also include
the **thunk's closure annotation** (from `self.closure.annotation()` at error
time). Even if `self.annotation` has been overwritten, the closure's own
annotation may still carry useful information — particularly if R1 is
implemented.

This is the hardest problem and may warrant a longer-term design discussion
about whether to:
- Accept the lazy evaluation limitation and focus on making
  compile-time/immediate errors excellent.
- Implement cost-centre-style provenance tracking on thunks (significant
  overhead).
- Add a strict evaluation mode for debugging purposes.

#### R4: Enrich the `Ann` node or replace it with a different mechanism

As an alternative to R1, the compiler could emit the annotation Smid on the
lambda form AND retain the `Ann` body wrapper. The `Ann` node would then serve
as a redundant "refresh" of the annotation (useful for nested lambdas or
higher-order cases where the closure annotation might not reflect the current
call site).

#### Priority ordering

1. **R1** (lambda form annotation) — highest impact, smallest change.
2. **R2** (ApplyTo annotation) — moderate impact, small change.
3. **R4** (redundant Ann) — minor reinforcement, trivial change.
4. **R3** (lazy provenance) — hardest problem, requires design decision.


## Analysis: Environment Trace vs Stack Trace

### What Each Trace Represents

The eucalypt VM captures two separate traces when a runtime error occurs, both
stored as `Vec<Smid>` inside `ExecutionError::Traced(error, env_trace,
stack_trace)`.

#### Stack Trace: Dynamic Call Flow

The stack trace is collected by `stack_trace_iter()` in
`src/eval/machine/vm.rs:593-617`. It iterates over the continuation stack in
reverse (most recent first) and extracts annotations from the environment
fields of `Branch`, `Update`, and `DeMeta` continuations. `ApplyTo`
continuations are skipped entirely because they carry no environment reference.

Semantically, the stack trace represents **what the VM is currently doing** at
the point the error occurs — the chain of pending case analyses, thunk updates,
and metadata inspections. In practice, because of lazy evaluation, this almost
always reflects the **rendering pipeline** rather than the user's call chain.
Typical entries are `SATURATED`, `AND`, `EMITx`, `RENDER`, `CONS` — all
intrinsics involved in forcing values for output.

The stack trace performs inline deduplication (consecutive identical Smids are
collapsed) and is capped at 64 entries.

#### Environment Trace: Lexical Scope Chain

The environment trace is collected by `env_trace()` in `vm.rs:122-124`, which
delegates to `annotation_trace()` in `src/eval/machine/env.rs:267-283`. This
walks the linked list of `EnvironmentFrame`s from the current frame to the root,
collecting non-default `Smid` annotations from each frame.

Semantically, the environment trace represents **the lexical nesting of the
current closure's scope**. Each environment frame is created when the VM
processes a `Let`, `LetRec`, function saturation, case branch binding, or
`DeMeta` handler. The annotation stamped on each frame is `self.annotation` —
the machine state's current annotation Smid at the time of frame creation.

In theory, this should show something like "inside function `f`, inside block
`outer`, inside the prelude" — a breadcrumb trail through the lexical structure
of the programme. In practice, it shows almost nothing useful.

### Empirical Results

Twenty-two test cases were run across two categories: custom test files
exercising various error scenarios, and the existing `harness/test/errors/`
suite. Below is a complete summary.

#### Custom Test Files

| Test | Error Type | Env Trace | Stack Trace |
|------|-----------|-----------|-------------|
| `x: 1 + "hello"` | NoBranchForDataTag(5) | *(empty)* | SATURATED, AND |
| `f(x): x + "hello"; main: f(42)` | NoBranchForDataTag(5) | *(empty)* | SATURATED, AND |
| `make-adder("hello").add(42)` | NoBranchForDataTag(5) | *(empty)* | SATURATED, AND |
| `apply(bad, 5)` where `bad(x): x / 0` | NumericRangeError | *(empty)* | EMITx |
| `a: 1; b: a + "x"; main: b` | NoBranchForDataTag(5) | *(empty)* | SATURATED, AND |
| `map(_ + "x", [1, 2, 3])` | NoBranchForDataTag(5) | *(empty)* | RENDER, CONS |
| `h(x): g(x); g(x): f(x); f(x): x + "hello"` | NoBranchForDataTag(5) | *(empty)* | SATURATED, AND |
| Deep nested blocks with `panic(...)` | Panic | **PANIC** | SATURATED, AND |
| `f(x): panic("...")` call chain | Panic | **PANIC** | SATURATED, AND |
| `factory("bad").worker(42).result` | NoBranchForDataTag(5) | *(empty)* | SATURATED, AND |

#### Harness Error Tests (Runtime Errors Only)

| Test File | Error Type | Env Trace | Stack Trace |
|-----------|-----------|-----------|-------------|
| 002_lists | NoBranchForDataTag | *(empty)* | SATURATED, AND |
| 009_panic | Panic | **PANIC** | SATURATED, AND |
| 010_assert | Panic (assertion) | **PANIC** | SATURATED, AND |
| 011_assert_pred | Panic (assertion) | **PANIC** | SATURATED, AND |
| 017_too_many_args | NotCallable | *(empty)* | SATURATED, AND |
| 019_no_such_key | BlackHole | *(empty)* | LOOKUPOR#, SATURATED, AND |
| 020_no_such_key_fn | Panic (key not found) | **PANIC** | SATURATED, AND |
| 021_bad_num_parse | (number format) | *(empty)* | EMITx |
| 023_arg_types | NoBranchForDataTag(3) | *(empty)* | SATURATED, AND |
| 027_self_ref | BlackHole | *(empty)* | SATURATED, AND |

### Pattern: When Is the Environment Trace Non-Empty?

The environment trace is non-empty in **exactly one scenario**: when the error
is a `Panic`. In every such case, the trace contains a single entry: `PANIC`.

This occurs because `panic()` is implemented as a call to the `PANIC`
intrinsic, whose global wrapper closure was compiled with `annotated_lambda()`
and carries the synthetic Smid for `"PANIC"`. When the panic intrinsic
executes, the machine's `self.annotation` is the PANIC intrinsic's Smid, and
the current environment frames were created with that annotation.

For **all other error types** (type mismatches, arithmetic errors, black holes,
arity errors, lookup failures), the environment trace is completely empty. This
is because:

1. **`self.annotation` is almost always `Smid::default()`** at the point where
   environment frames are created. User lambda forms use `dsl::lambda()` which
   stores `Smid::default()` on the closure. When the closure is entered,
   `self.annotation = self.closure.annotation()` sets it to the default. The
   `Ann` node in the body briefly sets a valid annotation, but it is immediately
   overwritten when the next sub-expression is entered.

2. **Errors tend to occur inside intrinsic evaluation**, where the environment
   belongs to the intrinsic machinery (rendering pipeline), not the user's
   code. The environment chain at that point does not include frames from the
   user's function definitions.

3. **Lazy evaluation** means errors are deferred to the rendering pipeline. By
   the time the error surfaces, the user's function frames are no longer in the
   current environment chain — the error is being forced in a completely
   different execution context.

### Does the Environment Trace Provide Unique Value?

**No.** Based on the empirical evidence:

- When it is non-empty, it contains only intrinsic names (specifically
  `PANIC`), which also appear in the stack trace or are obvious from the error
  message itself.
- It never contains user-defined function names, file locations, or any
  information that would help a user locate the source of the error.
- It never provides information that is not already available from either the
  stack trace or the error message.
- The one scenario where it shows something (PANIC) is precisely the scenario
  where the error message itself already says "panic: ..." — the env trace
  entry adds nothing.

### Is It Confusing for Users?

**Yes.** The environment trace is actively confusing for several reasons:

1. **The label "environment trace" is opaque.** Users of a data-templating
   language have no reason to know what an "environment" is in the context of a
   closure-based VM. Even experienced programmers would likely think
   "environment" refers to shell environment variables or similar.

2. **It is almost always empty.** An empty trace labelled "environment trace:"
   followed by nothing creates visual noise and suggests the tool is broken or
   has incomplete information. Users may waste time wondering what should be
   there.

3. **When non-empty, it shows only intrinsic names.** Seeing `- PANIC` under
   "environment trace" does not help the user understand their programme. The
   intrinsic name is an implementation detail.

4. **Two traces are harder to parse than one.** Having two separate traces with
   unfamiliar labels forces users to understand a distinction (dynamic vs
   lexical) that is irrelevant to debugging.

### Theoretical Basis

#### Why Haskell/GHC Does Not Have Environment Traces

In GHC's STG machine, environment frames (closures) are the fundamental
evaluation mechanism, just as in eucalypt. However, GHC has never implemented
an "environment trace" for error reporting because:

- **The lexical nesting of closures is visible from the source code.** If a
  function `f` is defined inside a `where` clause of `g`, the user can see
  that by reading the source. Walking the environment chain at runtime adds
  nothing beyond what the source structure already shows.

- **The interesting question is "how did we get here?"** — which is a dynamic
  (call stack) question, not a lexical (scope nesting) question. GHC addresses
  this with cost centre stacks (for profiling) and `HasCallStack` (for
  explicit call chain tracking).

- **Lazy evaluation makes the runtime environment chain misleading.** A thunk
  created in one lexical context may be forced in a completely different
  dynamic context. The environment chain at force time reflects the thunk's
  *definition site* scope, not its *use site* call chain. But the definition
  site is already visible in the source code, so walking the environment adds
  no new information.

#### Does the Environment Trace Compensate for Laziness?

**In theory, it could.** The environment trace walks the closure chain from
where the thunk was *defined*, not where it was *forced*. This could complement
the stack trace (which shows where it was forced) by showing where it was
created.

**In practice, it does not**, because:

1. The annotations on the environment frames are `Smid::default()` for
   user-defined code (as documented in the "Root Cause 2" section above).
2. Even if annotations were present, the environment chain shows *lexical
   nesting* (which scopes contain which), not *creation provenance* (which
   function call created this thunk). These are different things. Lexical
   nesting is already visible in the source code.

For the environment trace to genuinely compensate for laziness, it would need
to carry *dynamic provenance* — recording which function call led to the
creation of each thunk, similar to GHC's cost centre stacks. The current
implementation does not do this; it merely records which lexical scopes are
nested inside which, and even that information is lost due to the annotation
overwrite issue.

### Could the Two Traces Be Merged?

Given that the environment trace provides no unique information, merging is
not the right framing. The question is whether the information the environment
trace *could theoretically provide* (if annotations were fixed) would be
worth integrating into a single, improved trace.

If the annotation issues from Root Cause 2 were fixed (via recommendation R1:
storing annotations on lambda forms), the environment trace would show the
lexical nesting of the current scope — e.g., "inside function `worker`, inside
function `factory`, inside the top-level block". But this information:

- Is already visible from the source code structure.
- Would be more naturally presented as part of the error's source location
  rather than as a separate trace.
- Would not help with the laziness problem (it shows where the code was
  *written*, not how execution reached it).

The stack trace, if enhanced with user function annotations (via
recommendations R1 and R2), would be strictly more useful because it shows
the dynamic chain of evaluation — which is not visible from the source code.

### Recommendation

**Drop the environment trace from error output.** The specific recommendation
is:

1. **Immediately**: Stop displaying the environment trace in error output.
   Remove the "environment trace:" note from `src/driver/eval.rs:213-216`.
   This eliminates visual noise, removes a confusing label, and loses no
   useful information (since the trace is almost always empty, and when
   non-empty contains only redundant intrinsic names).

2. **Short term**: Invest the effort saved into improving the stack trace
   (recommendations R1 and R2 from the previous section), which has genuine
   potential to show useful information if enhanced with user function
   annotations.

3. **Long term**: If a "definition site" trace is desired to complement the
   stack trace (addressing the laziness problem), design it as a proper
   provenance mechanism — recording which call site created each thunk —
   rather than walking the lexical environment chain. This is a significant
   design effort akin to GHC's cost centre system and should be treated as a
   separate project.

4. **Retain the `env_trace()` machinery internally** for now (do not delete
   the code), in case it becomes useful for debugging the VM itself. Simply
   stop exposing it to users.

### Summary Table

| Criterion | Environment Trace | Stack Trace |
|-----------|------------------|-------------|
| Non-empty for type errors | Never | Always |
| Non-empty for panics | Yes (shows "PANIC") | Yes (shows "SATURATED", "AND") |
| Shows user function names | Never | Never (fixable via R1+R2) |
| Shows source locations | Never | Never (fixable via R1+R2) |
| Unique information provided | None | Some (intrinsic call chain) |
| Understandable label | No ("environment trace") | Somewhat ("stack trace") |
| Useful after R1+R2 fixes | Marginally (lexical nesting) | Significantly (call chain) |
| Confusing to users | Yes | Somewhat |
