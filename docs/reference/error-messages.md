# Error Messages Guide

This page explains how to read the diagnostics eucalypt prints when
something goes wrong — their layout, the main categories you will meet,
the difference between warnings and errors, and how to interpret stack
traces and source locations.

All the transcripts below were produced by running small broken programs
against the `eu` binary. They are shown without terminal colour; in a
real terminal the severity word and the underlines are colourised.

## Anatomy of a diagnostic

Eucalypt renders diagnostics in the `codespan` style — the same style
used by `rustc`. There are no numeric error codes to memorise; a
diagnostic is identified by its **message**, and the location is shown by
pointing at the offending source span.

A typical diagnostic has four parts:

```text
error: unresolved variable 'subtotal'
  ┌─ example.eu:1:8
  │
1 │ total: subtotal + tax
  │        ^^^^^^^^
  │
  = check that the variable is defined and in scope
```

1. **Severity and message** — the first line. `error:` (evaluation cannot
   proceed) or `warning:` (advisory; see below).
2. **The source pointer** — `┌─ file:line:column` names where the problem
   is.
3. **The primary label** — the `^^^^^^^^` underline marks the exact span
   the message is about. Some diagnostics add *secondary* labels (further
   underlines with their own text) to relate two spans, for example the
   annotation and the value it disagrees with.
4. **Notes and help** — the `=` lines carry advice: what the problem
   usually means and how to fix it. Some errors also print `help:` lines
   immediately under the message.

When an error originates inside an intrinsic or a piece of code with no
usable source span, the pointer may be omitted and replaced by an `in
<name>` note instead.

## Warnings versus errors, and `--strict`

Type checking runs on **every** `eu` invocation, but its findings are
**warnings**: they print to stderr and do **not** stop evaluation or
change the exit code. Everything else here — parse failures, unresolved
variables, runtime faults — are **errors**: they stop evaluation and set a
non-zero exit code.

A type warning that does not correspond to a real runtime fault leaves the
program running normally:

```text
$ eu example.eu
warning: expression type does not match annotation
  ┌─ example.eu:1:1
  │
1 │ ╭ ` { type: "number" }
2 │ │ first-score: [10, 20, 30] nth(0)
  │ ╰────────────────────────────────^ expected number, found number?

---
first-score: 10
```

The warning went to stderr; the result (`first-score: 10`) still went to
stdout and the exit code was `0`.

Pass `--strict` to promote type warnings to errors and abort:

| Command | Behaviour | Exit |
|---|---|---|
| `eu file.eu` | evaluate; type issues are warnings only | `0` |
| `eu check file.eu` | type-check only, report warnings, do not evaluate | `0` |
| `eu check file.eu --strict` | type-check only; any warning is an error | `1` |
| `eu file.eu --strict` | abort before evaluation if there are type warnings | `1` |
| `eu file.eu --suppress-type-warnings` | evaluate, silencing warning output | `0` |

`--type-check` is a deprecated no-op kept for backwards compatibility —
the checker already runs whether or not it is passed. See
[Type Checking](../guide/type-checking.md) for the full guide to the
checker.

## Error categories

Errors surface at different stages of the pipeline (parse → desugar →
cook → verify → type-check → evaluate). The stage rarely matters to a
user, but the *category* of message does.

### Parse errors

Raised when the source text is not well-formed — an unclosed delimiter,
a stray token, a malformed declaration head. The pointer shows where the
parser gave up; for an unterminated bracket the span often spills onto the
following line, because that is where the missing close-delimiter was
expected:

```text
error: unterminated block (missing '}')
  ┌─ example.eu:1:11
  │
1 │ ╭ greeting: { name: "Alice"
  │ ╭───────────^
2 │ │
  │ ╰^
```

### Binding errors (unresolved and self-referential names)

Once the syntax is valid, eucalypt resolves every name to a binding. A
name with no binding in scope is the single most common error:

```text
error: unresolved variable 'subtotal'
  ┌─ example.eu:1:8
  │
1 │ total: subtotal + tax
  │        ^^^^^^^^
  │
  = check that the variable is defined and in scope
```

This is also what you get for an **operator** that is not defined:
operators are ordinary names in eucalypt, so an unknown operator such as
`<?>` reports as `unresolved variable '<?>'`.

A binding whose right-hand side refers to itself (rather than to an outer
value of the same name — a frequent surprise, since a block key shadows any
outer binding of that name) is caught before it can diverge:

```text
error: binding 'loop' refers to itself — this will always diverge
  ┌─ example.eu:1:7
  │
1 │ loop: loop
  │       ^^^^
  │
  = the binding `loop: loop` (or `loop: loop(…)`) always loops
  = if you want a lazy fixpoint, put the self-reference in argument position: e.g. `ones: cons(1, ones)`
```

Declarations use `:` , not `=`; the assignment form is caught with a
targeted hint:

```text
error: assignment-style syntax: 'count = ...' is not a valid eucalypt declaration
  ┌─ example.eu:1:7
  │
1 │ count = 3
  │       ^
  │
  = eucalypt uses ':' for declarations, not '='; write 'count: <value>' instead of 'count = <value>'
```

### Type warnings

The type checker reports mismatches as warnings. A mismatch at a call site
names the function and points at the offending argument:

```text
warning: type mismatch calling 'double'
  ┌─ example.eu:3:16
  │
3 │ result: double("hello")
  │                ^^^^^^^ expected number, found "hello"
```

Under `--strict` this same finding becomes an error and exits `1`. See
[Type Checking](../guide/type-checking.md) for what the checker does and
does not catch.

### Execution errors

Raised while the program runs — applying a non-function, indexing past the
end of a list, taking the `head` of an empty list, an explicit `panic`, a
failed `requires:` constraint, and so on. These carry help text and,
importantly, a **stack trace**:

```text
error: tried to call a number as a function
  help: only functions and blocks can be called with arguments
  help: this often means too many arguments were passed to a function
  ┌─ example.eu:1:11
  │
1 │ result: 3(4)
  │           ^
  │
  = a number value is not a function; check for a missing operator or extra argument
  = note: catenation (juxtaposition) has low precedence — 'f(a) + 1' binds as 'f(a + 1)'; add parentheses to disambiguate
```

The last note is a recurring hint: because catenation binds very loosely,
an unexpected "tried to call X as a function" often means an infix
operator captured an argument you meant to apply separately. See
[Syntax Gotchas](../appendices/syntax-gotchas.md) for the precedence
traps behind these messages.

## Reading stack traces and source locations

When an error is raised deep inside a chain of calls, the diagnostic's
primary pointer shows **where the fault occurred**, and the `stack trace`
note shows **how execution got there**. Consider:

```eu,notest
first-elem(xs): xs head
process(xs): first-elem(xs)
main: process([])
```

Running it:

```text
error: head of empty list
  ┌─ example.eu:1:20
  │
1 │ first-elem(xs): xs head
  │                    ^^^^
  │
  = guard against empty lists with 'nil?', e.g. 'if(xs nil?, default, xs head)'
  = 'head' and 'tail' are only defined on non-empty lists; use 'nth(0, xs)' or pattern matching if the list may be empty
  = stack trace:
    - first-elem at example.eu:1:20
```

The pointer and the trace both land on `xs head` inside `first-elem` — the
actual failure site — rather than on `main`, which merely started the
chain. Each trace entry is `function at file:line:column`. Note that the
trace reflects the *evaluated* call structure: because eucalypt is lazy
and inlines aggressively, tail calls and fully-inlined helpers may not each
appear as a separate frame, so a trace can be shorter than the source call
chain suggests.

### Getting more location detail

Source spans are threaded through the pipeline as *source-map ids*
(`Smid`s). Occasionally an error's own span is synthetic (it came from an
intrinsic), and the renderer falls back to the nearest enclosing span with
a real file location. If you are debugging a diagnostic that points at a
surprising place, set `EU_ERROR_TRACE_DUMP=1` to dump the full environment
and stack traces with their `Smid` details to stderr, which shows every
source location available at the point of failure.
