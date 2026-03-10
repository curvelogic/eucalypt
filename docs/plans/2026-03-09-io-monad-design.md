# IO Monad Design for Eucalypt

**Date:** 2026-03-09
**Status:** Approved
**Blocks:** eu-08m8 (Eucalypt 0.5.0)
**Bead:** eu-yost

## Overview

Introduce controlled shell invocation to eucalypt via an IO monad. IO
actions are opaque data constructors interpreted by a new `io-run`
driver loop. Monadic sequencing uses the existing `:monad` block
desugaring. A `render` / `render-as` pure function is also added to
the prelude for value-to-string serialisation.

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Primitive operation | Shell/exec invocation | Everything else can be built on top |
| IO action representation | Data constructors + tagged spec block | Extensible without new constructors per action type |
| Sequencing | Existing `:monad` desugaring | No new syntax needed |
| World token | Threaded through bind/return, inert | Upgrade path to closure-based IO later |
| Error handling | IoFail constructor, chain aborts | Composable; `io.try` catchable later |
| Command results | Always structured `{stdout, stderr, exit-code}` | Caller decides what to do |
| Timeouts | Default 30s, per-call override | Prevents hangs |
| Security | `--allow-io` / `-I` flag required | Prevents accidental execution of untrusted files |
| Final value | Rendered in requested format | Consistent with normal `eu` behaviour |
| `render`/`render-as` | Pure prelude functions, not IO | Serialisation is not a side effect |

## Data Constructors

Four new tags added to `DataConstructor` enum in `src/eval/stg/tags.rs`:

```
Tag 12: IoReturn  (arity 2) — world, value
Tag 13: IoBind    (arity 3) — world, action, continuation
Tag 14: IoAction  (arity 2) — world, spec_block
Tag 15: IoFail    (arity 2) — world, error
```

## Action Spec Tags

Action specs are ordinary eucalypt blocks with metadata tags:

- `:io-shell` — `{cmd: Str, timeout: Num}`, optional `stdin: Str`
- `:io-exec` — `{cmd: Str, args: [Str], timeout: Num}`, optional `stdin: Str`

New action types added later just need a new tag and a handler in the
driver — no new constructors or intrinsics.

## Intrinsics

Three new intrinsics:

| Name | Signature | Purpose |
|------|-----------|---------|
| `IO_RETURN` | `(world, value) → IoReturn` | Wrap pure value |
| `IO_BIND` | `(world, action, continuation) → IoBind` | Sequence actions |
| `IO_ACTION` | `(world, spec_block) → IoAction` | Construct action from spec |

Plus one pure intrinsic:

| Name | Signature | Purpose |
|------|-----------|---------|
| `RENDER_TO_STRING` | `(value, format_sym) → Str` | Serialise value to string |

## Eucalypt API

```text
# Pure prelude functions
render(value): __render_to_string(value, :yaml)
render-as(value, fmt): __render_to_string(value, fmt)

# IO namespace
io: {
  :monad
  bind(a, f): __io_bind(a, f)
  return(a): __io_return(a)

  shell(cmd): __io_action({:io-shell cmd: cmd, timeout: 30})
  # shell-with and exec-with use helper functions to build the spec block.
  # Using << on a Meta-tagged block ({:io-shell ...}) strips the metadata tag,
  # so the spec is built explicitly via lookup-or to extract known opt fields.
  shell-with(opts, cmd): __io_action(__io-shell-spec(opts, cmd))
  exec([cmd : args]): __io_action({:io-exec cmd: cmd, args: args, timeout: 30})
  exec-with(opts, [cmd : args]): __io_action(__io-exec-spec(opts, cmd, args))

  check(result): ...  # IoFail if exit-code != 0, else IoReturn(result)

  # map(f, IO a) -> IO b
  # Derived: fmap in terms of bind and return
  map(f, action): io.bind(action, {x: io.return(f(•))}.x)
}
```

### Result Shape

```text
{stdout: Str, stderr: Str, exit-code: Num}
```

### Usage Examples

```text
# Simple command
files: { :io result: io.shell("ls -la") }.result.stdout

# Chained with failure check
rev: { :io
  r: io.exec(["git", "rev-parse", "HEAD"])
  _: io.check(r)
}.r.stdout

# Piping stdout of one command as stdin to another
sorted: { :io
  listing: io.shell("ls")
  result: io.shell-with({stdin: listing.stdout}, "sort")
}.result.stdout

# Render data and post
posted: { :io
  json: io.return(render-as(my-data, :json))
  r: io.shell-with({stdin: json}, "curl -X POST http://example.com")
  _: io.check(r)
}.r.stdout
```

## Architecture

### Execution Flow

```
eu -I --target :io foo.eu
        │
        ▼
┌──────────────────────┐
│  Parse + Compile     │  (existing pipeline)
│  Target expr → STG   │
└──────────┬───────────┘
           ▼
┌──────────────────────┐
│  Evaluate target     │  STG machine, lazy
│  Returns IO chain    │  (IoReturn | IoBind | IoAction | IoFail)
└──────────┬───────────┘
           ▼
┌──────────────────────────────────────────┐
│  io-run interpret loop                   │
│                                          │
│  match value:                            │
│    IoReturn(_, v) → done, render v       │
│    IoFail(_, e)   → report error, exit   │
│    IoBind(_, action, cont) →             │
│      result = io_run_single(action)      │
│      value = evaluate(cont(result))      │
│      loop                                │
│    IoAction(_, spec) →                   │
│      match spec tag:                     │
│        :io-shell → spawn shell           │
│        :io-exec  → spawn command         │
│        unknown   → IoFail                │
│      return IoReturn(world, result)      │
└──────────────────────────────────────────┘
           │
           ▼
┌──────────────────────┐
│  Render final value  │  (existing render pipeline)
│  → stdout            │
└──────────────────────┘
```

### VM Re-entry

The `io-run` loop must repeatedly:

1. **Evaluate an expression to WHNF** — run the STG machine until it
   produces a data constructor
2. **Inspect the result** — read the data constructor tag and extract
   fields from the heap
3. **Construct a new expression** — apply the bind continuation to the
   command result
4. **Re-enter the machine** — evaluate the new expression

This requires the machine to support partial evaluation — run to WHNF,
yield control, then resume with a new expression. The current
`MachineState::run()` runs to termination (the RENDER intrinsic drives
output). For `io-run`:

- **Option A**: Modify the machine to support "evaluate to WHNF" mode.
  Instead of rendering, the machine stops when it has a data
  constructor at the top level. The io-run loop then inspects and
  re-enters.
- **Option B**: Use a trampoline — compile each step as a fresh
  expression, create a new machine run for each step.

**Recommendation: Option A** — more efficient, avoids recompilation.
The machine already has `terminated` state; we add an "evaluated to
WHNF" yield point. The io-run driver checks the result, constructs the
next closure, sets it as the machine's current closure, and calls
`run()` again.

Concretely, when the machine evaluates a `Cons` with an IO tag
(12-15) and the continuation stack is empty (or contains only Update
continuations), it yields rather than trying to apply continuations.
The io-run driver inspects `machine.closure` to read the constructor
tag and fields.

### Shell Execution

Implemented in Rust using `std::process::Command`:

```rust
fn execute_shell(cmd: &str, stdin: Option<&str>, timeout_secs: u64)
    -> Result<IoResult, IoError>
{
    let mut child = Command::new("sh")
        .args(["-c", cmd])
        .stdin(if stdin.is_some() { Stdio::piped() } else { Stdio::null() })
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    if let Some(input) = stdin {
        child.stdin.take().unwrap().write_all(input.as_bytes())?;
    }

    match child.wait_timeout(Duration::from_secs(timeout_secs))? {
        Some(status) => Ok(IoResult { stdout, stderr, exit_code: status.code() }),
        None => { child.kill()?; Err(IoError::Timeout) }
    }
}
```

The result is injected back into the STG machine as a block:
`{stdout: Str, stderr: Str, exit-code: Num}`.

### String-Backed Render (RENDER_TO_STRING intrinsic)

Uses the existing `Emitter` trait with a new `StringEmitter` that
captures output to a `String` instead of writing to stdout. The
intrinsic:

1. Creates a `StringEmitter` for the requested format
2. Re-enters the machine's render pathway with this emitter
3. Returns the captured string as a Native::Str

This is the trickiest pure intrinsic because it needs to render a
sub-value during evaluation. Implementation options:

- **Recursive machine run**: snapshot machine state, render the value
  into a string, restore state. Heavy but clean.
- **Emitter swap**: temporarily swap the machine's emitter to a string
  buffer, render, swap back. Lighter but requires emitter to be
  accessible from intrinsic context.

**Recommendation**: Recursive machine run — cleaner separation, and
render-to-string won't be called in hot loops.

## CLI Changes

Add to `EucalyptCli` in `src/driver/options.rs`:

```rust
#[arg(short = 'I', long = "allow-io")]
pub allow_io: bool,
```

The `io-run` driver checks this flag before executing any IoAction. If
absent, fails with: `IO operations require the --allow-io (-I) flag`.

## Implementation Plan

### Task Breakdown

Tasks are ordered by dependency. Furnace (backend) and Quill
(frontend) can work in parallel on independent tasks.

#### Phase 1: Foundation (Furnace)

**F1. IO data constructor tags**
- File: `src/eval/stg/tags.rs`
- Add `IoReturn=12`, `IoBind=13`, `IoAction=14`, `IoFail=15`
- Update `arity()`, `Display`, `TryFrom<Tag>`
- Update `src/eval/memory/syntax.rs` GcScannable for new tags
- Risk: Low. Mechanical additions.

**F2. IO intrinsics (IO_RETURN, IO_BIND, IO_ACTION)**
- Files: `src/eval/intrinsics.rs`, new `src/eval/stg/io.rs`
- Register three new intrinsics in the INTRINSICS vector
- Implement `StgIntrinsic` trait for each
- IO_RETURN: construct `Cons(IoReturn, [world, value])`
- IO_BIND: construct `Cons(IoBind, [world, action, cont])`
- IO_ACTION: construct `Cons(IoAction, [world, spec])`
- Add wrappers to `make_standard_runtime()`
- Risk: Low-medium. Follows existing intrinsic patterns.
- Depends on: F1

**F3. CLI --allow-io / -I flag**
- File: `src/driver/options.rs`
- Add flag to `EucalyptCli` struct
- Thread through to executor
- Risk: Low.

#### Phase 2: VM Changes (Furnace)

**F4. Machine WHNF yield for IO constructors**
- File: `src/eval/machine/vm.rs`
- Modify `return_data()` or the run loop: when an IO constructor tag
  (12-15) is returned and the continuation stack is empty (no pending
  Branch/ApplyTo), the machine yields instead of terminating
- Add machine state to distinguish "yielded IO value" from "terminated"
- Add method to extract the yielded constructor tag and field values
- Add method to set a new closure and resume execution
- Risk: **High**. Core VM change. Must not break existing evaluation.
  Careful: the "stack is empty" check must account for Update
  continuations (which should be transparent). Branch and ApplyTo
  continuations mean we're inside a case/application and shouldn't
  yield.
- Depends on: F1

**F5. io-run driver loop**
- Files: new `src/driver/io_run.rs`, modify `src/driver/eval.rs`
- Implement the interpret loop (evaluate → inspect → execute → re-enter)
- Shell execution via `std::process::Command` with timeout
- Result injection: construct STG block from command output, apply
  continuation, set as new machine closure
- Hook into `Executor::try_execute()` — after evaluating the target,
  check if result is an IO constructor; if so, enter io-run loop
- Check `--allow-io` flag before executing any IoAction
- IoFail handling: extract error, format message, exit with non-zero
- Final IoReturn value: render using existing render pipeline
- Risk: **High**. The result injection (constructing STG values from
  Rust and feeding them back into the machine) is the most complex
  part. Must correctly allocate on the heap, construct environment
  frames, and apply continuations.
- Depends on: F2, F3, F4

#### Phase 3: Render-to-String (Furnace)

**F6. RENDER_TO_STRING intrinsic**
- Files: `src/eval/intrinsics.rs`, new intrinsic module,
  `src/eval/emit.rs` (StringEmitter)
- New `StringEmitter` implementing `Emitter` trait, captures to String
- Intrinsic: snapshot machine state, run render with StringEmitter,
  return captured string
- Register in INTRINSICS vector and runtime
- Risk: **Medium-High**. Needs careful machine state management.
  Must not corrupt the ongoing evaluation. May need to clone or
  fork machine state.
- Depends on: F4 (needs machine re-entry capability)

#### Phase 4: Prelude (Quill)

**Q1. IO namespace in prelude**
- File: `lib/prelude.eu`
- Add `io` block with `:monad` metadata, `bind`, `return`
- Add `shell`, `shell-with`, `exec`, `exec-with`
- Add `check` (pure eucalypt logic over result block)
- Depends on: F2 (intrinsics must exist)

**Q2. render / render-as prelude functions**
- File: `lib/prelude.eu`
- Add top-level `render(value)` and `render-as(value, fmt)`
- Depends on: F6 (RENDER_TO_STRING intrinsic must exist)

#### Phase 5: Testing (Joint)

**Q3. Harness tests for IO monad**
- Directory: `tests/harness/`
- Test io.return, io.bind, io.shell, io.exec, monadic block syntax
- Test error cases: missing --allow-io flag, command timeout,
  non-zero exit with io.check, unknown action tag
- Test render / render-as for JSON, YAML, TOML
- Register tests in `tests/harness_test.rs`
- Depends on: F5, Q1, Q2

**F7. Integration testing of io-run loop**
- Test VM re-entry, multi-step bind chains, nested IO actions
- Test IoFail propagation through bind chains
- Test timeout enforcement
- Stress test: long chains, large stdout capture
- Depends on: F5, Q1

### Dependency Graph

```
F1 (tags) ──┬──→ F2 (intrinsics) ──→ F5 (io-run) ──→ Q3 (tests)
             │                    ↗        ↑              ↑
             ├──→ F4 (VM yield) ─┘         │              │
             │         │                   │              │
             │         └──→ F6 (render) ──→ Q2 (render)──┘
             │                                            │
F3 (CLI) ────────────────────────→ F5                     │
                                                          │
                            F2 ──→ Q1 (prelude) ─────────┘
```

### Agent Allocation

| Task | Agent | Priority | Estimated Complexity |
|------|-------|----------|---------------------|
| F1 | Furnace | Must-do first | Low |
| F2 | Furnace | Must-do early | Low-Medium |
| F3 | Furnace | Can parallel with F1/F2 | Low |
| F4 | Furnace | Critical path | **High** |
| F5 | Furnace | Critical path | **High** |
| F6 | Furnace | Can follow F4 | Medium-High |
| Q1 | Quill | After F2 lands | Low |
| Q2 | Quill | After F6 lands | Low |
| Q3 | Joint | After Q1+Q2+F5 | Medium |
| F7 | Furnace | After F5+Q1 | Medium |

### Critical Path

F1 → F2 → F4 → F5 → Q3

Furnace owns the critical path. Quill can prepare Q1 in parallel once
F2 lands, but testing is blocked on the full stack.

### Risk Areas

1. **VM re-entry (F4/F5)**: The machine wasn't designed for
   suspend/resume. Ensuring Update continuations are handled correctly
   during yield, and that heap state is consistent when re-entering,
   requires careful testing.

2. **Result injection (F5)**: Constructing STG values (the result
   block) from Rust and injecting into the machine's heap. Must
   allocate correctly, create proper environment frames, and ensure GC
   can trace the injected values.

3. **Render-to-string (F6)**: Running a nested evaluation within an
   ongoing evaluation. Machine state must be preserved or cloned.
   Alternative: defer F6 and have io.render use io.shell("eu ...") as
   a bootstrap — ugly but unblocks everything else.

4. **Monad desugaring compatibility (Q1)**: The existing `:monad`
   metadata must work with the `io` namespace. Need to verify that
   `{:io x: io.shell("ls")}` correctly desugars to
   `io.bind(io.shell("ls"), x -> ...)`.

### Fallback for F6

If render-to-string proves too complex for 0.5.0, a pragmatic
fallback: `io.render-as(value, :json)` could be implemented as
`io.exec("eu", ["--stdin", "-f", "json"], {stdin: ...})` — i.e.
shell out to eu itself. This is obviously slower but unblocks the
feature. The proper intrinsic can follow in 0.5.1.
