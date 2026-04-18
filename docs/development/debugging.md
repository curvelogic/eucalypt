# Debugging Tools

This page documents the debugging and diagnostic tools available for
eucalypt developers. These are primarily useful when investigating GC
bugs, runtime errors, compiler issues, or performance problems.

## Environment Variables

### GC Debugging

#### `EU_GC_POISON=1`

Fill swept memory with a `0xDE` poison pattern after each garbage
collection cycle. Detects use-after-free bugs by checking for the
poison pattern in `mark()` — if a live GC root references memory
that has been poisoned, the collector panics immediately with the
object address and type name.

Also enables **hole verification** in the bump allocator: when a
recycled block's hole is about to be reused, the allocator checks
that it does not contain anything that looks like a valid marked
object, catching line-marking bugs where live data is classified
as a free hole.

Use this when investigating segfaults, heap corruption, or
non-deterministic runtime failures.

#### `EU_GC_VERIFY=1`

After each GC mark phase, re-traverse from roots and verify that
every reachable object was marked. Panics if any unmarked-but-reachable
objects are found. This catches bugs where the mark phase misses
objects due to incorrect scanning, missing root registration, or
pointer update errors.

Moderate performance cost — roughly doubles collection time.

#### `EU_GC_VERIFY=2`

Full multi-checkpoint structural verification of the heap during
garbage collection. Level 2 implies level 1 (mark completeness is
checked). Additionally verifies:

**Post-mark (Checkpoint 1):**
- Every marked object has a valid header (tag 0-15, plausible
  allocation length)
- Every marked object's full allocation (header + payload) has all
  covering Immix lines marked in the block's LineMap
- This catches the class of bugs where objects straddling line
  boundaries have only partial line marking

**Post-evacuation (Checkpoint 2, evacuation collections only):**
- Evacuation target blocks have their lines properly marked
- Prevents lazy sweep from recycling blocks containing evacuated
  copies of live objects

**Post-update (Checkpoint 3, evacuation collections only):**
- Evacuation target blocks are properly integrated into the heap
  after `finalise_evacuation()`

**Post-sweep (Checkpoint 4):**
- Block list disjointness — no block appears in multiple HeapState
  lists (head, overflow, rest, unswept, recycled)
- Recycled blocks have no marked lines (fully clear)
- Blocks in the rest list have at least one marked line (contain
  live data)

Significant performance cost — builds verification indexes at each
checkpoint. Intended for test suite runs, not interactive use:

```sh
EU_GC_VERIFY=2 cargo test --test harness_test
```

#### `EU_GC_STRESS=1`

Force selective evacuation on every GC collection, regardless of
heap pressure. This exercises the evacuation and pointer-update paths
on every cycle, catching bugs that only manifest when objects are
relocated. Without this, evacuation only triggers under memory
pressure, making evacuation bugs hard to reproduce.

Combine with `EU_GC_VERIFY=2` for maximum coverage:

```sh
EU_GC_STRESS=1 EU_GC_VERIFY=2 cargo test
```

### Runtime Diagnostics

#### `EU_STACK_DIAG=1`

Log continuation stack composition to stderr whenever a new maximum
stack depth is reached. Useful for investigating stack overflow or
understanding evaluation depth for specific programs.

Output format shows the count of each continuation type:

```
STACK MAX 1024: Branch=512 Update=256 ApplyTo=128 DeMeta=64 ...
```

#### `EU_ERROR_TRACE_DUMP=1`

On every execution error, dump the full environment trace and stack
trace Smid details as diagnostic notes. Useful for debugging which
source locations are available at the point an error is raised —
helps investigate missing or incorrect source location propagation
in error messages.

#### `EU_IO_TRACE=1`

Trace all `io.shell` and `io.exec` commands to stderr before
execution. Shows the command string and whether stdin is piped. Also
traces exit codes on completion and errors on failure.

```
IO TRACE: shell "echo hello"
IO TRACE: -> exit 0
IO TRACE: exec "ls" ["-la"]
IO TRACE: -> exit 0
```

### Crash Diagnostics

The crash signal handler installs unconditionally in `main()` (no
environment variable needed). On SIGSEGV or SIGBUS, it prints a
diagnostic to stderr showing the faulting address and a stack
backtrace before aborting. This is gated with `cfg(unix)` — not
available on Windows.

## Dump Commands

Use `eu dump <phase>` to inspect intermediate representations at
each stage of the compilation pipeline. These are the **primary
tool** for investigating compiler and core expression issues.

| Command | Shows |
|---------|-------|
| `eu dump ast <file>` | Parsed syntax tree |
| `eu dump desugared <file>` | Core expression after desugaring |
| `eu dump cooked <file>` | After operator precedence resolution |
| `eu dump inlined <file>` | After inlining |
| `eu dump pruned <file>` | After dead code elimination |
| `eu dump stg <file>` | Compiled STG syntax |
| `eu dump runtime <file>` | Runtime globals |

### Dump Flags

- `--debug-format` — Rust Debug representation showing full structure
  including de Bruijn indices
- `--embed` — eucalypt source representation
- `-B` — batch mode, skip `.eucalypt.d` config files (avoids loading
  local configuration that may alter the output)

### Examples

```sh
# See how an expression desugars
eu dump desugared -B myfile.eu --embed

# Inspect STG output for a specific file
eu dump stg myfile.eu --debug-format

# Check what the parser produces
eu dump ast myfile.eu
```

## Recommended Debugging Workflows

### Investigating a Segfault

1. Run with poison to detect use-after-free:
   ```sh
   EU_GC_POISON=1 eu myfile.eu
   ```
2. If that doesn't catch it, add full verification:
   ```sh
   EU_GC_VERIFY=2 EU_GC_POISON=1 eu myfile.eu
   ```
3. If it only reproduces intermittently, force evacuation:
   ```sh
   EU_GC_STRESS=1 EU_GC_VERIFY=2 EU_GC_POISON=1 eu myfile.eu
   ```

### Investigating a Wrong Result

1. Use `eu dump` to inspect the pipeline:
   ```sh
   eu dump desugared -B myfile.eu --embed | less
   eu dump stg -B myfile.eu --debug-format | less
   ```
2. Compare desugared output with expected core expression

### Investigating an Error Message

1. Enable error trace dump:
   ```sh
   EU_ERROR_TRACE_DUMP=1 eu myfile.eu
   ```
2. Check which Smids have source locations vs synthetic locations
