# Surge Agents Design

**Status:** Design approved
**Beads:** (no dedicated bead — these are cross-cutting process agents)

## Overview

Two autonomous agents that run as long sessions, each producing
multiple independent improvement proposals as GitHub PRs. They work
in isolated worktrees and never merge anything — the decision maker
(project owner) reviews and accepts or rejects each proposal.

- **Stopwatch** — performance surge. Profiles, hypothesises,
  benchmarks, and implements performance improvements.
- **Clarion** — error message surge. Provokes errors, evaluates
  messages for human and LLM diagnosability, and implements
  proof-of-concept improvements.

Both agents are defined in `.claude/agents/` and invoked as needed,
running alongside the main 0.4.0 development effort.

---

## Section 1: Stopwatch — Performance Surge Agent

**Goal:** Improve eucalypt performance as measured by speed of harness
tests, bench tests, and AoC examples. No ad hoc changes like moving
eucalypt logic into Rust intrinsics.

### Workflow cycle

1. **Select target** — pick a harness test, bench test, or AoC
   example to profile
2. **Baseline** — measure current performance using `eu -S` (built-in
   stats), `cargo bench`, or `hyperfine` as appropriate
3. **Hypothesise** — identify a bottleneck and form a theory about
   what improvement is possible
4. **Implement** — make the change on a fresh branch from master
   named `perf/stopwatch-<description>`
5. **Validate** — re-measure the target, then run the full harness
   suite and representative AoC examples with timing to catch
   regressions. The proposal must include any significant impact on
   other tests/examples, not just the target.
6. **Propose** — push the branch, create a GitHub PR with the
   standardised summary, and move to the next hypothesis

### Constraints

- No moving eucalypt logic into Rust intrinsics
- All changes must pass `cargo clippy --all-targets -- -D warnings`
  and `cargo test`
- Each proposal is a separate branch from master
- Target a mix of categories: compiler transforms, VM execution, GC
  tuning, STG compilation, intrinsic implementation

### PR format

```
## Performance: <short description>

### Hypothesis
<what bottleneck was identified and why this change should help>

### Change
<brief description of what was modified>

### Results
| Benchmark | Before | After | Change |
|-----------|--------|-------|--------|
| <target test> | Xms | Yms | -Z% |

### Regression check
| Test/Example | Impact |
|-------------|--------|
| <any affected> | <measurement> |

(or: "Full harness suite: no significant regressions detected")

### Risks
<anything that might bite later>
```

---

## Section 2: Clarion — Error Message Surge Agent

**Goal:** Improve the quality of eucalypt's error messages so that
both humans and LLMs can diagnose and fix errors from the output
alone.

### Workflow cycle

1. **Provoke errors** — take working eucalypt code (AoC solutions,
   harness tests, library code) and introduce realistic perturbations
2. **Capture output** — run the perturbed code, capture stderr and
   exit code
3. **Evaluate** — assess the error message:
   - Does it identify *what* went wrong?
   - Does it point to *where* in the source?
   - Does it suggest *how to fix it*?
   - Would an LLM given this error message be able to diagnose and
     fix the problem?
4. **Implement PoC** — modify Rust error handling code to improve the
   message
5. **Validate** — re-run the perturbation, confirm improved output,
   run existing error tests (`cargo test test_error`) to ensure no
   regressions against `.expect` sidecars
6. **Propose** — push branch named `errors/clarion-<description>`,
   create a GitHub PR with the standardised summary

### Perturbation strategies

- Delete a declaration that's referenced elsewhere
- Change a function's arity (add/remove arguments)
- Use wrong operator (e.g. `/` where `÷` is needed)
- Introduce syntax errors (unmatched brackets, missing colons)
- Reference non-existent imports
- Create type mismatches (pass number where string expected)
- Misuse anaphora (double `_`, wrong scope)
- Break block structure (missing keys, duplicate keys)

### Constraints

- Must pass existing error expectation tests
  (`harness/test/errors/*.expect`)
- Each proposal is a separate branch from master
- Changes to error formatting must not break structured error output
- Cover a range of error categories: parse errors, type mismatches,
  unresolved bindings, runtime panics, lookup failures

### PR format

```
## Error message: <error category / scenario>

### Scenario
<what perturbation was applied to what code>

### Before
<stderr output before the change>

### After
<stderr output after the change>

### Assessment
- Human diagnosability: <poor/fair/good> → <fair/good/excellent>
- LLM diagnosability: <poor/fair/good> → <fair/good/excellent>

### Change
<what was modified in the error handling code>

### Risks
<any concerns>
```

---

## Section 3: Agent Configuration

Both agents are defined in `.claude/agents/` as markdown files with
YAML frontmatter:

- `isolation: worktree` — each run gets an isolated repo copy
- `model: sonnet` — good balance of capability and cost
- `permissionMode: acceptEdits` — auto-approve file edits for
  autonomous work
- No `maxTurns` limit initially — adjust after observing session
  behaviour

### Branch naming

- Stopwatch: `perf/stopwatch-<short-description>`
- Clarion: `errors/clarion-<short-description>`

### Measurement tools

- `eu -S` / `eu --statistics` — built-in execution metrics
- `cargo bench` — Rust micro-benchmarks
- `hyperfine` — wall-clock timing of eu commands
- The agent picks whichever is appropriate for each hypothesis

---

## Section 4: Integration with 0.4.0

Both agents run independently of the main 0.4.0 development effort.
They work from master (or a stable base branch) and produce PRs that
can be merged at any time without blocking or being blocked by feature
work.

When we design the 0.4.0 agent team, Stopwatch and Clarion will be
included as dedicated team members with their existing roles and
branch conventions.
