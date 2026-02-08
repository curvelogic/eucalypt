# Eucalypt v0.3.0 Agent Team Implementation Plan

## Context

Eucalypt v0.3.0 has 16 design plans covering VM improvements, prelude
extensions, tooling enhancements, and an LSP server. All plans have been
reviewed and cross-checked. The work is tracked in beads with dependency
graphs. We need a team of autonomous agents to implement this in parallel,
with a review/architect agent ensuring quality and coherence.

## Team Structure

| Name | Role | Worktree | Scope |
|------|------|----------|-------|
| **Niamh** | VM, GC, memory, STG, DCE | `eucalypt-vm` | eu-4af, eu-brj, eu-xg2x, eu-8d8, eu-181d, eu-40jb |
| **Callum** | Prelude, intrinsics, features, ZDT literal | `eucalypt-prelude` | Quick wins, eu-da3, eu-ert, eu-kfe, eu-kd1, eu-z0l |
| **Fricka** | Tester, benchmarks, docs gen | `eucalypt-tooling` | eu-twg, eu-01v, eu-ber |
| **Ravi** | LSP server + editors | `eucalypt-lsp` | eu-307 (all phases) |
| **Seren** | Architect, PR review, docs | main repo | PR gate, cross-stream, docs freshness, eu-ptu |

**Orchestrator** (me): delegate, unblock, manage beads, handle escalations.

## Git Strategy

### Worktree Setup

Each coding agent gets its own git worktree branched from master. This
avoids checkout conflicts and gives each agent its own `target/` directory
for independent builds.

```bash
# Create worktree directory
mkdir -p /Users/greg/dev/curvelogic/eucalypt-worktrees

# Create one worktree per coding agent, each starting from master
git worktree add /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-vm master
git worktree add /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-prelude master
git worktree add /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-tooling master
git worktree add /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-lsp master
```

The reviewer works from the main repo directory.

### Branch-per-Task Flow

Each bead task gets its own branch:

```
feature/eu-XXXX-short-description
```

Agent workflow per task:
1. In their worktree: `git fetch origin && git checkout -b feature/eu-XXXX-desc origin/master`
2. Implement, test, commit (multiple commits fine)
3. Push: `git push -u origin feature/eu-XXXX-desc`
4. Create PR: `gh pr create --title "..." --body "..."`
5. Notify reviewer via SendMessage
6. Wait for merge (reviewer handles this)
7. After merge: `git fetch origin && git checkout -b feature/eu-YYYY-next origin/master`

### Merge Sequencing

- Seren **squash-merges** each PR (one clean commit per bead task)
- Squash commit message format: `feat(eu-XXXX): Short description of change`
  with a body summarising what was done and why
- After each merge, Seren runs full test suite on master
- If merge conflicts arise, Seren resolves or asks the authoring agent
- Agents always branch from latest `origin/master` for each new task

## Agent Operating Guidelines

### Shared Rules (All Agents)

```
WORKTREE: You work EXCLUSIVELY in your assigned worktree directory.
All file paths and all cargo/git commands use that directory.

QUALITY GATE: Before marking any task done, you MUST pass:
  1. cargo fmt --all
  2. cargo clippy --all-targets -- -D warnings
  3. cargo test
If any step fails, fix and retry (up to 5 attempts).
After 5 failures on the same issue, escalate to reviewer.

BRANCHES: One branch per bead task. Branch from latest origin/master.
Never commit directly to master.

COMMITS: Clear, descriptive messages. Include bead ID. Example:
  "feat(eu-4af): Add SymbolId and SymbolPool data structures"

UK ENGLISH: All comments, docs, error messages use UK English.

PUBLIC-READY: All code will be merged to the public eucalypt repo after
0.3.0. Write everything as if it's public from day one:
  - No internal jargon, no references to private repos or tools
  - Clean, helpful error messages that make sense to external users
  - Comments explain "why", not "what" — assume a competent Rust reader
  - Doc metadata on prelude functions (` "description") is user-facing

CLIPPY: Fix ALL warnings. Never use #[allow()] to suppress.

DECISION AUTHORITY: You CAN decide without escalating:
  - Implementation details not covered by the spec (choose sensibly,
    document in PR description)
  - Internal API additions (helper functions, internal modules)
  - Performance trade-offs within the spec's stated approach
  - Test strategy and coverage details beyond what the spec lists
  - Error message wording (following UK English, clear and helpful)
  - Minor refactoring needed to land your feature cleanly

ESCALATION: Send a message to Seren ONLY when:
  - You've failed 5 fix attempts on the same issue
  - You discover a cross-stream conflict affecting another agent
  - You need to change a user-visible API not covered by the spec
  - The spec is fundamentally unclear about the desired behaviour
    (not just missing detail — missing intent)

DESIGN PLANS: Read docs/plans/*.md for your assigned features BEFORE
starting implementation. These are the source of truth.

OPPORTUNISTIC P3 FIXES: When you are already modifying a file, fix
nearby issues from these beads if the fix is trivial and local:
  - eu-0yz: Replace unwrap() with expect("descriptive message")
  - eu-2254: (Niamh only) Lazy iteration for stack_trace if touching vm.rs
  - eu-mrg: Remove or resolve TODO/FIXME/HACK comments you encounter
Do NOT go hunting for these. Only fix them if they're in your path.

NO BEADS MANAGEMENT: The orchestrator manages beads. You report task
completion via SendMessage to the orchestrator. Do not run bd commands.

BEAD LIFECYCLE (orchestrator handles all of this):
  - in_progress: when orchestrator assigns/signals an agent to start
  - close (sub-bead): after Seren confirms squash-merge to master
    and master CI passes
  - close (parent epic): when all child sub-beads are closed
  - bd sync: orchestrator runs periodically and at session end
```

### Niamh (VM Specialist)

```
WORKTREE: /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-vm

TASK ORDER (strictly serial — each depends on the previous):
1. eu-4af: Symbol interning (eu-e1p3 → eu-jo9y → eu-pq9i → eu-uaoj → eu-yvpz → eu-eysh → eu-r9bv)
2. eu-brj: Block indexing (eu-z5br → eu-wnkl → eu-pb3e → eu-piho)
3. eu-xg2x: STG optimisations (eu-knx, eu-otxv, eu-354y, eu-drh0 — any order within)
4. eu-8d8: DCE inside blocks (eu-qmw3 → eu-eezd → eu-j642)
5. eu-40jb: String intrinsics phase 1 (eu-cb9k → eu-nq77)
6. eu-181d: GC Immix — all 3 phases, hyper-incrementally:
   Phase 1: Benchmarking infrastructure (statistics, --statistics-file,
            Criterion benchmarks, gc-bench.sh, workflow docs)
   Phase 2: Lazy sweeping — implement behind conditional, validate with
            benchmarks, then switch on in a separate small commit
   Phase 3: Evacuation — land in stages:
     a. Forwarding pointer support in AllocHeader (inert, tested in isolation)
     b. Evacuation candidate selection logic (inert, unit tested)
     c. Evacuation during marking (disabled by default, tested via unit tests)
     d. Reference updating in GcScannable (disabled, tested per-type)
     e. Enable opportunistic evacuation (small commit, full validation)
   Each sub-step is a separate branch/PR, validated independently.
   Code can land before it's active. The switch-on is always a separate,
   small, reviewable commit.

BENCHMARKING DISCIPLINE:
- After eu-4af: benchmark symbol-heavy programs before/after
- After eu-brj: benchmark block lookup before/after
- After each STG optimisation: check ticks/allocs in -S output
- Before/after every GC phase: full gc-bench.sh comparison
- Record all results in PR descriptions

SPECIAL RULES:
- Memory code is safety-critical. Document all unsafe blocks.
- When changing GcScannable, verify ALL implementors are updated.
- Symbol interning changes Native::Sym everywhere — grep exhaustively.
- After completing eu-4af, notify orchestrator so eu-kfe can start.
- GC changes: never enable new behaviour in the same PR that adds
  the code. Implement → validate → enable are separate PRs.
```

### Callum (Prelude & Features)

```
WORKTREE: /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-prelude

TASK ORDER:
Phase 1 (immediate, no dependencies):
  - eu-u1m: Polymorphic lt/gt for ZDTs + strings + symbols
  - eu-nbc: Version assertions (eu.requires)
  - eu-dyx: Base64 encode/decode
  - eu-dd5: SHA-256 hash

Phase 2 (after eu-u1m):
  - eu-da3: String comparison intrinsics (str.lt etc.) + sort-keys
  - eu-dlr: Catch {name: name} recursion (depends on eu-twg for error tests)

Phase 3 (after eu-da3 + eu-u1m):
  - eu-ert: Sorting lists (sort-nums, sort-strs, sort-by etc.)

Phase 4 (after eu-4af merges to master):
  - eu-kfe: Set implementation (Primitive::Sym uses SymbolId)

Phase 5 (independent, can start any time):
  - eu-kd1: ZDT literal syntax (eu-yfcw → eu-7y85 → eu-qarv, then tests)

Phase 6 (independent, lower priority):
  - eu-z0l: Deep find functions

NAMING CONVENTION:
- String comparison goes INSIDE str block: str.lt, str.gt, str.lte, str.gte
- New intrinsics follow existing pattern in src/eval/stg/
- Prelude functions have doc metadata: ` "description"

INTRINSIC PATTERN:
Read an existing intrinsic (e.g., src/eval/stg/string.rs) and follow
the exact pattern: struct, name(), execute(), CallGlobal trait, register
in src/eval/stg/mod.rs.

HARNESS TESTS:
Every new feature gets harness tests in harness/test/. Follow the
existing numbered naming: find the highest number, increment by 1.
```

### Fricka (Tooling & Infrastructure)

```
WORKTREE: /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-tooling

TASK ORDER:
Phase 1 (independent, start immediately):
  - eu-twg: Error message tests (eu-aiux → eu-icdc → eu-i1pi)
  - eu-01v: Benchmarks in tester (eu-s5n5 → eu-asdm → eu-2c1x → eu-1yxc → eu-f2pk)

Phase 2 (after prelude stabilises):
  - eu-ber: Prelude docs generator (eu-980b → eu-j2pu → eu-9ez6)

TESTER CHANGES:
The tester code is in src/driver/tester.rs and lib/test.eu.
Test plan analysis is in testplan.rs (or similar).
Read the existing code thoroughly before modifying.

ERROR TESTS:
- .expect sidecars go alongside .eu files in harness/test/errors/
- Format: YAML with exit: and stderr: fields
- Start with 3 smoke tests, then migrate the rest

BENCHMARKS:
- bench- prefixed targets in test files
- Report ticks, allocs, max-stack to stderr
- Stats already captured in evidence — surface them
```

### Ravi (LSP Server)

```
WORKTREE: /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-lsp

TASK ORDER (phase 1 only for initial sprint):
  1. eu-307.1: LSP scaffold (eu lsp subcommand, lsp-server, stdio loop)
  2. eu-307.2: Diagnostics from Rowan parser
  3. eu-307.3: Document symbols
  4. eu-307.4: Folding and selection ranges
  5. eu-307.5: VS Code extension

DEPENDENCIES:
- Add lsp-server and lsp-types to Cargo.toml
- Wire eu lsp subcommand into src/driver/options.rs clap structure
- New module: src/driver/lsp.rs (and src/driver/lsp/ submodules)

TESTING:
- Unit tests for each LSP feature
- Manual testing via VS Code extension during development
- Automated test: spawn eu lsp, send JSON-RPC messages, check responses

SCOPE:
Phase 1 only. Do NOT start phase 2 (semantic features) without
explicit go-ahead from orchestrator.
```

### Seren (Reviewer / Architect)

```
DIRECTORY: /Users/greg/dev/curvelogic/eucalypt (main repo)

ROLE: Architect, PR reviewer, documentation guardian.

PR REVIEW PROCESS:
1. When notified of a PR, fetch and check out the branch
2. Read the diff against master
3. Verify:
   a. Code matches the design plan in docs/plans/
   b. All tests pass (cargo test)
   c. Clippy clean (cargo clippy --all-targets -- -D warnings)
   d. Formatting clean (cargo fmt --all -- --check)
   e. UK English in comments/docs/errors
   f. No #[allow()] suppressions
   g. Harness tests added for new features
   h. No unintended changes to other features
4. If issues found: send specific feedback to the authoring agent
5. If clean: squash-merge to master via gh pr merge --squash
6. After merge: verify master CI passes
7. Notify orchestrator that eu-XXXX has merged (triggers bead close)

ARCHITECTURAL OVERSIGHT:
- Watch for cross-stream conflicts (e.g., two agents modifying intrinsics.rs)
- Ensure symbol interning changes propagate correctly
- Flag if an agent deviates from the design plan
- Check that new intrinsics follow the established pattern

DOCUMENTATION OVERSIGHT:
- Flag if harness tests are missing for new features
- Check that prelude doc metadata (` "description") is present
- Note if docs/plans/ need updating based on implementation changes
- Track whether docs/ user-facing docs need refresh

DOCS REFRESH (eu-ptu):
- After the bulk of features have landed, assess and fix the docs build
- Refresh user-facing documentation to reflect 0.3.0 features
- This feeds into Phase 5 documentation replacement work

DECISION AUTHORITY: You CAN decide without escalating:
  - Architectural details not specified in the plans (choose the
    simpler approach, document in PR review comments)
  - Merge conflict resolution for trivial conflicts (adjacent lines,
    import ordering in mod.rs)
  - Requesting minor code changes from agents (naming, structure,
    error handling)
  - Approving reasonable implementation choices agents made beyond
    what the spec covers

ESCALATION: Escalate to orchestrator ONLY when:
  - Semantic merge conflict between two agents' work
  - An agent's PR fundamentally deviates from the design plan
  - Two rounds of feedback haven't resolved an issue
  - Cross-stream dependency needs re-ordering

MERGE CONFLICT RESOLUTION:
- Trivial conflicts (e.g., adjacent lines in mod.rs): resolve yourself
- Semantic conflicts (overlapping features): escalate to orchestrator

ESCALATION TRIAGE: When a coding agent escalates to you (not a PR):
1. Build/clippy failure after 5 attempts:
   - Read the agent's code and error output
   - Suggest a concrete fix or alternative approach
   - If you can identify the root cause, send it back with instructions
   - If you cannot: escalate to orchestrator
2. Unclear spec intent:
   - Read the relevant design plan in docs/plans/
   - If the intent is recoverable from context, make a decision and
     document it in a message back to the agent
   - If the spec is genuinely ambiguous about user-visible behaviour:
     escalate to orchestrator
3. Cross-stream conflict:
   - Escalate to orchestrator immediately (they coordinate agents)
4. User-visible API change not in spec:
   - Escalate to orchestrator (they decide scope changes)
```

### Orchestrator Decision Authority

```
The orchestrator (me) has FULL authority to make decisions without
consulting the user, provided the decision:
  - Has only private/internal impact (code structure, implementation
    approach, task ordering, agent coordination)
  - Is REVERSIBLE (can be undone in a subsequent PR or commit)

This includes:
  - Resolving spec ambiguities (choosing between reasonable interpretations)
  - Re-ordering tasks or shifting work between agents
  - Approving minor scope adjustments (e.g., adding a helper function
    not in the spec, renaming an internal API)
  - Deciding on implementation approaches when agents or Seren escalate
  - Resolving cross-stream conflicts by choosing which agent yields
  - Adjusting GC phase ordering if benchmarks suggest a different sequence
  - Changing branch/merge order for dependency reasons

Decisions that require user consultation (NOT reversible or public-facing):
  - Dropping a planned feature from 0.3.0 scope
  - Changing user-visible syntax or semantics beyond what specs define
  - Altering the public merge-back content decisions
  - Any change to the release process or version numbering
```

## Coordination Protocol

### Cross-Stream Dependencies

| Dependency | Producer | Consumer | Signal |
|-----------|----------|----------|--------|
| eu-4af (symbol interning) done | Niamh | Callum (for eu-kfe) | orchestrator notifies Callum |
| eu-u1m (polymorphic lt) done | Callum | Callum (for eu-ert) | internal, same agent |
| eu-da3 (string comparison) done | Callum | Callum (for eu-ert) | internal, same agent |
| eu-twg (error tests) done | Fricka | Callum (for eu-dlr) | orchestrator notifies Callum |

### Communication Flow

```
Niamh/Callum/Fricka/Ravi → Seren (PR ready)
Seren → authoring agent (feedback / merge confirmation)
any agent → orchestrator (task complete / escalation)
orchestrator → any agent (unblock / new assignment)
orchestrator → Seren (priority guidance)
```

Coding agents do NOT message each other directly. All cross-stream
coordination goes through the orchestrator.

### Ralph-Loop Iteration Pattern

For each task, agents follow this cycle:

```
1. Read the design plan
2. Implement
3. cargo fmt --all
4. cargo clippy --all-targets -- -D warnings
5. cargo test
6. If 3-5 fail: fix and go to 3 (up to 5 iterations)
7. If pass: commit, push, create PR, notify Seren
8. If 5 iterations exhausted: escalate to Seren
```

Seren has a similar loop for merge issues:
```
1. Review PR
2. If issues: send specific feedback, agent fixes, re-review (up to 2 rounds)
3. If clean: merge to master, verify CI
4. If 2 rounds don't resolve: escalate to orchestrator
```

## Launch Sequence

### Step 0: Pre-launch Fixes (orchestrator, before spawning agents)
- Fix CI clippy to use `--all-targets` in `.github/workflows/build-rust.yaml`
  (line 63: `cargo clippy -- -D warnings` → `cargo clippy --all-targets -- -D warnings`)
  This aligns CI with the CLAUDE.md pre-commit checklist and what agents validate locally.
- Commit and push this fix to master before creating worktrees.

### Step 1: Infrastructure Setup (orchestrator)
- Create worktree directories
- Create worktrees from master (after the CI fix is on master)
- Create the team via TeamCreate
- Set up initial task list from beads

### Step 2: Spawn Agents (orchestrator)
- Spawn all 5 agents with their briefs
- Each agent starts by reading their assigned design plans
- Coding agents begin their first tasks immediately

### Step 3: Steady State
- Agents work autonomously, creating PRs
- Reviewer processes PRs as they arrive
- Orchestrator monitors progress, manages beads, unblocks dependencies
- When eu-4af merges, orchestrator signals Callum to start eu-kfe

### Step 4: Completion
- All PRs merged to master
- Full test suite passes on master
- Orchestrator closes beads and syncs
- Orchestrator shuts down team

## Verification

After all work is merged:
1. `cargo test` — full suite passes
2. `cargo clippy --all-targets -- -D warnings` — clean
3. `cargo fmt --all -- --check` — clean
4. `cargo build --release` — builds successfully
5. `target/release/eu harness/test -T` — release binary passes harness
6. All beads for targeted features are closed
7. `bd sync` — beads fully synced

## Context Management

Large tasks (especially eu-181d GC Immix with ~15+ sub-steps) risk
exhausting agent context windows. Mitigation:

- **One branch/PR per sub-bead**: Each sub-bead is a self-contained
  unit of work. The agent completes it, creates a PR, and moves on.
- **Re-briefing between sub-beads**: When an agent finishes a sub-bead,
  the orchestrator can send a fresh brief for the next one, including
  any relevant context from the completed work.
- **Agent resumption**: If an agent is suspended or loses context, the
  orchestrator re-spawns it with a focused prompt referencing the
  design plan and the current state of the branch.
- **Design plans as ground truth**: The `docs/plans/` files are the
  authoritative reference. Agents re-read them at the start of each
  sub-bead rather than relying on accumulated context.
- **PR descriptions as memory**: Each completed PR's description
  captures what was done and why. This serves as durable context for
  subsequent work.

For Niamh's GC Immix work specifically: each of the 5 evacuation
sub-steps (a-e) is a separate PR. If context runs out mid-step, the
orchestrator re-briefs Niamh with just that sub-step's scope.

## Phase 5: Public Merge-Back (eu-4bkv)

After all 0.3.0 work is verified on master, merge the private repo
back to the public `curvelogic/eucalypt` repository.

### Pre-Merge Content Decisions

| Item | Decision |
|------|----------|
| `.beads/` directory | Exclude — start fresh with GitHub issues on public repo |
| `openspec/` directory | Exclude — proposals are internal planning artefacts |
| `.claude/`, `.gemini/`, `.opencode/` | Exclude — AI tooling config is developer-specific |
| `docs/plans/` design documents | **Remove** — replace with proper user-facing documentation |
| Commit history | Squash/rebase to clean public history (see below) |

### Documentation Replacement

Before the merge, replace `docs/plans/` with public-facing documentation:

1. **CHANGELOG.md** — Comprehensive 0.3.0 release notes covering all new
   features, improvements, and breaking changes. Grouped by category
   (language features, performance, tooling, prelude additions).

2. **Updated README.md** — Reflect new capabilities (LSP, ZDT literals,
   sets, sorting, deep-find, etc.). Update installation and usage sections.

3. **Architecture guide** (`docs/architecture.md`) — High-level overview
   of the codebase for contributors. Cover the compilation pipeline,
   STG machine, GC, prelude, and test infrastructure.

4. **Generated prelude reference** — Output from eu-ber (Fricka's docs
   gen work). Auto-generated from prelude doc metadata.

5. **LSP setup guide** (`docs/lsp.md`) — How to configure Emacs and
   VS Code with the language server.

### Merge-Back Steps

1. **Create clean branch**: `git checkout -b release/0.3.0 master`

2. **Remove excluded content**:
   ```
   rm -rf .beads/ openspec/ .claude/ .gemini/ .opencode/ docs/plans/
   ```

3. **Add new documentation**: CHANGELOG.md, updated README, architecture
   guide, LSP guide (as listed above)

4. **Verify build**: `cargo test && cargo clippy --all-targets -- -D warnings`

5. **Squash to single commit** against `upstream/master`: one commit
   titled "Eucalypt 0.3.0" with a comprehensive message summarising
   all changes.

6. **Push to public**: `git push upstream release/0.3.0`

7. **Create PR** on `curvelogic/eucalypt` for review

8. **After merge**: Tag as `v0.3.0`, create GitHub release

### CI/CD on Public Repo

The public repo's CI/CD workflows are **identical** to the private repo
(zero drift). Three workflow files:

- **`build-rust.yaml`** — On every push/PR: check, test (ubuntu + macOS),
  fmt, clippy. On master only: builds release binaries (linux amd64 +
  macOS), generates changelog via Claude API, creates draft GitHub release.
- **`release.yaml`** — On published release: updates `latest` branch,
  generates homebrew formula.
- **`docs.yaml`** — Documentation workflow.

**Changes from 0.3.0 that affect CI**:
- Clippy flag change: `cargo clippy -- -D warnings` →
  `cargo clippy --all-targets -- -D warnings` (Step 0 fix, carried in
  the squashed commit)
- New crate dependencies (lsp-server, lsp-types, semver, base64, sha2)
  — these are in Cargo.toml and will be picked up automatically
- New `eu lsp` subcommand — no CI impact
- No other workflow changes needed

**The CI will work on public without modification** after the merge.

### Changelog and Release Process

The public repo auto-generates release notes from `git log latest..HEAD`
via the Claude API (using `.github/release-notes-prompt.md`). Since
we're squashing to a single commit, the auto-generated changelog would
only see that one commit.

**Strategy**: Write a **comprehensive squash commit message** that lists
all major changes in a structured format. The Claude API will use this
to generate polished release notes. The commit message should include:

```
Eucalypt 0.3.0

Major release with performance improvements, new language features,
expanded prelude, developer tooling, and LSP support.

## Performance
- Symbol interning: SymbolId(u32) replaces heap-allocated symbols
- Block indexing: O(1) lookup for blocks with 16+ keys
- STG case optimisations: known-constructor, tag-only, dead-alt, wrapper
- DCE inside blocks: static-access dead code elimination
- GC Immix completion: lazy sweeping and opportunistic evacuation

## Language Features
- ZDT literals: t"2023-01-15T10:30:00Z" syntax
- Set data type: {| :a, :b, :c |} with full set operations
- Polymorphic comparison: lt/gt work for numbers, strings, symbols, ZDTs

## Prelude
- Sorting: sort-nums, sort-strs, sort-by, sort-by-num, sort-by-str
- String comparison: str.lt, str.gt, str.lte, str.gte, sort-keys
- Deep find: deep-find, deep-find-first, deep-find-paths, deep-query
- Utilities: eu.requires, str.base64-encode, str.base64-decode, str.sha256
- Catch {name: name} self-reference cycles

## Tooling
- LSP server: eu lsp with diagnostics, document symbols, folding
- VS Code extension with TextMate grammar
- Error message test infrastructure (.expect sidecars)
- Benchmark support in test harness
- Generated prelude reference documentation

## String Intrinsics
- Streaming return for SPLIT, MATCH, MATCHES, LETTERS
```

After the squash commit is pushed to public master, CI will:
1. Run check/test/fmt/clippy (should all pass)
2. Build release binaries
3. Generate changelog from the commit message via Claude API
4. Create a **draft** GitHub release

The orchestrator then:
1. Reviews the draft release notes
2. Edits if needed (the Claude API output is a starting point)
3. Publishes the release
4. `release.yaml` runs: updates `latest` branch, generates homebrew formula

### Who Does This

Seren prepares the documentation (steps 1-4). The orchestrator handles
the git operations (steps 5-8) since interactive rebase and force-push
require human judgement.

### Timing and Approval Gate

This phase starts ONLY after:
1. Step 4 (Completion) of the main implementation is fully verified
2. The orchestrator presents a summary of all completed work to the user
3. **The user explicitly approves proceeding with the public merge-back**

No agent or the orchestrator may touch the public repository (upstream)
until the user has reviewed the final state of the private repo and
given the go-ahead. This is a hard gate — not a formality.

During 0.3.0 development (Steps 0-4), the user is not consulted unless
they are actively watching and showing interest. The orchestrator
operates autonomously within its decision authority.

## Risk Mitigation

| Risk | Mitigation |
|------|-----------|
| Merge conflicts in shared files (intrinsics.rs, mod.rs, prelude.eu) | Reviewer merges sequentially; agents always branch from latest master |
| Agent diverges from design plan | Reviewer checks every PR against the plan |
| GC/VM changes break correctness | Niamh runs full test suite; benchmarks before/after |
| Agents spin on unfixable issues | 5-iteration cap, then escalate |
| Beads get out of sync | Only orchestrator manages beads |
| Context exhaustion on long tasks | Each task is one branch/PR; agents can be resumed |
| Concurrent cargo builds in worktrees | Separate worktrees have separate target dirs |
| Agent context exhaustion on large tasks | One branch/PR per sub-bead; orchestrator re-briefs between sub-beads |
| Public merge-back loses important context | Design decisions captured in docs, not just plans; public-ready code from start |
| Internal references leak to public | PUBLIC-READY rule in shared guidelines; Seren checks during review |
