# Eucalypt v0.4.0 Agent Team Implementation Plan

## Context

Eucalypt v0.4.0 has ~45 beads across 9 workstreams covering error
messages, documentation, compiler formats, CI/infrastructure, STG
optimisation, changelog, LSP formatting, and Advent of Code examples.
All design plans and research reports are in `docs/plans/`. Work is
tracked in beads under the parent epic eu-ls93.

The repository is the public `curvelogic/eucalypt`. Pushes to master
each trigger a release — there is no private fork workflow. All code
and commits must be public-ready from day one.

## Team Structure

| Name | Role | Worktree | Scope |
|------|------|----------|-------|
| **Aoife** | Error messages & diagnostics | `eucalypt-errors` | eu-fti1 (20 children) |
| **Brennan** | Documentation | `eucalypt-docs` | eu-wma8 (13 children) |
| **Ciara** | Compiler, formats, LSP, changelog | `eucalypt-compiler` | eu-vt15, eu-7n8k, eu-ntib, eu-t99l |
| **Declan** | CI/infrastructure, then error messages | `eucalypt-ci` | eu-epe4, eu-1j9u, then joins Aoife |
| **Eilis** | Reviewer/architect | main repo | PR gate, cross-stream, quality |

**Orchestrator** (me): delegate, unblock, manage beads, handle
escalations, relay documentation reviews to user.

## Git Strategy

### Worktree Setup

Each coding agent gets its own git worktree branched from master. This
avoids checkout conflicts and gives each agent its own `target/`
directory for independent builds.

```bash
mkdir -p /Users/greg/dev/curvelogic/eucalypt-worktrees

git worktree add /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-errors master
git worktree add /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-docs master
git worktree add /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-compiler master
git worktree add /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-ci master
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

- Eilis **squash-merges** each PR (one clean commit per bead task)
- Squash commit message format: `feat(eu-XXXX): Short description of change`
  with a body summarising what was done and why
- After each merge, Eilis runs full test suite on master
- If merge conflicts arise, Eilis resolves or asks the authoring agent
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
  "feat(eu-dohu): Expand error test coverage baseline"

UK ENGLISH: All comments, docs, error messages use UK English.

PUBLIC-READY: All code will be pushed to the public eucalypt repo.
Write everything as if it's public from day one:
  - No internal jargon, no references to private tools
  - Clean, helpful error messages for external users
  - Comments explain "why", not "what" — assume a competent Rust reader

CLIPPY: Fix ALL warnings. Never use #[allow()] to suppress.

DECISION AUTHORITY: You CAN decide without escalating:
  - Implementation details not covered by the spec
  - Internal API additions (helper functions, internal modules)
  - Performance trade-offs within the spec's stated approach
  - Test strategy and coverage details beyond what the spec lists
  - Error message wording (following UK English, clear and helpful)
  - Minor refactoring needed to land your feature cleanly

ESCALATION: Send a message to Eilis ONLY when:
  - You've failed 5 fix attempts on the same issue
  - You discover a cross-stream conflict affecting another agent
  - You need to change a user-visible API not covered by the spec
  - The spec is fundamentally unclear about the desired behaviour

DESIGN PLANS: Read docs/plans/*.md for your assigned features BEFORE
starting implementation. These are the source of truth:
  - docs/plans/2026-02-09-error-messages-implementation-plan.md
  - docs/plans/2026-02-09-error-messages-research.md
  - docs/plans/2026-02-09-documentation-refresh-implementation-plan.md
  - docs/plans/2026-02-09-documentation-refresh-research.md
  - docs/plans/2026-02-09-dump-quote-formats-design.md
  - docs/plans/2026-02-09-aarch64-linux-release-design.md
  - docs/plans/2026-02-09-install-script-design.md

NO BEADS MANAGEMENT: The orchestrator manages beads. You report task
completion via SendMessage to the orchestrator. Do not run bd commands.

BEAD LIFECYCLE (orchestrator handles all of this):
  - in_progress: when orchestrator assigns/signals an agent to start
  - close (sub-bead): after Eilis confirms squash-merge to master
    and master CI passes
  - close (parent epic): when all child sub-beads are closed
  - bd sync: orchestrator runs periodically and at session end
```

### Aoife (Error Messages)

```
WORKTREE: /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-errors

DESIGN PLAN: docs/plans/2026-02-09-error-messages-implementation-plan.md
RESEARCH: docs/plans/2026-02-09-error-messages-research.md

TASK ORDER:
Phase 0 (do first — establishes baseline):
  - eu-dohu: Expand error test coverage baseline

Phase 1 (independent — all can run in parallel after Phase 0):
  - eu-l7e1: Human-readable data tag names
  - eu-dcio: Recognise division by zero
  - eu-bdnd: Clean up parse error formatting
  - eu-cb4b: Remove Smid values from messages
  - eu-h1kp: Improve 'not callable' error message
  - eu-wkpj: Drop environment trace from output

Phase 2 (sequential — each builds on the previous):
  - eu-qjs1: Use annotated_lambda for user closures (FOUNDATION)
  - eu-8fuh: Add annotation to ApplyTo continuations (depends on qjs1)
  - eu-5na7: Propagate Smid to intrinsic error constructors
  - eu-cckb: Map intrinsic names to user-facing names
  - eu-0r7w: Source-level stack traces (depends on all of the above)

Phase 3 (depends on Phase 2):
  - eu-w4s0: Expected vs actual type in type mismatches
  - eu-tk4r: Improve runtime stack traces
  - eu-knck: Multi-label diagnostics (depends on w4s0)

Phase 4 (long-term polish, lower priority):
  - eu-ul5g: Convert panics to proper errors
  - eu-67v5: Error codes and documentation
  - eu-6lbi: 'Did you mean?' suggestions
  - eu-7bbl: Error recovery and multiple errors
  - eu-de6w: Structured JSON error output

WHEN DECLAN ARRIVES: After Declan finishes CI work (eu-epe4, eu-1j9u),
he joins your worktree. Coordinate via orchestrator — give him Phase 1
tasks you haven't started yet, or Phase 4 tasks.

TESTING DISCIPLINE:
- Every task MUST create or update .expect sidecar files
- Run cargo test --test harness_test to verify error output matches
- Run full test suite after every change
- Error message changes are highly visible regressions — the .expect
  mechanism provides strong protection

SPECIAL RULES:
- Phase 2 trace machinery changes are safety-critical — test thoroughly
- After Phase 2E (source-level traces), ALL .expect files may need
  updating to reflect new trace format
- Annotations affect optimiser behaviour — run full test suite after
  Phase 2A (annotated_lambda change)
```

### Brennan (Documentation)

```
WORKTREE: /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-docs

DESIGN PLAN: docs/plans/2026-02-09-documentation-refresh-implementation-plan.md
RESEARCH: docs/plans/2026-02-09-documentation-refresh-research.md

TASK ORDER:
Phase 1 (infrastructure — blocks everything):
  - eu-tzy0: Switch documentation site to mdBook
  - eu-mfff: Custom eu syntax highlighting (depends on tzy0)

Phase 2 (content migration — depends on Phase 1):
  - eu-c2q5: Fix stale content and migrate to new structure
  - eu-b3pi: Document 12 undocumented features (depends on c2q5)
  - eu-wr4u: Auto-generate prelude reference from source
  - eu-iy92: Documentation testing for code examples

Phase 3 (new content — depends on c2q5):
  - eu-xf3f: Write the Eucalypt Guide (15 chapters)
  - eu-hiig: Write Eucalypt by Example (10-15 worked examples)
  - eu-dxwb: Write FAQ (20 questions)
  - eu-po7z: Write syntax cheat sheet

Phase 4 (agent-friendly docs — depends on content):
  - eu-joxw: Create llms.txt and llms-full.txt
  - eu-4wl6: Expand AGENTS.md with language reference
  - eu-e1xm: Create dedicated agent reference page

DOCUMENTATION REVIEW PROTOCOL:
Content produced for Phase 3 and Phase 4 (Guide chapters, Examples,
FAQ, cheat sheet, agent docs) REQUIRES USER REVIEW before merge.

The workflow:
  1. Complete the content in your branch
  2. Create a PR as normal
  3. Notify orchestrator: "Content ready for user review: eu-XXXX"
  4. Orchestrator presents the content to the user via /showme
  5. User reviews and gives the go-ahead (possibly with feedback)
  6. If feedback: address it and repeat from step 2
  7. Once approved: Eilis merges

Infrastructure work (Phase 1, Phase 2) does NOT require user review —
Eilis handles those PRs directly.

SPECIAL RULES:
- ALL code examples must be runnable with eu
- Use documentation testing (once eu-iy92 is complete) to validate
- Follow the agreed site structure exactly (see design plan)
- Prelude reference auto-generation should split by category
- Architecture/GC docs stay in repo but NOT in published site
```

### Ciara (Compiler, Formats, LSP, Changelog)

```
WORKTREE: /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-compiler

DESIGN PLANS:
  - docs/plans/2026-02-09-dump-quote-formats-design.md
  - Changelog spec in eu-ntib bead description
  - LSP formatting spec in eu-t99l bead description

TASK ORDER:
Stream 1 — Dump/Quote Formats (eu-vt15):
  - eu-6m0w: Redesign core expression pretty-printer
  - eu-bscw: Redesign STG pretty-printer
  - eu-lvbn: Complete core embed for all expression types
  - eu-w2c8: Complete STG embed for all expression types
  - eu-li2a: Document stable embed format for core and STG

  6m0w and bscw are independent. lvbn and w2c8 are independent.
  li2a depends on lvbn and w2c8.

Stream 2 — STG Optimisation (eu-7n8k):
  - eu-drh0: (nearly done from 0.3.0 — finish off)
  - eu-vzv: Benchmark text pipelines

Stream 3 — Changelog (eu-ntib):
  - Remove Claude API release notes from CI
  - Replace with git log formatting
  - Remove .changelog.yaml and .github/release-notes-prompt.md
  - Remove latest branch update job

Stream 4 — LSP Range Formatting (eu-t99l):
  - Declaration-level granularity for format-on-type and format-selection
  - Enable document_range_formatting_provider in server capabilities
  - See full spec in eu-t99l bead description

RECOMMENDED SEQUENCING:
1. Start with eu-ntib (changelog, small scope, quick win)
2. Stream 1 (dump/quote, do 6m0w + bscw in parallel, then lvbn + w2c8)
3. Stream 2 (STG optimisation, finish eu-drh0, then eu-vzv)
4. eu-t99l (LSP formatting, independent)
5. eu-li2a (document formats, after embeds complete)

DUMP/QUOTE DESIGN PRINCIPLES:
- Dump: compact, symbolic, honest intrinsic names, for debugging
- Embed: inline ASM for eucalypt — write syntactically valid eucalypt
  that directly constructs core/STG expressions
- Core dump: one output format for all transformation phases
- STG dump: tighter and better structured
- Stability by convention, not snapshot tests

SPECIAL RULES:
- Changelog changes affect CI — test workflow changes carefully
- LSP changes: test via unit tests and manual VS Code testing
- STG pretty-printer: don't break existing test output that uses dumps
- Format documentation (eu-li2a) is the last task — needs content from
  all preceding work
```

### Declan (CI/Infrastructure → Error Messages)

```
WORKTREE: /Users/greg/dev/curvelogic/eucalypt-worktrees/eucalypt-ci

DESIGN PLANS:
  - docs/plans/2026-02-09-aarch64-linux-release-design.md
  - docs/plans/2026-02-09-install-script-design.md

TASK ORDER:
Phase 1 — CI (primary assignment):
  - eu-epe4: Add aarch64-linux release binary
  - eu-1j9u: Create curl-installable install script (depends on epe4)

Phase 2 — Rebase to Aoife's worktree:
  After both CI tasks are merged to master, Declan switches to
  eucalypt-errors and takes error message tasks from Aoife.
  Orchestrator coordinates the handoff.

AARCH64 BINARY (eu-epe4):
  - Add ubuntu-24.04-arm runner to build-rust.yaml
  - Run full harness test on ARM (same as x86_64)
  - Three release tarballs: x86_64-linux, aarch64-linux, aarch64-darwin
  - ARM-only macOS (macos-latest is already ARM)
  - Keep architecture-OS naming convention

INSTALL SCRIPT (eu-1j9u):
  - install.sh at repo root, served via GitHub raw URL
  - Default install to ~/.local/bin (override via EUCALYPT_INSTALL_DIR)
  - Version configurable via EUCALYPT_VERSION (default: latest)
  - SHA256SUMS hash verification per release
  - Warn about PATH and print instructions

SPECIAL RULES:
- Test CI changes on a branch before merging — push and verify the
  GitHub Actions run
- Install script must be POSIX-compatible (not bash-specific)
- Install script must handle all three platforms gracefully
- SHA256SUMS file must be generated as part of the release workflow
```

### Eilis (Reviewer / Architect)

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
7. Notify orchestrator that eu-XXXX has merged

DOCUMENTATION CONTENT REVIEW:
For Brennan's content PRs (Phase 3 and Phase 4 of eu-wma8):
  - Do NOT merge directly — these require user approval
  - Do a technical review (links, code examples, structure)
  - Send technical feedback to Brennan
  - Notify orchestrator when technically clean: "eu-XXXX ready for
    user review"
  - Orchestrator handles the user approval flow
  - Only merge after orchestrator signals "user approved eu-XXXX"

For Brennan's infrastructure PRs (Phase 1, Phase 2):
  - Review and merge normally (no user approval needed)

ARCHITECTURAL OVERSIGHT:
- Watch for cross-stream conflicts (e.g., Aoife and Ciara both
  touching error.rs or vm.rs)
- Ensure annotation/trace changes don't break optimiser behaviour
- Flag if an agent deviates from the design plan
- Check that new error messages follow the agreed format
- Verify CI workflow changes on branch before merge

ESCALATION TRIAGE:
1. Build/clippy failure after 5 attempts:
   - Read the agent's code and error output
   - Suggest a concrete fix or alternative approach
   - If root cause identified: send fix instructions back
   - If not: escalate to orchestrator
2. Unclear spec intent:
   - Read the relevant design plan
   - If intent is recoverable from context: make a decision and
     document it in a message back to the agent
   - If genuinely ambiguous: escalate to orchestrator
3. Cross-stream conflict:
   - Escalate to orchestrator immediately
4. User-visible API change not in spec:
   - Escalate to orchestrator

DECISION AUTHORITY: You CAN decide without escalating:
  - Architectural details not in the specs (choose simpler approach)
  - Merge conflict resolution for trivial conflicts
  - Requesting minor code changes (naming, structure, error handling)
  - Approving reasonable implementation choices beyond spec
```

### Orchestrator Decision Authority

```
The orchestrator (me) has FULL authority to make decisions without
consulting the user, provided the decision:
  - Has only private/internal impact (code structure, implementation
    approach, task ordering, agent coordination)
  - Is REVERSIBLE (can be undone in a subsequent PR or commit)

This includes:
  - Resolving spec ambiguities
  - Re-ordering tasks or shifting work between agents
  - Approving minor scope adjustments
  - Deciding on implementation approaches when agents escalate
  - Resolving cross-stream conflicts
  - Changing branch/merge order for dependency reasons

DOCUMENTATION CONTENT is the exception:
  - Orchestrator does NOT approve documentation content merges
  - All content PRs (Guide, Examples, FAQ, cheat sheet, agent docs)
    MUST be shown to the user via /showme for approval
  - User feedback is relayed to Brennan; user approval triggers merge

Decisions requiring user consultation (NOT reversible or public-facing):
  - Dropping a planned feature from 0.4.0 scope
  - Changing user-visible syntax or semantics beyond specs
  - Merging documentation content without user review
  - Any change to the release process or version numbering
```

## Documentation Review Workflow

Documentation content is the one workstream where the user wants to
stay hands-on. The flow:

```
Brennan writes content (Guide chapter / Examples / FAQ / etc.)
  ↓
Brennan creates PR and notifies Eilis
  ↓
Eilis does technical review (code examples, links, structure)
  ↓
Eilis sends technical feedback → Brennan fixes
  ↓
Eilis notifies orchestrator: "eu-XXXX technically clean"
  ↓
Orchestrator invokes /showme for the user with the content
  ↓
User reviews and either:
  • Approves → orchestrator signals Eilis to merge
  • Gives feedback → orchestrator relays to Brennan → repeat
```

This does NOT apply to infrastructure work (mdBook setup, syntax
highlighting, prelude auto-generation, doc testing). Those go through
normal Eilis review and merge.

## Cross-Stream Dependencies

| Dependency | Producer | Consumer | Signal |
|-----------|----------|----------|--------|
| eu-epe4 (aarch64 binary) done | Declan | Declan (eu-1j9u) | same agent, sequential |
| eu-1j9u (install script) done | Declan | Brennan (Quick Start page) | orchestrator notifies Brennan |
| Declan finishes CI work | Declan | Aoife (takes Phase 1/4 tasks) | orchestrator reassigns |
| eu-dohu (error test baseline) done | Aoife | Aoife (Phase 1) | same agent, sequential |
| eu-qjs1 (annotated_lambda) done | Aoife | Aoife (eu-8fuh) | same agent, sequential |
| eu-w4s0 (type mismatches) done | Aoife | Aoife (eu-knck) | same agent, sequential |
| eu-tzy0 (mdBook) done | Brennan | Brennan (all Phase 2+) | same agent, sequential |
| eu-c2q5 (content migration) done | Brennan | Brennan (Guide, Examples, FAQ) | same agent, sequential |
| eu-fti1 (error messages) progress | Aoife | Brennan (Error Messages Guide) | orchestrator coordinates timing |
| eu-lvbn + eu-w2c8 (embeds) done | Ciara | Ciara (eu-li2a docs) | same agent, sequential |
| Install instructions available | Declan | Brennan (Quick Start docs) | orchestrator notifies |

## Communication Flow

```
Aoife/Brennan/Ciara/Declan → Eilis (PR ready)
Eilis → authoring agent (feedback / merge confirmation)
any agent → orchestrator (task complete / escalation)
orchestrator → any agent (unblock / new assignment)
orchestrator → Eilis (priority guidance)
orchestrator ↔ user (/showme for doc content review)
```

Coding agents do NOT message each other directly. All cross-stream
coordination goes through the orchestrator.

## Sequencing Overview

### Week 1

| Agent | Tasks |
|-------|-------|
| Aoife | eu-dohu (error test baseline), then start Phase 1 quick wins |
| Brennan | eu-tzy0 (mdBook setup), eu-mfff (syntax highlighting) |
| Ciara | eu-ntib (changelog, quick win), then eu-6m0w + eu-bscw (pretty-printers) |
| Declan | eu-epe4 (aarch64 binary) |
| Eilis | Review PRs as they arrive |

### Week 2

| Agent | Tasks |
|-------|-------|
| Aoife | Finish Phase 1, start Phase 2 (trace machinery) |
| Brennan | eu-c2q5 (content migration), eu-b3pi (document features), eu-wr4u (prelude gen) |
| Ciara | eu-lvbn + eu-w2c8 (embed completion), eu-drh0 (STG optimisation) |
| Declan | eu-1j9u (install script), then switch to eucalypt-errors |
| Eilis | Review PRs, watch for cross-stream conflicts |

### Week 3

| Agent | Tasks |
|-------|-------|
| Aoife + Declan | Phase 2 trace machinery, Phase 3 type errors |
| Brennan | eu-xf3f (Guide chapters) — content review cycle starts |
| Ciara | eu-vzv (benchmarks), eu-t99l (LSP formatting) |
| Eilis | Review PRs, technical review of doc content |

### Week 4+

| Agent | Tasks |
|-------|-------|
| Aoife + Declan | Phase 4 polish (error codes, suggestions, recovery) |
| Brennan | eu-hiig (Examples), eu-dxwb (FAQ), Phase 4 agent docs |
| Ciara | eu-li2a (format docs), any remaining tasks |
| Eilis | Final reviews, ensure all streams converge |

## Quality Gate (Ralph-Loop)

For each task, agents follow this cycle:

```
1. Read the design plan
2. Implement
3. cargo fmt --all
4. cargo clippy --all-targets -- -D warnings
5. cargo test
6. If 3-5 fail: fix and go to 3 (up to 5 iterations)
7. If pass: commit, push, create PR, notify Eilis
8. If 5 iterations exhausted: escalate to Eilis
```

Eilis review loop:
```
1. Review PR
2. If issues: send specific feedback, agent fixes, re-review (up to 2 rounds)
3. If clean: merge to master, verify CI
4. If 2 rounds don't resolve: escalate to orchestrator
```

## Launch Sequence

### Step 0: Pre-launch Fixes (orchestrator)
- Remove eu-bpe dependency from eu-7n8k (DONE)
- Verify all bead dependencies are correct
- Commit and push any pre-launch fixes

### Step 1: Infrastructure Setup (orchestrator)
- Create worktree directories
- Create worktrees from master
- Create the team via TeamCreate
- Set up initial task assignments from beads

### Step 2: Spawn Agents (orchestrator)
- Spawn all 5 agents with their briefs
- Each agent starts by reading their assigned design plans
- Coding agents begin their first tasks immediately

### Step 3: Steady State
- Agents work autonomously, creating PRs
- Eilis processes PRs as they arrive
- Orchestrator monitors progress, manages beads, unblocks dependencies
- When Declan finishes CI, orchestrator reassigns to error messages
- Documentation content goes through user review cycle

### Step 4: Completion
- All PRs merged to master
- Full test suite passes on master
- Orchestrator closes all beads and syncs
- Orchestrator shuts down team

## Verification

After all work is merged:
1. `cargo test` — full suite passes
2. `cargo clippy --all-targets -- -D warnings` — clean
3. `cargo fmt --all -- --check` — clean
4. `cargo build --release` — builds successfully
5. `target/release/eu harness/test -T` — release binary passes harness
6. `mdbook build` — documentation builds cleanly
7. All beads under eu-ls93 are closed
8. `bd sync` — beads fully synced

## Context Management

Large tasks (especially eu-xf3f Guide with 15 chapters) risk
exhausting agent context windows. Mitigation:

- **One branch/PR per sub-bead**: Each bead is a self-contained
  unit of work. The agent completes it, creates a PR, and moves on.
- **Re-briefing between tasks**: When an agent finishes a task,
  the orchestrator can send a fresh brief for the next one.
- **Design plans as ground truth**: The `docs/plans/` files are the
  authoritative reference. Agents re-read them at the start of each
  task rather than relying on accumulated context.
- **PR descriptions as memory**: Each completed PR's description
  captures what was done and why.

For Brennan's Guide work specifically: each chapter can be a separate
PR if context becomes an issue. The orchestrator re-briefs Brennan
with chapter scope and preceding context.

## Risk Mitigation

| Risk | Mitigation |
|------|-----------|
| Merge conflicts in shared files (error.rs, vm.rs) | Eilis merges sequentially; agents branch from latest master |
| Agent diverges from design plan | Eilis checks every PR against the plan |
| Trace machinery changes break correctness | Aoife runs full test suite; comprehensive error tests |
| Agents spin on unfixable issues | 5-iteration cap, then escalate |
| Beads get out of sync | Only orchestrator manages beads |
| Context exhaustion on long tasks | One branch/PR per bead; orchestrator re-briefs |
| Concurrent cargo builds in worktrees | Separate worktrees have separate target dirs |
| Documentation content not meeting user expectations | User review cycle via /showme before merge |
| Cross-stream timing (error messages needed for error guide) | Orchestrator coordinates; Brennan writes error guide late |
| CI workflow changes break releases | Test on branch before merging; Eilis verifies |
| Declan worktree switch disruption | Clean handoff: finish CI tasks, merge, then fresh branch in errors worktree |
