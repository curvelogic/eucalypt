# 0.11.0 Implementation Plan

- **Date:** 2026-06-26
- **Baseline:** 0.10.1
- **Theme:** Codegen wins, typing on, type-value foundation, bytecode spike

---

## 1. Team Roles

| Agent | Role | Merge authority |
|---|---|---|
| **Furnace** | Backend: STG compiler, VM, intrinsics, bytecode | PRs reviewed by Wicket, merged to master |
| **Quill** | Frontend: desugarer, type checker, core, prelude | PRs reviewed by Wicket, merged to master |
| **Wicket** | Gatekeeper: reviews and merges Furnace/Quill PRs | Merges to master after CI green |
| **Clarion** | Proactive: error diagnostics, source locations | PRs reviewed by owner personally |
| **Lantern** | Proactive: tooling, Emacs mode, LSP, VS Code | PRs reviewed by owner personally |

**Branch discipline:**
- No integration branch. All work lands on master via PRs.
- Furnace and Quill work on feature branches (`feat/cg1-direct-app`, `feat/ty-default-on`, etc.).
- BV0 spike works on `spike/bv0-bytecode` — merged to master only on go.
- Clarion and Lantern work on their own feature branches as usual.
- Wicket merges non-proactive PRs. Owner reviews proactive PRs (Clarion, Lantern).

---

## 2. Phasing

### Phase 1 — Independent foundations

**Goal:** land the independent items that unblock later phases.

| Agent | Bead | Work | Branch |
|---|---|---|---|
| **Furnace** | eu-dc72 | CG1: DirectApp node in StgSyn/HeapSyn/ArenaStgSyn, compiler detection, VM fast path | `feat/cg1-direct-app` |
| **Quill** | eu-1nxh | TY: warning inventory across harness + AoC, fix false positives, flip default, add `--suppress-type-warnings`, remove/deprecate `--type-check` | `feat/ty-default-on` |
| **Quill** | eu-luyx | SV optional fields: `FieldPresence` in type repr, parser `?` syntax, subtyping, checker handling, display | `feat/sv-optional-fields` |

**Notes:**
- CG1 and TY are fully independent — no shared files.
- Optional fields is independent type-checker work (no dependency on SV1 for the type-system core — the `t-*` projection and `match?` integration can be added when SV1/SV2 land).
- Quill can work TY and optional fields in sequence or parallel (different files: driver/options vs core/typecheck).

**Wicket:** reviews and merges all three as they arrive. CG1 requires `EU_GC_VERIFY=2` on CI.

### Phase 2 — Second codegen + type-value surface

**Goal:** land CG2 (coordinating blob bump with CG1) and SV1.

| Agent | Bead | Work | Branch |
|---|---|---|---|
| **Furnace** | eu-r92e | CG2: LookupLit node, compiler change in `compile_lookup`, VM fast path with block index/scan logic. **Coordinate blob version bump with CG1.** | `feat/cg2-literal-key` |
| **Quill** | eu-3a9w | SV1: compile-time validation in desugarer via `parse_type`, canonical string normalisation, `t-*` vocabulary definition, `to-data`/`from-data` prelude functions | `feat/sv1-type-data` |

**Notes:**
- CG2 modifies the same files as CG1 (compiler.rs, vm.rs, syntax.rs, arena.rs, loader.rs). Must land after CG1 to avoid conflicts. Single blob version bump covers both.
- SV1 is standalone on Quill's side. The `TYPE_TO_DATA` intrinsic (Furnace) is deferred to Phase 3 — Quill defines the `t-*` vocabulary and the prelude `to-data` wrapper; Furnace implements the Rust intrinsic once the vocabulary is settled.

**Wicket:** reviews and merges. CG2 requires `EU_GC_VERIFY=2`.

### Phase 3 — Recursive call optimisation + specs + intrinsic

**Goal:** land CG3, SV2, and the SV1 intrinsic.

| Agent | Bead | Work | Branch |
|---|---|---|---|
| **Furnace** | eu-r7vr | CG3: detect self-recursive calls in compiler, Seq-wrap all strict args before recursive call. Thread current binding name through compile context. | `feat/cg3-strict-recurse` |
| **Furnace** | (eu-3a9w) | SV1 intrinsic: `TYPE_TO_DATA` — parse BoxedTypeData string, project to `t-*` tagged list. Depends on Quill's vocabulary from Phase 2. | `feat/sv1-type-to-data-intrinsic` |
| **Quill** | eu-amjn | SV2: `to-spec`/`as-spec` prelude function walking `t-*` tagged list to build `match?` patterns. Add missing predicates (`null?`, `datetime?`) if needed. Wire optional-field presence logic into `match?`. | `feat/sv2-as-spec` |

**Notes:**
- CG3 modifies only compiler.rs (no new IR variants, no blob change). No conflict with CG1/CG2.
- SV1 intrinsic is a small Furnace task — one intrinsic impl, registered in the catalogue.
- SV2 depends on SV1 (merged in Phase 2) and benefits from optional fields (Phase 1).
- Quill's SV2 work includes the optional-field `match?` integration (§2.7 of the optional fields spec): `me?` gains the "absent OR matches" logic.

**Wicket:** reviews and merges. CG3 requires before/after tick measurements on the higher-order fold benchmark.

### Phase 4 — Bytecode spike

**Goal:** resolve the BV0 go/no-go gate.

| Agent | Bead | Work | Branch |
|---|---|---|---|
| **Furnace** | eu-3mj5 | BV0: `src/eval/bytecode/` module — opcode encoding, minimal threaded interpreter, dispatch-only benchmark on day11-p1. Measure ≥ 2× dispatch throughput. | `spike/bv0-bytecode` |

**Notes:**
- Self-contained module, no production code changes.
- If go (≥ 2×): merge to master, document measurements, BV1 proceeds in 0.12.
- If no-go (< 2×): archive branch, document measurements in `docs/development/bv0-results.md`, reassess.
- Furnace can start BV0 exploration earlier if CG work completes ahead of schedule.

**Wicket:** reviews. Owner makes the go/no-go decision based on measurements.

---

## 3. Proactive agents

### Clarion (error diagnostics)

Runs proactively throughout 0.11, looking for error improvement opportunities arising from:

- **CG1/CG2:** new `DirectApp`/`LookupLit` nodes may surface new error paths — Clarion checks that `Smid` propagation is correct and diagnostics are clear.
- **TY default-on:** type warnings become visible to all users — Clarion reviews warning prose for clarity and actionability.
- **CG3:** Seq at recursive call sites may change error context when a strict arg fails — Clarion verifies source locations are preserved.

**Scope:** error message improvements, source location propagation, diagnostic clarity. Does NOT add new compile errors that reject previously valid code.

**PRs:** reviewed by owner personally, not Wicket.

### Lantern (tooling, LSP, editor modes)

Runs proactively throughout 0.11, looking for tooling improvements arising from:

- **TY default-on:** LSP may want to surface type warnings differently now they're default. Inlay hints, diagnostic severity.
- **SV1 s-strings:** syntax highlighting for `s"…"` in Emacs mode, VS Code extension, tree-sitter grammar. Hover/completion for type-DSL content inside s-strings.
- **SV optional fields:** `?` suffix highlighting in type annotations.
- **New STG nodes (DirectApp, LookupLit):** `eu dump stg` display formatting.

**PRs:** reviewed by owner personally, not Wicket.

---

## 4. Coordination checklist

| Item | Who | When |
|---|---|---|
| Blob version bump | Furnace (CG1 + CG2) | CG2 PR includes bump covering both variants |
| Demand signature inventory | Furnace (CG1) | Verify completeness in CG1 PR |
| `t-*` vocabulary schema | Quill (SV1) | Document in SV1 PR; SV (optional) extends |
| `match?` optional-field logic | Quill (SV2) | SV2 PR includes `me?` change for optional arms |
| Higher-order fold benchmark | Furnace (CG3) | Before/after measurements in CG3 PR |
| BV0 go/no-go decision | Owner + Furnace | After Phase 4 measurements |

---

## 5. Definition of done (0.11.0 release)

All of the following before version bump:

- [ ] CG1 merged, harness green, `EU_GC_VERIFY=2` passes
- [ ] CG2 merged, harness green, `EU_GC_VERIFY=2` passes
- [ ] CG3 merged, harness green, higher-order fold scales O(n)
- [ ] TY merged, zero spurious warnings on harness + AoC
- [ ] SV1 merged, `s"…"` validates at compile time, `to-data` works
- [ ] SV2 merged, `as-spec` + `match?` validates correctly
- [ ] SV optional fields merged, `name?: T` works in type-DSL
- [ ] BV0 go/no-go decided and documented
- [ ] All proactive Clarion/Lantern PRs reviewed by owner
- [ ] CHANGELOG.md updated
- [ ] `cargo test --test harness_test` green
- [ ] `cargo clippy --all-targets -- -D warnings` clean
- [ ] `cargo fmt --all` clean
