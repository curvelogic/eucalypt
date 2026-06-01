# Authoring guide for eucalypt evolution proposals

**Audience:** anyone (human or agent) drafting a document in `docs/proposals/`.
**Status:** internal authoring guide, not itself a proposal.

These documents are RFC-style strategic proposals for how eucalypt should
evolve over the next few minor versions on the way to **v1.0** and beyond.
They exist to be *reviewed by the maintainer when deciding the next stages of
the language's evolution after 0.7* (which delivers HKT). They are arguments,
not commitments. Each should leave the reader better able to make a yes/no/defer
decision.

Read this whole file before drafting. Read `README.md` (the portfolio index)
for how your proposal sits among the other nineteen.

---

## 1. The non-negotiables

Every proposal must respect these, or must argue *explicitly and at length*
why an exception is justified (the bar is high; assume the answer is "honour
them"):

1. **Syntactic conservatism.** Eucalypt's surface syntax stays as it is. New
   language machinery lives in **metadata** (backtick blocks: `type:`, `monad:`,
   `export:`, …), in **symbols**, in **strings**, in **blocks**, in
   **operators**, or in **idiot-bracket pairs** — never in new keywords,
   statement forms, lambda arrows, or `let … in`/`match … with` expressions.
   See `docs/understanding/philosophy.md` and
   `docs/development/type-system-evolution.md` §3.
2. **Structural over nominal.** No named classes, no `implements` clauses, no
   nominal types. Constraints reference shapes and functions, not names.
   ("Typeclasses without classes.")
3. **Gradual and inference-first.** Types are advisory by default; a feature
   must deliver value when only the prelude is annotated. The default boundary
   policy is silent (`any → T` trusted), not blame-tracked.
4. **The runtime reality.** Evaluation is lazy, pure, and **single-threaded**
   over an `UnsafeCell` heap whose soundness depends on stop-the-world,
   single-threaded access (`src/eval/memory/heap.rs:8`). Blocks are cons-lists
   with O(n) lookup; persistent O(log n) blocks are blocked on a GC-finalisation
   problem (ADR-001, `docs/development/architectural-decisions.md`). IO is an
   explicit monad interpreted by a driver loop; the pure core performs no
   effects. Any proposal touching these must engage the constraint honestly.
5. **Tool-first.** Eucalypt is a tool for generating/templating/transforming
   YAML/JSON/TOML before it is a language. Its peers are configuration/data
   languages, not general-purpose languages. Weigh every idea against that use.
6. **UK English** throughout (optimise, behaviour, colour, normalise, …).

If your proposal *is* one of the deliberate forks in the road (e.g. an MLsub
rebuild, or post-1.0 concurrency), say so plainly and frame it as a decision,
not a foregone conclusion.

---

## 2. Document template

Use these sections, in this order, as `## ` headings. Aim for **~2,000 words**
(acceptable range 1,500–2,800). Open with a short YAML-ish front-matter block
exactly as below.

```
# NNNN — Title

- **Status:** Draft proposal for review
- **Track:** A/B/C/D/E — <track name>
- **Classification:** Extends-Roadmap | Whitespace | Stage-C-Fork | Process
- **Suggested horizon:** 0.8 | 0.9 | 1.0 | post-1.0
- **Related:** H<n> (type-system-evolution.md), TS-<bead>, ADR-00n, sibling proposals NNNN
```

Then:

1. **Summary** — 3–5 sentences. What is proposed, and why it matters for 1.0.
   A reader who stops here should still get the thesis.
2. **Motivation** — the concrete problem, grounded in eucalypt *as it is today*,
   with `path:line` references. Quantify where you can (cite the recon facts:
   e.g. compile latency ~500–700 ms; GC mark >95 % of traversal-heavy VM time;
   181 intrinsics; O(n) block lookup). Name the user pain or strategic gap.
3. **Prior art & landscape** — how peer languages and the PLT literature handle
   this. Compare against the relevant subset of {Jsonnet, Dhall, CUE, Nickel,
   Pkl, KCL, Starlark, Unison} and cite named papers/systems. Be specific about
   what eucalypt should and should not borrow, and why.
4. **Proposed design** — the concrete proposal. Show eucalypt examples in real,
   valid syntax (respect the gotchas in §4 below). Where machinery is added,
   show it living in metadata/symbols/operators. Be precise about semantics.
5. **Interaction with the existing roadmap** — relate to Stage A/B/C and the
   relevant H-hypotheses and TS-beads. State dependencies, what it supersedes,
   and any conflicts. Do not duplicate work already specced — build on it.
6. **Implementation sketch** — which components change (`src/core/...`,
   `src/eval/...`, `src/driver/lsp/...`), rough size/risk, and a sequencing
   into phases. Honest about cost.
7. **Alternatives considered** — the roads not taken, and why rejected or
   deferred.
8. **Risks & what would kill this** — the failure modes, the empirical results
   that would falsify the bet, and the maintainer open-questions it touches.
9. **Success criteria** — how we would know it worked (benchmarks, bug classes
   caught, adoption signals, latency targets, …).
10. **References** — eucalypt files cited, papers, and peer-language docs.

Cross-link sibling proposals by number and slug, e.g.
"see [0007 — type-directed compilation](0007-type-directed-compilation.md)".

---

## 3. Voice and evidence

- Precise, technical, and *argued*. Every claim earns its place. No marketing
  language, no "revolutionary", no padding.
- **Honest about cost and risk.** A proposal that hides its downsides is
  useless for decision-making. Where a thing is hard or speculative, say so.
- **Verify eucalypt claims against the code.** Do not trust this guide or the
  README for specifics — open the file and cite `path:line`. If you assert a
  behaviour, confirm it: build once (`cargo build --release`) and run
  `timeout 60 ./target/release/eu …` to check. Never invent prelude functions
  or syntax.
- **Cite external claims.** Name the paper, language, or system. The reading
  lists in `docs/development/type-system-evolution.md` §6 and the peer-language
  set are good starting points; web research is encouraged for currency.
- Prefer tables and short code blocks to long prose where they carry the
  argument better.

---

## 4. Eucalypt syntax facts you must not get wrong

(See `docs/reference/agent-reference.md`, `docs/appendices/syntax-gotchas.md`.)

- Catenation `x f` = `f(x)`; it is the **lowest-precedence** operator (20). All
  infix operators bind tighter. Use parentheses or named bindings.
- `.` is lookup at precedence 90 (very tight). `list head.name` parses as
  `list (head.name)`.
- **No lambda syntax.** `->` is `const`. Use sections `(+ 1)`, expression
  anaphora `(_ + 1)` (each `_` is a *new* parameter; reuse `_0`), or named
  functions.
- Backtick `` ` `` attaches **metadata** to the next declaration; `#` is the
  comment character.
- Type annotations are **strings** inside `type:` metadata, in a separate type
  DSL parsed by `src/core/typecheck/parse.rs`. Inside those strings records use
  escaped braces `{{..}}`.
- `f(x)` is a call; `f (x)` is catenation. No space before `(`.
- Many "obvious" functions do not exist (`abs`, `even?`, `flatten`,
  `str.trim` at the prelude level, …). Check before using one in an example.

---

## 5. Classification & horizon conventions

- **Extends-Roadmap** — deepens or sequences something already in the H/TS
  roadmap.
- **Whitespace** — addresses an area with no current roadmap entry.
- **Stage-C-Fork** — a "radical option" / decision-in-the-road; argue both
  sides, recommend, but frame as the maintainer's call.
- **Process** — about how 1.0 is defined, governed, tested, or released.

Horizons are *suggestions* for the maintainer, not schedule commitments.
Post-1.0 items are legitimate (the brief explicitly asks what 1.0 needs *and*
where the language goes after) but must be tagged as such.

---

## 6. The shared factual baseline (verify before relying on any of it)

A compressed digest of the reconnaissance, for orientation only — **confirm
each point in the code before citing it**:

- Current version **0.6.1**. 0.6.2 = type-system Phase A (specced, not
  implemented). 0.7 = Phase B incl. HKT (specced). All "in-flight" type work is
  **documentation only** today.
- Pipeline: parse (LALRPOP + a newer Rowan parser) → desugar → cook (shunting
  yard) → eliminate → inline ×2 → fuse → eliminate/compress → verify →
  **[type-check, advisory, erased]** → STG compile → STG optimise → load to
  heap → STG VM.
- VM: STG machine, `src/eval/machine/vm.rs`; 5 continuation kinds; off-heap
  `Vec` continuation stack; ~181 intrinsics; single worker thread w/ 64 MiB
  stack.
- GC: Immix-inspired, 32 KiB blocks / 128 B lines, mark-and-sweep **with**
  opportunistic evacuation **and** lazy sweep (the `gc-implementation.md` doc is
  **stale** in claiming otherwise). Mark cost dominates real workloads. Trigger
  is crude (every 500 steps vs a block-count limit). Excellent verify/stress
  debug infrastructure (`EU_GC_VERIFY`, `EU_GC_STRESS`, `EU_GC_POISON`).
- Type checker: bidirectional, freshen-and-unify with subtyping; advisory;
  `src/core/typecheck/` (8 files); rank-1 prenex; record/row representation
  present; `TypeScheme.constraints` reserved-but-unused.
- Tooling that already exists (do **not** propose greenfield): LSP (hover,
  completion, nav, diagnostics, inlay hints, code actions, folding, formatting,
  incremental sync), tree-sitter grammar, Emacs mode, VS Code extension, WASM
  API, CodeMirror browser playground (separate repo).
- The maintainer's seven open questions live in
  `docs/development/type-system-evolution.md` §5 — engage the relevant ones.

---

## 7. Practical

- Filenames: `NNNN-kebab-slug.md`, number per the README table.
- Keep each document self-contained but cross-linked.
- One commit (or small batch) per track; descriptive messages; no PR unless
  asked.
