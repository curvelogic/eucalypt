# 0000 — Priority fixes (do before / alongside the proposals)

- **Status:** Living backlog — to be imported into `bd` (beads)
- **Purpose:** High-priority **fixes** (bugs, latent-correctness issues, quick
  high-value wins) surfaced while reviewing proposals 0001–0020. These are *not*
  strategic proposals; they are concrete things that should land **before or
  alongside** the proposal work — typically because a proposal's value depends on
  them, or because they are cheap correctness/latency wins worth banking first.

Each entry is written **bead-ready** (title, priority, type, description,
acceptance criteria, references, relationships) so it can be imported directly.
The `F`-numbers are local handles for this document only; real IDs come from `bd`
on import.

---

## F1 — Eliminate the double STG compile of plain documents (GC-safe in-place render)

- **Priority:** P1 / high
- **Type:** bug (performance + GC correctness)
- **Status:** open

**Description.** For a plain (non-IO) document — the common config/data case —
`try_execute` compiles the prelude-plus-unit evaluand to STG **twice**: once
headless to classify IO-vs-document, then again under `RenderType::RenderDoc` on
a *fresh* machine to render (`src/driver/eval.rs:178–295`). The second compile
(~90 ms including the prelude, and not even timed) exists **only** as a
workaround: rendering the headless result in place via `render_headless_result`
(`src/driver/io_run.rs:1602`) crashes the GC on stale string pointers left by the
first run (`eval.rs:272–275`). The IO paths (cases 1/2) already render in place
safely; only the pure-document path (case 3) recompiles. Fix the stale-string
hazard so the pure-document result can render in place like the IO cases →
**one compile, one machine** on every plain-document run, and a known
heap-lifecycle landmine removed.

**Acceptance criteria.**
- A plain-document run performs exactly **one** STG compile of the evaluand
  (verifiable via the `stg-compile` timing; today the second compile is
  untimed).
- The headless pure-document result renders **in place** — no `RENDER_DOC`
  recompile, no fresh machine.
- No GC crash under `EU_GC_VERIFY=2` + `EU_GC_STRESS=1` for plain-document
  renders across the harness.
- Regression test: a plain-document harness case exercised under GC verification.

**References.** `src/driver/eval.rs:163–302` (`try_execute`, the three-case
flow), `:272–295` (the recompile workaround and the stale-string comment);
`src/driver/io_run.rs:1602` (`render_headless_result`, the in-place renderer that
crashes for pure documents); proposal [0004](0004-compiled-unit-caching.md)
(prelude/compiled-unit caching is *complementary* — it speeds the remaining
single compile across invocations; it does **not** eliminate the double compile,
this fix does).

**Relationships.** relates-to 0004 (caching); relates-to 0005 / 0020 (shares
heap/GC-lifecycle expertise). Not blocked by any of them — this is a standalone
fix that can land first.

---

## F2 — Block-style idiot brackets are per-file (don't compose across imports)

- **Priority:** P1 / high
- **Type:** bug (correctness — silent mis-parse across the import boundary)
- **Status:** open

**Description.** Whether a user bracket pair `⟦ … ⟧` parses its contents as a
**block** (declarations) or a **soup/expression** is decided at **parse time**
from a **per-file** `BracketRegistry`, populated by a pre-scan of *the same
file's* tokens for `⟦{}⟧: …` / `(⟦{}⟧): …` declarations
(`prescan_bracket_declarations`, `src/syntax/rowan/parse.rs:45,100`; consulted at
`:516-518`). There is **no cross-file seeding** of this registry — in pointed
contrast to the monad-namespace registry, which *does* expose
`seed`/`drain_monad_namespace_registry` (`src/core/desugar/desugarer.rs:171-177`).
Because each file is parsed independently and imports are resolved *after* parse,
a block-style bracket pair **defined in an imported file** is invisible to the
importing file's pre-scan, so its uses there default to soup/expr mode — a wrong
parse or error. A program that works with the definition and its uses in one file
**silently breaks** when the definition is moved to a library and imported. This
violates the import model (definitions should compose across files) and is a
silent correctness hazard — it is a bug *today*, independent of separate
compilation.

**Repro (shape).** `lib.eu` declares a block bracket (`⟦{}⟧: …`); `main.eu` does
`{ import: "lib.eu" }` and uses `⟦ a: x  b: y ⟧`; the use mis-parses in `main.eu`
because its pre-scan never saw `lib.eu`'s declaration. Capture as a concrete
failing case in the bead.

**Fix directions** (the deciding info is cross-file, but the decision is at parse —
a phase-ordering problem):
- **(a)** Make parse import-aware for bracket declarations — but parse currently
  precedes import resolution, so this is a phase change.
- **(b)** *Defer the block-vs-soup decision past parse* to an import-aware stage
  (desugar), parsing bracket contents generically and resolving content-mode
  against a *seedable* registry — the mechanism the monad namespaces already use
  (`desugarer.rs:171`). Cleanest, and aligns with separate compilation.
- **(c)** Signal block-mode syntactically at the use site (no registry).

**Interaction with [0021](0021-separate-compilation.md).** This is a *parse-time*
cross-unit dependency. Fix (b) keeps brackets out of the separate-compilation
"wall" (the decision moves to the seedable desugar registry); fix (a) would make
bracket content-mode a parse-time wall — the hardest kind. The fix choice and the
separate-compilation architecture are therefore coupled; prefer (b).

**References.** `src/syntax/rowan/parse.rs:45,100,500-525`;
`src/syntax/rowan/brackets.rs:35-52`; `src/core/desugar/desugarer.rs:171-177` (the
analogous seedable registry brackets lack).

**Relationships.** relates-to 0021 (fix direction couples to separate
compilation); independent of F1.

---

<!-- Add further high-priority fixes below as the review surfaces them. -->
