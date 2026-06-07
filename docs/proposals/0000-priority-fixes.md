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

**Interaction with [0004](0004-compiled-unit-caching.md)'s separate compilation.** This is a *parse-time*
cross-unit dependency. Fix (b) keeps brackets out of the separate-compilation
"wall" (the decision moves to the seedable desugar registry); fix (a) would make
bracket content-mode a parse-time wall — the hardest kind. The fix choice and the
separate-compilation architecture are therefore coupled; prefer (b).

**References.** `src/syntax/rowan/parse.rs:45,100,500-525`;
`src/syntax/rowan/brackets.rs:35-52`; `src/core/desugar/desugarer.rs:171-177` (the
analogous seedable registry brackets lack).

**Relationships.** relates-to 0004 (fix direction couples to its separate
compilation work); independent of F1.

---

## F3 — Unify cross-unit compilation contributions into a single Unit Interface

- **Priority:** P2 / medium (code-cleanliness refactor; subsumes F2; foundation for 0004's separate compilation)
- **Type:** refactor / architecture
- **Status:** open

**Description.** A dependency makes several *source-level contributions* to the
compilation of a dependent unit, currently handled by **four separate,
inconsistent mechanisms** across phases:

| Phase | Contribution | Today |
|---|---|---|
| Parse | bracket content-modes (block/soup) | per-file `BracketRegistry`, no seeding (**F2 bug**) |
| Desugar | monad namespace registry | `seed`/`drain_monad_namespace_registry` (`desugarer.rs:160-177`) |
| Cook | operator table (fixity/precedence) | rediscovered from the merged tree, no seeding |
| Typecheck | schemes, aliases, branch shapes, **operator overloads** | `PreludeSummary` + `with_seed` (`check.rs:197,382`) |
| Link | exported binding names → slots | not present (0004 separate-compilation territory) |

Some are seedable, some per-file, one is a bug — and **operators are captured
twice** (in `PreludeSummary` for typecheck *and* rediscovered at cook). Refactor
these into a single per-unit **Unit Interface** (à la a GHC `.hi` / ML signature;
`PreludeSummary` is already a partial one) with one consistent pattern: *build a
unit's interface once; seed each phase of a dependent from its dependencies'
interfaces.*

**Independently valuable** (before any separate-compilation work): one mechanism
instead of four; removes the operator-table redundancy (extract once, serve both
cook and typecheck); **fixes F2** (brackets gain a slot and the seeding they
lack); and makes the cross-unit surface explicit and testable.

**Also the shared foundation later:** separate compilation and caching
([0004](0004-compiled-unit-caching.md)) and incremental tooling
([0014](0014-incremental-query-core.md)) consume exactly this interface.

**Design wrinkles.** Parse is the awkward phase — desugar/cook/typecheck run
*after* import resolution (clean to seed), but parse runs first, so the
bracket-mode contribution needs F2 fix (b) (defer the block/soup decision past
parse) to join the interface cleanly. Interfaces build bottom-up over the import
DAG (the diamond-dedup graph exists); mutual recursion *across* units is the
residual hard case.

**References.** `src/syntax/rowan/parse.rs:45` (brackets);
`src/core/desugar/desugarer.rs:160-177` (monad seed/drain);
`src/core/cook/fixity.rs:143` (operator rediscovery);
`src/core/typecheck/check.rs:197,382` (`PreludeSummary`/`with_seed` — the partial
interface to generalise).

**Relationships.** subsumes F2's fix; foundation for 0004 (separate compilation/caching) and 0014;
independent of F1.

---

## F4 — Unify the thunk/update/strictness heuristics behind one demand annotation

- **Priority:** P2 / medium (code-cleanliness refactor; foundation for 0006's analysis; complements F3)
- **Type:** refactor / architecture
- **Status:** open

**Description.** Eucalypt has ~half a dozen separate, hand-rolled mechanisms that
all decide *`Value`-vs-`Thunk` / skip-`Update`* from facets of one question — *is
this binding used-at-most-once / strict / already-WHNF?*:

| Mechanism | Decides | Where |
|---|---|---|
| `LambdaForm::Value` vs `Thunk` | the underlying distinction | `src/eval/memory/syntax.rs:747` |
| per-intrinsic `strict_args` | hand-listed args to force before a BIF | `src/eval/intrinsics.rs:15,27` |
| per-intrinsic `single_use_args` | hand-listed args to compile as `value` | `src/eval/machine/intrinsic.rs:124` |
| `suppress_update`/`suppress_next_update` | bespoke `IF`→`Case`→`Branch` threading to skip `Update` | `src/eval/machine/vm.rs:276-431`, `cont.rs:49` |
| `is_whnf` | "already a value" check | `src/eval/memory/syntax.rs:284` |
| `--suppress-updates` | global call-by-name escape hatch | `src/eval/stg/mod.rs:286` |

Two are *per-intrinsic hand lists*; one is bespoke threading for `IF` alone.
Unify them behind **one demand annotation per binding** (cardinality + strictness
+ WHNF) as the single representation the `Value`-vs-`Thunk` and update-suppression
decisions consult. The existing heuristics populate it today; a real demand
analysis ([0006](0006-strictness-analysis.md)) populates it more completely later.

**Independently valuable** (before any analysis): one decision point instead of
six — testable and consistent. The strictness/update analogue of F3's registry
unification.

**Interaction with separate compilation ([0004](0004-compiled-unit-caching.md)).**
The demand annotation on an *exported* binding is a **strictness signature** a
dependent needs to optimise cross-unit calls — exactly what GHC ships in `.hi`. So
this annotation is another field of the **Unit Interface** (F3). *Not* a wall:
unlike operators, a missing signature only costs optimisation (the boundary
degrades to conservative-but-correct).

**References.** the table above; `src/eval/stg/compiler.rs:924-952` (`single_use`
propagation); `src/eval/stg/boolean.rs:133-159` (`If` `single_use_args`).

**Relationships.** foundation for 0006; complements F3 (its annotation is interface
payload for 0004); subsumes the listed heuristics; independent of F1.

---

## F5 — Restore git imports (regression; dropped in the Haskell→Rust rewrite)

- **Priority:** P1 / high (documented feature absent; silent mis-behaviour; a
  third-party-sharing gap at exactly the moment 1.0 invites it)
- **Type:** regression / feature restoration
- **Status:** open

**Description.** `{ import: { git: …, commit: …, import: … } }` is **documented
as working** (`docs/guide/imports-and-modules.md:183-207`,
`docs/reference/import-formats.md:87-114`) but does nothing in the Rust
implementation. It was a *real, shipped feature of the Haskell eucalypt* —
added 2019-03-05 (PR #115, `6e56dc5c`, "Add import direct from git repo
capability": `src/Eucalypt/Driver/Git.hs` cloned a repo at a commit into a
local cache under `.eucalypt.d`) — and was dropped wholesale in the rewrite
("Remove Haskell implementation", 2021-05-10 `6217817f`), never re-ported. The
docs survived the rewrite (the "doc fixes for rust impl" PR missed this
section), so the guide has promised a non-existent feature for ~4 years. Today
`scrape_rowan_imports` (`src/syntax/import.rs:259-293`) matches only string and
list elements; a `{ git: … }` block is an `Element::Block` and is **silently
dropped** (the `_ => {}` arm), so the import never happens and the user gets a
downstream "unbound name", not a clear "git imports unsupported". There is no
`Git` variant in `Locator` (`src/syntax/input.rs:17`), no git dependency in
`Cargo.toml`, and the `Url` locator parses but the loader has no fetch arm for
it either.

**Repro.** A file does `{ import: { git: "https://github.com/u/r", commit:
"<sha>", import: "lib.eu" } }` and uses a binding from it; the binding is
unresolved, despite the guide saying this works.

**Fix — restore it *consistent with [0018]*, not as the bare 2019 form.** Don't
just re-port the Haskell mechanism; implement git as a **fetch backend beneath
a content hash**, which is exactly [0018]'s git-only distribution model:

- teach `scrape_rowan_imports` the **block form** (the same change [0018] (a)
  and F3 also need) — read `git:`/`commit:`/`import:` plus an optional
  `sha256:`;
- add a git-capable locator + clone-and-cache at the pinned commit (the Haskell
  `.eucalypt.d` cache is the reference design), keyed by commit SHA;
- verify the optional `sha256:` **content** hash on the fetched bytes ([0018]
  (a)): the commit SHA pins the ref; the content hash additionally defends
  against a force-push / rewritten history a bare ref cannot.

This makes the regression-fix the **foundation [0018] builds on** rather than a
throwaway: [0018]'s manifest/lockfile (b) and MVS (d) then layer over a git
backend that already exists and is already hash-verified. One move restores both
the docs' promise and the [0018] approach.

**Interaction.** Implements the fetch half of [0018] (d) and the integrity half
of [0018] (a); shares the block-scraper change with F2/F3; the unwired `Url`
locator is the same loader gap and can be closed alongside. Until this lands the
git-import docs are actively wrong — either fix the code (this F5) or, as an
interim, mark the docs not-yet-implemented.

**References.** `src/syntax/import.rs:259-293` (`scrape_rowan_imports`, block
ignored); `src/syntax/input.rs:17` (`Locator` — no `Git`; `Url` parses but
unwired in `src/driver/source.rs`); `docs/guide/imports-and-modules.md:183-207`,
`docs/reference/import-formats.md:87-114` (the stale promise); Haskell prior art
`6e56dc5c` (`src/Eucalypt/Driver/Git.hs`), removed in `6217817f`;
[0018](0018-module-package-system.md) §(a),(d).

**Relationships.** regression of the Haskell feature; foundation for 0018
(d)/(a); shares the block-scraper change with F2/F3; relates-to the `Url`-locator
gap.

---

## F6 — Generalise `vec` to hold arbitrary values (GC-traced `Array<Closure>`)

- **Priority:** P2 / medium (expressiveness limit + a minor latent leak; gated on
  whether random-access-over-records is a near-term target)
- **Type:** enhancement (expressiveness) + bug (latent GC leak)
- **Status:** open

**Description.** `vec` — the only O(1)-indexed sequence — can hold **only
primitive scalars** (`Num`/`Str`/`Sym`), because its backing is `Vec<Primitive>`
(`src/eval/memory/vec.rs:14`; `Primitive` at `src/eval/memory/set.rs:20`) and
`VEC.OF` rejects anything else (`extract_primitive` panics,
`src/eval/stg/vec.rs:86`; `native_to_set_primitive` → `TypeMismatch`,
`src/eval/stg/support.rs:530`). So you **cannot** get indexed/random access to a
sequence of **records (blocks) or nested lists** — the canonical "array of
objects" shape of CSV/JSON/YAML inputs — and must fall back to O(n) cons-lists.

The reason "non-primitive" is non-trivial: there is **no
`Native::Block`/`List`/`Closure`** (`src/eval/memory/syntax.rs:37–58`) — blocks,
lists and lambdas are `HeapSyn` *closures with environments*, not native values.
So an element cannot be a richer `Native`, nor a bare `Ref` (which needs an env);
it must be a **closure** (code + captured env), exactly as list elements are. The
right backing is therefore a GC-managed **`Array<Closure>`**, not `Array<Ref>`.

**This is well-precedented.** A GC-traced, evacuation-safe array of heterogeneous
closures already exists as the **environment frame** (`EnvFrame { bindings:
Array<C> }`, `src/eval/machine/env.rs:196`; `impl GcScannable`, `:498`), and
`HeapString` is the template for a native that owns a GC-managed backing and is
properly traced (`src/eval/memory/string.rs`; pushed for scanning at
`syntax.rs:333`). The change is essentially "re-shape `HeapVec` to follow
`EnvFrame`."

**Secondary benefit (a real fix).** Today `Native::Vec` is *marked but never
scanned* (`syntax.rs:343,380` — only `marker.mark`), and its `Vec<Primitive>`
buffer (plus contained `String`s) lives on the **Rust heap** — so a reclaimed
vec's backing **leaks** (the bump allocator never runs `Drop`), the same class as
ADR-001/0020, tolerated only because vecs are rare. Moving to a GC-managed
`Array` *fixes* that leak.

**Surface (localised; sets, ndarrays, and the collector core untouched).**
- `vec.rs`: `Vec<Primitive>` → `Array<Closure>`; small API rewrite.
- `syntax.rs`: **(correctness-critical)** push `Native::Vec` for scanning and
  handle it in the forward-pointer update (today both only mark, `:343`/`:380`);
  add `impl GcScannable for HeapVec` mirroring `EnvFrame`.
- `stg/vec.rs`: rework the 7 intrinsics — **most simplify** (the `Primitive`
  round-trip goes): `VEC.OF` stores the head `SynClosure` it already resolves
  (`:151`) instead of extracting a primitive (and can drop the `SeqList`
  deep-force); `VEC.NTH` returns the stored closure (no `Boxed*` re-wrap,
  `:247–257`); `VEC.TO_LIST` drops the box-tag match (`:469–479`);
  `SLICE`/`SAMPLE`/`SHUFFLE` permute element handles.
- `emit.rs`: `emit_vec` routes each element through the normal per-element value
  render (as lists do), not `set_primitive_to_render_primitive` (`:105–118`).

**Semantics decisions (none blocking).**
- *Strictness:* store source element closures as-is and drop `VEC.OF`'s
  deep-force → vec inherits list laziness; O(1) is about *index*, not evaluation.
- *Equality:* `Native::Vec` stays pointer-identity (`syntax.rs:71`, unaffected);
  the element-compare `HeapVec: PartialEq` is test-only; structural value-`=` over
  vecs is out of scope.
- *Render:* a vec renders as a sequence of rendered values.

**Acceptance criteria.**
- A vec can be constructed from, and round-trip (`vec.of` → `vec.to-list`), a list
  of **blocks** and of **nested lists**, preserving order and element identity.
- `vec.nth` over a vec of blocks returns the block (indexable further) in O(1).
- A non-primitive vec renders identically to the equivalent list across the
  harness (golden output).
- Full harness passes under `EU_GC_VERIFY=2`, `EU_GC_POISON=1`,
  `EU_GC_STRESS=1`; **a use-after-free under poison falsifies it.**
- GC-churn benches show no regression; the prior Rust-heap vec-backing leak is
  gone.
- Regression test: a vec-of-records program exercised under GC verification.

**References.** `src/eval/memory/vec.rs:14`; `src/eval/memory/set.rs:20`;
`src/eval/memory/syntax.rs:37–58,71,333–336,343,380`;
`src/eval/machine/env.rs:196,498` (the `Array<Closure>` template);
`src/eval/memory/string.rs` (GC-traced-native template);
`src/eval/stg/vec.rs:86,151,247–257,469–479`; `src/eval/stg/support.rs:530`;
`src/eval/stg/emit.rs:105–118`; `lib/prelude.eu:1949–1990` (vec API).

**Relationships.** relates-to 0020 (shares the GC-scannable-array machinery; it
**refines** 0020's note, which mis-stated the backing as `Array<Ref>` — it must be
`Array<Closure>`); **independent** of 0020's CHAMP work (can land before or
after); independent of F1–F5. Gating question: worth banking now if **random
access over arrays-of-records** is a near-term target; otherwise a clean post-1.0
enhancement.

---

<!-- Add further high-priority fixes below as the review surfaces them. -->
