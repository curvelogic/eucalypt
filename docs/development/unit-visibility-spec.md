# Spec: Unit-Level Declaration Visibility — `export: :internal`

**Status**: Specification — ready to implement.
**Date**: 2026-05-20
**Branch**: `type-system-exploration`

**Scope note**: This is a units / imports / visibility feature. It is
*independent of the type-system roadmap* (TS-A / TS-B) and earns its own
bead. The branch is shared only because that is where current design
work lives.

## Overview

A eucalypt **unit** is a flat list of declarations forming a block
(`Unit { meta, declarations }`, `src/syntax/ast.rs`). Every declaration
is simultaneously a *field* of the unit's block value and *in scope* for
the unit's other declarations — those are the same thing. There is
consequently no way for a library unit to mark a binding as
*implementation*: a helper the unit uses internally but that should not
be part of its external surface.

Today such helpers leak. An importer or merger of the library sees the
helpers in its namespace. Lexically frozen references (§Background) mean
this does not *break* the library, but it pollutes the importer's
namespace and creates ambiguity when two units contribute same-named
helpers.

This spec adds **per-declaration visibility control**: a declaration
marked `export: :internal` stays usable within its own unit but is
removed from the unit's external surface — invisible to importers and
mergers, and absent from rendered output. It is a small, contained
change: one new metadata value, three consult sites.

A *region* form — marking a whole block of helpers private at once — is
**deferred** (§Deferred). This spec is the per-declaration mechanism;
the region form depends on nothing here and can follow later.

## Background — verified facts

All of the following were confirmed by reading the code during design.

- **A unit desugars to `let <bindings> in <block-value>`** — a `let`
  nest binding every declaration, with a body that is the block value
  `{field: …, …}` re-listing them.
- **Composition is uniformly `rebody`** (`src/core/expr.rs:548`,
  `rebody_int`):
  - **Bare import**: `src/core/desugar/rowan_ast.rs:2638` (also :2763,
    :2932) — `imports.iter().rfold(expr, |acc, import|
    import.rebody(acc))`. The imported unit's `let`-nest is spliced as
    an enclosing scope around the importing unit; the imported unit's
    block value is discarded.
  - **Named import** (`name=file.eu`): `translate_import` calls
    `apply_name` (`expr.rs:527`) → `let name = <unit-expr> in …`, then
    the same `rebody` splice — contributing a single binding `name`
    whose value is the unit (its block value, reached `name.field`).
  - **CLI merge**: `merge_in` (`expr.rs:1073`) *is* `self.rebody(other)`.
- **References are lexically frozen.** Name→variable resolution happens
  once, at desugar time (de Bruijn); `rebody`/merge never re-resolve. A
  unit's references to its own bindings are bound de Bruijn variables.
- **Rendering** (`src/eval/stg/render.rs:332–340`): the renderer looks
  up the `export` key in a binding's metadata, defaults it to `:normal`,
  and suppresses the binding from output iff it equals `:suppress`.
- **`export:` today** has one recognised value, `:suppress` (output
  suppression only); the bare-symbol shorthand `` ` :suppress ``
  normalises to `` ` { export: :suppress } `` (`src/core/metadata.rs:44–48`).
  The implicit default is `:normal`.

## The design — `export:` as a three-value enum

`export:` declares how a binding participates in its unit's **external
surface** — the two channels by which a unit is consumed: *rendered
output*, and *visibility to importers / mergers*.

| value | rendered to output | visible to importers / mergers |
|-------|--------------------|-------------------------------|
| `:normal` (default) | yes | yes |
| `:suppress` | **no** | yes |
| `:internal` (new) | **no** | **no** |

`:normal` is the implicit default. `:suppress` keeps its **exact
current meaning** — output-suppressed, importer-visible — which is the
right description of a public *data* export that should not litter an
importer's rendered output. `:internal` is the new value: a binding in
neither channel — a genuine unit-private implementation binding.

The fourth logical combination — rendered but importer-private — is not
offered: it is not meaningful. A rendered field *is* a field of the
unit's block value, and a field is therefore reachable by a named
importer; you cannot render something and hide it from import.

`:internal` is **purely additive**: `:suppress` and `:normal` are
unchanged, so no existing unit's behaviour changes and no migration is
forced.

## Mechanism

A binding marked `export: :internal` produces **two exclusions** from
one marker, and **remains a live `let` binding** throughout.

### M1. The marker, and its core representation

`export: :internal` is binding metadata. It must **persist into the
core** as `Meta` on the binding's bound expression. Metadata already
survives desugaring this way — the renderer reads `export` from core
metadata at render time (`render.rs:332–340`), which is only possible
because it is carried there. Persistence is required because the
capture exclusion (M3) is consulted by `merge`, which runs *after*
desugaring, and must hold *transitively* (§M6).

Add the bare-symbol shorthand `` ` :internal `` → `` ` { export:
:internal } ``, mirroring the existing `:suppress` shorthand
(`metadata.rs:44`).

### M2. Exclusion from the block value

When a unit/block is desugared to `let <bindings> in <block-value>`, the
block-value field list **omits** `:internal` bindings. The binding stays
in the `let`; only the `{…}` body drops it as a field.

This is **index-safe**: removing a `(key, value)` entry from the block
value does not touch the `let` or any de Bruijn index — the remaining
fields' variables still point at the same `let` slots.

M2 handles a *named* import (`name.internal` is not a field → lookup
fails) and **rendering** (a binding that is no block field is never
rendered).

### M3. Exclusion from `rebody` capture — position-preserving

Bare imports and CLI merge splice a unit's `let`-bindings into the
consumer's scope via `rebody`. `rebody_int` (`expr.rs:548`) collects
every binding name of each `Let` scope — at `prepare_capture_vars`
(`expr.rs:551`) and the `names` list (`expr.rs:560`) — and uses them to
**capture** the consumer's free variables via `bind_free_vars`.

`:internal` bindings must be excluded from that capture. **But the
exclusion must not be a naive list-shortening.** `bind_free_vars`
(`expr.rs:1306–1320`) assigns the de Bruijn `binder` index as the
*position of the matched name in the `names` slice*
(`binder: i`, `i = names.iter().position(…)`). Dropping an internal
name from `names` would shift every binding after it by one and
mis-resolve the consumer's *other* (public) references.

So the exclusion is a **position-preserving mask**: the `names` array
keeps one slot per pattern entry, in order, and internal slots are
marked non-capturable. Concretely, either:

- extend `bind_free_vars` to take a capturable mask (`&[bool]` aligned
  with `names`), binding a free var only at a capturable slot while
  still indexing by full position; or
- have `rebody` pass `names` with internal slots replaced by a
  non-matching placeholder (a string no variable can be named), so
  `position` still yields the true index and no free var ever matches
  an internal slot.

Either keeps every public binding's index correct and simply never
binds a free reference to an internal name. The consumer's reference to
an internal name then stays *free* and fails as an unresolved variable
— the correct private outcome. The `prepare_capture_vars` / `env` site
(`expr.rs:551`, feeding `substs_free`) carries no index and may simply
omit internal names.

`rebody` is the single splice chokepoint for bare import, named import
and merge, so this one masked-capture change covers all three
composition paths.

### M4. Rendering

Widen the renderer's suppression test (`render.rs:339`) from
`export == :suppress` to `export ∈ {:suppress, :internal}`. This is
strictly redundant given M2 (an `:internal` binding is never a block
field, so never reaches the renderer as one) but it is explicit, cheap,
and robust against any other path to the renderer.

### M5. Soundness — why this is clean

The exclusion is *surgical*, for a structural reason. `rebody_int` does
`scope.pattern.clone()` — the binding list is copied **verbatim**,
internal bindings included — so the internal binding keeps its slot and
the de Bruijn layout is unchanged. `rebody` recurses only into
`scope.body`, **never into the bound expressions**, so a unit's own
declaration bodies pass through untouched.

This splits cleanly along the **bound / free** line:

- A unit's *own* references to its internal binding are **bound de
  Bruijn variables**, frozen at the unit's desugaring, inside the
  cloned-intact `pattern`. `rebody` never touches bound variables —
  `bind_free_vars` is chosen precisely to leave existing indices alone
  (`expr.rs:556–559`).
- A *consumer's* reference is a **free variable** awaiting capture.

M3 acts only on the capture of *free* variables. Bound-versus-free is
exactly the "unit's own use" versus "consumer's use" boundary, so
excluding `:internal` from capture stops only the consumer and cannot
affect the unit's own use. The binding is never removed, only
un-captured — that is what makes it safe.

### M6. Transitivity

Privacy must hold transitively: if A imports B and C imports A, then C
must not see B's internal bindings. This holds because (a) the
`:internal` mark persists on the core binding (M1), and (b) `rebody_int`
walks *every* nested `Let` level, applying the M3 mask at each — so B's
internal bindings, which sit in an enclosing `let` level of A's
desugared core, are masked when C's `rebody` splices A. No additional
mechanism is needed.

## Backwards compatibility

`:internal` is a new value; `:suppress` and `:normal` are unchanged — no
existing unit changes behaviour, no migration is forced. A follow-up
audit could migrate library bindings that are `:suppress`-marked *and*
genuinely private to `:internal`; that is separate clean-up, not part of
this spec.

## Test plan

- A unit with an `:internal` helper, **bare-imported** by another unit:
  the importer cannot reference the helper (unresolved variable); it
  *can* reference the unit's `:normal` bindings, and those resolve to
  the correct bindings (de Bruijn indices intact — the M3 mask test).
- The same unit **CLI-merged**: the merged unit cannot reference the
  `:internal` helper.
- The same unit **named-imported** (`u=lib.eu`): `u.helper` fails (not a
  field); `u.public` works.
- The unit's *own* public bindings that call the `:internal` helper
  still evaluate correctly (frozen references intact).
- **Transitivity**: A imports B (B has an `:internal` helper); C imports
  A — C cannot see the helper; A's public bindings still work.
- **Index integrity**: a unit whose first declaration is `:internal`
  and whose later public bindings are referenced by an importer — the
  public references resolve correctly (guards the M3 position-preserving
  requirement).
- **Rendering**: an `:internal` binding does not appear in YAML/JSON
  output; a `:suppress` binding still does not render but stays
  importer-visible.
- The `` ` :internal `` shorthand normalises correctly.
- Regression: existing `:suppress` behaviour unchanged; full
  `cargo test` green; clippy clean.

## File-by-file change summary

| File | Change |
|------|--------|
| `src/core/metadata.rs` | recognise the `` ` :internal `` bare-symbol shorthand → `{ export: :internal }` (alongside `:suppress`, ~:44) |
| block/unit desugaring (`src/core/desugar/`) | omit `:internal` bindings from the block-value field list (M2); ensure the `export: :internal` `Meta` survives onto the core binding (M1) |
| `src/core/expr.rs` | `rebody_int` (:548) — apply a position-preserving capture mask for `:internal` bindings at `prepare_capture_vars` (:551) and `names` (:560); `bind_free_vars` (:1306) gains a capturable mask, or `rebody` passes a placeholder-padded `names` (M3) |
| `src/eval/stg/render.rs` | widen the suppression test (:339) from `:suppress` to `{:suppress, :internal}` (M4) |
| `tests/` | the cases in the test plan |

## Deferred — the region ("splice block") form

Marking each private declaration individually is acceptable but verbose
for a helper-heavy library; a *region* form — one marker over a block of
helpers — is desirable. The candidate is a block-valued declaration
whose fields are spliced into unqualified unit scope while the block
itself is `:internal`. That "splice block fields into sibling scope"
operation has uses beyond visibility and broader implications, and the
language author has asked to design it separately and cautiously. It is
**not** part of this spec. The per-declaration `:internal` mechanism
stands on its own and is the prerequisite either way: the region form,
when designed, will mark a block of bindings as `:internal` *en masse* —
it changes the *ergonomics of marking*, not the underlying mechanism.
