# Eucalypt Roadmap

## Released

### 0.7.0 (June 2026)

Type system Phase B — higher-kinded types and full row inference:

- **Higher-kinded type variables** — `Functor(f)`, `Monad(m)` etc. in type schemes
- **Structural operator constraints** — `monad()` metadata and monadic-do block checking
- **Partiality and indexed access** — `!!` typed as returning partial `T | error`
- **Prelude type-cache** — prelude type summaries cached for fast checking
- **Full row-variable inference** — open record rows inferred across call boundaries
- **Higher-order pattern unification** — HKT unification via pattern matching on spines

### 0.6.2 (June 2026)

Type system Phase A — gradual typing foundations:

- **Row polymorphism and Dict types** — open/closed records, `Dict(symbol, T)` homogeneous blocks
- **Recursive types** — equirecursive `mu` types for self-referential structures
- **Literal types and flow narrowing** — symbol singletons, `NonEmpty`, flow-sensitive narrowing
- **First-class alias references** — `type:` and `result:` metadata in type DSL
- **Monad type checking** — monadic block checking via `monad()` metadata (A10 hints)

---

## Planned / Under Design

### Unimplemented features with specs

The following have written specifications but have not yet been implemented:

- **Type-alias shorthand** (`type-alias-shorthand-spec.md`) — metadata shorthand for
  `type:` annotations and `result-def` convention for result aliases. Independent of
  the type system roadmap.
- **Unit-level declaration visibility** (`unit-visibility-spec.md`) — `export: :internal`
  to mark declarations as implementation-private, not accessible from importing units.
  An imports/units feature, independent of the type system.

### Deferred (longer horizon)

- **Unified lazy streams** (`unified-streams-spec.md`) — lazy stream producers and
  consumers with a unified interface. Deferred pending architectural work on environment
  copying and thunk memoisation.
- **Full GC heap verification** (`gc-verification-spec.md`) — deep verification pass
  over the Immix heap for use in CI. Ongoing.

### Exploratory

`docs/development/type-system-evolution.md` contains design hypotheses (H1–H19) for
further type system evolution. Nothing there is decided or scheduled.

---

## Architecture Notes

See `docs/development/` for implementation specs, architecture decisions, and debugging
guides. User-facing documentation is in `docs/guide/` and `docs/reference/`.
