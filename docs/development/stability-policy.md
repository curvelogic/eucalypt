# Stability Policy

This document defines eucalypt's stability tiers and the meaning of
each semver field. It was ratified at 0.8.0. The enumerated Stable
surface is frozen at 1.0.

## Semver field meanings

| Field | Meaning |
|-------|---------|
| **MAJOR** | A change an unmodified `.eu` file can observe in its output, or a removal from the Stable surface. Requires a documented deprecation path. |
| **MINOR** | A backwards-compatible addition to the Stable surface. |
| **PATCH** | No observable semantic change to any input/output or CLI behaviour. |

Build metadata (`+N`, e.g. `0.8.0+1685`) is ignored by
`VersionReq::matches()`. Use `eu.requires(">=0.8")` to pin against a
minimum version.

## Stability tiers

| Tier | Contents | Promise |
|------|----------|---------|
| **Stable** | Core syntax and semantics; prelude API (all exported names and their signatures); block-merge and lookup semantics; CLI surface (`eu`, `-t`, `-x`, `-e`, `--allow-io`, `--lib`, `--help`, `--version`, `check`, `test`, `dump`); all import/export formats (YAML, JSON, TOML, EDN, CSV, XML, text); error codes and source-location accuracy; exit codes | Will not break in a MINOR or PATCH. Breaking changes require a MAJOR with a deprecation path. |
| **Experimental** | Gradual type-annotation DSL (`:type` metadata, `eu check`, bidirectional checker); type warning messages and exact locations | May change in a MINOR. The feature is opt-in and advisory — it never changes runtime behaviour. |
| **Not covered** | Internal IR (Core, STG); GC internals and `EU_GC_*` env variables; `eu dump` output format; exact prose of error messages; names with `` ` :internal `` metadata | No promise. These may change in any release. |

## `eu.requires` pinning convention

Every `.eu` file that depends on features introduced in a specific
release should declare a version requirement near the top:

```eu
eu.requires(">=0.8")
```

Shipped library files (`lib/lens.eu`, `lib/state.eu`, `lib/markup.eu`)
serve as exemplars of this pattern.

Running a file on an incompatible version produces a
`VersionRequirementFailed` error with a clear message.

## Applying this policy

When considering a change to eucalypt:

1. **Identify the tier.** Is the affected surface Stable, Experimental,
   or Not covered?
2. **If Stable:** the change must not break existing files that rely on
   the current behaviour. If a breaking change is necessary, deprecate
   in a MINOR and remove in the next MAJOR.
3. **If Experimental:** a MINOR bump with a changelog entry is
   sufficient. No deprecation path is required.
4. **If Not covered:** any version bump is sufficient. Document changes
   in the changelog if they may affect debugging workflows.
