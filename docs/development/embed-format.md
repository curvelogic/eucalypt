# Embed Format Reference

The embed system allows core and STG expressions to be represented as
tagged lists within eucalypt syntax. This enables testing, debugging,
and programmatic construction of compiler intermediate representations.

## Overview

Embedding works in two directions:

- **Embed** (`Embed` trait): converts a core/STG expression into a
  eucalypt AST representation (tagged lists)
- **Disembed** (parse-embed): reads tagged list structures from
  eucalypt source and reconstructs core expressions

## Parse-Embed Blocks

A parse-embed block is a eucalypt declaration block with a `parse-embed`
metadata key indicating the target representation:

```
{ parse-embed: :CORE }

` { embedding: :core }
CORE: [:c-app [:c-bif :HEAD] [[:c-var "x"]]]
```

The `parse-embed` metadata value determines the interpreter:
- `:CORE` — interpreted as core expressions
- `:STG` — interpreted as STG expressions (future)

## Core Embedding Tags

All core embedding tags use the `c-` prefix.

### Variables and Names

| Tag | Format | Description |
|-----|--------|-------------|
| `c-var` | `[:c-var "name"]` | Variable reference (resolved against enclosing scope) |
| `c-name` | `[:c-name "identifier"]` | Unresolved name (for lookup) |

### Literals and Intrinsics

| Tag | Format | Description |
|-----|--------|-------------|
| `c-lit` | `[:c-lit value]` | Primitive value (string, symbol, number, boolean, null) |
| `c-bif` | `[:c-bif :NAME]` | Built-in function reference |

Literal values are represented directly:
- Strings: `"hello"`
- Symbols: `:foo`
- Numbers: `42`, `3.14`
- Booleans: `true`, `false`
- Null: `null`

### Compound Expressions

| Tag | Format | Description |
|-----|--------|-------------|
| `c-list` | `[:c-list item1 item2 ...]` | List construction |
| `c-block` | `[:c-block {key: val, ...}]` | Block/object construction |
| `c-args` | `[:c-args arg1 arg2 ...]` | Function argument tuple |
| `c-soup` | `[:c-soup item1 item2 ...]` | Operator soup (unresolved precedence) |

### Binding and Abstraction

| Tag | Format | Description |
|-----|--------|-------------|
| `c-let` | `[:c-let {x: binding, ...} body]` | Let bindings (recursive) |
| `c-lam` | `[:c-lam ["p1" "p2"] body]` | Lambda function |
| `c-app` | `[:c-app func [arg1 arg2]]` | Function application |

### Metadata and Lookup

| Tag | Format | Description |
|-----|--------|-------------|
| `c-meta` | `[:c-meta expr metadata]` | Metadata annotation |
| `c-lookup` | `[:c-lookup obj "key"]` | Property lookup |
| `c-lookup` | `[:c-lookup obj "key" fallback]` | Lookup with fallback |

### Operators

| Tag | Format | Description |
|-----|--------|-------------|
| `c-op` | `[:c-op :fixity precedence expr]` | Operator with fixity info |

Fixity values: `:nullary`, `:prefix`, `:postfix`, `:infixl`, `:infixr`

### Anaphora

| Tag | Format | Description |
|-----|--------|-------------|
| `c-bk-ana` | `[:c-bk-ana]` | Block anaphor |
| `c-ex-ana` | `[:c-ex-ana]` | Expression anaphor |

### Error Markers

| Tag | Format | Description |
|-----|--------|-------------|
| `e-unresolved` | `[:e-unresolved "name"]` | Unresolved variable |
| `e-redeclaration` | `[:e-redeclaration "name"]` | Redeclared variable |
| `e-eliminated` | `[:e-eliminated]` | Eliminated code marker |
| `e-pseudodot` | `[:e-pseudodot]` | Pseudo dot operator |
| `e-pseudocall` | `[:e-pseudocall]` | Pseudo call operator |
| `e-pseudocat` | `[:e-pseudocat]` | Pseudo concatenation |

## STG Embedding Tags

All STG embedding tags use the `s-` prefix.

### Atoms and References

| Tag | Format | Description |
|-----|--------|-------------|
| `s-atom` | `[:s-atom ref]` | Single reference or value |

References are represented as:
- Local: `"L0"`, `"L1"`, etc. (index into local environment)
- Global: `"G0"`, `"G1"`, etc. (index into global environment)
- Native values: strings, symbols, numbers directly

### Evaluation

| Tag | Format | Description |
|-----|--------|-------------|
| `s-case` | `[:s-case scrutinee [[tag body]...] fallback]` | Case expression |
| `s-cons` | `[:s-cons tag ref1 ref2 ...]` | Data constructor |
| `s-app` | `[:s-app callable ref1 ref2 ...]` | Function application |
| `s-bif` | `[:s-bif :NAME ref1 ref2 ...]` | Saturated intrinsic call |

### Bindings

| Tag | Format | Description |
|-----|--------|-------------|
| `s-let` | `[:s-let [binding1 ...] body]` | Let bindings |
| `s-letrec` | `[:s-letrec [binding1 ...] body]` | Recursive let bindings |

### Lambda Forms

| Tag | Format | Description |
|-----|--------|-------------|
| `s-lambda` | `[:s-lambda bound body]` | Lambda (bound = arity) |
| `s-thunk` | `[:s-thunk body]` | Thunk (updateable) |
| `s-value` | `[:s-value body]` | Value (no update needed) |

### Annotations and Metadata

| Tag | Format | Description |
|-----|--------|-------------|
| `s-ann` | `[:s-ann smid body]` | Source annotation |
| `s-meta` | `[:s-meta meta-ref body-ref]` | Metadata wrapper |
| `s-demeta` | `[:s-demeta scrutinee handler or-else]` | Metadata destructuring |
| `s-hole` | `[:s-hole]` | Black hole (uninitialised) |

## Scoping Rules

### Core Embed Scoping

Variables in `c-var` are resolved against the enclosing eucalypt scope.
This allows embedded core to reference variables defined in surrounding
eucalypt code:

```
x: 42

` { parse-embed: :CORE }
result: [:c-app [:c-bif :HEAD] [[:c-var "x"]]]
```

The `c-var "x"` reference will be resolved to the `x` binding in the
enclosing scope.

For `c-let` and `c-lam`, the bound variable names are added to the
scope for the duration of their body:

```
[:c-let {y: [:c-lit 10]} [:c-var "y"]]
```

### STG Embed Scoping

STG uses de Bruijn-style indices rather than names. Local references
(`L0`, `L1`, etc.) refer to positions in the current environment
frame. Global references (`G0`, `G1`, etc.) refer to the global
environment (runtime intrinsic wrappers).

## Stability

The embed format is stable by convention. Changes to tag names, argument
order, or representation structure should be documented here and made
deliberately with backward compatibility in mind.

The format is not enforced by snapshot tests. Correctness is verified
by round-trip tests (embed then disembed) and by unit tests on
individual expression types.
