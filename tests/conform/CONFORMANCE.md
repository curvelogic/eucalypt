# Conformance Contract

Files in `tests/conform/` plus their `.golden` sidecars form the
**output-stability contract** for eucalypt's rendered output.

## Structure

- Each `.eu` file has exactly one `` ` :conform `` target.
- The target metadata specifies the export format via `format:` (default `:yaml`).
- The corresponding `.golden` file contains the exact expected byte output.

## Running

```
cargo test --test conform_test
```

## Regenerating golden files

After an intentional output change, regenerate all sidecars:

```
BLESS=1 cargo test --test conform_test
```

Then review the diff (`git diff tests/conform/`) before committing.

## Adding a new conformance file

1. Create `tests/conform/<name>.eu` with a `` ` { target: :conform format: :yaml } ``
   target declaration.
2. Run `BLESS=1 cargo test --test conform_test` to generate the golden sidecar.
3. Review the golden output and commit both files together.

## Coverage

| File | What it pins |
|------|-------------|
| `arithmetic.eu` | Basic numeric operations |
| `list_ops.eu` | map, filter, fold, zip |
| `block_ops.eu` | merge, deep-merge, lookup, keys |
| `string_ops.eu` | interpolation, split, replace, join |
| `boolean_logic.eu` | Boolean operators and predicates |
| `conditionals.eu` | if, then, cond clauses |
| `cond_clause.eu` | Multi-way conditional (cond/=>) |
| `sort_order.eu` | sort-nums, sort-by-num |
| `shallow_merge.eu` | Block merge precedence |
| `deep_merge.eu` | Recursive deep-merge (<<) |
| `merge_with_metadata.eu` | Merge plus metadata interaction |
| `operator_precedence.eu` | Catenation vs infix precedence |
| `numeric_predicates.eu` | zero?, positive?, negative? |
| `list_construction.eu` | cons, range, iota |
| `string_formatting.eu` | str.len, str.contains?, str.trim |
| `yaml_output.eu` | YAML export format |
| `json_output.eu` | JSON export format |
| `toml_output.eu` | TOML export format |
| `text_output.eu` | Plain text export format |
| `edn_output.eu` | EDN export format |

## Stability promise

A change that alters any golden file is a breaking output change and
requires explicit acknowledgement by running `BLESS=1` and committing
the updated sidecars. CI will fail on any unblessed mismatch.
