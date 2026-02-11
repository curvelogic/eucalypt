# IO

## Prelude Versioning

| Function | Description |
|----------|-------------|
| `eu.prelude` | Metadata about this version of the standard prelude |
| `eu.build` | Metadata about this version of the eucalypt executable |
| `eu.requires` | Assert that the eucalypt version satisfies the given semver constraint (e.g. '>=0.2.0') |

## IO Functions

| Function | Description |
|----------|-------------|
| `io.env` | Read access to environent variables at time of launch |
| `io.args` | Line arguments passed after -- separator |
| `io.RANDOM_SEED` | Seed for random number generation (from --seed or system time) |
| `io.random` | Infinite lazy stream of random floats in [0,1), seeded from system entropy or --seed flag |

## Other

| Function | Description |
|----------|-------------|
| `alter?(k?, v!, k, v)` | If `k` satisfies `k?` then `v!` else `v` |
| `update?(k?, f, k, v)` | If `k` satisfies `k?` then `v!` else `v` |
