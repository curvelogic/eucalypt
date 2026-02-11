# IO

## `eu` Namespace

- `eu.prelude` -- Metadata about the standard prelude (includes `version`)
- `eu.build` -- Build metadata for the eucalypt executable
- `eu.requires` -- Assert the eucalypt version satisfies a semver constraint (e.g. `eu.requires(">=0.3.0")`)

## `io` Namespace

- `io.env` -- Block of environment variables at launch time
- `io.epoch-time` -- Unix timestamp at launch time
- `io.args` -- List of command-line arguments passed after `--` separator
- `io.RANDOM_SEED` -- Seed for random number generation (from `--seed` or system time)
- `io.random` -- Infinite lazy stream of random floats in `[0,1)` (see [Random Numbers](random.md))
