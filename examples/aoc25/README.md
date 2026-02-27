# Advent of Code 2025 in Eucalypt

Working through [Advent of Code 2025](https://adventofcode.com/2025) using
[eucalypt](https://github.com/curvelogic/eucalypt) as the solving language.

## Running Solutions

Run from this directory (`aoc/`):

```sh
eu day01.eu -t part-1           # part 1, real input
eu day01.eu -t part-2           # part 2, real input
eu test day01.eu                # run tests against example data
eu test .                       # run all tests (day12 has no test target)

# Quick exploration of input
eu inputs/day01.txt -e 'lines count'
```

Import paths are relative to the working directory, so `eu` must be
run from here (where `inputs/` is visible).

## Structure

- `day{01..12}.eu` - solutions
- `inputs/` - puzzle inputs and examples (`dayNN.txt`, `dayNN-exN.txt`)

## Style

The AoC style guide lives in the eucalypt repo at `docs/eucalypt-style.md`.
