# Advent of Code 2025 in Eucalypt

Working through [Advent of Code 2025](https://adventofcode.com/2025) using
[eucalypt](https://github.com/curvelogic/eucalypt) as the solving language.

## Supplying your own puzzle input

Advent of Code asks that puzzle inputs are not redistributed, so this
repository does not include them. To run a solution against a real input,
download your own from the puzzle page (you need to be logged in) and save
it as `inputs/dayNN.txt`, for example `inputs/day01.txt`. Those paths are
ignored by git, so your inputs stay out of commits.

The tests use only the small worked examples in `inputs/dayNN-exN.txt` and
run without any real input.

## Running solutions

```sh
eu test .                       # run every day's tests against the examples
eu test day01.eu                # one day's tests
eu run/day01.eu -t part-1       # part 1 against inputs/day01.txt
eu run/day01.eu -t part-2       # part 2 against inputs/day01.txt

# Quick exploration of an input
eu inputs/day01.txt -e 'lines count'
```

Imports resolve relative to the importing file, so these commands work
from any directory once the paths are adjusted. Without
`inputs/dayNN.txt`, a `run/` target stops with `could not read
'../inputs/dayNN.txt'`.

## Structure

- `day{01..12}.eu` - solutions, each with a `test` target over the worked
  examples (day12 has none)
- `run/day{01..12}.eu` - the `part-1` and `part-2` targets, which import
  the solution and your real input
- `inputs/` - the worked examples (`dayNN-exN.txt`) and, once you add
  them, your puzzle inputs (`dayNN.txt`)

## Style

The AoC style guide lives in the eucalypt repo at `docs/eucalypt-style.md`.
