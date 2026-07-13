#!/usr/bin/env python3
"""Corpus-parse equivalence check for the tree-sitter grammar (eu-wrf7).

Parses every .eu file in the "non-error-test" corpus (tests/harness/,
excluding tests/harness/errors/, plus lib/ and examples/aoc25/) with the
tree-sitter grammar and compares the pass/fail outcome against a checked-in
list of known/expected failures
(editors/tree-sitter-eucalypt/test/corpus-expected-failures.txt).

tests/harness/errors/ is deliberately excluded: the Rowan parser is
lossless/error-tolerant and defers rejection of those fixtures to later
compiler stages (verify/cook), so they are not meaningful tree-sitter
grammar divergences — see docs/superpowers/reports/2026-07-13-parser-equivalence-audit.md
section 1 for the full rationale.

Exit codes:
  0 - no new divergences (files not in the expected-failures list that now
      fail to parse). Files that were expected to fail but now parse
      successfully are reported as informational (the list should be
      pruned, but this does not fail the build) — this is the "ratchets
      down" mechanism the check is meant to encourage.
  1 - one or more NEW divergences found: a file outside the expected-
      failures list fails to parse with the current grammar. This is a
      genuine regression (or newly-discovered gap) and must be either
      fixed in the grammar or deliberately added to the expected-failures
      list with a reason.

This script only exercises the tree-sitter side. It assumes every corpus
file is valid eucalypt (accepted by the Rowan parser) — that invariant is
enforced separately by the main `cargo test` / `eu test` harness run.
"""
import argparse
import os
import subprocess
import sys

REPO_ROOT = os.path.normpath(os.path.join(os.path.dirname(__file__), ".."))
GRAMMAR_DIR = os.path.join(REPO_ROOT, "editors", "tree-sitter-eucalypt")
EXPECTED_FAILURES_FILE = os.path.join(
    GRAMMAR_DIR, "test", "corpus-expected-failures.txt"
)

CORPUS_DIRS = [
    "lib",
    "tests/harness",
    "examples/aoc25",
]
EXCLUDE_PREFIXES = [
    "tests/harness/errors/",
]


def find_corpus_files():
    files = []
    for rel_dir in CORPUS_DIRS:
        abs_dir = os.path.join(REPO_ROOT, rel_dir)
        if not os.path.isdir(abs_dir):
            continue
        for dirpath, _dirnames, filenames in os.walk(abs_dir):
            for fname in filenames:
                if not fname.endswith(".eu"):
                    continue
                abspath = os.path.join(dirpath, fname)
                relpath = os.path.relpath(abspath, REPO_ROOT)
                if any(relpath.startswith(p) for p in EXCLUDE_PREFIXES):
                    continue
                files.append(relpath)
    return sorted(files)


def load_expected_failures():
    if not os.path.exists(EXPECTED_FAILURES_FILE):
        return set()
    expected = set()
    with open(EXPECTED_FAILURES_FILE, encoding="utf-8") as f:
        for line in f:
            line = line.split("#", 1)[0].strip()
            if line:
                expected.add(line)
    return expected


def parses_ok(relpath):
    abspath = os.path.join(REPO_ROOT, relpath)
    try:
        p = subprocess.run(
            ["tree-sitter", "parse", "-q", abspath],
            cwd=GRAMMAR_DIR,
            capture_output=True,
            text=True,
            timeout=30,
        )
        return p.returncode == 0
    except Exception as e:  # noqa: BLE001 - report as a failure, not a crash
        print(f"  (exception running tree-sitter on {relpath}: {e})", file=sys.stderr)
        return False


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--write-expected",
        action="store_true",
        help="Overwrite the expected-failures file with the files that "
        "currently fail to parse, instead of checking against it. Use "
        "when deliberately re-seeding the list.",
    )
    args = parser.parse_args()

    corpus = find_corpus_files()
    if not corpus:
        print("error: no corpus files found — check REPO_ROOT/CORPUS_DIRS", file=sys.stderr)
        return 2

    failing = [f for f in corpus if not parses_ok(f)]

    if args.write_expected:
        os.makedirs(os.path.dirname(EXPECTED_FAILURES_FILE), exist_ok=True)
        with open(EXPECTED_FAILURES_FILE, "w", encoding="utf-8") as f:
            f.write(
                "# Files the tree-sitter grammar cannot currently parse, "
                "even though the Rowan\n"
                "# parser accepts them as valid eucalypt. Catalogued in\n"
                "# docs/superpowers/reports/2026-07-13-parser-equivalence-audit.md.\n"
                "# Regenerate with: python3 scripts/check-tree-sitter-corpus.py --write-expected\n"
                "# Prune entries as the underlying grammar gaps are fixed.\n"
            )
            for rel in failing:
                f.write(rel + "\n")
        print(f"Wrote {len(failing)} entries to {EXPECTED_FAILURES_FILE}")
        return 0

    expected = load_expected_failures()
    failing_set = set(failing)

    new_divergences = sorted(failing_set - expected)
    resolved = sorted(expected - failing_set)
    stale_entries = sorted(expected - set(corpus))

    print(f"Corpus files checked: {len(corpus)}")
    print(f"Currently failing to parse: {len(failing)}")
    print(f"Expected failures (seeded list): {len(expected)}")
    print()

    if resolved:
        print(f"INFO: {len(resolved)} previously-expected failure(s) now parse "
              f"successfully — consider pruning corpus-expected-failures.txt:")
        for r in resolved:
            print(f"  - {r}")
        print()

    if stale_entries:
        print(f"INFO: {len(stale_entries)} expected-failures entr(y/ies) no "
              f"longer exist in the corpus (renamed/removed file?):")
        for s in stale_entries:
            print(f"  - {s}")
        print()

    if new_divergences:
        print(f"FAIL: {len(new_divergences)} NEW divergence(s) — the tree-sitter "
              f"grammar rejects source the Rowan parser accepts, and this isn't "
              f"in the expected-failures list:")
        for d in new_divergences:
            print(f"  - {d}")
        print()
        print("Either fix the grammar, or if this is a deliberate/known gap, "
              "add the file to")
        print(f"  {os.path.relpath(EXPECTED_FAILURES_FILE, REPO_ROOT)}")
        print("with a comment explaining why.")
        return 1

    print("OK: no new tree-sitter grammar divergences.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
