#!/usr/bin/env python3
"""Test eucalypt code examples extracted from documentation.

Extracts code blocks from markdown files and runs them through the eu
binary to verify they produce valid output or pass assertions.

Code blocks are identified by their language tag:
    ```eu          -- testable (default)
    ```eu,notest   -- explicitly skipped
    ```eu,seed=42  -- run with --seed flag

Blocks containing //=> assertions are self-checking: the assertion
panics if the value does not match, causing eu to exit non-zero.

Usage:
    python3 scripts/test-doc-examples.py [options]

Options:
    --verbose       Show output from each test
    --file FILE     Test only examples from FILE
    --eu PATH       Path to eu binary (default: eu)
    --list          List all extracted examples without running them
"""

import re
import os
import sys
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Optional


@dataclass
class CodeBlock:
    """A code block extracted from documentation."""
    source_file: str
    line_number: int
    code: str
    language: str  # "eu", "eu,notest", etc.
    block_index: int  # index within the file

    @property
    def skip(self) -> bool:
        return "notest" in self.language

    @property
    def seed(self) -> Optional[int]:
        match = re.search(r'seed=(\d+)', self.language)
        return int(match.group(1)) if match else None

    @property
    def has_assertions(self) -> bool:
        return '//=>' in self.code or '//!' in self.code

    @property
    def label(self) -> str:
        rel = os.path.relpath(self.source_file)
        return f"{rel}:{self.line_number}"


def extract_code_blocks(filepath: str) -> list[CodeBlock]:
    """Extract eu code blocks from a markdown file."""
    blocks = []
    with open(filepath, 'r') as f:
        lines = f.readlines()

    in_block = False
    block_lines = []
    block_lang = ""
    block_start = 0
    block_index = 0

    for i, line in enumerate(lines, 1):
        stripped = line.rstrip()

        if not in_block:
            # Check for opening fence with eu language
            match = re.match(r'^```(eu\b[^\s]*)$', stripped)
            if match:
                in_block = True
                block_lang = match.group(1)
                block_start = i
                block_lines = []
                continue
        else:
            if stripped == '```':
                # Closing fence
                code = '\n'.join(block_lines)
                if code.strip():
                    blocks.append(CodeBlock(
                        source_file=filepath,
                        line_number=block_start,
                        code=code,
                        language=block_lang,
                        block_index=block_index,
                    ))
                    block_index += 1
                in_block = False
                continue
            block_lines.append(line.rstrip())

    return blocks


def is_likely_fragment(code: str) -> bool:
    """Heuristic: is this code block likely a fragment that won't run standalone?

    Returns True for blocks that look like they need surrounding context.
    """
    stripped = code.strip()

    # Empty or whitespace only
    if not stripped:
        return True

    # Single expression without a declaration (no colon)
    lines = [l.strip() for l in stripped.split('\n') if l.strip()
             and not l.strip().startswith('#')]
    if not lines:
        return True

    # If the first non-comment line doesn't contain a declaration
    # (name: ...) it's probably a fragment
    first_line = lines[0]

    # Check for common fragment patterns
    # - starts with a pipe (shell command)
    if first_line.startswith('|') or first_line.startswith('$'):
        return True

    # - is a bare expression (no `:` declaration)
    # But be careful: `x: 1` is a declaration, `x` alone is not
    has_declaration = any(':' in l and not l.strip().startswith('#')
                         for l in lines)
    if not has_declaration:
        return True

    return False


def run_example(block: CodeBlock, eu_path: str, verbose: bool = False,
                timeout: int = 10) -> tuple[bool, str]:
    """Run a code block through eu and return (success, message)."""
    if block.skip:
        return True, "SKIP (notest)"

    if is_likely_fragment(block.code):
        return True, "SKIP (fragment)"

    # Write to temp file
    with tempfile.NamedTemporaryFile(
        mode='w', suffix='.eu', prefix='doctest_', delete=False
    ) as f:
        f.write(block.code)
        f.write('\n')
        tmpfile = f.name

    try:
        cmd = [eu_path]
        if block.seed is not None:
            cmd.extend(['--seed', str(block.seed)])
        cmd.append(tmpfile)

        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout,
        )

        if result.returncode == 0:
            if verbose:
                return True, f"OK\n{result.stdout.strip()}"
            return True, "OK"
        else:
            err = result.stderr.strip()
            # Clean ANSI escape codes
            err = re.sub(r'\x1b\[[0-9;]*m', '', err)
            return False, f"FAIL (exit {result.returncode})\n{err}"

    except subprocess.TimeoutExpired:
        return False, f"FAIL (timeout after {timeout}s)"
    finally:
        os.unlink(tmpfile)


def find_doc_files(doc_dir: str) -> list[str]:
    """Find all markdown files in the doc directory."""
    files = []
    for root, dirs, filenames in os.walk(doc_dir):
        for name in sorted(filenames):
            if name.endswith('.md'):
                files.append(os.path.join(root, name))
    return sorted(files)


def main():
    import argparse

    parser = argparse.ArgumentParser(
        description="Test eucalypt code examples from documentation"
    )
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Show output from each test")
    parser.add_argument("--file", type=str, default=None,
                        help="Test only examples from this file")
    parser.add_argument("--eu", type=str, default="eu",
                        help="Path to eu binary")
    parser.add_argument("--list", action="store_true",
                        help="List all extracted examples without running")
    parser.add_argument("--timeout", type=int, default=10,
                        help="Timeout in seconds per test (default: 10)")

    args = parser.parse_args()

    # Determine paths
    script_dir = Path(__file__).parent
    project_dir = script_dir.parent
    doc_dir = str(project_dir / "doc")

    # Check eu binary
    if not args.list:
        try:
            subprocess.run([args.eu, "version"], capture_output=True,
                           timeout=5)
        except (FileNotFoundError, subprocess.TimeoutExpired):
            print(f"Error: eu binary not found at '{args.eu}'",
                  file=sys.stderr)
            print("Install eu or specify --eu /path/to/eu", file=sys.stderr)
            sys.exit(1)

    # Find and extract blocks
    if args.file:
        doc_files = [args.file]
    else:
        doc_files = find_doc_files(doc_dir)

    all_blocks = []
    for filepath in doc_files:
        blocks = extract_code_blocks(filepath)
        all_blocks.extend(blocks)

    if not all_blocks:
        print("No eu code blocks found in documentation")
        sys.exit(0)

    if args.list:
        print(f"Found {len(all_blocks)} eu code blocks:\n")
        for block in all_blocks:
            skip = " [notest]" if block.skip else ""
            frag = " [fragment]" if is_likely_fragment(block.code) else ""
            assert_ = " [asserts]" if block.has_assertions else ""
            seed = f" [seed={block.seed}]" if block.seed else ""
            print(f"  {block.label}{skip}{frag}{assert_}{seed}")
            if args.verbose:
                for line in block.code.split('\n')[:3]:
                    print(f"    | {line}")
                if block.code.count('\n') > 3:
                    print(f"    | ...")
                print()
        return

    # Run tests
    total = 0
    passed = 0
    failed = 0
    skipped = 0
    failures = []

    print(f"Testing {len(all_blocks)} eu code blocks from documentation...\n")

    for block in all_blocks:
        total += 1
        success, message = run_example(block, args.eu, args.verbose,
                                       args.timeout)

        if "SKIP" in message:
            skipped += 1
            if args.verbose:
                print(f"  SKIP  {block.label}")
        elif success:
            passed += 1
            if args.verbose:
                print(f"  OK    {block.label}")
        else:
            failed += 1
            failures.append((block, message))
            print(f"  FAIL  {block.label}")
            # Show first few lines of code
            for line in block.code.split('\n')[:5]:
                print(f"        | {line}")
            # Show error
            for line in message.split('\n'):
                print(f"        {line}")
            print()

    # Summary
    print(f"\n{'='*60}")
    print(f"Results: {passed} passed, {failed} failed, "
          f"{skipped} skipped, {total} total")

    if failures:
        print(f"\nFailed tests:")
        for block, msg in failures:
            print(f"  {block.label}")
        sys.exit(1)
    else:
        print("\nAll tests passed!")
        sys.exit(0)


if __name__ == "__main__":
    main()
