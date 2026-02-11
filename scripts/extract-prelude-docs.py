#!/usr/bin/env python3
"""Extract documentation from lib/prelude.eu backtick doc strings.

Parses the prelude source to extract function signatures, doc strings,
and metadata, then generates markdown reference pages split by category.

Usage:
    python3 scripts/extract-prelude-docs.py [--check] [--output DIR]

Options:
    --check     Compare generated docs with existing and report differences
    --output    Write generated pages to DIR (default: doc/reference/prelude/)
"""

import re
import sys
import os
from dataclasses import dataclass, field
from typing import Optional
from pathlib import Path


@dataclass
class PreludeEntry:
    """A documented prelude function or value."""
    name: str
    signature: str  # e.g. "sort-nums(xs)" or "head"
    doc: str
    namespace: Optional[str] = None  # e.g. "str", "cal", "set", "io", "eu"
    section: Optional[str] = None  # e.g. "List basics", "Arithmetic"
    is_operator: bool = False
    is_private: bool = False
    export_suppress: bool = False
    precedence: Optional[str] = None
    associates: Optional[str] = None
    line_number: int = 0


# The prelude has these sections (in order) delimited by ## comments:
#   "Prelude versioning and run metadata"
#   "Random number generation"
#   "Error / debug support"
#   "Essentials"
#   "List basics"
#   "Blocks / merge"
#   "Deep find"
#   "Deep query"
#   "Boolean"
#   "Polymorphic equality"
#   "Arithmetic"
#   "Text and regexes"
#   "Combinators"
#   "Sets"
# Plus single-# comments for utilities and block library

# Section-to-category mapping
SECTION_TO_CATEGORY = {
    "Prelude versioning and run metadata": "io",
    "Random number generation": "random",
    "Error / debug support": "booleans",
    "Essentials": "booleans",
    "List basics": "lists",
    "Blocks / merge": "blocks",
    "Deep find": "blocks",
    "Deep query": "blocks",
    "Boolean": "booleans",
    "Polymorphic equality": "booleans",
    "Arithmetic": "numbers",
    "Text and regexes": "strings",
    "Combinators": "combinators",
    "Sets": "sets",
    # Sections from single-# comments
    "Utilities": "combinators",
    "Metadata basics": "metadata",
    "List library functions": "lists",
    "Block library functions": "blocks",
}

# Section display names for grouping within category pages
SECTION_DISPLAY = {
    # booleans page
    "Error / debug support": "Error Handling",
    "Essentials": "Constants and Control Flow",
    "Boolean": "Boolean Functions and Operators",
    "Polymorphic equality": "Equality and Comparison",
    # lists page
    "List basics": "Basic Operations",
    "List library functions": "Transformations",
    # blocks page
    "Blocks / merge": "Construction and Access",
    "Deep find": "Deep Find and Query",
    "Deep query": "Deep Find and Query",
    "Block library functions": "Transformation",
    # numbers page
    "Arithmetic": "Operators and Functions",
    # strings page
    "Text and regexes": "",
    # combinators page
    "Combinators": "",
    "Utilities": "",
    # random page
    "Random number generation": "",
    # sets page
    "Sets": "",
    # io page
    "Prelude versioning and run metadata": "`eu` Namespace",
    # metadata page
    "Metadata basics": "",
}

# Namespace-to-category mapping
NAMESPACE_TO_CATEGORY = {
    "str": "strings",
    "cal": "calendar",
    "set": "sets",
    "io": "io",
    "eu": "io",
    "ch": "strings",
}

# Known namespaces that open with name: {
KNOWN_NAMESPACES = {"eu", "io", "str", "cal", "set", "ch", "assertions", "_block"}


def parse_doc_string(doc_text: str) -> dict:
    """Parse a backtick doc string, which may be a plain string or a block."""
    doc_text = doc_text.strip()

    # Simple string: ` "..."
    if doc_text.startswith('"') and doc_text.endswith('"'):
        return {"doc": doc_text[1:-1]}

    # Block form: ` { doc: "..." precedence: :level ... }
    if doc_text.startswith('{') and doc_text.endswith('}'):
        inner = doc_text[1:-1].strip()
        result = {}

        # Extract doc string - handle multi-line doc strings
        doc_match = re.search(r'doc:\s*"((?:[^"\\]|\\.)*)"', inner, re.DOTALL)
        if doc_match:
            # Collapse whitespace in multi-line doc strings
            doc = doc_match.group(1)
            doc = re.sub(r'\s+', ' ', doc).strip()
            result["doc"] = doc

        # Extract export
        export_match = re.search(r'export:\s*:(\w+)', inner)
        if export_match:
            result["export"] = export_match.group(1)

        # Extract precedence
        prec_match = re.search(r'precedence:\s*(?::(\w[\w-]*)|(\d+))', inner)
        if prec_match:
            result["precedence"] = prec_match.group(1) or prec_match.group(2)

        # Extract associates
        assoc_match = re.search(r'associates:\s*:(\w+)', inner)
        if assoc_match:
            result["associates"] = assoc_match.group(1)

        return result

    return {"doc": doc_text}


def parse_declaration(line: str) -> Optional[dict]:
    """Parse a declaration line to extract name and signature."""

    # Operator declaration: (l + r): ...
    op_match = re.match(r'\((.+?)\)\s*:', line)
    if op_match:
        sig = op_match.group(1).strip()
        return {"signature": sig, "is_operator": True, "name": sig}

    # Function/value declaration: name(params): ... or name: ...
    func_match = re.match(r'([a-zA-Z_∅][\w?!∅-]*(?:\([^)]*\))?)\s*:', line)
    if func_match:
        sig = func_match.group(1).strip()
        # Extract just the name (without params)
        name_match = re.match(r'([a-zA-Z_∅][\w?!∅-]*)', sig)
        name = name_match.group(1) if name_match else sig
        is_private = name.startswith('__') or name.startswith('_')
        return {"signature": sig, "is_operator": False, "name": name,
                "is_private": is_private}

    return None


def parse_prelude(prelude_path: str) -> list[PreludeEntry]:
    """Parse the prelude file and extract all documented entries."""
    entries = []
    current_section = None
    current_namespace = None
    namespace_stack = []  # (namespace_name, brace_depth_at_open)
    pending_doc = None
    pending_doc_line = 0
    brace_depth = 0

    with open(prelude_path, 'r') as f:
        lines = f.readlines()

    i = 0
    while i < len(lines):
        line = lines[i].rstrip()
        stripped = line.strip()

        # ---- Section comment detection ----
        # Pattern: line is exactly "##", next non-blank line starts with "## "
        if stripped == '##':
            # Look ahead for section name
            j = i + 1
            while j < len(lines) and not lines[j].strip():
                j += 1
            if j < len(lines):
                next_line = lines[j].strip()
                name_match = re.match(r'^##\s+(.+)$', next_line)
                if name_match:
                    section_text = name_match.group(1).strip()
                    # Normalise: take text before first comma for multi-line
                    # section descriptions
                    if ',' in section_text:
                        section_text = section_text.split(',')[0].strip()
                    # Remove trailing ## if present
                    section_text = section_text.rstrip('#').rstrip()
                    # Remove em-dash descriptions
                    if '\u2014' in section_text:
                        section_text = section_text.split('\u2014')[0].strip()
                    current_section = section_text
                    # Skip past the section comment block (all ## lines)
                    i = j + 1
                    while i < len(lines) and lines[i].strip().startswith('##'):
                        i += 1
                    continue
            i += 1
            continue

        # Single-line # section comments (e.g. "# Utilities" or
        # "# Block library functions, maps and folds")
        single_section = re.match(r'^#\s+(.+)$', stripped)
        if single_section and not stripped.startswith('##'):
            section_text = single_section.group(1).strip()
            if ',' in section_text:
                section_text = section_text.split(',')[0].strip()
            # Only track known section names
            if section_text in SECTION_TO_CATEGORY:
                current_section = section_text
            i += 1
            continue

        # ---- Brace depth tracking ----
        # Count braces (crude but sufficient for prelude structure)
        # Exclude braces inside strings
        line_for_braces = re.sub(r'"[^"]*"', '', line)
        open_braces = line_for_braces.count('{')
        close_braces = line_for_braces.count('}')

        # ---- Namespace detection ----
        # Check for namespace opening at current depth
        if pending_doc is not None:
            ns_match = re.match(r'^(\w[\w-]*)\s*:\s*\{', stripped)
            if ns_match and ns_match.group(1) in KNOWN_NAMESPACES:
                ns_name = ns_match.group(1)
                namespace_stack.append((ns_name, brace_depth))
                current_namespace = ns_name
                pending_doc = None  # Namespace-level doc, don't emit
                brace_depth += (open_braces - close_braces)
                i += 1
                continue
        else:
            ns_match = re.match(r'^(\w[\w-]*)\s*:\s*\{', stripped)
            if ns_match and ns_match.group(1) in KNOWN_NAMESPACES:
                ns_name = ns_match.group(1)
                namespace_stack.append((ns_name, brace_depth))
                current_namespace = ns_name
                brace_depth += (open_braces - close_braces)
                i += 1
                continue

        brace_depth += (open_braces - close_braces)

        # Check if we've exited a namespace
        while namespace_stack and brace_depth <= namespace_stack[-1][1]:
            namespace_stack.pop()
            current_namespace = (namespace_stack[-1][0]
                                 if namespace_stack else None)

        # ---- Backtick doc string detection ----
        doc_match = re.match(r'^(\s*)` (.+)$', line)
        if doc_match:
            doc_text = doc_match.group(2)
            # May span multiple lines if it's a block
            if doc_text.strip().startswith('{') and '}' not in doc_text:
                while i + 1 < len(lines) and '}' not in doc_text:
                    i += 1
                    doc_text += ' ' + lines[i].strip()
            pending_doc = doc_text
            pending_doc_line = i + 1
            i += 1
            continue

        # ---- Declaration detection ----
        if pending_doc is not None and stripped and not stripped.startswith('#'):
            decl = parse_declaration(stripped)
            if decl:
                doc_info = parse_doc_string(pending_doc)
                doc_str = doc_info.get("doc", "")

                entry = PreludeEntry(
                    name=decl["name"],
                    signature=decl["signature"],
                    doc=doc_str,
                    namespace=current_namespace,
                    section=current_section,
                    is_operator=decl["is_operator"],
                    is_private=decl.get("is_private", False),
                    export_suppress=doc_info.get("export") == "suppress",
                    precedence=doc_info.get("precedence"),
                    associates=doc_info.get("associates"),
                    line_number=i + 1,
                )
                entries.append(entry)
                pending_doc = None

        i += 1

    return entries


def categorise_entries(entries: list[PreludeEntry]) -> dict:
    """Assign entries to categories, returning {cat_key: {section: [entries]}}."""
    result = {}

    for entry in entries:
        # Skip private functions
        if entry.is_private:
            continue

        cat_key = None
        section = ""

        # Skip _block internal helpers
        if entry.namespace and entry.namespace.startswith('_'):
            continue

        # 1. Namespace-based
        if entry.namespace and entry.namespace in NAMESPACE_TO_CATEGORY:
            cat_key = NAMESPACE_TO_CATEGORY[entry.namespace]
            # For namespaced entries, group by namespace
            if entry.namespace in ("eu",):
                section = "`eu` Namespace"
            elif entry.namespace in ("io",):
                section = "`io` Namespace"
            elif entry.namespace == "ch":
                section = "Character Constants"
            else:
                section = ""  # Single group for str, cal, set
        elif entry.namespace == "assertions":
            cat_key = "metadata"
            section = "Assertions"
        # 2. Section-based
        elif entry.section:
            cat_key = SECTION_TO_CATEGORY.get(entry.section)
            if cat_key:
                section = SECTION_DISPLAY.get(entry.section, entry.section)

        if cat_key is None:
            continue

        if cat_key not in result:
            result[cat_key] = {}
        if section not in result[cat_key]:
            result[cat_key][section] = []
        result[cat_key][section].append(entry)

    return result


def format_signature(entry: PreludeEntry) -> str:
    """Format a function signature for the markdown table."""
    if entry.is_operator:
        return f"`{entry.signature}`"

    if entry.namespace and entry.namespace not in ('assertions', '_block'):
        return f"`{entry.namespace}.{entry.signature}`"

    return f"`{entry.signature}`"


def format_description(entry: PreludeEntry) -> str:
    """Extract a clean description from the doc string."""
    doc = entry.doc

    # Remove backtick-quoted signature prefix: `name(args)` - description
    doc = re.sub(r'^`[^`]+`\s*[-\u2014\u2013]\s*', '', doc)

    # Remove unquoted signature prefix: name(args) - description
    # Also handles: 'name(args) - description (extra parens)
    doc = re.sub(r"^'?[a-zA-Z_∅][\w?!∅.-]*(?:\([^)]*\))?\s*[-\u2014\u2013]\s*", '', doc)

    # Remove leading single quote if leftover
    doc = doc.lstrip("'")

    # Remove duplicate description (e.g. "reverse(l) - reverse list `l`")
    doc = re.sub(r"^[a-zA-Z_∅][\w?!∅.-]*(?:\([^)]*\))?\s*[-\u2014\u2013]\s*", '', doc)

    # Clean up: remove trailing backtick artifacts, quotes
    doc = doc.strip().rstrip('"').rstrip("'")

    # Capitalise first letter
    if doc and doc[0].islower():
        doc = doc[0].upper() + doc[1:]

    # Remove trailing period if present
    doc = doc.rstrip('.')

    return doc


def generate_page(cat_key: str, sections: dict,
                  handwritten: dict) -> str:
    """Generate markdown for a category page.

    Uses the handwritten page structure as a template where available,
    merging in any entries from the prelude that aren't already documented.
    For categories without handwritten pages, generates from scratch.
    """
    titles = {
        "lists": "Lists",
        "blocks": "Blocks",
        "strings": "Strings",
        "numbers": "Numbers and Arithmetic",
        "booleans": "Booleans and Comparison",
        "combinators": "Combinators",
        "calendar": "Calendar",
        "sets": "Sets",
        "random": "Random Numbers",
        "metadata": "Metadata",
        "io": "IO",
    }

    lines = [f"# {titles.get(cat_key, cat_key.title())}\n"]
    lines.append("")

    for section_name, entries in sections.items():
        if section_name:
            lines.append(f"## {section_name}\n")

        lines.append("| Function | Description |")
        lines.append("|----------|-------------|")

        for entry in entries:
            sig = format_signature(entry)
            desc = format_description(entry)
            lines.append(f"| {sig} | {desc} |")

        lines.append("")

    return "\n".join(lines)


def check_coverage(categorised: dict, existing_dir: str) -> list[str]:
    """Compare extracted entries with existing handwritten docs."""
    report = []
    filenames = {
        "lists": "lists.md", "blocks": "blocks.md", "strings": "strings.md",
        "numbers": "numbers.md", "booleans": "booleans.md",
        "combinators": "combinators.md", "calendar": "calendar.md",
        "sets": "sets.md", "random": "random.md", "metadata": "metadata.md",
        "io": "io.md",
    }

    # Build global set of all documented names across all files
    all_documented_names = set()
    all_docs_content = ""
    for filename in filenames.values():
        filepath = os.path.join(existing_dir, filename)
        if os.path.exists(filepath):
            with open(filepath, 'r') as f:
                content = f.read()
            all_docs_content += content
            stripped = re.sub(r'```.*?```', '', content, flags=re.DOTALL)
            for match in re.finditer(r'`([^`\n]+)`', stripped):
                text = match.group(1)
                name = re.match(r'([a-zA-Z_∅][\w?!∅.\-]*)', text)
                if name:
                    all_documented_names.add(name.group(1))

    for cat_key, filename in filenames.items():
        filepath = os.path.join(existing_dir, filename)
        sections = categorised.get(cat_key, {})

        # Collect all prelude function names for this category
        prelude_names = set()
        for section_entries in sections.values():
            for entry in section_entries:
                if entry.is_operator:
                    prelude_names.add(entry.signature)
                elif entry.namespace:
                    prelude_names.add(f"{entry.namespace}.{entry.name}")
                else:
                    prelude_names.add(entry.name)

        if not os.path.exists(filepath):
            report.append(f"\n{filename}: MISSING (would contain "
                          f"{len(prelude_names)} entries)")
            continue

        # Check each prelude name against ALL docs (not just this file)
        undocumented = set()
        for name in prelude_names:
            bare = name.split('.')[-1] if '.' in name else name

            # Check if documented anywhere
            if name in all_documented_names or bare in all_documented_names:
                continue

            # For operators, check if the operator symbol appears
            # (also check with escaped markdown pipes)
            if ' ' in name:
                parts = name.split()
                op_syms = [p for p in parts
                           if not p.isalpha() or p in ('not',)]
                # Also check escaped versions (e.g. || -> \|\|)
                escaped_syms = [sym.replace('|', '\\|') for sym in op_syms]
                all_syms = op_syms + escaped_syms
                if any(sym in all_docs_content for sym in all_syms):
                    continue

            undocumented.add(name)

        if undocumented:
            report.append(f"\n{filename}: {len(undocumented)} entries "
                          f"in prelude but not documented anywhere:")
            for name in sorted(undocumented):
                report.append(f"  + {name}")
        else:
            report.append(f"\n{filename}: OK ({len(prelude_names)} entries "
                          f"all documented)")

    return report


def main():
    import argparse

    parser = argparse.ArgumentParser(
        description="Extract documentation from prelude.eu"
    )
    parser.add_argument("--check", action="store_true",
                        help="Compare with existing docs and report differences")
    parser.add_argument("--output", type=str, default=None,
                        help="Output directory for generated pages")
    parser.add_argument("--summary", action="store_true",
                        help="Print summary of extracted entries")
    parser.add_argument("--generate", action="store_true",
                        help="Write generated pages to output directory")
    parser.add_argument("--prelude", type=str, default=None,
                        help="Path to prelude.eu")

    args = parser.parse_args()

    # Determine paths
    script_dir = Path(__file__).parent
    project_dir = script_dir.parent
    prelude_path = args.prelude or str(project_dir / "lib" / "prelude.eu")
    existing_dir = str(project_dir / "doc" / "reference" / "prelude")
    output_dir = args.output or str(project_dir / "generated" / "prelude")

    if not os.path.exists(prelude_path):
        print(f"Error: prelude not found at {prelude_path}", file=sys.stderr)
        sys.exit(1)

    # Parse
    entries = parse_prelude(prelude_path)
    public_entries = [e for e in entries if not e.is_private]

    if args.summary:
        print(f"Extracted {len(entries)} entries ({len(public_entries)} public)"
              f" from prelude\n")
        for e in entries:
            ns = f"{e.namespace}." if e.namespace else ""
            priv = " [PRIVATE]" if e.is_private else ""
            sup = " [SUPPRESSED]" if e.export_suppress else ""
            op = " [OP]" if e.is_operator else ""
            sect = e.section or "?"
            print(f"  L{e.line_number:4d}  [{sect:40s}]  "
                  f"{ns}{e.signature}{priv}{sup}{op}")

    # Categorise
    categorised = categorise_entries(entries)

    if args.summary:
        print(f"\nCategories:")
        for cat_key, sections in sorted(categorised.items()):
            total = sum(len(ents) for ents in sections.values())
            section_names = [s or "(default)" for s in sections.keys()]
            print(f"  {cat_key}: {total} entries in sections: "
                  f"{', '.join(section_names)}")

    if args.check:
        report = check_coverage(categorised, existing_dir)
        for line in report:
            print(line)
        return

    if args.generate:
        os.makedirs(output_dir, exist_ok=True)
        for cat_key, sections in categorised.items():
            if not any(sections.values()):
                continue
            content = generate_page(cat_key, sections, {})
            filenames = {
                "lists": "lists.md", "blocks": "blocks.md",
                "strings": "strings.md", "numbers": "numbers.md",
                "booleans": "booleans.md", "combinators": "combinators.md",
                "calendar": "calendar.md", "sets": "sets.md",
                "random": "random.md", "metadata": "metadata.md",
                "io": "io.md",
            }
            filepath = os.path.join(output_dir, filenames[cat_key])
            with open(filepath, 'w') as f:
                f.write(content)
            total = sum(len(ents) for ents in sections.values())
            print(f"  {filenames[cat_key]}: {total} entries")

        print(f"\nGenerated pages in {output_dir}")


if __name__ == "__main__":
    main()
