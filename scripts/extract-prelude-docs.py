#!/usr/bin/env python3
"""Extract documentation from lib/prelude.eu and generate markdown reference pages.

Usage:
    python3 scripts/extract-prelude-docs.py --summary    # Show extraction summary
    python3 scripts/extract-prelude-docs.py --check      # Check coverage
    python3 scripts/extract-prelude-docs.py --generate   # Generate reference pages
"""

import argparse
import os
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional


@dataclass
class PreludeEntry:
    """A single documented declaration from the prelude."""
    name: str
    signature: str
    description: str
    section: str
    namespace: str = ""
    is_operator: bool = False
    is_suppressed: bool = False
    precedence: Optional[str] = None
    associates: Optional[str] = None
    line_number: int = 0


# ── Section to category mapping ──────────────────────────────────────────

SECTION_TO_CATEGORY = {
    "Prelude versioning and run metadata": "io",
    "Random number generation": "random",
    "Error / debug support": "booleans",
    "Essentials": "booleans",
    "List basics": "lists",
    "Blocks / merge": "blocks",
    "Deep find — recursive key search": "blocks",
    "Deep query — pattern-based data querying": "blocks",
    "Boolean": "booleans",
    "Polymorphic equality": "booleans",
    "Arithmetic": "numbers",
    "Text and regexes": "strings",
    "Combinators": "combinators",
    "Sets": "sets",
}

# Namespace to category mapping
NAMESPACE_TO_CATEGORY = {
    "eu": "io",
    "io": "io",
    "str": "strings",
    "ch": "strings",
    "cal": "calendar",
    "set": "sets",
    "assertions": "metadata",
}

# Single-line section (#) to category mapping
SINGLE_SECTION_TO_CATEGORY = {
    "Utilities": "combinators",
    "Metadata basics": "metadata",
    "List library functions, maps and folds": "lists",
    "Block library functions": "blocks",
    "By property alteration of blocks": "blocks",
}

# Known namespaces that open with name: {
KNOWN_NAMESPACES = {"eu", "io", "str", "cal", "set", "ch", "assertions", "_block"}


# ── Parsing ──────────────────────────────────────────────────────────────

def parse_doc_string(lines: list[str], start: int) -> tuple[dict, int]:
    """Parse a backtick doc string starting at line `start`.

    Returns (metadata_dict, next_line_index).
    metadata_dict has at least 'doc' key if documentation found.
    """
    line = lines[start].strip()
    if not line.startswith('`'):
        return {}, start

    # Content after the backtick
    content = line[1:].strip()

    # Simple string doc: ` "..."
    if content.startswith('"'):
        # Single-line string
        match = re.match(r'^"((?:[^"\\]|\\.)*)"', content)
        if match:
            return {"doc": match.group(1)}, start + 1
        # Multi-line string - keep collecting
        doc_text = content[1:]  # strip opening quote
        idx = start + 1
        while idx < len(lines):
            l = lines[idx]
            if '"' in l:
                doc_text += " " + l.strip().rstrip('"')
                return {"doc": doc_text.strip()}, idx + 1
            doc_text += " " + l.strip()
            idx += 1
        return {"doc": doc_text.strip()}, idx

    # Block metadata: ` { ... }
    if content.startswith('{'):
        block_text = content
        idx = start
        brace_depth = content.count('{') - content.count('}')
        while brace_depth > 0 and idx + 1 < len(lines):
            idx += 1
            block_text += " " + lines[idx].strip()
            brace_depth += lines[idx].count('{') - lines[idx].count('}')

        meta = {}
        # Extract doc
        doc_match = re.search(r'doc:\s*"((?:[^"\\]|\\.)*)"', block_text)
        if doc_match:
            # Collapse whitespace in multi-line doc strings
            doc = doc_match.group(1)
            doc = re.sub(r'\s+', ' ', doc).strip()
            meta["doc"] = doc
        # Extract export
        exp_match = re.search(r'export:\s*:(\w+)', block_text)
        if exp_match:
            meta["export"] = exp_match.group(1)
        # Extract precedence
        prec_match = re.search(r'precedence:\s*:?([\w-]+)', block_text)
        if prec_match:
            meta["precedence"] = prec_match.group(1)
        # Extract associates
        assoc_match = re.search(r'associates:\s*:(\w+)', block_text)
        if assoc_match:
            meta["associates"] = assoc_match.group(1)
        return meta, idx + 1

    return {}, start


def parse_declaration(line: str) -> tuple[str, str, bool]:
    """Parse a declaration line.

    Returns (name, signature, is_operator).
    """
    stripped = line.strip()

    # Operator: (l op r): ... or (op x): ...
    op_match = re.match(r'^\(([^)]+)\)\s*:', stripped)
    if op_match:
        sig = op_match.group(1).strip()
        # Extract operator symbol
        parts = sig.split()
        if len(parts) == 3:
            name = parts[1]
        elif len(parts) == 2:
            name = parts[0]
        else:
            name = sig
        return name, f"({sig})", True

    # Regular: name(args): ... or name: ...
    decl_match = re.match(r'^([a-zA-Z_∅][\w?!∅.-]*(?:\([^)]*\))?)\s*:', stripped)
    if decl_match:
        sig = decl_match.group(1)
        name = sig.split('(')[0]
        return name, sig, False

    return "", "", False


def parse_prelude(prelude_path: str) -> list[PreludeEntry]:
    """Parse the entire prelude file and extract documented entries."""
    with open(prelude_path) as f:
        lines = [l.rstrip('\n') for l in f.readlines()]

    entries = []
    current_section = ""
    current_namespace = ""
    namespace_stack = []  # list of (name, brace_depth_at_open)
    brace_depth = 0
    pending_doc = None
    pending_meta = {}

    i = 0
    while i < len(lines):
        line = lines[i]
        stripped = line.strip()

        # Skip empty lines
        if not stripped:
            i += 1
            continue

        # ── Section comment detection ──

        # Double-hash section headers: ## ... ##
        if stripped == '##':
            # Collect multi-line section comment
            section_lines = []
            j = i + 1
            while j < len(lines):
                sl = lines[j].strip()
                if sl == '##':
                    break
                if sl.startswith('##'):
                    section_lines.append(sl[2:].strip())
                j += 1
            if section_lines:
                current_section = "\n".join(section_lines)
                # Normalise known multi-line section names
                for known in SECTION_TO_CATEGORY:
                    if current_section.startswith(known):
                        current_section = known
                        break
            i = j + 1
            continue

        # Single-hash section comment: # Section Name
        single_section_match = re.match(r'^#\s+(.+)$', stripped)
        if single_section_match and not stripped.startswith('##'):
            sec_name = single_section_match.group(1).strip()
            if sec_name in SINGLE_SECTION_TO_CATEGORY:
                current_section = sec_name
            i += 1
            continue

        # ── Brace depth tracking (exclude strings) ──
        line_for_braces = re.sub(r'"[^"]*"', '', line)
        open_b = line_for_braces.count('{')
        close_b = line_for_braces.count('}')

        # ── Backtick doc string ──
        if stripped.startswith('`') and not stripped.startswith('`_'):
            pending_meta, next_i = parse_doc_string(lines, i)
            pending_doc = pending_meta.get("doc", "")
            i = next_i
            continue

        # ── Namespace detection ──
        ns_match = re.match(r'^(\w[\w-]*)\s*:\s*\{', stripped)
        if ns_match and ns_match.group(1) in KNOWN_NAMESPACES:
            ns_name = ns_match.group(1)
            namespace_stack.append((ns_name, brace_depth))
            current_namespace = ns_name
            brace_depth += (open_b - close_b)
            # Consume the namespace-level doc string
            pending_doc = None
            pending_meta = {}
            i += 1
            continue

        # Update brace depth
        brace_depth += (open_b - close_b)

        # Check if we've exited namespaces
        while namespace_stack and brace_depth <= namespace_stack[-1][1]:
            namespace_stack.pop()
            current_namespace = (
                namespace_stack[-1][0] if namespace_stack else ""
            )

        # ── Internal helper (starts with _), skip ──
        if stripped.startswith('_') and not stripped.startswith('('):
            pending_doc = None
            pending_meta = {}
            i += 1
            continue

        # ── Declaration detection ──
        if pending_doc is not None and stripped and not stripped.startswith('#'):
            name, sig, is_op = parse_declaration(stripped)
            if name:
                entry = PreludeEntry(
                    name=name,
                    signature=sig,
                    description=pending_doc,
                    section=current_section,
                    namespace=current_namespace,
                    is_operator=is_op,
                    is_suppressed=pending_meta.get("export") == "suppress",
                    precedence=pending_meta.get("precedence"),
                    associates=pending_meta.get("associates"),
                    line_number=i + 1,
                )
                entries.append(entry)
                pending_doc = None
                pending_meta = {}
                i += 1
                continue

        i += 1

    return entries


# ── Categorisation ───────────────────────────────────────────────────────

def categorise_entries(
    entries: list[PreludeEntry],
) -> dict[str, list[PreludeEntry]]:
    """Group entries by category."""
    categories: dict[str, list[PreludeEntry]] = {}

    for entry in entries:
        # Determine category
        if entry.namespace:
            cat = NAMESPACE_TO_CATEGORY.get(entry.namespace, "io")
        elif entry.section in SECTION_TO_CATEGORY:
            cat = SECTION_TO_CATEGORY[entry.section]
        elif entry.section in SINGLE_SECTION_TO_CATEGORY:
            cat = SINGLE_SECTION_TO_CATEGORY[entry.section]
        else:
            cat = "io"  # fallback

        if cat not in categories:
            categories[cat] = []
        categories[cat].append(entry)

    return categories


# ── Formatting ───────────────────────────────────────────────────────────

def format_signature(entry: PreludeEntry) -> str:
    """Format signature for display in a markdown table cell."""
    sig = entry.signature

    if entry.namespace and entry.namespace not in ("assertions", "_block"):
        if not entry.is_operator:
            sig = f"{entry.namespace}.{sig}"

    # Escape pipe for markdown tables
    sig = sig.replace('|', '\\|')

    return f"`{sig}`"


def format_description(entry: PreludeEntry) -> str:
    """Format description for display in a markdown table cell.

    Strips leading signature prefix patterns like `name(args) -` from
    the doc string, since the signature is shown in its own column.
    """
    doc = entry.description
    if not doc:
        return ""

    # Strip single leading backtick only if it's unmatched
    if doc.startswith('`'):
        if doc.count('`') % 2 == 1:
            doc = doc[1:].strip()

    # Strip backtick-quoted signature prefix: `name(args)` - description
    doc = re.sub(r'^`[^`]*`\s*[-\u2014\u2013]\s*', '', doc)

    # Strip signature prefix: name(args) - description
    prefix_re = re.compile(
        r"^'?"                              # optional leading quote
        r"[a-zA-Z_∅∸¬↑!][\w?!∅.,-]*"       # name (incl ! prefix)
        r"(?:\([^)]*\))?"                   # optional (args)
        r"`?"                               # optional trailing backtick
        r"\s*[-\u2014\u2013]\s*"            # dash separator
    )
    doc = prefix_re.sub('', doc)

    # Also handle operator prefix: (l op r) - description
    op_prefix_re = re.compile(
        r"^`?\([^)]+\)`?\s*[-\u2014\u2013]\s*"
    )
    doc = op_prefix_re.sub('', doc)

    # Strip trailing unmatched backtick
    if doc.endswith('`') and doc.count('`') % 2 == 1:
        doc = doc[:-1].strip()

    # Capitalise first letter
    if doc and doc[0].islower():
        doc = doc[0].upper() + doc[1:]

    # Escape pipe for markdown tables
    doc = doc.replace('|', '\\|')

    # Remove trailing period
    doc = doc.rstrip('.')

    return doc


# ── Supplement system ────────────────────────────────────────────────────

def section_slug(display_name: str) -> str:
    """Convert a section display name to a slug for supplement lookup."""
    slug = display_name.lower()
    slug = re.sub(r'[^a-z0-9]+', '-', slug)
    slug = slug.strip('-')
    return slug


def load_supplement(category: str, slug: str, supplements_dir: str) -> str:
    """Load a supplement file if it exists."""
    path = os.path.join(supplements_dir, category, f"{slug}.md")
    if os.path.exists(path):
        with open(path) as f:
            content = f.read().strip()
        return content
    return ""


# ── Page generation ──────────────────────────────────────────────────────

PAGE_CONFIG = {
    "lists": {
        "title": "Lists",
        "sections": [
            {"display": "Basic Operations",
             "sources": ["List basics"]},
            {"display": "List Construction",
             "filter": lambda e: e.name in {
                 "repeat", "ints-from", "range", "cycle", "iterate", "nil",
             }},
            {"display": "Transformations",
             "filter": lambda e: e.name in {
                 "map", "<$>", "map2", "filter", "remove", "reverse",
                 "take", "drop", "take-while", "take-until",
                 "drop-while", "drop-until", "cross",
             }},
            {"display": "Combining Lists",
             "filter": lambda e: e.name in {
                 "append", "++", "prepend", "concat", "mapcat",
                 "zip", "zip-with", "zip-apply",
             }},
            {"display": "Splitting Lists",
             "filter": lambda e: e.name in {
                 "split-at", "split-after", "split-when",
                 "window", "partition", "discriminate",
             }},
            {"display": "Folds and Scans",
             "filter": lambda e: e.name in {
                 "foldl", "foldr", "scanl", "scanr",
             }},
            {"display": "Predicates",
             "filter": lambda e: e.name in {
                 "all", "all-true?", "any", "any-true?",
             }},
            {"display": "Sorting",
             "filter": lambda e: e.name in {
                 "qsort", "sort-nums", "sort-strs", "sort-zdts",
                 "sort-by", "sort-by-num", "sort-by-str", "sort-by-zdt",
                 "group-by",
             }},
            {"display": "Other",
             "filter": lambda e: e.name in {
                 "over-sliding-pairs", "differences", "count", "last",
                 "nth", "!!",
             }},
        ],
    },
    "blocks": {
        "title": "Blocks",
        "sections": [
            {"display": "Block Construction and Merging",
             "sources": ["Blocks / merge"]},
            {"display": "Block Utilities",
             "sources": ["Block library functions"]},
            {"display": "Block Alteration",
             "sources": ["By property alteration of blocks"]},
            {"display": "Deep Find and Query",
             "sources": [
                 "Deep find — recursive key search",
                 "Deep query — pattern-based data querying",
             ]},
        ],
    },
    "strings": {
        "title": "Strings",
        "sections": [
            {"display": "String Processing",
             "filter": lambda e: e.namespace == "str"},
            {"display": "Character Constants",
             "filter": lambda e: e.namespace == "ch"},
        ],
    },
    "numbers": {
        "title": "Numbers and Arithmetic",
        "sections": [
            {"display": "Arithmetic Operators",
             "filter": lambda e: e.is_operator},
            {"display": "Numeric Functions",
             "filter": lambda e: not e.is_operator},
        ],
    },
    "booleans": {
        "title": "Booleans and Comparison",
        "sections": [
            {"display": "Essentials",
             "sources": ["Essentials"]},
            {"display": "Error and Debug Support",
             "sources": ["Error / debug support"]},
            {"display": "Boolean Logic",
             "sources": ["Boolean"]},
            {"display": "Equality and Comparison",
             "sources": ["Polymorphic equality"]},
        ],
    },
    "combinators": {
        "title": "Combinators",
        "sections": [
            {"display": "Combinators",
             "sources": ["Combinators"]},
            {"display": "Utilities",
             "sources": ["Utilities"]},
        ],
    },
    "calendar": {
        "title": "Calendar",
        "sections": [
            {"display": "Date and Time Functions",
             "filter": lambda _: True},
        ],
    },
    "sets": {
        "title": "Sets",
        "sections": [
            {"display": "Set Operations",
             "filter": lambda _: True},
        ],
    },
    "random": {
        "title": "Random Numbers",
        "sections": [
            {"display": "Random Number Generation",
             "filter": lambda _: True},
        ],
    },
    "metadata": {
        "title": "Metadata",
        "sections": [
            {"display": "Metadata Basics",
             "sources": ["Metadata basics"]},
            {"display": "Assertions",
             "filter": lambda e:
                e.namespace == "assertions" or e.name.startswith("//")},
        ],
    },
    "io": {
        "title": "IO",
        "sections": [
            {"display": "Prelude Versioning",
             "filter": lambda e: e.namespace == "eu"},
            {"display": "IO Functions",
             "filter": lambda e: e.namespace == "io"},
        ],
    },
}


def entries_for_section(
    all_entries: list[PreludeEntry],
    section_config: dict,
) -> list[PreludeEntry]:
    """Select entries that belong to a section based on its config."""
    if "sources" in section_config:
        sources = section_config["sources"]
        return [e for e in all_entries if e.section in sources]
    elif "filter" in section_config:
        return [e for e in all_entries if section_config["filter"](e)]
    return []


def generate_table(entries: list[PreludeEntry]) -> str:
    """Generate a markdown table from a list of entries."""
    if not entries:
        return ""

    lines = ["| Function | Description |", "|----------|-------------|"]
    for entry in entries:
        sig = format_signature(entry)
        desc = format_description(entry)
        lines.append(f"| {sig} | {desc} |")

    return "\n".join(lines)


def generate_page(
    category: str,
    all_entries: list[PreludeEntry],
    supplements_dir: str,
) -> str:
    """Generate a complete markdown page for a category."""
    config = PAGE_CONFIG[category]
    parts = [f"# {config['title']}"]

    # Top-level supplement (preamble)
    top_supp = load_supplement(category, "top", supplements_dir)
    if top_supp:
        parts.append("")
        parts.append(top_supp)

    # Track which entries have been assigned to a section
    used_entries: set[int] = set()

    for section_config in config["sections"]:
        display = section_config["display"]
        section_entries = entries_for_section(all_entries, section_config)

        # Filter to public (non-suppressed) entries
        public_entries = [e for e in section_entries if not e.is_suppressed]

        # Deduplicate by (name, line) and track used entries
        seen = set()
        unique_entries = []
        for e in public_entries:
            key = (e.name, e.line_number)
            if key not in seen and id(e) not in used_entries:
                seen.add(key)
                unique_entries.append(e)
                used_entries.add(id(e))

        # Check for section supplement even if no entries
        slug = section_slug(display)
        supp = load_supplement(category, slug, supplements_dir)

        if not unique_entries and not supp:
            continue

        parts.append("")
        parts.append(f"## {display}")
        parts.append("")

        if unique_entries:
            table = generate_table(unique_entries)
            parts.append(table)

        if supp:
            if unique_entries:
                parts.append("")
            parts.append(supp)

    # Default supplement (bottom of page)
    default_supp = load_supplement(category, "default", supplements_dir)
    if default_supp:
        parts.append("")
        parts.append(default_supp)

    # Remaining entries not assigned to any section
    remaining = [
        e for e in all_entries
        if id(e) not in used_entries and not e.is_suppressed
    ]
    if remaining:
        parts.append("")
        parts.append("## Other")
        parts.append("")
        parts.append(generate_table(remaining))

    parts.append("")
    return "\n".join(parts)


def generate_index(categories: dict[str, list[PreludeEntry]]) -> str:
    """Generate the prelude index page."""
    parts = [
        "# Prelude Reference",
        "",
        "The eucalypt **prelude** is a standard library of functions, operators,",
        "and constants that is automatically loaded before your code runs.",
        "",
        "You can suppress the prelude with `-Q` if needed, though this leaves",
        "a very bare environment (even `true`, `false`, and `if` are defined",
        "in the prelude).",
        "",
        "## Categories",
        "",
    ]

    category_info = [
        ("lists", "Lists", "list construction, transformation, folding, sorting"),
        ("blocks", "Blocks", "block construction, access, merging, transformation"),
        ("strings", "Strings", "string manipulation, regex, formatting"),
        ("numbers", "Numbers and Arithmetic", "numeric operations and predicates"),
        ("booleans", "Booleans and Comparison",
         "boolean logic and comparison operators"),
        ("combinators", "Combinators",
         "function composition, application, utilities"),
        ("calendar", "Calendar", "date and time functions"),
        ("sets", "Sets", "set operations"),
        ("random", "Random Numbers", "random number generation"),
        ("metadata", "Metadata", "metadata and assertion functions"),
        ("io", "IO", "environment, time, and argument access"),
    ]

    for cat_key, cat_title, cat_desc in category_info:
        count = len([
            e for e in categories.get(cat_key, [])
            if not e.is_suppressed
        ])
        parts.append(
            f"- [{cat_title}]({cat_key}.md) -- {cat_desc} ({count} entries)"
        )

    total = sum(
        len([e for e in entries if not e.is_suppressed])
        for entries in categories.values()
    )
    parts.append("")
    parts.append(f"*{total} documented entries in total.*")
    parts.append("")

    return "\n".join(parts)


# ── Coverage check ───────────────────────────────────────────────────────

def check_coverage(
    entries: list[PreludeEntry],
    categories: dict[str, list[PreludeEntry]],
):
    """Check and report documentation coverage."""
    public = [e for e in entries if not e.is_suppressed]
    documented = [e for e in public if e.description]
    undocumented = [e for e in public if not e.description]

    print(f"Total entries extracted: {len(entries)}")
    print(f"Public entries: {len(public)}")
    print(f"Suppressed entries: {len(entries) - len(public)}")
    print(f"Documented: {len(documented)}")
    print(f"Undocumented: {len(undocumented)}")
    print()

    if undocumented:
        print("UNDOCUMENTED entries:")
        for e in undocumented:
            ns = f"{e.namespace}." if e.namespace else ""
            print(
                f"  - {ns}{e.name} (line {e.line_number},"
                f" section: {e.section})"
            )
    else:
        print("All public entries are documented.")

    print()
    print("Coverage by category:")
    for cat_key in PAGE_CONFIG:
        cat_entries = categories.get(cat_key, [])
        cat_public = [e for e in cat_entries if not e.is_suppressed]
        cat_doc = [e for e in cat_public if e.description]
        print(f"  {cat_key}: {len(cat_doc)}/{len(cat_public)} documented")


# ── Main ─────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description="Extract and generate prelude documentation"
    )
    parser.add_argument(
        "--summary", action="store_true",
        help="Show extraction summary",
    )
    parser.add_argument(
        "--check", action="store_true",
        help="Check documentation coverage",
    )
    parser.add_argument(
        "--generate", action="store_true",
        help="Generate markdown reference pages",
    )
    parser.add_argument(
        "--root", default=None,
        help="Project root directory (default: auto-detect)",
    )
    args = parser.parse_args()

    if not (args.summary or args.check or args.generate):
        parser.print_help()
        sys.exit(1)

    # Find project root
    if args.root:
        root = Path(args.root)
    else:
        root = Path(__file__).resolve().parent.parent

    prelude_path = root / "lib" / "prelude.eu"
    output_dir = root / "doc" / "reference" / "prelude"
    supplements_dir = output_dir / "supplements"

    if not prelude_path.exists():
        print(f"Error: prelude not found at {prelude_path}", file=sys.stderr)
        sys.exit(1)

    # Parse
    entries = parse_prelude(str(prelude_path))
    categories = categorise_entries(entries)

    if args.summary:
        print(f"Extracted {len(entries)} entries from {prelude_path}")
        public = [e for e in entries if not e.is_suppressed]
        print(f"Public: {len(public)}, Suppressed: {len(entries) - len(public)}")
        print()
        for cat_key in PAGE_CONFIG:
            cat_entries = categories.get(cat_key, [])
            cat_public = [e for e in cat_entries if not e.is_suppressed]
            print(f"  {cat_key}: {len(cat_public)} public entries")

    if args.check:
        check_coverage(entries, categories)

    if args.generate:
        os.makedirs(output_dir, exist_ok=True)

        # Generate category pages
        for cat_key in PAGE_CONFIG:
            cat_entries = categories.get(cat_key, [])
            page = generate_page(cat_key, cat_entries, str(supplements_dir))
            outpath = output_dir / f"{cat_key}.md"
            with open(outpath, 'w') as f:
                f.write(page)
            public_count = len([
                e for e in cat_entries if not e.is_suppressed
            ])
            print(f"  Generated {outpath.name} ({public_count} public entries)")

        # Generate index
        index = generate_index(categories)
        index_path = output_dir / "index.md"
        with open(index_path, 'w') as f:
            f.write(index)
        print(f"  Generated {index_path.name}")

        print(f"\nDone. Generated pages in {output_dir}")


if __name__ == "__main__":
    main()
