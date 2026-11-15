#!/usr/bin/env node
// Regenerate the prelude name patterns in the TextMate grammar from
// lib/prelude.eu.
//
// The LSP's symbol table (src/driver/lsp/symbol_table.rs) takes the
// prelude's top-level declarations and classifies each as a function (a
// head with parameters) or a property. The grammar mirrors that split:
// functions get support.function.prelude.eucalypt and properties
// support.variable.prelude.eucalypt. Operator and bracket declarations
// are highlighted by the grammar's operator rules instead.
//
// Usage:
//   node scripts/generate-prelude-names.js          rewrite the grammar
//   node scripts/generate-prelude-names.js --check  exit 1 if it is stale

"use strict";

const fs = require("fs");
const path = require("path");

const REPO = path.resolve(__dirname, "..", "..", "..");
const PRELUDE = path.join(REPO, "lib", "prelude.eu");
const GRAMMAR = path.resolve(__dirname, "..", "syntaxes", "eucalypt.tmLanguage.json");

// Names the grammar scopes as language constants rather than as prelude
// properties.
const LANGUAGE_CONSTANTS = new Set(["true", "false", "null"]);

// A top-level declaration head starts in column 0 with a normal
// identifier (the lexer's is_normal_start / is_normal_continuation).
// Nested declarations are indented and are not part of the symbol table.
const HEAD = /^([\p{Alphabetic}_•$?][\p{Alphabetic}\p{N}_$?!\-*]*)(\s*:(?![\p{Alphabetic}_•$?'])|[(\[{])/u;

function preludeNames(source) {
  const functions = new Set();
  const properties = new Set();
  for (const line of source.split("\n")) {
    const m = HEAD.exec(line);
    if (!m) continue;
    const [, name, follow] = m;
    if (follow.trim() === ":") {
      if (!LANGUAGE_CONSTANTS.has(name)) properties.add(name);
    } else {
      functions.add(name);
    }
  }
  // A name declared both ways keeps the first-declared kind in the LSP;
  // none currently are, but prefer the function reading if one appears.
  for (const name of functions) properties.delete(name);
  const sorted = (s) => [...s].sort((a, b) => b.length - a.length || a.localeCompare(b));
  return { functions: sorted(functions), properties: sorted(properties) };
}

function escape(name) {
  return name.replace(/[?*$!.\\-]/g, (c) => "\\" + c);
}

// Bounded by the lexer's identifier characters on both sides, and not
// preceded by '.', where a name is a block member rather than the prelude
// binding.
function namePattern(names) {
  return (
    "(?<![\\p{Alphabetic}\\p{N}_$?!\\-*•.])(?:" +
    names.map(escape).join("|") +
    ")(?![\\p{Alphabetic}\\p{N}_$?!\\-*])"
  );
}

function main() {
  const check = process.argv.includes("--check");
  const { functions, properties } = preludeNames(fs.readFileSync(PRELUDE, "utf8"));
  const text = fs.readFileSync(GRAMMAR, "utf8");
  const grammar = JSON.parse(text);
  grammar.repository["prelude-function"].match = namePattern(functions);
  grammar.repository["prelude-variable"].match = namePattern(properties);
  const updated = JSON.stringify(grammar, null, 2) + "\n";
  if (check) {
    if (updated !== text) {
      console.error(
        "eucalypt.tmLanguage.json is out of date with lib/prelude.eu or its canonical form;\n" +
          "run: node scripts/generate-prelude-names.js"
      );
      process.exit(1);
    }
    return;
  }
  fs.writeFileSync(GRAMMAR, updated);
  console.log(`prelude: ${functions.length} functions, ${properties.length} properties`);
}

module.exports = { preludeNames, namePattern };

if (require.main === module) main();
