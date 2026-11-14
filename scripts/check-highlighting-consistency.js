#!/usr/bin/env node
// Highlighting consistency check (eu-ioz21.1).
//
// Classifies every token of a corpus of eucalypt files with three
// highlighters and reports where the editor grammars disagree with the
// rowan parser's LSP semantic tokens, the reference classification:
//
//   rowan        `eu lsp`, textDocument/semanticTokens/full
//   textmate     editors/vscode/syntaxes/eucalypt.tmLanguage.json via
//                vscode-textmate (editors/vscode/test/tokenise.js)
//   tree-sitter  editors/tree-sitter-eucalypt/queries/highlights.scm via
//                `tree-sitter query --captures`
//
// Each highlighter paints every UTF-16 code unit of a file with a fine
// class; the comparison is on a coarse projection of those classes (see
// COARSE), at the granularity where the highlighters are meant to agree.
// Two kinds of disagreement are reported:
//
//   mismatch  a rowan token whose extent the other highlighter does not
//             paint wholly with the same coarse class
//   missing   a run the other highlighter paints with a lexical or
//             declaration class (MISSING_CLASSES) that rowan leaves
//             without a token
//
// Disagreements matching a rule in
// editors/test-fixtures/highlighting/consistency-allowlist.json are
// accepted; any other disagreement fails the check (exit 1). Tooling
// failures exit 2.
//
// Prerequisites: a built `eu` (target/release/eu, target/debug/eu or
// --eu PATH), `npm ci` in editors/vscode, and `npm ci` in
// editors/tree-sitter-eucalypt (which provides the tree-sitter CLI and,
// through it, compiles the committed parser).
//
// Usage:
//   node scripts/check-highlighting-consistency.js [options] [files...]
//
//   --eu PATH        eu binary to run as the LSP server
//   --only NAME      check only `textmate` or `tree-sitter`
//   --summary        group disagreements (allowed ones too) by
//                    highlighter, kind, classes and text, with counts
//   --max N          print at most N unallowed disagreements (default 200)
//
// With no files, the corpus is the highlighting fixtures, lib/*.eu
// (without prelude.eu, whose names are all locals to rowan),
// tests/harness/**/*.eu outside errors/, and examples/aoc25/**/*.eu.

"use strict";

const fs = require("fs");
const path = require("path");
const { spawn, spawnSync } = require("child_process");

const ROOT = path.resolve(__dirname, "..");
const FIXTURES = path.join(ROOT, "editors", "test-fixtures", "highlighting");
const ALLOWLIST = path.join(FIXTURES, "consistency-allowlist.json");
const VSCODE = path.join(ROOT, "editors", "vscode");
const TS_DIR = path.join(ROOT, "editors", "tree-sitter-eucalypt");
// Files the tree-sitter grammar is known not to parse (gated by
// scripts/check-tree-sitter-corpus.py); tree-sitter is not compared on
// them.
const TS_EXPECTED_FAILURES = path.join(TS_DIR, "test", "corpus-expected-failures.txt");

// ---------------------------------------------------------------- classes

// Fine class -> coarse class compared across highlighters. Rowan looks
// names up in a symbol table rather than resolving scope, and does not
// distinguish call position, anaphora, constants or intrinsics, so every
// kind of name reference compares as `name`.
const COARSE = {
  comment: "comment",
  string: "string",
  escape: "string",
  doc: "string",
  number: "number",
  symbol: "symbol",
  operator: "operator",
  declaration: "declaration",
  parameter: "parameter",
  metadata: "metadata",
  punctuation: "punctuation",
  interpolation: "name",
  reference: "name",
  call: "name",
  prelude: "name",
  constant: "name",
  builtin: "name",
  anaphor: "name",
  keyword: "name",
};

// Coarse classes that rowan is expected to tokenise wherever another
// highlighter shows them.
const MISSING_CLASSES = new Set([
  "string",
  "number",
  "comment",
  "symbol",
  "operator",
  "declaration",
  "parameter",
  "metadata",
]);

// Rowan semantic token type (+ modifiers) -> fine class.
function rowanClass(type, modifiers) {
  const declaration = modifiers.includes("declaration");
  switch (type) {
    case "comment":
    case "number":
    case "string":
    case "operator":
    case "parameter":
      return type;
    case "enumMember":
      return "symbol";
    case "decorator":
      return "metadata";
    case "property":
    case "function":
      if (declaration) return "declaration";
      return modifiers.includes("defaultLibrary") ? "prelude" : "reference";
    case "variable":
      return modifiers.includes("defaultLibrary") ? "prelude" : "reference";
    default:
      throw new Error(`unknown rowan token type ${type}`);
  }
}

// TextMate scope prefix -> fine class, tried innermost scope first; the
// first scope with a matching prefix decides. Scopes matching no prefix
// (meta.*, markup.*, source.*) defer to the enclosing scope.
const TM_SCOPES = [
  ["comment", "comment"],
  ["string.other.format-spec", "string"],
  ["punctuation.separator.format-spec", "punctuation"],
  ["punctuation.section.interpolation", "punctuation"],
  ["variable.other.interpolation", "interpolation"],
  ["constant.character.escape", "escape"],
  ["punctuation.definition.string", "string"],
  ["string.quoted.double.documentation", "doc"],
  ["string", "string"],
  ["constant.numeric", "number"],
  ["constant.other.symbol", "symbol"],
  ["constant.language", "constant"],
  ["punctuation.definition.metadata", "metadata"],
  ["keyword.operator.metadata", "metadata"],
  ["keyword.other.metadata", "metadata"],
  ["entity.other.attribute-name.metadata", "metadata"],
  ["keyword.operator", "operator"],
  ["keyword", "keyword"],
  ["entity.name.function.call", "call"],
  ["entity.name.function", "declaration"],
  ["variable.other.property.declaration", "declaration"],
  ["variable.parameter", "parameter"],
  ["variable.language.anaphor", "anaphor"],
  ["support.function.builtin", "builtin"],
  ["support.function.prelude", "prelude"],
  ["support.variable.prelude", "prelude"],
  ["variable", "reference"],
  ["entity", "reference"],
  ["punctuation", "punctuation"],
];

function textmateClass(scopes) {
  for (let i = scopes.length - 1; i >= 0; i--) {
    const scope = scopes[i].replace(/\.eucalypt$/, "");
    for (const [prefix, cls] of TM_SCOPES) {
      if (scope === prefix || scope.startsWith(prefix + ".")) return cls;
    }
  }
  return null;
}

// highlights.scm capture name -> fine class, by longest matching prefix
// (`@function.call` before `@function`). A capture named `_...` is a
// predicate's helper: it still claims its node, which tree-sitter-highlight
// then leaves unstyled.
const TS_CAPTURES = {
  comment: "comment",
  "comment.documentation": "doc",
  number: "number",
  string: "string",
  "string.special": "string",
  "string.escape": "escape",
  constant: "symbol",
  variable: "interpolation",
  "variable.special": "anaphor",
  operator: "operator",
  punctuation: "punctuation",
  property: "declaration",
  function: "declaration",
  "function.call": "call",
  "function.builtin": "builtin",
  parameter: "parameter",
  keyword: "prelude",
  attribute: "metadata",
};

function treeSitterClass(capture) {
  if (capture.startsWith("_")) return null;
  let name = capture;
  while (name) {
    if (name in TS_CAPTURES) return TS_CAPTURES[name];
    const dot = name.lastIndexOf(".");
    name = dot < 0 ? "" : name.slice(0, dot);
  }
  throw new Error(`unmapped tree-sitter capture @${capture}`);
}

// ----------------------------------------------------------------- corpus

function walk(dir, out, skip) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      if (!skip || !skip(full)) walk(full, out, skip);
    } else if (entry.name.endsWith(".eu")) {
      out.push(full);
    }
  }
  return out;
}

function defaultCorpus() {
  const files = walk(FIXTURES, []);
  for (const f of fs.readdirSync(path.join(ROOT, "lib")).sort()) {
    if (f.endsWith(".eu") && f !== "prelude.eu") files.push(path.join(ROOT, "lib", f));
  }
  const errors = path.join(ROOT, "tests", "harness", "errors");
  walk(path.join(ROOT, "tests", "harness"), files, (d) => d === errors);
  walk(path.join(ROOT, "examples", "aoc25"), files);
  return files.sort();
}

// ------------------------------------------------------------ text layout

function lineStarts(text) {
  const starts = [0];
  for (let i = 0; i < text.length; i++) if (text[i] === "\n") starts.push(i + 1);
  return starts;
}

// UTF-16 offset of a (row, byte column) position.
function byteColumnToOffset(text, starts, row, byteCol) {
  let offset = starts[row];
  let bytes = 0;
  while (bytes < byteCol && offset < text.length) {
    const cp = text.codePointAt(offset);
    bytes += cp < 0x80 ? 1 : cp < 0x800 ? 2 : cp < 0x10000 ? 3 : 4;
    offset += cp >= 0x10000 ? 2 : 1;
  }
  return offset;
}

// ------------------------------------------------------------------ rowan

// Semantic tokens for every file, as [{ start, end, cls }] in UTF-16
// offsets, from one `eu lsp` process.
async function rowanTokens(eu, files, texts) {
  const child = spawn(eu, ["lsp"], { stdio: ["pipe", "pipe", "pipe"] });
  let stderr = "";
  child.stderr.on("data", (d) => (stderr += d));
  const failed = new Promise((_, reject) => {
    child.on("error", reject);
    child.on("exit", (code) => reject(new Error(`eu lsp exited (${code}):\n${stderr}`)));
  });

  const pending = new Map();
  let buffer = Buffer.alloc(0);
  const send = (msg) => {
    const body = Buffer.from(JSON.stringify({ jsonrpc: "2.0", ...msg }), "utf8");
    child.stdin.write(`Content-Length: ${body.length}\r\n\r\n`);
    child.stdin.write(body);
  };
  child.stdout.on("data", (chunk) => {
    buffer = Buffer.concat([buffer, chunk]);
    for (;;) {
      const sep = buffer.indexOf("\r\n\r\n");
      if (sep < 0) return;
      const length = Number(/Content-Length: (\d+)/i.exec(buffer.slice(0, sep).toString())[1]);
      if (buffer.length < sep + 4 + length) return;
      const msg = JSON.parse(buffer.slice(sep + 4, sep + 4 + length).toString("utf8"));
      buffer = buffer.slice(sep + 4 + length);
      if (msg.method && msg.id !== undefined) {
        send({ id: msg.id, result: null }); // server-to-client request
      } else if (msg.id !== undefined && pending.has(msg.id)) {
        const { resolve, reject } = pending.get(msg.id);
        pending.delete(msg.id);
        if (msg.error) reject(new Error(JSON.stringify(msg.error)));
        else resolve(msg.result);
      }
    }
  });
  let nextId = 1;
  const request = (method, params) =>
    Promise.race([
      failed,
      new Promise((resolve, reject) => {
        const id = nextId++;
        pending.set(id, { resolve, reject });
        send({ id, method, params });
      }),
    ]);

  const init = await request("initialize", {
    processId: process.pid,
    rootUri: null,
    capabilities: {
      textDocument: {
        semanticTokens: {
          requests: { full: true },
          tokenTypes: [],
          tokenModifiers: [],
          formats: ["relative"],
        },
      },
    },
  });
  const legend = init.capabilities.semanticTokensProvider.legend;
  send({ method: "initialized", params: {} });

  const result = new Map();
  for (const file of files) {
    const uri = "file://" + file;
    const text = texts.get(file);
    send({
      method: "textDocument/didOpen",
      params: { textDocument: { uri, languageId: "eucalypt", version: 1, text } },
    });
    const { data } = await request("textDocument/semanticTokens/full", { textDocument: { uri } });
    send({ method: "textDocument/didClose", params: { textDocument: { uri } } });
    const starts = lineStarts(text);
    const tokens = [];
    let line = 0;
    let col = 0;
    for (let i = 0; i < data.length; i += 5) {
      const [dLine, dStart, length, type, mods] = data.slice(i, i + 5);
      line += dLine;
      col = dLine === 0 ? col + dStart : dStart;
      const modifiers = legend.tokenModifiers.filter((_, bit) => mods & (1 << bit));
      const start = starts[line] + col;
      tokens.push({ start, end: start + length, cls: rowanClass(legend.tokenTypes[type], modifiers) });
    }
    result.set(file, tokens);
  }
  child.removeAllListeners("exit");
  await request("shutdown", null).catch(() => {});
  send({ method: "exit" });
  child.stdin.end();
  return result;
}

// --------------------------------------------------------------- textmate

async function textmatePaint(files, texts) {
  const { tokenise } = require(path.join(VSCODE, "test", "tokenise.js"));
  const result = new Map();
  for (const file of files) {
    const text = texts.get(file);
    const paint = new Array(text.length).fill(null);
    const starts = lineStarts(text);
    const lines = await tokenise(text);
    lines.forEach((tokens, row) => {
      for (const t of tokens) {
        const cls = textmateClass(t.scopes);
        for (let i = t.startIndex; i < t.endIndex; i++) paint[starts[row] + i] = cls;
      }
    });
    result.set(file, paint);
  }
  return result;
}

// ------------------------------------------------------------ tree-sitter

const CAPTURE_LINE =
  /^\s+pattern:\s*(\d+), capture: \d+ - ([\w.]+), start: \((\d+), (\d+)\), end: \((\d+), (\d+)\)/;

// Paints each file with the highlights.scm captures: an inner capture
// overrides an enclosing one and, of two captures of the same extent, the
// later pattern wins. This approximates tree-sitter-highlight; it does not
// model the highlighter dropping a whole multi-capture match when a later
// pattern claims one of its nodes (see the note atop highlights.scm). highlights.scm is written for that convention
// (Neovim's): its specific patterns (`@function` heads, prelude
// `@keyword`s, metadata's `@attribute` backtick) follow the general ones
// they refine.
function treeSitterPaint(allFiles, texts) {
  const failures = new Set(
    fs
      .readFileSync(TS_EXPECTED_FAILURES, "utf8")
      .split("\n")
      .map((l) => l.trim())
      .filter((l) => l && !l.startsWith("#")),
  );
  const files = allFiles.filter((f) => !failures.has(path.relative(ROOT, f)));
  const bin = path.join(TS_DIR, "node_modules", ".bin", "tree-sitter");
  const proc = spawnSync(bin, ["query", "--captures", "queries/highlights.scm", ...files], {
    cwd: TS_DIR,
    encoding: "utf8",
    maxBuffer: 1 << 30,
  });
  if (proc.error) throw proc.error;
  if (proc.status !== 0) throw new Error(`tree-sitter query failed:\n${proc.stderr}`);

  const fileSet = new Set(files);
  const lineIndex = new Map();
  const captures = new Map(files.map((f) => [f, []]));
  let current = null;
  for (const line of proc.stdout.split("\n")) {
    if (fileSet.has(line)) {
      current = line;
      continue;
    }
    const m = CAPTURE_LINE.exec(line);
    if (!m || !current) continue;
    const cls = treeSitterClass(m[2]);
    const text = texts.get(current);
    if (!lineIndex.has(current)) lineIndex.set(current, lineStarts(text));
    const starts = lineIndex.get(current);
    captures.get(current).push({
      pattern: Number(m[1]),
      start: byteColumnToOffset(text, starts, Number(m[3]), Number(m[4])),
      end: byteColumnToOffset(text, starts, Number(m[5]), Number(m[6])),
      cls,
    });
  }

  const result = new Map();
  for (const file of files) {
    const paint = new Array(texts.get(file).length).fill(null);
    const list = captures.get(file);
    // Paint wide captures first and, among equal extents, earlier patterns
    // first, so the innermost capture and the latest pattern end on top.
    list.sort((a, b) => b.end - b.start - (a.end - a.start) || a.pattern - b.pattern);
    for (const c of list) for (let i = c.start; i < c.end; i++) paint[i] = c.cls;
    result.set(file, paint);
  }
  return result;
}

// ------------------------------------------------------------- comparison

function compare(highlighter, file, text, tokens, paint) {
  const out = [];
  const coarse = (cls) => (cls ? COARSE[cls] : "none");
  const covered = new Uint8Array(text.length);
  for (const tok of tokens) {
    const seen = new Set();
    for (let i = tok.start; i < tok.end; i++) {
      covered[i] = 1;
      if (text[i] !== "\n") seen.add(coarse(paint[i]));
    }
    const expected = coarse(tok.cls);
    if (seen.size === 1 && seen.has(expected)) continue;
    out.push({
      highlighter,
      file,
      start: tok.start,
      kind: "mismatch",
      rowan: expected,
      other: [...seen].sort().join("+") || "none",
      text: text.slice(tok.start, tok.end),
    });
  }
  for (let i = 0; i < text.length; ) {
    const cls = coarse(paint[i]);
    if (covered[i] || !MISSING_CLASSES.has(cls) || /\s/.test(text[i])) {
      i++;
      continue;
    }
    let j = i + 1;
    while (j < text.length && !covered[j] && coarse(paint[j]) === cls && text[j] !== "\n") j++;
    out.push({
      highlighter,
      file,
      start: i,
      kind: "missing",
      rowan: "none",
      other: cls,
      text: text.slice(i, j).trimEnd(),
    });
    i = j;
  }
  return out;
}

// -------------------------------------------------------------- allow-list

function globToRegExp(glob) {
  let re = "";
  for (let i = 0; i < glob.length; i++) {
    const c = glob[i];
    if (c === "*" && glob[i + 1] === "*") {
      re += ".*";
      i++;
      if (glob[i + 1] === "/") i++;
    } else if (c === "*") re += "[^/]*";
    else if (c === "?") re += "[^/]";
    else re += c.replace(/[.+^${}()|[\]\\]/g, "\\$&");
  }
  return new RegExp(`^${re}$`);
}

function loadAllowlist() {
  const rules = JSON.parse(fs.readFileSync(ALLOWLIST, "utf8")).rules;
  return rules.map((rule, index) => {
    for (const key of ["highlighter", "kind", "rowan", "other", "reason", "ref"]) {
      if (!(key in rule)) throw new Error(`allow-list rule ${index} has no "${key}"`);
    }
    return {
      ...rule,
      index,
      used: 0,
      textRe: rule.text === undefined ? null : new RegExp(`^(?:${rule.text})$`, "u"),
      fileRes: (rule.files || ["**"]).map(globToRegExp),
    };
  });
}

function matches(rule, d) {
  const field = (want, got) => want === "*" || want === got;
  return (
    field(rule.highlighter, d.highlighter) &&
    field(rule.kind, d.kind) &&
    field(rule.rowan, d.rowan) &&
    field(rule.other, d.other) &&
    (!rule.textRe || rule.textRe.test(d.text)) &&
    rule.fileRes.some((re) => re.test(d.rel))
  );
}

// ------------------------------------------------------------------- main

function parseArgs(argv) {
  const opts = { only: null, summary: false, max: 200, files: [] };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--eu") opts.eu = argv[++i];
    else if (a === "--only") opts.only = argv[++i];
    else if (a === "--summary") opts.summary = true;
    else if (a === "--max") opts.max = Number(argv[++i]);
    else if (a.startsWith("--")) throw new Error(`unknown option ${a}`);
    else opts.files.push(path.resolve(a));
  }
  if (opts.only && !["textmate", "tree-sitter"].includes(opts.only)) {
    throw new Error(`--only takes textmate or tree-sitter, not ${opts.only}`);
  }
  if (!opts.eu) {
    // The more recently built of the release and debug binaries.
    opts.eu = ["release", "debug"]
      .map((p) => path.join(ROOT, "target", p, "eu"))
      .filter((p) => fs.existsSync(p))
      .sort((a, b) => fs.statSync(b).mtimeMs - fs.statSync(a).mtimeMs)[0];
    if (!opts.eu) throw new Error("no eu binary in target/: build one or pass --eu");
  }
  return opts;
}

function position(text, offset) {
  const before = text.slice(0, offset);
  const line = before.split("\n").length;
  return `${line}:${offset - before.lastIndexOf("\n")}`;
}

async function main() {
  const opts = parseArgs(process.argv.slice(2));
  const files = opts.files.length ? opts.files : defaultCorpus();
  const texts = new Map(files.map((f) => [f, fs.readFileSync(f, "utf8")]));
  const allowlist = loadAllowlist();

  const rowan = await rowanTokens(opts.eu, files, texts);
  const painters = [];
  if (opts.only !== "tree-sitter") painters.push(["textmate", await textmatePaint(files, texts)]);
  if (opts.only !== "textmate") painters.push(["tree-sitter", treeSitterPaint(files, texts)]);

  const unallowed = [];
  const groups = new Map();
  for (const [name, paints] of painters) {
    for (const file of files) {
      if (!paints.has(file)) continue;
      const text = texts.get(file);
      for (const d of compare(name, file, text, rowan.get(file), paints.get(file))) {
        d.rel = path.relative(ROOT, file);
        const rule = allowlist.find((r) => matches(r, d));
        if (rule) rule.used++;
        else unallowed.push(d);
        if (opts.summary) {
          const key = `${d.highlighter} ${d.kind} rowan=${d.rowan} other=${d.other} ${JSON.stringify(d.text)}`;
          const g = groups.get(key) || { count: 0, rule: rule ? rule.index : "-", example: null };
          g.count++;
          g.example ||= `${d.rel}:${position(text, d.start)}`;
          groups.set(key, g);
        }
      }
    }
  }

  if (opts.summary) {
    const rows = [...groups].sort((a, b) => b[1].count - a[1].count);
    for (const [key, g] of rows) console.log(`${String(g.count).padStart(6)}  rule ${g.rule}  ${key}  (${g.example})`);
    console.log("");
  }

  const checked = painters.map(([n]) => n).join(" and ");
  console.log(`Checked ${files.length} files with ${checked} against rowan semantic tokens.`);
  for (const rule of allowlist) {
    if (!rule.used) {
      console.log(`warning: allow-list rule ${rule.index} (${rule.ref}) matched nothing: ${rule.reason}`);
    }
  }
  if (unallowed.length === 0) {
    console.log("No disagreements outside the allow-list.");
    return 0;
  }
  console.log(`${unallowed.length} disagreement(s) outside the allow-list:`);
  for (const d of unallowed.slice(0, opts.max)) {
    console.log(
      `  ${d.rel}:${position(texts.get(d.file), d.start)}  ${d.highlighter} ${d.kind}: ` +
        `rowan=${d.rowan} ${d.highlighter}=${d.other}  ${JSON.stringify(d.text)}`,
    );
  }
  if (unallowed.length > opts.max) console.log(`  ... and ${unallowed.length - opts.max} more`);
  return 1;
}

main().then(
  (code) => process.exit(code),
  (err) => {
    console.error(`check-highlighting-consistency: ${err.stack || err}`);
    process.exit(2);
  },
);
