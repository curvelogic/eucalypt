// Unit tests for the pure highlighter core. Run with: node --test
import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import {
  tokenize,
  tokenizeToLines,
  highlightFileLines,
  highlightDiffRows,
  highlightOneLine,
  CLASS,
} from "../src/eucalypt-highlight.mjs";

const here = dirname(fileURLToPath(import.meta.url));

// Find the class assigned to the first token whose text equals `text`.
function classOf(src, text) {
  const tok = tokenize(src).find((t) => t.t === text);
  return tok ? tok.c : undefined;
}

test("comments", () => {
  assert.equal(classOf("# hello", "# hello"), CLASS.comment);
});

test("property vs function declaration heads", () => {
  assert.equal(classOf("region: 1", "region"), CLASS.property);
  assert.equal(classOf("f(x): x", "f"), CLASS.function);
  assert.equal(classOf("g{a}: a", "g"), CLASS.function);
});

test("keywords and prelude are distinguished from variables", () => {
  assert.equal(classOf("x: if true", "if"), CLASS.keyword);
  assert.equal(classOf("xs map(f)", "map"), CLASS.prelude);
  assert.equal(classOf("xs nonsuch(f)", "nonsuch"), CLASS.call);
  assert.equal(classOf("y: somevar", "somevar"), CLASS.variable);
});

test("kebab-case prelude predicates stay one token", () => {
  // `non-nil?` must not split on `-` and must be recognised as prelude.
  assert.equal(classOf("x: y non-nil?", "non-nil?"), CLASS.prelude);
  assert.equal(classOf("x: y block?", "block?"), CLASS.prelude);
});

test("builtins, anaphora, symbols, numbers", () => {
  assert.equal(classOf("x: __NULL", "__NULL"), CLASS.builtin);
  assert.equal(classOf("f: _ * _0", "_0"), CLASS.anaphor);
  assert.equal(classOf("s: :Enabled", ":Enabled"), CLASS.symbol);
  assert.equal(classOf("p: [22, 80]", "22"), CLASS.number);
  assert.equal(classOf("r: 3.14", "3.14"), CLASS.number);
});

test("quoted symbol with colon inside", () => {
  assert.equal(classOf("x: { 'AWS:SourceArn': 1 }", ":'AWS:SourceArn'"), undefined);
  // the quoted symbol form is `:'...'`
  assert.equal(classOf("x: :'AWS:SourceArn'", ":'AWS:SourceArn'"), CLASS.symbol);
});

test("operators: ascii runs and unicode singletons", () => {
  assert.equal(classOf("x: a << b", "<<"), CLASS.operator);
  assert.equal(classOf("x: a @ b", "@"), CLASS.operator);
  assert.equal(classOf("x: a ‖ b", "‖"), CLASS.operator);
  assert.equal(classOf("x: a ÷ b", "÷"), CLASS.operator);
  assert.equal(classOf("x: a ∧ b", "∧"), CLASS.operator);
});

test("kebab identifiers are a single token (not subtraction)", () => {
  const toks = tokenize("x: subnet-group");
  assert.ok(toks.some((t) => t.t === "subnet-group"));
  assert.ok(!toks.some((t) => t.t === "-"));
});

test("strings: interpolation and brace escapes", () => {
  const src = 'x: "cidr {i} and {{literal}} done"';
  assert.equal(classOf(src, "{{"), CLASS.escape);
  assert.equal(classOf(src, "i"), CLASS.interp);
  // the surrounding text is a string
  assert.equal(classOf(src, "cidr "), CLASS.string);
});

test("c-string escapes and t-string datetime", () => {
  assert.equal(classOf('x: c"a\\tb"', "\\t"), CLASS.escape);
  // t-string opening prefix tagged as datetime
  const toks = tokenize('x: t"2024-03-15"');
  assert.ok(toks.some((t) => t.t === "t" && t.c === CLASS.datetime));
});

test("multi-line string keeps string class across lines", () => {
  const src = ['a: "line one', "still string", 'end"'].join("\n");
  const lines = tokenizeToLines(src);
  assert.equal(lines.length, 3);
  // every token on the middle line is string-classed
  assert.ok(lines[1].every((t) => t.c === CLASS.string));
});

test("tokenizeToLines length matches input line count", () => {
  const src = "a: 1\nb: 2\nc: 3";
  assert.equal(tokenizeToLines(src).length, 3);
  assert.equal(highlightFileLines(src.split("\n")).length, 3);
});

test("highlightDiffRows: multi-line string opened by an added line", () => {
  // A docstring opened on an added line continues across following added
  // lines; none of it should be mis-highlighted as code.
  const rows = [
    { type: "context", text: "key: value" },
    { type: "add", text: 'doc: "first line' },
    { type: "add", text: "second line with: colons and (parens)" },
    { type: "add", text: 'third"' },
  ];
  const html = highlightDiffRows(rows);
  assert.equal(html.length, 4);
  // The middle added line is inside the string, so its colon/paren must not
  // be coloured as punctuation/operator — it should be string-classed only.
  assert.ok(html[2].includes("eu-string"));
  assert.ok(!html[2].includes("eu-punct"));
  assert.ok(!html[2].includes("eu-property"));
});

test("highlightDiffRows: deletions use the old side", () => {
  const rows = [
    { type: "del", text: "old: 1" },
    { type: "add", text: "new: 2" },
  ];
  const html = highlightDiffRows(rows);
  assert.ok(html[0].includes("old"));
  assert.ok(html[1].includes("new"));
});

test("HTML is escaped", () => {
  const html = highlightOneLine('x: "a < b & c > d"');
  assert.ok(html.includes("&lt;"));
  assert.ok(html.includes("&amp;"));
  assert.ok(html.includes("&gt;"));
  assert.ok(!html.includes("< b"));
});

test("sample.eu tokenises without throwing and covers classes", () => {
  const src = readFileSync(join(here, "..", "examples", "sample.eu"), "utf8");
  const toks = tokenize(src);
  const classes = new Set(toks.map((t) => t.c));
  for (const c of [
    CLASS.comment, CLASS.string, CLASS.symbol, CLASS.number, CLASS.prelude,
    CLASS.builtin, CLASS.operator, CLASS.function, CLASS.property,
    CLASS.metadata, CLASS.anaphor, CLASS.datetime,
  ]) {
    assert.ok(classes.has(c), `expected sample to contain class ${c}`);
  }
  // round-trips: concatenating token text reproduces the source exactly
  assert.equal(toks.map((t) => t.t).join(""), src);
});
