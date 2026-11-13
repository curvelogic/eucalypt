// Scope assertions for the TextMate grammar over the shared highlighting
// fixtures (editors/test-fixtures/highlighting). Each case names a line
// (by its declaration name, or by a substring), a token's exact text and
// the scope it must carry; the token's extent must equal the text.

"use strict";

const test = require("node:test");
const assert = require("node:assert");
const fs = require("fs");
const path = require("path");
const { tokenise } = require("./tokenise.js");

const FIXTURES = path.resolve(__dirname, "..", "..", "test-fixtures", "highlighting");

// [line selector, token text, expected scope (without ".eucalypt"), occurrence (default 0)]
const CASES = {
  "strings.eu": [
    ["interpolated", '"', "punctuation.definition.string.begin"],
    ["interpolated", "name", "variable.other.interpolation"],
    ["interpolated", "{", "punctuation.section.interpolation.begin"],
    ["formatted", "%06d", "string.other.format-spec"],
    ["dotted", "point", "variable.other.interpolation"],
    ["dotted", ".", "keyword.operator.accessor"],
    ["dotted", "x", "variable.other.interpolation"],
    ["spaced", "name", "variable.other.interpolation"],
    ["escaped-braces", "{{", "constant.character.escape"],
    ["escaped-braces", "}}", "constant.character.escape"],
    ["escape-then-target", "{{", "constant.character.escape"],
    ["escape-then-target", "{", "punctuation.section.interpolation.begin"],
    ["escape-then-target", "name", "variable.other.interpolation"],
    ["escape-then-target", "}}", "constant.character.escape"],
    ["closing-run", "name", "variable.other.interpolation"],
    ["closing-run", "}}", "constant.character.escape"],
    ["string-anaphora", "}", "punctuation.section.interpolation.end"],
    ["numbered-anaphora", "1", "variable.other.interpolation"],
    ["numbered-anaphora", "0", "variable.other.interpolation"],
    ["raw:", "^\\d+ ", "string.quoted.double.raw"],
    ["raw:", "n", "variable.other.interpolation"],
    ["raw-escaped", "{{", "constant.character.escape"],
    ["c-string:", "\\t", "constant.character.escape"],
    ["c-string-braces", "\\{", "constant.character.escape"],
    ["c-string-braces", "literal", "string.quoted.double.c"],
    ["c-string-interpolated", "name", "variable.other.interpolation"],
    ["c-string-interpolated", "\\}", "constant.character.escape"],
    ["c-string-quote", '\\"', "constant.character.escape"],
    ["datetime", "2024-03-15T14:30:00Z", "string.quoted.double.datetime"],
    ["type-data", 's"', "string.quoted.double.type-data"],
    ["type-data", "string → null", "string.quoted.double.type-data"],
    ["second line", "second line", "string.quoted.double"],
  ],
  "declarations.eu": [
    ['"A documented', "`", "keyword.other.metadata"],
    ['"A documented', "`code`", "markup.inline.raw"],
    ['"A documented', "A documented property with ", "string.quoted.double.documentation"],
    ["documented", "documented", "variable.other.property.declaration"],
    ["{ doc:", "doc", "entity.other.attribute-name.metadata"],
    ["add(", "add", "entity.name.function"],
    ["add(", "x", "variable.parameter"],
    ["add(", "y", "variable.parameter"],
    ["inline-block", "a", "variable.other.property.declaration"],
    ["inline-block", "c", "variable.other.property.declaration"],
    ["pair-sum[", "pair-sum", "entity.name.function"],
    ["pair-sum[", "b", "variable.parameter"],
    ["area{", "area", "entity.name.function"],
    ["area{", "height", "variable.parameter"],
    ["swap(", "a", "variable.parameter"],
    ["norm(", "y", "variable.parameter"],
    ["rest(", "h", "variable.parameter"],
    ["rest(", ":", "punctuation.separator.cons"],
    ["rest(", "t", "variable.parameter"],
    ["'quoted key'", "'quoted key'", "variable.other.property.declaration"],
    ["{ associates:", "precedence", "entity.other.attribute-name.metadata"],
    ["{ associates:", ":right", "constant.other.symbol"],
    ["(l +++ r)", "+++", "keyword.operator.declaration"],
    ["(l +++ r)", "r", "variable.parameter"],
    ["(¬¬ x)", "¬¬", "keyword.operator.declaration"],
    ["(x !?)", "!?", "keyword.operator.declaration"],
    ["(x !?)", "x", "variable.parameter"],
    ["(∅)", "∅", "keyword.operator.declaration"],
    ["inner(z)", "inner", "entity.name.function"],
    ["value:", "inner", "entity.name.function.call"],
  ],
  "calls-and-anaphora.eu": [
    ["paren-call", "max", "support.function.prelude"],
    ["list-call", "sum", "support.function.prelude"],
    ["block-call", "merge", "support.variable.prelude"],
    ["block-call", "a", "variable.other.property.declaration"],
    ["pipeline", "xs", "variable.other"],
    ["pipeline", "count", "support.function.prelude"],
    ["section", "+", "keyword.operator"],
    ["expression-anaphor", "_", "variable.language.anaphor"],
    ["numbered-anaphor", "_0", "variable.language.anaphor"],
    ["block-anaphor", "•", "variable.language.anaphor"],
    ["numbered-block-anaphor", "•1", "variable.language.anaphor"],
    ["generalised-lookup", ".", "keyword.operator"],
    ["namespace-lookup", "str", "support.variable.prelude"],
    ["namespace-lookup", "to-upper", "variable.other"],
    ["conditional", "then", "support.function.prelude"],
    ["transform", "when", "support.function.prelude"],
    ["multi-way", "cond", "support.variable.prelude"],
    ["multi-way", "=>", "keyword.operator"],
    ["composed", ";", "keyword.operator"],
    ["constants", "true", "constant.language"],
    ["constants", "null", "constant.language"],
    ["constants", "nil", "support.variable.prelude"],
    ["intrinsic", "__NULL", "support.function.builtin"],
  ],
  "brackets-and-blocks.eu": [
    ["⟦ xs ⟧:", "⟦", "keyword.operator.bracket.declaration"],
    ["⟦ xs ⟧:", "xs", "variable.parameter"],
    ["doubled", "⟦", "keyword.operator.bracket.begin"],
    ["doubled", "⟧", "keyword.operator.bracket.end"],
    ["« ys »:", "»", "keyword.operator.bracket.declaration"],
    ["reversed", "«", "keyword.operator.bracket.begin"],
    ["ceiling", "⌈", "keyword.operator.bracket.begin"],
    ["floored", "⌋", "keyword.operator.bracket.end"],
    ["products", ":for", "constant.other.symbol.block-meta"],
    ["products", "x", "variable.other.property.declaration"],
    ["products", "y", "variable.other.property.declaration"],
    ["filtered", "_", "variable.other.property.declaration"],
    ["let-block", ":let", "constant.other.symbol.block-meta"],
    ["⟪{}⟫:", "⟪", "keyword.operator.bracket.declaration"],
    ["⟪{}⟫:", ":monad", "constant.other.symbol.block-meta"],
    ["⟪{}⟫:", "bind", "entity.name.function"],
    ["pairs", "⟪", "keyword.operator.bracket.begin"],
    ["‹ [a, b] ›:", "a", "variable.parameter"],
    ["‹ [a, b] ›:", "›", "keyword.operator.bracket.declaration"],
  ],
  "operators-and-assertions.eu": [
    ["integers", "-7", "constant.numeric"],
    ["floats", "3.14", "constant.numeric"],
    ["symbols", ":key", "constant.other.symbol"],
    ["symbols", ":'quoted symbol'", "constant.other.symbol.quoted"],
    ["arithmetic", "-", "keyword.operator"],
    ["division", "÷", "keyword.operator"],
    ["comparison", "∧", "keyword.operator"],
    ["comparison", ">=", "keyword.operator"],
    ["negation", "¬", "keyword.operator"],
    ["not-null", "✓", "keyword.operator"],
    ["divides", "∣", "keyword.operator"],
    ["deep-merge", "<<", "keyword.operator"],
    ["nth", "!!", "keyword.operator"],
    ["safe-lookup", "~", "keyword.operator"],
    ["head:", "↑", "keyword.operator"],
    ["assert-equal", "//=>", "keyword.operator.assertion"],
    ["expect-equal", "//=", "keyword.operator.assertion"],
    ["expect-true", "//!", "keyword.operator.assertion"],
    ["expect-predicate", "//=?", "keyword.operator.assertion"],
    ["assert-predicate", "//=?>", "keyword.operator.assertion"],
    ["with-meta", "//", "keyword.operator.metadata"],
    ["merge-meta", "//<<", "keyword.operator.metadata"],
  ],
};

function findLine(lines, selector) {
  const decl = /^[\p{Alphabetic}_•$?][\p{Alphabetic}\p{N}_$?!\-*]*$/u.test(selector);
  const index = lines.findIndex((l) =>
    decl ? l.startsWith(selector + ":") : l.includes(selector)
  );
  assert.notStrictEqual(index, -1, `no line matches ${JSON.stringify(selector)}`);
  return index;
}

for (const [fixture, cases] of Object.entries(CASES)) {
  test(fixture, async (t) => {
    const source = fs.readFileSync(path.join(FIXTURES, fixture), "utf8");
    const lines = source.split("\n");
    const tokens = await tokenise(source);
    for (const [selector, text, scope, occurrence = 0] of cases) {
      await t.test(`${selector} ${text} → ${scope}`, () => {
        const line = findLine(lines, selector);
        const matches = tokens[line].filter((k) => k.text === text);
        assert.ok(
          matches.length > occurrence,
          `no token ${JSON.stringify(text)} on line ${line + 1}; tokens: ` +
            JSON.stringify(tokens[line].map((k) => k.text))
        );
        assert.ok(
          matches[occurrence].scopes.includes(`${scope}.eucalypt`),
          `${JSON.stringify(text)} on line ${line + 1} has scopes ${matches[occurrence].scopes.join(" ")}`
        );
      });
    }
  });
}

test("every non-blank token in the fixtures is scoped", async () => {
  for (const fixture of fs.readdirSync(FIXTURES).filter((f) => f.endsWith(".eu"))) {
    const tokens = await tokenise(fs.readFileSync(path.join(FIXTURES, fixture), "utf8"));
    tokens.forEach((line, i) =>
      line.forEach((k) => {
        if (k.text.trim() === "") return;
        assert.ok(
          k.scopes.length > 1,
          `${fixture}:${i + 1}: ${JSON.stringify(k.text)} has no scope beyond source.eucalypt`
        );
      })
    );
  }
});
