// The grammar's prelude name patterns must match lib/prelude.eu.

"use strict";

const test = require("node:test");
const assert = require("node:assert");
const fs = require("fs");
const path = require("path");
const { preludeNames, namePattern } = require("../scripts/generate-prelude-names.js");

const REPO = path.resolve(__dirname, "..", "..", "..");
const grammar = JSON.parse(
  fs.readFileSync(path.resolve(__dirname, "..", "syntaxes", "eucalypt.tmLanguage.json"), "utf8")
);

test("prelude name patterns are generated from lib/prelude.eu", () => {
  const { functions, properties } = preludeNames(
    fs.readFileSync(path.join(REPO, "lib", "prelude.eu"), "utf8")
  );
  assert.ok(functions.includes("map") && functions.includes("then"));
  assert.ok(properties.includes("str") && properties.includes("nil"));
  assert.ok(!properties.includes("true"), "true is a language constant");
  assert.strictEqual(
    grammar.repository["prelude-function"].match,
    namePattern(functions),
    "prelude functions are stale: run node scripts/generate-prelude-names.js"
  );
  assert.strictEqual(
    grammar.repository["prelude-variable"].match,
    namePattern(properties),
    "prelude properties are stale: run node scripts/generate-prelude-names.js"
  );
});
