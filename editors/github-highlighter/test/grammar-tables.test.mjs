// Guard the generated prelude/keyword tables: they must stay in lock-step with
// the canonical TextMate grammar. If this fails after a grammar change, run
// `npm run gen` (or `npm run build`) and commit src/grammar-tables.mjs.
import { test } from "node:test";
import assert from "node:assert/strict";
import { computeTables } from "../gen-tables.mjs";
import {
  PRELUDE_WORDS,
  KEYWORD_WORDS,
  PRELUDE,
  KEYWORDS,
} from "../src/grammar-tables.mjs";

test("committed tables match the TextMate grammar", () => {
  const { prelude, keywords } = computeTables();
  assert.deepEqual(
    KEYWORD_WORDS,
    keywords,
    "src/grammar-tables.mjs keywords are stale — run `npm run gen`"
  );
  assert.deepEqual(
    PRELUDE_WORDS,
    prelude,
    "src/grammar-tables.mjs prelude is stale — run `npm run gen`"
  );
});

test("escapes are decoded and key words are present", () => {
  // `?`-suffixed predicates survive the `\?` -> `?` unescape.
  assert.ok(PRELUDE.has("non-nil?"));
  assert.ok(PRELUDE.has("all-true?"));
  assert.ok(PRELUDE.has("deep-transform"));
  assert.ok(KEYWORDS.has("cond"));
  // No regex backslashes leaked into the word list.
  assert.ok(PRELUDE_WORDS.every((w) => !w.includes("\\")));
  assert.ok(PRELUDE_WORDS.length > 150, "expected the full prelude");
});
