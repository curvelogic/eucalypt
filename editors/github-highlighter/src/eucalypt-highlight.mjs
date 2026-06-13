// Eucalypt syntax highlighter — pure, dependency-free core.
//
// This is a small hand-written tokeniser for the Eucalypt native syntax,
// mirroring the canonical TextMate grammar in
// `editors/vscode/syntaxes/eucalypt.tmLanguage.json` (rule order and scopes
// are kept deliberately faithful so the two stay in step). It exists so the
// GitHub web UI can be given `.eu` syntax highlighting in PR diffs and blob
// views, where GitHub's Linguist has no grammar for the language.
//
// The module has no DOM or browser dependencies: it turns source text into
// classed tokens and HTML. The DOM glue lives in `github-dom.mjs`, and both
// are bundled into a userscript / web extension by `build.mjs`.
//
// IMPORTANT: keep this in sync with the TextMate grammar when the language
// changes. The prelude/keyword tables below are copied from it verbatim.

// ---------------------------------------------------------------------------
// Character classes (match the TextMate grammar's identifier classes)
// ---------------------------------------------------------------------------

const ID_START = /[\p{L}_•$?]/u; // letter, _, • (bullet anaphor), $, ?
const ID_CONT = /[\p{L}\p{N}_$?!\-*]/u; // adds digits, !, *, - (kebab-case)

// Unicode operators (from the grammar's `unicode-operator` pattern). `•`
// (U+2022) is handled as an anaphor before it can be read as an operator.
const UNICODE_OP =
  /[∧∨¬≤≥≠∸÷∘•‖↑✓⊕⊝≪≫∅▶→←⊂⊃∈∉∀∃⊥⊤≈≡∩∪∼±×√∞∂∫∑∏∇△▽⊢⊣⊨⊩⊗⊙⊡⊞⊟⋅⋆⋈⋉⋊⟵⟶⟷⟸⟹⟺]/u;

// ASCII operator characters (the grammar's `operator` pattern).
const ASCII_OP = /[+\-*/<>=!&|^~%@$?.\\]/;

// Unicode bracket pairs (grammar's punctuation `bracket` patterns).
const UNI_BRACKET =
  /[⟦⟨⟪⌈⌊⦃⦇⦉«‹【〔〖〘〚⟧⟩⟫⌉⌋⦄⦈⦊»›】〕〗〙〛]/u;

const KEYWORDS = new Set([
  "if", "then", "when", "cond", "true", "false", "null", "nil",
]);

// Standard prelude functions — copied verbatim from the TextMate grammar.
const PRELUDE = new Set([
  "cons", "head", "tail", "first", "second", "head-or", "tail-or", "second-or",
  "map", "filter", "foldl", "foldr", "scanl", "scanr", "and", "or", "not",
  "merge", "concat", "append", "prepend", "identity", "const", "compose",
  "apply", "flip", "take", "drop", "take-while", "drop-while", "all", "any",
  "all-true?", "any-true?", "keys", "values", "lookup", "has", "range",
  "repeat", "iterate", "cycle", "zip", "zip-with", "reverse", "remove",
  "mapcat", "group-by", "qsort", "partition", "negate", "inc", "dec", "floor",
  "ceiling", "max", "min", "abs", "num", "panic", "assert", "deep-merge",
  "merge-all", "elements", "block", "lookup-in", "lookup-or", "lookup-or-in",
  "lookup-alts", "lookup-across", "lookup-path", "complement", "curry",
  "uncurry", "juxt", "fnil", "with-meta", "meta", "merge-meta", "assertions",
  "split-at", "take-until", "drop-until", "split-after", "split-when", "nth",
  "count", "last", "map2", "zip-apply", "window", "over-sliding-pairs",
  "differences", "discriminate", "key", "value", "bimap", "map-first",
  "map-second", "map-kv", "map-as-block", "pair", "zip-kv", "with-keys",
  "map-values", "map-keys", "filter-items", "by-key", "by-key-name",
  "by-key-match", "by-value", "match-filter-values", "filter-values",
  "alter-value", "update-value", "alter", "update", "update-value-or",
  "set-value", "tongue", "merge-at", "nil?", "zero?", "pos?", "neg?", "any?",
  "non-nil?", "block?", "bool?", "list?", "number?", "string?", "symbol?",
  "is-array?", "match?", "max-of", "min-of", "max-of-by", "max-of-or",
  "min-of-by", "min-of-or", "sym", "ch", "str", "arr", "monad", "random",
  "vec", "render", "render-as", "parse-as", "parse-args", "deep-transform",
  "deep-fold", "deep-find", "deep-find-first", "deep-find-paths", "deep-query",
  "deep-query-first", "deep-query-fold", "deep-query-paths", "deep-merge-at",
  "sort-by", "sort-by-num", "sort-by-str", "sort-by-zdt", "sort-keys",
  "sort-nums", "sort-strs", "sort-zdts", "kv-block", "map-elements",
  "coalesce", "group-consecutive", "group-consecutive-by", "iota", "reduce",
  "rotate", "tails", "update-nth", "update-first", "interleave", "unzip",
  "butlast", "ints-from", "snoc", "cross", "sum", "product", "running-sum",
  "running-max", "running-min", "nub-by", "uniq", "partition-all",
  "window-all", "div", "mod", "rem", "quot", "pow", "dbg", "eu", "io", "cal",
  "iosm",
]);

// CSS class names used for each token kind. Themed in `theme.css`.
export const CLASS = {
  comment: "eu-comment",
  doc: "eu-doc",
  docCode: "eu-doc-code",
  string: "eu-string",
  escape: "eu-escape",
  datetime: "eu-datetime",
  symbol: "eu-symbol",
  number: "eu-number",
  metadata: "eu-metadata",
  builtin: "eu-builtin",
  keyword: "eu-keyword",
  prelude: "eu-prelude",
  anaphor: "eu-anaphor",
  function: "eu-function",
  property: "eu-property",
  call: "eu-call",
  operator: "eu-operator",
  punct: "eu-punct",
  interpPunct: "eu-interp-punct",
  interp: "eu-interp",
  interpFmt: "eu-interp-fmt",
  variable: "eu-variable",
};

// ---------------------------------------------------------------------------
// Tokeniser
// ---------------------------------------------------------------------------

const ANAPHOR_RE = /^(?:_[0-9]*|•[0-9]*)$/u;
const BUILTIN_RE = /^__[A-Z][A-Z0-9_]*$/;

// Scan a quoted string body starting at `i` (the opening quote). Handles
// `{...}` interpolation and `{{`/`}}` brace escapes, plus optional C-style
// escapes. Returns { tokens, end } where `end` is just past the closing
// quote (or end-of-input for an unterminated string).
function scanString(src, i, { cls, escapes }) {
  const toks = [];
  const n = src.length;
  toks.push({ t: '"', c: cls }); // opening quote
  let j = i + 1;
  let buf = "";
  const flush = () => {
    if (buf) {
      toks.push({ t: buf, c: cls });
      buf = "";
    }
  };
  while (j < n) {
    const ch = src[j];
    if (ch === '"') {
      flush();
      toks.push({ t: '"', c: cls });
      return { tokens: toks, end: j + 1 };
    }
    if (escapes && ch === "\\") {
      const m = /^\\(?:[nrt0\\"{}]|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8})/.exec(
        src.slice(j)
      );
      if (m) {
        flush();
        toks.push({ t: m[0], c: CLASS.escape });
        j += m[0].length;
        continue;
      }
    }
    if (ch === "{" && src[j + 1] === "{") {
      flush();
      toks.push({ t: "{{", c: CLASS.escape });
      j += 2;
      continue;
    }
    if (ch === "}" && src[j + 1] === "}") {
      flush();
      toks.push({ t: "}}", c: CLASS.escape });
      j += 2;
      continue;
    }
    if (ch === "{") {
      // interpolation: { target[:format] }
      flush();
      const close = src.indexOf("}", j + 1);
      const end = close === -1 ? n : close;
      const inner = src.slice(j + 1, end);
      toks.push({ t: "{", c: CLASS.interpPunct });
      const colon = inner.indexOf(":");
      const target = colon === -1 ? inner : inner.slice(0, colon);
      if (target) toks.push({ t: target, c: CLASS.interp });
      if (colon !== -1) {
        toks.push({ t: inner.slice(colon), c: CLASS.interpFmt });
      }
      if (close !== -1) toks.push({ t: "}", c: CLASS.interpPunct });
      j = close === -1 ? n : close + 1;
      continue;
    }
    buf += ch;
    j += 1;
  }
  flush(); // unterminated
  return { tokens: toks, end: n };
}

// Classify a maximal identifier run.
function classifyIdent(word, atLineStart, src, after) {
  if (BUILTIN_RE.test(word)) return CLASS.builtin;
  if (KEYWORDS.has(word)) return CLASS.keyword;
  if (PRELUDE.has(word)) return CLASS.prelude;
  if (ANAPHOR_RE.test(word)) return CLASS.anaphor;
  if (atLineStart) {
    // Look past trailing spaces for a declaration head.
    let k = after;
    while (k < src.length && (src[k] === " " || src[k] === "\t")) k++;
    const nxt = src[k];
    if (nxt === "(" || nxt === "{" || nxt === "[") return CLASS.function;
    if (nxt === ":") return CLASS.property;
  }
  if (src[after] === "(") return CLASS.call; // immediate call
  return CLASS.variable;
}

// Tokenise source into a flat list of { t, c } tokens. Token text may span
// newlines (e.g. multi-line strings); `tokenizeToLines` splits those.
export function tokenize(src) {
  const out = [];
  const n = src.length;
  let i = 0;
  let lineHasContent = false;

  const push = (t, c) => {
    if (t) out.push({ t, c: c || null });
  };

  while (i < n) {
    const ch = src[i];

    if (ch === "\n") {
      push("\n", null);
      lineHasContent = false;
      i += 1;
      continue;
    }
    if (ch === " " || ch === "\t" || ch === "\r") {
      let j = i + 1;
      while (j < n && (src[j] === " " || src[j] === "\t" || src[j] === "\r")) j++;
      push(src.slice(i, j), null);
      i = j;
      continue;
    }

    // 1. comment
    if (ch === "#") {
      let j = i + 1;
      while (j < n && src[j] !== "\n") j++;
      push(src.slice(i, j), CLASS.comment);
      i = j;
      lineHasContent = true;
      continue;
    }

    // 2. docstring: ` followed by optional ws then a string
    if (ch === "`") {
      const m = /^`[ \t]*"/.exec(src.slice(i));
      if (m) {
        const quote = i + m[0].length - 1;
        push(src.slice(i, quote), CLASS.metadata); // ` and spaces
        const { tokens, end } = scanString(src, quote, { cls: CLASS.doc });
        for (const tk of tokens) out.push(tk);
        i = end;
        lineHasContent = true;
        continue;
      }
      // bare metadata backtick
      push("`", CLASS.metadata);
      i += 1;
      lineHasContent = true;
      continue;
    }

    // 3/4/5/6. typed and plain strings
    if (ch === "t" && src[i + 1] === '"') {
      const { tokens, end } = scanString(src, i + 1, { cls: CLASS.datetime });
      // re-tag opening quote area to include the `t` prefix
      out.push({ t: "t", c: CLASS.datetime });
      for (const tk of tokens) out.push(tk);
      i = end;
      lineHasContent = true;
      continue;
    }
    if ((ch === "c" || ch === "r") && src[i + 1] === '"') {
      const escapes = ch === "c";
      out.push({ t: ch, c: CLASS.string });
      const { tokens, end } = scanString(src, i + 1, { cls: CLASS.string, escapes });
      for (const tk of tokens) out.push(tk);
      i = end;
      lineHasContent = true;
      continue;
    }
    if (ch === '"') {
      const { tokens, end } = scanString(src, i, { cls: CLASS.string });
      for (const tk of tokens) out.push(tk);
      i = end;
      lineHasContent = true;
      continue;
    }

    // 7. symbol
    if (ch === ":") {
      if (src[i + 1] === "'") {
        const close = src.indexOf("'", i + 2);
        const end = close === -1 ? n : close + 1;
        push(src.slice(i, end), CLASS.symbol);
        i = end;
        lineHasContent = true;
        continue;
      }
      if (i + 1 < n && ID_START.test(src[i + 1])) {
        let j = i + 1;
        while (j < n && ID_CONT.test(src[j])) j++;
        push(src.slice(i, j), CLASS.symbol);
        i = j;
        lineHasContent = true;
        continue;
      }
      // bare colon punctuation
      push(":", CLASS.punct);
      i += 1;
      lineHasContent = true;
      continue;
    }

    // 8. number (only when not part of an identifier — identifiers are read below)
    if (
      (ch >= "0" && ch <= "9") ||
      (ch === "-" && src[i + 1] >= "0" && src[i + 1] <= "9")
    ) {
      const m = /^-?[0-9]+(?:\.[0-9]+)?/.exec(src.slice(i));
      if (m) {
        push(m[0], CLASS.number);
        i += m[0].length;
        lineHasContent = true;
        continue;
      }
    }

    // 9/10. identifiers (incl. builtins, keywords, prelude, anaphora) and
    //       quoted identifiers
    if (ch === "'") {
      const close = src.indexOf("'", i + 1);
      const end = close === -1 ? n : close + 1;
      push(src.slice(i, end), CLASS.variable);
      i = end;
      lineHasContent = true;
      continue;
    }
    if (ID_START.test(ch)) {
      let j = i + 1;
      while (j < n && ID_CONT.test(src[j])) j++;
      const word = src.slice(i, j);
      const cls = classifyIdent(word, !lineHasContent, src, j);
      push(word, cls);
      i = j;
      lineHasContent = true;
      continue;
    }

    // 11. unicode operator
    if (UNICODE_OP.test(ch)) {
      let j = i + 1;
      while (j < n && UNICODE_OP.test(src[j])) j++;
      push(src.slice(i, j), CLASS.operator);
      i = j;
      lineHasContent = true;
      continue;
    }

    // 12. ascii operator
    if (ASCII_OP.test(ch)) {
      let j = i + 1;
      while (j < n && ASCII_OP.test(src[j])) j++;
      push(src.slice(i, j), CLASS.operator);
      i = j;
      lineHasContent = true;
      continue;
    }

    // 13. punctuation
    if (ch === "{" || ch === "}" || ch === "[" || ch === "]" || ch === "(" || ch === ")" || ch === "," || UNI_BRACKET.test(ch)) {
      push(ch, CLASS.punct);
      i += 1;
      lineHasContent = true;
      continue;
    }

    // fallback: emit a single unknown char as plain text
    push(ch, null);
    i += 1;
    lineHasContent = true;
  }

  return out;
}

// Split a flat token list into an array of lines, each an array of { t, c }
// with no embedded newlines.
export function tokenizeToLines(src) {
  const flat = tokenize(src);
  const lines = [[]];
  for (const tok of flat) {
    if (tok.c === null && tok.t.indexOf("\n") !== -1) {
      const parts = tok.t.split("\n");
      for (let k = 0; k < parts.length; k++) {
        if (parts[k]) lines[lines.length - 1].push({ t: parts[k], c: null });
        if (k < parts.length - 1) lines.push([]);
      }
    } else if (tok.t.indexOf("\n") !== -1) {
      // classed multi-line token (e.g. string spanning lines)
      const parts = tok.t.split("\n");
      for (let k = 0; k < parts.length; k++) {
        if (parts[k]) lines[lines.length - 1].push({ t: parts[k], c: tok.c });
        if (k < parts.length - 1) lines.push([]);
      }
    } else {
      lines[lines.length - 1].push(tok);
    }
  }
  return lines;
}

// ---------------------------------------------------------------------------
// HTML rendering
// ---------------------------------------------------------------------------

export function escapeHtml(s) {
  return s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

export function renderLine(tokens) {
  let html = "";
  for (const { t, c } of tokens) {
    html += c ? `<span class="${c}">${escapeHtml(t)}</span>` : escapeHtml(t);
  }
  return html;
}

// Highlight an array of source lines (a whole file or a reconstructed side of
// a diff), returning one HTML string per input line.
export function highlightFileLines(lineTexts) {
  const src = lineTexts.join("\n");
  const lines = tokenizeToLines(src);
  // tokenizeToLines yields exactly lineTexts.length lines for joined input.
  const out = lines.map(renderLine);
  while (out.length < lineTexts.length) out.push("");
  return out.slice(0, lineTexts.length);
}

// Highlight a single line in isolation (no cross-line state). Used for split
// diffs where reconstruction is not attempted.
export function highlightOneLine(text) {
  return renderLine(tokenizeToLines(text)[0] || []);
}

// Highlight unified-diff rows with multi-line awareness. `rows` is an array of
// { type: 'context'|'add'|'del'|'other', text }. Returns one HTML string per
// row (rows of type 'other' — hunk headers etc. — come back null so callers
// can leave them untouched).
export function highlightDiffRows(rows) {
  const newLines = [];
  const oldLines = [];
  const slot = new Array(rows.length);
  for (let r = 0; r < rows.length; r++) {
    const row = rows[r];
    if (row.type === "context") {
      slot[r] = { side: "new", idx: newLines.length };
      newLines.push(row.text);
      oldLines.push(row.text);
    } else if (row.type === "add") {
      slot[r] = { side: "new", idx: newLines.length };
      newLines.push(row.text);
    } else if (row.type === "del") {
      slot[r] = { side: "old", idx: oldLines.length };
      oldLines.push(row.text);
    } else {
      slot[r] = null;
    }
  }
  const newHtml = highlightFileLines(newLines);
  const oldHtml = highlightFileLines(oldLines);
  return rows.map((_, r) => {
    const s = slot[r];
    if (!s) return null;
    return s.side === "new" ? newHtml[s.idx] : oldHtml[s.idx];
  });
}
