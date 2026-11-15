// Tokenise eucalypt source with the extension's TextMate grammar, using
// the same engine VS Code uses (vscode-textmate over vscode-oniguruma).

"use strict";

const fs = require("fs");
const path = require("path");
const vsctm = require("vscode-textmate");
const oniguruma = require("vscode-oniguruma");

const GRAMMAR = path.resolve(__dirname, "..", "syntaxes", "eucalypt.tmLanguage.json");

let grammarPromise;

function loadGrammar() {
  if (!grammarPromise) {
    const wasm = fs.readFileSync(require.resolve("vscode-oniguruma/release/onig.wasm"));
    const onigLib = oniguruma.loadWASM(wasm.buffer).then(() => ({
      createOnigScanner: (patterns) => new oniguruma.OnigScanner(patterns),
      createOnigString: (s) => new oniguruma.OnigString(s),
    }));
    const registry = new vsctm.Registry({
      onigLib,
      loadGrammar: async (scopeName) =>
        scopeName === "source.eucalypt"
          ? vsctm.parseRawGrammar(fs.readFileSync(GRAMMAR, "utf8"), GRAMMAR)
          : null,
    });
    grammarPromise = registry.loadGrammar("source.eucalypt");
  }
  return grammarPromise;
}

// Returns one array of tokens per line: { startIndex, endIndex, text, scopes }.
async function tokenise(source) {
  const grammar = await loadGrammar();
  let state = vsctm.INITIAL;
  return source.split("\n").map((line) => {
    const result = grammar.tokenizeLine(line, state);
    state = result.ruleStack;
    return result.tokens.map((t) => ({
      startIndex: t.startIndex,
      endIndex: t.endIndex,
      text: line.slice(t.startIndex, t.endIndex),
      scopes: t.scopes,
    }));
  });
}

module.exports = { tokenise };
