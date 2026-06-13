# Eucalypt syntax highlighting for the GitHub web UI

GitHub has no syntax highlighting for Eucalypt (`.eu`) — its Linguist
highlighter only knows languages above a popularity threshold — so `.eu` diffs
in pull requests render as flat grey text, which makes reviewing them in the
browser hard.

This is a tiny, dependency-free **userscript** (and equivalent **web
extension**) that injects Eucalypt highlighting into GitHub's diff and file
views *in place*. It runs entirely inside the page you're already looking at,
so it needs no GitHub token and works on private repositories (e.g.
`eucalypt-grove`).

Open [`demo-rendered.html`](./demo-rendered.html) in a browser for a preview of
the colours (GitHub dark and light) without installing anything.

## What it highlights

- Pull-request **Files changed** diffs (unified and split)
- **Commit** and **compare** diffs
- Single-file **blob** views (classic, non-virtualised)

…for any file ending in `.eu` / `.eucalypt`, or named `Eufile`. Multi-line
strings (the long `doc:` strings throughout grove) are handled correctly: the
unified-diff path reconstructs each side of the hunk so a string opened on one
line stays a string on the next.

## Install — userscript (fastest)

1. Install [Tampermonkey](https://www.tampermonkey.net/) (or Violentmonkey) in
   your browser.
2. Open
   [`dist/eucalypt-github.user.js`](https://raw.githubusercontent.com/curvelogic/eucalypt/master/editors/github-highlighter/dist/eucalypt-github.user.js)
   — Tampermonkey offers to install it. It self-updates from the same URL.
3. Open any PR with `.eu` changes. Done.

## Install — web extension (no Tampermonkey)

Load `extension/` as an unpacked extension:

- **Chrome/Edge**: `chrome://extensions` → enable *Developer mode* → *Load
  unpacked* → select `editors/github-highlighter/extension`.
- **Firefox**: `about:debugging` → *This Firefox* → *Load Temporary Add-on* →
  pick `extension/manifest.json`.

## How it works

Two dependency-free ES modules, bundled into the artifacts by `build.mjs`:

| File | Role |
|------|------|
| `src/eucalypt-highlight.mjs` | Pure tokeniser + HTML renderer. Mirrors the canonical TextMate grammar (`editors/vscode/syntaxes/eucalypt.tmLanguage.json`) rule-for-rule. No DOM. |
| `src/grammar-tables.mjs` | **Generated** by `gen-tables.mjs` — the prelude/keyword word lists, extracted from the TextMate grammar so they never drift. |
| `src/github-dom.mjs` | Finds `.eu` files in GitHub's diff/blob DOM and rewrites the code cells. All GitHub selectors live in one `SELECTORS` object. |
| `src/theme.css` | One Dark / One Light palette, following GitHub's own light/dark mode. |

The tokeniser is a regex-style port of the TextMate grammar — the same ruleset
VS Code and Shiki use. (It is *not* a separate parser from the tree-sitter or
LSP highlighters; Emacs and the tree-sitter editors share the tree-sitter
grammar, and the LSP uses the compiler's Rowan parser.) **The prelude/keyword
lists are generated from the TextMate grammar (`npm run gen`), so they track it
automatically; a test fails if the committed `grammar-tables.mjs` goes stale.
The structural rules — string forms, operators, declaration heads — are still
hand-maintained here to match the grammar.**

## Develop

```bash
npm install      # dev-only (jsdom, for tests)
npm test         # tokeniser + DOM-glue + built-bundle tests (node --test)
npm run build    # regenerate dist/ and extension/ from src/
npm run demo     # regenerate demo-rendered.html
```

`dist/` and `extension/` (`content.js`, `manifest.json`, `theme.css`) are
generated — edit `src/` and rebuild, don't edit them by hand. The extension
ships the theme via the manifest's `content_scripts.css` (browser-injected, so
it's immune to page CSP); the userscript injects the same CSS from JS.

## Verifying against live GitHub

The tokeniser and DOM glue are unit-tested (including against synthetic copies
of GitHub's classic diff markup), but GitHub's DOM is not a public contract.
After installing, confirm on a real PR:

1. Open a PR touching `.eu` files (e.g. an `eucalypt-grove` terraform PR).
2. `.eu` hunks should be coloured; other languages unchanged.
3. Scroll a large diff (lazy-loaded hunks should colour as they appear).
4. Navigate between the Conversation/Files tabs (Turbo navigation should
   re-trigger highlighting).
5. Try a single-file blob view of a `.eu` file.

If a view isn't highlighted, GitHub has likely changed its markup — adjust the
`SELECTORS` in `src/github-dom.mjs` and rebuild.

## Limitations

- The newer **React-based code view** for blobs (virtualised) isn't targeted;
  PR diffs — the priority — use the classic table markup.
- **Split** diffs are highlighted line-by-line (no cross-line string
  reconstruction); a string spanning lines in side-by-side mode may colour
  imperfectly. Unified diffs are fully reconstructed.
- This is lexical highlighting (like TextMate), not the compiler's semantic
  highlighting (which the LSP/`eu lsp` provides in editors).
