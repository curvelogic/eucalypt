# tree-sitter-eucalypt

Tree-sitter grammar for the [Eucalypt](https://github.com/curvelogic/eucalypt) language.

This grammar is part of the main eucalypt repository at `editors/tree-sitter-eucalypt`.

## Installation

### For Emacs (29.1+)

Emacs 29.1 and later includes built-in tree-sitter support. To use this grammar:

1. Add the grammar source to your Emacs config:

```elisp
(add-to-list 'treesit-language-source-alist
             '(eucalypt "https://github.com/curvelogic/eucalypt"
                        nil nil "editors/tree-sitter-eucalypt/src"))
```

2. Install the grammar:

```
M-x treesit-install-language-grammar RET eucalypt RET
```

3. Add the mode to your config:

```elisp
(add-to-list 'load-path "/path/to/eucalypt/editors/emacs")
(require 'eucalypt-mode)
```

Or with `use-package`:

```elisp
(use-package eucalypt-mode
  :load-path "/path/to/eucalypt/editors/emacs")
```

### For Neovim

Add to your tree-sitter config:

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.eucalypt = {
  install_info = {
    url = "https://github.com/curvelogic/eucalypt",
    files = {"editors/tree-sitter-eucalypt/src/parser.c",
             "editors/tree-sitter-eucalypt/src/scanner.c"},
    branch = "master",
    generate_requires_npm = true,
    requires_generate_from_grammar = false,
  },
  filetype = "eu",
}
```

Then run `:TSInstall eucalypt`.

### Building from Source

From the `editors/tree-sitter-eucalypt` directory:

```bash
npm install
npm run build
```

To test:

```bash
npm run test
npx tree-sitter parse /path/to/file.eu
```

### Rust

The Rust binding (`bindings/rust/lib.rs`) exports `language()`,
`NODE_TYPES` and `HIGHLIGHTS_QUERY` (the contents of
`queries/highlights.scm`), ready for `tree-sitter-highlight`.

## Highlighted HTML

To share eucalypt code as a highlighted web page, run from the repository
root:

```bash
cargo xtask highlight-html -o day01.html examples/aoc25/day01.eu
```

This writes a standalone HTML page with an inline stylesheet that covers
every capture name in `queries/highlights.scm`, in light and dark (dark
follows `prefers-color-scheme`; set `data-theme="light"` or
`data-theme="dark"` on `<html>` to force one). Options:

- `-o PATH` — write to `PATH` instead of stdout
- `--title TITLE` — the page title (default: the first file's path)
- `--fragment` — write only the `<pre class="eu"><code>` blocks, without
  page or stylesheet, for embedding in another page
- `--css` — write only the stylesheet (no files), to accompany fragments

Several files may be given; each gets its own block under a heading.
Spans carry one class per level of the capture name, so `@string.escape`
is rendered as `<span class="ts-string ts-string-escape">`.

## Browser use (WASM)

A WebAssembly build of the grammar, `tree-sitter-eucalypt.wasm`, is
committed in this directory next to `queries/highlights.scm`, so every
eucalypt tag carries a matching pair. `wasm-manifest.json` records what the
WASM was built from: the grammar version, the ABI version, the tree-sitter
CLI and emscripten versions, the `web-tree-sitter` version it was checked
with, and SHA-256 digests of the WASM and of every C source it was compiled
from. A cargo test (`tests/tree_sitter_wasm_test.rs`) fails when the
sources change without a rebuild, so the WASM at a tag is always the
grammar at that tag.

### Obtaining the WASM and query for a eucalypt version

Take both files from the same tag (or commit), `<tag>` below:

```text
https://cdn.jsdelivr.net/gh/curvelogic/eucalypt@<tag>/editors/tree-sitter-eucalypt/tree-sitter-eucalypt.wasm
https://cdn.jsdelivr.net/gh/curvelogic/eucalypt@<tag>/editors/tree-sitter-eucalypt/queries/highlights.scm
https://cdn.jsdelivr.net/gh/curvelogic/eucalypt@<tag>/editors/tree-sitter-eucalypt/wasm-manifest.json
```

`https://raw.githubusercontent.com/curvelogic/eucalypt/<tag>/editors/tree-sitter-eucalypt/…`
serves the same files, as does a checkout of the tag. Load the WASM with the
`web-tree-sitter` version named in the manifest; other versions are
untested.

### Building it

From this directory, with docker, podman or emscripten's `emcc` on `PATH`:

```bash
npm ci
npm run build-wasm   # writes tree-sitter-eucalypt.wasm and wasm-manifest.json
npm run check-wasm   # rebuilds into a temporary file and compares
```

The build runs the lockfile-pinned `tree-sitter build --wasm`, which uses its
pinned `emscripten/emsdk` image under docker or podman when `emcc` is
absent; with the same CLI and image the output is byte-for-byte
reproducible. It compiles the committed `src/parser.c`, so after changing
`grammar.js` run `npm run build` (`tree-sitter generate`) first. Commit
both files.

### Example page

`web/index.html` is a minimal consumer: it loads the WASM and
`queries/highlights.scm` with `web-tree-sitter` and renders highlighted
HTML (`?file=<url>` highlights another file). Serve the repository over HTTP
after `npm ci` and open it, or check it headless:

```bash
npm run test-wasm
```

This opens the page in Chromium via Playwright (which must be installed;
`CHROMIUM_PATH` selects a browser binary) for several sample files and
checks that each parses without errors and yields exactly the captures that
the native CLI (`tree-sitter query --captures`) yields.

## Language Overview

Eucalypt is a functional language for generating, templating, and rendering structured data formats (YAML, JSON, TOML).

### Syntax Highlights

- **Declarations**: `name: expression` or `f(x, y): body`
- **Operator declarations**: `(x + y): __ADD(x, y)`
- **Blocks**: `{ a: 1, b: 2 }`
- **Lists**: `[1, 2, 3]`
- **Symbols**: `:foo`, `:'quoted symbol'`
- **String interpolation**: `"Hello {name}!"`
- **Metadata**: `` ` "description" `` before declarations
- **Comments**: `# line comment`

## Licence

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](../../LICENSE-APACHE) or
  http://www.apache.org/licenses/LICENSE-2.0)
- MIT License ([LICENSE-MIT](../../LICENSE-MIT) or
  http://opensource.org/licenses/MIT)

at your option.
