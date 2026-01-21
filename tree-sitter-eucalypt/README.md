# tree-sitter-eucalypt

Tree-sitter grammar for the [Eucalypt](https://github.com/curvelogic/eucalypt) language.

## Installation

### For Emacs (29.1+)

Emacs 29.1 and later includes built-in tree-sitter support. To use this grammar:

1. Add the grammar source to your Emacs config:

```elisp
(add-to-list 'treesit-language-source-alist
             '(eucalypt "https://github.com/curvelogic/tree-sitter-eucalypt"))
```

2. Install the grammar:

```elisp
M-x treesit-install-language-grammar RET eucalypt RET
```

3. Add the mode to your config:

```elisp
(require 'eucalypt-ts-mode)
```

Or with `use-package`:

```elisp
(use-package eucalypt-ts-mode
  :load-path "/path/to/eucalypt/editors/emacs")
```

### For Neovim

Add to your tree-sitter config:

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.eucalypt = {
  install_info = {
    url = "https://github.com/curvelogic/tree-sitter-eucalypt",
    files = {"src/parser.c", "src/scanner.c"},
    branch = "main",
  },
  filetype = "eu",
}
```

Then run `:TSInstall eucalypt`.

### Building from Source

```bash
npm install
npm run build
```

To test:

```bash
npm run test
npx tree-sitter parse path/to/file.eu
```

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

## License

MIT
