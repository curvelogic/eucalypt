# WASM API and Browser Playground

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** (1) Add a wasm-bindgen JS API to the eucalypt library for browser-based evaluation. (2) Build a standalone browser playground using that API.

**Architecture:** The WASM API lives in the eucalypt repo as a `cfg(target_arch = "wasm32")` module. It exposes a single `evaluate(source, format)` function returning JSON with output or structured error. The playground is a separate repo/project — a static site using the WASM module, web-tree-sitter for syntax highlighting, and a standard editor component.

**Tech Stack:** Rust/wasm-bindgen/wasm-pack (API), TypeScript/HTML/CSS (playground), web-tree-sitter (highlighting), GitHub Pages (hosting)

---

## Part A: WASM API (eu-7z2m)

### Task 1: Add wasm-bindgen and create API module

**Files:**
- Modify: `Cargo.toml` (add wasm-bindgen dependency)
- Create: `src/wasm.rs` (API module)
- Modify: `src/lib.rs` (conditionally include wasm module)

- [ ] **Step 1: Add wasm-bindgen dependency**

In `Cargo.toml`, add under the wasm32 dependencies:

```toml
[target.'cfg(target_arch = "wasm32")'.dependencies]
getrandom = { version = "0.2", features = ["js"] }
wasm-bindgen = "0.2"
serde-wasm-bindgen = "0.6"
```

Also add `serde` with derive if not already present (for serialising EvalResult).

- [ ] **Step 2: Create `src/wasm.rs`**

```rust
//! WASM API for eucalypt — browser-facing evaluation interface.

use wasm_bindgen::prelude::*;

/// Result of evaluating eucalypt source.
///
/// Serialised to JSON for the JS caller:
/// ```json
/// { "success": true, "output": "---\nhello: world\n" }
/// ```
/// or:
/// ```json
/// { "success": false, "error": { "message": "...", "location": { ... } } }
/// ```
#[derive(serde::Serialize)]
struct EvalResult {
    success: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    output: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<ErrorInfo>,
}

#[derive(serde::Serialize)]
struct ErrorInfo {
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<SourceLocation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    notes: Option<Vec<String>>,
}

#[derive(serde::Serialize)]
struct SourceLocation {
    line: usize,
    column: usize,
    end_line: usize,
    end_column: usize,
}

/// Evaluate eucalypt source code and return the result as JSON.
///
/// # Arguments
/// * `source` - Eucalypt source code string
/// * `format` - Output format: "yaml", "json", "toml", "text", etc.
///
/// # Returns
/// JSON string containing EvalResult
#[wasm_bindgen]
pub fn evaluate(source: &str, format: &str) -> String {
    // Implementation outline:
    //
    // 1. Create a SourceLoader with no lib path
    // 2. Load the source as an inline expression or pseudo-file
    // 3. Prepare (parse, desugar, cook, verify, simplify)
    // 4. Compile to STG with RenderType::RenderDoc
    // 5. Create machine with string-capture emitter and step limit
    // 6. Run
    // 7. Collect output or error
    // 8. Serialise as EvalResult JSON

    let result = match evaluate_inner(source, format) {
        Ok(output) => EvalResult {
            success: true,
            output: Some(output),
            error: None,
        },
        Err(error_info) => EvalResult {
            success: false,
            output: None,
            error: Some(error_info),
        },
    };

    serde_json::to_string(&result).unwrap_or_else(|e| {
        format!(r#"{{"success":false,"error":{{"message":"serialisation error: {e}"}}}}"#)
    })
}

fn evaluate_inner(source: &str, format: &str) -> Result<String, ErrorInfo> {
    // The implementor needs to wire this through the existing evaluation
    // pipeline. Key reference points:
    //
    // - src/driver/eval.rs: Executor::try_execute() — the main evaluation flow
    // - src/driver/source.rs: SourceLoader — loading and preparing source
    // - src/driver/prepare.rs: prepare() — parse/desugar/cook pipeline
    //
    // The challenge is that the existing pipeline assumes CLI options,
    // file-based input, and an Emitter writing to a Write stream.
    // For WASM, we need:
    //
    // a) Load source from a string, not a file
    // b) Capture output to a String, not stdout
    // c) No CLI argument parsing (clap is cfg'd out for wasm32)
    // d) Step limit to prevent infinite loops
    // e) Convert ExecutionError to ErrorInfo with source locations
    //
    // The SourceLoader already supports loading from expressions (-e flag).
    // Study how the -e path works and adapt it.
    //
    // For output capture, create a Vec<u8> writer and pass it to the
    // emitter. After evaluation, convert to String.
    //
    // For the step limit, pass Some(MAX_STEPS) to machine.run().
    // A reasonable default: 1_000_000 steps.
    //
    // For error conversion, use ExecutionError::to_diagnostic() to get
    // the codespan Diagnostic, then extract location and message.

    todo!("implement evaluation pipeline for WASM")
}

/// Return the list of supported output formats as a JSON array.
#[wasm_bindgen]
pub fn formats() -> String {
    r#"["yaml","json","toml","text","edn","html"]"#.to_string()
}
```

**IMPORTANT NOTES FOR IMPLEMENTOR:**

The main challenge is creating a minimal evaluation path that doesn't depend on:
- `clap` (CLI parsing — already cfg'd out for wasm32)
- `dirs` (home directory — cfg'd out)
- `webbrowser` (cfg'd out)
- `lsp-server` / `lsp-types` (cfg'd out)
- File I/O for imports (prelude is embedded, no other imports needed)

Study how `EucalyptOptions` is constructed and what `prepare::prepare()` and `eval::run()` need from it. Consider creating a minimal `WasmOptions` struct that provides just what the pipeline needs.

The prelude loading path needs to work — it's loaded via `SourceLoader::new(lib_path)`. In WASM, the prelude should be available as an embedded resource or loaded from the lib/ directory relative to the binary. Check how this works in the existing WASM build.

- [ ] **Step 3: Add module to lib.rs**

In `src/lib.rs`, add:

```rust
#[cfg(target_arch = "wasm32")]
pub mod wasm;
```

- [ ] **Step 4: Build for WASM**

```bash
wasm-pack build --target web
```

This produces `pkg/` with the `.wasm` file and JS glue.

- [ ] **Step 5: Test the API**

Create a simple HTML test page:

```html
<script type="module">
  import init, { evaluate, formats } from './pkg/eucalypt.js';
  await init();
  console.log(formats());
  console.log(evaluate('{ hello: "world" }', 'json'));
</script>
```

Serve locally and verify in browser console.

- [ ] **Step 6: Commit**

```bash
git commit -m "feat: add wasm-bindgen JS API for browser evaluation"
```

### Task 2: CI for WASM build

**Files:**
- Modify: `.github/workflows/build.yml` (extend WASM check to include wasm-pack build)

- [ ] **Step 1: Add wasm-pack build step**

The existing "WASM Compilation Check" job does `cargo check`. Extend it to also run `wasm-pack build --target web` to verify the full WASM output is produced.

- [ ] **Step 2: Commit**

```bash
git commit -m "ci: add wasm-pack build to WASM compilation check"
```

---

## Part B: Browser Playground (eu-vgq4)

This is a separate repository. The plan below outlines the project structure and key implementation steps.

### Task 3: Create playground repository

- [ ] **Step 1: Create repo**

Create `curvelogic/eucalypt-playground` on GitHub. Initialise with:

```
eucalypt-playground/
├── index.html
├── src/
│   ├── main.ts          # Entry point
│   ├── editor.ts         # Editor setup (Monaco or CodeMirror)
│   ├── evaluator.ts      # WASM API wrapper
│   ├── ui.ts             # UI state management
│   └── examples.ts       # Example programs
├── styles/
│   └── playground.css
├── public/
│   └── tree-sitter-eucalypt.wasm  # Pre-built grammar
├── pkg/                   # WASM output (from wasm-pack, gitignored or committed)
├── package.json
├── tsconfig.json
├── vite.config.ts         # Or equivalent bundler
└── .github/
    └── workflows/
        └── deploy.yml     # GitHub Pages deployment
```

- [ ] **Step 2: Initialise beads**

```bash
cd eucalypt-playground
bd init
```

- [ ] **Step 3: Commit skeleton**

### Task 4: Editor with tree-sitter highlighting

**Files:**
- `src/editor.ts`
- `public/tree-sitter-eucalypt.wasm`

- [ ] **Step 1: Choose and set up editor**

Either Monaco or CodeMirror. Both support custom languages.

For **CodeMirror 6** (lighter, more modern):
- Install `@codemirror/view`, `@codemirror/state`, `@codemirror/language`
- Create a language support extension using web-tree-sitter for parsing

For **Monaco** (heavier, VS Code-like):
- Install `monaco-editor`
- Register custom language with monarch tokeniser or tree-sitter

- [ ] **Step 2: Integrate web-tree-sitter**

```typescript
import Parser from 'web-tree-sitter';

async function initTreeSitter(): Promise<Parser> {
  await Parser.init();
  const parser = new Parser();
  const lang = await Parser.Language.load('/tree-sitter-eucalypt.wasm');
  parser.setLanguage(lang);
  return parser;
}
```

The tree-sitter grammar WASM file needs to be built from the existing `tree-sitter-eucalypt` grammar. Check if the grammar exists in the main repo or in `editors/`:

The implementor should:
1. Find the tree-sitter grammar (likely in `editors/` or a separate repo)
2. Build it for web: `tree-sitter build --wasm`
3. Place the `.wasm` file in `public/`

- [ ] **Step 3: Connect tree-sitter to editor highlighting**

Use the tree-sitter parse tree to apply syntax highlighting classes to the editor content. The specifics depend on the editor choice.

- [ ] **Step 4: Commit**

### Task 5: Evaluation and UI

**Files:**
- `src/evaluator.ts`
- `src/ui.ts`
- `src/examples.ts`
- `index.html`
- `styles/playground.css`

- [ ] **Step 1: WASM evaluator wrapper**

```typescript
import init, { evaluate, formats } from '../pkg/eucalypt.js';

let ready = false;

export async function initEvaluator(): Promise<void> {
  await init();
  ready = true;
}

export interface EvalResult {
  success: boolean;
  output?: string;
  error?: {
    message: string;
    location?: {
      line: number;
      column: number;
      end_line: number;
      end_column: number;
    };
    notes?: string[];
  };
}

export function run(source: string, format: string): EvalResult {
  if (!ready) throw new Error('WASM not initialised');
  const json = evaluate(source, format);
  return JSON.parse(json);
}

export function getFormats(): string[] {
  return JSON.parse(formats());
}
```

- [ ] **Step 2: UI layout**

```html
<div id="playground">
  <header>
    <h1>Eucalypt Playground</h1>
    <select id="format-select"></select>
    <button id="run-btn">Run</button>
  </header>
  <main>
    <div id="editor-pane"></div>
    <div id="output-pane">
      <pre id="output"></pre>
      <div id="error-panel" class="hidden"></div>
    </div>
  </main>
</div>
```

- [ ] **Step 3: Wire up run button**

```typescript
document.getElementById('run-btn').addEventListener('click', () => {
  const source = editor.getValue();
  const format = formatSelect.value;
  const result = run(source, format);

  if (result.success) {
    outputEl.textContent = result.output;
    errorPanel.classList.add('hidden');
  } else {
    outputEl.textContent = '';
    errorPanel.textContent = result.error.message;
    errorPanel.classList.remove('hidden');
    // Highlight error location in editor if available
    if (result.error.location) {
      highlightError(result.error.location);
    }
  }
});
```

- [ ] **Step 4: Example programs**

```typescript
export const examples: Record<string, string> = {
  "Hello World": '{ hello: "world" }',
  "List Operations": `{
  numbers: [1, 2, 3, 4, 5]
  doubled: numbers map(* 2)
  sum: numbers foldl(+, 0)
  evens: numbers filter(even?)
}`,
  "String Processing": `{
  greeting: "hello, world"
  upper: greeting str.upper
  words: greeting str.split-on(" ")
}`,
  // ... more examples
};
```

- [ ] **Step 5: URL sharing**

```typescript
function encodeSource(source: string): string {
  return btoa(unescape(encodeURIComponent(source)));
}

function decodeSource(hash: string): string {
  return decodeURIComponent(escape(atob(hash)));
}

// On load: check URL hash
const hash = window.location.hash.slice(1);
if (hash) {
  editor.setValue(decodeSource(hash));
}

// Share button: update URL
function share() {
  window.location.hash = encodeSource(editor.getValue());
}
```

- [ ] **Step 6: Commit**

### Task 6: GitHub Pages deployment

**Files:**
- `.github/workflows/deploy.yml`

- [ ] **Step 1: Create deployment workflow**

```yaml
name: Deploy to GitHub Pages
on:
  push:
    branches: [main]

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Setup Node
        uses: actions/setup-node@v4
        with:
          node-version: '20'

      - name: Install dependencies
        run: npm ci

      - name: Build
        run: npm run build

      - name: Deploy to GitHub Pages
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./dist
```

The WASM binary should either be:
- Pre-built and committed to the playground repo
- Built as part of the deployment pipeline (requires Rust toolchain in CI)

For simplicity, pre-build and commit the WASM binary. Update it when the eucalypt API changes.

- [ ] **Step 2: Enable GitHub Pages**

In repo settings, enable GitHub Pages from the `gh-pages` branch.

- [ ] **Step 3: Verify deployment**

Push and verify the playground loads at `https://curvelogic.github.io/eucalypt-playground/`.

---

## Implementation Order

```
Task 1: WASM API in eucalypt repo
  ↓
Task 2: CI for WASM build
  ↓
Task 3: Create playground repo
  ↓
Task 4: Editor with tree-sitter
  ↓
Task 5: Evaluation and UI
  ↓
Task 6: GitHub Pages deployment
```

All tasks are sequential. The playground depends on the WASM API being built first.

## Key Risk: Evaluation Pipeline for WASM

The hardest part is Task 1's `evaluate_inner()` — wiring the existing evaluation pipeline to work from a string input without CLI dependencies. The implementor should:

1. Study `src/driver/eval.rs` `Executor::try_execute()` end-to-end
2. Study how `-e` (expression) mode loads source
3. Create a minimal path that bypasses `EucalyptOptions` / `clap`
4. Verify the prelude loads in WASM (it may need to be embedded as a const string if filesystem access isn't available)

The prelude embedding is critical — in native mode, the prelude is loaded from `lib/prelude.eu` relative to the binary. In WASM, there's no filesystem. Options:
- Embed prelude source as `include_str!("../../lib/prelude.eu")` at compile time
- Fetch it via HTTP in the playground
- Bundle it in the WASM binary

`include_str!` is simplest and most reliable.
