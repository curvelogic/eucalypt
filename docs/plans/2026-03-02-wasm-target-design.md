# WASM Target Design

**Bead:** eu-raaa
**Status:** Design approved, implementation plan pending

## Overview

Compile eucalypt to WebAssembly for use in browser playgrounds and
Node.js. The core evaluation engine is already well-isolated from
I/O — the main work is feature-gating OS-specific dependencies,
introducing an I/O abstraction layer, and exposing a JS API via
`wasm-bindgen`.

Delivery is phased: Phase 1 (compilation gate) is the 0.4.0 minimum;
Phases 2-4 deliver the full JS API, imports, streaming, and npm
package.

---

## Section 1: Architecture — Library Crate Split

The project is split into two crates:

**`eucalypt-core`** (library crate) — compiles to both native and
`wasm32-unknown-unknown`:
- `src/syntax/` — parsing
- `src/core/` — desugaring, cooking, transforms
- `src/eval/` — STG compiler, VM, GC, intrinsics
- `src/import/` — format parsers (YAML, JSON, TOML, CSV, XML, EDN)
- `src/export/` — format emitters
- `src/common/` — shared types
- `src/driver/` — evaluation pipeline, source loading

**`eucalypt` / `eu`** (binary crate) — native only:
- `src/bin/eu.rs` — clap argument parsing, file discovery, LSP server,
  terminal handling

The existing `src/lib.rs` + `src/bin/eu.rs` split is already partway
there. The key change: I/O operations in the driver move behind
traits so the library works with different backends.

---

## Section 2: I/O Abstraction Layer

Three categories of I/O to abstract:

### File Reading

```rust
pub trait ResourceLoader {
    fn read_resource(&self, path: &str) -> Result<String, EucalyptError>;
    fn resolve_import(&self, base: &str, relative: &str) -> String;
    fn resource_exists(&self, path: &str) -> bool;
}
```

Uses `&str` paths rather than `&Path` — in WASM there's no real
filesystem, paths are just keys into a virtual filesystem.

Native implementation: delegates to `std::fs`.
WASM implementation: looks up a `HashMap<String, String>` of
pre-loaded files.

### Environment Data

```rust
pub trait Environment {
    fn epoch_time(&self) -> f64;
    fn env_vars(&self) -> Vec<(String, String)>;
    fn random_seed(&self) -> u64;
    fn args(&self) -> Vec<String>;
}
```

Native: delegates to `std::time`, `std::env`.
WASM browser: `Date.now()` via `js_sys`, empty env vars,
`Math.random()` for seed.
WASM Node: can use Node's `process.env`, `Date.now()`, etc.

### Output

The emitter system already uses a trait (`Emitter`) that writes to
`dyn Write`. In WASM, a `Vec<u8>` buffer serves as the writer. No
changes needed.

---

## Section 3: Dependency Feature-Gating

Blocking dependencies are gated by target architecture:

```rust
#[cfg(not(target_arch = "wasm32"))]
```

| Crate | Gate | Reason |
|-------|------|--------|
| `dirs` | native only | Home directory lookup |
| `webbrowser` | native only | Opens browser (test reports) |
| `lsp-server` | native only | Stdio-based LSP |
| `clap` | native only | CLI arg parsing |
| `wasm-bindgen` | wasm only | JS interop |
| `js-sys` | wasm only | `Date.now()`, `Math.random()` |
| `web-sys` | wasm only | Console logging |

WASM-specific dependencies use target config in `Cargo.toml`:

```toml
[target.'cfg(target_arch = "wasm32")'.dependencies]
wasm-bindgen = "0.2"
js-sys = "0.3"
web-sys = { version = "0.3", features = ["console"] }
```

Code modules gated by target:
- `src/driver/project.rs` — config file discovery (native only)
- `src/driver/lsp/` — entire module (native only)
- `src/driver/io.rs` — split implementations per target
- `src/bin/eu.rs` — CLI binary (native only)
- `std::io::IsTerminal` usage in `options.rs` (native only)

Everything else compiles for both targets without gates.

---

## Section 4: WASM JavaScript API

Exposed via `wasm-bindgen`:

### Core evaluation function

```rust
#[wasm_bindgen]
pub fn evaluate(source: &str, options: JsValue) -> Result<String, JsError> {
    // options: { format: "yaml"|"json"|"toml",
    //            inputs: { "name": "content", ... } }
}
```

### Engine API for full feature parity

```rust
#[wasm_bindgen]
pub struct EucalyptEngine { /* ... */ }

#[wasm_bindgen]
impl EucalyptEngine {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self;

    /// Add a file to the virtual filesystem (for imports)
    pub fn add_file(&mut self, path: &str, content: &str);

    /// Add a named input (equivalent to `name=content@-` on CLI)
    pub fn add_input(&mut self, name: &str, content: &str, format: &str);

    /// Set the main source
    pub fn set_source(&mut self, source: &str);

    /// Set an environment variable
    pub fn set_env_var(&mut self, key: &str, value: &str);

    /// Set the timezone
    pub fn set_timezone(&mut self, tz: &str);

    /// Evaluate and return rendered output
    pub fn evaluate(&self, output_format: &str) -> Result<String, JsError>;

    /// Run tests and return results as JSON
    pub fn test(&self, source: &str) -> Result<String, JsError>;
}
```

### Virtual filesystem for imports

```javascript
const engine = new EucalyptEngine();
engine.addFile("lib/utils.eu", "helper(x): x + 1");
engine.addFile("data.yaml", "items:\n  - a\n  - b");
engine.setSource('import "lib/utils" ...');
const result = engine.evaluate("json");
```

The `ResourceLoader` trait implementation for WASM stores files in a
`HashMap<String, String>` and resolves imports against it.

In Node.js, a wrapper package could implement `ResourceLoader` using
Node's `fs` module via callbacks, giving transparent file loading.

---

## Section 5: Streaming Imports

Eucalypt supports streaming imports (JSONL, CSV, line-by-line text).
These currently use `File::open` and `BufReader`.

**Native:** Works as now.

**WASM:** Pre-load entire content. The JS side provides the full file
content as a string. The WASM side parses it in one go rather than
streaming. The `StreamProducer` trait's `fn next() -> Option<Primitive>`
interface is implemented by parsing the full input upfront into a
`Vec` and yielding from it.

The `src/import/stream.rs` module gets feature-gated:
- native: opens files, reads lazily via `BufReader`
- wasm: receives pre-loaded string, parses eagerly into `Vec`

JS callback-based streaming for large data is deferred to Phase 4.

---

## Section 6: Time, Random, and System Intrinsics

The `__IO` pseudoblock provides system data. WASM implementations:

| Intrinsic | Native | WASM |
|-----------|--------|------|
| `EPOCHTIME` | `SystemTime::now()` | `js_sys::Date::now()` |
| `TIMEZONE` | `chrono::Local` | Provided via `set_timezone()` |
| `ENV` vars | `std::env::vars()` | Provided via `set_env_var()` |
| Random seed | `SystemTime` nanos | `js_sys::Math::random()` |
| `UUID` | `uuid::Uuid::new_v4()` | Seeded from `Math.random()` |

Two trait implementations:

```rust
pub struct NativeEnvironment;    // #[cfg(not(target_arch = "wasm32"))]
pub struct WasmEnvironment;      // #[cfg(target_arch = "wasm32")]
```

Chrono itself is WASM-compatible — date parsing, formatting, and
arithmetic all work. Only the "current time" source differs.

---

## Section 7: Build and CI

### Cargo configuration

```toml
[lib]
crate-type = ["cdylib", "rlib"]

[target.'cfg(target_arch = "wasm32")'.dependencies]
wasm-bindgen = "0.2"
js-sys = "0.3"
web-sys = { version = "0.3", features = ["console"] }
```

`cdylib` for `wasm-pack`, `rlib` for native linking.

### CI WASM check

Add to GitHub Actions workflow:

```yaml
wasm:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - uses: dtolnay/rust-toolchain@stable
      with:
        targets: wasm32-unknown-unknown
    - run: cargo build --target wasm32-unknown-unknown --lib
```

This ensures every PR keeps the WASM target compiling. Tests run
native-only (harness tests use file I/O).

### npm package (future)

`wasm-pack build --target web` produces a package publishable to npm
as `@curvelogic/eucalypt-wasm` or similar.

---

## Section 8: Phased Delivery

**Phase 1: Compilation gate** (0.4.0 minimum)
- Feature-gate blocking dependencies and modules
- `cargo build --target wasm32-unknown-unknown --lib` succeeds
- Add CI check
- No JS API yet

**Phase 2: Core eval API**
- Implement `ResourceLoader` and `Environment` traits
- Expose `evaluate(source, options) -> Result<String>` via
  `wasm-bindgen`
- Pre-loaded inputs only, no streaming
- Browser playground becomes possible

**Phase 3: Full API**
- `EucalyptEngine` with virtual filesystem and imports
- Named data inputs, test runner
- Environment variable and timezone configuration
- Node.js library becomes practical

**Phase 4: Streaming and polish**
- JS callback-based streaming for large data
- Published npm package
- Usage documentation and examples

Each phase is independently useful and shippable.

---

## Compatibility Notes

Already WASM-compatible (no changes needed):
- Core evaluation engine (STG compiler, VM, GC)
- All parsers (rowan, yaml-rust, toml, quick-xml, edn-format)
- All export formats (JSON, YAML, TOML, EDN, HTML, text)
- Date/time operations (chrono, chrono-tz)
- Data structures (indexmap, petgraph, itertools, im)
- Crypto (base64, sha2)
- Custom memory management (bump allocation, mark-sweep GC)

## Out of Scope

- LSP server in WASM (not meaningful in browser/Node context)
- Browser UI for the playground (separate project)
- WASI target (potential future alternative to wasm-bindgen)
