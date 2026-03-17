# Monadic Blocks Without .expr and Bracket Registry

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> **MANDATORY**: Before writing ANY eucalypt (.eu) code, read: `docs/reference/agent-reference.md`, `docs/appendices/syntax-gotchas.md`, `docs/appendices/cheat-sheet.md`

**Goal:** (1) Allow monadic blocks without `.expr` by synthesising an implicit return block from bind names. (2) Replace the parser's colon-peeking heuristic for bracket content with a proper bracket registry.

**Architecture:** Both features centre on registries in the desugarer. The monad namespace registry tracks which names are monads (via `monad: true` declaration metadata). The bracket registry tracks which bracket pairs expect block-mode content (from bracket pair declarations). A single Desugarer instance carries both registries across imports. For the parser, a pre-scan or import-time extraction populates a parser-level bracket registry before full parsing.

**Tech Stack:** Rust (desugarer, parser)

---

## Part A: Monadic Blocks Without .expr (eu-2jbn)

### Task 1: Register monad namespaces from declaration metadata

**Files:**
- Modify: `src/core/desugar/desugarer.rs` (add namespace registry alongside bracket registry)
- Modify: `src/core/desugar/rowan_ast.rs` (detect `monad: true` on declarations, register)

- [ ] **Step 1: Extend Desugarer with monad namespace registry**

In `src/core/desugar/desugarer.rs`, add alongside the existing `monad_registry`:

```rust
/// Registry mapping namespace names (e.g. "io") to MonadSpec.
/// Populated when a declaration with `monad: true` metadata is processed.
monad_namespace_registry: HashMap<String, MonadSpec>,
```

Add accessor methods:

```rust
pub fn register_monad_namespace(&mut self, name: String, spec: MonadSpec) {
    self.monad_namespace_registry.insert(name, spec);
}

pub fn monad_namespace_spec(&self, name: &str) -> Option<&MonadSpec> {
    self.monad_namespace_registry.get(name)
}
```

Initialise as empty in `Desugarer::new()`.

- [ ] **Step 2: Detect `monad: true` on top-level declarations**

In `src/core/desugar/rowan_ast.rs`, find where top-level declarations are desugared. When a declaration has metadata containing `monad: true`:

1. Extract the declaration name
2. Create `MonadSpec::Namespace(name.clone())`
3. Call `desugarer.register_monad_namespace(name, spec)`

The metadata is available in the raw Rowan AST before desugaring the body. The implementor needs to:
- Find the declaration desugaring entry point (the `Desugarable` impl for `Declaration` or `Unit`)
- Check the declaration's metadata block for a `monad` key with value `true`
- This must happen BEFORE the declaration body is desugared, so that monadic blocks in the same file (after this declaration) can find it

**Critical:** Only register at the top level. Declarations nested inside blocks should not register. Check the desugarer's stack depth or context.

- [ ] **Step 3: Build, verify no regressions**

```bash
cargo build
cargo test
```

- [ ] **Step 4: Commit**

```bash
git commit -m "feat: register monad namespaces from monad: true declaration metadata"
```

### Task 2: Use namespace registry for monadic block detection

**Files:**
- Modify: `src/core/desugar/rowan_ast.rs` (change block metadata monadic detection)

- [ ] **Step 1: Replace `extract_block_monad_spec_from_raw` usage**

In `src/core/desugar/rowan_ast.rs` at lines 1793-1846 (the Block element with monadic metadata path in soup desugaring):

Currently:
```rust
if let Some(spec) = extract_block_monad_spec_from_raw(block) {
```

Change to: extract the symbol from the block's metadata, then look it up in the namespace registry:

```rust
if let Some(ns_name) = extract_block_metadata_symbol(block) {
    if let Some(spec) = desugarer.monad_namespace_spec(&ns_name) {
        let spec = spec.clone();
        // ... proceed with monadic desugaring
    }
}
```

Write `extract_block_metadata_symbol(block) -> Option<String>` — this extracts a bare symbol from block metadata (the `:io` in `{ :io ... }`). This is simpler than `extract_block_monad_spec_from_raw` because it just gets the symbol name; the MonadSpec comes from the registry.

This means `{ :io-shell cmd: c }` will NOT be treated as monadic (because `io-shell` is not in the namespace registry), which is the correct behaviour.

- [ ] **Step 2: Keep `extract_block_monad_spec_from_raw` for bracket blocks**

The BracketBlock path (lines 1719-1791) uses the bracket monad_registry, not the namespace registry. This path stays as-is. The two registries serve different purposes:
- `monad_registry`: bracket pair name → MonadSpec (for `⟦ ... ⟧`)
- `monad_namespace_registry`: namespace name → MonadSpec (for `{ :io ... }`)

- [ ] **Step 3: Test existing monadic blocks still work**

```bash
cargo test test_harness_103   # IO basic
cargo test test_harness_104   # IO basic
cargo test test_harness_119   # monad utility
```

- [ ] **Step 4: Commit**

```bash
git commit -m "feat: use monad namespace registry for block monadic detection"
```

### Task 3: Implement implicit return (no .expr)

**Files:**
- Modify: `src/core/desugar/rowan_ast.rs` (handle missing .expr)

- [ ] **Step 1: Change the "no .expr" path from error to implicit return**

In the Block monadic detection path (around line 1843-1844), currently:
```rust
// No .expr following — fall through to normal block desugaring
```

Change to: if the block IS monadic (namespace found in registry) but has no `.expr`, synthesise the implicit return:

```rust
if let Some(ns_name) = extract_block_metadata_symbol(block) {
    if let Some(spec) = desugarer.monad_namespace_spec(&ns_name) {
        let spec = spec.clone();
        let block_span = text_range_to_span(block.syntax().text_range());
        let smid = desugarer.new_smid(block_span);

        // Check for .expr pattern
        if /* .expr lookahead succeeds */ {
            // Existing path: use explicit return expression
        } else {
            // NEW: implicit return — synthesise block from bind names
            let block_decls: Vec<rowan_ast::Declaration> = block.declarations().collect();
            let monadic_expr = desugar_monadic_block_implicit_return(
                smid, block_decls, &spec, desugarer
            )?;
            soup.push(monadic_expr);
            idx += 1;
            continue;
        }
    }
}
```

Similarly for the BracketBlock path (lines 1766-1777): change the error to call `desugar_monadic_block_implicit_return`.

- [ ] **Step 2: Implement `desugar_monadic_block_implicit_return`**

New function, based on `desugar_monadic_block` but synthesising the return expression:

```rust
fn desugar_monadic_block_implicit_return(
    smid: Smid,
    decls: Vec<rowan_ast::Declaration>,
    spec: &MonadSpec,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    if decls.is_empty() {
        return Err(CoreError::EmptyMonadicBlock(smid));
    }

    // Extract bind names
    let bind_names: Vec<String> = decls
        .iter()
        .map(extract_declaration_name)
        .collect::<Result<Vec<_>, CoreError>>()?;

    // Push bind names into environment
    if !bind_names.is_empty() {
        desugarer.env_mut().push_keys(bind_names.iter().cloned());
    }

    // Collect bind FreeVars
    let bind_vars: Vec<_> = bind_names
        .iter()
        .map(|name| desugarer.env().get(name).unwrap().clone())
        .collect();

    // Desugar all declaration bodies
    let mut name_value_pairs: Vec<(String, RcExpr)> = Vec::with_capacity(decls.len());
    for (i, decl) in decls.iter().enumerate() {
        let decl_name = bind_names[i].clone();
        let span = text_range_to_span(decl.syntax().text_range());
        let value = /* desugar body — same as desugar_monadic_block */;
        name_value_pairs.push((decl_name, value));
    }

    // Synthesise return expression: a block of all non-underscore bind names
    // { k1: k1, k2: k2, ... } where each ki refers to the lambda-bound variable
    let non_underscore: Vec<(&String, &moniker::FreeVar<String>)> = bind_names
        .iter()
        .zip(bind_vars.iter())
        .filter(|(name, _)| *name != "_")
        .collect();

    let return_expr = if non_underscore.is_empty() {
        // No non-underscore bindings — return empty block / unit
        core::block(smid, vec![])
    } else {
        // Build block: { k1: k1_var, k2: k2_var, ... }
        let pairs: Vec<(Smid, String, RcExpr)> = non_underscore
            .iter()
            .map(|(name, var)| {
                let var_expr = RcExpr::from(Expr::Var(smid, (*var).clone()));
                (smid, (*name).clone(), var_expr)
            })
            .collect();
        core::block(smid, pairs)
    };

    // Pop bind names
    if !bind_names.is_empty() {
        desugarer.env_mut().pop();
    }

    // Build bind chain (same as desugar_monadic_block)
    // ... m.return(return_expr) wrapped in nested binds
}
```

**IMPORTANT:** The implementor needs to check how `core::block` constructs a block expression in the core AST. Look at how blocks are built elsewhere in the desugarer. The key challenge is creating `{r: r}` where `r` on the RHS is the lambda-bound variable, NOT a self-referential block binding. The core AST uses `Var` nodes with `FreeVar` references, which correctly refer to the lambda binding.

Consider refactoring: `desugar_monadic_block` and `desugar_monadic_block_implicit_return` share most of their logic. Extract the common parts (name extraction, body desugaring, bind chain construction) and only differ in how the return expression is obtained.

- [ ] **Step 3: Write tests**

Create `tests/harness/127_monadic_implicit_return.eu`:

```eu
"Monadic blocks with implicit return (no .expr)"

# Simple implicit return — all non-underscore bindings in a block
` { target: :test-implicit-return requires-io: true }
test-implicit-return: {
  result: { :io
    r: io.shell("echo hello")
    _: io.check(r)
  }
  RESULT: if(result.r.stdout str.starts-with?("hello"), :PASS, :FAIL)
}

# Multiple bindings in implicit return
` { target: :test-multi-binding requires-io: true }
test-multi-binding: {
  result: { :io
    a: io.shell("echo first")
    b: io.shell("echo second")
  }
  pass: (result.a.stdout str.starts-with?("first")) && (result.b.stdout str.starts-with?("second"))
  RESULT: pass then(:PASS, :FAIL)
}

# Underscore bindings excluded from implicit return
` { target: :test-underscore-excluded requires-io: true }
test-underscore-excluded: {
  result: { :io
    _: io.shell("echo setup")
    r: io.shell("echo result")
  }
  # result should have r but not _
  RESULT: if(result has(:r), :PASS, :FAIL)
}

# Explicit .expr still works (backwards compatibility)
` { target: :test-explicit-return requires-io: true }
test-explicit-return: {
  result: { :io
    r: io.shell("echo hello")
    _: io.check(r)
  }.r.stdout
  RESULT: if(result str.starts-with?("hello"), :PASS, :FAIL)
}
```

Add to `tests/harness_test.rs`:
```rust
harness_test!(test_harness_127, "127_monadic_implicit_return");
```

- [ ] **Step 4: Run tests**

```bash
cargo test test_harness_127
cargo test  # full suite
```

- [ ] **Step 5: Commit**

```bash
git commit -m "feat: monadic blocks with implicit return from bind names"
```

### Task 4: Update prelude — add monad: true and let monad

**Files:**
- Modify: `lib/prelude.eu`

- [ ] **Step 1: Add monad: true to io declaration**

Change:
```eu
` "IO related declarations..."
io: monad{...} { ... }
```

To:
```eu
` { doc: "IO related declarations. The io namespace is a monad."
    monad: true }
io: monad{...} { ... }
```

- [ ] **Step 2: Add monad: true to random declaration**

Find the `random:` namespace declaration and add `monad: true` to its metadata similarly.

- [ ] **Step 3: Add let monad**

```eu
` { doc: "Identity monad for sequential let-bindings. Use { :let x: expr1  y: f(x) } for sequential evaluation without the {x: x} self-reference gotcha."
    monad: true }
let: monad({bind(m, f): f(m), return: identity})
```

- [ ] **Step 4: Test let monad**

Add to the test file or a new one:

```eu
` { target: :test-let-monad }
test-let-monad: {
  result: { :let
    x: 2 + 3
    y: x * 2
  }
  RESULT: if(result.x = 5 && result.y = 10, :PASS, :FAIL)
}
```

- [ ] **Step 5: Run full test suite**

```bash
cargo test
```

- [ ] **Step 6: Commit**

```bash
git commit -m "feat: add monad: true to io/random, add let monad namespace"
```

### Task 5: Documentation

**Files:**
- Modify: `docs/guide/monads.md`
- Modify: `docs/reference/prelude/io.md`

- [ ] **Step 1: Document implicit return**

In `docs/guide/monads.md`, add section explaining:
- `{ :io ... }.expr` still works (explicit return)
- `{ :io ... }` without `.expr` returns a block of all non-underscore bind names
- Examples of both forms
- The `let` monad for sequential bindings

- [ ] **Step 2: Document `monad: true` metadata**

Explain how to create custom monad namespaces:
```eu
` { monad: true }
my-monad: monad({bind: my-bind, return: my-return})
```

Document the top-level only restriction.

- [ ] **Step 3: Update io.md reference**

Update any examples that show `.expr` as required.

- [ ] **Step 4: Commit**

```bash
git commit -m "docs: document implicit monadic return and monad: true metadata"
```

---

## Part B: Bracket Registry (eu-8dh7)

### Task 6: Parser-level bracket content type registry

**Files:**
- Modify: `src/syntax/rowan/parse.rs` (add registry, remove heuristic)
- Modify: `src/syntax/rowan/brackets.rs` (add content type to BracketPair)
- Modify: `src/driver/source.rs` (pass registry to parser if needed)

- [ ] **Step 1: Design the registry integration**

The key challenge: the parser runs before the desugarer, but bracket pair declarations are in the source being parsed. Options:

**Option A: Two-pass parsing**
1. First pass: scan tokens for bracket pair declaration patterns (identifier, bracket-open, brace-open, brace-close, bracket-close, colon). This is a simple token-level scan, not a full parse.
2. Register discovered bracket pairs as block-mode.
3. Second pass: full parse using the registry.

**Option B: Parser recognises bracket declarations inline**
When the parser encounters a pattern that looks like a bracket pair declaration (e.g. `⟦{}⟧` as a declaration head), it registers the bracket pair immediately before continuing. This requires the parser to look ahead slightly.

**Option C: Import-time pre-registration**
When loading imported files (including prelude), extract bracket pair declarations and pass them to the parser for subsequent files. For the current file's own declarations, fall back to a heuristic or require bracket declarations before first use.

The implementor should evaluate these options. Option A is simplest to implement. Option B is more elegant. Option C handles cross-file but not same-file ordering.

**For 0.5.1, a pragmatic approach:** The prelude doesn't currently define any bracket pairs. If bracket pairs are always defined in imported files (never the same file where they're used), Option C works. For same-file use, require bracket declarations to appear before their first use, and do a pre-scan (Option A).

- [ ] **Step 2: Add content type to bracket registration**

In `src/syntax/rowan/brackets.rs` or a new registry module:

```rust
#[derive(Clone, Debug)]
pub enum BracketContentType {
    /// Block-mode: content parsed as declarations (like {})
    Block,
    /// Expression-mode: content parsed as a soup expression
    Expression,
    // Future: List, etc.
}

/// Parser-level registry of bracket pairs and their content types
#[derive(Default, Clone, Debug)]
pub struct BracketRegistry {
    pairs: HashMap<char, BracketContentType>,
}

impl BracketRegistry {
    pub fn register(&mut self, open: char, content_type: BracketContentType) {
        self.pairs.insert(open, content_type);
    }

    pub fn content_type(&self, open: char) -> Option<&BracketContentType> {
        self.pairs.get(&open)
    }
}
```

- [ ] **Step 3: Replace heuristic in parser**

In `src/syntax/rowan/parse.rs`, replace `peek_for_top_level_colon_in_bracket()` usage:

```rust
// OLD:
if self.peek_for_top_level_colon_in_bracket() {
    // block mode
} else {
    // expression mode
}

// NEW:
let open_char = /* extract from current BRACKET_OPEN token */;
match self.bracket_registry.content_type(open_char) {
    Some(BracketContentType::Block) => {
        // block mode
    }
    _ => {
        // expression mode (default for unknown brackets)
    }
}
```

Remove `peek_for_top_level_colon_in_bracket()` entirely.

- [ ] **Step 4: Wire registry into parser**

The parser needs to receive the registry. Options:
- Pass it as a parameter to the parser constructor
- Build it from a pre-scan of the token stream

The implementor needs to trace how the parser is constructed (find `Parser::new` or equivalent) and add the registry parameter.

- [ ] **Step 5: Implement pre-scan for bracket declarations**

Add a function that scans a token stream for bracket pair declaration patterns:

```rust
fn prescan_bracket_declarations(tokens: &[Token]) -> BracketRegistry {
    let mut registry = BracketRegistry::default();
    // Look for patterns like:
    //   BRACKET_OPEN OPEN_BRACE CLOSE_BRACE BRACKET_CLOSE COLON
    // or:
    //   OPEN_PAREN BRACKET_OPEN OPEN_BRACE CLOSE_BRACE BRACKET_CLOSE CLOSE_PAREN COLON
    // Register the bracket pair as block-mode.
    registry
}
```

This is a simple token-level scan — no full parsing needed.

- [ ] **Step 6: Handle imported bracket declarations**

When processing imports, pass the bracket registry from imported files to the parser for the importing file. This may require changes to `src/driver/source.rs` to accumulate bracket registrations across imports.

- [ ] **Step 7: Test**

Verify:
1. Existing bracket pair tests still pass
2. The colon heuristic is gone — a bracket expression with an incidental colon is NOT misdetected as block-mode
3. Bracket pair declarations properly register

```bash
cargo test
```

- [ ] **Step 8: Commit**

```bash
git commit -m "feat: replace bracket content heuristic with proper bracket registry"
```

### Task 7: Error for unexpected colon in unregistered brackets

**Files:**
- Modify: `src/syntax/rowan/validate.rs` or `src/core/desugar/rowan_ast.rs`

- [ ] **Step 1: Add validation**

When the parser encounters a bracket expression (expression-mode) that contains a top-level colon, emit a warning or error: "unexpected ':' inside bracket expression — did you mean to define a bracket pair declaration?"

This catches the case where a user writes `⟦ a: x ⟧` without having declared the bracket pair.

- [ ] **Step 2: Test the error**

Create an error test:
```eu
# Using undeclared bracket pair with block-like content
result: ⟦ a: 1 ⟧
```

Expected error about undeclared bracket pair or unexpected colon.

- [ ] **Step 3: Commit**

```bash
git commit -m "feat: error on unexpected colon in unregistered bracket expression"
```

---

## Implementation Order

```
Task 1: Monad namespace registry in desugarer
  ↓
Task 2: Use namespace registry for { :io ... } detection
  ↓
Task 3: Implicit return (no .expr)
  ↓
Task 4: Prelude changes (monad: true, let monad)
  ↓
Task 5: Documentation

Task 6: Parser bracket registry (independent of Tasks 1-5)
  ↓
Task 7: Colon error for unregistered brackets
```

Parts A and B are independent and can be parallelised.
