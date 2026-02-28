# Monadic Blocks Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Allow block declarations inside user-defined bracket pairs to be desugared as monadic bind chains, with the generalised lookup expression as the return value.

**Architecture:** Monadic brackets are distinguished from idiot brackets by the `{}` parameter in the bracket definition (e.g., `⟦{}⟧: { :monad bind: rb return: rr }`). The parser uses content-based detection (top-level colons) to parse bracket contents as declarations. At desugar time, the monadic bracket expression and its subsequent `.expr` lookup are consumed together and emitted as nested `bind(action, λname. ...)` applications with `return(expr)` at the leaf.

**Tech Stack:** Rust, Rowan parser

**Design doc:** `docs/plans/2026-02-28-monadic-blocks-design.md`

**Depends on:** Idiot brackets implementation (shared lexer/parser/AST infrastructure)

---

## Key Concept: Bind Chain Desugaring

A monadic block:

```eucalypt
⟦ a: action1
  b: action2(a)
  c: action3(a, b)
⟧.[a, b, c]
```

Desugars to:

```
bind(action1, λa.
  bind(action2(a), λb.
    bind(action3(a, b), λc.
      return([a, b, c]))))
```

Where `bind` and `return` are names from the monad specification.
Each declaration becomes a `bind(value, λname. rest)` call.
The `.expr` lookup becomes the `return(expr)` at the innermost level.
Bindings are sequential — each can reference earlier ones but not
later ones.

---

## Monad Spec Sources

Two ways to specify a monad:

1. **Bracket pair definition** (primary):
   ```eucalypt
   ⟦{}⟧: { :monad  bind: rand-bind  return: rand-return }
   ```
   The `{}` parameter signals block-mode parsing. The value is a block
   with `:monad` unit metadata containing `bind` and `return`
   declarations.

2. **Inline block metadata** (secondary):
   ```eucalypt
   { ` { :monad  bind: list-bind  return: list-return }
     x: [1, 2, 3]
     y: [10, 20]
   }.[x, y]
   ```
   The backtick attaches the monad spec as metadata to the block.

Both sources are statically extractable at desugar time.

---

### Task 1: Block-Mode Bracket Parsing

Extend the parser (from idiot brackets infrastructure) to parse
bracket contents as block declarations when top-level colons are
detected.

**Files:**
- Modify: `src/syntax/rowan/kind.rs` (add `BRACKET_BLOCK`)
- Modify: `src/syntax/rowan/parse.rs` (block-mode bracket parsing)
- Modify: `src/syntax/rowan/ast.rs` (add `BracketBlock` AST node)

**Step 1: Add BRACKET_BLOCK syntax kind**

In `src/syntax/rowan/kind.rs`, add after `BRACKET_EXPR`:
```rust
    /// User-defined bracket block expression e.g. ⟦ a: x  b: y ⟧
    BRACKET_BLOCK,
```

**Step 2: Add BracketBlock AST node**

In `src/syntax/rowan/ast.rs`, add after `BracketExpr`:

```rust
// A user-defined bracket block expression e.g. ⟦ a: x  b: y ⟧
//
// Parsed like a block (with declarations) but delimited by user
// brackets instead of braces. Used for monadic blocks.
ast_node!(BracketBlock, BRACKET_BLOCK);

impl BracketBlock {
    pub fn open_bracket(&self) -> Option<SyntaxToken> {
        support::syntax_token(self.syntax(), SyntaxKind::RESERVED_OPEN)
    }

    pub fn close_bracket(&self) -> Option<SyntaxToken> {
        support::syntax_token(self.syntax(), SyntaxKind::RESERVED_CLOSE)
    }

    pub fn open_char(&self) -> Option<char> {
        self.open_bracket()
            .and_then(|t| t.text().chars().next())
    }

    pub fn bracket_name(&self) -> Option<String> {
        self.open_char()
            .and_then(crate::syntax::rowan::brackets::bracket_pair_name)
    }

    pub fn declarations(&self) -> AstChildren<Declaration> {
        support::children::<Declaration>(self.syntax())
    }

    pub fn meta(&self) -> Option<BlockMetadata> {
        support::child::<BlockMetadata>(self.syntax())
    }
}
```

Add `BracketBlock` to the `Element` enum and its `can_cast`/`cast`
implementations.

**Step 3: Parser — content-based mode detection**

Modify `parse_bracket_expression` in `src/syntax/rowan/parse.rs` to
detect whether the bracket content contains top-level colons. If so,
parse as block content; otherwise parse as expression soup.

The detection approach: after consuming the opening bracket, peek at
tokens. If we find a COLON at depth 0 (not inside nested brackets),
parse as block content using the existing `BlockEventSink`. Otherwise,
parse as soup (expression mode).

```rust
fn parse_bracket_expression(&mut self) {
    // Record opening bracket
    let open_token = self.next();

    // Peek ahead to detect block mode (top-level colons)
    let is_block_mode = self.peek_for_top_level_colon();

    if is_block_mode {
        self.sink().start_node(BRACKET_BLOCK);
        if let Some((_, _)) = open_token {
            self.sink().token(RESERVED_OPEN);
        }
        self.add_trivia();
        self.parse_block_content_until(RESERVED_CLOSE);
        self.expect(RESERVED_CLOSE);
        self.sink().finish_node();
    } else {
        self.sink().start_node(BRACKET_EXPR);
        if let Some((_, _)) = open_token {
            self.sink().token(RESERVED_OPEN);
        }
        self.add_trivia();
        self.parse_soup();
        self.expect(RESERVED_CLOSE);
        self.sink().finish_node();
    }
}
```

The `peek_for_top_level_colon` method scans ahead through the token
stream (tracking bracket depth) looking for a COLON at depth 0.
This requires buffering — the parser may need a lookahead buffer
or the ability to checkpoint and backtrack. Alternatively, use the
existing `token_buffer` mechanism or convert to two-pass parsing for
bracket content.

A simpler approach: try parsing as block content. If no declarations
are found (no colons), re-parse as expression soup. The
`BlockEventSink` already handles blocks with just metadata (no
declarations).

Note: The exact implementation of `parse_block_content_until` may
require factoring out the termination condition from
`parse_block_content` (currently hardcoded to expect `CLOSE_BRACE`).
Make it parameterisable on the expected closing token.

**Step 4: Run tests**

Run: `cargo test --lib`
Expected: PASS

**Step 5: Commit**

```bash
git add src/syntax/rowan/kind.rs src/syntax/rowan/parse.rs src/syntax/rowan/ast.rs
git commit -m "feat: block-mode bracket parsing with content-based detection"
```

---

### Task 2: BracketBlock Validation

Add validation for bracket block nodes — verify matching pairs and
validate declarations.

**Files:**
- Modify: `src/syntax/rowan/validate.rs`

**Step 1: Add BracketBlock validation**

```rust
impl Validatable for BracketBlock {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        // Verify matching bracket pair
        if let (Some(open), Some(close)) = (self.open_bracket(), self.close_bracket()) {
            let open_char = open.text().chars().next();
            let close_char = close.text().chars().next();
            if let Some(oc) = open_char {
                let expected = crate::syntax::rowan::brackets::closing_bracket(oc);
                if expected != close_char {
                    errors.push(ParseError::MismatchedBrackets {
                        open_range: open.text_range(),
                        close_range: close.text_range(),
                    });
                }
            }
        } else if self.close_bracket().is_none() {
            errors.push(ParseError::UnterminatedBracketExpr {
                open_range: self.open_bracket().map(|t| t.text_range()),
                range: self.syntax().text_range(),
            });
        }

        // Validate block metadata
        if let Some(m) = self.meta() {
            m.validate(errors);
        }

        // Validate declarations
        for d in self.declarations() {
            d.validate(errors);
        }
    }
}
```

Add `BracketBlock` to `Element::validate`.

**Step 2: Run tests**

Run: `cargo test --lib`
Expected: PASS

**Step 3: Commit**

```bash
git add src/syntax/rowan/validate.rs
git commit -m "feat: validation for bracket block nodes"
```

---

### Task 3: Monad Spec Extraction

Add infrastructure to extract `bind` and `return` names from a monad
specification block. This is used both for bracket pair definitions
and inline metadata.

**Files:**
- Create: `src/core/desugar/monad.rs`
- Modify: `src/core/desugar/mod.rs` (register module)

**Step 1: Create monad spec extraction module**

```rust
// src/core/desugar/monad.rs
//! Monad specification extraction for monadic block desugaring.

use crate::common::sourcemap::Smid;
use crate::core::error::CoreError;
use crate::core::expr::RcExpr;
use crate::syntax::rowan::ast as rowan_ast;

/// Extracted monad specification — bind and return function names.
#[derive(Debug, Clone)]
pub struct MonadSpec {
    pub bind: String,
    pub return_: String,
}

/// Extract a monad spec from a block AST node that has `:monad` unit
/// metadata and `bind`/`return` declarations.
///
/// The block must have the form:
/// ```text
/// { :monad  bind: <name>  return: <name> }
/// ```
///
/// Returns None if the block doesn't have `:monad` metadata.
/// Returns Err if `:monad` is present but bind/return are missing
/// or not simple names.
pub fn extract_monad_spec_from_block(
    block: &rowan_ast::Block,
    smid: Smid,
) -> Result<Option<MonadSpec>, CoreError> {
    // Check for :monad unit metadata
    let has_monad_meta = block.meta().and_then(|m| {
        m.soup().and_then(|s| {
            s.singleton().and_then(|e| {
                // Check if the metadata is the symbol :monad
                if let rowan_ast::Element::Lit(lit) = e {
                    let text = lit.syntax().text().to_string();
                    if text == ":monad" {
                        Some(())
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
        })
    });

    if has_monad_meta.is_none() {
        return Ok(None);
    }

    // Extract bind and return names from declarations
    let mut bind_name = None;
    let mut return_name = None;

    for decl in block.declarations() {
        if let Some(head) = decl.head() {
            let kind = head.classify_declaration();
            if let rowan_ast::DeclarationKind::Property(prop) = kind {
                let key = prop.text().to_string();
                if key == "bind" {
                    bind_name = extract_single_name_from_body(&decl);
                } else if key == "return" {
                    return_name = extract_single_name_from_body(&decl);
                }
            }
        }
    }

    match (bind_name, return_name) {
        (Some(b), Some(r)) => Ok(Some(MonadSpec {
            bind: b,
            return_: r,
        })),
        (None, _) => Err(CoreError::MonadSpecMissingBind(smid)),
        (_, None) => Err(CoreError::MonadSpecMissingReturn(smid)),
    }
}

/// Extract a single name from a declaration body.
/// The body must contain exactly one element which is a normal
/// identifier.
fn extract_single_name_from_body(decl: &rowan_ast::Declaration) -> Option<String> {
    decl.body().and_then(|body| {
        body.soup().and_then(|soup| {
            soup.singleton().and_then(|element| {
                element
                    .as_normal_identifier()
                    .map(|id| id.text().to_string())
            })
        })
    })
}
```

**Step 2: Add error variants**

In `src/core/error.rs`, add:
```rust
MonadSpecMissingBind(Smid),
MonadSpecMissingReturn(Smid),
MonadSpecMissingLookup(Smid),
```

**Step 3: Register the module**

In `src/core/desugar/mod.rs`:
```rust
pub mod monad;
```

**Step 4: Run tests**

Run: `cargo test --lib`
Expected: PASS

**Step 5: Commit**

```bash
git add src/core/desugar/monad.rs src/core/desugar/mod.rs src/core/error.rs
git commit -m "feat: monad specification extraction from block AST"
```

---

### Task 4: Bracket Pair Definition Registry

During block desugaring, collect bracket pair definitions that have
`:monad` metadata so they're available when desugaring bracket usage
later in the same scope.

**Files:**
- Modify: `src/core/desugar/rowan_ast.rs` (Desugarer state, block
  desugaring)

**Step 1: Add monad spec registry to Desugarer**

The `Desugarer` struct needs a map from bracket pair name to
`MonadSpec`. This should be scoped (pushed/popped with block scopes)
so inner definitions can override outer ones.

```rust
// In the Desugarer struct, add:
monad_specs: SimpleEnvironment<String, monad::MonadSpec>,
```

Or simpler: a `Vec<HashMap<String, MonadSpec>>` stack that's pushed
when entering a block and popped when leaving.

**Step 2: Collect monad specs during block desugaring**

In the block desugaring code (`Desugarable for Block`), when
processing declarations, check if any declaration is a BracketPair
whose value is a block with `:monad` metadata.

When `extract_rowan_declaration_components` encounters a BracketPair
declaration:
1. Get the bracket pair name from the declaration head
2. Inspect the value (declaration body) — if it's a block with
   `:monad` metadata, extract the MonadSpec
3. Register the MonadSpec in the Desugarer's registry

**Step 3: Make monad specs accessible**

Add a method to the Desugarer:
```rust
pub fn get_monad_spec(&self, bracket_name: &str) -> Option<&MonadSpec> {
    self.monad_specs.get(&bracket_name.to_string())
}
```

**Step 4: Run tests**

Run: `cargo test --lib`
Expected: PASS

**Step 5: Commit**

```bash
git add src/core/desugar/rowan_ast.rs
git commit -m "feat: register monad specs from bracket pair definitions"
```

---

### Task 5: Monadic Block Desugaring — Bind Chain Emission

The core desugaring logic: when a monadic bracket block element
appears in soup, consume the subsequent `.expr` lookup and emit the
bind chain.

**Files:**
- Modify: `src/core/desugar/rowan_ast.rs` (Element desugar,
  desugar_rowan_soup)

**Step 1: Handle BracketBlock in Element desugaring**

When desugaring `Element::BracketBlock`, we need to produce the bind
chain. However, the return expression (`.expr`) comes from subsequent
soup elements. We have two options:

**Option A**: Handle in `desugar_rowan_soup` (recommended). When the
soup desugarer encounters a BracketBlock element, it peeks ahead for
the dot + return expression, consumes them, and emits the complete
bind chain.

**Option B**: Desugar BracketBlock to a Let like a normal block, then
transform later.

Use Option A — it's cleaner and avoids transforming an already-built
Let expression.

In `desugar_rowan_soup`, change the element processing loop from
a simple for-each to an indexed loop so we can consume multiple
elements:

```rust
fn desugar_rowan_soup(
    span: Span,
    elements: Vec<Element>,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    // ... existing code ...

    let mut i = 0;
    while i < elements.len() {
        let element = &elements[i];

        // Check for monadic bracket block
        if let Element::BracketBlock(bb) = element {
            if let Some(spec) = lookup_monad_spec(bb, desugarer)? {
                // Consume dot + return expression
                let return_expr = consume_return_expression(
                    &elements, &mut i, span, desugarer
                )?;

                // Desugar as bind chain
                let bind_chain = desugar_monadic_block(
                    bb, &spec, return_expr, span, desugarer
                )?;

                soup.push(bind_chain);
                i += 1;
                continue;
            }
        }

        // ... existing element processing ...
        i += 1;
    }

    // ... rest of soup finalisation ...
}
```

**Step 2: Implement consume_return_expression**

Look ahead in the elements for a dot operator followed by an
expression. The dot is an OperatorIdentifier with text ".". The
expression after it is the return expression.

```rust
fn consume_return_expression(
    elements: &[Element],
    i: &mut usize,
    span: Span,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    // Next element should be dot operator
    let dot_idx = *i + 1;
    let expr_idx = *i + 2;

    if dot_idx >= elements.len() {
        return Err(CoreError::MonadSpecMissingLookup(
            desugarer.new_smid(span),
        ));
    }

    // Check for dot operator
    let is_dot = elements[dot_idx]
        .as_operator_identifier()
        .map(|op| op.text() == ".")
        .unwrap_or(false);

    if !is_dot {
        return Err(CoreError::MonadSpecMissingLookup(
            desugarer.new_smid(span),
        ));
    }

    if expr_idx >= elements.len() {
        return Err(CoreError::MonadSpecMissingLookup(
            desugarer.new_smid(span),
        ));
    }

    // Desugar the return expression
    let return_expr = elements[expr_idx].desugar(desugarer)?;

    // Advance past dot + expression
    *i = expr_idx;

    Ok(desugarer.varify(return_expr))
}
```

**Step 3: Implement desugar_monadic_block**

Build the nested bind chain from the bracket block's declarations:

```rust
fn desugar_monadic_block(
    bb: &rowan_ast::BracketBlock,
    spec: &MonadSpec,
    return_expr: RcExpr,
    span: Span,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    let smid = desugarer.new_smid(span);

    // Collect declarations in order
    let declarations: Vec<_> = bb.declarations().collect();

    if declarations.is_empty() {
        // No declarations — just return(expr)
        let return_fn = desugarer.varify(core::name(smid, &spec.return_));
        return Ok(RcExpr::from(Expr::App(smid, return_fn, vec![return_expr])));
    }

    // Push declaration names into scope so bodies can reference them
    let keys: Vec<String> = declarations
        .iter()
        .filter_map(|d| extract_declaration_name(d).ok())
        .collect();
    desugarer.env_mut().push_keys(keys);

    // Build bind chain right-to-left
    // Start with the innermost: return(expr) in scope of all bindings
    let return_fn = desugarer.varify(core::name(smid, &spec.return_));
    let mut result = RcExpr::from(Expr::App(smid, return_fn, vec![return_expr]));

    // Process declarations from last to first
    for decl in declarations.iter().rev() {
        let name = extract_declaration_name(decl)?;
        let body = desugar_declaration_body(decl, desugarer)?;

        let fv = desugarer.env().get(&name).cloned()
            .unwrap_or_else(|| moniker::FreeVar::fresh_named(&name));

        // Wrap result in: bind(body, λname. result)
        let lam = core::lam(smid, vec![fv], result);
        let bind_fn = desugarer.varify(core::name(smid, &spec.bind));
        result = RcExpr::from(Expr::App(smid, bind_fn, vec![body, lam]));
    }

    desugarer.env_mut().pop();
    Ok(result)
}
```

Note: `extract_declaration_name` already exists in rowan_ast.rs
(~line 816). `desugar_declaration_body` would need to be factored
out from the existing declaration desugaring code (the body is just
the soup after the colon, desugared normally).

**Step 4: Implement lookup_monad_spec**

```rust
fn lookup_monad_spec(
    bb: &rowan_ast::BracketBlock,
    desugarer: &Desugarer,
) -> Result<Option<MonadSpec>, CoreError> {
    // First check bracket pair definition registry
    if let Some(name) = bb.bracket_name() {
        if let Some(spec) = desugarer.get_monad_spec(&name) {
            return Ok(Some(spec.clone()));
        }
    }

    // Then check inline block metadata (for blocks with ` { :monad ... })
    // (This handles the case where the BracketBlock has inline monad metadata)
    // For now, bracket pair definitions are the primary mechanism.
    Ok(None)
}
```

**Step 5: Run tests**

Run: `cargo test --lib`
Expected: PASS

**Step 6: Commit**

```bash
git add src/core/desugar/rowan_ast.rs
git commit -m "feat: monadic block desugaring — bind chain emission"
```

---

### Task 6: Inline Monadic Block Support

Support monadic blocks specified via inline metadata on regular blocks:
`{ ` { :monad bind: lb return: lr } x: ... }.expr`.

**Files:**
- Modify: `src/core/desugar/rowan_ast.rs` (Block desugaring)

**Step 1: Detect :monad in block metadata during desugaring**

In the `Desugarable for Block` implementation (~line 978), after
extracting block metadata, check for `:monad`:

```rust
// After existing metadata extraction (line 994-998):
let monad_spec = if let Some(ref meta_expr) = metadata {
    extract_monad_spec_from_core_meta(meta_expr)?
} else {
    None
};
```

Where `extract_monad_spec_from_core_meta` inspects the desugared
metadata expression for a block with `:monad` tag and `bind`/`return`
fields.

**Step 2: Emit bind chain instead of Let**

When `monad_spec` is Some:
1. Don't create `Expr::Let(DefaultBlockLet, ...)`
2. Instead, build the bind chain from the bindings
3. The body (return expression) comes from PendingLookup/rebody later

Problem: at this point we don't have the return expression. It comes
from the `.expr` lookup in the enclosing soup. We need a different
strategy for inline monadic blocks.

**Approach: Use a marker LetType**

Add a new `LetType` variant:
```rust
pub enum LetType {
    DefaultBlockLet,
    OtherLet,
    MonadicBlockLet,  // NEW: signals monadic desugaring needed
}
```

When `:monad` is detected, emit:
```rust
Expr::Let(smid, scope, LetType::MonadicBlockLet)
```

And store the monad spec alongside (either in the Expr or as metadata).

Then, during soup desugaring, the PendingLookup state machine
recognises `MonadicBlockLet`:
- `is_default_let()` should return true for `MonadicBlockLet` (so
  PendingLookup::Static is triggered)
- After rebody, the Let has the return expression as its body
- A post-rebody transform converts the MonadicBlockLet into bind chains

**Step 3: Post-rebody monadic transformation**

After the PendingLookup state machine has rebodied the Let:
```
Meta(
  Let(MonadicBlockLet, Scope(Rec(bindings), body=return_expr)),
  monad_metadata
)
```

Transform into:
```
bind(action1, λa. bind(action2, λb. return(return_expr)))
```

This transformation can be done as a walk over the soup elements
after desugar_rowan_soup completes, or as a separate pass.

**Step 4: Handle MonadicBlockLet in rebody**

The `rebody` method (expr.rs:584) needs to work with
`MonadicBlockLet` the same way as `DefaultBlockLet`.

**Step 5: Run tests**

Run: `cargo test --lib`
Expected: PASS

**Step 6: Commit**

```bash
git add src/core/desugar/rowan_ast.rs src/core/expr.rs
git commit -m "feat: inline monadic block support via MonadicBlockLet"
```

---

### Task 7: List Monad Harness Test

Write a comprehensive harness test for the list monad — the simplest
monad to reason about.

**Files:**
- Create: `harness/test/086_monadic_blocks.eu`
- Modify: `tests/harness_test.rs` (register test)

**Step 1: Write list monad test**

```eucalypt
##
## 086: Monadic blocks — bind chain desugaring via monad specification
##

# List monad operations
list-bind(xs, f): xs mapcat(f)
list-return(x): [x]

# Assign bracket pair
⟦{}⟧: { :monad  bind: list-bind  return: list-return }

tests: {

  basic-combinations: {
    ` "Two lists produce all combinations"
    a: ⟦
      x: [1, 2, 3]
      y: [10, 20]
    ⟧.[x, y]
    result: a //= [[1, 10], [1, 20], [2, 10], [2, 20], [3, 10], [3, 20]]
  }

  single-declaration: {
    ` "Single declaration with return"
    a: ⟦
      x: [1, 2, 3]
    ⟧.x
    result: a //= [1, 2, 3]
  }

  dependent-bindings: {
    ` "Later declarations can reference earlier ones"
    a: ⟦
      x: [1, 2]
      y: [x * 10, x * 100]
    ⟧.[x, y]
    result: a //= [[1, 10], [1, 100], [2, 20], [2, 200]]
  }

  computed-return: {
    ` "Return expression can be a computation"
    a: ⟦
      x: [1, 2, 3]
      y: [10, 20]
    ⟧.(x + y)
    result: a //= [11, 21, 12, 22, 13, 23]
  }

  block-return: {
    ` "Return expression can be a block"
    a: ⟦
      x: [1, 2]
      y: [10, 20]
    ⟧.{sum: x + y  product: x * y}
    result: a map(.sum) //= [11, 21, 12, 22]
  }

  filtering: {
    ` "Guard pattern — empty list filters out"
    a: ⟦
      x: [1, 2, 3, 4, 5]
      ok: if(x % 2 = 0, [true], [])
    ⟧.x
    result: a //= [2, 4]
  }

}

RESULT: tests values map(values) map(all-true?) all-true? then(:PASS, :FAIL)
```

**Step 2: Register the test**

Add to `tests/harness_test.rs`:
```rust
harness_test!(test_harness_086, "086_monadic_blocks");
```

**Step 3: Run the test**

Run: `cargo test test_harness_086`
Expected: PASS

**Step 4: Commit**

```bash
git add harness/test/086_monadic_blocks.eu tests/harness_test.rs
git commit -m "test: list monad harness tests for monadic blocks"
```

---

### Task 8: Random Monad Harness Test

Test the random monad — the motivating use case from the design doc.

**Files:**
- Modify: `harness/test/086_monadic_blocks.eu`

**Step 1: Add random monad tests**

```eucalypt
# Random monad operations
rand-bind(action, f): (stream): {
  r: action(stream)
  next: f(r.value)(r.rest)
  value: next.value
  rest: next.rest
}

rand-return(x): (stream): { value: x  rest: stream }

# Assign different bracket pair for random monad
⟨{}⟩: { :monad  bind: rand-bind  return: rand-return }

tests: {
  # ... existing list monad tests ...

  random-monad: {
    ` "Three random draws produce value and rest"
    three-dice: ⟨
      a: random-int(6)
      b: random-int(6)
      c: random-int(6)
    ⟩.[a, b, c]

    result: three-dice(io.random)
    a: result.value length //= 3
    b: result.value map(_ > 0) all-true?
    c: result.value map(_ <= 6) all-true?
  }
}
```

**Step 2: Run the test**

Run: `cargo test test_harness_086`
Expected: PASS

**Step 3: Commit**

```bash
git add harness/test/086_monadic_blocks.eu
git commit -m "test: random monad harness tests"
```

---

### Task 9: Inline Monadic Blocks Test

Test monadic blocks using inline metadata (no bracket pair).

**Files:**
- Modify: `harness/test/086_monadic_blocks.eu`

**Step 1: Add inline metadata tests**

```eucalypt
tests: {
  # ... existing tests ...

  inline-metadata: {
    ` "Inline monad spec via block metadata"
    a: { ` { :monad  bind: list-bind  return: list-return }
      x: [1, 2]
      y: [10, 20]
    }.[x, y]
    result: a //= [[1, 10], [1, 20], [2, 10], [2, 20]]
  }
}
```

**Step 2: Run the test**

Run: `cargo test test_harness_086`
Expected: PASS

**Step 3: Commit**

```bash
git add harness/test/086_monadic_blocks.eu
git commit -m "test: inline monadic blocks with block metadata"
```

---

### Task 10: Error Cases

Test error handling for malformed monad specifications.

**Files:**
- Create: `harness/test/errors/086a_monad_missing_bind.eu`
- Create: `harness/test/errors/086a_monad_missing_bind.expect`
- Create: `harness/test/errors/086b_monad_missing_lookup.eu`
- Create: `harness/test/errors/086b_monad_missing_lookup.expect`
- Modify: `tests/harness_test.rs`

**Step 1: Missing bind error**

`harness/test/errors/086a_monad_missing_bind.eu`:
```eucalypt
# Monad spec without bind
⟦{}⟧: { :monad  return: list-return }

result: ⟦ x: [1, 2] ⟧.x
RESULT: result
```

`harness/test/errors/086a_monad_missing_bind.expect`:
```
exit: 1
stderr: "bind"
```

**Step 2: Missing lookup error**

`harness/test/errors/086b_monad_missing_lookup.eu`:
```eucalypt
list-bind(xs, f): xs mapcat(f)
list-return(x): [x]
⟦{}⟧: { :monad  bind: list-bind  return: list-return }

# Monadic block without return expression
result: ⟦ x: [1, 2] ⟧
RESULT: result
```

`harness/test/errors/086b_monad_missing_lookup.expect`:
```
exit: 1
stderr: "lookup"
```

**Step 3: Register error tests**

Add to `tests/harness_test.rs`.

**Step 4: Run error tests**

Run: `cargo test test_error_086`
Expected: PASS

**Step 5: Commit**

```bash
git add harness/test/errors/086*.eu harness/test/errors/086*.expect tests/harness_test.rs
git commit -m "test: error cases for malformed monad specifications"
```

---

### Task 11: Bracket Pair Scoping

Verify that monadic bracket definitions follow operator scoping
rules — defined in block scope, visible in nested scopes, overridable.

**Files:**
- Modify: `harness/test/086_monadic_blocks.eu`

**Step 1: Add scoping tests**

```eucalypt
tests: {
  # ... existing tests ...

  scoping: {
    ` "Monadic bracket defined in inner scope"
    a: {
      ⟦{}⟧: { :monad  bind: list-bind  return: list-return }
      result: ⟦ x: [1, 2]  y: [10, 20] ⟧.(x + y)
    }.result
    result: a //= [11, 21, 12, 22]
  }

  different-monads: {
    ` "Different bracket pairs for different monads"
    combos: ⟦
      x: [1, 2]
      y: [10, 20]
    ⟧.[x, y]

    dice: ⟨
      a: random-int(6)
    ⟩.a

    a: combos length //= 4
    b: dice(io.random).value > 0
  }
}
```

**Step 2: Run tests**

Run: `cargo test test_harness_086`
Expected: PASS

**Step 3: Commit**

```bash
git add harness/test/086_monadic_blocks.eu
git commit -m "test: bracket pair scoping for monadic blocks"
```

---

## Task Dependency Graph

```
                   Idiot Brackets (prerequisite)
                            ↓
Task 1 (block-mode parsing) ─── Task 2 (validation)
            ↓
Task 3 (monad spec extraction)
            ↓
Task 4 (bracket pair definition registry)
            ↓
Task 5 (bind chain desugaring) ←── core implementation
            ↓
Task 6 (inline monadic blocks) ←── secondary mechanism
            ↓
Tasks 7-11 (tests)
```

Tasks 1-5 are sequential (each depends on the previous).
Task 6 is independent of tests but depends on Task 5.
Tasks 7-11 are additive and can be combined.

---

## Notes

- **Sequential scoping**: Monadic bindings are sequential (each sees
  earlier ones). This is naturally expressed by the nested lambda
  structure. No `Rec` (mutual recursion) needed.

- **Generalised lookup forms**: The return expression supports all
  lookup syntaxes: `.name`, `.[list]`, `.(expr)`, `.{block}`. For
  bracket-pair monadic blocks (Task 5), the return expression is
  consumed directly from the soup. For inline monadic blocks (Task 6),
  it goes through the PendingLookup state machine.

- **Static constraint**: The monad spec's `bind` and `return` values
  must be simple names extractable at desugar time. Computed or
  conditional monad specs are not supported. The extracted names are
  emitted as unresolved `Name` references and resolved during cook.

- **Content-based disambiguation**: The parser uses top-level colons
  to detect block mode. This is pragmatic — monadic brackets always
  contain declarations (with colons), idiot brackets never do. The
  `{}` parameter convention in the bracket definition provides
  semantic verification during desugaring.

- **No new core-to-core pass**: The monadic desugaring emits standard
  core expressions (applications, lambdas, names). The cook phase
  and downstream passes handle them normally.
