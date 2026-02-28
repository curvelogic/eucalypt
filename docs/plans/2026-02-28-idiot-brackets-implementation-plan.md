# Idiot Brackets Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add user-defined Unicode bracket pairs that collect space-separated contents into a list and pass it to a bracket function.

**Architecture:** The lexer already emits `RESERVED_OPEN`/`RESERVED_CLOSE` for non-ASCII Unicode brackets. We add a `BRACKET_EXPR` AST node, extend declaration classification to recognise bracket pair definitions, and desugar bracket usage by splitting soup at catenation boundaries into list items applied to the bracket function. No cook/shunt changes needed — each sub-soup is resolved independently by the existing pipeline.

**Tech Stack:** Rust, Rowan parser, LALRPOP-free (hand-written lexer/parser)

**Design doc:** `docs/plans/2026-02-28-idiot-brackets-design.md`

---

## Key Concept: Catenation Boundary Splitting

Inside idiot brackets, top-level catenation (spaces between value-like
elements) becomes a list separator instead of pipeline application.
Operators bind tighter than catenation, so they keep their operands
together.

Example: `«a + 1  b * 2»`

AST elements: `[Name(a), Name(+), Literal(1), Name(b), Name(*), Literal(2)]`

Classification (AST level):
- `OperatorIdentifier` → OpLike on both sides (binary assumption)
- Everything else → ValueLike on both sides
- `ApplyTuple` → OpLike on left, ValueLike on right (call-like)

Boundary detection (mirrors `fill.rs:bind_sides` logic):
- Between elements where right-side of left = ValueLike AND left-side of right = ValueLike → **catenation boundary** = list separator

Result: two sub-soups `[a, +, 1]` and `[b, *, 2]`, each cooked
independently to `(a + 1)` and `(b * 2)`, then collected as
`App(Name("«»"), [List([(a+1), (b*2)])])`.

---

### Task 1: Bracket Pair Lookup Table

Add a utility module mapping Unicode opening brackets to their closing
counterparts, used by the parser to verify matching pairs.

**Files:**
- Create: `src/syntax/rowan/brackets.rs`
- Modify: `src/syntax/rowan/mod.rs` (add module declaration)

**Step 1: Create the bracket pair module**

```rust
// src/syntax/rowan/brackets.rs
//! Unicode bracket pair mapping for user-defined bracket pairs.

/// Return the matching closing bracket for an opening bracket character,
/// or None if the character is not a recognised opening bracket.
pub fn closing_bracket(open: char) -> Option<char> {
    match open {
        '«' => Some('»'),     // Guillemets
        '‹' => Some('›'),     // Single guillemets
        '⟦' => Some('⟧'),     // Mathematical white square bracket
        '⟨' => Some('⟩'),     // Mathematical angle bracket
        '⟪' => Some('⟫'),     // Mathematical double angle bracket
        '⌈' => Some('⌉'),     // Ceiling
        '⌊' => Some('⌋'),     // Floor
        '「' => Some('」'),    // CJK corner bracket
        '『' => Some('』'),    // CJK white corner bracket
        '【' => Some('】'),    // CJK lenticular bracket
        '〈' => Some('〉'),    // CJK angle bracket
        '《' => Some('》'),    // CJK double angle bracket
        '〔' => Some('〕'),    // CJK tortoise shell bracket
        '〖' => Some('〗'),    // CJK white lenticular bracket
        '〘' => Some('〙'),    // CJK white tortoise shell bracket
        '〚' => Some('〛'),    // CJK white square bracket
        '❨' => Some('❩'),     // Ornament parenthesis
        '❪' => Some('❫'),     // Ornament parenthesis
        '❬' => Some('❭'),     // Ornament angle bracket
        '❮' => Some('❯'),     // Ornament angle bracket
        '❰' => Some('❱'),     // Ornament angle bracket
        '❲' => Some('❳'),     // Ornament bracket
        '❴' => Some('❵'),     // Ornament brace
        '⦃' => Some('⦄'),     // White curly bracket
        '⦅' => Some('⦆'),     // White parenthesis
        '⦇' => Some('⦈'),     // Z notation image bracket
        '⦉' => Some('⦊'),     // Z notation anti-image bracket
        _ => None,
    }
}

/// Return the bracket pair name (open + close concatenated) for a given
/// opening bracket character, or None if not recognised.
pub fn bracket_pair_name(open: char) -> Option<String> {
    closing_bracket(open).map(|close| format!("{open}{close}"))
}

/// Return true if the character is a language-reserved bracket that
/// cannot be used as a user-defined bracket pair.
pub fn is_language_bracket(c: char) -> bool {
    matches!(c, '(' | ')' | '[' | ']' | '{' | '}')
}
```

**Step 2: Register the module**

In `src/syntax/rowan/mod.rs`, add:
```rust
pub mod brackets;
```

**Step 3: Run tests**

Run: `cargo test --lib`
Expected: PASS (no existing tests break)

**Step 4: Commit**

```bash
git add src/syntax/rowan/brackets.rs src/syntax/rowan/mod.rs
git commit -m "feat: add Unicode bracket pair lookup table"
```

---

### Task 2: New SyntaxKind and AST Node

Add `BRACKET_EXPR` to the syntax kind enum and create a `BracketExpr`
AST node that preserves the opening and closing bracket tokens.

**Files:**
- Modify: `src/syntax/rowan/kind.rs` (add BRACKET_EXPR)
- Modify: `src/syntax/rowan/ast.rs` (add BracketExpr, extend Element)

**Step 1: Add BRACKET_EXPR to SyntaxKind**

In `src/syntax/rowan/kind.rs`, add after `ARG_TUPLE`:
```rust
    /// User-defined bracket expression e.g. «a b c»
    BRACKET_EXPR,
```

Update `is_callable_terminal` to include `CLOSE_SQUARE` check — actually,
first verify whether `RESERVED_CLOSE` should be callable. A bracket
expression like `«x»` followed by `(y)` should probably parse as
apply — so add `RESERVED_CLOSE` to `is_callable_terminal`:

```rust
    pub fn is_callable_terminal(&self) -> bool {
        *self == CLOSE_PAREN
            || *self == CLOSE_BRACE
            || *self == RESERVED_CLOSE
            || *self == UNQUOTED_IDENTIFIER
            || *self == STRING_PATTERN_END
            || *self == C_STRING_PATTERN_END
            || *self == RAW_STRING_PATTERN_END
    }
```

Also update `from_raw` assertion bound if needed (the transmute range
must cover the new variant).

**Step 2: Add BracketExpr AST node**

In `src/syntax/rowan/ast.rs`, after the `ParenExpr` definition
(~line 519), add:

```rust
// A user-defined bracket expression e.g. «a b c»
//
// AST embedding syntax:
// - `[:a-bracket-expr soup]` - Expression enclosed in user-defined brackets
ast_node!(BracketExpr, BRACKET_EXPR);

impl BracketExpr {
    pub fn open_bracket(&self) -> Option<SyntaxToken> {
        support::syntax_token(self.syntax(), SyntaxKind::RESERVED_OPEN)
    }

    pub fn close_bracket(&self) -> Option<SyntaxToken> {
        support::syntax_token(self.syntax(), SyntaxKind::RESERVED_CLOSE)
    }

    /// The opening bracket character
    pub fn open_char(&self) -> Option<char> {
        self.open_bracket()
            .and_then(|t| t.text().chars().next())
    }

    /// The bracket pair name (open + close characters concatenated)
    pub fn bracket_name(&self) -> Option<String> {
        self.open_char()
            .and_then(crate::syntax::rowan::brackets::bracket_pair_name)
    }
}

impl HasSoup for BracketExpr {}
```

**Step 3: Add BracketExpr to the Element enum**

In the `Element` enum (~line 1040), add a new variant:

```rust
    /// User-defined bracket expression - Embedding: `[:a-bracket-expr soup]`
    BracketExpr(BracketExpr),
```

Update `Element::can_cast` to include `SyntaxKind::BRACKET_EXPR`.

Update `Element::cast` to handle `BRACKET_EXPR`:
```rust
SyntaxKind::BRACKET_EXPR => {
    BracketExpr::cast(syntax).map(Element::BracketExpr)
}
```

Update `Element::syntax` match for the new variant.

**Step 4: Run tests**

Run: `cargo test --lib`
Expected: PASS

**Step 5: Commit**

```bash
git add src/syntax/rowan/kind.rs src/syntax/rowan/ast.rs
git commit -m "feat: add BRACKET_EXPR syntax kind and BracketExpr AST node"
```

---

### Task 3: Parser Changes

Modify the parser to emit `BRACKET_EXPR` instead of `PAREN_EXPR` for
Unicode bracket expressions, and verify that the closing bracket
matches the opening one.

**Files:**
- Modify: `src/syntax/rowan/parse.rs` (~lines 326-334)

**Step 1: Update parse_reserved_paren_expression**

Replace the current `parse_reserved_paren_expression` method:

```rust
    /// Parse a user-defined bracket expression: «a b c»
    ///
    /// Contents are parsed as soup (same as parenthesised expressions).
    /// The bracket pair matching is verified by checking the actual
    /// characters against the bracket pair lookup table.
    fn parse_bracket_expression(&mut self) {
        self.sink().start_node(BRACKET_EXPR);
        // Consume and record the opening bracket
        if let Some((_, span)) = self.next() {
            self.sink().token(RESERVED_OPEN);
        }
        self.add_trivia();

        self.parse_soup();

        // Expect matching closing bracket
        self.expect(RESERVED_CLOSE);
        self.sink().finish_node();
    }
```

Update the dispatch in `try_parse_element` (~line 236):
```rust
Some((RESERVED_OPEN, _)) => {
    self.parse_bracket_expression();
    true
}
```

**Step 2: Run parser tests**

Run: `cargo test --lib -- test_exotic_quotes`

This test currently expects errors for `« a b c »`. It will need
updating — the parse should now succeed (but validation may still
flag undefined brackets).

**Step 3: Update the exotic quotes test**

The test in `src/syntax/rowan/validate.rs` (~line 480) asserts errors
for `« a b c »`. Update it to expect successful parsing into a
BRACKET_EXPR node. The validation error changes from `InvalidParenExpr`
+ `ReservedCharacter` to... nothing (if validation accepts bracket
exprs) or a different error (if we validate bracket definitions at
a later stage).

For now, remove the `ReservedCharacter` errors from the expected
output and verify the AST structure is BRACKET_EXPR.

**Step 4: Run all tests**

Run: `cargo test --lib`
Expected: PASS

**Step 5: Commit**

```bash
git add src/syntax/rowan/parse.rs src/syntax/rowan/validate.rs
git commit -m "feat: parse Unicode brackets as BRACKET_EXPR nodes"
```

---

### Task 4: Validation Changes

Stop treating `RESERVED_OPEN`/`RESERVED_CLOSE` as errors when they
appear inside `BRACKET_EXPR` nodes.

**Files:**
- Modify: `src/syntax/rowan/validate.rs` (~lines 23-37, 191-209)

**Step 1: Update no_error_children**

The `no_error_children` function flags `RESERVED_OPEN` and
`RESERVED_CLOSE` tokens as `ReservedCharacter` errors. This must
be conditional — these tokens are valid inside `BRACKET_EXPR` nodes
but invalid elsewhere.

Option A: Don't flag them in `no_error_children` at all (they're now
valid tokens in some contexts). Instead, check for orphaned reserved
tokens at the Unit/Block level.

Option B: Add a `BracketExpr` validation that doesn't call
`no_error_children` (or calls a variant that accepts bracket tokens).

Recommended: Option A — remove `RESERVED_OPEN`/`RESERVED_CLOSE` from
`no_error_children`. Add a separate check for orphaned reserved
brackets (RESERVED_OPEN/CLOSE that appear outside of BRACKET_EXPR
nodes) if needed.

**Step 2: Add BracketExpr validation**

```rust
impl Validatable for BracketExpr {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        // Verify matching bracket pair
        if let (Some(open), Some(close)) = (self.open_bracket(), self.close_bracket()) {
            let open_char = open.text().chars().next();
            let close_char = close.text().chars().next();
            if let Some(oc) = open_char {
                let expected_close = crate::syntax::rowan::brackets::closing_bracket(oc);
                if expected_close != close_char {
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

        if let Some(s) = self.soup() {
            s.validate(errors);
        }
    }
}
```

**Step 3: Add new ParseError variants**

In the ParseError enum (find in `src/syntax/rowan/mod.rs` or
`src/syntax/rowan/error.rs`), add:

```rust
MismatchedBrackets {
    open_range: TextRange,
    close_range: TextRange,
},
UnterminatedBracketExpr {
    open_range: Option<TextRange>,
    range: TextRange,
},
```

**Step 4: Update Element validation**

Add `BracketExpr` to the `Element::validate` match:
```rust
Element::BracketExpr(be) => be.validate(errors),
```

**Step 5: Run tests**

Run: `cargo test --lib`
Expected: PASS

**Step 6: Commit**

```bash
git add src/syntax/rowan/validate.rs src/syntax/rowan/mod.rs
git commit -m "feat: validate bracket expressions with pair matching"
```

---

### Task 5: Bracket Pair Declaration Classification

Extend `classify_declaration` to recognise bracket pair definitions
in declaration heads: `«xs»: body`.

**Files:**
- Modify: `src/syntax/rowan/ast.rs` (~lines 559-735)

**Step 1: Add BracketPair to DeclarationKind**

In the `DeclarationKind` enum (~line 559):

```rust
    /// Bracket pair declaration (e.g. «xs»: ...) - Embedding: `[:a-decl-bracket name param]`
    BracketPair(BracketExpr),
```

**Step 2: Handle BracketExpr in classify_declaration**

In `classify_declaration` (~line 680), in the `1 =>` match arm, add
a case before the `malformed()` fallback:

```rust
    } else if let Some(be) = BracketExpr::cast(items[0].clone()) {
        DeclarationKind::BracketPair(be)
```

This goes after the `ParenExpr::cast` → `classify_operator` branch
and before the `malformed()` fallback.

**Step 3: Run tests**

Run: `cargo test --lib`
Expected: PASS

**Step 4: Commit**

```bash
git add src/syntax/rowan/ast.rs
git commit -m "feat: classify bracket pair declarations in declaration heads"
```

---

### Task 6: Desugar Bracket Pair Declarations

Handle `BracketPair` declarations in the desugarer so that `«xs»: body`
produces a function binding with the bracket pair name as the key.

**Files:**
- Modify: `src/core/desugar/rowan_ast.rs` (~lines 295-436)

**Step 1: Handle BracketPair in extract_rowan_declaration_components**

In `extract_rowan_declaration_components` (or the equivalent match on
`DeclarationKind`), add handling for the new variant.

The bracket pair declaration `«xs»: body` should produce:
- Declaration name: `"«»"` (the bracket pair name)
- Parameters: whatever is inside the brackets (e.g., `xs` as a single
  parameter name)
- Body: desugared normally
- is_operator: false (bracket functions are not operators)
- Fixity: None

The parameter extraction depends on the bracket contents:
- Single name `«xs»:` → one parameter named `xs`
- For now, only single-name parameters are supported (destructuring
  patterns `«[x : xs]»:` will be supported when the destructuring
  feature is implemented)

```rust
DeclarationKind::BracketPair(be) => {
    let name = be.bracket_name().ok_or_else(|| {
        CoreError::InvalidBracketPair(/* smid */)
    })?;

    // Extract parameter from bracket contents
    let param = be.soup().and_then(|s| s.singleton())
        .and_then(|e| e.as_normal_identifier())
        .map(|id| id.text().to_string())
        .ok_or_else(|| {
            CoreError::InvalidBracketParameter(/* smid */)
        })?;

    // Build as a function declaration: name(param): body
    RowanDeclarationComponents {
        name,
        params: vec![param],
        is_operator: false,
        fixity: None,
        // ... other fields
    }
}
```

Note: `CoreError` will need new variants for bracket-related errors.
Add `InvalidBracketPair` and `InvalidBracketParameter` to
`src/core/error.rs`.

**Step 2: Run tests**

Run: `cargo test --lib`
Expected: PASS

**Step 3: Commit**

```bash
git add src/core/desugar/rowan_ast.rs src/core/error.rs
git commit -m "feat: desugar bracket pair declarations as named functions"
```

---

### Task 7: Desugar Bracket Expression Usage

When a bracket expression is used (e.g., `«a + 1  b * 2»`), split the
soup at catenation boundaries and emit
`App(Name("«»"), [List([items])])`.

**Files:**
- Modify: `src/core/desugar/rowan_ast.rs` (Element desugar impl, around the
  Element match arms)

**Step 1: Add AST-level catenation boundary detection**

Add a helper function for classifying soup elements at the AST level:

```rust
/// Classify whether an AST element behaves as operator-like or
/// value-like for the purpose of detecting catenation boundaries.
///
/// Returns (left_side, right_side) where true = value-like.
fn ast_value_sides(element: &rowan_ast::Element) -> (bool, bool) {
    match element {
        rowan_ast::Element::Name(n) => {
            // Operator identifiers are op-like on both sides
            if n.identifier()
                .map(|id| matches!(id, rowan_ast::Identifier::OperatorIdentifier(_)))
                .unwrap_or(false)
            {
                (false, false)
            } else {
                (true, true)
            }
        }
        rowan_ast::Element::ApplyTuple(_) => (false, true), // call-like
        _ => (true, true), // everything else is value-like
    }
}

/// Split a sequence of AST elements at catenation boundaries.
///
/// A catenation boundary occurs between two adjacent value-like
/// elements — exactly where the cook phase would insert a cat
/// operator. Inside bracket expressions, these become list item
/// separators instead.
fn split_at_catenation_boundaries(
    elements: Vec<rowan_ast::Element>,
) -> Vec<Vec<rowan_ast::Element>> {
    if elements.is_empty() {
        return vec![];
    }

    let mut groups: Vec<Vec<rowan_ast::Element>> = vec![vec![]];

    for (i, element) in elements.iter().enumerate() {
        if i > 0 {
            let prev_right = ast_value_sides(&elements[i - 1]).1;
            let curr_left = ast_value_sides(element).0;
            if prev_right && curr_left {
                // Catenation boundary — start new group
                groups.push(vec![]);
            }
        }
        groups.last_mut().unwrap().push(element.clone());
    }

    groups
}
```

**Step 2: Desugar BracketExpr in Element impl**

In the `Desugarable` impl for `Element` (or wherever elements are
matched for desugaring), add a case for `BracketExpr`:

```rust
Element::BracketExpr(be) => {
    let span = text_range_to_span(be.syntax().text_range());
    let smid = desugarer.new_smid(span);

    // Get the bracket function name
    let name = be.bracket_name().ok_or_else(|| {
        CoreError::InvalidBracketPair(smid)
    })?;

    // Get soup elements
    let elements: Vec<_> = be.soup()
        .map(|s| s.elements().collect())
        .unwrap_or_default();

    // Split at catenation boundaries
    let groups = split_at_catenation_boundaries(elements);

    // Desugar each group as an independent sub-soup
    let items: Vec<RcExpr> = groups
        .into_iter()
        .map(|group| {
            if group.len() == 1 {
                let expr = group[0].desugar(desugarer)?;
                Ok(desugarer.varify(expr))
            } else {
                desugar_rowan_soup(span, group, desugarer)
            }
        })
        .collect::<Result<_, _>>()?;

    // Build: App(Name("«»"), [List([items])])
    let list = RcExpr::from(Expr::List(smid, items));
    let bracket_fn = core::name(smid, &name);
    Ok(RcExpr::from(Expr::Soup(
        smid,
        vec![list, core::cat(), desugarer.varify(bracket_fn)],
    )))
}
```

Wait — the bracket function needs to be applied to the list. This
should be `App(bracket_fn, [list])`. But at desugar time we don't
have cooked expressions yet. We can emit this as a Soup that the
cook phase will process: `[list, cat, bracket_fn]` which cook turns
into `bracket_fn(list)`. Or we can emit it directly as an App if
the function name is known.

Actually, the cleanest approach is to emit it as Soup so the cook
phase handles the application naturally:

```rust
    // Emit as soup: bracket_fn list
    // Cook phase will gap-fill with cat and shunt to App(bracket_fn, [list])
    let bracket_fn = desugarer.varify(core::name(smid, &name));
    Ok(RcExpr::from(Expr::Soup(smid, vec![list, bracket_fn])))
```

Or more directly:
```rust
    // Emit direct application
    let bracket_fn = desugarer.varify(core::name(smid, &name));
    Ok(RcExpr::from(Expr::App(smid, bracket_fn, vec![list])))
```

The direct `App` is simpler and avoids cook-phase indirection.
Use `App`.

**Step 3: Write a basic harness test**

Create `harness/test/085_idiot_brackets.eu`:

```eucalypt
##
## 085: Idiot brackets — user-defined bracket pair expressions
##

# Simple bracket function that receives a list
«xs»: xs

tests: {
  basic-collection: {
    ` "Bracket collects items into a list"
    a: «1 2 3» //= [1, 2, 3]
  }

  operators-inside: {
    ` "Operators bind within items"
    a: «1 + 2  3 * 4» //= [3, 12]
  }

  single-item: {
    ` "Single item still wrapped in list"
    a: «42» //= [42]
  }
}

RESULT: tests values map(values) map(all-true?) all-true? then(:PASS, :FAIL)
```

Register the test in `tests/harness_test.rs`.

**Step 4: Run the test**

Run: `cargo test test_harness_085`
Expected: PASS

**Step 5: Commit**

```bash
git add src/core/desugar/rowan_ast.rs harness/test/085_idiot_brackets.eu tests/harness_test.rs
git commit -m "feat: desugar bracket expressions — split soup and apply bracket function"
```

---

### Task 8: Idiom Bracket Example

Add a more realistic test demonstrating idiom brackets with a fold
pattern.

**Files:**
- Modify: `harness/test/085_idiot_brackets.eu`

**Step 1: Extend the test file**

Add to the existing test file:

```eucalypt
# Bracket function that folds — simple sum
⟨xs⟩: xs foldl1(+)

# Bracket function for product
⟦xs⟧: xs foldl1(*)

tests: {
  # ... existing tests ...

  fold-brackets: {
    ` "Sum bracket"
    a: ⟨1 2 3 4⟩ //= 10

    ` "Product bracket"
    b: ⟦2 3 4⟧ //= 24
  }

  parens-protect: {
    ` "Parentheses protect inner catenation"
    a: «1 (2 str) 3» //= [1, "2", 3]
  }

  nested-brackets: {
    ` "Nested same-type brackets"
    a: «1 «2 3» 4» //= [1, [2, 3], 4]
  }

  different-brackets: {
    ` "Nested different-type brackets"
    a: «⟨1 2 3⟩ ⟨4 5 6⟩» //= [6, 15]
  }

  multiple-bracket-types: {
    ` "Multiple bracket types in same scope"
    a: ⟨⟨1 2⟩ ⟨3 4⟩⟩ //= 10
  }
}
```

Note: some of these tests may need adjustment based on how deeply
nested bracket resolution works. The key thing is that inner bracket
expressions evaluate first and their results become items in the
outer bracket's list.

**Step 2: Run the test**

Run: `cargo test test_harness_085`
Expected: PASS

**Step 3: Commit**

```bash
git add harness/test/085_idiot_brackets.eu
git commit -m "test: idiom bracket examples with folds, nesting, multiple types"
```

---

### Task 9: Bracket Scoping

Verify that bracket functions follow operator scoping rules — defined
in a block scope, visible in that scope and nested scopes, overridable
in inner scopes.

**Files:**
- Modify: `harness/test/085_idiot_brackets.eu`

**Step 1: Add scoping tests**

```eucalypt
tests: {
  # ... existing tests ...

  scoping: {
    ` "Bracket defined in inner scope"
    a: {
      «xs»: xs foldl1(+)
      result: «10 20 30»
    }.result //= 60

    ` "Override in inner scope"
    b: {
      «xs»: xs foldl1(+)
      inner: {
        «xs»: xs foldl1(*)
        result: «2 3 4»
      }.result
    }.inner //= 24
  }
}
```

**Step 2: Run the test**

Run: `cargo test test_harness_085`
Expected: PASS

**Step 3: Commit**

```bash
git add harness/test/085_idiot_brackets.eu
git commit -m "test: bracket function scoping and override"
```

---

### Task 10: Error Cases

Add harness error tests for bracket-related error conditions.

**Files:**
- Create: `harness/test/errors/085a_undefined_bracket.eu`
- Create: `harness/test/errors/085a_undefined_bracket.expect`
- Create: `harness/test/errors/085b_mismatched_bracket.eu`
- Create: `harness/test/errors/085b_mismatched_bracket.expect`
- Modify: `tests/harness_test.rs` (register error tests)

**Step 1: Undefined bracket error test**

`harness/test/errors/085a_undefined_bracket.eu`:
```eucalypt
# Using brackets without defining the bracket function
result: «1 2 3»
RESULT: result
```

`harness/test/errors/085a_undefined_bracket.expect`:
```
exit: 1
stderr: "«»"
```

(The exact error message will depend on how undefined names are
reported — likely during cook/verify phase as an unresolved name.)

**Step 2: Mismatched bracket error test**

`harness/test/errors/085b_mismatched_bracket.eu`:
```eucalypt
# Mismatched opening and closing brackets
result: «1 2 3⟩
RESULT: result
```

`harness/test/errors/085b_mismatched_bracket.expect`:
```
exit: 1
stderr: "mismatched"
```

**Step 3: Register error tests in harness**

Add test functions in `tests/harness_test.rs` following the pattern
of existing error tests.

**Step 4: Run error tests**

Run: `cargo test test_error_085`
Expected: PASS

**Step 5: Commit**

```bash
git add harness/test/errors/085a_undefined_bracket.eu \
        harness/test/errors/085a_undefined_bracket.expect \
        harness/test/errors/085b_mismatched_bracket.eu \
        harness/test/errors/085b_mismatched_bracket.expect \
        tests/harness_test.rs
git commit -m "test: error cases for undefined and mismatched brackets"
```

---

### Task 11: Empty Brackets and Edge Cases

Handle edge cases: empty brackets, single-element brackets, brackets
with only operators.

**Files:**
- Modify: `harness/test/085_idiot_brackets.eu`

**Step 1: Add edge case tests**

```eucalypt
tests: {
  # ... existing tests ...

  edge-cases: {
    ` "Empty brackets — function receives empty list"
    «xs»: xs length
    a: «» //= 0

    ` "Bracket with block literal"
    b: «{x: 1} {y: 2}» //= [{x: 1}, {y: 2}]

    ` "Bracket with list literals"
    c: «[1, 2] [3, 4]» //= [[1, 2], [3, 4]]
  }
}
```

**Step 2: Run tests**

Run: `cargo test test_harness_085`
Expected: PASS

**Step 3: Commit**

```bash
git add harness/test/085_idiot_brackets.eu
git commit -m "test: edge cases for empty brackets and structured items"
```

---

## Task Dependency Graph

```
Task 1 (bracket table)
  ↓
Task 2 (SyntaxKind + AST) ← depends on Task 1
  ↓
Task 3 (parser) ← depends on Task 2
  ↓
Task 4 (validation) ← depends on Task 2, Task 3
  ↓
Task 5 (declaration classification) ← depends on Task 2
  ↓
Task 6 (declaration desugar) ← depends on Task 5
  ↓
Task 7 (usage desugar) ← depends on Task 2, Task 6
  ↓
Tasks 8-11 (tests) ← depend on Task 7
```

Tasks 1-4 (infrastructure) are sequential.
Tasks 5-6 (declaration support) can proceed in parallel with Task 4.
Task 7 depends on both streams converging.
Tasks 8-11 (tests) are additive and can be combined if desired.

## Notes

- **No cook/shunt changes**: Each sub-soup inside a bracket expression
  is processed by the existing cook pipeline independently. The bracket
  function name is resolved like any other name during fixity
  distribution.

- **Destructuring deferred**: The design shows `«[x : xs]»:` with list
  destructuring. This depends on the destructuring feature (eu-spqy).
  For now, plain parameter names work: `«xs»: body`.

- **Fusion deferred**: The design mentions eliding list construction
  via destructure fusion. This also depends on eu-spqy. For now, the
  list is constructed and passed normally.

- **Monadic bracket disambiguation**: The design's `{}` parameter
  convention for monadic brackets (eu-1x9a) doesn't affect this
  implementation. Currently all user brackets are expression-mode
  (idiot brackets). The `{}` parameter recognition will be added
  when monadic blocks are implemented.
