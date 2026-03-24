# Markdown Formatting in Docstrings

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Highlight markdown syntax (`` `code` ``, `*italic*`, `**bold**`) within eucalypt docstrings in eucalypt-mode for Emacs.

**Bead:** eu-9fph

**Strategy — Halfway House:**
1. **Tree-sitter grammar** identifies which strings are docstrings (structural question)
2. **Emacs font-lock** applies markdown regex highlighting within those docstring regions (formatting question)

This avoids both extremes: trying to parse markdown in the tree-sitter grammar (too complex) and trying to identify docstrings with Emacs regexes (unreliable).

**Tech Stack:** JavaScript (tree-sitter grammar), Emacs Lisp (eucalypt-mode), tree-sitter queries

---

## What is a Docstring?

Eucalypt has three kinds of metadata:

- **Declaration metadata** — backtick-prefixed (`` ` "doc" x: 1 ``)
- **Block metadata** — a string or block at the start of a block literal (`{ "doc" x: 1 }`)
- **Unit metadata** — a special case of block metadata at the file level (string or block before any declarations)

A string is a docstring if it appears in any of these positions:

1. **String as unit metadata** — a string literal at file level before declarations
2. **String as block metadata** — a string literal at the start of a block literal before declarations (e.g. `{ "this is a docstring" x: 1 y: 2 }`)
3. **String as declaration metadata** — a string literal after backtick (`` ` "doc text" ``)
4. **`doc:` value in block-valued unit metadata** — the value of a `doc:` key in a block used as unit metadata
5. **`doc:` value in block-valued block metadata** — the value of a `doc:` key in a block at the start of a block literal
6. **`doc:` value in block-valued declaration metadata** — the value of a `doc:` key in a block used as declaration metadata (`` ` {doc: "doc text", ...} ``)

All three string types (plain `"..."`, c-string `c"..."`, raw-string `r"..."`) can be docstrings.

---

## Chunk 1: Tree-sitter Grammar — Tag Docstrings

### Task 1: Introduce `docstring` node via alias

**Files:**
- Modify: `editors/tree-sitter-eucalypt/grammar.js`

- [ ] **Step 1: Alias strings in declaration metadata positions to `docstring`**

In the `_meta_value` rule (line 90), alias string alternatives:

```javascript
_meta_value: $ => choice(
  alias($.string, $.docstring),
  alias($.c_string, $.docstring),
  alias($.r_string, $.docstring),
  $.symbol,
  $.block,
),
```

This makes any string in declaration metadata position appear as a `docstring` node in the AST. The internal structure (string_content, interpolation, etc.) is preserved — `alias` only changes the node name.

- [ ] **Step 2: Add block metadata to the `block` rule**

The grammar currently has no concept of block metadata — a string at the start of a block literal. Add it:

```javascript
block: $ => seq(
  '{',
  optional($.block_metadata),
  repeat(seq(
    $.declaration,
    optional(','),
  )),
  '}',
),

block_metadata: $ => choice(
  alias($.string, $.docstring),
  alias($.c_string, $.docstring),
  alias($.r_string, $.docstring),
  $.symbol,      // keyword tag (e.g. { :tag x: 1 })
  $.block,       // block metadata (e.g. { {doc: "text"} x: 1 })
  $.list,        // list metadata (e.g. { [:x, :y] x: 1 })
  $.literal,     // any literal (e.g. { 22 x: 1 })
),
```

Any literal value is valid as block metadata. Only the string alternatives get aliased to `docstring`. The full language allows arbitrary expression soups as block metadata, but that requires external scanner support to resolve the ambiguity with declarations — a known limitation of the tree-sitter grammar, not worth the complexity for highlighting.

**Note:** The parser can distinguish block metadata from declarations because metadata values are not followed by `:`. A `literal`, `list`, `block`, or `symbol` at the start of a block that isn't followed by `:` is unambiguously metadata. An `identifier` would be ambiguous (could be metadata or the start of a declaration), so identifiers and general expressions are not included — this is the known limitation.

- [ ] **Step 3: Redefine `unit_metadata` in terms of `block_metadata`**

Unit metadata is just block metadata at the file level. For consistency:

```javascript
unit_metadata: $ => $.block_metadata,
```

Or, if we want to keep unit_metadata as a distinct node for different highlighting:

```javascript
unit_metadata: $ => choice(
  alias($.string, $.docstring),
  alias($.c_string, $.docstring),
  alias($.r_string, $.docstring),
  $.block,
),
```

Either way, the string alternatives should alias to `docstring`.

- [ ] **Step 3: Rebuild the grammar**

```bash
cd editors/tree-sitter-eucalypt
npx tree-sitter generate
npx tree-sitter test
```

Verify existing tests pass. The `docstring` node should appear in the parse tree wherever a string was previously used as metadata.

**Verification:** `npx tree-sitter parse` on a file with docstrings shows `(docstring)` nodes.

---

## Chunk 2: Highlights Query — Tag `doc:` in Block Metadata

### Task 2: Query for `doc:` value in block-valued metadata

**Files:**
- Modify: `editors/tree-sitter-eucalypt/queries/highlights.scm`

- [ ] **Step 1: Add highlight query for doc values in block metadata**

```scheme
; Docstrings — string-valued metadata (already tagged as docstring by grammar)
(docstring) @comment.documentation

; Doc values inside block-valued declaration metadata
; Matches: ` {doc: "text", ...}
(metadata
  (block
    (declaration
      (declaration_head
        (identifier) @_key)
      (soup
        [(string) (c_string) (r_string)] @comment.documentation))
    (#eq? @_key "doc")))

; Doc values inside block-valued unit metadata
(unit_metadata
  (block
    (declaration
      (declaration_head
        (identifier) @_key)
      (soup
        [(string) (c_string) (r_string)] @comment.documentation))
    (#eq? @_key "doc")))

; Doc values inside block-valued block metadata
; Matches: { {doc: "text", ...} x: 1 }
(block_metadata
  (block
    (declaration
      (declaration_head
        (identifier) @_key)
      (soup
        [(string) (c_string) (r_string)] @comment.documentation))
    (#eq? @_key "doc")))
```

Note: `@_key` is a capture used only for the predicate (prefixed with `_` to indicate it's not a highlight target).

- [ ] **Step 2: Remove or update the existing `unit_metadata` highlight**

The current rule `(unit_metadata) @comment.documentation` (line 154) highlights the *entire* unit_metadata node. With docstring aliasing, this may conflict or be redundant. Update to only target block-valued unit_metadata:

```scheme
; Block-valued unit metadata (non-docstring)
(unit_metadata (block) @comment.documentation)
```

String-valued unit metadata is now handled by `(docstring) @comment.documentation`.

**Verification:** `npx tree-sitter highlight` on test files shows docstrings correctly tagged.

---

## Chunk 3: Emacs — Markdown Highlighting within Docstrings

### Task 3: Update eucalypt-mode font-lock for docstrings

**Files:**
- Modify: `editors/emacs/eucalypt-mode.el`

- [ ] **Step 1: Add `docstring` feature to tree-sitter font-lock rules**

Add a new font-lock feature that matches `docstring` nodes:

```elisp
:feature 'docstring
'((docstring) @font-lock-doc-face)
```

Add `docstring` to the feature list (probably Layer 1 with `comment` and `string`, or Layer 4 with `metadata`).

- [ ] **Step 2: Update metadata feature rules**

Remove or update the existing `(unit_metadata) @font-lock-doc-face` rule to avoid conflicts with the new `docstring` feature.

- [ ] **Step 3: Add markdown overlay function**

Create a function that applies markdown formatting overlays within docstring regions:

```elisp
(defun eucalypt--fontify-markdown-in-docstrings (start end)
  "Apply markdown highlighting within docstring regions between START and END."
  (save-excursion
    (goto-char start)
    (let ((root (treesit-buffer-root-node)))
      ;; Find all docstring nodes overlapping [start, end]
      (dolist (node (treesit-query-capture root '((docstring) @ds) start end))
        (let ((ds-start (treesit-node-start (cdr node)))
              (ds-end (treesit-node-end (cdr node))))
          (eucalypt--apply-markdown-faces ds-start ds-end))))))

(defun eucalypt--apply-markdown-faces (start end)
  "Apply markdown faces within region START to END."
  ;; Inline code: `code`
  (save-excursion
    (goto-char start)
    (while (re-search-forward "`\\([^`]+\\)`" end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'font-lock-constant-face)))
  ;; Bold: **text** or __text__
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\*\\*\\([^*]+\\)\\*\\*" end t)
      (put-text-property (match-beginning 1) (match-end 1)
                         'face 'bold)))
  ;; Italic: *text* (but not **)
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\(?:^\\|[^*]\\)\\*\\([^*]+\\)\\*\\(?:[^*]\\|$\\)" end t)
      (put-text-property (match-beginning 1) (match-end 1)
                         'face 'italic))))
```

**Important:** The backtick regex for inline code must not conflict with eucalypt's metadata backtick. Inside a string (which docstrings are), backticks are literal characters, not metadata markers. The regex `\`[^\`]+\`` is safe within string content.

- [ ] **Step 4: Register the overlay function**

Hook into Emacs's font-lock mechanism using `treesit-font-lock-rules` custom function or `jit-lock-register`:

```elisp
(jit-lock-register #'eucalypt--fontify-markdown-in-docstrings)
```

This ensures markdown faces are applied and updated as the buffer changes.

- [ ] **Step 5: Handle `doc:` values in block metadata**

For `doc:` values inside block metadata (not aliased to `docstring` in the grammar), use a tree-sitter query to find them and apply the same markdown faces:

```elisp
(defun eucalypt--fontify-markdown-in-docstrings (start end)
  "Apply markdown highlighting within docstring regions between START and END."
  (save-excursion
    (let ((root (treesit-buffer-root-node)))
      ;; Direct docstring nodes
      (dolist (cap (treesit-query-capture root '((docstring) @ds) start end))
        (eucalypt--apply-markdown-faces
         (treesit-node-start (cdr cap))
         (treesit-node-end (cdr cap))))
      ;; doc: values in block-valued metadata (declaration, unit, and block)
      (dolist (query '(((metadata (block (declaration
                          (declaration_head (identifier) @_key)
                          (soup [(string) (c_string) (r_string)] @ds)))
                         (#eq? @_key "doc")))
                       ((unit_metadata (block (declaration
                          (declaration_head (identifier) @_key)
                          (soup [(string) (c_string) (r_string)] @ds)))
                         (#eq? @_key "doc")))
                       ((block_metadata (block (declaration
                          (declaration_head (identifier) @_key)
                          (soup [(string) (c_string) (r_string)] @ds)))
                         (#eq? @_key "doc")))))
        (dolist (cap (treesit-query-capture root query start end))
          (when (eq (car cap) 'ds)
            (eucalypt--apply-markdown-faces
             (treesit-node-start (cdr cap))
             (treesit-node-end (cdr cap)))))))))
```

**Verification:** Open a .eu file with docstrings in Emacs. Inline code, bold, and italic should be highlighted within docstrings but not in regular strings.

---

## Chunk 4: Tests

### Task 4: Test the grammar changes

**Files:**
- Modify: `editors/tree-sitter-eucalypt/test/corpus/*.txt` (add docstring test cases)

- [ ] **Step 1: Add test cases for docstring node aliasing**

Test that strings in metadata positions produce `docstring` nodes:

```
================
String metadata becomes docstring
================

` "This is a docstring"
x: 1

---

(source_file
  (declaration
    (metadata (docstring))
    (declaration_head (identifier))
    (soup (literal (number)))))
```

- [ ] **Step 2: Test unit_metadata docstring**

```
================
Unit metadata string becomes docstring
================

"File documentation"

x: 1

---

(source_file
  (unit_metadata (docstring))
  (declaration
    (declaration_head (identifier))
    (soup (literal (number)))))
```

- [ ] **Step 3: Test block metadata with doc key**

Verify the query matches `doc:` values in block metadata (this is a highlight query test, not a grammar test — verify manually or with `tree-sitter highlight`).

**Verification:** `npx tree-sitter test` passes. All existing tests still pass.

---

## Implementation Order

1. **Chunk 1** (grammar alias) — prerequisite for everything else
2. **Chunk 2** (highlights query) — works with any tree-sitter consumer
3. **Chunk 3** (Emacs markdown overlays) — the user-facing feature
4. **Chunk 4** (tests) — can be done alongside Chunks 1-2

## Risk Assessment

- **Low risk:** `alias` in tree-sitter only changes the node name, not the parse behaviour. All existing tests should pass unchanged.
- **Low risk:** The markdown regex patterns are well-understood and operate only within docstring boundaries.
- **Medium risk:** The backtick pattern for inline code could interact with string interpolation braces. Mitigation: the regex operates on the text content, and interpolation `{...}` regions are separate tree-sitter nodes — the Emacs overlay function should skip interpolation regions.
- **Low risk:** `jit-lock-register` is the standard Emacs mechanism for layered font-lock and works well with `treesit`.
