; Tree-sitter highlights for Eucalypt
; For use with Emacs tree-sitter mode and other editors

; Comments
(comment) @comment

; Literals
(number) @number

; Plain strings
(string) @string
(string_content) @string

; C-strings (with C-style escape processing) - entire literal including prefix
(c_string) @string
(c_string_content) @string
(c_escape_sequence) @string.escape
(c_string "c\"" @string)
(c_string "\"" @string)

; Raw strings (explicit raw, no escape processing) - entire literal including prefix
(r_string) @string
(r_string_content) @string
(r_string "r\"" @string)
(r_string "\"" @string)

; T-strings (ZDT / zoned date-time literals) - no interpolation, no escapes
(t_string) @string.special
(t_string_content) @string.special
(t_string "t\"" @string.special)
(t_string "\"" @string.special)

; Symbols
(symbol
  ":" @constant
  (identifier) @constant)
(symbol
  ":" @constant
  (quoted_identifier) @constant)

; String interpolation (works in all string types)
(interpolation
  "{" @punctuation.special
  "}" @punctuation.special)
(interpolation_content) @variable
(format_spec) @string.special

; Brace escapes in plain and raw strings
(brace_escape) @string.escape

; Anaphora (special variables)
(anaphor) @variable.special

; Operators
(operator) @operator

; Punctuation
":" @punctuation.delimiter
"," @punctuation.delimiter
"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"`" @punctuation.special

; Declaration heads - property name
(declaration
  (declaration_head
    (identifier) @property))

; Declaration heads - function name with parameters
(declaration
  (declaration_head
    (identifier) @function
    (parameter_list)))

; Operator declarations
(operator_declaration
  (identifier) @parameter
  (operator) @function
  (identifier) @parameter)

; Function parameters — simple identifiers
(parameter_list
  (identifier) @parameter)

; Destructuring parameters — block pattern field names (renamed/nested
; fields, e.g. `{x: a}` or `{outer: {inner}}`). The field name is always
; the FIRST child; the value may be a plain identifier (rename) or a
; nested block/list/cons pattern, so it isn't matched positionally here —
; the list_pattern/cons_pattern/block_pattern_field rules below cover any
; identifiers nested inside it.
(block_pattern_field
  .
  (identifier) @parameter)

; Destructuring parameters — list pattern element names
(list_pattern
  (identifier) @parameter)

; Destructuring parameters — cons pattern head and tail names
(cons_pattern
  (identifier) @parameter)

; Idiot bracket expressions — highlight the bracket delimiters distinctively
(bracket_expr) @punctuation.special

; Function application
(application
  (name
    (identifier) @function.call))

; Built-in intrinsics (__XXX)
((identifier) @function.builtin
  (#match? @function.builtin "^__[A-Z]+$"))

; Prelude functions (commonly used)
((identifier) @keyword
  (#any-of? @keyword
    "if" "then" "when" "cond"
    "true" "false" "null" "nil"
    "cons" "head" "tail" "first" "second"
    "head-or" "tail-or" "second-or"
    "map" "filter" "foldl" "foldr" "scanl" "scanr"
    "and" "or" "not"
    "merge" "concat" "append" "prepend"
    "identity" "const" "compose" "apply" "flip"
    "take" "drop" "take-while" "drop-while"
    "all" "any" "all-true?" "any-true?"
    "keys" "values" "lookup" "has"
    "range" "repeat" "iterate" "cycle"
    "zip" "zip-with" "reverse" "remove"
    "mapcat" "group-by" "qsort" "partition"
    "negate" "inc" "dec" "floor" "ceiling"
    "max" "min" "abs" "num"
    "panic" "assert"
    "deep-merge" "merge-all" "elements" "block"
    "lookup-in" "lookup-or" "lookup-or-in" "lookup-alts" "lookup-across" "lookup-path"
    "complement" "curry" "uncurry" "juxt" "fnil"
    "with-meta" "meta" "merge-meta" "assertions"
    "split-at" "take-until" "drop-until"
    "split-after" "split-when" "nth" "count" "last"
    "map2" "zip-apply"
    "window" "over-sliding-pairs" "differences"
    "discriminate"
    "key" "value" "bimap" "map-first" "map-second" "map-kv" "map-as-block"
    "pair" "zip-kv" "with-keys" "map-values" "map-keys"
    "filter-items" "by-key" "by-key-name" "by-key-match" "by-value"
    "match-filter-values" "filter-values"
    "alter-value" "update-value" "alter" "update" "update-value-or"
    "set-value" "tongue" "merge-at"
    "nil?" "zero?" "pos?" "neg?"
    "any?" "non-nil?" "block?" "bool?" "list?" "number?" "string?" "symbol?" "is-array?" "match?"
    "max-of" "min-of" "max-of-by" "max-of-or" "min-of-by" "min-of-or"
    "sym" "ch" "str"
    "arr" "monad" "random" "vec"
    "render" "render-as" "parse-as" "parse-args"
    "deep-transform" "deep-fold"
    "deep-find" "deep-find-first" "deep-find-paths"
    "deep-query" "deep-query-first" "deep-query-fold" "deep-query-paths"
    "deep-merge-at"
    "sort-by" "sort-by-num" "sort-by-str" "sort-by-zdt"
    "sort-keys" "sort-nums" "sort-strs" "sort-zdts"
    "kv-block" "map-elements" "coalesce" "group-consecutive" "group-consecutive-by"
    "iota" "reduce" "rotate" "tails" "update-nth" "update-first"
    "interleave" "unzip" "butlast" "ints-from" "snoc" "cross"
    "sum" "product" "running-sum" "running-max" "running-min"
    "nub-by" "uniq" "partition-all" "window-all"
    "div" "mod" "rem" "quot" "pow"
    "dbg"
    "eu" "io" "cal" "iosm"))

; Metadata
(metadata
  "`" @attribute)

; Docstrings — string-valued metadata (aliased to docstring by grammar)
(docstring) @comment.documentation

; Block-valued unit metadata (non-docstring)
(unit_metadata (block) @comment.documentation)

; Doc values inside block-valued declaration metadata
; Matches: ` {doc: "text", ...}
(metadata
  (block
    (declaration
      (declaration_head
        (identifier) @_key)
      (soup
        (literal (string) @comment.documentation))))
  (#eq? @_key "doc"))

(metadata
  (block
    (declaration
      (declaration_head
        (identifier) @_key)
      (soup
        (literal (c_string) @comment.documentation))))
  (#eq? @_key "doc"))

(metadata
  (block
    (declaration
      (declaration_head
        (identifier) @_key)
      (soup
        (literal (r_string) @comment.documentation))))
  (#eq? @_key "doc"))

; Doc values inside block-valued block metadata
; Matches: { {doc: "text", ...} x: 1 }
(block_metadata
  (block
    (declaration
      (declaration_head
        (identifier) @_key)
      (soup
        (literal (string) @comment.documentation))))
  (#eq? @_key "doc"))

(block_metadata
  (block
    (declaration
      (declaration_head
        (identifier) @_key)
      (soup
        (literal (c_string) @comment.documentation))))
  (#eq? @_key "doc"))

(block_metadata
  (block
    (declaration
      (declaration_head
        (identifier) @_key)
      (soup
        (literal (r_string) @comment.documentation))))
  (#eq? @_key "doc"))

; Doc values inside block-valued unit metadata
; Matches: { doc: "text", ... } at file level (bare block at top of file)
(unit_metadata
  (block
    (declaration
      (declaration_head
        (identifier) @_key)
      (soup
        (literal (string) @comment.documentation))))
  (#eq? @_key "doc"))

(unit_metadata
  (block
    (declaration
      (declaration_head
        (identifier) @_key)
      (soup
        (literal (c_string) @comment.documentation))))
  (#eq? @_key "doc"))

(unit_metadata
  (block
    (declaration
      (declaration_head
        (identifier) @_key)
      (soup
        (literal (r_string) @comment.documentation))))
  (#eq? @_key "doc"))
