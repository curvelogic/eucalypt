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

; Function parameters
(parameter_list
  (identifier) @parameter)

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
    "max-of" "min-of"
    "sym" "ch" "str"
    "eu" "io" "cal" "iosm"))

; Metadata
(metadata
  "`" @attribute)

; Unit metadata
(unit_metadata) @comment.documentation
