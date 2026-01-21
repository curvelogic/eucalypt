; Tree-sitter highlights for Eucalypt
; For use with Emacs tree-sitter mode and other editors

; Comments
(comment) @comment

; Literals
(number) @number
(string) @string
(string_content) @string
(symbol
  ":" @constant
  (identifier) @constant)
(symbol
  ":" @constant
  (quoted_identifier) @constant)

; String interpolation
(interpolation
  "{" @punctuation.special
  "}" @punctuation.special)
(interpolation_content) @variable
(format_spec) @string.special
(escape_sequence) @string.escape

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
    "map" "filter" "foldl" "foldr" "reduce" "scanl" "scanr"
    "and" "or" "not"
    "merge" "concat" "append" "prepend"
    "identity" "const" "compose" "apply" "flip"
    "take" "drop" "take-while" "drop-while"
    "all" "any" "all-true?" "any-true?"
    "keys" "values" "lookup" "has"
    "range" "repeat" "iterate" "cycle"))

; Metadata
(metadata
  "`" @attribute)

; Unit metadata
(unit_metadata) @comment.documentation
