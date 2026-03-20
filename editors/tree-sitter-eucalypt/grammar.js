/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

/**
 * Tree-sitter grammar for Eucalypt
 *
 * Eucalypt is a functional language for generating, templating, and
 * rendering structured data formats (YAML, JSON, TOML).
 */

// Helper to define operator characters.
// Includes: standard punctuation operators, Unicode math/arrows, and
// special characters used in the prelude.
// Notable inclusions: ? (for //=?, //!?), $ (for <$>), ∸ (unary minus),
// ‖ (U+2016 DOUBLE VERTICAL LINE — the cons operator),
// ✓ (U+2713 CHECK MARK — postfix non-nil check).
// Note: bracket characters (⟦⟧⟨⟩⟪⟫⌈⌉⌊⌋) are intentionally excluded here;
// they are handled by the bracket_expr rule instead.
const OPER_CHARS = /[.!@£%^&*|><\/+\=\-~;?$∸∧∨∘‖✓→←⊕⊗⊙⊡⊞⊟¬∀∃∈∉⊂⊃⊆⊇∪∩∼≈≠≡≤≥≪≫±×÷√∞∂∫∑∏∇△▽⊥⊤⊢⊣⊨⊩⊸⊺⋀⋁⋂⋃⋄⋅⋆⋈⋉⋊⋮⋯⋰⋱⟵⟶⟷⟸⟹⟺⟻⟼⟽⟾⟿←→↑↓↔↕↖↗↘↙↚↛↜↝↞↟↠↡↢↣↤↥↦↧↨↩↪↫↬↭↮↯↰↱↲↳↴↵↶↷↸↹↺↻⇐⇑⇒⇓⇔⇕⇖⇗⇘⇙⇚⇛⇜⇝⇞⇟⇠⇡⇢⇣⇤⇥⇦⇧⇨⇩⇪⊂⊃⊄⊅⊆⊇⊈⊉⊊⊋¡££€⨈∅∏]+/;

// Unicode idiot bracket open characters (must match brackets.rs BUILTIN_BRACKET_PAIRS).
//
// NOTE ON BRACKET DETECTION LIMITATIONS:
// The Rust parser uses dynamic Unicode Ps/Pe category detection, meaning any
// Unicode bracket pair (Open/Close punctuation categories) works automatically.
// This tree-sitter grammar hardcodes specific pairs instead.  To add a new
// bracket pair, add the open character to BRACKET_OPEN_RE and the corresponding
// close character to BRACKET_CLOSE_RE, then regenerate parser.c with
// `tree-sitter generate`.
const BRACKET_OPEN_RE = /[⟦⟨⟪⌈⌊⦃⦇⦉«【〔〖〘〚]/;
const BRACKET_CLOSE_RE = /[⟧⟩⟫⌉⌋⦄⦈⦊»】〕〗〙〛]/;

module.exports = grammar({
  name: 'eucalypt',

  extras: $ => [
    /\s+/,
    $.comment,
  ],

  externals: $ => [
    $._declaration_end,
  ],

  word: $ => $.identifier,

  conflicts: $ => [
    [$.declaration_head, $.name],
    [$.declaration_head, $.block_param],
    [$.operator_declaration, $.name],
    [$.operator_declaration, $.soup],
    [$.soup],
  ],

  rules: {
    // Top-level: a unit is optionally a block (test metadata) followed by declarations
    // Some test files start with a bare block for metadata
    source_file: $ => seq(
      optional($.unit_metadata),
      repeat($.declaration),
    ),

    // Some files start with a bare block for metadata (test files)
    unit_metadata: $ => choice(
      $.block,
      alias($.string, $.docstring),    // Documentation string
      alias($.c_string, $.docstring),  // Documentation c-string
      alias($.r_string, $.docstring),  // Documentation r-string
    ),

    // Comments start with # and go to end of line
    comment: $ => /#[^\n\r]*/,

    // === Declarations ===

    declaration: $ => prec.right(seq(
      optional($.metadata),
      $.declaration_head,
      ':',
      optional($.soup),
      optional($._declaration_end),
    )),

    // Metadata: backtick followed by expression
    metadata: $ => seq(
      '`',
      $._meta_value,
    ),

    _meta_value: $ => choice(
      alias($.string, $.docstring),
      alias($.c_string, $.docstring),
      alias($.r_string, $.docstring),
      $.symbol,
      $.block,
    ),

    // Declaration head: the part before the colon
    declaration_head: $ => choice(
      // Simple property: name (identifier or quoted)
      $.identifier,
      $.quoted_identifier,
      // Function: name(params) — params may include destructuring patterns
      seq(choice($.identifier, $.quoted_identifier), $.parameter_list),
      // Juxtaposed destructuring definitions:
      //   f{x y}: ...  — single block-destructured parameter (sugar for f({x y}): ...)
      //   f[x, y]: ... — single list-destructured parameter (sugar for f([x, y]): ...)
      //   f[h : t]: ... — single cons-destructured parameter (sugar for f([h : t]): ...)
      seq(choice($.identifier, $.quoted_identifier), $.block_param),
      seq(choice($.identifier, $.quoted_identifier), $.list_param),
      // Operator declarations in parentheses
      $.operator_declaration,
    ),

    // Block destructuring parameter in juxtaposed position: f{x y} or f{x, y}
    // Uses token.immediate to require no whitespace (matching Rust parser).
    // Bare identifiers are allowed as shorthand bindings (like block_pattern).
    block_param: $ => seq(
      token.immediate('{'),
      repeat(seq(
        choice($.declaration, $.identifier),
        optional(','),
      )),
      '}',
    ),

    // List or cons destructuring parameter in juxtaposed position: f[x, y] or f[h : t]
    // Uses token.immediate to require no whitespace.
    list_param: $ => seq(
      token.immediate('['),
      choice(
        // Cons pattern: [h : t]
        seq($.identifier, ':', $.identifier),
        // List pattern: [x, y, ...]
        seq(
          $.identifier,
          repeat(seq(',', $.identifier)),
          optional(','),
        ),
      ),
      ']',
    ),

    // Operator declaration patterns
    operator_declaration: $ => seq(
      '(',
      choice(
        // Idiot bracket operator: (⟦ x ⟧) — declares a bracket-pair function
        seq(BRACKET_OPEN_RE, $.identifier, BRACKET_CLOSE_RE),
        // Binary operator: (l op r)
        seq($.identifier, $.operator, $.identifier),
        // Unary prefix operator: (op x)
        seq($.operator, $.identifier),
        // Unary postfix operator: (x op)
        seq($.identifier, $.operator),
        // Nullary operator: (op)
        $.operator,
      ),
      ')',
    ),

    // A parameter in a function declaration may be:
    //   - a simple identifier: f(x, y)
    //   - a block destructuring pattern: f({x y}) or f({x: a  y: b})
    //   - a fixed-length list destructuring pattern: f([a, b])
    //   - a head/tail cons pattern: f([h : t])
    _param: $ => choice(
      $.identifier,
      $.block_pattern,
      $.list_pattern,
      $.cons_pattern,
    ),

    parameter_list: $ => seq(
      '(',
      optional(seq(
        $._param,
        repeat(seq(',', $._param)),
        optional(','),
      )),
      ')',
    ),

    // Block destructuring pattern in parameter position: {x y} or {x: a  y: b}
    // A block_pattern has the same surface syntax as a block but only appears
    // as a function parameter.  Fields may be shorthand (`x` binds field x) or
    // renamed (`x: a` binds field x as local variable a).
    block_pattern: $ => seq(
      '{',
      repeat(seq(
        choice($.declaration, $.identifier),  // bare identifier = shorthand binding
        optional(','),
      )),
      '}',
    ),

    // Fixed-length list destructuring pattern: [a, b, c]
    list_pattern: $ => seq(
      '[',
      seq(
        $.identifier,
        repeat(seq(',', $.identifier)),
        optional(','),
      ),
      ']',
    ),

    // Head/tail cons destructuring pattern: [h : t]
    // Uses ':' as the separator (not ',') to distinguish from list_pattern.
    cons_pattern: $ => seq(
      '[',
      $.identifier,
      ':',
      $.identifier,
      ']',
    ),

    // === Expressions (soup) ===

    // Soup: sequence of elements with operators between them
    // Terminates when external scanner detects a new declaration
    soup: $ => seq(
      repeat1(choice(
        $._element,
        $.operator,
      )),
    ),

    _element: $ => choice(
      $.literal,
      $.name,
      $.block,
      $.list,
      $.paren_expr,
      $.application,
      $.block_application,
      $.list_application,
      $.bracket_expr,
    ),

    // Parenthesized expression
    paren_expr: $ => seq('(', optional($.soup), ')'),

    // Function application: name(args) or element(args)
    application: $ => prec(2, seq(
      choice($.name, $.paren_expr, $.block),
      $.argument_list,
    )),

    argument_list: $ => prec(2, seq(
      token.immediate('('),
      optional(seq(
        $.soup,
        repeat(seq(',', $.soup)),
        optional(','),
      )),
      ')',
    )),

    // Juxtaposed block call: f{x: 1, y: 2} — sugar for f({x: 1, y: 2}).
    // The Rust parser handles this via OPEN_BRACE_APPLY token ('{' immediately
    // following a name/expression, with no whitespace).
    block_application: $ => prec(2, seq(
      choice($.name, $.paren_expr),
      $.block_argument,
    )),

    block_argument: $ => prec(2, seq(
      token.immediate('{'),
      repeat(seq(
        $.declaration,
        optional(','),
      )),
      '}',
    )),

    // Juxtaposed list call: f[1, 2] — sugar for f([1, 2]).
    // The Rust parser handles this via OPEN_SQUARE_APPLY token ('[' immediately
    // following a name/expression, with no whitespace).
    list_application: $ => prec(2, seq(
      choice($.name, $.paren_expr),
      $.list_argument,
    )),

    list_argument: $ => prec(2, seq(
      token.immediate('['),
      optional(seq(
        $.soup,
        repeat(seq(',', $.soup)),
        optional(','),
      )),
      ']',
    )),

    // Idiot bracket expression: ⟦ expr ⟧, «expr», ⌈ expr ⌉, etc.
    // The bracket pair determines which bracket-pair function is applied.
    //
    // Monadic blocks use bracket syntax and may contain declarations rather
    // than plain expressions, so we allow either soup or declarations inside.
    bracket_expr: $ => seq(
      BRACKET_OPEN_RE,
      optional(choice(
        $.soup,
        repeat1(seq($.declaration, optional(','))),
      )),
      BRACKET_CLOSE_RE,
    ),

    // === Literals ===

    literal: $ => choice(
      $.number,
      $.string,
      $.c_string,
      $.r_string,
      $.symbol,
    ),

    number: $ => token(choice(
      // Integer
      /-?[0-9]+/,
      // Float
      /-?[0-9]+\.[0-9]+/,
    )),

    // String with optional interpolation (plain string - no backslash escape processing)
    string: $ => seq(
      '"',
      repeat(choice(
        $.string_content,
        $.interpolation,
        $.brace_escape,
      )),
      '"',
    ),

    // C-string with C-style escape sequences
    c_string: $ => seq(
      'c"',
      repeat(choice(
        $.c_string_content,
        $.interpolation,
        $.c_escape_sequence,
      )),
      '"',
    ),

    // Raw string (explicit raw - same as plain, no backslash escape processing)
    r_string: $ => seq(
      'r"',
      repeat(choice(
        $.r_string_content,
        $.interpolation,
        $.brace_escape,
      )),
      '"',
    ),

    // String content rules use token.immediate() to prevent extras
    // (especially comments starting with #) from being injected inside
    // string literals.
    string_content: $ => token.immediate(prec(1, /[^"{}]+/)),

    c_string_content: $ => token.immediate(prec(1, /[^"{}\\]+/)),

    r_string_content: $ => token.immediate(prec(1, /[^"{}]+/)),

    interpolation: $ => seq(
      '{',
      optional(field('content', $.interpolation_content)),
      optional($.format_spec),
      '}',
    ),

    // Interpolation can contain:
    // - simple references: {foo}, {0}, {_}
    // - dotted paths: {foo.bar.baz}
    // - positional args: {0}, {1}, {2}
    interpolation_content: $ => choice(
      $.anaphor,
      $.dotted_reference,
      $.identifier,
      /[0-9]+/,  // positional argument
    ),

    dotted_reference: $ => seq(
      $.identifier,
      repeat1(seq('.', $.identifier)),
    ),

    format_spec: $ => /:[^}]+/,

    // Brace escapes for plain and raw strings ({{ and }})
    brace_escape: $ => choice(
      '{{',
      '}}',
    ),

    // C-string escape sequences (full C-style escapes)
    c_escape_sequence: $ => choice(
      // Basic escapes: \n \r \t \\ \" \0 \{ \}
      /\\[nrt0\\"{}]/,
      // Hex escape: \xHH
      /\\x[0-9a-fA-F]{2}/,
      // Unicode 4-digit: \uHHHH
      /\\u[0-9a-fA-F]{4}/,
      // Unicode 8-digit: \UHHHHHHHH
      /\\U[0-9a-fA-F]{8}/,
    ),

    // Symbol: :name or :'quoted'
    symbol: $ => choice(
      seq(':', $.identifier),
      seq(':', $.quoted_identifier),
    ),

    // === Names and Identifiers ===

    // Name in expression context: identifier or quoted
    // (operators are separate in expressions)
    name: $ => choice(
      $.identifier,
      $.quoted_identifier,
    ),

    // Identifiers support Unicode letters including Latin Extended, Greek, Cyrillic.
    // The Latin Extended range U+00C0-U+02AF is split to exclude × (U+00D7) and
    // ÷ (U+00F7) which are operator characters.
    identifier: $ => /[•$?_a-zA-Z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02AF\u0370-\u03FF\u0400-\u04FF\u1F00-\u1FFF][•$?_!*\-a-zA-Z0-9\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02AF\u0370-\u03FF\u0400-\u04FF\u1F00-\u1FFF]*/,

    quoted_identifier: $ => seq(
      "'",
      /[^']+/,
      "'",
    ),

    // Operators: sequences of operator characters.
    // Bracket characters (⟦⟧⟨⟩⟪⟫⌈⌉⌊⌋) are deliberately excluded here;
    // they are matched by the bracket_expr rule via BRACKET_OPEN_RE/BRACKET_CLOSE_RE.
    // ‖ (U+2016 DOUBLE VERTICAL LINE) is included as the cons operator.
    operator: $ => token(prec(-1, /[.!@£%^&*|><\/+\=\-~;?$∸∧∨∘‖→←⊕⊗⊙⊡⊞⊟¬∀∃∈∉⊂⊃⊆⊇∪∩∼≈≠≡≤≥≪≫±×÷√∞∂∫∑∏∇△▽⊥⊤⊢⊣⊨⊩⊸⊺⋀⋁⋂⋃⋄⋅⋆⋈⋉⋊⋮⋯⋰⋱⟵⟶⟷⟸⟹⟺⟻⟼⟽⟾⟿←→↑↓↔↕↖↗↘↙↚↛↜↝↞↟↠↡↢↣↤↥↦↧↨↩↪↫↬↭↮↯↰↱↲↳↴↵↶↷↸↹↺↻⇐⇑⇒⇓⇔⇕⇖⇗⇘⇙⇚⇛⇜⇝⇞⇟⇠⇡⇢⇣⇤⇥⇦⇧⇨⇩⇪⊂⊃⊄⊅⊆⊇⊈⊉⊊⊋¡££€⨈∅∏]+/)),

    // Anaphora: _ or _0, _1, etc., or • or •0, •1, etc.
    // NOTE: bare _ and • also match the identifier regex.  Because tree-sitter's
    // lexer cannot use context to disambiguate, anaphors that overlap with
    // identifiers (_, _0, •, •0) will be lexed as identifiers.  Downstream
    // consumers (e.g. the emacs mode) can treat standalone _ / • identifiers as
    // anaphors for highlighting purposes.  The anaphor rule still exists for use
    // inside string interpolation where identifier is not an alternative.
    anaphor: $ => choice(
      /\u2022[0-9]*/,  // • followed by optional digits
      /_[0-9]*/,       // _ followed by optional digits
    ),

    // === Compound structures ===

    // Block: { optional-metadata declarations }
    block: $ => seq(
      '{',
      optional($.block_metadata),
      repeat(seq(
        $.declaration,
        optional(','),
      )),
      '}',
    ),

    // Block metadata: a value at the start of a block before any declarations.
    // Strings are aliased to docstring for highlighting.
    // $.literal is not used directly (it includes strings, causing conflicts);
    // instead number is listed explicitly.
    // Identifiers are excluded (ambiguous with declaration heads).
    block_metadata: $ => choice(
      alias($.string, $.docstring),
      alias($.c_string, $.docstring),
      alias($.r_string, $.docstring),
      $.symbol,
      $.block,
      $.list,
      $.number,
    ),

    // List: [elements]
    list: $ => seq(
      '[',
      optional(seq(
        $.soup,
        repeat(seq(',', $.soup)),
        optional(','),
      )),
      ']',
    ),
  },
});
