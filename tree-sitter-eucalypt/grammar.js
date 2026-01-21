/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

/**
 * Tree-sitter grammar for Eucalypt
 *
 * Eucalypt is a functional language for generating, templating, and
 * rendering structured data formats (YAML, JSON, TOML).
 */

// Helper to define operator characters
const OPER_CHARS = /[.!@£%^&*|><\/+\=\-~;∧∨∘→←⊕⊗⊙⊡⊞⊟⟨⟩⟪⟫⟦⟧⌈⌉⌊⌋¬∀∃∈∉⊂⊃⊆⊇∪∩∼≈≠≡≤≥≪≫±×÷√∞∂∫∑∏∇△▽⊥⊤⊢⊣⊨⊩⊸⊺⋀⋁⋂⋃⋄⋅⋆⋈⋉⋊⋮⋯⋰⋱⟵⟶⟷⟸⟹⟺⟻⟼⟽⟾⟿←→↑↓↔↕↖↗↘↙↚↛↜↝↞↟↠↡↢↣↤↥↦↧↨↩↪↫↬↭↮↯↰↱↲↳↴↵↶↷↸↹↺↻⇐⇑⇒⇓⇔⇕⇖⇗⇘⇙⇚⇛⇜⇝⇞⇟⇠⇡⇢⇣⇤⇥⇦⇧⇨⇩⇪⊂⊃⊄⊅⊆⊇⊈⊉⊊⊋¡££€⨈]+/;

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
    [$.declaration_head],
    [$.soup],
    [$.dotted_reference, $.interpolation_content],
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
      $.string,  // Documentation string
    ),

    // Comments start with # and go to end of line
    comment: $ => /#[^\n\r]*/,

    // === Declarations ===

    declaration: $ => prec.right(seq(
      optional($.metadata),
      $.declaration_head,
      ':',
      optional($.soup),
    )),

    // Metadata: backtick followed by expression
    metadata: $ => seq(
      '`',
      $._meta_value,
    ),

    _meta_value: $ => choice(
      $.string,
      $.symbol,
      $.block,
    ),

    // Declaration head: the part before the colon
    declaration_head: $ => choice(
      // Simple property: name (identifier or quoted)
      $.identifier,
      $.quoted_identifier,
      // Function: name(params)
      seq(choice($.identifier, $.quoted_identifier), $.parameter_list),
      // Operator declarations in parentheses
      $.operator_declaration,
    ),

    // Operator declaration patterns
    operator_declaration: $ => seq(
      '(',
      choice(
        // Binary operator: (l op r)
        seq($.identifier, $.operator, $.identifier),
        // Unary prefix operator: (op x)
        seq($.operator, $.identifier),
        // Unary postfix operator: (x op)
        seq($.identifier, $.operator),
      ),
      ')',
    ),

    parameter_list: $ => seq(
      '(',
      optional(seq(
        $.identifier,
        repeat(seq(',', $.identifier)),
        optional(','),
      )),
      ')',
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

    // === Literals ===

    literal: $ => choice(
      $.number,
      $.string,
      $.symbol,
    ),

    number: $ => token(choice(
      // Integer
      /-?[0-9]+/,
      // Float
      /-?[0-9]+\.[0-9]+/,
    )),

    // String with optional interpolation
    string: $ => seq(
      '"',
      repeat(choice(
        $.string_content,
        $.interpolation,
        $.escape_sequence,
      )),
      '"',
    ),

    string_content: $ => /[^"{}\\]+/,

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

    escape_sequence: $ => choice(
      '{{',
      '}}',
      /\\./,
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

    // Identifiers support Unicode letters including Latin Extended, Greek, Cyrillic
    identifier: $ => /[•$?_a-zA-Z\u00C0-\u02AF\u0370-\u03FF\u0400-\u04FF\u1F00-\u1FFF][•$?_!*\-a-zA-Z0-9\u00C0-\u02AF\u0370-\u03FF\u0400-\u04FF\u1F00-\u1FFF]*/,

    quoted_identifier: $ => seq(
      "'",
      /[^']+/,
      "'",
    ),

    // Operators: sequences of operator characters
    operator: $ => token(prec(-1, /[.!@£%^&*|><\/+\=\-~;∧∨∘→←⊕⊗⊙⊡⊞⊟⟨⟩⟪⟫⟦⟧⌈⌉⌊⌋¬∀∃∈∉⊂⊃⊆⊇∪∩∼≈≠≡≤≥≪≫±×÷√∞∂∫∑∏∇△▽⊥⊤⊢⊣⊨⊩⊸⊺⋀⋁⋂⋃⋄⋅⋆⋈⋉⋊⋮⋯⋰⋱⟵⟶⟷⟸⟹⟺⟻⟼⟽⟾⟿←→↑↓↔↕↖↗↘↙↚↛↜↝↞↟↠↡↢↣↤↥↦↧↨↩↪↫↬↭↮↯↰↱↲↳↴↵↶↷↸↹↺↻⇐⇑⇒⇓⇔⇕⇖⇗⇘⇙⇚⇛⇜⇝⇞⇟⇠⇡⇢⇣⇤⇥⇦⇧⇨⇩⇪¡££€⨈]+/)),

    // Anaphora: _ or _0, _1, etc., or • or •0, •1, etc.
    anaphor: $ => choice(
      /\u2022[0-9]*/,  // • followed by optional digits
      /_[0-9]*/,       // _ followed by optional digits
    ),

    // === Compound structures ===

    // Block: { declarations }
    block: $ => seq(
      '{',
      repeat(seq(
        $.declaration,
        optional(','),
      )),
      '}',
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
