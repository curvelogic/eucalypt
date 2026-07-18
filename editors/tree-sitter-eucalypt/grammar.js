/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

/**
 * Tree-sitter grammar for Eucalypt
 *
 * Eucalypt is a functional language for generating, templating, and
 * rendering structured data formats (YAML, JSON, TOML).
 */

// Single source of truth for operator characters — referenced by the
// `operator` rule below. Includes: standard punctuation operators, Unicode
// math/arrows, and special characters used in the prelude.
// Notable inclusions: ? (for //=?, //!?), $ (for <$>), ∸ (unary minus),
// ‖ (U+2016 DOUBLE VERTICAL LINE — the cons operator),
// ✓ (U+2713 CHECK MARK — postfix non-nil check),
// ▶ (U+25B6 BLACK RIGHT-POINTING TRIANGLE — debug trace),
// ⊝ (U+229D CIRCLED MINUS — bitwise NOT).
// Note: bracket characters (⟦⟧⟨⟩⟪⟫⌈⌉⌊⌋‹›) are intentionally excluded here;
// they are handled by the bracket_expr rule instead.
const OPER_CHARS = /[.!@£%^&*|><\/\\+\=\-~;?$∸∧∨∘‖✓▶⊝→←⊕⊗⊙⊡⊞⊟¬∀∃∈∉⊂⊃⊆⊇∪∩∼≈≠≡≤≥≪≫±×÷√∞∂∫∑∏∇△▽⊥⊤⊢⊣⊨⊩⊸⊺⋀⋁⋂⋃⋄⋅⋆⋈⋉⋊⋮⋯⋰⋱⟵⟶⟷⟸⟹⟺⟻⟼⟽⟾⟿←→↑↓↔↕↖↗↘↙↚↛↜↝↞↟↠↡↢↣↤↥↦↧↨↩↪↫↬↭↮↯↰↱↲↳↴↵↶↷↸↹↺↻⇐⇑⇒⇓⇔⇕⇖⇗⇘⇙⇚⇛⇜⇝⇞⇟⇠⇡⇢⇣⇤⇥⇦⇧⇨⇩⇪⊂⊃⊄⊅⊆⊇⊈⊉⊊⊋¡££€⨈∅∏]+/;

// Unicode idiot bracket open characters (must match brackets.rs BUILTIN_BRACKET_PAIRS).
//
// NOTE ON BRACKET DETECTION LIMITATIONS:
// The Rust parser uses dynamic Unicode Ps/Pe category detection, meaning any
// Unicode bracket pair (Open/Close punctuation categories) works automatically.
// This tree-sitter grammar hardcodes specific pairs instead.  To add a new
// bracket pair, add the open character to BRACKET_OPEN_RE and the corresponding
// close character to BRACKET_CLOSE_RE, then regenerate parser.c with
// `tree-sitter generate`.
const BRACKET_OPEN_RE = /[⟦⟨⟪⌈⌊⦃⦇⦉«‹【〔〖〘〚]/;
const BRACKET_CLOSE_RE = /[⟧⟩⟫⌉⌋⦄⦈⦊»›】〕〗〙〛]/;

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
    [$.declaration_head, $._element],
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

    // Some files have no top-level declarations at all — just a bare
    // expression (soup), of which a single string literal (aliased to
    // docstring, for highlighting — see queries/highlights.scm) and a bare
    // block (test metadata) are the common cases, but ANY soup is valid
    // here: the Rowan parser treats a unit with no leading `name:` as a
    // whole as bare BLOCK_META wrapping an arbitrary soup (confirmed via
    // `eu dump ast`), not just block/string. This also covers block-dot
    // access at the top level, e.g. `{ :for x: 42 }.(x)`, where the block
    // is only the FIRST soup element, followed by `.` and a paren_expr.
    unit_metadata: $ => choice(
      alias($.string, $.docstring),    // Documentation string
      alias($.c_string, $.docstring),  // Documentation c-string
      alias($.r_string, $.docstring),  // Documentation r-string
      // A bare block, optionally followed by more soup content (e.g.
      // block-dot access at the top level: `{ :for x: 42 }.(x)`, where the
      // block is only the FIRST element of the unit's soup, followed by
      // the `.` operator and a paren_expr). Scoped to require the unit to
      // START with `{` specifically, rather than allowing an arbitrary
      // soup here — a fully general top-level soup collides with
      // `repeat($.declaration)` (e.g. `f(x): x` gets misparsed as a
      // 2-element soup, application "f(x)" then symbol literal ": x")
      // since that ambiguity isn't flagged as a real GLR conflict by the
      // generator and silently resolves the wrong way.
      prec.right(seq(
        $.block,
        repeat(choice($._element, $.operator)),
        optional($._declaration_end),
      )),
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
      // Bracket-pair ("idiot bracket") declarations: ⟦ x ⟧: x declares the
      // ⟦⟧ bracket pair as a function of x. The bracket's content is an
      // ordinary soup (often a list_pattern-shaped `[x]`, or a block for
      // inline monad definitions like ⟦{}⟧: { :monad ... }) — bracket_expr
      // already parses all of that; it just wasn't reachable as a
      // declaration head. Audit report §2.3.
      $.bracket_expr,
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

    // Uses token.immediate: `f(x)` is a function declaration head, but
    // `f (x)` (with a space) is NOT — the Rowan parser only recognises a
    // parameter list immediately adjacent to the name (mirrors
    // OPEN_PAREN_APPLY vs OPEN_PAREN in src/syntax/rowan/lex.rs). Without
    // this, a bare identifier that is the *value* of one declaration can be
    // misparsed as the head of the next declaration when that next
    // declaration starts with '(' on its own line, corrupting the parse.
    parameter_list: $ => seq(
      token.immediate('('),
      optional(seq(
        $._param,
        repeat(seq(',', $._param)),
        optional(','),
      )),
      ')',
    ),

    // Block destructuring pattern in parameter position: {x y} or {x: a  y: b}
    // A block_pattern has the same surface syntax as a block but only appears
    // as a function parameter.  Fields may be shorthand (`x` binds field x),
    // renamed (`x: a` binds field x as local variable a), or nested (`x: [a,
    // b]` / `x: {y}` binds field x by destructuring it further — audit
    // report §2.6, e.g. lib/prelude.eu's `{data: [a, b]}`-shaped params and
    // tests/harness/141_deep_destructuring.eu's `{outer: {inner}}`).
    block_pattern: $ => seq(
      '{',
      repeat(seq(
        choice($.block_pattern_field, $.identifier),  // bare identifier = shorthand binding
        optional(','),
      )),
      '}',
    ),

    // A renamed/nested block_pattern field: `name: param`, where `param` is
    // recursively any pattern (identifier, nested block/list/cons pattern),
    // not the generic declaration-value soup grammar — `{outer: {inner}}`'s
    // `{inner}` is only valid as a block_pattern (bare-identifier shorthand
    // binding), not as a general block expression.
    block_pattern_field: $ => seq(
      choice($.identifier, $.quoted_identifier),
      ':',
      $._param,
    ),

    // Fixed-length list destructuring pattern: [a, b, c], with each element
    // recursively any pattern — nested lists/cons/blocks (audit report
    // §2.6, e.g. `[a, [b, c]]`, `[a, [b: c]]`, `[{x y}, z]`).
    list_pattern: $ => seq(
      '[',
      seq(
        $._param,
        repeat(seq(',', $._param)),
        optional(','),
      ),
      ']',
    ),

    // Head/tail cons destructuring pattern: [h : t] or, with multiple
    // heads, [h1, h2 : t] (audit report §2.6's "multi-head cons pattern",
    // e.g. `[a, b : rest]`). Each head is recursively any pattern (e.g.
    // `[[a, b] : rest]`'s nested-list head); the tail is always a plain
    // binding name.
    // Uses ':' as the separator (not ',') to distinguish from list_pattern.
    cons_pattern: $ => seq(
      '[',
      $._param,
      repeat(seq(',', $._param)),
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
    // Callable targets mirror the Rowan parser's is_callable_terminal():
    // names, parenthesised expressions, blocks, and bracket expressions.
    application: $ => prec(2, seq(
      choice($.name, $.paren_expr, $.block, $.bracket_expr),
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
    // Callable targets mirror the Rowan parser's is_callable_terminal():
    // names, parenthesised expressions, and bracket expressions.
    block_application: $ => prec(2, seq(
      choice($.name, $.paren_expr, $.bracket_expr),
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
    // Callable targets mirror the Rowan parser's is_callable_terminal():
    // names, parenthesised expressions, and bracket expressions.
    list_application: $ => prec(2, seq(
      choice($.name, $.paren_expr, $.bracket_expr),
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
      $.t_string,
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

    // T-string: ZDT (zoned date-time) literal — no interpolation, no escape processing.
    // Examples: t"2024-03-15", t"2024-03-15T14:30:00Z", t"2024-03-15T14:30:00+05:00"
    // The entire token is opaque — just a prefix + quoted date/time string.
    t_string: $ => seq(
      't"',
      optional($.t_string_content),
      '"',
    ),

    t_string_content: $ => token.immediate(prec(1, /[^"]+/)),

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
    // - arbitrary expressions, including nested braces: row-polymorphic
    //   type-DSL syntax like s"symbol → Lens({..r}, a)" or doubly-nested
    //   block/list content like s"{ users: [{ name: string }] }" (audit
    //   report §2.7). These aren't simple references at all — `..r` isn't
    //   an identifier or dotted path, and the nested `{...}` needs the
    //   full `block`/`list` grammar, not a restricted interpolation-target
    //   mini-language. A general `$.soup` covers all of the above (a bare
    //   identifier or `foo.bar.baz` dotted path is just a soup of
    //   catenated names and `.` operators) as well as anything else that's
    //   a valid expression, so it replaces the narrower
    //   identifier/dotted_reference/number alternatives entirely.
    // Comma-separated, matching the row-polymorphic type-DSL idiom
    // `{..r, ..s}` (multiple row variables in one type expression, e.g.
    // s"{..r} → {..s} → {..r, ..s}" in lib/prelude.eu's merge() doc). A
    // bare `,` isn't part of `$.soup`'s own grammar (it's always used in
    // an explicit comma-separated-list context elsewhere, e.g.
    // argument_list), so it has to be threaded through here explicitly.
    interpolation_content: $ => choice(
      prec(1, $.anaphor),
      seq($.soup, repeat(seq(',', $.soup))),
    ),

    // A format spec is ':' followed by everything up to the interpolation's
    // closing '}' — normally a simple format string like ':.2f', but the
    // type-DSL idiom (s"{ users: [{ name: string }] }") abuses this same
    // "everything after the colon" slot to carry a whole nested
    // block/list type expression. A flat /:[^}]+/ regex can't tell an
    // OUTER closing '}' from an INNER one, so it stops at the first '}' it
    // sees — wrong as soon as the spec contains a nested `{...}` pair
    // (audit report §2.7). Made brace-aware via recursion instead: any
    // '{'...'}' pair nested inside the spec is matched structurally
    // (correctly balanced, arbitrarily deep) rather than by the regex.
    format_spec: $ => seq(
      ':',
      repeat(choice(
        /[^{}]+/,
        $._balanced_braces,
      )),
    ),

    _balanced_braces: $ => seq(
      '{',
      repeat(choice(
        /[^{}]+/,
        $._balanced_braces,
      )),
      '}',
    ),

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

    // Identifiers support Unicode letters including Latin Extended, Greek,
    // Cyrillic, and Letterlike Symbols (ℕ, ℝ, ℤ etc. — used as identifiers
    // in lib/prelude.eu itself, e.g. `ℕ: iota(0)`, and in examples/aoc25/;
    // audit report §2.9, a companion gap to eu-fbyk). The Latin Extended
    // range U+00C0-U+02AF is split to exclude × (U+00D7) and ÷ (U+00F7)
    // which are operator characters; Letterlike Symbols U+2100-U+214F
    // doesn't overlap OPER_CHARS (nearest operator range starts at
    // U+2190), so no such split is needed there.
    identifier: $ => /[•$?_a-zA-Z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02AF\u0370-\u03FF\u0400-\u04FF\u1F00-\u1FFF\u2100-\u214F][•$?_!*\-a-zA-Z0-9\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02AF\u0370-\u03FF\u0400-\u04FF\u1F00-\u1FFF\u2100-\u214F]*/,

    quoted_identifier: $ => seq(
      "'",
      /[^']+/,
      "'",
    ),

    // Operators: sequences of operator characters.
    // Bracket characters (⟦⟧⟨⟩⟪⟫⌈⌉⌊⌋) are deliberately excluded here;
    // they are matched by the bracket_expr rule via BRACKET_OPEN_RE/BRACKET_CLOSE_RE.
    // ‖ (U+2016 DOUBLE VERTICAL LINE) is included as the cons operator.
    // NB: this rule must stay in sync with OPER_CHARS above (single source of
    // truth) — a hand-duplicated copy previously drifted and silently dropped
    // ✓ ▶ ⊝, breaking real prelude syntax (eu-wrf7).
    operator: $ => token(prec(-1, OPER_CHARS)),

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
