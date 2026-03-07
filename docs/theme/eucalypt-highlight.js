// highlight.js language definition for Eucalypt
// Registered as both 'eu' and 'eucalypt'
(function() {
  function eucalyptLanguage(hljs) {
    var COMMENT = hljs.COMMENT('#', '$');

    var STRING = {
      className: 'string',
      variants: [
        // C-strings with escape sequences
        {
          begin: 'c"',
          end: '"',
          contains: [
            { className: 'subst', begin: /(?<!\{)\{(?!\{)/, end: /\}/ },
            {
              className: 'char.escape',
              begin: /\\[nrtv0\\"afbe]|\\x[0-9a-fA-F]{2}|\\u\{[0-9a-fA-F]+\}/
            }
          ]
        },
        // Raw strings
        {
          begin: 'r"',
          end: '"',
          contains: [
            { className: 'subst', begin: /(?<!\{)\{(?!\{)/, end: /\}/ }
          ]
        },
        // Regular double-quoted strings with interpolation
        {
          begin: '"',
          end: '"',
          contains: [
            { className: 'subst', begin: /(?<!\{)\{(?!\{)/, end: /\}/ }
          ]
        }
      ]
    };

    var NUMBER = {
      className: 'number',
      variants: [
        { begin: /\b0[xX][0-9a-fA-F]+\b/ },
        { begin: /\b0[oO][0-7]+\b/ },
        { begin: /\b0[bB][01]+\b/ },
        { begin: /\b-?[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?\b/ }
      ]
    };

    var SYMBOL = {
      className: 'symbol',
      begin: /:[a-zA-Z_\u00C0-\u024F][a-zA-Z0-9_?!\-]*/
    };

    var METADATA = {
      className: 'meta',
      begin: /`/
    };

    return {
      name: 'Eucalypt',
      aliases: ['eu', 'eucalypt'],
      case_insensitive: false,
      keywords: {
        keyword: 'if then when cond import',
        literal: 'true false null nil'
      },
      contains: [
        COMMENT,
        STRING,
        NUMBER,
        SYMBOL,
        METADATA,
        {
          // Function declarations: name(params):
          className: 'title.function',
          begin: /[a-zA-Z_][a-zA-Z0-9_?!\-]*(?=\s*\()/
        },
        {
          // Property declarations: name:
          className: 'attr',
          begin: /[a-zA-Z_][a-zA-Z0-9_?!\-]*(?=\s*:)/
        },
        {
          // Namespace-qualified names: str.of, io.env, cal.parse
          className: 'built_in',
          begin: /\b(?:str|io|eu|cal|set|ch)\.[a-zA-Z_][a-zA-Z0-9_?!\-]*/
        },
        {
          // Common prelude functions
          className: 'built_in',
          begin: /\b(?:map|filter|head|tail|cons|reverse|append|concat|foldl|foldr|take|drop|zip|range|repeat|count|merge|lookup|keys|values|elements|identity|compose|flip|not|and|or|qsort|group-by|mapcat|zip-with|block|pair|deep-merge|map-values|map-keys|filter-items|inc|dec|negate|num|floor|ceiling|max|min|with-meta|meta|panic|assert)\b/
        },
        {
          // Assertion operators
          className: 'operator',
          begin: /\/\/=>|\/\/=\?|\/\/!\?|\/\/!{1,2}|\/\/=|\/\/<</
        },
        {
          // Anaphora
          className: 'variable.language',
          begin: /\b_[0-9]*\b|•[0-9]*/
        },
        {
          // Quoted identifiers
          className: 'string',
          begin: /'/,
          end: /'/
        }
      ]
    };
  }

  // Register with highlight.js
  if (typeof hljs !== 'undefined') {
    hljs.registerLanguage('eu', eucalyptLanguage);
    hljs.registerLanguage('eucalypt', eucalyptLanguage);

    // Re-highlight all eu code blocks that were missed on initial pass
    document.querySelectorAll('code.language-eu, code.language-eucalypt').forEach(function(block) {
      hljs.highlightBlock(block);
    });
  }
})();
