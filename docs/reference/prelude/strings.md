# Strings

## String Processing

> String processing functions

| Function | Description |
|----------|-------------|
| `str.of` | Convert `e` to string |
| `str.split` | Split string `s` on separators matching regex `re` |
| `str.split-on` | Split string `s` on separators matching regex `re` |
| `str.join` | Join list of strings `l` by interposing string s |
| `str.join-on` | Join list of strings `l` by interposing string s |
| `str.match` | Match string `s` using regex `re`, return list of full match then capture groups |
| `str.match-with` | Match string `s` using regex `re`, return list of full match then capture groups |
| `str.extract(re)` | Use regex `re` (with single capture) to extract substring of s - or error |
| `str.extract-or(re, d, s)` | Use regex `re` (with single capture) to extract substring of `s` - or default `d` |
| `str.matches` | Return list of all matches in string `s` of regex `re` |
| `str.matches-of` | Return list of all matches in string `s` of regex `re` |
| `str.matches?(re, s)` | Return true if `re` matches full string `s` |
| `str.suffix(b, a)` | Return string `b` suffixed onto `a` |
| `str.prefix(b, a)` | Return string `b` prefixed onto `a` |
| `str.letters` | Return individual letters of `s` as list of strings |
| `str.len` | Return length of string in characters |
| `str.format(spec, x)` | Format `x` using printf-style format `spec` (`%d`, `%x`, `%X`, `%o`, `%b`, `%f`, `%e`, `%s`, with width and flags). Pipeline-friendly: `255 str.format` with a spec argument |
| `str.fmt` | DEPRECATED: use `str.format(spec, x)`, which takes the spec first so it works in pipelines |
| `str.to-upper` | Convert string `s` to upper case |
| `str.to-lower` | Convert string `s` to lower case |
| `str.lt(a, b)` | True if string `a` is lexicographically less than `b` |
| `str.gt(a, b)` | True if string `a` is lexicographically greater than `b` |
| `str.lte(a, b)` | True if string `a` is lexicographically less than or equal to `b` |
| `str.gte(a, b)` | True if string `a` is lexicographically greater than or equal to `b` |
| `str.base64-encode` | Encode string `s` as base64 |
| `str.base64-decode` | Decode base64 string `s` back to its original string |
| `str.sha256` | Return the SHA-256 hash of string `s` as lowercase hex |
| `str.md5` | Return the MD5 hash of string `s` (its UTF-8 bytes) as lowercase hex. MD5 is not secure: use it only to match existing checksums |
| `str.code-points` | The Unicode code points of the characters of string `s` (one per character, as `str.letters` splits it), as a list of numbers |
| `str.from-code-points` | The string whose characters have the Unicode code points `ns`; the inverse of `str.code-points`. An error for an element that is not a whole number in 0 to 0x10FFFF or is a surrogate (0xD800 to 0xDFFF) |
| `str.replace(pattern, replacement, s)` | Replace all occurrences of regex `pattern` with `replacement` in string `s`. Pipeline: s str.replace(pat, rep) |
| `str.contains?(pattern, s)` | True if string `s` contains a match for regex `pattern`. Pipeline: s str.contains?(pat) |
| `str.trim` | Trim leading and trailing whitespace from string `s` |
| `str.starts-with?(re, s)` | True if string `s` starts with a match for regex `re` |
| `str.ends-with?(re, s)` | True if string `s` ends with a match for regex `re` |
| `str.shell-escape(s)` | Wrap string in single quotes for safe shell use. Embedded single quotes are escaped |
| `str.dq-escape(s)` | Escape string for use inside double quotes in shell commands. Escapes backslash, dollar, backtick, and double quote |

## Character Constants

> We don't support the usual string escapes so these make it
a little more convenient to use interpolation instead..

| Function | Description |
|----------|-------------|
| `ch.n` | A newline character, for interpolation |
| `ch.t` | A tab character, for interpolation |
| `ch.dq` | A double-quote character, for interpolation |
| `ch.code-point` | The Unicode code point of the single character `c`, as a number; an error unless `c` is exactly one character |
| `ch.of-code-point` | The one-character string with Unicode code point `n`; an error unless `n` is a whole number in 0 to 0x10FFFF and not a surrogate (0xD800 to 0xDFFF) |

The `ch` namespace provides special characters:

- `ch.n` -- Newline
- `ch.t` -- Tab
- `ch.dq` -- Double quote

`ch.code-point(c)` gives the Unicode code point of a one-character string and
`ch.of-code-point(n)` is its inverse; see
[Code Points](#code-points) for the string-level `str.code-points` and
`str.from-code-points`.

### Encoding and Hashing Examples

```eu
encoded: "hello" str.base64-encode    # "aGVsbG8="
decoded: "aGVsbG8=" str.base64-decode # "hello"
hash: "hello" str.sha256              # "2cf24dba5fb0a30e..."
legacy: "abc" str.md5                 # "900150983cd24fb0d6963f7d28e17f72"
```

`str.md5` hashes the UTF-8 bytes of the string and returns 32 lowercase hex
digits. MD5 is not secure; use it only to match an existing checksum.

### Code Points

`str.code-points` and `str.from-code-points` convert between a string and the
list of Unicode code points of its characters (one number per character, as
`str.letters` splits it, so `"é"` is `[233]`, not two bytes). `ch.code-point`
and `ch.of-code-point` do the same for a single character.

```eu
points:  "aé😀" str.code-points                    # [97, 233, 128512]
text:    [104, 105] str.from-code-points           # "hi"
shifted: "abc" str.code-points map(+ 1) str.from-code-points  # "bcd"
one:     "é" ch.code-point                         # 233
char:    128512 ch.of-code-point                   # "😀"
```

`str.from-code-points` and `ch.of-code-point` stop with an error for a number
that is not a whole number from 0 to 0x10FFFF, or that is a surrogate (0xD800
to 0xDFFF); `ch.code-point` stops unless its argument is exactly one
character.
