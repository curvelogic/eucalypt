# Strings

## String Processing

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
| `str.fmt` | Format `x` using printf-style format `spec` |
| `str.to-upper` | Convert string `s` to upper case |
| `str.to-lower` | Convert string `s` to lower case |
| `str.lt(a, b)` | True if string `a` is lexicographically less than `b` |
| `str.gt(a, b)` | True if string `a` is lexicographically greater than `b` |
| `str.lte(a, b)` | True if string `a` is lexicographically less than or equal to `b` |
| `str.gte(a, b)` | True if string `a` is lexicographically greater than or equal to `b` |
| `str.replace(pattern, replacement, s)` | Replace all matches of regex `pattern` with `replacement` in `s` |
| `str.contains?(pattern, s)` | True if `s` contains a match for regex `pattern` |
| `str.trim` | Trim leading and trailing whitespace |
| `str.starts-with?(re, s)` | True if `s` starts with a match for regex `re` |
| `str.ends-with?(re, s)` | True if `s` ends with a match for regex `re` |
| `str.shell-escape(s)` | Wrap in single quotes for safe shell use, escaping embedded `'` |
| `str.dq-escape(s)` | Escape `$`, `` ` ``, `"`, `\` for use inside double quotes |
| `str.base64-encode` | Encode string `s` as base64 |
| `str.base64-decode` | Decode base64 string `s` back to its original string |
| `str.sha256` | Return the SHA-256 hash of string `s` as lowercase hex |

## Character Constants

The `ch` namespace provides special characters:

- `ch.n` -- Newline
- `ch.t` -- Tab
- `ch.dq` -- Double quote

### Encoding and Hashing Examples

```eu
encoded: "hello" str.base64-encode    # "aGVsbG8="
decoded: "aGVsbG8=" str.base64-decode # "hello"
hash: "hello" str.sha256              # "2cf24dba5fb0a30e..."
```

## Serialisation

Serialise eucalypt values to strings using a named output format.  These
are pure functions — no IO is required.

| Function | Description |
|----------|-------------|
| `render(value)` | Serialise `value` to a YAML string |
| `render-as(fmt, value)` | Serialise `value` to a string in format `fmt` |

Supported formats for `fmt`: `:yaml`, `:json`, `:toml`, `:text`, `:edn`, `:html`.

### Serialisation Examples

```eu,notest
yaml-str: render({a: 1, b: 2})           # "a: 1\nb: 2\n"
json-str: render-as(:json, {a: 1, b: 2}) # "{\"a\":1,\"b\":2}"
```

These functions are backed by the `RENDER_TO_STRING` intrinsic, which
traverses the evaluated heap value and serialises it using the same
emitter pipeline as normal output.

## Parsing

Parse a string of structured data back into eucalypt data.  This is the
inverse of `render-as` and is a pure function — no IO is required.

| Function | Description |
|----------|-------------|
| `parse-as(fmt, str)` | Parse `str` as structured data in format `fmt` |

Supported formats for `fmt`: `:json`, `:yaml`, `:toml`, `:csv`, `:xml`,
`:edn`, `:jsonl`.

`:json` and `:yaml` share the same parser.

**Safety**: `parse-as` always uses data-only mode.  YAML `!eu` tags and
other embedded-code constructs are returned as plain string values and
never evaluated.  It is safe to parse untrusted input (e.g. shell command
output) with this function.

### Parsing Examples

```eu,notest
# Parse JSON
data: "{\"x\": 1}" parse-as(:json)
data.x  # 1

# Round-trip
original: {x: 1, y: 2}
recovered: render-as(:json, original) parse-as(:json)
recovered.x  # 1

# Pipeline style (parse-as with first arg partially applied)
{ :io r: io.shell("kubectl get configmap foo -o json") }.r.stdout
  parse-as(:json)
```

`parse-as` is backed by the `PARSE_STRING` intrinsic.
