# Strings

The `str` namespace contains string functions:

| Function | Description |
|----------|-------------|
| `str.of(e)` | Convert to string |
| `str.split(s, re)` | Split string on regex |
| `str.split-on(re, s)` | Split (pipeline-friendly) |
| `str.join(l, s)` | Join list with separator |
| `str.join-on(s, l)` | Join (pipeline-friendly) |
| `str.match(s, re)` | Match regex, return captures |
| `str.match-with(re, s)` | Match (pipeline-friendly) |
| `str.matches(s, re)` | All matches of regex |
| `str.matches-of(re, s)` | All matches (pipeline-friendly) |
| `str.matches?(re, s)` | True if regex matches full string |
| `str.extract(re, s)` | Extract single capture |
| `str.extract-or(re, d, s)` | Extract with default |
| `str.suffix(b, a)` | Suffix `b` onto `a` |
| `str.prefix(b, a)` | Prefix `b` onto `a` |
| `str.letters(s)` | List of characters |
| `str.len(s)` | String length |
| `str.fmt(x, spec)` | Printf-style formatting |
| `str.to-upper(s)` | Convert to upper case |
| `str.to-lower(s)` | Convert to lower case |

## Encoding and Hashing

| Function | Description |
|----------|-------------|
| `str.base64-encode(s)` | Encode string `s` as base64 |
| `str.base64-decode(s)` | Decode base64 string `s` |
| `str.sha256(s)` | SHA-256 hash of string `s` as lowercase hex |

```eu
encoded: "hello" str.base64-encode    # "aGVsbG8="
decoded: "aGVsbG8=" str.base64-decode # "hello"
hash: "hello" str.sha256              # "2cf24dba5fb0a30e..."
```

## Character Constants

The `ch` namespace provides special characters:

- `ch.n` -- Newline
- `ch.t` -- Tab
- `ch.dq` -- Double quote
