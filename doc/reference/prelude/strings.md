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
