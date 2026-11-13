# Dicts

## Dict Operations

> Functions for working with dicts — persistent dictionaries ordered by key. Keys are numbers, strings, symbols or lists of those; values are arbitrary and lazy.

| Function | Description |
|----------|-------------|
| `dict.of` | Create a dict from list `pairs` of `[key, value]` pairs. Keys are evaluated fully; values stay lazy. Where keys are equal (`1` and `1.0` are equal keys), the first key and the last value are kept |
| `dict.from-list` | Alias of `dict.of` |
| `dict.empty` | The empty dict |
| `dict.to-list` | Return the entries of dict `d` as `[key, value]` pairs in key order |
| `dict.get` | Return the value at key `k` in dict `d`; an error naming the key if `d` does not hold it |
| `dict.get-or` | Return the value at key `k` in dict `d`, or `default` if `d` does not hold it |
| `dict.assoc` | Return dict `d` with key `k` mapped to the lazy value `v`. If `d` holds an equal key, that key is kept and only its value replaced |
| `dict.dissoc` | Return dict `d` without key `k` |
| `dict.contains?` | True if dict `d` holds key `k` |
| `dict.size` | Return the number of entries in dict `d` |
| `dict.empty?(d)` | True if dict `d` has no entries |
