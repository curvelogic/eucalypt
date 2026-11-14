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
| `dict.keys` | Return the keys of dict `d` in key order |
| `dict.values` | Return the values of dict `d` in key order |
| `dict.min` | Return `[key, value]` for the least key of dict `d`; an error if `d` is empty |
| `dict.max` | Return `[key, value]` for the greatest key of dict `d`; an error if `d` is empty |
| `dict.pop-min` | Return `[key, value, rest]` for the least key of dict `d`, where `rest` is `d` without it; an error if `d` is empty. With keys `[priority, item]` this makes a dict a priority queue |
| `dict.pop-max` | Return `[key, value, rest]` for the greatest key of dict `d`, where `rest` is `d` without it; an error if `d` is empty |
| `dict.range` | Return the dict of the entries of dict `d` whose keys `k` satisfy `lo ≤ k < hi` |
| `dict.update` | Return dict `d` with key `k` mapped to the lazy value `f(old)`, where `old` is the value at `k`, or `default` if `d` does not hold `k` |
| `dict.update-strict` | As `dict.update`, but `f(old)` is evaluated before it is stored, so repeated updates (such as counting) do not build up unevaluated values |
| `dict.merge-with` | Return the union of dicts `a` and `b`. Where both hold a key, the value is the lazy `f(va, vb)`, with `a`'s value first, and `a`'s key is kept |
| `dict.merge-with-strict(f, a, b)` | As `dict.merge-with`, but each combined value `f(va, vb)` is evaluated before it is stored |
| `dict.from-block(b)` | Create a dict from block `b`, keyed by its symbol keys. Values stay lazy |
| `dict.to-block` | Return a block of the entries of dict `d` in key order. Symbol keys are kept, string keys become symbols, and number keys become symbols of their rendered text. A list key, or two keys that become the same symbol (a string and a symbol with the same text, say), is an error |
