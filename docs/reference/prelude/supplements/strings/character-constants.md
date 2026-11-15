The `ch` namespace provides special characters:

- `ch.n` -- Newline
- `ch.t` -- Tab
- `ch.dq` -- Double quote

`ch.code-point(c)` gives the Unicode code point of a one-character string and
`ch.of-code-point(n)` is its inverse; see
[Code Points](#code-points) for the string-level `str.code-points` and
`str.from-code-points`.
