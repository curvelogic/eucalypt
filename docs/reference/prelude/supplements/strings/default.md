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
