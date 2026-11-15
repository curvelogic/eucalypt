# Bitwise Operations

## Bit Functions

> Bitwise utility functions for integer bit manipulation.

| Function | Description |
|----------|-------------|
| `bit.and(l, r)` | Bitwise AND of integers `l` and `r` |
| `bit.or(l, r)` | Bitwise OR of integers `l` and `r` |
| `bit.xor(l, r)` | Bitwise XOR of integers `l` and `r` |
| `bit.not(n)` | Bitwise NOT of integer `n` |
| `bit.shl(n, k)` | Left shift `n` by `k` bits |
| `bit.shr(n, k)` | Arithmetic right shift `n` by `k` bits |
| `bit.popcount(n)` | Count set bits in integer `n` |
| `bit.ctz(n)` | Count trailing zeros in integer `n` (position of lowest set bit). Returns 64 if n is 0 |
| `bit.clz(n)` | Count leading zeros in integer `n` |
