# Numbers and Arithmetic

## Arithmetic Operators

| Function | Description |
|----------|-------------|
| `(l ∣ r)` | `true` if `l` divides `r`, i.e. `r % l = 0`; `3 ∣ 12` is `true`. Note `∣` is U+2223 (divides), not `\|` |
| `(l ∤ r)` | `true` if `l` does not divide `r`, i.e. `not(l ∣ r)`; `5 ∤ 12` is `true` |
| `(∸ x)` | Unary minus; negate |

## Numeric Functions

| Function | Description |
|----------|-------------|
| `inc` | Increment number `x` by 1 |
| `dec` | Decrement number `x` by 1 |
| `negate` | Negate number `n` |
| `abs(n)` | Absolute value of number `n` |
| `zero?` | Return true if and only if number `n` is 0 |
| `pos?` | Return true if and only if number `n` is strictly positive |
| `neg?` | Return true if and only if number `n` is strictly negative |
| `num` | Parse number from string |
| `floor` | Round number downwards to nearest integer |
| `ceiling` | Round number upwards to nearest integer |
| `⌈⌉` | Ceiling bracket notation, round `n` upwards to nearest integer |
| `⌊⌋` | Floor bracket notation, round `n` downwards to nearest integer |
| `pow(b, e)` | Raise `b` to the power `e` |
| `div(a, b)` | Floor division; same as `a / b` |
| `mod(a, b)` | Floor modulus; same as `a % b` |
| `quot(a, b)` | Truncation division; rounds toward zero |
| `rem(a, b)` | Truncation remainder; result has same sign as dividend |
| `mulmod(a, b, m)` | `a * b` modulo `m`, exact for all 64-bit signed integers (computed in 128 bits); result in `[0, m)` |
| `powmod(b, e, m)` | `b` raised to the power `e` modulo `m`, exact for all 64-bit signed integers; `e` must be non-negative; result in `[0, m)` |
| `modinv(a, m)` | The inverse of `a` modulo `m`: `x` in `[0, m)` with `mulmod(a, x, m) = 1 % m` (so `modinv(a, 1)` is 0); an error when `a` and `m` are not coprime |
| `crt(congruences)` | Chinese remainder theorem: given `[[r, m], ...]`, the least non-negative `x` with `x ≡ r (mod m)` for every pair; the moduli must be pairwise coprime (otherwise an error from `modinv`) and the running product of the moduli must stay within 64 bits (past 2^63 it is a `numeric overflow` error, even if the answer would fit). Remainders are normalised; the empty list gives 0 |
| `sum(l)` | Sum a list of numbers |
| `product(l)` | Multiply a list of numbers |
| `max(l, r)` | Return max of `l` and `r` by `>` |
| `max-of(l)` | `max-of(l) - return max element in list of numbers `l` - error if empty` |
| `max-of-by(f, l)` | Return the element of `l` that maximises `f`. Error if empty |
| `max-map(f, l)` | Return maximum value of `f(x)` across elements of `l` |
| `max-of-or(d, l)` | Return max element in list `l`, or default `d` if empty |
| `min(l, r)` | Return min of `l` and `r` by `<` |
| `min-of(l)` | `min-of(l) - return min element in list of numbers `l` - error if empty` |
| `min-of-by(f, l)` | Return the element of `l` that minimises `f`. Error if empty |
| `min-map(f, l)` | Return minimum value of `f(x)` across elements of `l` |
| `min-of-or(d, l)` | Return min element in list `l`, or default `d` if empty |
