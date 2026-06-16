# Numbers and Arithmetic

## Arithmetic Operators

| Function | Description |
|----------|-------------|
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
| `sum(l)` | Sum a list of numbers |
| `product(l)` | Multiply a list of numbers |
| `max(l, r)` | Return max of `l` and `r` by `>` |
| `max-of(l)` | `max-of(l) - return max element in list of numbers `l` - error if empty` |
| `max-map(f, l)` | Return maximum value of `f(x)` across elements of `l` |
| `max-of-or(d, l)` | Return max element in list `l`, or default `d` if empty |
| `min(l, r)` | Return min of `l` and `r` by `<` |
| `min-of(l)` | `min-of(l) - return min element in list of numbers `l` - error if empty` |
| `min-map(f, l)` | Return minimum value of `f(x)` across elements of `l` |
| `min-of-or(d, l)` | Return min element in list `l`, or default `d` if empty |
