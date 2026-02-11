# Booleans and Comparison

## Constants

- `true` -- Boolean true
- `false` -- Boolean false
- `null` -- Null value (exports as `null` in JSON, `~` in YAML)
- `nil` -- Empty list `[]`

## Control Flow

| Function | Description |
|----------|-------------|
| `if(c, t, f)` | If `c` is true return `t`, else `f` |
| `then(t, f, c)` | Pipeline-friendly if: `x? then(t, f)` |
| `when(p?, f, x)` | When `x` satisfies `p?`, apply `f`, else pass through |
| `cond(l, d)` | Select first true condition from list of `[condition, value]` pairs, else default `d` |

## Error Handling

| Function | Description |
|----------|-------------|
| `panic(s)` | Raise runtime error with message `s` |
| `assert(c, s, v)` | If `c` is true return `v`, else error with message `s` |

## Boolean Functions

| Function | Description |
|----------|-------------|
| `not(b)` | Toggle boolean |
| `and(l, r)` | Logical and |
| `or(l, r)` | Logical or |

## Boolean Operators

| Operator | Description |
|----------|-------------|
| `!x` or `¬x` | Not (prefix) |
| `l && r` or `l ∧ r` | And |
| `l \|\| r` or `l ∨ r` | Or |

## Equality and Comparison

| Operator | Description |
|----------|-------------|
| `l = r` | Equality |
| `l != r` | Inequality |
| `l < r` | Less than |
| `l > r` | Greater than |
| `l <= r` | Less than or equal |
| `l >= r` | Greater than or equal |
