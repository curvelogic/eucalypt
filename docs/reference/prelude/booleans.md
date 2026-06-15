# Booleans and Comparison

## Essentials

| Function | Description |
|----------|-------------|
| `null` | A null value. To export as `null` in JSON or ~ in YAML |
| `true` | Constant logical true |
| `false` | Constant logical false |
| `if` | If `c` is `true`, return `t` else `f` |
| `then(t, f, c)` | For pipeline if: `x? then(t, f)` |
| `when(p?, f, x)` | When `x` satisfies `p?` apply `f` else pass through unchanged |

## Error and Debug Support

| Function | Description |
|----------|-------------|
| `panic` | Raise runtime error with message string `s` |

## Boolean Logic

| Function | Description |
|----------|-------------|
| `not` | Toggle boolean |
| `(! x)` | Not x, toggle boolean |
| `(¬ x)` | Not x, toggle boolean |
| `and` | True if and only if `l` and `r` are true |
| `or` | True if and only if `l` or `r` is true |
