# N-Dimensional Arrays

Eucalypt provides a native n-dimensional array type (also called tensors)
backed by a flat contiguous store with shape metadata. Arrays enable
efficient grid and matrix operations, in particular for Advent of Code-style
grid simulations where list-based approaches are too slow.

Arrays are an immutable, purely functional data structure. All mutation
operations return new arrays.

## Construction

| Function | Description |
|----------|-------------|
| `arr.zeros(shape)` | Create an array of zeros with the given shape list |
| `arr.fill(shape, val)` | Create an array filled with `val` with the given shape list |
| `arr.from-flat(shape, vals)` | Create an array from a flat list of numbers with the given shape |

The `shape` argument is always a list of integers, e.g. `[3]` for a 1D
array of 3 elements or `[2, 3]` for a 2×3 matrix.

## Access and Query

| Function | Description |
|----------|-------------|
| `arr.get(a, coords)` | Get element at coordinate list `coords` in array `a` |
| `arr.set(a, coords, val)` | Return new array with element at `coords` set to `val` |
| `arr.shape(a)` | Return shape of array `a` as a list of integers |
| `arr.rank(a)` | Return number of dimensions of array `a` |
| `arr.length(a)` | Return total number of elements in array `a` |
| `arr.to-list(a)` | Return flat list of elements in row-major order |
| `arr.array?(x)` | `true` if `x` is an n-dimensional array |
| `is-array?(x)` | `true` if `x` is an n-dimensional array (alias) |

The `coords` argument to `arr.get` and `arr.set` is a list of integers,
one per dimension, e.g. `[row, col]` for a 2D array.

### The `!!` Operator

The indexing operator `!!` is overloaded for arrays. When the left operand
is an array, `!!` delegates to `arr.get`:

```eu
my-2d-array !! [row, col]   # same as arr.get(my-2d-array, [row, col])
my-1d-array !! [idx]        # same as arr.get(my-1d-array, [idx])
```

For plain lists, `!!` retains its original list-index behaviour:

```eu
[10, 20, 30] !! 1   # => 20
```

## Transformations

| Function | Description |
|----------|-------------|
| `arr.transpose(a)` | Reverse all axes of array `a` |
| `arr.reshape(a, shape)` | Reshape array `a` to new shape (total elements must match) |
| `arr.slice(a, axis, idx)` | Take a slice along `axis` at `idx`, reducing rank by 1 |

## Arithmetic

Array-specific arithmetic (explicit namespace):

| Function | Description |
|----------|-------------|
| `arr.add(a, b)` | Element-wise addition; `b` may be a scalar |
| `arr.sub(a, b)` | Element-wise subtraction; `b` may be a scalar |
| `arr.mul(a, b)` | Element-wise multiplication; `b` may be a scalar |
| `arr.div(a, b)` | Element-wise division; `b` may be a scalar |

The standard arithmetic operators `+`, `-`, `*`, `/` are polymorphic:
when either operand is an array, the operation is applied element-wise.

```eu,notest
a: arr.from-flat([3], [1, 2, 3])
b: arr.from-flat([3], [10, 20, 30])
c: a + b          # element-wise: [11, 22, 33]
d: a * 2          # scalar broadcast: [2, 4, 6]
```

Note: `/` on arrays performs element-wise float division (not floor
division as it does for plain integers).

## Higher-order Operations

| Function | Description |
|----------|-------------|
| `arr.indices(a)` | Return list of coordinate lists for every element, in row-major order |
| `arr.map(f, a)` | Apply `f` to each element; return new array of same shape |
| `arr.map-indexed(f, a)` | Apply `f(coords, val)` to each element; return new array of same shape |
| `arr.fold(f, init, a)` | Left-fold `f` over all elements in row-major order, starting from `init` |
| `arr.neighbours(a, coords, offsets)` | Return list of values at valid in-bounds neighbours of `coords`, given a list of offset vectors |

`arr.indices` returns coordinates as lists; for a 2D array of shape `[rows, cols]`, each entry is `[row, col]`.

`arr.neighbours` silently skips any out-of-bounds coordinates, so it is safe to call on border elements without special-casing.

```eu
# Double every element of a 1D array
a: arr.from-flat([3], [1, 2, 3])
b: arr.map((_ * 2), a)   # => [2, 4, 6] (same shape)

# Sum all elements
total: arr.fold((+), 0, a)   # => 6

# List all coordinates of a 2×2 array
coords: arr.from-flat([2, 2], [0, 0, 0, 0]) arr.indices
# => [[0, 0], [0, 1], [1, 0], [1, 1]]

# Neighbours of centre cell in a 3×3 grid (4-connected)
grid: arr.from-flat([3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9])
ns: arr.neighbours(grid, [1, 1], [[-1, 0], [1, 0], [0, -1], [0, 1]])
# => [2, 8, 4, 6]
```

## Example

```eu
# 3×3 grid initialised to zero
grid: arr.zeros([3, 3])

# Set a value at row 1, col 2
grid2: grid arr.set([1, 2], 42)

# Read back the value
val: grid2 arr.get([1, 2])    # => 42
val2: grid2 !! [1, 2]         # same thing via !! operator

# Compute element-wise sum of two grids
a: arr.from-flat([2, 2], [1, 2, 3, 4])
b: arr.from-flat([2, 2], [5, 6, 7, 8])
c: a + b   # => [[6, 8], [10, 12]] (as flat list: [6, 8, 10, 12])
```
