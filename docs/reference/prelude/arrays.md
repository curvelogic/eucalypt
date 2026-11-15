# Vecs and Arrays

## Vecs

> Functions for working with vecs — flat primitive sequences with O(1) indexed access.

| Function | Description |
|----------|-------------|
| `vec.of` | Convert list `xs` of primitive values to a vec for O(1) indexed access |
| `vec.from-list` | Alias of `vec.of`: convert list `xs` of primitive values to a vec |
| `vec.len` | Return the number of elements in vec `v` |
| `vec.nth` | Return the element at index `n` (0-based) in vec `v` |
| `vec.slice` | Return a new vec with elements in the range `[from, to)` of `v` |
| `vec.sample(n, v, stream)` | Random monad action: pick `n` random elements from vec `v` without replacement; a negative `n` samples nothing; a non-integer `n` is an error |
| `vec.shuffle(v, stream)` | Random monad action: return vec `v` in random order |
| `vec.to-list` | Convert vec `v` back to a cons-list |

## N-dimensional Arrays

| Function | Description |
|----------|-------------|
| `arr.zeros` | Create an array of zeros with the given shape list |
| `arr.fill` | Create an array filled with `val` with the given shape list |
| `arr.from-flat` | Create an array from a flat list of numbers with the given shape |
| `arr.get` | Get element at coordinate list `coords` in array `a`. Pipeline: `a arr.get(coords)` |
| `arr.set` | Return new array with element at `coords` set to `val`. Pipeline: `a arr.set(coords, val)` |
| `arr.shape` | Return shape of array `a` as a list of integers |
| `arr.rank` | Return number of dimensions of array `a` |
| `arr.length` | Return total number of elements in array `a` |
| `arr.to-list` | Return flat list of elements in row-major order |
| `arr.array?` | True if `x` is an array |
| `arr.transpose` | Reverse all axes of array `a` |
| `arr.reshape` | Reshape array `a` to new shape (total elements must match). Pipeline: `a arr.reshape(shape)` |
| `arr.slice(axis, idx, a)` | Take a slice along `axis` at `idx`, reducing rank by 1. Pipeline: `a arr.slice(axis, idx)` |
| `arr.add` | Element-wise addition; `b` may be a scalar |
| `arr.sub` | Element-wise subtraction; `b` may be a scalar |
| `arr.mul` | Element-wise multiplication; `b` may be a scalar |
| `arr.div` | Element-wise division; `b` may be a scalar |
| `arr.indices` | Return list of coordinate lists for all elements in row-major order |
| `arr.map(f, a)` | Apply `f` to each element, return new array of same shape |
| `arr.map-indexed(f, a)` | Apply `f(coords, val)` to each element, return new array of same shape |
| `arr.fold(f, init, a)` | Left-fold `f` over all elements of `a` in row-major order |
| `arr.neighbours(coords, offsets, a)` | Return list of values at valid neighbouring coordinates. Pipeline: `a arr.neighbours(coords, offsets)`. `offsets` is a list of offset lists (one per neighbour direction). Out-of-bounds neighbours are omitted |
| `arr.unit-arrays(shape)` | List of arrays with a single 1 at each coordinate position |
| `is-array?(x)` | True if `x` is an n-dimensional array |
