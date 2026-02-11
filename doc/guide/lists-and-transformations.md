# Lists and Transformations

In this chapter you will learn:

- How to create and deconstruct lists
- The core list operations: `map`, `filter`, `foldl`, `foldr`
- Other useful list functions from the prelude
- How to combine list operations into pipelines

## Creating Lists

Lists are written with square brackets and commas:

```eu
numbers: [1, 2, 3, 4, 5]
strings: ["hello", "world"]
empty: []
nested: [[1, 2], [3, 4]]
```

## Basic List Operations

### `head` and `tail`

`head` returns the first element; `tail` returns everything after it:

```sh
eu -e '[10, 20, 30] head'
```

```yaml
10
```

```sh
eu -e '[10, 20, 30] tail'
```

```yaml
- 20
- 30
```

Use `head-or` to provide a default for empty lists:

```sh
eu -e '[] head-or(0)'
```

```yaml
0
```

### `first` and `second`

`first` is an alias for `head`. `second` returns the second element:

```sh
eu -e '[:a, :b, :c] second'
```

```yaml
b
```

### `cons`

`cons` prepends an element to a list:

```sh
eu -e 'cons(0, [1, 2, 3])'
```

```yaml
- 0
- 1
- 2
- 3
```

### `nil?`

Test whether a list is empty:

```sh
eu -e '[] nil?'
```

```yaml
true
```

### `count`

Count the elements:

```sh
eu -e '[10, 20, 30] count'
```

```yaml
3
```

## Transforming Lists

### `map`

Apply a function to every element:

```sh
eu -e '[1, 2, 3] map(inc)'
```

```yaml
- 2
- 3
- 4
```

```sh
eu -e '[1, 2, 3] map(* 10)'
```

```yaml
- 10
- 20
- 30
```

### `filter`

Keep only elements satisfying a predicate:

```sh
eu -e '[1, 2, 3, 4, 5, 6] filter(> 3)'
```

```yaml
- 4
- 5
- 6
```

### `remove`

The opposite of `filter` -- remove elements satisfying the predicate:

```sh
eu -e '[1, 2, 3, 4, 5] remove(> 3)'
```

```yaml
- 1
- 2
- 3
```

### The Functor Operator `<$>`

The `<$>` operator is an alias for `map`:

```sh
eu -e 'inc <$> [1, 2, 3]'
```

```yaml
- 2
- 3
- 4
```

## Folding

Folds reduce a list to a single value by applying a binary function
across all elements.

### `foldl`

Left fold: `foldl(op, init, list)` applies `op` from the left:

```sh
eu -e 'foldl(+, 0, [1, 2, 3, 4, 5])'
```

```yaml
15
```

### `foldr`

Right fold: `foldr(op, init, list)` applies `op` from the right:

```sh
eu -e 'foldr(++, [], [[1, 2], [3, 4], [5]])'
```

```yaml
- 1
- 2
- 3
- 4
- 5
```

## Slicing

### `take` and `drop`

```sh
eu -e '[1, 2, 3, 4, 5] take(3)'
```

```yaml
- 1
- 2
- 3
```

```sh
eu -e '[1, 2, 3, 4, 5] drop(3)'
```

```yaml
- 4
- 5
```

### `take-while` and `drop-while`

```sh
eu -e '[1, 2, 3, 4, 5] take-while(< 4)'
```

```yaml
- 1
- 2
- 3
```

## Combining Lists

### `append` and `++`

```sh
eu -e '[1, 2] ++ [3, 4]'
```

```yaml
- 1
- 2
- 3
- 4
```

### `concat`

Flatten a list of lists:

```sh
eu -e 'concat([[1, 2], [3], [4, 5]])'
```

```yaml
- 1
- 2
- 3
- 4
- 5
```

### `mapcat`

Map then flatten (also known as `flatMap` or `concatMap`):

```sh
eu -e '["ab", "cd"] mapcat(str.letters)'
```

```yaml
- a
- b
- c
- d
```

## Checking Lists

### `all-true?` and `any-true?`

```sh
eu -e '[true, true, false] all-true?'
```

```yaml
false
```

```sh
eu -e '[true, true, false] any-true?'
```

```yaml
true
```

### `all` and `any`

Test with a predicate:

```sh
eu -e '[2, 4, 6] all(> 0)'
```

```yaml
true
```

```sh
eu -e '[1, 2, 3] any(zero?)'
```

```yaml
false
```

## Reordering

### `reverse`

```sh
eu -e '[:a, :b, :c] reverse'
```

```yaml
- c
- b
- a
```

### `zip-with`

Combine two lists element by element:

```sh
eu -e 'zip-with(+, [1, 2, 3], [10, 20, 30])'
```

```yaml
- 11
- 22
- 33
```

### `zip-with` and `pair` to create blocks

```sh
eu -e 'zip-with(pair, [:x, :y, :z], [1, 2, 3]) block'
```

```yaml
x: 1
y: 2
z: 3
```

## Infinite Lists

Eucalypt supports lazy evaluation, so you can work with infinite lists:

```sh
eu -e 'repeat(:x) take(4)'
```

```yaml
- x
- x
- x
- x
```

Use `take` to extract a finite portion.

## Sorting

### `qsort`

Sort with a comparison function:

```sh
eu -e '[5, 3, 1, 4, 2] qsort(<)'
```

```yaml
- 1
- 2
- 3
- 4
- 5
```

### `sort-nums`

A convenience for sorting numbers in ascending order:

```sh
eu -e '[30, 10, 20] sort-nums'
```

```yaml
- 10
- 20
- 30
```

## Putting It Together

Here is a more complete example combining multiple list operations:

```eu
data: [
  { name: "Alice" score: 85 }
  { name: "Bob" score: 92 }
  { name: "Charlie" score: 78 }
  { name: "Diana" score: 95 }
]

top-scorers: data
  filter(_.score >= 90)
  map(_.name)
```

```yaml
data:
- name: Alice
  score: 85
- name: Bob
  score: 92
- name: Charlie
  score: 78
- name: Diana
  score: 95
top-scorers:
- Bob
- Diana
```

## Key Concepts

- Lists are created with `[...]` and can be heterogeneous
- `map`, `filter`, and `foldl`/`foldr` are the core transformation
  functions
- `take`, `drop`, `reverse`, `append` (`++`), and `concat` reshape
  lists
- `all`, `any`, `all-true?`, and `any-true?` test list conditions
- Lazy evaluation allows working with infinite lists via `repeat`
- `qsort` sorts with a custom comparator; `sort-nums` sorts numbers
