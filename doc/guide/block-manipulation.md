# Block Manipulation

Blocks are key-value mappings -- the core data structure in eucalypt.
This chapter covers the prelude functions for querying and
transforming blocks as data.

## Inspecting blocks

### Keys, values, and elements

```eu
b: { x: 1 y: 2 z: 3 }
ks: keys(b)     //=> [:x, :y, :z]
vs: values(b)   //=> [1, 2, 3]
es: elements(b) //=> [[:x, 1], [:y, 2], [:z, 3]]
```

`keys` returns a list of key names as symbols. `values` returns the
corresponding values. `elements` returns key-value pairs.

### Lookup functions

```eu
b: { x: 1 y: 2 }
a: lookup(:x, b)           //=> 1
c: lookup-or(:z, 99, b)    //=> 99
d: has(:x, b)              //=> true
e: has(:z, b)              //=> false
```

`lookup` retrieves a value by symbol key. `lookup-or` provides a
default if the key is missing. `has` tests for key existence.

### Length

```eu
b: { x: 1 y: 2 z: 3 }
n: b keys count //=> 3
```

## Transforming blocks

### map-values

Apply a function to every value in a block, keeping keys:

```eu
b: { x: 1 y: 2 z: 3 }
result: b map-values(* 10) //=> { x: 10 y: 20 z: 30 }
```

### map-keys

Apply a function to every key:

```eu,notest
b: { x: 1 y: 2 }
result: b map-keys(str.to-upper)
```

### map-elements

Transform key-value pairs:

```eu,notest
b: { x: 1 y: 2 }
result: b map-elements(k v: [str.to-upper(k), v * 10])
```

## Selecting and removing keys

### select

Keep only specified keys:

```eu,notest
b: { x: 1 y: 2 z: 3 }
result: b select([:x, :y]) //=> { x: 1 y: 2 }
```

### dissoc

Remove specified keys:

```eu,notest
b: { x: 1 y: 2 z: 3 }
result: b dissoc([:z]) //=> { x: 1 y: 2 }
```

## Merging blocks

### Shallow merge (catenation)

Juxtapose blocks to merge them. The right block's values win:

```eu
base: { a: 1 b: 2 }
overlay: { b: 3 c: 4 }
merged: base overlay //=> { a: 1 b: 3 c: 4 }
```

### Deep merge (`<<`)

Recursively merge nested blocks:

```eu,notest
base: { db: { host: "localhost" port: 5432 } }
override: { db: { port: 3306 } }
result: base << override
# => { db: { host: "localhost" port: 3306 } }
```

Shallow merge would replace the entire `db` block. Deep merge
preserves nested keys that are not overridden.

## Building blocks from lists

### block

Convert a list of key-value pairs to a block:

```eu,notest
pairs: [["x", 1], ["y", 2]]
result: block(pairs)   # => { x: 1 y: 2 }
```

### from-list

Build a block by extracting keys from list elements:

```eu,notest
items: [{ id: "a" val: 1 }, { id: "b" val: 2 }]
result: items from-list(.id)
```

## Sorting keys

```eu,notest
b: { c: 3 a: 1 b: 2 }
result: b sort-keys   # => { a: 1 b: 2 c: 3 }
```

## Nested access

### Dot lookup

Chain dots for nested access:

```eu
config: { db: { host: "localhost" port: 5432 } }
h: config.db.host //=> "localhost"
```

### Generalised lookup

The dot operator can take an expression on the right:

```eu
point: { x: 3 y: 4 }
sum: point.(x + y) //=> 7
```

The expression is evaluated in the scope of the block on the left.

## Deep queries

### deep-find

Search recursively through nested blocks for a key:

```eu,notest
data: { a: { b: { target: 42 } } }
result: data deep-find("target")  # => [42]
```

`deep-find` returns a list of all values found at matching keys
anywhere in the nested structure.

### deep-query

Apply a predicate to all nested values:

```eu,notest
data: { a: 1 b: { c: 2 d: { e: 3 } } }
nums: data deep-query(number?)
```

## Type checking

Test whether a value is a block:

```eu,notest
a: block?({ x: 1 })   # => true
b: block?([1, 2])     # => false
```

Other type predicates: `number?`, `string?`, `list?`, `nil?`,
`bool?`, `sym?`.

## Practical example

Merge a base configuration with environment-specific overrides:

```eu
base: {
  app: { name: "myapp" debug: false }
  db: { host: "localhost" port: 5432 }
}

prod: {
  app: { debug: false }
  db: { host: "prod-db.example.com" }
}

config: base << prod
host: config.db.host //=> "prod-db.example.com"
```

## Next steps

- [Imports and Modules](imports-and-modules.md) -- loading external
  data and code
- [Working with Data](working-with-data.md) -- end-to-end data
  processing
