# Sorting Lists — Design

## Problem

The prelude has `qsort(lt, xs)` for sorting with a custom comparator, but no
convenience functions for common sorting tasks. Users must write:

```eu
[3, 1, 2] qsort(lt)
["c", "a"] qsort(str.lt)
people qsort(λ(a, b): a.age lt(b.age))
```

## Solution

Type-specific sort functions and sort-by conveniences, all built on `qsort`.

## Core Sort Functions

Type-specific sorting using existing comparison infrastructure:

```eu
sort-nums(xs): xs qsort(lt)
sort-strs(xs): xs qsort(str.lt)
sort-zdts(xs): xs qsort(lt)      # requires polymorphic lt from eu-u1m
```

**Usage:**

```eu
[3, 1, 2] sort-nums              # [1, 2, 3]
["c", "a", "b"] sort-strs        # ["a", "b", "c"]
[t"2024-01-01", t"2023-06-15"] sort-zdts  # [t"2023-06-15", t"2024-01-01"]
```

**Descending order** via composition with existing `reverse`:

```eu
[3, 1, 2] sort-nums reverse      # [3, 2, 1]
```

## Sort-By Functions

Generic sort-by with key extractor and comparator:

```eu
sort-by(key-fn, cmp, xs): xs qsort(λ(a, b): key-fn(a) cmp(key-fn(b)))
```

**Usage:**

```eu
people: [{name: "Bob", age: 30}, {name: "Alice", age: 25}]

people sort-by(_.age, lt)        # sorted by age ascending
people sort-by(_.name, str.lt)   # sorted by name ascending
```

**Convenience wrappers:**

```eu
sort-by-num(key-fn, xs): xs sort-by(key-fn, lt)
sort-by-str(key-fn, xs): xs sort-by(key-fn, str.lt)
sort-by-zdt(key-fn, xs): xs sort-by(key-fn, lt)
```

**Usage:**

```eu
people sort-by-num(_.age)        # sorted by age
people sort-by-str(_.name)       # sorted by name
events sort-by-zdt(_.date)       # sorted by date
```

## Implementation

All functions are pure prelude — no new intrinsics needed.

```eu
` "`sort-nums(xs)` - sort list of numbers ascending"
sort-nums(xs): xs qsort(lt)

` "`sort-strs(xs)` - sort list of strings/symbols ascending"
sort-strs(xs): xs qsort(str.lt)

` "`sort-zdts(xs)` - sort list of ZDTs ascending"
sort-zdts(xs): xs qsort(lt)

` "`sort-by(key-fn, cmp, xs)` - sort list by key using comparator"
sort-by(key-fn, cmp, xs): xs qsort(λ(a, b): key-fn(a) cmp(key-fn(b)))

` "`sort-by-num(key-fn, xs)` - sort list by numeric key"
sort-by-num(key-fn, xs): xs sort-by(key-fn, lt)

` "`sort-by-str(key-fn, xs)` - sort list by string key"
sort-by-str(key-fn, xs): xs sort-by(key-fn, str.lt)

` "`sort-by-zdt(key-fn, xs)` - sort list by ZDT key"
sort-by-zdt(key-fn, xs): xs sort-by(key-fn, lt)
```

## Dependencies

- `str.lt` from eu-da3 (string comparison intrinsics)
- Polymorphic `lt` for ZDTs from eu-u1m

## Testing

**Sort functions:**

- `sort-nums`: empty, single, sorted, reverse, duplicates, negatives, floats
- `sort-strs`: empty, single, case sensitivity, unicode, symbols
- `sort-zdts`: different dates, same date different times, timezones

**Sort-by functions:**

- `sort-by-num`: blocks sorted by numeric field
- `sort-by-str`: blocks sorted by string field
- `sort-by-zdt`: blocks sorted by date field
- Nested keys: `sort-by-num(_.stats.score, items)`

**Edge cases:**

- Empty lists return empty
- Single element unchanged
- Equal elements (note: order not guaranteed — qsort is not stable)

**Descending:**

- `sort-nums reverse` produces descending
- `sort-by-num(key) reverse` works

## Out of Scope

- Stable sort guarantees (the prelude `qsort` is a classic quicksort
  partition which is **not** stable — equal elements may be reordered)
- Custom comparator builders (users have `qsort` for that)
- In-place sorting (eucalypt is functional)
