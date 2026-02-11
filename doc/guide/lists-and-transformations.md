# Lists and Transformations

Lists are ordered sequences of values. Eucalypt provides a rich set of
functions for creating, querying, and transforming them.

## Creating lists

Use square brackets with optional commas:

```eu
a: [1, 2, 3]
c: ["hello", true, 42]
empty: []
```

## Accessing elements

`head` and `tail` decompose a list:

```eu
xs: [10, 20, 30]
first: head(xs)  //=> 10
rest: tail(xs)   //=> [20, 30]
```

`last` returns the final element:

```eu
xs: [10, 20, 30]
end: last(xs) //=> 30
```

Index with `nth` (zero-based):

```eu
xs: [10, 20, 30]
second: nth(1, xs) //=> 20
```

## Length and predicates

```eu
xs: [1, 2, 3]
n: count(xs)      //=> 3
e: nil?(xs)       //=> false
f: nil?([])       //=> true
```

## Map

Apply a function to every element:

```eu
xs: [1, 2, 3]
doubled: xs map(* 2)         //=> [2, 4, 6]
named: xs map("{0}")          //=> ["1", "2", "3"]
```

The `{0}` in a string is a string anaphor -- it interpolates the
current element. See [Anaphora](anaphora.md) for details.

## Filter

Keep elements that satisfy a predicate:

```eu
is-even(n): n % 2 = 0
xs: [1, 2, 3, 4, 5, 6]
evens: xs filter(is-even)         //=> [2, 4, 6]
big: xs filter(> 3)               //=> [4, 5, 6]
```

## Fold

Reduce a list to a single value:

```eu
xs: [1, 2, 3, 4]
total: xs foldl(+, 0)   //=> 10
product: xs foldl(*, 1)  //=> 24
```

`foldl` folds from the left. `foldr` folds from the right:

```eu
xs: [1, 2, 3]
result: xs foldr(+, 0) //=> 6
```

## Sorting

Eucalypt provides typed sort functions:

```eu
nums: [3, 1, 4, 1, 5] sort-nums        //=> [1, 1, 3, 4, 5]
strs: ["banana", "apple"] sort-strs     //=> ["apple", "banana"]
```

Sort by a key function:

```eu
items: [{name: "b" age: 30}, {name: "a" age: 20}]
by-name: items sort-by-str(.name)
result: by-name map(.name) //=> ["a", "b"]
```

For custom comparisons, use `qsort`:

```eu
desc-cmp(a, b): b < a
xs: [3, 1, 4, 1, 5]
desc: xs qsort(desc-cmp) //=> [5, 4, 3, 1, 1]
```

## Take and drop

```eu
xs: [1, 2, 3, 4, 5]
first3: xs take(3)       //=> [1, 2, 3]
last2: xs drop(3)        //=> [4, 5]
```

With predicates:

```eu
xs: [1, 2, 3, 4, 5]
small: xs take-while(< 4)  //=> [1, 2, 3]
big: xs drop-while(< 4)    //=> [4, 5]
```

## Append and cons

```eu
a: [1, 2]
b: [3, 4]
joined: a ++ b  //=> [1, 2, 3, 4]
```

Prepend with `cons`:

```eu
result: cons(0, [1, 2, 3]) //=> [0, 1, 2, 3]
```

## Zip

Pair elements from two lists:

```eu
names: ["Alice", "Bob"]
ages: [30, 25]
pairs: zip(names, ages) //=> [["Alice", 30], ["Bob", 25]]
```

## Reverse and unique

```eu
xs: [3, 1, 2]
result: reverse(xs) //=> [2, 1, 3]
```

```eu,notest
xs: [3, 1, 2, 1, 3]
uni: unique(xs) //=> [3, 1, 2]
```

## Flatten and concat

`concat` joins a list of lists:

```eu
nested: [[1, 2], [3, 4], [5]]
flat: concat(nested) //=> [1, 2, 3, 4, 5]
```

`mapcat` maps then flattens (also known as flat-map or concat-map):

```eu
pair-up(x): [x, x * 10]
xs: [1, 2, 3]
result: xs mapcat(pair-up) //=> [1, 10, 2, 20, 3, 30]
```

## All and any

Test whether all or any elements satisfy a predicate:

```eu
is-even(n): n % 2 = 0
xs: [2, 4, 6]
all-even: xs all(is-even)  //=> true
any-big: xs any(> 5)       //=> true
```

## Range

Generate a range of numbers:

```eu
r: range(1, 5) //=> [1, 2, 3, 4]
```

The range is half-open: it includes the start but excludes the end.

## Practical pipeline example

Combine list operations into a pipeline:

```eu
data: [5, 3, 8, 1, 9, 2, 7]
result: data filter(> 3) sort-nums map(* 10) //=> [50, 70, 80, 90]
```

Read left to right: keep elements greater than 3, sort them, multiply
each by 10.

## Next steps

- [String Interpolation](string-interpolation.md) -- working with
  strings
- [Functions and Combinators](functions-and-combinators.md) -- more
  on function composition
