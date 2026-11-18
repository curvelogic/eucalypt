# Dicts

A **dict** is a persistent dictionary ordered by key. Where a block is
keyed by symbols and looks a key up by scanning, a dict takes keys of
several kinds — numbers, strings, symbols, or lists of those, such as
`[x, y]` coordinates — and finds, adds or removes one in O(log n). Its
entries are always kept in key order, which also makes it a priority queue.

All the functions live in the `dict` namespace, and, as for sets, the dict
comes last, so they read well in a pipeline: `d dict.get(k)`. The
[cheat sheet](../appendices/cheat-sheet.md) lists them all, and the
[prelude reference](../reference/prelude/dicts.md) documents each one.

## Building a dict

`dict.of` (alias `dict.from-list`) builds a dict from `[key, value]`
pairs; `dict.empty` is the empty dict, and `dict.assoc` adds an entry.

```eu
ages: dict.of[["ann", 31], ["bob", 27]]
more: ages dict.assoc("cy", 45)

n: more dict.size //=> 3
names: more dict.keys //=> ["ann", "bob", "cy"]
```

A dict is persistent: `assoc` and `dissoc` return a new dict and leave the
old one as it was (`ages` above still has two entries). The new dict shares
almost all of its structure with the old, so this is cheap.

`dict.from-block` turns a block into a dict keyed by its symbols, and
`dict.to-block` turns a dict back into a block.

## Looking up

```eu
` :suppress
d: dict.of[[[0, 0], :start], [[3, 4], :goal]]

goal: d dict.get([3, 4]) //=> :goal
none: d dict.get-or([1, 1], :wall) //=> :wall
there: d dict.contains?([0, 0]) //=> true
```

`dict.get` of a key the dict does not hold is an error that names the key;
`dict.get-or` gives a default instead. (`d` is marked `:suppress` because a
dict with list keys cannot be rendered; see [Rendering](#rendering).)

## Keys and their order

Keys are evaluated fully when they are stored; values are not (see below).
Entries come out — from `to-list`, `keys`, `values` and in rendered output —
in key order. Numbers order numerically, strings by their text, symbols by
name and lists element by element, and across kinds numbers come before
strings, strings before symbols, and symbols before lists. `1` and `1.0` are
the same key.

```eu
` :suppress
mixed: dict.of[[:b, 1], ["a", 2], [[1], 3], [10, 4], [2, 5]]

order: mixed dict.keys //=> [2, 10, "a", :b, [1]]
```

## Counting and updating

`dict.update(k, f, default)` maps `k` to `f` of its old value, starting
from `default` when the key is absent. In a loop that counts, use
`dict.update-strict`, which evaluates the new value at once, so a long run
of updates does not build up a chain of unevaluated additions:

```eu
` "count one more of word `w` in dict `d`"
count(d, w): d dict.update-strict(w, + 1, 0)

` "count the words of a list"
tally(words): words foldl(count, dict.empty)

counts: ["to", "be", "or", "not", "to", "be"] tally dict.to-list
          //=> [["be", 2], ["not", 1], ["or", 1], ["to", 2]]
```

`dict.merge-with(f, a, b)` combines two dicts, using `f(va, vb)` where both
hold a key (`merge-with-strict` evaluates it at once):

```eu
a: dict.of[[:x, 1], [:y, 2]]
b: dict.of[[:y, 10], [:z, 20]]

sum: dict.merge-with(+, a, b) dict.to-list //=> [[:x, 1], [:y, 12], [:z, 20]]
```

## Lazy values and memo tables

A value is stored unevaluated and computed at most once, the first time it
is needed, however many dicts share it. So a dict can refer to itself, which
makes it a memo table:

```eu
` "the Fibonacci number `n`, defined from the earlier entries of the table `fib`"
fib-entry(n): (n < 2) then(n, (fib dict.get(n - 1)) + (fib dict.get(n - 2)))

` "the entry for `n` in the table"
entry(n): [n, fib-entry(n)]

` "a table of Fibonacci numbers, each defined from the earlier entries"
fib: range(0, 91) map(entry) dict.of

f90: fib dict.get(90) //=> 2880067194370816120
```

Each entry is computed once, so this takes 91 additions rather than an
exponential number.

## Ordered operations

Because entries are kept in order, the least and greatest keys, and the
entries in a range of keys, are each found in O(log n):

```eu
d: dict.of[[5, :e], [1, :a], [3, :c], [7, :g]]

least: d dict.min //=> [1, :a]
greatest: d dict.max //=> [7, :g]
middle: d dict.range(2, 6) dict.keys //=> [3, 5]
```

`dict.range(lo, hi)` holds the keys `k` with `lo ≤ k < hi`. `dict.pop-min`
returns the least entry and the rest of the dict as `[key, value, rest]`
(`pop-max` likewise for the greatest); `min`, `max` and the pops are errors
on an empty dict, so test `dict.empty?` first.

## Priority queues and Dijkstra's algorithm

A dict keyed by `[priority, item]` is a priority queue: `dict.assoc` pushes
an item and `dict.pop-min` takes the item with the least priority. When two
items could share a priority, make the key unique, for instance
`[priority, item]` with the item itself, or `[priority, seq, item]` with a
counter.

Here is Dijkstra's algorithm on a 141 × 141 grid, finding the cheapest path
from the top-left corner to the bottom-right, where entering a cell costs
its weight. The queue is a dict keyed by `[distance, [x, y]]`; the best
distances found so far are a dict keyed by `[x, y]`. Rather than finding
and changing a queued entry when a shorter path turns up, the search pushes
a new entry and skips stale ones as they are popped.

```eu
` "the side of the grid"
n: 141

` "a distance longer than any path"
far: 1000000000

` "the target cell"
goal: [n - 1, n - 1]

` "the weight of the cell `[x, y]`"
weight([x, y]): (7 * x + 13 * y + x * y) % 9 + 1

` "true if the cell `[x, y]` is on the grid"
inside?([x, y]): x >= 0 ∧ x < n ∧ y >= 0 ∧ y < n

` "the cells next to `[x, y]` on the grid"
neighbours([x, y]): [[x + 1, y], [x - 1, y], [x, y + 1], [x, y - 1]] filter(inside?)

` "the cost of entering cell `c` from a cell at distance `d`"
via(d, c): d + weight(c)

` "true if entering cell `c` at distance `d` beats its best distance so far"
better?(d, dist, c): via(d, c) < (dist dict.get-or(c, far))

` "queue cell `c`, entered from a cell at distance `d`"
push(d, queue, c): queue dict.assoc([via(d, c), c], null)

` "record the distance of cell `c`, entered from a cell at distance `d`"
record(d, dist, c): dist dict.assoc(c, via(d, c))

` "take the nearest queued cell and continue the search from it"
search(queue, dist): queue dict.pop-min visit(dist)

` "visit cell `c` at distance `d`, with `rest` still queued; a stale entry relaxes nothing"
visit(dist, [[d, c], v, rest]): {
  fresh: d <= (dist dict.get-or(c, far))
  improved: fresh then(c neighbours filter(better?(d, dist)), [])
}.((c = goal) then(d, search(improved foldl(push(d), rest), improved foldl(record(d), dist))))

shortest: search(dict.of[[[0, [0, 0]], null]], dict.of[[[0, 0], 0]]) //=> 1085
```

The queue holds no values of its own (`null`): the key carries everything.
Each step costs O(log n) in the size of the queue, and the whole search runs
in a few seconds.

## Rendering

A dict renders, in every output format, as a mapping of its entries in key
order: a symbol key as its name, a string key as its text and a number key
as its rendered text.

```eu
scores: dict.of[[:bob, 27], [:ann, 31]]
```

```yaml
scores:
  ann: 31
  bob: 27
```

A list key cannot be a mapping key, and two keys with the same text (`1`
and `"1"`, or `"x"` and `:x`) would give the mapping the same key twice, so
both are errors; render `d dict.to-list` to output such a dict as
`[key, value]` pairs. See [Export Formats](../reference/export-formats.md).

## Types

The type checker writes the type of a dict as `dict(k, v)`, with keys of
type `k` and values of type `v`; bare `dict` is `dict(any, any)`. It is a
different type from `Dict(T)`, the type of a block whose values all have
type `T`. See [Type Checking](type-checking.md#dictk-v).

## Equality

`=` on two dicts compares identity, not contents, as it does for sets: two
dicts built separately with the same entries are not `=`. Compare
`d dict.to-list` instead.
