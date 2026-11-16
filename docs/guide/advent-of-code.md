# Advent of Code

This chapter is a cookbook for solving [Advent of Code](https://adventofcode.com)
puzzles in eucalypt: how to lay out a day, parse the input, work with grids,
search, memoise, iterate, and find out why a solution is slow. Each recipe
is a short, complete program; copy it and adapt it.

The recipes follow the [style guide](../eucalypt-style.md): pipelines over
nested calls, `then` for a two-way choice, `cond` for three or more, and
functions whose data argument comes last. The
[syntax gotchas](../appendices/syntax-gotchas.md) are worth a read before the
first puzzle; the ones that bite hardest in AoC code are noted where they
come up.

## Running a puzzle

Keep each day in one file, with the worked example and your puzzle input
beside it:

```text
day01.eu
inputs/day01-ex1.txt    # the worked example from the puzzle page
inputs/day01.txt        # your puzzle input (do not commit it)
```

A text file imported by name arrives as a list of strings, one per line.
Write the solution as functions of that list, then give each part a target
that imports the real input, and add a `test` target that checks the
worked example:

```eu,notest
` "the two columns of numbers in `lines`"
columns(lines): lines map(str.matches-of("\d+") ; map(num)) transpose

` "the total distance between the sorted columns"
part-1(lines): columns(lines) map(sort-nums) uncurry(zip-with(-)) map(abs) sum

` { target: :test
    import: "ex=inputs/day01-ex1.txt" }
test: {
  p1: ex part-1 //= 11
  RESULT: p1 then(:PASS, :FAIL)
}

` { target: :part-1
    import: "input=inputs/day01.txt" }
answer-1: input part-1
```

```sh
eu test day01.eu            # check the worked example
eu day01.eu -t part-1       # solve part 1 with your input
eu check day01.eu           # type-check without running
```

`eu test` judges a file by its `test` targets. A file with none is judged
by its default output, which has no `RESULT`, so it reports `FAIL`; give
every day a `test` target from the start. `RESULT` is computed from the
checks so that each one counts (see [Testing](testing.md)).

To explore an input before writing a solution, bind it to a name on the
command line and evaluate an expression against it:

```sh
eu input=inputs/day01.txt -e 'input count'
eu input=inputs/day01.txt -e 'input map(str.len) max-of'
```

The same check, with the example inline rather than in a file, looks like
this:

```eu
` "the two columns of numbers in `lines`"
columns(lines): lines map(str.matches-of("\d+") ; map(num)) transpose

` "the total distance between the sorted columns"
part-1(lines): columns(lines) map(sort-nums) uncurry(zip-with(-)) map(abs) sum

` :suppress
ex: ["3   4", "4   3", "2   5", "1   3", "3   9", "3   3"]

distance: ex part-1 //=> 11
```

## Parsing

### Lines and blank-line blocks

Many inputs are groups of lines separated by blank lines. `split-on`
splits a list wherever its predicate holds, dropping the separators:

```eu
` :suppress
lines: ["1000", "2000", "", "4000", "", "5000", "6000"]

groups: lines split-on(= "") //=> [["1000", "2000"], ["4000"], ["5000", "6000"]]
totals: groups map(map(num) ; sum) //=> [3000, 4000, 11000]
```

A single input that has two sections (rules, then updates, say) is the
same thing with two groups; destructure them with a function parameter:

```eu
` :suppress
lines: ["a: 1", "b: 2", "", "a", "b", "a"]

` "the number of uses of each rule, from `[rules, uses]`"
summary[rs, us]: { rules: rs count uses: us count }

both: lines split-on(= "") summary //=> { rules: 2 uses: 3 }
```

### Numbers

`str.matches-of` finds every match of a regex; `num` turns each into a
number. Include the minus sign in the pattern to keep negative numbers:

```eu
` "all the integers in string `s`, including negative ones"
ints(s): s str.matches-of("-?\d+") map(num)

robot: "p=0,4 v=3,-3" ints //=> [0, 4, 3, -3]
```

Where a hyphen is a separator rather than a sign, as in a range `10-20`,
match `\d+` instead, or `10-20` reads as `10` and `-20`.

### Regex groups

`str.match-with` returns the whole match followed by its groups, so `tail`
leaves the groups. Name the parts with a destructuring parameter:

```eu
` "a parsed `move n from a to b` instruction"
instruction[k, a, b]: { n: k from: a to: b }

` "parse one instruction line"
move(line): line str.match-with("move (\d+) from (\d+) to (\d+)") tail map(num) instruction

m: "move 3 from 1 to 2" move //=> { n: 3 from: 1 to: 2 }
```

The parameters are `k`, `a` and `b`, not `n`, `from` and `to`: a block
field sees itself, so `{ n: n }` would refer to the field, not the
parameter (see [the gotcha](../appendices/syntax-gotchas.md)).

### Characters and digits

`str.letters` splits a string into one-character strings:

```eu
row: "#.#" str.letters //=> ["#", ".", "#"]
digits: "9021" str.letters map(num) //=> [9, 0, 2, 1]
```

## Grids

### A grid as a vec

A list has O(n) indexed access, so a grid you look into at random is best
held as a flat [vec](../reference/prelude/index.md) of its characters, with
the cell `[x, y]` at index `y * w + x`:

```eu
` :suppress
rows: ["#....", "..#..", "....S"]

` :suppress
grid: { w: rows head str.len  h: rows count  cells: rows mapcat(str.letters) vec.of }

` "true if `[x, y]` is on the grid"
inside?([x, y]): x >= 0 ∧ x < grid.w ∧ y >= 0 ∧ y < grid.h

` "the character at `[x, y]`, or null off the grid"
at([x, y]): inside?([x, y]) then(grid.cells vec.nth(y * grid.w + x), null)

` "every cell `[x, y]` that holds character `c`"
find-all(c): { :for y: range(0, grid.h), x: range(0, grid.w), _: [at([x, y])] filter(= c) }.[x, y]

start: find-all("S") head //=> [4, 2]
corner: at([0, 0]) //=> "#"
off: at([5, 0]) //=> null
```

For a sparse grid, or one you change as you go, keep the interesting cells
in a set instead: `walls: find-all("#") set.of`, then `c ∈ walls`. Sets and
[dicts](dicts.md) take `[x, y]` lists as elements and keys.

### Neighbours

List the offsets and add them, filtering by the bounds, or use a `:for`
comprehension for the eight cells around a point:

```eu
` "the four directions, as `[dx, dy]`"
dirs: [[1, 0], [0, 1], [-1, 0], [0, -1]]

` "the cell one step from `[x, y]` in direction `[dx, dy]`"
step([x, y], [dx, dy]): [x + dx, y + dy]

` "the four cells next to `c`"
adjacent(c): dirs map(step(c))

` "the eight cells around `[x, y]`"
around([x, y]): { :for dx: [-1, 0, 1], dy: [-1, 0, 1], _: [[dx, dy]] filter(!= [0, 0]) }.[x + dx, y + dy]

four: adjacent([2, 2]) //=> [[3, 2], [2, 3], [1, 2], [2, 1]]
eight: around([0, 0]) count //=> 8
```

Turning right is a step along `dirs`: the next direction after index `i` is
`dirs !! ((i + 1) % 4)`.

### Columns and transposes

`transpose` turns rows into columns, which is the easy way to read a grid
column by column or to rotate it:

```eu
` :suppress
rows: ["abc", "def"]

cols: rows map(str.letters) transpose map(str.join-on("")) //=> ["ad", "be", "cf"]
```

### Numeric grids

For a grid of numbers, an n-dimensional `arr` has indexing by `[row, col]`,
whole-array arithmetic, and `arr.neighbours`, which drops the offsets that
fall off the edge:

```eu
` :suppress
heights: arr.from-flat([2, 3], [1, 2, 3, 4, 5, 6])

centre: heights arr.get([1, 1]) //=> 5
next-to-corner: heights arr.neighbours([0, 0], [[0, 1], [1, 0], [-1, 0], [0, -1]]) //=> [2, 4]
```

An `arr` holds its numbers as 64-bit floats (`5` comes back as `5.0`), so
it is exact only up to 2⁵³. Keep large counts in lists or dicts.

## Search

### Breadth-first search

Breadth-first search finds the fewest steps when every step costs the
same. Keep the current ring of states and a set of the states seen, and
expand one ring at a time. The search below is written over any states:
`moves(s)` lists the states one step from `s`.

```eu
` :suppress
maze: ["S..#....", "#.##.##.", "...#..#.", ".#...#E."]

` :suppress
grid: { w: maze head str.len  h: maze count  cells: maze mapcat(str.letters) vec.of }

` "the character at `[x, y]`, or a wall off the grid"
at([x, y]): (x >= 0 ∧ x < grid.w ∧ y >= 0 ∧ y < grid.h) then(grid.cells vec.nth(y * grid.w + x), "#")

` "true if cell `c` is not a wall"
open?(c): at(c) != "#"

` "the open cells next to `[x, y]`"
moves([x, y]): [[x + 1, y], [x - 1, y], [x, y + 1], [x, y - 1]] filter(open?)

` "the fewest steps from `start` to a state satisfying `goal?`, or null if there is none"
bfs(moves, goal?, start): {
  ` "the states one step beyond `ring` not yet in `seen`"
  beyond(ring, seen): ring mapcat(moves) remove(∈ seen) set.of set.to-list

  ` "search from `ring`, at distance `d`, having seen `seen`"
  go(d, ring, seen): cond[
    ring nil? => null,
    ring any(goal?) => d,
    { next: beyond(ring, seen) }.(go(d + 1, next, seen set.union(set.of(next))))
  ]
}.(go(0, [start], set.of[start]))

steps: bfs(moves, = [6, 3], [0, 0]) //=> 17
```

A state need not be a cell. When the puzzle depends on more than position
(the direction you face, the keys you hold), make the state a list of all
of it, such as `[x, y, dir]`; sets and dicts take any list of primitives.

### Dijkstra's algorithm

When steps cost different amounts, use Dijkstra's algorithm with a
priority queue. A [dict](dicts.md) is the priority queue: its entries are
kept in key order, `dict.assoc` pushes and `dict.pop-min` takes the entry
with the least key. Key the queue by `[priority, seq]`, where `seq` is a
counter that makes every key unique, and store the state as the value: the
queue then orders entries by two numbers alone, and two states at the same
distance never collide.

The template below threads `[queue, seq, dist]` through the search, where
`dist` is a dict of the best distance found for each state. Rather than
finding a queued entry and lowering its priority when a shorter path turns
up, it pushes a new entry and skips stale ones as they are popped. Because
`dist` is keyed by state, a state must be a valid dict key: a number,
string, symbol or list of those. As for `bfs`, `moves(s)` describes the
puzzle, here as `[cost, next]` pairs. The
example is a 141 × 141 grid, the size of a typical AoC map, where entering
a cell costs its weight:

```eu
` "a distance longer than any path"
far: 1000000000

` "the least distance from `start` to a state satisfying `goal?`, where `moves(s)` lists the `[cost, next]` steps out of state `s`; null if there is none"
dijkstra(moves, goal?, start): {
  ` "true if reaching state `s` at distance `d` beats the best found so far"
  better?(dist, [d, s]): d < (dist dict.get-or(s, far))

  ` "queue state `s` at distance `d` under number `seq`, and record its distance"
  push([queue, seq, dist], [d, s]): [queue dict.assoc([d, seq], s), seq + 1, dist dict.assoc(s, d)]

  ` "a step of cost `c` to state `s`, taken from distance `d`"
  from(d, [c, s]): [d + c, s]

  ` "take the nearest queued state and continue from it"
  search([queue, seq, dist]): queue dict.empty? then(null, visit(seq, dist, queue dict.pop-min))

  ` "visit state `s` at distance `d`, with `rest` still queued, skipping a stale entry"
  visit(seq, dist, [[d, _], s, rest]): cond[
    goal?(s) => d,
    d > (dist dict.get(s)) => search([rest, seq, dist]),
    search(s moves map(from(d)) filter(better?(dist)) foldl(push, [rest, seq, dist]))
  ]
}.(search(push([dict.empty, 0, dict.empty], [0, start])))

` "the side of the grid"
n: 141

` "the weight of the cell `[x, y]`"
weight([x, y]): (7 * x + 13 * y + x * y) % 9 + 1

` "true if the cell `[x, y]` is on the grid"
inside?([x, y]): x >= 0 ∧ x < n ∧ y >= 0 ∧ y < n

` "a step into cell `c`, costing its weight"
costed(c): [weight(c), c]

` "the steps out of cell `[x, y]`"
moves([x, y]): [[x + 1, y], [x - 1, y], [x, y + 1], [x, y - 1]] filter(inside?) map(costed)

shortest: dijkstra(moves, = [n - 1, n - 1], [0, 0]) //=> 1085
```

Each push and pop is O(log n) in the size of the queue, and this search of
nearly 20,000 cells runs in about four seconds. In a loop this hot, small
named helpers such as `from` and `costed` are quicker than the same
functions built with `bimap` or `juxt`.

For a priority queue whose states are themselves lists of primitives, the
counter is optional: `[d, s]` is already a unique key. The
[dict guide](dicts.md#priority-queues-and-dijkstras-algorithm) has that
form.

### Union-find and topological order

For connected components and dependency orders on numbered nodes, the
`graph` namespace has union-find and Kahn's algorithm built in. Both take
the number of nodes and a list of `[i, j]` edges:

```eu
` "a component label for each of six nodes"
labels: graph.union-find(6, [[0, 1], [1, 2], [3, 4]])

components: labels set.of set.size //=> 3
order: graph.topo-sort(3, [[2, 0], [0, 1]]) //=> [2, 0, 1]
```

The results are numbers in floating-point form (`2.0`), which compare equal
to integers. When the graph has a cycle, `topo-sort` returns only the
nodes it could order: check `order count = n`. To use names rather than numbers, number
them first, for instance with a dict from name to index.

## Memoisation and dynamic programming

### Rolling rows

When each row of a table depends only on the row before, fold over the
rows, keeping one row. Here, the number of paths from the top-left to the
bottom-right of a grid, moving only right or down and avoiding `#`:

```eu
` :suppress
rows: ["....", "..#.", ".#..", "...."]

` "the paths to a cell from `left` and from `up`, or none if it is a wall"
cell(left, [up, c]): (c = "#") then(0, left + up)

` "the path counts for `row`, given the counts for the row above"
next-row(above, row): zip(above, row str.letters) scanl(cell, 0) tail

` "the path counts above the first row: one way in, at the top-left"
entry: [1] ++ (repeat(0) take(3))

paths: rows foldl(next-row, entry) last //=> 2
```

### Counting by key

When the puzzle has many identical things that each become several more,
count them instead of listing them: a dict from thing to count. Each step
builds the next dict with `dict.update-strict`, which adds as it goes rather
than building up unevaluated sums. These stones split when they have an
even number of digits; after 75 steps there are 65 trillion of them, but
only 54 distinct numbers:

```eu
` "the stones that stone `x` becomes in one step"
blink-stone(x): {
  s: str.of(x)
  half: (s str.len) / 2
  ` "the number formed by the characters `cs`"
  join(cs): cs str.join-on("") num
}.(cond[
  x = 0 => [1],
  (s str.len) % 2 = 0 => s str.letters split-at(half) map(join),
  [x * 2024]
])

` "add `k` stones numbered `y` to the counts `d`"
add(k, d, y): d dict.update-strict(y, + k, 0)

` "add the stones that `k` stones numbered `x` become"
add-children(d, [x, k]): blink-stone(x) foldl(add(k), d)

` "one step for every stone"
blink(counts): counts dict.to-list foldl(add-children, dict.empty)

` "a count of one stone numbered `x`"
one(x): [x, 1]

` "the number of stones after `n` steps from the list `xs`"
stones(n, xs): iterate(blink, xs map(one) dict.of) nth(n) dict.values sum

short: stones(25, [125, 17]) //=> 55312
long: stones(75, [125, 17]) //=> 65601038650482
```

`dict.merge-with(+, a, b)` adds two such tallies together.

### A lazy table

A dict's values are stored unevaluated and computed at most once, so a
dict can be defined in terms of itself: write down every entry of the
table as a formula over the others, and only the entries you reach are
computed. Here is the longest common subsequence of two strings, a 2-D
table keyed by `[i, j]`:

```eu
` :suppress
a: "AGGTAB" str.letters vec.of
` :suppress
b: "GXTXAYB" str.letters vec.of

` "the length of the longest common subsequence of the first `i` of `a` and the first `j` of `b`"
lcs([i, j]): cond[
  i = 0 ∨ j = 0 => 0,
  (a vec.nth(i - 1)) = (b vec.nth(j - 1)) => (table dict.get([i - 1, j - 1])) + 1,
  max(table dict.get([i - 1, j]), table dict.get([i, j - 1]))
]

` { doc: "every entry of the table, each defined from earlier entries"
    export: :suppress }
table: { :for i: range(0, (a vec.len) + 1), j: range(0, (b vec.len) + 1) }.[[i, j], lcs([i, j])] dict.of

length: table dict.get([a vec.len, b vec.len]) //=> 4
```

The same works for a one-dimensional table keyed by an integer, and for
any state space you can list in advance.

### Memoising discovered states

When the states turn up only as the recursion finds them, thread a memo
dict through it: each call takes the memo and returns `[result, memo]` with
what it learned added. Here, the number of ways to build each design from
the given patterns, memoised on the rest of the design still to build:

```eu
` :suppress
patterns: ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]

` "the number of ways to build `design`, as `[ways, memo]`, given the answers in `memo`"
ways(memo, design): cond[
  design = "" => [1, memo],
  memo dict.contains?(design) => [memo dict.get(design), memo],
  {
    rests: patterns filter(begins?(design)) map(str.len ; str-drop(design))
    found: rests foldl(add-ways, [0, memo])
  }.([first(found), second(found) dict.assoc(design, first(found))])
]

` "true if `design` begins with pattern `p`"
begins?(design, p): design str.starts-with?(p)

` "the rest of `s` after its first `k` characters"
str-drop(s, k): s str.letters drop(k) str.join-on("")

` "add the ways to build `rest` to the running `[total, memo]`"
add-ways([total, memo], rest): {
  r: ways(memo, rest)
}.([total + first(r), second(r)])

` "the ways to build each design, sharing one memo"
all-ways(designs): designs foldl(add-ways-to-list, [[], dict.empty]) first reverse

` "add the ways to build `design` to the list, as `[counts, memo]`"
add-ways-to-list([counts, memo], design): {
  r: ways(memo, design)
}.([first(r) ‖ counts, second(r)])

counts: all-ways["brwrr", "bggr", "gbbr", "rrbgbr", "ubwu", "bwurrg", "brgr", "bbrwb"]
          //=> [2, 1, 4, 6, 0, 1, 2, 0]
```

Patterns are used as regexes by `str.starts-with?`, which is fine for
letters; escape them if they hold regex characters.

## Iteration

### Loops

`iterate(f, x)` is the list `x`, `f(x)`, `f(f(x))`, … (each element is
evaluated to weak head normal form as its cell is reached) and
`iterate(f, x) nth(n)` is `f` applied `n` times. For a number state that runs
in constant stack, but a block or list state is evaluated only to its outermost
constructor, so its fields can still build chains. A tail-recursive function
that calls the step directly avoids building the list:

```eu
` "one step of the simulation"
step(s): (s * 7 + 3) % 1000003

` "the state after `n` steps from `s`"
run(n, s): (n = 0) then(s, run(n - 1, step(s)))

final: run(100000, 1) //=> 940193
```

This runs in constant stack: eucalypt can see that `step` uses its
argument, so each state is computed as the loop goes. A generic loop that
takes the step as a parameter loses that, such as
`times(f, n, x): (n = 0) then(x, times(f, n - 1, f(x)))`: eucalypt cannot
tell whether an unknown `f` uses its argument, so it builds a chain of
unevaluated steps that is only worked through at the end, using stack and
memory in proportion. (`iterate(step, s) nth(n)` evaluates each state to weak
head normal form as its cell is reached, which is enough for a number state
but not for the fields of a block state.)

So for a long loop of your own, name the step function in the loop itself. A loop that
runs until something holds tests the state in the same place, as in
`run-until(s): (s done?) then(s, run-until(step(s)))`, and looking at the
state in the test also keeps it evaluated.

### Cycle detection

"What is the state after a billion steps?" usually means the states repeat.
Record the step at which each state was seen in a dict; at the first repeat,
skip whole cycles and run only the remainder. The state must be a valid
dict key — a number, string, symbol or list of them — so encode a grid as a
string or a list of rows.

```eu
` "`f` applied `n` times to `x`"
times(f, n, x): (n = 0) then(x, times(f, n - 1, f(x)))

` "the state after `n` steps of `f` from `x`, skipping repeated cycles"
after(f, n, x): {
  ` "continue from state `s` at step `i`, with the step of each earlier state in `seen`"
  go(i, s, seen): cond[
    i = n => s,
    seen dict.contains?(s) => times(f, (n - i) % (i - (seen dict.get(s))), s),
    go(i + 1, f(s), seen dict.assoc(s, i))
  ]
}.(go(0, x, dict.empty))

` "one step of a sequence that soon repeats"
next(x): (x * x + 1) % 97

state: after(next, 1000000000, 0) //=> 26
```

## Intervals and geometry

### Merging intervals

Sort the intervals by start, then fold, extending the last interval while
the next one overlaps or touches it:

```eu
` "add interval `[lo, hi]` to the merged list `acc`, built in reverse"
absorb(acc, [lo, hi]): cond[
  acc nil? => [[lo, hi]],
  lo <= (acc head second) + 1 => [acc head first, max(hi, acc head second)] ‖ (acc tail),
  [lo, hi] ‖ acc
]

` "the intervals `xs` merged where they overlap"
merge-intervals(xs): xs sort-by-num(first) foldl(absorb, []) reverse

` "the number of integers covered by `[lo, hi]`"
width([lo, hi]): hi - lo + 1

` :suppress
ranges: [[3, 5], [10, 14], [16, 20], [12, 18]]

merged: ranges merge-intervals //=> [[3, 5], [10, 20]]
covered: ranges merge-intervals map(width) sum //=> 14
```

### Areas of lattice polygons

The shoelace formula gives the area of a polygon from its corners, and
Pick's theorem turns that into a count of cells: the cells inside a loop
dug along the grid, plus the loop itself, number `area + perimeter / 2 + 1`.

```eu
` "the offset of one step in each direction"
offsets: { R: [1, 0] L: [-1, 0] U: [0, -1] D: [0, 1] }

` "the corner reached from `[x, y]` by `k` steps in direction `d`"
dig([x, y], [d, k]): { o: offsets lookup(d) }.[x + k * first(o), y + k * second(o)]

` "twice the signed area of the polygon through the corners `ps`"
shoelace(ps): zip-with(cross, ps, (ps tail) ++ [ps head]) sum

` "the cross product of two corners"
cross([x1, y1], [x2, y2]): x1 * y2 - x2 * y1

` :suppress
plan: [[:R, 6], [:D, 5], [:L, 2], [:D, 2], [:R, 2], [:D, 2], [:L, 5],
       [:U, 2], [:L, 1], [:U, 2], [:R, 2], [:U, 3], [:L, 2], [:U, 2]]

` "the cells inside the dug loop, including the loop"
lagoon(steps): {
  corners: steps scanl(dig, [0, 0]) tail
  perimeter: steps map(second) sum
}.((corners shoelace abs) / 2 + perimeter / 2 + 1)

cells: plan lagoon //=> 62
```

`/` is floor division and `÷` exact division; both areas here are whole.
The Manhattan distance between two points is
`abs(x1 - x2) + abs(y1 - y2)`.

### Common divisors

There is no `gcd` in the prelude yet; Euclid's algorithm is two lines, and
`lcm` follows. Puzzles where several cycles must line up ask for the `lcm`
of their lengths:

```eu
` "the greatest common divisor of `a` and `b`"
gcd(a, b): (b = 0) then(a, gcd(b, a % b))

` "the least common multiple of `a` and `b`"
lcm(a, b): a / gcd(a, b) * b

together: [12, 18, 30] foldl(lcm, 1) //=> 180
```

## Debugging

- **`▶ x`** prints `x` to stderr and returns it, so it can go anywhere in an
  expression: `(▶ (a + b)) * 2`. `x dbg{label: "dist"}` does the same with
  a label, and fits at any point in a pipeline.
- **`` ` :trace ``** on a declaration prints each call and its arguments;
  `` ` { trace: :strict-exit } `` also forces and prints the result. See
  [Debug Tracing](../reference/agent-reference.md#9-debug-tracing).
- **`//=>`** asserts a value inline and passes it on: put
  `parse(ex) count //=> 10` in a test target while you work.
- **`eu dump <phase> day01.eu`** shows the program after each compiler
  phase, which helps when a parse is not what you meant.
- **`eu day01.eu -t part-1 -S`** prints the time, steps, stack depth and
  allocation of a run; see the [CLI reference](../reference/cli.md).
- **`timeout 60 eu …`** stops a run that loops forever, and
  `--heap-limit-mib 4096` stops one that would use all your memory; both
  are worth having on by default during a puzzle.

```eu
` "the square of `x`"
square(x): x * x

` "the sum of the squares, printing the list of squares on stderr"
loud: [1, 2, 3] map(square) dbg{label: "squares"} sum //=> 14
```

## Performance

Know what each structure costs:

| Operation | Cost |
|---|---|
| `xs nth(i)`, `xs !! i`, `xs count`, `xs last` on a list | O(n) |
| `v vec.nth(i)`, `v vec.len` on a vec | O(1) |
| `x ∈ s`, `s set.add(x)`, `s set.remove(x)` on a set | O(log n) |
| `d dict.get(k)`, `dict.assoc`, `dict.pop-min` on a dict | O(log n) |
| `b.key`, `b lookup(:k)`, `b merge(c)` on a block | O(n) in the block's size |
| `xs ++ ys` | O(length of `xs`) |
| `s str.letters` | O(length of `s`) |

So: hold a grid as a vec, not a list of lists you index into; hold
visited states in a set, not a list you search; and use a dict, not a
block, for any table that grows as the program runs.

Some habits that pay:

- **Build lists by prepending** with `‖` and reverse once at the end;
  appending one item at a time with `++` is quadratic.
- **Parse once.** Bind the parsed input to a name inside the part's
  function rather than parsing inside a loop.
- **Use `dict.update-strict`** (and `dict.merge-with-strict`) in a loop that
  counts, so the counts do not build up as chains of unevaluated sums.
- **Parallelise independent work** with `par-map`, `par-sum`, `par-max`,
  `par-min` and `par-concat`, which give the same answer as their
  sequential forms (`map`, then `sum`, and so on) on several cores. They
  are strict, so use them for a finite list of expensive pieces, such as
  one per input line. See [Parallel Evaluation](parallelism.md).
- **Measure** with `-S` before changing anything.
