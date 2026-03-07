# Lazy Evaluation

*This chapter is under construction.*

Eucalypt uses lazy evaluation, meaning expressions are only evaluated
when their values are needed. This has important consequences:

- `if` is just a function (both branches are not evaluated)
- Infinite lists are possible (e.g. `repeat(1)`, `ints-from(0)`)
- Unused computations have no cost
