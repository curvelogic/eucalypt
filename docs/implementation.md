# Eucalypt implementation

Since **v0.2**, Eucalypt has been written in [Rust](https://rust-lang.org).
This replaced a previous **0.1.x** implementation in Haskell.

When you run `eu`, execution proceeds in several phases:

- **parsing** inputs to abstract syntax trees
- **transformation** into a *core* syntax and merging and manipulation
  of these representations
- **compilation** into *STG* syntax
- **execution** which is interpretation of the *STG* syntax by the
  *STG machine*

The core syntax facilitates experimentation by allow powerful features
to be implemented simply by syntax transformation (e.g. user definable
operator precedence, block catenation).

It is also in transformation to core syntax that the two roles of
blocks in Eucalypt (name binding and data structuring) are peeled
apart into separate elements (a recursive let and a data structure).

The interpreter is modeled on an eval-apply STG (spineless tagless
G-machine) implementation simply to have a well defined reference
point in view for a lazy functional language abstract machine, not for
any other reason (concurrency etc.) Ultimately it could well be
quicker and clearer to tree-walk the core representation.

The implementation currently uses a crude cycle-collector to manage
memory and is single-threaded only right now.

## Diagnostics

Raw ASTs can be dumped (as JSON) using the `-p` command line switch.

Execution can be traced out using the `-d` debug switch.

Core syntax can be dumped at various stages using a variety of
`--dump-xxx` command line switches. STG syntax wrappers for intrinsic
functions can be viewed with `--dump-runtime`.

The final STG syntax which is executed by the STG machine may be
dumped using `--dump-stg`.
