# Eucalypt implementation

At **0.1.x** very little effort has been spent on performance or other
quality of implementation aspects in comparison to attributes which
aid experimentation or evolution of the tool. In fact, the main reason
it is currently written in Haskell is just for the ease of defining
and redefining the various syntax data structures. It should be "fast
enough" for the intended use cases but would completely suck in any
compute intensive benchmarks.

When you run `eu`, the execution consists of several phases:

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

The machine is parasitic on the garbage collection of the underlying
implementation runtime (Haskell) by attempting to be economical with
the heap references to it maintains.

While these phases are reminiscent of Haskell's own (GHC)
implementation, the resemblance is shallow and crude. Eucalypt's core
syntax is untyped, the STG syntax differs and the machine is an
interpreter, not a compiler.

## Diagnostics

Raw ASTs can be dumped (as JSON) using the `-p` command line switch.

Core syntax can be dumped at various stages using a variety of
`--dump-xxx` command line switches.

The final STG syntax which is executed by the STG machine may be
dumped using `--dump-stg`
