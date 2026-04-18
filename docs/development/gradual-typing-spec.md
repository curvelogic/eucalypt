# Gradual Typing for Eucalypt

**Status**: Draft spec — brainstorming  
**Bead**: eu-oh3p  
**Date**: 2026-04-17

## 1. Goals and Non-Goals

### Goals

- **Better error messages** — catch type misuse at check time with clear
  diagnostics
- **Documentation** — type annotations as machine-checked documentation
  of intent
- **Tooling** — types inform LSP autocomplete, hover info, and
  refactoring
- **Incremental adoption** — untyped code continues to work; types are
  never required

### Non-Goals (for now)

- Runtime performance optimisation from type info
- Runtime type-directed dispatch or overloading
- Full Hindley-Milner inference
- Dependent types
- Effect types

## 2. Type Language

### Primitives

| Type       | Values                        |
|------------|-------------------------------|
| `number`   | integers and floats           |
| `string`   | string values                 |
| `symbol`   | symbolic atoms (`:name`)      |
| `bool`     | `true`, `false`               |
| `null`     | `null`                        |
| `datetime` | zoned date-time values        |

### Composite Types

| Syntax            | Meaning                                          |
|-------------------|--------------------------------------------------|
| `[T]`             | homogeneous list of `T`                          |
| `(A,)`            | 1-tuple (trailing comma required)                |
| `(A, B)`          | 2-tuple                                          |
| `(A, B, C)`       | 3-tuple, etc.                                    |
| `{k: T, ..}`      | open record — has at least `k: T`, may have more |
| `{k: T}`          | closed record — has exactly `k: T`               |
| `set`             | ordered set of primitives                        |
| `vec`             | flat vector of primitives (O(1) indexed access)  |
| `array`           | n-dimensional array of numbers (floats)          |
| `A -> B`          | function from `A` to `B`                         |
| `A \| B`          | union type                                       |

Parentheses serve double duty — grouping and tuples — distinguished by
the presence of commas. `(A -> B)` is grouping; `(A, B)` is a tuple.

### Special Types

| Type     | Meaning                                                    |
|----------|------------------------------------------------------------|
| `any`    | gradual/dynamic — consistent with everything in both directions. The checker does not flag errors involving `any` |
| `top`    | supertype of all types — accepts any value but nothing can be done with it without narrowing |
| `never`  | bottom — subtype of all types. Represents unreachable code or empty collections |

### Type Variables

Lowercase identifiers (`a`, `b`, `r`, etc.) are type variables in
annotations. They are universally quantified at the declaration level:

```eu
` { type: "(a -> b) -> [a] -> [b]" }
map: __MAP
```

This means: for all types `a` and `b`, `map` takes a function `a -> b`
and a list `[a]` and returns a list `[b]`.

At call sites, type variables are **instantiated** from the argument
types. `map(str.of, [1, 2])` instantiates `a = number, b = string`,
giving result type `[string]`.

### Row Variables (future)

Named row variables allow expressing that extra fields are preserved:

```
# Future syntax
merge: {..r} -> {..s} -> {..r, ..s}
over: lens -> (a -> a) -> {..r} -> {..r}
```

Initially, open records (`{k: T, ..}`) without named row variables are
sufficient. The checker treats `{..}` as "some block" without tracking
which extra fields flow through.

## 3. Type Lattice and Subtyping

### Lattice Structure

```
                          top
                       /  |  \
                     /    |    \
              number  string  symbol  bool  null  datetime  [any]  {..}  (any -> any)
                     \    |    /
                       \  |  /
                        never
```

`any` sits outside this lattice — it is **consistent** with every type
but is neither a supertype nor subtype. This is the fundamental
distinction from `top`.

### Subtyping Rules

**Primitives** — flat, no subtyping between them:
```
never <: T <: top           for all T
null is NOT a subtype of number, string, etc.
```

Nullable values use explicit unions: `number | null`.

**Lists** — covariant:
```
[A] <: [B]                  if A <: B
```

**`set`, `vec`, `array`** — opaque primitive types with no subtyping
between them or with lists. Element types are fixed (`set` and `vec`
hold primitives; `array` holds numbers). Conversion requires explicit
functions (`set.to-list`, `vec.to-list`, `arr.to-list`).

**Tuples** — widen to lists:
```
(A, B) <: [A | B]           tuple is subtype of homogeneous list
(A, B, C) <: [A | B | C]
(A,) <: [A]                 1-tuple widens to single-element list
```

A tuple `(A, B)` is also a subtype of tuple `(C, D)` if `A <: C` and
`B <: D`.

**Records** — width and depth subtyping:
```
{x: A, y: B, ..} <: {x: C, ..}     if A <: C
```

More fields is a subtype of fewer fields (open records). Deeper fields
are covariant.

**Functions** — contravariant input, covariant output:
```
(A -> B) <: (C -> D)        if C <: A and B <: D
```

**Unions**:
```
A <: A | B                  always
A | B <: C                  if A <: C and B <: C
```

### Consistency (the gradual part)

The `any` type uses **consistency** (`~`) rather than subtyping:

```
any ~ T                     for all T (in both directions)
T ~ any                     for all T
```

When the checker encounters `any`, it does not report an error. This
allows untyped code to interoperate freely with typed code.

**Boundary principle**: when a value of type `any` flows into a
position expecting type `T`, the checker trusts it. In a future version,
this could generate a runtime cast/check — but initially it is silent.

## 4. Annotation Format

Type annotations live in declaration metadata, alongside doc strings:

```eu
` { doc: "`double(x)` - double a number."
    type: "number -> number" }
double(x): x * 2
```

### Parsing

The `type` metadata value is a string containing a type expression.
A dedicated type parser (separate from the eucalypt expression parser)
handles this string. The grammar:

```
type       ::= union
union      ::= arrow ( '|' arrow )*
arrow      ::= primary ( '->' primary )*        # right-associative
primary    ::= 'number' | 'string' | 'symbol' | 'bool' | 'null'
             | 'datetime' | 'any' | 'top' | 'never'
             | LOWER_IDENT                        # type variable
             | '[' type ']'                       # homogeneous list
             | 'set'                               # set of primitives
             | 'vec'                               # vec of primitives
             | 'array'                             # ndarray of numbers
             | '{' row '}'                        # record
             | '(' paren_body ')'                 # grouping or tuple
paren_body ::= type                               # grouping: (A -> B)
             | type ','                            # 1-tuple: (A,)
             | type ( ',' type )+  ','?           # n-tuple: (A, B) or (A, B,)
row        ::= field ( ',' field )* ( ',' '..' )? # open if '..' present
field      ::= IDENT ':' type
```

### Conventions

- Multi-argument functions are curried: `a -> b -> c` means `a -> (b -> c)`
- Open records use trailing `..`: `{name: string, ..}`
- Closed records have no `..`: `{name: string}`
- Type variables are single lowercase letters or short lowercase words
- The `..` in records is syntactic (not a named row variable initially)

## 5. Bidirectional Type Checking

The checker uses two modes:

- **Synthesis** (⇒): compute the type of an expression from its
  structure
- **Checking** (⇐): verify an expression has an expected type

### Synthesis Rules

```
Literal n           ⇒ number
Literal "s"         ⇒ string
Literal :s          ⇒ symbol
Literal true/false  ⇒ bool
Literal null        ⇒ null

[e1, e2, ..., en]   ⇒ [T1 | T2 | ... | Tn]    where ei ⇒ Ti

{k1: e1, ..., kn: en, f(...): ...}
                    ⇒ {k1: T1, ..., kn: Tn, ..}
                       where ei ⇒ Ti
                       (functions omitted from record type,
                        open because functions may exist)

Variable x          ⇒ lookup x in type environment

Application (e1 e2) ⇒ instantiate e1's type, check e2, return result type
```

### Checking Rules

```
e ⇐ any             always ok (gradual boundary)
e ⇐ T               if e ⇒ S and S <: T
Lambda ⇐ (A -> B)   bind parameter to A, check body ⇐ B
```

### Catenation (Pipeline) Typing

`data f g` desugars to nested application. The checker processes
left-to-right:

1. Synthesise type of `data` → `T`
2. Check `f` accepts `T` (instantiate type vars), get return type → `U`
3. Check `g` accepts `U`, get return type → `V`
4. Result type is `V`

### Anaphora

`_ + 1` desugars to a lambda with a fresh parameter. The checker:
1. Sees `+` expects `number -> number -> number`
2. Infers `_` must be `number` (from the first argument position)
3. Synthesises the lambda as `number -> number`

`_ + _` (two params): both inferred as `number`, giving
`number -> number -> number`.

### Instantiation

When calling a polymorphic function like `map: (a -> b) -> [a] -> [b]`
with arguments `(str.of, [1, 2])`:

1. Fresh type variables: `a0`, `b0`
2. Check first arg `str.of : number -> string` against `a0 -> b0`
   → learn `a0 = number, b0 = string`
3. Check second arg `[1, 2] : [number]` against `[a0]`
   → consistent with `a0 = number` ✓
4. Return type: `[b0]` = `[string]`

## 6. Prelude Typing Strategy

### Intrinsics (Rust-implemented)

Intrinsic functions (`__MAP`, `__FILTER`, etc.) get type annotations in
the prelude source alongside their doc metadata:

```eu
` { doc: "`map(f, xs)` - apply `f` to each element of list `xs`."
    type: "(a -> b) -> [a] -> [b]" }
map: __MAP

` { doc: "`filter(p?, l)` - elements of `l` satisfying `p?`."
    type: "(a -> bool) -> [a] -> [a]" }
filter: __FILTER

` { doc: "`fold(f, z, l)` - left fold."
    type: "(b -> a -> b) -> b -> [a] -> b" }
fold: __FOLD

` { doc: "`head(xs)` - first element."
    type: "[a] -> a" }
head: __HEAD

` { doc: "`elements(b)` - kv pairs of block."
    type: "{..} -> [(symbol, any)]" }
elements: __ELEMENTS

` { doc: "`lookup(k, b)` - look up key in block."
    type: "symbol -> {..} -> any" }
lookup: __LOOKUP
```

### Functions That Need `any`

Some functions are genuinely hard to type precisely:

```eu
# Return type depends on runtime key — needs any
` { type: "symbol -> {..} -> any" }
lookup: __LOOKUP

# Recursive structural transform
` { type: "(any -> any | null) -> any -> any" }
deep-transform(rule, data): ...

# Block construction from pairs
` { type: "[(symbol, any)] -> {..}" }
block: __BLOCK

# Dynamic merge — row polymorphism would help
` { type: "{..} -> {..} -> {..}" }
merge: __MERGE
```

### Eucalypt-Defined Prelude Functions

Functions defined in eucalypt (not intrinsics) get their types
**inferred** from their bodies, checked against annotations if present:

```eu
` { doc: "`sum(l)` - sum of numbers."
    type: "[number] -> number" }
sum: fold(+, 0)
# Checker: fold: (b -> a -> b) -> b -> [a] -> b
#   with (+) : number -> number -> number and 0 : number
#   → b = number, a = number → [number] -> number ✓
```

### Phased Annotation

Not all prelude functions need annotations from day one. Priority:

1. **Phase 1**: Arithmetic, comparison, string ops, list basics
   (`map`, `filter`, `fold`, `head`, `tail`, `cons`, `reverse`, etc.)
2. **Phase 2**: Block operations (`lookup`, `elements`, `merge`,
   `keys`, `values`, `map-kv`, etc.)
3. **Phase 3**: Higher-order combinators (`compose`, `flip`, `const`,
   etc.), monadic operations
4. **Phase 4**: IO, import/export, advanced features

Unannotated functions default to `any` for all positions.

### Prelude Typing Survey

A survey of the full prelude identifies the following categories of
typing difficulty:

**Straightforward** (majority of prelude):
Most functions type cleanly with type variables. Eucalypt's pattern of
pushing constraints into explicit function parameters means very few
functions need typeclass-like constraints:

```
map: (a -> b) -> [a] -> [b]
filter: (a -> bool) -> [a] -> [a]
fold: (b -> a -> b) -> b -> [a] -> b
qsort: (a -> a -> bool) -> [a] -> [a]
sort-by: (a -> b) -> (b -> b -> bool) -> [a] -> [a]
group-by: (a -> any) -> [a] -> {..}
nub-by: (a -> any) -> [a] -> [a]
max-of-by: (a -> number) -> [a] -> a
```

**Overloaded operators** — typed as union of function types:

| Operator | Type |
|----------|------|
| `+` `-` `*` `/` | `number -> number -> number \| [a] -> [a] -> [a] \| array -> array -> array` |
| `<` `>` `<=` `>=` | `number -> number -> bool \| string -> string -> bool \| symbol -> symbol -> bool \| datetime -> datetime -> bool` |
| `=` `!=` | `a -> a -> bool` (polymorphic equality) |
| `++` | `[a] -> [a] -> [a]` (list-only) |
| `!!` | `[a] -> number -> a \| array -> [number] -> number` |

**Implicit comparison** — `max`/`min` use `<` implicitly. Without
constraints, type as the dominant use case or union:

```
max: number -> number -> number
min: number -> number -> number
# Or with unions: number -> number -> number | string -> string -> string
```

**Dependent on runtime values** — need `any`, unavoidable:

| Function | Type | Why |
|----------|------|-----|
| `lookup` | `symbol -> {..} -> any` | Return type depends on key value |
| `lookup-or` | `symbol -> a -> {..} -> any \| a` | Same |
| `lookup-path` | `[symbol] -> {..} -> any` | Chained dynamic lookup |
| `apply` | `any -> [any] -> any` | Arity depends on list length |
| `cond` | `[(bool, a)] -> a -> a` | Heterogeneous pair list |
| `parse-args` | `{..} -> [string] -> {..}` | Return shape mirrors input |
| `deep-transform` | `(any -> any \| null) -> any -> any` | Recursive structure walk |
| `deep-fold` | `any` throughout | Callback-driven recursion |
| `match?` | `any -> any -> bool` | Structurally overloaded pattern |

**Key insight**: eucalypt's explicit-comparator pattern means we need
almost no typeclass-like constraints. The hard cases are about dependent
types (return type depending on a runtime value), not about Ord/Eq.
These correctly fall to `any` in a gradual system.

## 7. Pipeline Integration

### Position in the Compilation Pipeline

```
parse → desugar → cook → verify → simplify → [TYPE CHECK] → inline → STG
```

The type checker runs on simplified core expressions. At this stage:
- Operator precedence is resolved
- Syntactic sugar is desugared
- Bindings are verified
- Expressions are simplified (but not yet inlined)

### Invocation

**CLI**:
```bash
eu check file.eu              # type-check, report warnings
eu check file.eu --strict     # treat type warnings as errors
eu --type-check file.eu       # check then evaluate (warnings)
```

**LSP**:
- Type checking runs on save / on change (debounced)
- Diagnostics reported as warnings
- Hover shows inferred types
- Autocomplete uses record types for field suggestions

### Output

Type errors are reported as warnings (not errors). This requires new
infrastructure — the existing diagnostic pipeline uses
`Diagnostic::error()` exclusively (both `codespan-reporting` in CLI
and `DiagnosticSeverity::ERROR` in LSP). Warning support must be added:

- **CLI**: use `Diagnostic::warning()` from `codespan-reporting` (already
  supported by the crate, just never used)
- **LSP**: emit `DiagnosticSeverity::WARNING` in
  `src/driver/lsp/diagnostics.rs`
- **Exit code**: type warnings should not cause non-zero exit from
  normal evaluation; `eu check --strict` would promote them to errors

```
warning[T001]: type mismatch
  --> example.eu:5:10
   |
 5 |   double("hello")
   |          ^^^^^^^ expected number, found string
   |
   = note: double is typed as number -> number
```

## 8. Implementation Architecture

### New Module: `src/core/typecheck/`

| File          | Responsibility                                    |
|---------------|---------------------------------------------------|
| `types.rs`    | Type representation (extends `IntrinsicType`)     |
| `parse.rs`    | Parser for type annotation strings                |
| `env.rs`      | Type environment — maps names to type schemes     |
| `subtype.rs`  | Subtyping and consistency checks                  |
| `unify.rs`    | Type variable instantiation and unification       |
| `check.rs`    | Bidirectional type checker on core expressions    |
| `error.rs`    | Type diagnostic messages                          |
| `prelude.rs`  | Bootstrap type environment from prelude metadata  |

### Type Representation

Expanding beyond the existing `IntrinsicType`:

```rust
enum Type {
    // Primitives
    Number,
    String,
    Symbol,
    Bool,
    Null,
    DateTime,

    // Special
    Any,        // gradual dynamic type
    Top,        // supertype of all
    Never,      // bottom type

    // Composite
    List(Box<Type>),                                 // [T]
    Tuple(Vec<Type>),                                // (A, B) or (A,)
    Set,                                             // set (primitives)
    Vec,                                             // vec (primitives)
    Array,                                           // array (numbers)
    Record { fields: BTreeMap<SmolStr, Type>, open: bool },
    Function(Box<Type>, Box<Type>),
    Union(Vec<Type>),

    // Variables
    Var(TypeVarId),
}

struct TypeScheme {
    vars: Vec<TypeVarId>,
    body: Type,
}
```

## 9. Implementation Phases

### Phase 1: Foundation

- Type representation (`types.rs`)
- Type annotation parser (`parse.rs`)
- Type environment (`env.rs`)
- `eu check` CLI command (no-op initially, reports parse errors in
  annotations)
- Extract type annotations from prelude metadata

### Phase 2: Primitive Type Checking

- Subtyping for primitive types and unions
- Synthesis for literals, lists, simple blocks
- Checking for annotated functions with primitive types
- First prelude annotations (arithmetic, string ops)
- Basic diagnostic output

### Phase 3: Polymorphism and Functions

- Type variable instantiation at call sites
- Catenation / pipeline type checking
- Anaphora type inference
- Generic prelude annotations (`map`, `filter`, `fold`, etc.)
- Partial application typing

### Phase 4: Records and Blocks

- Open/closed record types
- Width and depth subtyping
- Block literal synthesis
- Lookup typing
- Record field autocomplete in LSP

### Phase 5: LSP Integration

- Hover: show inferred types
- Diagnostics: type warnings on save
- Autocomplete: field names from record types
- Go to type definition

### Phase 6: Refinements (Future)

- Type narrowing via predicates (`number?`, `string?`)
- Named row variables
- Constrained type variables
- Literal symbol types (`:active | :inactive`)
- Tuple destructuring in type annotations

## 10. Inline Type Assertions

### Declaration Annotations (primary mechanism)

The main annotation mechanism is declaration-level metadata:

```eu
` { type: "[number] -> number" }
sum: fold(+, 0)
```

This covers the vast majority of cases — annotate function signatures
and property types.

### Inline Assertions (deferred)

Inline type assertions on arbitrary expressions would be useful but
present an implementation challenge. The obvious syntax `expr // { type:
"T" }` uses the metadata merge operator, but `//` is a prelude-defined
function, not syntax. By the time the type checker sees core
expressions, it's an opaque function call — special-casing "merge where
the second arg is a block with a `type` key" would be fragile.

Options for future inline assertions:

1. **Dedicated syntax** — e.g. `expr :: "T"` as a type assertion
   operator. Requires a parser change but is unambiguous.
2. **Desugar-time recognition** — the desugarer could recognise
   `// { type: "..." }` patterns and emit a core annotation node
   before it becomes a merge call.
3. **Pragma on merge** — the type checker recognises
   `merge(expr, {type: "..."})` as a type assertion. Fragile.

**Decision**: defer inline assertions. Declaration annotations plus
inference cover the primary use cases. Revisit if there is demand,
likely via option 1 or 2.

## 11. Resolved Design Decisions

1. **Recursive types** — low urgency but the design must accommodate
   them. The type alias mechanisms (section 12) support recursive
   references — e.g. `{ types: { Tree: "{value: number, left: Tree |
   null, right: Tree | null}" } }`. Deferred to Phase 6+. The type
   representation (`Type` enum) should use indirection (`Box`/`Rc`)
   throughout so recursive types can be added without restructuring.

2. **Module-level type exports** — yes. When importing a `.eu` file,
   the importer sees its type annotations. The type environment is
   exported alongside the compiled module. This means imported functions
   get full type checking at call sites.

3. **Type annotation for operators** — same metadata approach works.
   Operator declarations in the prelude use backtick metadata:
   ```eu
   ` { doc: "`l + r` - adds l and r."
       type: "number -> number -> number" }
   (l + r): __ADD(l, r)
   ```

4. **Overloaded operators** — `+` works on numbers, lists, and arrays.
   Union of function types:
   ```
   type: "number -> number -> number | [a] -> [a] -> [a] | array -> array -> array"
   ```
   The checker tries each alternative and succeeds if any match. This
   is equivalent to TypeScript's function overloads.

5. **Interaction with `//=` assertions** — yes, the checker can verify
   `expr //= value` for type consistency. If `expr` has type `T` and
   `value` has type `U`, warn if `T` and `U` are inconsistent. Low-
   hanging fruit for Phase 2.

## 12. Type Aliases

Type aliases name complex types for reuse in annotations. Two
complementary mechanisms:

### Unit Metadata `types:` Block

For types without a natural default value, or for recursive types,
declare aliases in the unit metadata block at the top of the file:

```eu
{ import: "data.eu"
  types: { Person: "{name: string, age: number, email: string | null, ..}"
           Response: "{status: number, body: string | null}"
           Tree: "{value: number, left: Tree | null, right: Tree | null}" } }
```

The `types` key maps alias names to type strings. These are available
throughout the file (and to importers — see section 11.2).

### Declaration-Derived Aliases via `type-def:`

When a declaration provides a canonical instance of a type, `type-def`
names an alias inferred from its value:

```eu
` { type-def: "Point" }
origin: { x: 0, y: 0 }
# Defines: Point = {x: number, y: number, ..}
# And: origin : Point
```

The alias shape is inferred from the declaration's value. Member
annotations refine the inferred type:

```eu
` { type-def: "Person" }
nobody: {
  ` { type: "string" }
  name: ""
  ` { type: "number" }
  age: 0
  ` { type: "string | null" }
  email: null
}
# Defines: Person = {name: string, age: number, email: string | null, ..}
```

Without annotations, `email: null` would infer as `null` not
`string | null` — member annotations earn their keep here.

`type-def` and `type` can be combined — `type` provides an explicit
shape (overriding inference), `type-def` names it:

```eu
` { type-def: "Point"
    type: "{x: number, y: number}" }
origin: { x: 0, y: 0 }
```

### Usage in Annotations

Once defined (by either mechanism), aliases are used by name in type
annotations:

```eu
` { type: "Point -> Point -> number" }
distance(a, b): ...

` { type: "[Person] -> [string]" }
names: map(_.name)
```

### Summary

| Mechanism      | Syntax                          | Use case                         |
|----------------|---------------------------------|----------------------------------|
| Unit metadata  | `{ types: { Name: "..." } }`   | Abstract types, recursive types  |
| `type-def:`    | `` ` { type-def: "Name" } ``   | Types with a canonical instance  |

## 13. Additional Resolved Decisions

6. **`deep-transform` and structural recursion** — these are inherently
   `any -> any`. Recursive type aliases don't help — a `Json`-like
   recursive union is effectively `any` with extra steps. The rule
   function passed to `deep-transform` gets typed at the point of use
   via inference from its body.

7. **Prelude namespace typing** — `str`, `arr`, `set`, etc. are block
   literals in the prelude. Each function inside the block gets a `type`
   annotation in its metadata, just like top-level functions:

   ```eu
   str: {
     ` { doc: "of(e) - convert e to string."
         type: "any -> string" }
     of: __STR

     ` { doc: "length(s) - return length of string s."
         type: "string -> number" }
     length: __STR_LEN
     ...
   }
   ```

   The checker synthesises the block's record type from its members:
   `str : {of: any -> string, length: string -> number, ..}`. Then
   `str.length` resolves via record field lookup to `string -> number`.
   This is the same mechanism as any other block — no special-casing
   needed for namespaces.

   Note: function declarations (with parameters) are not rendered in
   output but are part of the record type for checking purposes. The
   checker treats all block members equally.

## 14. Future Considerations (Out of Scope)

### Embedded Type Syntax

Type annotations currently live in strings (`type: "number -> number"`).
A future enhancement could embed types in eucalypt syntax directly.
However, eucalypt syntax clashes heavily with type notation: `->` is the
`const` operator, `(a, b)` is function application, `..` has other
meanings. Symbols could represent type variables (`:a`), but arrow types
and tuple types would need new syntax. Strings are honest about being a
separate language. Deferred.

### Metadata-Typed Values

Lenses require `fmap` metadata, IO actions carry `io-action` metadata.
Expressing "T with metadata M" (e.g. `number @ {fmap: ...}`) could
constrain lens pipelines and IO chains. However, metadata is attached at
runtime via `with-meta`/`//` and flows through operations unpredictably.
Tracking it statically would add significant complexity. Deferred —
lenses and IO actions use opaque type aliases or `any` initially.

### Structural Operator Constraints

The prelude survey (section 6) shows that eucalypt's explicit-comparator
pattern means very few functions need implicit constraints. The main
cases are `max`/`min` (implicit `<`) and comparison operators (overloaded
across types). These are handled via union types initially.

However, the design should leave a clean path to structural operator
constraints. The proposed future syntax:

```
# min requires < to work on a
type: "<(a, a) => a -> a -> a"

# general form: fn(arg-types) => body-type
type: "str.of(a) => [a] -> [string]"
```

This reads as: "this type is valid in contexts where the named function
accepts the given argument types." No named typeclasses needed — the
constraint references the operator directly, consistent with eucalypt's
structural philosophy.

**Research context**: this approach aligns with several established
designs:

- **Structural operator constraints with gradual typing** — Xie et al.
  (2018, "Consistent Subtyping for All") shows constrained polymorphism
  works in a gradual setting: constraints are checked when static,
  deferred to `any` when the gradual type is involved
- **Row polymorphism** (Rémy, PureScript) — if operators are viewed as
  record fields on a type's "operations" record, operator constraints
  become row constraints. Eucalypt's namespace blocks already work this
  way
- **Go type constraints** — Go 1.18 enumerates concrete types in
  constraints (`~int | ~float64 | ~string`). Our union overloads are
  the same approach. Structural constraints generalise this
- **MLsub** (Dolan & Mycroft, 2017) — algebraic subtyping infers
  structural constraints without explicit annotations. A possible
  inspiration for inference-driven constraints

**Forwards compatibility**: union overloads are concrete specialisations
of structural constraints. `min: number -> number -> number | string ->
string -> string` is a valid (less general) annotation that would be
subsumed by `min: <(a, a) => a -> a -> a` if constraints are added
later. Existing annotations need not change.

**Implementation path**:

1. **Now**: union overloads for operators (`+`, `<`, etc.)
2. **Later**: add `Constraint` to the `Type` enum, parse
   `fn(args) =>` prefix in type strings, resolve constraints at
   call sites by checking against known operator overloads
3. **Future**: infer constraints from function bodies (MLsub-style),
   potentially eliminating the need for explicit constraint annotations

The type representation should reserve space for constraints from the
start:

```rust
struct TypeScheme {
    vars: Vec<TypeVarId>,
    constraints: Vec<Constraint>,  // empty initially
    body: Type,
}

struct Constraint {
    function: String,       // "<", "str.of", etc.
    args: Vec<Type>,        // types the function must accept
}
```
