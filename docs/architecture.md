# Eucalypt Architecture

This document provides a comprehensive overview of Eucalypt's design and implementation architecture.

## Overview

Eucalypt is a functional programming language and tool for generating, templating, rendering, and processing structured data formats like YAML, JSON, and TOML. Written in Rust (~44,000 lines), it features a classic multi-phase compiler design with an STG (Spineless Tagless G-machine) runtime for lazy evaluation.

## System Architecture

### High-Level Pipeline

```
Source Code (*.eu files)
        │
        ▼
┌───────────────────────┐
│    Parsing Phase      │  src/syntax/
│  Lexer → Parser → AST │
└───────────────────────┘
        │
        ▼
┌───────────────────────┐
│     Core Phase        │  src/core/
│  Desugar → Cook →     │
│  Transform → Verify   │
└───────────────────────┘
        │
        ▼
┌───────────────────────┐
│   Evaluation Phase    │  src/eval/
│  STG Compile → VM →   │
│  Memory Management    │
└───────────────────────┘
        │
        ▼
┌───────────────────────┐
│    Export Phase       │  src/export/
│  JSON/YAML/TOML/etc   │
└───────────────────────┘
```

### Module Structure

```
eucalypt/
├── src/
│   ├── bin/eu.rs           # CLI entry point
│   ├── lib.rs              # Library root
│   ├── common/             # Shared utilities
│   ├── syntax/             # Parsing and AST
│   │   └── rowan/          # Rowan-based incremental parser
│   ├── core/               # Core expression representation
│   │   ├── desugar/        # AST to core transformation
│   │   ├── cook/           # Operator fixity resolution
│   │   ├── transform/      # Expression transformations
│   │   ├── simplify/       # Optimisation passes
│   │   ├── inline/         # Inlining passes
│   │   ├── verify/         # Validation
│   │   └── analyse/        # Program analysis
│   ├── eval/               # Evaluation engine
│   │   ├── stg/            # STG syntax and compiler
│   │   ├── machine/        # Virtual machine
│   │   └── memory/         # Heap and garbage collection
│   ├── driver/             # CLI orchestration
│   ├── export/             # Output format generation
│   └── import/             # Input format parsing
├── lib/                    # Standard library (eucalypt source)
│   ├── prelude.eu          # Core prelude
│   ├── test.eu             # Test framework
│   └── markup.eu           # Markup utilities
└── docs/                   # Documentation
```

## Parsing Pipeline

The parsing pipeline transforms source text into a structured AST using Rowan, an incremental parsing library that preserves full source fidelity including whitespace and comments.

### Lexer

*Implementation*: `src/syntax/rowan/lex.rs`, `src/syntax/rowan/string_lex.rs`

The lexer (`Lexer<C>`) tokenises source text into a stream of `SyntaxKind` tokens:

```rust
pub struct Lexer<C: Iterator<Item = char>> {
    chars: Peekable<C>,           // Character stream with lookahead
    location: ByteIndex,           // Source position tracking
    last_token: Option<SyntaxKind>, // Context for disambiguation
    whitespace_since_last_token: bool,
    token_buffer: VecDeque<(SyntaxKind, Span)>,
}
```

**Key features:**
- Unicode-aware identifier and operator recognition
- Context-sensitive tokenisation (distinguishes `OPEN_PAREN` from `OPEN_PAREN_APPLY`)
- String pattern lexing with interpolation support (`"Hello {name}"`)
- Preserves trivia (whitespace, comments) for full-fidelity AST

**Token categories:**
- Delimiters: `{ } ( ) [ ] : , \``
- Identifiers: `foo`, `'quoted name'`, `+`, `&&`
- Literals: Numbers, strings, symbols (`:keyword`)
- Annotations: Whitespace, comments (`#`)

### Parser

*Implementation*: `src/syntax/rowan/parse.rs`

The parser uses an event-driven recursive descent approach:

```rust
pub struct Parser<'text> {
    tokens: Vec<(SyntaxKind, &'text str)>,
    next_token: usize,
    sink_stack: Vec<Box<dyn EventSink>>,
    errors: Vec<ParseError>,
}
```

**Parse events:**
```rust
enum ParseEvent {
    StartNode(SyntaxKind),  // Begin syntax node
    Finish,                  // Complete node
    Token(SyntaxKind),      // Include token
}
```

**Key parsing methods:**
- `parse_unit()` - Top-level file (no braces required)
- `parse_expression()` / `parse_soup()` - Expression sequences
- `parse_block_expression()` - `{ ... }` blocks with declarations
- `parse_string_pattern()` - Interpolated strings

The parser maintains error recovery for LSP support, collecting errors while continuing to parse.

### AST

*Implementation*: `src/syntax/rowan/ast.rs`

The AST uses a two-layer design:
1. **SyntaxNode** (Rowan) - Rich, source-preserving tree
2. **AST Nodes** - Typed wrappers via macros

```rust
macro_rules! ast_node {
    ($ast:ident, $kind:ident) => {
        pub struct $ast(SyntaxNode);
        impl AstNode for $ast { ... }
    }
}
```

**Key AST types:**

| Type | Purpose |
|------|---------|
| `Unit` | Top-level file structure |
| `Block` | Enclosed `{ ... }` block |
| `Declaration` | Property/function declaration |
| `DeclHead` | Declaration name (before `:`) |
| `DeclBody` | Declaration value (after `:`) |
| `Soup` | Unordered expression sequence |
| `List` | List expression `[a, b, c]` |
| `Name` | Identifier reference |
| `Literal` | Literal value |
| `StringPattern` | Interpolated string |

## Core Expression Representation

The core representation is an intermediate language that facilitates powerful transformations while maintaining source information for error reporting.

### Expression Type

*Implementation*: `src/core/expr.rs`

The `Expr<T>` enum (where `T: BoundTerm<String>`) represents all expression forms:

```rust
pub enum Expr<T> {
    // Variables
    Var(Smid, Var<String>),           // Free or bound variable

    // Primitives
    Literal(Smid, Primitive),          // Number, string, symbol, bool, null

    // Binding forms
    Let(Smid, LetScope<T>, LetType),   // Recursive let binding
    Lam(Smid, bool, LamScope<T>),      // Lambda abstraction

    // Application
    App(Smid, T, Vec<T>),              // Function application

    // Data structures
    List(Smid, Vec<T>),                // List literal
    Block(Smid, BlockMap<T>),          // Object/record literal

    // Operators (pre-cooking)
    Operator(Smid, Fixity, Precedence, T),
    Soup(Smid, Vec<T>),                // Unresolved operator soup

    // Anaphora (implicit parameters)
    BlockAnaphor(Smid, ...),
    ExprAnaphor(Smid, ...),

    // Access
    Lookup(Smid, T, String, Option<T>), // Property access

    // Metadata
    Meta(Smid, T, T),                   // Expression with metadata

    // Intrinsics
    Intrinsic(Smid, String),           // Built-in function reference

    // Error nodes
    ErrUnresolved, ErrRedeclaration, ...
}
```

**Primitive types:**
```rust
pub enum Primitive {
    Str(String),
    Sym(String),
    Num(Number),
    Bool(bool),
    Null,
}
```

**Standard wrapper**: `RcExpr` provides reference-counted immutable expressions with substitution and transformation methods.

### Transformation Pipeline

The core pipeline transforms expressions through several phases:

```
AST → Desugar → Cook → Simplify → Inline → Verify → STG
```

#### Desugaring

*Implementation*: `src/core/desugar/`

Transforms parsed AST into core expressions:
- Converts block declarations into recursive let bindings
- Extracts targets and documentation metadata
- Handles imports and cross-file references
- Processes both native AST and embedded data (JSON/YAML)

#### Cooking

*Implementation*: `src/core/cook/`

Resolves operator precedence and anaphora:
1. **Fixity distribution** - Propagate operator precedence info
2. **Anaphor filling** - Infer missing implicit parameters (`(+ 10)` → `(_ + 10)`)
3. **Shunting yard** - Apply precedence climbing to linearise operator soup
4. **Anaphor processing** - Wrap lambda abstractions around anaphoric expressions

Example transformation:
```
(+ 10)        →  (λ _ . (_ + 10))
a + b * c     →  (+ a (* b c))      // with standard precedence
```

#### Simplification and Inlining

*Implementation*: `src/core/simplify/`, `src/core/inline/`

- **Compression** - Remove eliminated bindings
- **Pruning** - Dead code elimination
- **Inlining** - Inline marked expressions

#### Verification

*Implementation*: `src/core/verify/`

Validates transformed expressions before STG compilation:
- Binding verification
- Content validation

## STG Compilation and Evaluation Model

Eucalypt uses a Spineless Tagless G-machine (STG) as its evaluation model, providing lazy evaluation with memoisation.

### STG Syntax

*Implementation*: `src/eval/stg/syntax.rs`

The STG syntax represents executable code:

```rust
pub enum StgSyn {
    Atom { evaluand: Ref },              // Value or reference
    Case { scrutinee, branches, fallback }, // Pattern matching (evaluation point)
    Cons { tag: Tag, args: Vec<Ref> },   // Data constructor
    App { callable: Ref, args: Vec<Ref> }, // Function application
    Bif { intrinsic: u8, args: Vec<Ref> }, // Built-in intrinsic
    Let { bindings: Vec<LambdaForm>, body }, // Non-recursive let
    LetRec { bindings: Vec<LambdaForm>, body }, // Recursive let
    Ann { smid: Smid, body },            // Source annotation
    Meta { meta: Ref, body: Ref },       // Metadata wrapper
    DeMeta { scrutinee, handler, or_else }, // Metadata destructure
    BlackHole,                            // Uninitialized marker
}
```

**Reference types:**
```rust
pub enum Reference<T> {
    L(usize),  // Local environment index
    G(usize),  // Global environment index
    V(T),      // Direct value (Native)
}
```

**Lambda forms** control laziness:
- `Lambda` - Function with explicit arity
- `Thunk` - Lazy expression (evaluated and updated in-place)
- `Value` - Already in WHNF (no update needed)

### STG Compiler

*Implementation*: `src/eval/stg/compiler.rs`

The compiler transforms core expressions to STG syntax:

```rust
impl Compiler {
    fn compile_body(&mut self, expr: &RcExpr) -> ProtoSyntax;
    fn compile_binding(&mut self, expr: &RcExpr) -> ProtoBinding;
    fn compile_lambda(&mut self, expr: &RcExpr) -> ProtoSyntax;
    fn compile_application(&mut self, f: &RcExpr, args: &[RcExpr]) -> ProtoSyntax;
}
```

**Key decisions:**
- **Thunk creation**: Expressions not in WHNF and used more than once become thunks
- **WHNF detection**: Constructors, native values, and metadata wrappers are WHNF
- **Deferred compilation**: `ProtoSyntax` allows deferring binding construction until environment size is known

### Virtual Machine

*Implementation*: `src/eval/machine/vm.rs`

The STG machine is a state machine executing closures:

```rust
pub struct MachineState {
    root_env: SynEnvPtr,           // Empty root environment
    closure: SynClosure,           // Current (code, environment) pair
    globals: SynEnvPtr,            // Global bindings
    stack: Vec<Continuation>,      // Continuation stack
    terminated: bool,
    annotation: Smid,              // Current source location
}
```

**Execution loop:**
```rust
fn run(&mut self) {
    while !self.terminated {
        if self.gc_check_needed() {
            self.collect_garbage();
        }
        self.step();
    }
}
```

**Instruction dispatch** (`handle_instruction`):

| Code Form | Action |
|-----------|--------|
| `Atom` | Resolve reference; push Update continuation if thunk |
| `Case` | Push Branch continuation, evaluate scrutinee |
| `Cons` | Return data constructor |
| `App` | Push ApplyTo continuation, evaluate callable |
| `Bif` | Execute intrinsic directly |
| `Let` | Allocate environment frame, continue in body |
| `LetRec` | Allocate frame with backfilled recursive references |

### Continuations

*Implementation*: `src/eval/machine/cont.rs`

Four continuation types manage control flow:

1. **Branch** - Pattern matching branches for CASE
2. **Update** - Deferred thunk update (memoisation)
3. **ApplyTo** - Pending arguments for function application
4. **DeMeta** - Metadata destructuring handler

### Lazy Evaluation

Laziness is achieved through thunks and updates:

1. **Thunk creation** (compile time): Non-WHNF expressions become `LambdaForm::Thunk`
2. **Thunk evaluation** (runtime): When a thunk is entered, push Update continuation
3. **Memoisation**: After evaluation, update the environment slot with the result

```rust
// When entering a local reference
if closure.update() {
    stack.push(Continuation::Update { environment, index });
}

// After evaluation completes
Continuation::Update { environment, index } => {
    self.update(environment, index);  // Replace thunk with result
}
```

## Memory Management and Garbage Collection

Eucalypt uses an Immix-inspired memory layout with mark-and-sweep collection.

### Memory Layout

*Implementation*: `src/eval/memory/heap.rs`, `src/eval/memory/bump.rs`

```
Block (32KB)
├── Line 0 (128B) ┐
├── Line 1 (128B) │ 256 lines per block
├── ...           │
└── Line 255      ┘
```

**Size classes:**
- **Small** (< 128 bytes) - Single line
- **Medium** (128B - 32KB) - Multiple lines within block
- **Large** (> 32KB) - Dedicated Large Object Block

**Heap state:**
```rust
pub struct HeapState {
    head: Option<BumpBlock>,           // Active small allocation
    overflow: Option<BumpBlock>,       // Active medium allocation
    recycled: VecDeque<BumpBlock>,     // Blocks with reusable holes
    rest: VecDeque<BumpBlock>,         // Used blocks pending collection
    lobs: Vec<LargeObjectBlock>,       // Large objects
}
```

### Object Headers

*Implementation*: `src/eval/memory/header.rs`

Every object has a 16-byte header:
```rust
pub struct AllocHeader {
    bits: HeaderBits,                  // Mark bit + forwarded flag
    alloc_length: u32,                 // Object size
    forwarded_to: Option<NonNull<()>>, // For potential evacuation
}
```

### Garbage Collection

*Implementation*: `src/eval/memory/collect.rs`

**Mark phase:**
1. Reset line maps across all blocks
2. Breadth-first root scanning from machine state
3. Transitive closure following object references
4. Mark lines containing live objects

**Sweep phase:**
1. Scan line maps in each block
2. Identify holes (2+ consecutive free lines)
3. Move recyclable blocks to recycled list

**Collection triggering:**
- When `--heap-limit-mib` is set and limit exceeded
- Check performed every 500 VM execution steps
- Emergency collection on allocation failure

See `gc-implementation.md` for detailed analysis.

## The Prelude and Standard Library

### Intrinsic Functions

*Implementation*: `src/eval/intrinsics.rs`, `src/eval/stg/`

Built-in functions are implemented in Rust and indexed by position:

**Categories:**
- **Control flow**: `__IF`, `__PANIC`, `__TRUE`, `__FALSE`, `__NULL`
- **Lists**: `__CONS`, `__HEAD`, `__TAIL`, `__NIL`, `__REVERSE`
- **Blocks**: `__MERGE`, `__DEEPMERGE`, `__ELEMENTS`, `__BLOCK`, `__LOOKUP`
- **Arithmetic**: `__ADD`, `__SUB`, `__MUL`, `__DIV`, `__MOD`, comparisons
- **Strings**: `__STR`, `__SPLIT`, `__JOIN`, `__MATCH`, `__FMT`
- **Metadata**: `__META`, `__WITHMETA`
- **Time**: `__ZDT`, `__ZDT.PARSE`, `__ZDT.FORMAT`
- **I/O**: `__io.ENV`, `__io.EPOCHTIME`
- **Emission**: `__RENDER`, `__EMIT*` family

Each intrinsic implements the `StgIntrinsic` trait with direct access to machine state.

### Prelude

*Implementation*: `lib/prelude.eu`

The prelude (~29KB) is written entirely in eucalypt, wrapping intrinsics with ergonomic functions:

**List functions:**
- `take`, `drop`, `nth` (`!!`), `fold`/`foldr`, `map`, `filter`
- `append` (`++`), `concat`, `reverse`, `zip`, `group-by`, `qsort`

**Block functions:**
- `merge-all`, `keys`, `values`, `map-kv`, `map-keys`, `map-values`
- `lookup-path`, `alter-value`, `update-value`

**Combinators:**
- `identity`, `const`, `compose` (`∘`, `;`), `flip`, `curry`, `uncurry`

**String functions:**
- `str.split`, `str.join`, `str.match`, `str.fmt`, `str.letters`

**Loading:**
- Prelude is embedded in the binary at compile time
- Loaded by default unless `--no-prelude` / `-Q` flag is used

## Driver and CLI Architecture

### Entry Point

*Implementation*: `src/bin/eu.rs`, `src/driver/options.rs`

The CLI uses `clap v4` with derive macros:

```rust
#[derive(Parser)]
struct EucalyptCli {
    #[command(subcommand)]
    command: Option<Commands>,
    files: Vec<String>,
    // Global options...
}

enum Commands {
    Run(RunArgs),
    Test(TestArgs),
    Dump(DumpArgs),
    Fmt(FmtArgs),
    Explain(ExplainArgs),
    ListTargets(ListTargetsArgs),
    Version,
}
```

### Modes of Operation

**Run (default):**
```bash
eu file.eu                    # Implicit run
eu -e "expression" file.eu    # With evaluand
eu -x json file.eu            # JSON output
eu -t target file.eu          # Select target
```

**Test:**
```bash
eu test tests/              # Run all tests in directory
eu test -t specific file.eu  # Run specific test target
```

**Format:**
```bash
eu fmt file.eu              # Print formatted to stdout
eu fmt --write file.eu      # Modify in place
eu fmt --check file.eu      # Check formatting (exit 1 if needs format)
```

**Dump:**
```bash
eu dump ast file.eu          # Dump AST
eu dump desugared file.eu    # Dump after desugaring
eu dump stg file.eu          # Dump STG syntax
eu dump runtime              # Dump intrinsic definitions
```

### Output Formats

*Implementation*: `src/export/`

Emitters for each format implement the `Emitter` trait:
- **YAML** (`yaml.rs`) - Uses `yaml_rust`, supports tags from metadata
- **JSON** (`json.rs`) - Uses `serde_json`
- **TOML** (`toml.rs`) - Structured output
- **Text** (`text.rs`) - Plain text
- **EDN** (`edn.rs`) - Clojure-like format
- **HTML** (`html.rs`) - Markup with serialisation

### Input Handling

Inputs are merged in order:
1. **Prologue**: Prelude, config files, build metadata, IO block
2. **Explicit**: Files and options (`-c`/`--collect-as`)
3. **Epilogue**: CLI evaluand (`-e`)

Names from earlier inputs are available to later inputs.

## Key Design Decisions and Trade-offs

### Why STG?

The STG machine provides a well-defined reference point for lazy functional language implementation:
- Clear semantics for lazy evaluation with memoisation
- Established compilation strategies
- Potential for future optimisations

*Trade-off*: More complex than direct interpretation but provides a solid foundation.

### Why Rowan for Parsing?

Rowan provides:
- Incremental parsing for IDE support
- Full source fidelity (preserves whitespace and comments)
- Error recovery for partial parsing

*Trade-off*: More complex API than traditional parser generators.

### Core as Intermediate Language

The core representation enables powerful transformations:
- User-definable operator precedence resolved by syntax transformation
- Block semantics (binding vs structuring) separated cleanly
- Source information preserved through `Smid` for error reporting

*Trade-off*: Additional compilation phase, but enables experimentation.

### Block Duality

Eucalypt blocks serve two roles:
1. **Name binding** - Like `let` expressions
2. **Data structuring** - Like objects/records

The core phase separates these into distinct `Let` and `Block` expressions.

*Trade-off*: Elegant surface syntax at cost of semantic complexity.

### Immix-Inspired GC

The memory layout provides:
- Efficient bump allocation
- Cache-friendly organisation
- Effective hole reuse

*Current limitation*: No evacuation/compaction (mark-sweep only).

See `gc-implementation.md` for detailed analysis.

### Embedded Prelude

The prelude is:
- Written in eucalypt itself
- Compiled into the binary
- Loaded unless explicitly disabled

*Benefit*: Dogfooding, consistent semantics
*Trade-off*: Slower startup if prelude is large

## Performance Characteristics

### Compilation
- Parsing: O(n) with incremental support
- Desugaring: O(n) pass over AST
- Cooking: O(n log n) for operator precedence resolution
- STG compilation: O(n) with binding analysis

### Execution
- Bump allocation: O(1) for most objects
- Thunk updates: O(1) memoisation
- GC: O(live objects) for marking, O(total blocks) for sweeping

### Memory
- 16-byte header overhead per object
- Block-level allocation granularity
- Large objects get dedicated blocks

## Code Organisation Summary

| Directory | Purpose | Key Files |
|-----------|---------|-----------|
| `src/syntax/rowan/` | Incremental parsing | `lex.rs`, `parse.rs`, `ast.rs` |
| `src/core/` | Expression representation | `expr.rs` |
| `src/core/desugar/` | AST to core | `desugarer.rs` |
| `src/core/cook/` | Operator resolution | `shunt.rs`, `fixity.rs` |
| `src/eval/stg/` | STG syntax and compiler | `syntax.rs`, `compiler.rs` |
| `src/eval/machine/` | Virtual machine | `vm.rs`, `cont.rs`, `env.rs` |
| `src/eval/memory/` | Heap and GC | `heap.rs`, `collect.rs` |
| `src/driver/` | CLI orchestration | `options.rs`, `eval.rs` |
| `src/export/` | Output formats | `yaml.rs`, `json.rs` |
| `lib/` | Standard library | `prelude.eu` |

## Further Reading

- `implementation.md` - Brief implementation overview
- `gc-implementation.md` - Detailed GC documentation
- `command-line.md` - CLI usage guide
- `syntax.md` - Language syntax reference
- `operators-and-identifiers.md` - Operator definitions
- `anaphora-and-lambdas.md` - Implicit parameter handling
