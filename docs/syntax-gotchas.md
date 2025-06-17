# Syntax Gotchas

This document records unintuitive consequences of Eucalypt's syntax design decisions that can lead to subtle bugs or confusion. These are documented here until such time as we can implement a linter or improve error messages to catch them more clearly.

## Operator Precedence Issues

### Field Access vs Catenation

**Problem**: The lookup operator (`.`) has higher precedence (90) than catenation (precedence 20), which can lead to unexpected parsing.

**Gotcha**: Writing `objects head.id` is parsed as `objects (head.id)` rather than `(objects head).id`.

**Example**:
```eu
# This doesn't work as expected:
objects: range(0, 5) map({ id: _ })
result: objects head.id  # Parsed as: objects (head.id)

# Correct syntax requires parentheses:
result: (objects head).id  # Explicitly groups the field access
```

**Error Message**: When this occurs, you may see confusing errors like:
- `cannot return function into case table without default`
- `bad index 18446744073709551615 into environment` (under memory pressure)

**Solution**: Always use parentheses to group the expression you want to access fields from:
- Use `(expression).field` instead of `expression target.field`
- Be explicit about precedence when combining catenation with field access

**Reference**: Operator precedences are defined in `src/core/metadata.rs:165-186`:
- `lookup` (`.`): precedence 90
- `cat` (catenation): precedence 20

## Anaphora and Function Syntax

### Lambda Syntax Does Not Exist

**Problem**: Eucalypt does not have lambda expressions like other functional languages.

**Gotcha**: Attempting to write lambda-style syntax will cause syntax errors.

**Invalid Examples**:
```eu
# These syntaxes DO NOT exist in Eucalypt:
map(\x -> x + 1)     # Invalid
map(|x| x + 1)       # Invalid  
map(fn(x) => x + 1)  # Invalid
map(λx.x + 1)        # Invalid
```

**Correct Approach**: Use anaphora (`_`, `_0`, `_1`, etc.) or define named functions:
```eu
# Using anaphora:
map(_ + 1)

# Using named function:
add-one(x): x + 1
map(add-one)

# Using block with anaphora for complex expressions:
map({ result: _ + 1, doubled: _ * 2 })
```

**Reference**: See `docs/anaphora-and-lambdas.md` for detailed explanation of anaphora usage.

## Future Improvements

These gotchas highlight areas where the language could benefit from:

1. **Better Error Messages**: More specific error messages when precedence issues occur
2. **Linting Rules**: Static analysis to catch common precedence mistakes  
3. **IDE Support**: Syntax highlighting and warnings for ambiguous expressions
4. **Documentation**: Better examples showing correct precedence usage

## Contributing

When you encounter a new syntax gotcha, please add it to this document with:
- A clear description of the problem
- Example of incorrect usage
- Example of correct usage  
- The error message(s) that result
- Reference to relevant code or documentation