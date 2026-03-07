# What is Eucalypt?

**Eucalypt** is a tool, and a little language, for generating,
templating, rendering and processing structured data formats like
YAML, JSON and TOML.

If you use text-based templating to process these formats or you pipe
these formats through several different tools or build steps,
eucalypt might be able to help you generate your output more cleanly
and with fewer cognitive somersaults.

## Key Features

- A concise native syntax for defining data, functions, and operators
- A simple embedding into YAML files for in-place manipulation (a la
  templating)
- Facilities for manipulating blocks (think JSON objects, YAML mappings)
- String interpolation and regular expressions
- An ergonomic command line interface with environment variable access
- Metadata annotations and numerous extension points
- A [prelude](../reference/prelude/index.md) of built-in functions acting
  as a standard library

## Supported Formats

**Input:** YAML, JSON, JSON Lines, TOML, EDN, XML, CSV, plain text,
and eucalypt's own `.eu` syntax.

**Output:** YAML, JSON, TOML, EDN, or plain text.

## When to Use Eucalypt

Eucalypt is a good fit when you need to:

- Transform data between structured formats (e.g. JSON to YAML)
- Generate configuration files with shared logic
- Query and filter structured data from the command line
- Template YAML or JSON with embedded expressions
- Build data processing pipelines

## Learn More

- [Quick Start](quick-start.md) -- install eucalypt and run your first program
- [Lightning Tour](index.md#a-lightning-tour) -- a quick taste of the syntax
- [The Eucalypt Guide](../guide/blocks-and-declarations.md) -- a progressive tutorial
