# Working with Data

In this chapter you will learn:

- How to process JSON, YAML, TOML, CSV, and XML data
- How to convert between formats on the command line
- Patterns for querying and transforming structured data
- How to combine multiple data sources

## Format Conversion

The simplest use of eucalypt is converting between data formats. By
default, output is YAML:

```sh
# JSON to YAML
eu data.json

# YAML to JSON
eu data.yaml -j

# JSON to TOML
eu data.json -x toml
```

## Processing JSON

Pipe JSON from other tools into eucalypt:

```sh
curl -s https://api.example.com/users | eu -e 'map(_.name)'
```

Or process a JSON file:

```sh
eu -e 'users filter(_.active) map(_.email)' data.json
```

## Processing YAML

YAML files are read natively. All YAML features including anchors,
aliases, and merge keys are supported:

```yaml
# config.yaml
defaults: &defaults
  timeout: 30
  retries: 3

production:
  <<: *defaults
  debug: false
```

```sh
eu config.yaml -e 'production'
```

```yaml
timeout: 30
retries: 3
debug: false
```

### YAML Timestamps

YAML timestamps are automatically converted to date-time values:

```yaml
created: 2024-03-15
updated: 2024-03-15T14:30:00Z
```

Quote the value to keep it as a string: `created: "2024-03-15"`.

## Processing TOML

```sh
eu config.toml -e 'database.port'
```

```yaml
5432
```

## Processing CSV

CSV files are imported as a list of blocks, where each row becomes a
block with column headers as keys:

```sh
eu -e 'rows filter(_.age num > 30)' rows=people.csv
```

CSV values are always strings. Use `num` to convert to numbers when
needed.

## Processing XML

XML is imported as a nested list structure. Each element is
represented as `[tag, attributes, ...children]`:

```sh
eu -e 'root' root=xml@data.xml
```

Use list functions to navigate the structure:

```eu
{ import: "root=xml@data.xml" }

# Get the tag name (first element)
tag: root first

# Get attributes (second element)
attrs: root second

# Get child elements (everything after the first two)
children: root drop(2)
```

## Named Inputs

Use named inputs to make data available under a specific name:

```sh
eu users=users.json roles=roles.json -e 'users map(_.name)'
```

Named inputs are essential for list-based formats (CSV, JSON Lines,
text):

```sh
eu lines=text@log.txt -e 'lines filter(str.matches?("ERROR")) count'
```

## Combining Multiple Sources

A powerful pattern is combining data from multiple sources:

```sh
eu users.yaml roles.yaml merge.eu
```

Where `merge.eu` contains logic that uses names from both inputs:

```eu
# merge.eu
summary: {
  user-count: users count
  role-count: roles count
}
```

## Using Evaluands

The `-e` flag specifies an expression to evaluate against the loaded
inputs:

```sh
# Select a nested value
eu config.yaml -e 'database.host'

# Transform and filter
eu data.json -e 'items filter(_.price > 100) map(_.name)'

# Aggregate
eu data.json -e 'items map(_.price) foldl(+, 0)'
```

## Collecting Inputs

The `--collect-as` (`-c`) flag gathers multiple files into a list:

```sh
eu -c configs *.yaml -e 'configs map(_.name)'
```

Add `--name-inputs` (`-N`) to get a block keyed by filename:

```sh
eu -c configs -N *.yaml
```

```yaml
configs:
  a.yaml:
    name: alpha
  b.yaml:
    name: beta
```

## Output Formats

Control the output format:

| Flag | Format |
|------|--------|
| (default) | YAML |
| `-j` | JSON |
| `-x json` | JSON |
| `-x toml` | TOML |
| `-x edn` | EDN |
| `-x text` | Plain text |

The format can also be inferred from the output file:

```sh
eu data.yaml -o output.json
```

## Practical Example: Data Pipeline

Suppose you have a CSV of sales data and want to generate a JSON
summary:

```sh
eu sales=sales.csv -j -e '{
  total: sales map(_.amount num) foldl(+, 0)
  count: sales count
  regions: sales map(_.region) unique
}'
```

Or as a reusable eucalypt file:

```eu
# report.eu
{ import: "sales=sales.csv" }

` :suppress
amounts: sales map(_.amount num)

report: {
  total: amounts foldl(+, 0)
  count: sales count
  average: report.total / report.count
}
```

```sh
eu report.eu -j -e report
```

## Key Concepts

- Eucalypt reads JSON, YAML, TOML, CSV, XML, EDN, JSON Lines, and
  plain text
- Output defaults to YAML; use `-j` for JSON or `-x` for other
  formats
- **Named inputs** (`name=file`) give data a name for reference
- The `-e` flag evaluates expressions against loaded data
- `--collect-as` gathers multiple files into a list or block
- CSV values are strings; use `num` to convert to numbers
- Combine multiple sources with the command line input system or
  imports
