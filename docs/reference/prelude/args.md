# parse-args

Parse command-line arguments against a defaults block.

## Signature

```eu,notest
parse-args(defaults, args)
```

- `defaults` — a block where each key defines an option with its default value; field metadata configures the option
- `args` — a list of argument strings (e.g. from `io.args`)

Returns the `defaults` block overridden with parsed values, plus an `args` key containing positional arguments as a list.

## Metadata Keys

Each field in `defaults` may carry metadata to configure parsing:

| Key     | Type   | Description                                              |
|---------|--------|----------------------------------------------------------|
| `short` | symbol | Single-character short flag (e.g. `:v` for `-v`)        |
| `doc`   | string | Description shown in auto-generated `--help` output      |
| `flag`  | bool   | If `true`, option is a boolean toggle (no value argument)|

## Type Coercion

The type of the default value determines coercion:
- Default is a **number** → parse value as JSON number (`"42"` → `42`)
- Default is a **string** → value stays as string
- Default is a **boolean with `flag: true`** → presence toggles to `true`

## Syntax Forms

| Syntax              | Description                             |
|---------------------|-----------------------------------------|
| `--key value`       | Long option with separate value         |
| `--key=value`       | Long option with inline value           |
| `--flag`            | Boolean flag toggle                     |
| `-x value`          | Short option with value                 |
| `-x`                | Short flag toggle                       |
| `-xy`               | Combined short flags (all flags) or flag + value |
| positional          | Non-option arguments collected in `args`|
| `--help`            | Print auto-generated help and exit      |

## Example

```eu,notest
` :suppress
defaults: {
  ` { short: :v  doc: "Enable verbose output"  flag: true }
  verbose: false

  ` { short: :o  doc: "Output file path" }
  output: "out.yaml"

  ` { doc: "Number of iterations" }
  count: 1
}

main: io.args parse-args(defaults)
```

Usage:

```sh
eu script.eu -- --verbose -o result.yaml --count 5 input.eu
```

Result block: `{ verbose: true, output: "result.yaml", count: 5, args: ["input.eu"] }`.

## Errors

- Unknown long option → `panic: unknown option: --name`
- Unknown short option → `panic: unknown option: -c`
- Missing value for option → `panic: option --name requires a value`

## Pipeline Usage

```eu,notest
# Typical entry point
main: io.args parse-args(defaults)

# With positional file processing
run: {
  opts: io.args parse-args(defaults)
  files: opts.args
  ...
}
```
