# Testing with Eucalypt

Eucalypt has a built-in test runner which can be used to run tests
embedded in eucalypt files.

Test mode is invoked by the `eu test` subcommand and:

- analyses the file to build a test plan consisting of a list of test
  targets and validations to run
- executes the test plan and generates an *evidence* file
- applies validations against the evidence to generate a results file
- outputs results and generates an HTML report

## Simple tests

By default eucalypt searches for targets beginning with `test-` and
runs each to render a `yaml` output. The result is parsed read back in
and eucalypt checks for the presence of a `RESULT` key. If it finds it
and the value is `PASS`, the test passes. Anything else is considered
a fail.

```eu
my-add(x, y): x + y

` { target: :test-add }
test: {
  RESULT: (2 + 2 = 4) then(:PASS, :FAIL)
}
```

Several test targets can be embedded in one file. Each is run as a
separate test.

## How the default verdict is computed

Each test target is rendered to output, and that output is parsed back
into eucalypt data as `result`. An *expectation function* maps the
evidence to `:PASS` or `:FAIL`. Unless a target names its own validator
(see [Custom validators](#custom-validators)), the built-in
`default-expectation` in `lib/test.eu` is used, and it derives the
verdict from `result` as follows:

- if `result` is a block containing a `RESULT` key, the verdict is that
  key's value — `true` gives `:PASS`, `false` gives `:FAIL`, and any
  other value is used as the verdict directly;
- otherwise, if `result` is a block, it passes when every value in the
  block is `true`;
- otherwise `result` is treated as a bare boolean, passing on `true`.

Because the `RESULT` key takes precedence, a target that defines
`RESULT` is judged solely on that value; its other bindings reach the
verdict only through whatever computes `RESULT`. Computing `RESULT`
from the checks keeps every check in the verdict — see
`tests/harness/189_r9oy_union_as_spec.eu` and
`tests/harness/182_typedata_alias_resolution.eu`, which gather their
checks into a list and finish with `all-true? then(:PASS, :FAIL)`.

## Inline assertions

An expression can carry an inline expected value with the `//=>`
operator:

```eu
sum: [1, 2, 3] sum //=> 6
```

`//=>` is a pass-through assertion: when it holds, the expression keeps
its value and so composes inside larger expressions. When it does not
hold, eucalypt emits an `EXPECT FAILED` diagnostic to `stderr`; in test
mode the expression then evaluates to `false` and execution continues
(outside test mode an assertion error is raised instead). Like any
binding, an inline assertion reaches a target's verdict only through
the `result` the expectation function sees — for instance by feeding
its value into `RESULT`, or via the all-values-true inference when no
`RESULT` key is present.

## Test files

If your intention is not to embed tests in a eucalypt file but instead
to write a test as a single file, then you can omit the test targets.
Eucalypt will use a `main` target or run the entire file as usual and
then validate the result (looking for a `RESULT` key, by default).

## Other formats

In test mode, eucalypt processes the test subject to generate output
and then parses that back to validate the result. This is to provide
for validation of the rendered text and the parsing machinery.

By default YAML is generated and parsed back for each test target in
the file but other formats can be selected in header metadata.

```eu
{
  test-targets: [:yaml, :json]
}

` { target: :test-add }
add: {
  RESULT: (2 + 2 = 4) then(:PASS, :FAIL)
}

` { target: :test-sub }
sub: {
  RESULT: (2 - 2 = 0) then(:PASS, :FAIL)
}
```

Running this file using `eu test` will result in four tests being run,
two formats for each of the two targets.

Using the default validator, for all formats for which eucalypt
provides import and export capability, it shouldn't make any
difference which format is used. However, custom validators provide
the ability to check the precise text that is rendered.

## Custom validators

When a test runs, the execution generates an evidence block which has
the following keys:

- `exit` the exit code (0 on success) of the eucalypt execution
- `stdout` text as a list of strings
- `stderr` text as a list of strings
- `result` (the stdout parsed back)
- `stats` some statistics from the run
