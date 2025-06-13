# Test Mode

Eucalypt has an experimental built-in test runner which can be used to
run tests embedded in eucalypt files.

Test mode is invoked by the `-T` command line flag and:

- analyses the file to build a test plan consisting of a list of test
  targets and validations to run
- executes the test plan and generates an *evidence* file
- applies validations against the evidence to generate a results file
- outputs results and generates an HTML report

## Simple tests

By default eucalypt searches for targets beginning with `test-` and
runs each to render a `yaml` output. The result is parsed read back in
a eucalypt checks for the presence of a `RESULT`' key. If it finds it
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

Running this file using `-T` will result in four tests being run, two
formats for each of the two targets.

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
