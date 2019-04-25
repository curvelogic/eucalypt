#!/usr/bin/env python3
"""
eut.py - A test harness for Eucalypt implementations.
"""

import subprocess
import sys
import re
import timeit
import argparse
from itertools import (product, chain)
try:
    import yaml
except:
    print("WARNING: yaml not available")
try:
    import toml
except:
    print("WARNING: toml not available")
import json
from enum import Enum
from pathlib import (Path)

class Result(Enum):
    SUCCESS = 1
    IGNORE = 2
    FAIL = 3

class Test:

    """ A test that by default passes with a zero return code """

    RE = re.compile(r"(x?)(\d+)_(.*?).(eu|yaml|json|toml)")

    def __init__(self, filepath, outdir, format="yaml"):
        self.filepath = filepath
        (ignore, id, name, ext) = Test.RE.fullmatch(filepath.name).groups()
        self.ignore = ignore == 'x'
        self.id = id
        self.name = name
        self.format = format
        self.outfile = outdir / filepath.relative_to(test_directory).with_suffix(f".out.{format}")
        self.outfile.parent.mkdir(parents = True, exist_ok = True)
        self.errfile = outdir / filepath.relative_to(test_directory).with_suffix(f".{format}.err")
        self.errfile.parent.mkdir(parents = True, exist_ok = True)
        self.eu_args = ["eu", "-B", "-x", format, str(filepath), "+RTS", "-s"]

    def failed(self):
        return self.result is Result.FAIL

    def check(self):
        content_passes = True

        if self.format == "yaml":
            with open(self.outfile) as stream:
                try:
                    content_passes = yaml.load(stream, Loader=yaml.SafeLoader).get("RESULT") == "PASS"
                except:
                    pass

        if self.format == "json":
            with open(self.outfile) as stream:
                content_passes = json.load(stream).get("RESULT") == "PASS"

        if self.format == "toml":
            with open(self.outfile) as stream:
                try:
                    content_passes = toml.load(stream).get("RESULT") == "PASS"
                except:
                    pass

        return self.proc.returncode == 0 and content_passes

    def execute(self):
        with open(self.outfile, "wb") as out:
            with open(self.errfile, "wb") as err:
                proc = subprocess.run(self.eu_args,
                                      stdout=out,
                                      stderr=err)
        self.proc = proc


    def run(self):

        """ Run eucalypt files """

        print(self.id, self.name, f"(as {self.format})", sep=' ', end=' ')

        if self.ignore:

            self.result = Result.IGNORE
            print("ignore")

        else:
            self.execute()

            if self.check():
                print("pass")
                self.result = Result.SUCCESS
            else:
                print("FAIL\n")
                with open(self.errfile, "r") as err:
                    errout = err.read().strip()
                    if errout:
                        print("error output:\n---")
                        sys.stdout.write(errout)
                        print("---\n")
                    else:
                        with open(self.outfile, "r") as out:
                            print("failing output:\n--")
                            sys.stdout.write(out.read())
                            print("---\n")

                self.result = Result.FAIL

        return self

    def bench(self, repeats):

        """ Time repeated runs. """

        if self.ignore:
            return

        print(self.id, self.name, sep=' ', end=' ')

        self.sec = timeit.timeit(self.execute, number=repeats)

        print(f"{self.sec}s")

        return self


class BenchmarkTest(Test):

    """   A test run purely for timing.   """

    def __init__(self, filepath, outdir, format="yaml"):
        super().__init__(filepath, outdir, format)
        self.id = "B" + self.id

class ErrorTest(Test):

    """A test that expects a non-zero returncode"""

    def __init__(self, filepath, outdir, format="yaml"):
        super().__init__(filepath, outdir, format)
        self.id = "E" + self.id

    def check(self):
        return self.proc.returncode > 0


class ShellTest:

    """A test that runs a shell script and requires a zero returncode"""
    RE = re.compile(r"(x?)(\d+)_(.*?).sh")

    def __init__(self, filepath, outdir):
        self.filepath = filepath
        (ignore, id, name) = ShellTest.RE.fullmatch(filepath.name).groups()
        self.id = "S" + id
        self.ignore = ignore == 'x'
        self.name = name
        self.format = format
        self.outfile = outdir / filepath.relative_to(test_directory).with_suffix(f".out.{format}")
        self.outfile.parent.mkdir(parents = True, exist_ok = True)
        self.errfile = outdir / filepath.relative_to(test_directory).with_suffix(f".{format}.err")
        self.errfile.parent.mkdir(parents = True, exist_ok = True)


    def execute(self):
        with open(self.outfile, "wb") as out:
            with open(self.errfile, "wb") as err:
                self.proc = subprocess.run(["bash", self.filepath],
                                           stdout=out,
                                           stderr=err)

    def run(self):

        """ Run eucalypt files """

        print(self.id, self.name, sep=' ', end=' ')

        if self.ignore:

            self.result = Result.IGNORE
            print("ignore")

        else:
            self.execute()

            if self.check():
                print("pass")
                self.result = Result.SUCCESS
            else:
                print("FAIL\n")
                with open(self.errfile, "r") as err:
                    errout = err.read().strip()
                    if errout:
                        print("error output:\n---")
                        sys.stdout.write(errout)
                        print("---\n")
                    else:
                        with open(self.outfile, "r") as out:
                            print("failing output:\n--")
                            sys.stdout.write(out.read())
                            print("---\n")

                self.result = Result.FAIL

        return self

    def check(self):
        return self.proc.returncode == 0

    def failed(self):
        return self.result == Result.FAIL

    def bench(self, repeats):

        """ Time repeated runs. """

        if self.ignore:
            return

        print(self.id, self.name, sep=' ', end=' ')

        self.sec = timeit.timeit(self.execute, number=repeats)

        print(f"{self.sec}s")

        return self


def find_simple_tests(testdir, outdir):
    return [Test(p, outdir, fmt) for (p, fmt) in
            product(sorted(chain(testdir.glob("*.eu"),
                                 testdir.glob("*.yaml"),
                                 testdir.glob("*.json"),
                                 testdir.glob("*.toml"))),
                           ["yaml", "json", "toml"])]

def find_error_tests(testdir, outdir):
    return [ErrorTest(p, outdir) for p in sorted(testdir.glob("errors/*.eu"))]

def find_benchmark_tests(testdir, outdir):
    return [BenchmarkTest(p, outdir) for p in sorted(testdir.glob("bench/*.eu"))]

def find_shell_tests(testdir, outdir):
    return [ShellTest(p, outdir) for p in sorted(testdir.glob("shell/*.sh"))]

test_directory = Path("test")
default_output_directory = Path(".build/output")

def prepare_directories():
    if not test_directory.is_dir():
        raise RuntimeError("Cannot locate test directory")

parser = argparse.ArgumentParser("Eucalypt test harness")
parser.add_argument("-b", "--bench", action='store_true', default=False)
parser.add_argument("-n", "--repeats", action='store', type=int, default=100)
parser.add_argument("-o", "--output-directory", action='store', default=default_output_directory)

def main():
    opts = parser.parse_args()
    prepare_directories()
    tests = find_simple_tests(test_directory, opts.output_directory) + \
            find_error_tests(test_directory, opts.output_directory) + \
            find_shell_tests(test_directory, opts.output_directory)

    if opts.bench:
        print(f"Timing standard tests with {opts.repeats} repeats.")
        results = [t.bench(opts.repeats) for t in tests]

        print(f"Timing benchmark tests without repeat.")
        results = [t.bench(1) for t in find_benchmark_tests(test_directory, opts.output_directory)]
    else:
        results = [t.run() for t in tests]
        if any(r.failed() for r in results):
            print("FAIL")
            return 1
        else:
            return 0

if __name__ == '__main__':
    main()
