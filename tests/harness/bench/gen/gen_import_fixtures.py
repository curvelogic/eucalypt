#!/usr/bin/env python3
"""Generate the import->transform->export fixtures for benches 016/017.

Writes two deterministic fixtures next to the benches:
  - 016_fixture.yaml : N records with nested maps/lists (YAML import path)
  - 017_fixture.toml : N records, flat (TOML array-of-tables import path)

These exercise workload class I (import -> reshape -> export), the product
core, which had no engine benchmark before eu-2sa6.6. Kept a few hundred KB
(not megabytes): the eval-side reshape is O(n^2) over the records (shared
fold/env-walk ceiling), so a modest N already exceeds the >1.5s eval band.

Regenerate with:  python3 tests/harness/bench/gen/gen_import_fixtures.py
"""

import os

N = 1600  # records; kept below the bytecode large-TOML-import panic threshold (~1900 full-schema records, see 017 bench + PROTOCOL exception). Both benches reach >1.5s via reshape rounds, not record count.

ZONES = ["north", "south", "east", "west"]
KINDS = ["proxy", "cache", "worker", "db"]


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    bench_dir = os.path.dirname(here)

    # ---- YAML fixture: nested maps + lists ----
    yaml_path = os.path.join(bench_dir, "016_fixture.yaml")
    with open(yaml_path, "w") as f:
        f.write("records:\n")
        for i in range(N):
            enabled = "true" if (i % 3 != 0) else "false"
            f.write(f"  - id: {i}\n")
            f.write(f'    name: "svc-{i}"\n')
            f.write(f'    zone: "{ZONES[i % len(ZONES)]}"\n')
            f.write(f'    kind: "{KINDS[i % len(KINDS)]}"\n')
            f.write(f"    enabled: {enabled}\n")
            f.write(f"    weight: {i % 100}\n")
            f.write("    limits:\n")
            f.write(f"      cpu: {100 + (i % 50)}\n")
            f.write(f"      mem: {256 + (i % 64)}\n")
            f.write(f'    tags: ["t{i % 10}", "t{i % 7}"]\n')

    # ---- TOML fixture: flat array-of-tables ----
    toml_path = os.path.join(bench_dir, "017_fixture.toml")
    with open(toml_path, "w") as f:
        for i in range(N):
            enabled = "true" if (i % 3 != 0) else "false"
            f.write("[[records]]\n")
            f.write(f"id = {i}\n")
            f.write(f'name = "svc-{i}"\n')
            f.write(f'zone = "{ZONES[i % len(ZONES)]}"\n')
            f.write(f'kind = "{KINDS[i % len(KINDS)]}"\n')
            f.write(f"enabled = {enabled}\n")
            f.write(f"weight = {i % 100}\n")
            f.write(f"cpu = {100 + (i % 50)}\n")
            f.write(f"mem = {256 + (i % 64)}\n\n")

    ysz = os.path.getsize(yaml_path)
    tsz = os.path.getsize(toml_path)
    print(f"wrote {yaml_path} ({ysz} bytes, {N} records)")
    print(f"wrote {toml_path} ({tsz} bytes, {N} records)")


if __name__ == "__main__":
    main()
