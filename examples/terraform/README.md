# Generating Terraform from Eucalypt

These examples generate Terraform **JSON** configuration (`*.tf.json`)
from a compact Eucalypt model. Terraform reads `*.tf.json` natively, so
the rendered output can be applied directly with `terraform plan` /
`terraform apply` — the same mechanism HashiCorp's own CDKTF uses. The
pitch is simple: keep Terraform's runtime graph, state and providers,
but replace HCL *authoring* with a real functional language — proper
loops, functions, data structures, in-place computation and typing.

## The libraries

| Module                | Purpose                                                         |
|-----------------------|----------------------------------------------------------------|
| `lib/tf.eu`           | Provider-agnostic core: `ref`/handles, `resource`/`data` nodes, `document` assembly, and the other top-level blocks (`provider`, `terraform-settings`, `variable`, `output`, `locals`). |
| `lib/tf-cloudflare.eu`| Cloudflare DNS constructors (zone lookup, A/AAAA/CNAME/TXT/MX records). Provider **v5**. |
| `lib/tf-cloudflare-waf.eu` | Cloudflare WAF / rate-limiting: `cloudflare_ruleset` constructors plus a wirefilter expression builder (`all-of`, `path-prefix`, `negate`, `country-in`, ...). |

### Generation time vs apply time

The division of labour is deliberate:

- Values known at **generation** time (names, counts, tags, region/zone
  fan-out) are computed in Eucalypt and emitted as literal JSON.
- Values that only exist after **apply** (resource ids, ARNs, ...) are
  emitted as Terraform `${...}` references via *handles*, so they stay
  inside Terraform's dependency graph.

A constructor returns a handle, and you reference downstream attributes
through it:

```eu
zone:   cf.zone("main", "example.com")   # data-source handle
record: cf.a(zone, "www", "203.0.113.10")
# record's zone_id becomes "${data.cloudflare_zone.main.id}"
```

Typo `zone.idd` and you get a Eucalypt error — not a silently broken
`${...}` string.

## Running the Cloudflare DNS example

```sh
eu -j -L lib examples/terraform/cloudflare-dns.eu > main.tf.json
terraform init && terraform validate    # offline; no credentials needed
```

`cloudflare-dns.eu` defines a handful of records once and fans them out
across every zone in the model — two domains × eight records expands to
sixteen `cloudflare_dns_record` resources, each wired to the right zone
via its handle. Credentials are read from `CLOUDFLARE_API_TOKEN` at apply
time; no secrets appear in the generated JSON.

> The `-L lib` flag puts the Terraform modules on the import path.

## Running the WAF example

```sh
eu -j -L lib examples/terraform/cloudflare-waf.eu > security.tf.json
```

`cloudflare-waf.eu` builds an edge-security baseline — WAF custom rules
and a rate-limit rule — as `cloudflare_ruleset` resources across every
zone. Rule expressions are *composed* from wirefilter helpers rather than
hand-written; the admin rule, for instance, becomes:

    (starts_with(http.request.uri.path, "/admin")) and (not (ip.src in {203.0.113.0/24 198.51.100.0/24}))

## Roadmap

**Implemented** (Cloudflare provider v5):

- DNS — zone lookup + A/AAAA/CNAME/TXT/MX (`lib/tf-cloudflare.eu`).
- WAF / rate-limiting — `cloudflare_ruleset` + a wirefilter expression
  builder (`lib/tf-cloudflare-waf.eu`).

**Next (resume here under eu 0.8):**

1. **Library-wide type pass.** Replace the prose shape descriptions with
   checked types: `Node` / `Document` in `tf.eu`, and `Rule` / `Expr`
   plus literal-union `action` / `phase` types in the Cloudflare modules
   (so a typo'd action or phase is a type error). Done as one coherent
   layer across `tf.eu` + cloudflare + waf, not module by module. While
   here, swap the local `select` helper in `tf-cloudflare.eu` for the
   prelude `select` that lands in 0.8.
2. **Cloudflare dev platform** — Workers / KV / R2 / D1 / Queues / Pages.
3. **More Cloudflare** — Zero Trust / Access, load balancing, account / ops.
4. **Other providers** — GitHub, then AWS networking. The `tf.eu` core is
   provider-agnostic; a new provider is mostly constructor data entry.

**Tooling follow-ups** (raised, not scheduled):

- A `merge-with` / `deep-merge-with` prelude primitive, and a
  collision-detecting `document` — a duplicate resource address should be
  a typed error, not a silently dropped resource.
- `eu fmt` / lint enforcement of idioms (juxtaposed `f[...]` / `g{...}`
  calls; no `:suppress` on functions) wired into CI, so style is
  mechanically enforced rather than reviewer-caught.
- Bake the `tf*` modules into the binary (`src/driver/resources.rs`) so
  `{ import: "tf.eu" }` works without `-L lib`, like `state.eu`.
- `terraform validate` in CI — needs an environment with the `terraform`
  binary and network access; the generated JSON is currently checked
  structurally against the v5 provider schema only.
