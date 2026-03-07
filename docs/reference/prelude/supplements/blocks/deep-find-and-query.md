### Deep Find

Searches for a key at any nesting level:

```eu
config: {
  server: { host: "localhost" port: 8080 }
  db: { host: "db.local" port: 5432 }
}

hosts: config deep-find("host")  # ["localhost", "db.local"]
first-host: config deep-find-first("host", "unknown")  # "localhost"
```

### Deep Query

Queries using dot-separated patterns with wildcards:

- Bare name `foo` is sugar for `**.foo` (find at any depth)
- `*` matches one level
- `**` matches any depth

```eu
data: {
  us: { config: { host: "us.example.com" } }
  eu: { config: { host: "eu.example.com" } }
}

# Find all hosts under any config
hosts: data deep-query("config.host")  # ["us.example.com", "eu.example.com"]

# Wildcard: any key at one level, then host
hosts: data deep-query("*.config.host")
```
