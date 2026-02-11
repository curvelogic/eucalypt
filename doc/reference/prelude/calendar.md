# Calendar

The `cal` namespace provides date/time functions:

| Function | Description |
|----------|-------------|
| `cal.now` | Current time as fields block |
| `cal.epoch` | Unix epoch as fields block |
| `cal.zdt(y,m,d,H,M,S,Z)` | Create zoned datetime |
| `cal.datetime(b)` | Create from block with defaults |
| `cal.parse(s)` | Parse ISO8601 string |
| `cal.format(t)` | Format as ISO8601 |
| `cal.fields(t)` | Decompose to `{y,m,d,H,M,S,Z}` block |
