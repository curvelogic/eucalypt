# Calendar

## Date and Time Functions

| Function | Description |
|----------|-------------|
| `cal.zdt` | Create zoned date time from datetime components and timezone string (e.g. '+0100') |
| `cal.datetime(b)` | Convert block of time fields to zoned datetime (defaults: y=1, m=1, d=1, H=0, M=0, S=0, Z=UTC) |
| `cal.parse` | Parse an ISO8601 formatted date string into a zoned date time |
| `cal.format` | Format a zoned date time as ISO8601 |
| `cal.fields` | Decompose a zoned date time into a block of its component fields (y,m,d,H,M,S,Z) |
