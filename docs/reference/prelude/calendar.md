# Calendar

## Date and Time Functions

> Function for working with a human view of time.

| Function | Description |
|----------|-------------|
| `cal.ifields` | Break an epoch time `t` (seconds) into a block of date-time fields (`y`, `m`, `d`, `H`, `M`, `S`, ...) |
| `cal.now` | The current time as a block of date-time fields (as `cal.ifields`), taken from `io.epoch-time` |
| `cal.epoch` | The Unix epoch (time 0) as a block of date-time fields |
| `cal.zdt` | Create zoned date time from datetime components and timezone string (e.g. '+0100') |
| `cal.datetime(b)` | Convert block of time fields to zoned datetime (defaults: y=1, m=1, d=1, H=0, M=0, S=0, Z=UTC) |
| `cal.parse` | Parse an ISO8601 formatted date string into a zoned date time |
| `cal.format` | Format a zoned date time as ISO8601 |
| `cal.fields` | Decompose a zoned date time into a block of its component fields (y,m,d,H,M,S,Z) |
