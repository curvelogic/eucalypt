//! Source-level profiler for the bytecode engine (`eu run --profile`,
//! eu-oaz72.37).
//!
//! The machine's profiled dispatch loops call [`Profiler::record`] once per
//! dispatch (tick) with the scope-accurate annotation register — the source
//! location of the code executing — and the running allocation and byte
//! totals. Each record attributes one tick plus the allocation and byte
//! deltas since the previous record to that location. Deltas are taken
//! against the profiler's own running totals, so a nested sub-run (a BIF
//! forcing a thunk through `drive_to_whnf`) attributes its own work and the
//! enclosing dispatch is charged only with the remainder: nothing is
//! counted twice and the column totals equal the `-S` figures.
//!
//! When the profiler is off the machine runs the unprofiled instantiation of
//! its dispatch loops, which contains no call into this module.

use std::collections::HashMap;

use codespan_reporting::files::SimpleFiles;

use crate::common::sourcemap::{Smid, SourceMap};

/// How often (in ticks) the folded-stack sampler walks the continuation
/// stack. A prime, so the sample does not alias a loop of fixed period.
pub const FOLDED_SAMPLE_INTERVAL: u64 = 1009;

/// Innermost continuation frames kept per folded-stack sample, bounding the
/// cost of a sample on a deep stack.
pub const FOLDED_STACK_DEPTH: usize = 64;

/// Row label for a location with no resolvable name or site.
fn unresolved_label(source_map: &SourceMap, smid: Smid) -> String {
    if source_map.is_unmapped(smid) {
        PRELUDE_INTERNAL.to_string()
    } else {
        "<unannotated>".to_string()
    }
}

/// Row label for code inside the pre-compiled prelude blob, whose
/// annotations carry no source info; `--source-prelude` names it.
pub const PRELUDE_INTERNAL: &str = "<prelude internal> (use --source-prelude to name)";

/// Per-location counters.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ProfileCounts {
    /// Dispatches (machine ticks) executed with this location current.
    pub ticks: u64,
    /// `let`/`letrec` closure allocations (the `-S` "Allocs" count).
    pub allocs: u64,
    /// Heap bytes allocated (header + payload, after size rounding).
    pub bytes: u64,
}

/// Accumulated profile of one machine.
#[derive(Debug, Default)]
pub struct Profiler {
    by_location: HashMap<Smid, ProfileCounts>,
    last_allocs: u64,
    last_bytes: u64,
    ticks: u64,
    /// Folded-stack samples (innermost-first smids → sample count), when
    /// enabled.
    folded: Option<HashMap<Vec<Smid>, u64>>,
}

impl Profiler {
    /// A profiler starting from the machine's current allocation totals.
    pub fn new(allocs: u64, bytes: u64, folded: bool) -> Self {
        Profiler {
            by_location: HashMap::new(),
            last_allocs: allocs,
            last_bytes: bytes,
            ticks: 0,
            folded: folded.then(HashMap::new),
        }
    }

    /// Attribute one tick, and the allocations and bytes since the previous
    /// record, to `location`. Returns `true` when a folded-stack sample is
    /// due (the caller then supplies it through [`Profiler::sample`]).
    #[inline]
    pub fn record(&mut self, location: Smid, allocs: u64, bytes: u64) -> bool {
        let entry = self.by_location.entry(location).or_default();
        entry.ticks += 1;
        entry.allocs += allocs.wrapping_sub(self.last_allocs);
        entry.bytes += bytes.wrapping_sub(self.last_bytes);
        self.last_allocs = allocs;
        self.last_bytes = bytes;
        self.ticks += 1;
        self.folded.is_some() && self.ticks.is_multiple_of(FOLDED_SAMPLE_INTERVAL)
    }

    /// Record one folded-stack sample: `location` (the current code) then
    /// the continuation-stack trace, innermost first.
    pub fn sample(&mut self, location: Smid, mut trace: Vec<Smid>) {
        if let Some(folded) = &mut self.folded {
            if location.is_valid() && trace.first() != Some(&location) {
                trace.insert(0, location);
            }
            *folded.entry(trace).or_default() += 1;
        }
    }

    /// Whether folded-stack sampling is enabled.
    pub fn folded_enabled(&self) -> bool {
        self.folded.is_some()
    }

    /// Total ticks recorded.
    pub fn ticks(&self) -> u64 {
        self.ticks
    }

    /// Counters per source location.
    pub fn locations(&self) -> &HashMap<Smid, ProfileCounts> {
        &self.by_location
    }

    /// Aggregate the per-location counters by display label (several smids
    /// can resolve to the same function and site), sorted by ticks, then
    /// bytes, then label.
    pub fn rows(
        &self,
        source_map: &SourceMap,
        files: &SimpleFiles<String, String>,
    ) -> Vec<(String, ProfileCounts)> {
        let mut by_label: HashMap<String, ProfileCounts> = HashMap::new();
        for (smid, counts) in &self.by_location {
            let label = source_map
                .describe_smid(*smid, files)
                .unwrap_or_else(|| unresolved_label(source_map, *smid));
            let row = by_label.entry(label).or_default();
            row.ticks += counts.ticks;
            row.allocs += counts.allocs;
            row.bytes += counts.bytes;
        }
        let mut rows: Vec<_> = by_label.into_iter().collect();
        rows.sort_by(|(la, a), (lb, b)| {
            b.ticks
                .cmp(&a.ticks)
                .then(b.bytes.cmp(&a.bytes))
                .then(la.cmp(lb))
        });
        rows
    }

    /// Render the top-`top` table.
    pub fn report(
        &self,
        source_map: &SourceMap,
        files: &SimpleFiles<String, String>,
        top: usize,
    ) -> String {
        let rows = self.rows(source_map, files);
        let total = rows.iter().fold(ProfileCounts::default(), |mut t, (_, c)| {
            t.ticks += c.ticks;
            t.allocs += c.allocs;
            t.bytes += c.bytes;
            t
        });
        let pct = |n: u64, d: u64| {
            if d == 0 {
                0.0
            } else {
                n as f64 * 100.0 / d as f64
            }
        };
        let mut out = String::new();
        out.push_str(&format!(
            "Profile: {} ticks, {} allocs, {} bytes over {} locations (top {} by ticks)\n",
            total.ticks,
            total.allocs,
            total.bytes,
            rows.len(),
            top.min(rows.len())
        ));
        out.push_str(&format!(
            "{:>14} {:>6} {:>12} {:>6} {:>14} {:>6}  location\n",
            "ticks", "%", "allocs", "%", "bytes", "%"
        ));
        for (label, c) in rows.iter().take(top) {
            out.push_str(&format!(
                "{:>14} {:>5.1}% {:>12} {:>5.1}% {:>14} {:>5.1}%  {}\n",
                c.ticks,
                pct(c.ticks, total.ticks),
                c.allocs,
                pct(c.allocs, total.allocs),
                c.bytes,
                pct(c.bytes, total.bytes),
                label
            ));
        }
        out
    }

    /// Render the folded-stack samples (`outer;…;inner count` per line,
    /// the input format of `flamegraph.pl` and compatible tools).
    pub fn folded_report(
        &self,
        source_map: &SourceMap,
        files: &SimpleFiles<String, String>,
    ) -> String {
        let Some(folded) = &self.folded else {
            return String::new();
        };
        let mut lines: HashMap<String, u64> = HashMap::new();
        for (trace, count) in folded {
            let frames: Vec<String> = trace
                .iter()
                .rev()
                .map(|smid| {
                    source_map
                        .describe_smid(*smid, files)
                        .unwrap_or_else(|| unresolved_label(source_map, *smid))
                        .replace(';', ":")
                })
                .collect();
            let key = if frames.is_empty() {
                "<unannotated>".to_string()
            } else {
                frames.join(";")
            };
            *lines.entry(key).or_default() += count;
        }
        let mut lines: Vec<_> = lines.into_iter().collect();
        lines.sort();
        lines
            .into_iter()
            .map(|(stack, count)| format!("{stack} {count}\n"))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn record_attributes_deltas_without_double_counting() {
        let a = Smid::fake(1);
        let b = Smid::fake(2);
        let mut p = Profiler::new(10, 100, false);
        p.record(a, 12, 150); // +2 allocs, +50 bytes to a
        p.record(b, 12, 150); // nothing new to b
        p.record(a, 13, 170); // +1, +20 to a
        let ca = p.locations()[&a];
        let cb = p.locations()[&b];
        assert_eq!(
            ca,
            ProfileCounts {
                ticks: 2,
                allocs: 3,
                bytes: 70
            }
        );
        assert_eq!(
            cb,
            ProfileCounts {
                ticks: 1,
                allocs: 0,
                bytes: 0
            }
        );
        assert_eq!(p.ticks(), 3);
    }

    #[test]
    fn folded_sampling_is_periodic_and_off_by_default() {
        let a = Smid::fake(1);
        let mut off = Profiler::new(0, 0, false);
        let mut on = Profiler::new(0, 0, true);
        let mut due = 0;
        for _ in 0..(3 * FOLDED_SAMPLE_INTERVAL) {
            assert!(!off.record(a, 0, 0));
            if on.record(a, 0, 0) {
                due += 1;
                on.sample(a, vec![]);
            }
        }
        assert_eq!(due, 3);
        assert!(!off.folded_enabled());
    }
}
