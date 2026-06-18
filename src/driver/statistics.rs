//! Capture and report statistics for optimisation

use std::{cmp::max, fmt::Display, time::Duration};

use indexmap::IndexMap;

#[derive(Default, Debug)]
pub struct Timings {
    timings: IndexMap<String, Duration>,
}

pub(crate) type TimingPartition = (Vec<(String, Duration)>, Vec<(String, Duration)>);

impl Timings {
    pub fn record<T: AsRef<str>>(&mut self, name: T, elapsed: Duration) {
        self.timings.insert(name.as_ref().to_string(), elapsed);
    }

    pub fn merge(&mut self, other: Timings) {
        self.timings.extend(other.timings);
    }

    /// Partition timings into pipeline entries and VM entries.
    ///
    /// VM entries have keys starting with `"VM-"`.
    pub(crate) fn partition(&self) -> TimingPartition {
        let mut pipeline = Vec::new();
        let mut vm = Vec::new();
        for (k, v) in &self.timings {
            if k.starts_with("VM-") {
                vm.push((k.clone(), *v));
            } else {
                pipeline.push((k.clone(), *v));
            }
        }
        (pipeline, vm)
    }
}

impl Display for Timings {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let width = self.timings.keys().map(|k| k.len()).max().unwrap_or(0) + 1;

        for (k, v) in &self.timings {
            writeln!(f, "{:width$}: {:14.9}s", k, v.as_secs_f64(), width = width)?;
        }
        Ok(())
    }
}

/// The statistics captured during a run
#[derive(Default, Debug)]
pub struct Statistics {
    /// VM ticks
    machine_ticks: u64,
    /// Allocated object count (i.e. let binding count)
    machine_allocs: u64,
    /// Max stack height reached
    machine_max_stack: usize,
    /// Elapsed timings
    timings: Timings,
    /// Heap blocks allocated
    blocks_allocated: usize,
    /// Large object blocks allocated
    lobs_allocated: usize,
    /// Used and not recycled memory blocks
    blocks_used: usize,
    /// Recycled memory blocks
    blocks_recycled: usize,
    /// Number of GC collections performed
    collections_count: u64,
    /// High-water mark of allocated blocks
    peak_heap_blocks: usize,
    /// Aggregate mark phase time
    total_mark_time: Duration,
    /// Aggregate sweep phase time
    total_sweep_time: Duration,
}

impl Statistics {
    pub fn ticks(&self) -> u64 {
        self.machine_ticks
    }

    pub fn set_ticks(&mut self, ticks: u64) {
        self.machine_ticks = ticks;
    }

    pub fn allocs(&self) -> u64 {
        self.machine_allocs
    }

    pub fn set_allocs(&mut self, allocs: u64) {
        self.machine_allocs = allocs;
    }

    pub fn max_stack(&self) -> usize {
        self.machine_max_stack
    }

    pub fn set_max_stack(&mut self, max_stack: usize) {
        self.machine_max_stack = max_stack;
    }

    pub fn timings_mut(&mut self) -> &mut Timings {
        &mut self.timings
    }

    pub fn blocks_allocated(&self) -> usize {
        self.blocks_allocated
    }

    pub fn set_blocks_allocated(&mut self, count: usize) {
        self.blocks_allocated = count
    }

    pub fn lobs_allocated(&self) -> usize {
        self.lobs_allocated
    }

    pub fn set_lobs_allocated(&mut self, count: usize) {
        self.lobs_allocated = count
    }

    pub fn blocks_used(&self) -> usize {
        self.blocks_used
    }

    pub fn set_blocks_used(&mut self, count: usize) {
        self.blocks_used = count
    }

    pub fn blocks_recycled(&self) -> usize {
        self.blocks_recycled
    }

    pub fn set_blocks_recycled(&mut self, count: usize) {
        self.blocks_recycled = count
    }

    pub fn collections_count(&self) -> u64 {
        self.collections_count
    }

    pub fn set_collections_count(&mut self, count: u64) {
        self.collections_count = count
    }

    pub fn peak_heap_blocks(&self) -> usize {
        self.peak_heap_blocks
    }

    pub fn set_peak_heap_blocks(&mut self, count: usize) {
        self.peak_heap_blocks = count
    }

    pub fn total_mark_time(&self) -> Duration {
        self.total_mark_time
    }

    pub fn set_total_mark_time(&mut self, duration: Duration) {
        self.total_mark_time = duration
    }

    pub fn total_sweep_time(&self) -> Duration {
        self.total_sweep_time
    }

    pub fn set_total_sweep_time(&mut self, duration: Duration) {
        self.total_sweep_time = duration
    }

    /// Serialise all statistics fields to a JSON value
    pub fn to_json(&self) -> serde_json::Value {
        let mut timings_map = serde_json::Map::new();
        for (k, v) in &self.timings.timings {
            timings_map.insert(k.clone(), serde_json::json!(v.as_secs_f64()));
        }

        serde_json::json!({
            "machine_ticks": self.machine_ticks,
            "machine_allocs": self.machine_allocs,
            "machine_max_stack": self.machine_max_stack,
            "blocks_allocated": self.blocks_allocated,
            "lobs_allocated": self.lobs_allocated,
            "blocks_used": self.blocks_used,
            "blocks_recycled": self.blocks_recycled,
            "collections_count": self.collections_count,
            "peak_heap_blocks": self.peak_heap_blocks,
            "total_mark_time_secs": self.total_mark_time.as_secs_f64(),
            "total_sweep_time_secs": self.total_sweep_time.as_secs_f64(),
            "timings": serde_json::Value::Object(timings_map),
        })
    }

    pub fn merge(&mut self, other: Statistics) {
        self.machine_ticks += other.machine_ticks;
        self.machine_allocs += other.machine_allocs;
        self.machine_max_stack = max(self.machine_max_stack, other.machine_max_stack);
        self.timings.merge(other.timings);
        self.blocks_allocated = max(self.blocks_allocated, other.blocks_allocated);
        self.lobs_allocated = max(self.lobs_allocated, other.lobs_allocated);
        self.blocks_used = max(self.blocks_used, other.blocks_used);
        self.blocks_recycled = max(self.blocks_recycled, other.blocks_recycled);
        self.collections_count += other.collections_count;
        self.peak_heap_blocks = max(self.peak_heap_blocks, other.peak_heap_blocks);
        self.total_mark_time += other.total_mark_time;
        self.total_sweep_time += other.total_sweep_time;
    }
}

impl Display for Statistics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (pipeline, vm) = self.timings.partition();

        // Summary bar
        let summary = render_summary_bar(&pipeline, &vm);
        if !summary.is_empty() {
            writeln!(f, "{summary}")?;
            writeln!(f)?;
        }

        // Machine counters
        writeln!(f, "{}", section_header("Machine"))?;
        writeln!(
            f,
            "Ticks          : {:>14}",
            fmt_thousands(self.machine_ticks)
        )?;
        writeln!(
            f,
            "Allocs         : {:>14}",
            fmt_thousands(self.machine_allocs)
        )?;
        writeln!(
            f,
            "Max Stack      : {:>14}",
            fmt_thousands_usize(self.machine_max_stack)
        )?;
        writeln!(f)?;

        // Heap counters
        writeln!(f, "{}", section_header("Heap"))?;
        writeln!(
            f,
            "Blocks Allocated  : {:>10}",
            fmt_thousands_usize(self.blocks_allocated)
        )?;
        writeln!(
            f,
            "LOBs Allocated    : {:>10}",
            fmt_thousands_usize(self.lobs_allocated)
        )?;
        writeln!(
            f,
            "Blocks Used       : {:>10}",
            fmt_thousands_usize(self.blocks_used)
        )?;
        writeln!(
            f,
            "Blocks Recycled   : {:>10}",
            fmt_thousands_usize(self.blocks_recycled)
        )?;
        writeln!(
            f,
            "Peak Blocks       : {:>10}",
            fmt_thousands_usize(self.peak_heap_blocks)
        )?;
        writeln!(f)?;

        // GC counters
        writeln!(f, "{}", section_header("GC"))?;
        writeln!(
            f,
            "Collections    : {:>14}",
            fmt_thousands(self.collections_count)
        )?;
        writeln!(
            f,
            "Mark Time      : {:>11.6}s",
            self.total_mark_time.as_secs_f64()
        )?;
        writeln!(
            f,
            "Sweep Time     : {:>11.6}s",
            self.total_sweep_time.as_secs_f64()
        )?;
        writeln!(f)?;

        // Pipeline timings
        if !pipeline.is_empty() {
            writeln!(f, "{}", section_header("Pipeline"))?;
            write!(f, "{}", render_timing_section(&pipeline))?;
            writeln!(f)?;
        }

        // VM timings
        if !vm.is_empty() {
            writeln!(f, "{}", section_header("VM"))?;
            write!(f, "{}", render_timing_section(&vm))?;
        }

        Ok(())
    }
}

/// Format an integer with comma thousands separators.
pub(crate) fn fmt_thousands(n: u64) -> String {
    let s = n.to_string();
    let mut result = String::with_capacity(s.len() + s.len() / 3);
    for (i, c) in s.chars().enumerate() {
        if i > 0 && (s.len() - i).is_multiple_of(3) {
            result.push(',');
        }
        result.push(c);
    }
    result
}

/// Format a usize with comma thousands separators.
pub(crate) fn fmt_thousands_usize(n: usize) -> String {
    fmt_thousands(n as u64)
}

pub(crate) const BAR_WIDTH: usize = 30;
pub(crate) const BAR_FILLED: char = '█';
pub(crate) const BAR_EMPTY: char = '░';

/// Render a bar of `BAR_WIDTH` characters proportional to `value / max_value`.
pub(crate) fn render_bar(value: f64, max_value: f64) -> String {
    if max_value <= 0.0 {
        return String::new();
    }
    let filled = ((value / max_value) * BAR_WIDTH as f64).round() as usize;
    let filled = filled.min(BAR_WIDTH);
    let empty = BAR_WIDTH - filled;
    format!(
        "{}{}",
        BAR_FILLED.to_string().repeat(filled),
        BAR_EMPTY.to_string().repeat(empty),
    )
}

/// Render a top-level summary bar showing where total time was spent.
pub(crate) fn render_summary_bar(
    pipeline: &[(String, Duration)],
    vm: &[(String, Duration)],
) -> String {
    let mut fractions: Vec<(String, f64)> = Vec::new();

    for (name, dur) in pipeline {
        fractions.push((name.clone(), dur.as_secs_f64()));
    }

    // Add VM as a single entry (exclude the VM-Total synthetic key)
    let vm_total: f64 = vm
        .iter()
        .filter(|(k, _)| k != "VM-Total")
        .map(|(_, v)| v.as_secs_f64())
        .sum();
    if vm_total > 0.0 {
        fractions.push(("VM".to_string(), vm_total));
    }

    let total: f64 = fractions.iter().map(|(_, s)| s).sum();
    if total <= 0.0 {
        return String::new();
    }

    // Sort descending, collapse entries < 5% into "other"
    fractions.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

    let mut shown = Vec::new();
    let mut other = 0.0;
    for (label, secs) in &fractions {
        let pct = secs / total * 100.0;
        if pct >= 5.0 && shown.len() < 4 {
            shown.push((label.clone(), pct));
        } else {
            other += pct;
        }
    }
    if other > 0.5 {
        shown.push(("other".to_string(), other));
    }

    // Build the proportional bar (first entry filled, rest empty)
    let mut bar = String::with_capacity(BAR_WIDTH * 4);
    let mut chars_used = 0;
    for (i, (_, pct)) in shown.iter().enumerate() {
        let chars = if i == shown.len() - 1 {
            BAR_WIDTH - chars_used
        } else {
            ((pct / 100.0) * BAR_WIDTH as f64).round() as usize
        };
        let chars = chars.min(BAR_WIDTH - chars_used);
        let ch = if i == 0 { BAR_FILLED } else { BAR_EMPTY };
        for _ in 0..chars {
            bar.push(ch);
        }
        chars_used += chars;
    }

    let labels: Vec<String> = shown
        .iter()
        .map(|(name, pct)| format!("{name} {pct:.0}%"))
        .collect();

    format!("Total: {:.3}s  [{}] {}", total, bar, labels.join(" │ "))
}

/// Render a section header line.
pub(crate) fn section_header(title: &str) -> String {
    let prefix = format!("── {title} ");
    let padding = 54_usize.saturating_sub(prefix.len());
    format!("{}{}", prefix, "─".repeat(padding))
}

/// Render a group of timings with bar charts and a subtotal.
pub(crate) fn render_timing_section(entries: &[(String, Duration)]) -> String {
    let entries: Vec<_> = entries.iter().filter(|(k, _)| k != "VM-Total").collect();

    if entries.is_empty() {
        return String::new();
    }

    let display_names: Vec<String> = entries
        .iter()
        .map(|(k, _)| k.strip_prefix("VM-").unwrap_or(k).to_string())
        .collect();
    let display_width = display_names.iter().map(|n| n.len()).max().unwrap_or(0);

    let max_secs = entries
        .iter()
        .map(|(_, v)| v.as_secs_f64())
        .fold(0.0_f64, f64::max);

    let mut total = Duration::ZERO;
    let mut out = String::new();

    for (i, (_, dur)) in entries.iter().enumerate() {
        total += *dur;
        let bar = render_bar(dur.as_secs_f64(), max_secs);
        let bar_suffix = if bar.is_empty() {
            String::new()
        } else {
            format!("  {bar}")
        };
        out.push_str(&format!(
            "{:width$}  :    {:.6}s{}\n",
            display_names[i],
            dur.as_secs_f64(),
            bar_suffix,
            width = display_width,
        ));
    }

    // Subtotal right-aligned
    out.push_str(&format!(
        "{:>width$} {:.6}s\n",
        "Total:",
        total.as_secs_f64(),
        width = display_width + 8,
    ));

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn thousands_separator_small() {
        assert_eq!(fmt_thousands(0), "0");
        assert_eq!(fmt_thousands(1), "1");
        assert_eq!(fmt_thousands(999), "999");
    }

    #[test]
    fn thousands_separator_medium() {
        assert_eq!(fmt_thousands(1_000), "1,000");
        assert_eq!(fmt_thousands(12_345), "12,345");
        assert_eq!(fmt_thousands(999_999), "999,999");
    }

    #[test]
    fn thousands_separator_large() {
        assert_eq!(fmt_thousands(1_000_000), "1,000,000");
        assert_eq!(fmt_thousands(1_234_567_890), "1,234,567,890");
    }

    #[test]
    fn bar_chart_full() {
        let bar = render_bar(100.0, 100.0);
        assert_eq!(bar.chars().filter(|&c| c == '█').count(), BAR_WIDTH);
        assert_eq!(bar.chars().filter(|&c| c == '░').count(), 0);
    }

    #[test]
    fn bar_chart_empty() {
        let bar = render_bar(0.0, 100.0);
        assert_eq!(bar.chars().filter(|&c| c == '█').count(), 0);
        assert_eq!(bar.chars().filter(|&c| c == '░').count(), BAR_WIDTH);
    }

    #[test]
    fn bar_chart_half() {
        let bar = render_bar(50.0, 100.0);
        assert_eq!(bar.chars().filter(|&c| c == '█').count(), 15);
        assert_eq!(bar.chars().filter(|&c| c == '░').count(), 15);
    }

    #[test]
    fn bar_chart_zero_max() {
        assert_eq!(render_bar(0.0, 0.0), "");
    }

    #[test]
    fn summary_bar_single_entry() {
        let pipeline = vec![("parse".to_string(), Duration::from_millis(100))];
        let bar = render_summary_bar(&pipeline, &[]);
        assert!(bar.contains("Total: 0.100s"));
        assert!(bar.contains("parse 100%"));
    }

    #[test]
    fn summary_bar_empty() {
        assert_eq!(render_summary_bar(&[], &[]), "");
    }

    #[test]
    fn summary_bar_with_vm() {
        let pipeline = vec![("parse".to_string(), Duration::from_millis(80))];
        let vm = vec![("VM-Mutator".to_string(), Duration::from_millis(20))];
        let bar = render_summary_bar(&pipeline, &vm);
        assert!(bar.contains("parse 80%"));
        assert!(bar.contains("VM 20%"));
    }

    #[test]
    fn section_header_format() {
        let h = section_header("Pipeline");
        assert!(h.starts_with("── Pipeline "));
        assert!(h.contains("─────"));
    }

    #[test]
    fn timing_section_with_entries() {
        let entries = vec![
            ("parse".to_string(), Duration::from_millis(100)),
            ("cook".to_string(), Duration::from_millis(10)),
        ];
        let rendered = render_timing_section(&entries);
        assert!(rendered.contains("parse"));
        assert!(rendered.contains("cook"));
        assert!(rendered.contains("Total:"));
        assert!(rendered.contains("█"));
    }

    #[test]
    fn timing_section_strips_vm_prefix() {
        let entries = vec![
            ("VM-Mutator".to_string(), Duration::from_millis(50)),
            ("VM-Total".to_string(), Duration::from_millis(50)),
        ];
        let rendered = render_timing_section(&entries);
        assert!(rendered.contains("Mutator"));
        assert!(!rendered.contains("VM-Mutator"));
        assert!(!rendered.contains("VM-Total"));
    }

    #[test]
    fn timing_section_empty() {
        assert_eq!(render_timing_section(&[]), "");
    }
}
