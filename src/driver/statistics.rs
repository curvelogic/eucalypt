//! Capture and report statistics for optimisation

use std::{cmp::max, fmt::Display, time::Duration};

use indexmap::IndexMap;

#[derive(Default, Debug)]
pub struct Timings {
    timings: IndexMap<String, Duration>,
}

impl Timings {
    pub fn record<T: AsRef<str>>(&mut self, name: T, elapsed: Duration) {
        self.timings.insert(name.as_ref().to_string(), elapsed);
    }

    pub fn merge(&mut self, other: Timings) {
        self.timings.extend(other.timings);
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
        writeln!(f, "Machine Ticks          : {:10}", self.machine_ticks)?;
        writeln!(f, "Machine Allocs         : {:10}", self.machine_allocs)?;
        writeln!(f, "Machine Max Stack      : {:10}", self.machine_max_stack)?;
        writeln!(f, "Heap Blocks Allocated  : {:10}", self.blocks_allocated)?;
        writeln!(f, "Heap LOBs Allocated    : {:10}", self.lobs_allocated)?;
        writeln!(f, "Heap Blocks Used       : {:10}", self.blocks_used)?;
        writeln!(f, "Heap Blocks Recycled   : {:10}", self.blocks_recycled)?;
        writeln!(f, "Heap Peak Blocks       : {:10}", self.peak_heap_blocks)?;
        writeln!(f, "GC Collections         : {:10}", self.collections_count)?;
        writeln!(
            f,
            "GC Mark Time           : {:14.9}s",
            self.total_mark_time.as_secs_f64()
        )?;
        writeln!(
            f,
            "GC Sweep Time          : {:14.9}s",
            self.total_sweep_time.as_secs_f64()
        )?;
        writeln!(f)?;
        writeln!(f, "{}", self.timings)
    }
}
