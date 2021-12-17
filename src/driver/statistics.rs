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
        self.timings.extend(other.timings.into_iter());
    }
}

impl Display for Timings {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let width = self.timings.keys().map(|k| k.len()).max().unwrap() + 1;

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

    pub fn merge(&mut self, other: Statistics) {
        self.machine_ticks += other.machine_ticks;
        self.machine_allocs += other.machine_allocs;
        self.machine_max_stack = max(self.machine_max_stack, other.machine_max_stack);
        self.timings.merge(other.timings);
        self.blocks_allocated = max(self.blocks_allocated, other.blocks_allocated);
        self.lobs_allocated = max(self.lobs_allocated, other.lobs_allocated);
    }
}

impl Display for Statistics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Machine Ticks          : {:10}", self.machine_ticks)?;
        writeln!(f, "Machine Allocs         : {:10}", self.machine_allocs)?;
        writeln!(f, "Machine Max Stack      : {:10}", self.machine_max_stack)?;
        writeln!(f, "Machine Max Stack      : {:10}", self.machine_max_stack)?;
        writeln!(f, "Heap Blocks Allocated  : {:10}", self.blocks_allocated)?;
        writeln!(f, "Heap LOBs Allocated    : {:10}", self.lobs_allocated)?;
        writeln!(f)?;
        writeln!(f, "{}", self.timings)
    }
}
