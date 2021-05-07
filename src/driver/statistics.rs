//! Capture and report statistics for optimisation

use std::{fmt::Display, time::Duration};

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
    machine_ticks: u64,
    machine_allocs: u64,
    timings: Timings,
}

impl Statistics {
    pub fn set_ticks(&mut self, ticks: u64) {
        self.machine_ticks = ticks;
    }

    pub fn set_allocs(&mut self, allocs: u64) {
        self.machine_allocs = allocs;
    }

    pub fn timings_mut(&mut self) -> &mut Timings {
        &mut self.timings
    }

    pub fn merge(&mut self, other: Statistics) {
        self.machine_ticks += other.machine_ticks;
        self.machine_allocs += other.machine_allocs;
        self.timings.merge(other.timings);
    }
}

impl Display for Statistics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Machine Ticks  : {:10}", self.machine_ticks)?;
        writeln!(f, "Machine Allocs : {:10}", self.machine_allocs)?;
        writeln!(f)?;
        writeln!(f, "{}", self.timings)
    }
}
