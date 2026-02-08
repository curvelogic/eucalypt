//! Machine metrics

use std::cmp::max;
use std::collections::BTreeMap;
use std::time;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum ThreadOccupation {
    Initialisation,
    Mutator,
    CollectorMark,
    CollectorSweep,
}

#[derive(Default)]
pub struct Clock {
    durations: BTreeMap<ThreadOccupation, time::Duration>,
    current: Option<(ThreadOccupation, time::Instant)>,
}

impl Clock {
    pub fn report(&self) -> Vec<(String, time::Duration)> {
        let mut total = time::Duration::default();
        let mut report = vec![];
        for (k, v) in &self.durations {
            total += *v;
            report.push((format!("VM-{k:?}"), *v));
        }

        report.push(("VM-Total".to_string(), total));
        report
    }

    pub fn switch(&mut self, occupation: ThreadOccupation) {
        self.commit();
        self.current = Some((occupation, time::Instant::now()))
    }

    pub fn duration(&self, occupation: ThreadOccupation) -> time::Duration {
        self.durations.get(&occupation).copied().unwrap_or_default()
    }

    pub fn stop(&mut self) {
        self.commit();
        self.current = None
    }

    fn commit(&mut self) {
        if let Some((task, since)) = &self.current {
            let duration = since.elapsed();
            self.durations
                .entry(*task)
                .and_modify(|v| *v += duration)
                .or_insert(duration);
        }
    }
}

/// Record some metrics as we execute code
#[derive(Default)]
pub struct Metrics {
    ticks: u64,
    allocs: u64,
    max_stack: usize,
}

impl Metrics {
    pub fn tick(&mut self) {
        self.ticks += 1;
    }

    pub fn ticks(&self) -> u64 {
        self.ticks
    }

    pub fn alloc(&mut self, count: usize) {
        self.allocs += count as u64;
    }

    pub fn allocs(&self) -> u64 {
        self.allocs
    }

    pub fn stack(&mut self, size: usize) {
        self.max_stack = max(self.max_stack, size);
    }

    pub fn max_stack(&self) -> usize {
        self.max_stack
    }
}
