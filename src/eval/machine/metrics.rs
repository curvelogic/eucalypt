//! Machine metrics

use std::cmp::max;

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
