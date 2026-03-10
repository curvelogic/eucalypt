//! Mark state
//!
//! The mark state is now held per-`Heap` instance (see `Heap::mark_state` and
//! `Heap::flip_mark_state`) rather than as a process-wide global.  This module
//! is retained as a placeholder so that existing `use` paths compile during any
//! transitional period; it may be removed once all references are confirmed gone.
